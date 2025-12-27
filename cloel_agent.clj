(ns cloel-agent
  (:require [cloel :as cloel]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]))

;; --- 配置 ---
(def ^:dynamic *project-dir* (str (System/getProperty "user.home") "/.emacs.d/cloel-agent/workspace"))
(def ollama-url "http://192.168.1.1:11434/api/generate")

;; --- 界面反馈 ---
(defn log-to-elisp [msg & [type]] (cloel/elisp-eval-async "cloel-agent-append-log" msg (or type "info")))
(defn update-elisp-status [status] (cloel/elisp-eval-async "cloel-agent-update-status" status))

;; 心跳提示
(defn with-keep-alive [task-fn]
  (let [stop (atom false)]
    (future (while (not @stop) (Thread/sleep 15000) (when-not @stop (log-to-elisp "⏳ AI 正在深度思考中，请稍候..." "debug"))))
    (try (task-fn) (finally (reset! stop true)))))

;; --- Ollama POST 交互 ---
(defn manual-json-post [url prompt attempt]
  (log-to-elisp (format "📡 [第 %d 次] 正在向 Ollama 提交请求..." attempt) "info")
  (let [payload (str "{\"model\":\"deepcoder:32b\",\"prompt\":\"" 
                     (-> prompt (str/replace "\\" "\\\\") (str/replace "\"" "\\\"") (str/replace "\n" "\\n"))
                     "\",\"stream\":false}")
        conn (.openConnection (java.net.URL. url))]
    (doto conn (.setRequestMethod "POST") (.setDoOutput true)
          (.setRequestProperty "Content-Type" "application/json")
          (.setConnectTimeout 20000) (.setReadTimeout 300000))
    (with-open [os (.getOutputStream conn)] (.write os (.getBytes payload "UTF-8")))
    (if (= 200 (.getResponseCode conn))
      (let [body (slurp (.getInputStream conn) :encoding "UTF-8")]
        (-> (re-find #"\"response\":\"(.*?)\",\"done\"" body) second
            (str/replace "\\n" "\n") (str/replace "\\\"" "\"") (str/replace "\\\\" "\\")))
      (throw (Exception. (str "Ollama 响应异常: " (.getResponseCode conn)))))))

;; --- 解析 AI 响应 ---
(defn parse-ai-response [raw-text]
  (let [deps (re-find #"(?s)```clojure\s+;; deps.edn\n(.*?)\n```" raw-text)
        code (re-find #"(?s)```clojure\s+;; core.clj\n(.*?)\n```" raw-text)
        blocks (re-seq #"(?s)```clojure\s*(.*?)\s*```" raw-text)]
    {:deps (cond deps (str/trim (second deps)) (> (count blocks) 1) (str/trim (nth (first blocks) 1)) :else "{:deps {}}")
     :code (cond code (str/trim (second code)) (> (count blocks) 1) (str/trim (nth (second blocks) 1)) 
                 (= (count blocks) 1) (str/trim (nth (first blocks) 1)) :else nil)}))

;; --- 核心逻辑：分阶段测试 (依赖阶段 + 执行阶段) ---

(defn run-step-by-step [deps-content code-content]
  (try
    (io/make-parents (str *project-dir* "/core.clj"))
    (spit (str *project-dir* "/deps.edn") deps-content)
    (spit (str *project-dir* "/core.clj") code-content)

    ;; 阶段 1: 验证 deps.edn (预下载依赖)
    (log-to-elisp "🔍 正在验证依赖配置 (deps.edn)..." "info")
    (let [prep-res (shell/sh "clojure" "-P" :dir *project-dir*)]
      (if-not (= 0 (:exit prep-res))
        {:stage :deps-error :out (str (:out prep-res) "\n" (:err prep-res))}
        
        ;; 阶段 2: 执行代码
        (do
          (log-to-elisp "⚡ 依赖配置正确，正在运行代码..." "info")
          (let [exec-res (shell/sh "clojure" "-M" "-i" "core.clj" :dir *project-dir*)]
            (if (= 0 (:exit exec-res))
              {:stage :success :out (:out exec-res)}
              {:stage :code-error :out (str (:out exec-res) "\n" (:err exec-res))})))))
    (catch Exception e
      {:stage :system-error :out (.getMessage e)})))

;; --- 引擎循环 ---

(defn start-ai-agent-engine [goal]
  (future
    (try
      (loop [attempt 1
             current-prompt (str "任务目标：" goal "
             要求：
             1. 必须输出 ;; deps.edn 和 ;; core.clj 两个代码块。
             2. 如果使用第三方库，请务必在 deps.edn 中包含正确的 Maven 坐标。")]
        (if (> attempt 100)
          (do (log-to-elisp "❌ 超过重试上限，任务中止。" "error") (update-elisp-status "中止"))
          
          (let [ai-raw (try (with-keep-alive #(manual-json-post ollama-url current-prompt attempt))
                            (catch Exception e (str "COMM_ERROR:" (.getMessage e))))]
            
            (if (str/starts-with? ai-raw "COMM_ERROR:")
              (do (log-to-elisp (str "❌ 通信失败: " ai-raw) "error") (Thread/sleep 5000) (recur (inc attempt) current-prompt))
              
              (let [{:keys [deps code]} (parse-ai-response ai-raw)]
                (if-not code
                  (recur (inc attempt) (str current-prompt "\n错误：未检测到 ;; core.clj 代码块。"))
                  
                  (let [res (run-step-by-step deps code)]
                    (case (:stage res)
                      :success 
                      (do (log-to-elisp (str "✅ 任务完成！结果：\n" (:out res)) "info")
                          (update-elisp-status "成功")
                          (cloel/elisp-eval-async "cloel-agent-task-finished" (str (:out res))))

                      :deps-error
                      (do (log-to-elisp "⚠️ 依赖配置 (deps.edn) 错误，请求 AI 修正..." "error")
                          (recur (inc attempt) (str "你提供的 deps.edn 无法配置成功。错误信息：\n" (:out res) 
                                                  "\n请修复依赖坐标并重新提供 deps.edn 和 core.clj。")))

                      :code-error
                      (do (log-to-elisp "⚠️ 代码运行报错，请求 AI 修正..." "error")
                          (recur (inc attempt) (str "代码运行报错：\n" (:out res) 
                                                  "\n请分析错误并修复 core.clj。")))

                      (do (log-to-elisp (str "❌ 系统错误: " (:out res)) "error") 
                          (update-elisp-status "系统错误"))))))))))
      (catch Exception e (log-to-elisp (str "引擎崩溃: " (.getMessage e)) "error")))))

;; --- 路由 ---
(defn agent-handle-client-connected [client-id]
  (cloel/elisp-eval-async "cloel-agent-start-process-confirm" (str client-id)))

(defn agent-handle-client-async-call [& all-args]
  (let [data (first all-args)
        func (if (map? data) (:func data) data)
        args (if (map? data) (:args data) (second all-args))
        goal (if (coll? args) (first args) args)]
    (case func "agent-success" (start-ai-agent-engine goal) (println "Unknown call"))))

(alter-var-root #'cloel/handle-client-connected (constantly agent-handle-client-connected))
(alter-var-root #'cloel/handle-client-async-call (constantly agent-handle-client-async-call))
(cloel/start-server (Integer/parseInt (first *command-line-args*)))

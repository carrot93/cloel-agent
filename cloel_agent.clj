(ns cloel-agent
  (:require [cloel :as cloel]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]))

;; --- config ---
(def ^:dynamic *project-dir* (str (System/getProperty "user.home") "/.emacs.d/cloel-agent/workspace"))
(def ollama-url "http://192.168.1.1:11434/api/generate")

;; --- interface ---
(defn log-to-elisp [msg & [type]] (cloel/elisp-eval-async "cloel-agent-append-log" msg (or type "info")))
(defn update-elisp-status [status] (cloel/elisp-eval-async "cloel-agent-update-status" status))

;; heart-beat
(defn with-keep-alive [task-fn]
  (let [stop (atom false)]
    (future (while (not @stop) (Thread/sleep 15000) (when-not @stop (log-to-elisp "⏳ AI in thinking, please wait..." "debug"))))
    (try (task-fn) (finally (reset! stop true)))))

;; --- Ollama POST chat ---
(defn manual-json-post [url prompt attempt]
  (log-to-elisp (format "📡 [NO. %d ] post to ollama..." attempt) "info")
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
      (throw (Exception. (str "Ollama response error: " (.getResponseCode conn)))))))

;; --- parse AI info ---
(defn parse-ai-response [raw-text]
  (let [deps (re-find #"(?s)```clojure\s+;; deps.edn\n(.*?)\n```" raw-text)
        code (re-find #"(?s)```clojure\s+;; core.clj\n(.*?)\n```" raw-text)
        blocks (re-seq #"(?s)```clojure\s*(.*?)\s*```" raw-text)]
    {:deps (cond deps (str/trim (second deps)) (> (count blocks) 1) (str/trim (nth (first blocks) 1)) :else "{:deps {}}")
     :code (cond code (str/trim (second code)) (> (count blocks) 1) (str/trim (nth (second blocks) 1)) 
                 (= (count blocks) 1) (str/trim (nth (first blocks) 1)) :else nil)}))

;; --- core logicical ： step testing (env + process) ---

(defn run-step-by-step [deps-content code-content]
  (try
    (io/make-parents (str *project-dir* "/core.clj"))
    (spit (str *project-dir* "/deps.edn") deps-content)
    (spit (str *project-dir* "/core.clj") code-content)

    ;; step 1: check deps.edn (predownload deps)
    (log-to-elisp "🔍 Check the deps (deps.edn)..." "info")
    (let [prep-res (shell/sh "clojure" "-P" :dir *project-dir*)]
      (if-not (= 0 (:exit prep-res))
        {:stage :deps-error :out (str (:out prep-res) "\n" (:err prep-res))}
        
        ;; step 2: run code
        (do
          (log-to-elisp "⚡ deps.edn passed，runing code..." "info")
          (let [exec-res (shell/sh "clojure" "-M" "-i" "core.clj" :dir *project-dir*)]
            (if (= 0 (:exit exec-res))
              {:stage :success :out (:out exec-res)}
              {:stage :code-error :out (str (:out exec-res) "\n" (:err exec-res))})))))
    (catch Exception e
      {:stage :system-error :out (.getMessage e)})))

;; --- cycle Engine ---

(defn start-ai-agent-engine [goal]
  (future
    (try
      (loop [attempt 1
             current-prompt (str "task target：" goal "
             rules：
             1. must output ;; deps.edn and ;; core.clj code blocks。
             2. if using third-part library，plase in deps.edn including Maven coordination")]
        (if (> attempt 100)
          (do (log-to-elisp "❌ beyond the task count max limited，task stopped" "error") (update-elisp-status "stopped"))
          
          (let [ai-raw (try (with-keep-alive #(manual-json-post ollama-url current-prompt attempt))
                            (catch Exception e (str "COMM_ERROR:" (.getMessage e))))]
            
            (if (str/starts-with? ai-raw "COMM_ERROR:")
              (do (log-to-elisp (str "❌ chat failed: " ai-raw) "error") (Thread/sleep 5000) (recur (inc attempt) current-prompt))
              
              (let [{:keys [deps code]} (parse-ai-response ai-raw)]
                (if-not code
                  (recur (inc attempt) (str current-prompt "\nerror：can't find ;; core.clj code block"))
                  
                  (let [res (run-step-by-step deps code)]
                    (case (:stage res)
                      :success 
                      (do (log-to-elisp (str "✅ task success！result：\n" (:out res)) "info")
                          (update-elisp-status "success")
                          (cloel/elisp-eval-async "cloel-agent-task-finished" (str (:out res))))

                      :deps-error
                      (do (log-to-elisp "⚠️ Deps (deps.edn) error，ask AI to fix..." "error")
                          (recur (inc attempt) (str "Your deps.edn can't be right. error info \n" (:out res) 
                                                  "\nPlease fix the coordination and reply  deps.edn and core.clj。")))

                      :code-error
                      (do (log-to-elisp "⚠️ code running error, ask AI to fix..." "error")
                          (recur (inc attempt) (str "code running error：\n" (:out res) 
                                                  "\nPlease analysis and fix core.clj。")))

                      (do (log-to-elisp (str "❌ system error: " (:out res)) "error") 
                          (update-elisp-status "system error"))))))))))
      (catch Exception e (log-to-elisp (str "engine crashed: " (.getMessage e)) "error")))))

;; --- route ---
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

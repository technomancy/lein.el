;;; lein.el --- Eshell interface to Leiningen

;; Copyright © 2013 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/lein.el
;; Version: 0.1
;; Created: 2013-01-26
;; Keywords: tools, convenience
;; Package-Requires: ((nrepl "0.1.7"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This provides an eshell interface to the Leiningen project
;; automation tool for Clojure. (http://leiningen.org) It communicates
;; over nREPL (https://github.com/kingtim/nrepl.el) to avoid starting
;; a new process for every command. Note that tasks which call
;; eval-in-project will still start a project JVM; it's only
;; Leiningen's own startup time which is avoided.

;;; Usage

;; Currently you need to launch Leiningen once per Emacs instance with
;; M-x lein-launch. Then start eshell with M-x eshell and use
;; Leiningen as you would normally.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl)
(require 'nrepl)

(defvar lein-nrepl-connection-buffer "*lein-nrepl-connection*")

(defvar lein-server-buffer "*lein-server*")

(defcustom lein-home (expand-file-name "~/.lein") "Leiningen home directory.")

(defcustom lein-version "2.0.0"
  "Version of Leiningen to use. Note that changing this
  arbitrarily will not always work.")

(defcustom lein-java-command (or (getenv "LEIN_JAVA_CMD")
                                 (getenv "JAVA_CMD")
                                 "java")
  "Java executable to use to launch Leiningen.")

(defcustom lein-jvm-opts (or (getenv "LEIN_JVM_OPTS") "-Xms64m -Xmx512m")
  "Extra arguments to the java command to launch Leiningen.")

(defvar lein-words-of-inspiration
  '("Take this project automation tool brother; may it serve you well."))

(defvar lein-download-url
  "https://leiningen.s3.amazonaws.com/downloads/leiningen-%s-standalone.jar")

(defun lein-self-install (lein-jar)
  (message "Leiningen not found; downloading...")
  ;; TODO: make this async
  (with-current-buffer (url-retrieve-synchronously
                        (format lein-download-url lein-version))
    ;; TODO: remove HTTP headers
    (write-file lein-jar))
  (message "...done."))

;; TODO: launch lein process with nohup so it can outlast Emacs
;; TODO: check for repl-port written to lein-home
(defun lein-launch-command ()
  (let ((lein-jar (format "%s/self-installs/leiningen-%s-standalone.jar"
                          lein-home lein-version)))
    (when (not (file-exists-p lein-jar))
      (lein-self-install lein-jar))
    (concat "LEIN_VERSION=" lein-version " "
            lein-java-command " -client -XX:+TieredCompilation"
            " -Xbootclasspath/a:" lein-jar lein-jvm-opts
            " -Dfile.encoding=UTF-8 -Dmaven.wagon.http.ssl.easy=false"
            " -Dleiningen.original.pwd=" default-directory
            " -classpath " lein-jar " clojure.main -m"
            " leiningen.core.main repl :headless")))

(defun lein-project-root (&optional file)
  (locate-dominating-file (or file default-directory) "project.clj"))

(defun lein-command-string (root task &rest args)
  (when (string= "trampoline" task)
    (error "Cannot trampoline from lein.el."))
  (let ((project-clj (expand-file-name "project.clj" root)))
    (format "(binding [leiningen.core.main/*exit-process?* false]
               (try (leiningen.core.main/apply-task \"%s\" %s '%s)
                    (catch Exception e
                      (if (:exit-code (ex-data e))
                        (when-not (= \"Suppressed exit\" (.getMessage e))
                          (println (.getMessage e)))
                        (clj-stacktrace.repl/pst e)))))"
            task (if (file-exists-p project-clj)
                     (format "(leiningen.core.project/read \"%s\")" project-clj)
                   "nil")
                (or (mapcar (apply-partially 'format "\"%s\"") args) []))))

(defun lein-launched? ()
  (and (get-buffer-process lein-nrepl-connection-buffer)
       (process-live-p (get-buffer-process lein-nrepl-connection-buffer))))

(defun lein-launch ()
  (interactive)
  (let* ((default-directory lein-home)
         (process (start-process-shell-command
                  "lein-server" lein-server-buffer
                  (lein-launch-command))))
    (set-process-filter process 'lein-server-filter)
    (set-process-sentinel process 'lein-server-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (message "Starting Leiningen...")))

(defun lein-server-filter (process output)
  (with-current-buffer (process-buffer process)
    (insert output))
  (when (string-match "nREPL server started on port \\([0-9]+\\)" output)
    (let ((port (string-to-number (match-string 1 output)))
          (nrepl-words-of-inspiration lein-words-of-inspiration)
          (original-nrepl-connection-list nrepl-connection-list))
      (let ((nrepl-process (save-window-excursion
                             (nrepl-connect "localhost" port))))
        (with-current-buffer (process-buffer process)
          (setq nrepl-connection-buffer
                (buffer-name (process-buffer nrepl-process))
                lein-nrepl-connection-buffer
                (buffer-name (process-buffer nrepl-process))))
        (with-current-buffer (process-buffer nrepl-process)
          (setq nrepl-server-buffer
                (buffer-name (process-buffer process))
                lein-server-buffer
                (buffer-name (process-buffer process))))
        (bury-buffer (nrepl-current-nrepl-buffer)) ; TODO: this does nothing; ugh
        (when original-nrepl-connection-list
          (nrepl-make-repl-connection-default
           (car original-nrepl-connection-list)))))))

(defun lein-server-sentinel (process event)
  (let* ((b (process-buffer process))
         (problem (and b (buffer-live-p b)
                       (with-current-buffer b
                         (buffer-substring (point-min) (point-max))))))
    (when b
      (kill-buffer b))
    (cond ((string-match "^killed" event) nil)
          ((string-match "^hangup" event) (nrepl-quit))
          (t (error "Could not start Leiningen: %s" (or problem ""))))))

(defun lein-handler (task-complete? buffer response)
  (let ((out (cdr (assoc "out" response)))
        (err (cdr (assoc "err" response)))
        (status (cdr (assoc "status" response))))
    (when out
      (with-current-buffer buffer
        (eshell-output-filter nil out)))
    (when err
      (with-current-buffer buffer
        (eshell-output-filter nil err)))
    (when (member "eval-error" status)
      (nrepl-dbind-response response (value ns out err status id ex root-ex
                                            session)
        (funcall nrepl-err-handler buffer ex root-ex session)))
    (when (or (member "done" status)
              (member "eval-error" status))
      (setf (car task-complete?) t)
      (eshell-remove-process-entry entry))))

(defun eshell/lein (&rest args)
  (if (lein-launched?)
      (let ((nrepl-connection-buffer lein-nrepl-connection-buffer)
            ;; woo promises for dummies
            (task-complete? (list nil)))
        (nrepl-send-string (apply 'lein-command-string
                                  (lein-project-root) args)
                           (apply-partially 'lein-handler
                                            task-complete?
                                            (current-buffer)))
        (while (not (car task-complete?))
          (sit-for eshell-process-wait-seconds
                   eshell-process-wait-milliseconds)))
    (lein-launch) ; TODO: callback to execute command instead of manual retry
    "Launching Leiningen; wait till it's up and try your command again."))

;; TODO: port from pcmpl-lein.el
(defun pcomplete/lein ())

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(provide 'lein)
;;; lein.el ends here

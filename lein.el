;;; lein.el --- Leiningen interface

;; Copyright © 2013 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/lein.el
;; Version: 0.1
;; Created: 2013-01-26
;; Keywords: tools
;; Package-Requires: ((nrepl "0.1.5"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:


;;; Install

;;; Usage


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

(require 'nrepl)

(defvar lein-nrepl-connection-buffer "*lein-nrepl-connection*")

(defvar lein-server-buffer "*lein server*")

(defcustom lein-home (expand-file-name "~/.lein") "Leiningen home directory.")

(defcustom lein-version "2.0.0" "Version of Leiningen to use.")

(defcustom lein-java-command (or (getenv "LEIN_JAVA_CMD")
                                 (getenv "JAVA_CMD")
                                 "java")
  "Java executable to use to launch Leiningen.")

(defcustom lein-jvm-opts (or (getenv "LEIN_JVM_OPTS") "-Xms64m -Xmx512m")
  "Extra arguments to the java command to launch Leiningen.")

(defun lein-launch-command ()
  (let ((lein-jar (format "%s/self-installs/leiningen-%s-standalone.jar"
                          lein-home lein-version)))
    (concat "LEIN_VERSION=" lein-version " "
            lein-java-command " -client -XX:+TieredCompilation"
            " -Xbootclasspath/a:" lein-jar lein-jvm-opts
            " -Dfile.encoding=UTF-8 -Dmaven.wagon.http.ssl.easy=false"
            " -Dleiningen.original.pwd=" default-directory
            " -classpath " lein-jar " clojure.main -m"
            " leiningen.core.main repl :headless :port")))

(defun lein-project-root (&optional file)
  (locate-dominating-file (or file default-directory) "project.clj"))

(defun lein-command-string (root task &rest args)
  ;; TODO: blacklist trampoline
  (format "(binding [leiningen.core.main/*exit-process?* false]
               (try (leiningen.core.main/apply-task \"%s\"
                      (leiningen.core.project/read \"%s\") '%s)
                    (catch Exception e
                      (if (:exit-code (ex-data e))
                        (when-not (= \"Suppressed exit\" (.getMessage e))
                          (println (.getMessage e)))
                        (clj-stacktrace.repl/pst e)))))"
          task (expand-file-name "project.clj" root) (or args [])))

;; for now, you must launch it yourself
(defun lein-launch ()
  (interactive)
  (let ((process (start-process-shell-command
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
          (nrepl-connection-buffer lein-nrepl-connection-buffer))
      ;; (flet nrepl-init-client-sessions
      ;;   nil as second nrepl-new-session-handler arg
      ;;   )
      (nrepl-connect "localhost" port))))

(defun lein-server-sentinel (process event)
  (let* ((b (process-buffer process))
         (problem (if (and b (buffer-live-p b))
                      (with-current-buffer b
                        (buffer-substring (point-min) (point-max)))
                    "")))
    (when b
      (kill-buffer b))
    (cond
     ((string-match "^killed" event)
      nil)
     ((string-match "^hangup" event)
      (nrepl-quit))
     (t (error "Could not start Leiningen: %s" problem)))))

(defun eshell/lein (&rest args)
  (let ((nrepl-connection-buffer lein-nrepl-connection-buffer))
    ;; TODO: make this async ... somehow
    (eshell-print (plist-get (nrepl-send-string-sync
                              (apply 'lein-command-string
                                     (lein-project-root) args))
                             :stdout)))
  nil)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(provide 'lein)
;;; lein.el ends here

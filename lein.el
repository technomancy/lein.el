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

;; TODO: allow lein.el nrepl connection to co-exist with project connection
(defvar lein-nrepl-connection-buffer "*lein-nrepl-connection*")

(defvar lein-nrepl-session nil)

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

(defun lein-output-handler (buffer string)
  (with-current-buffer (get-buffer-create buffer)
    (delete-region (point-min) (point-max))
    (insert string)
    (display-buffer buffer)
    (nrepl-show-maximum-output)
    (define-key (kbd "q") 'bury-buffer)
    (setq buffer-read-only t)))

(defun lein-make-handler (buffer)
  (nrepl-make-response-handler buffer nil
                               'lein-output-handler 'lein-output-handler nil))

;; for now, manual connection to running Leiningen process is required.
(defun lein-launch (project-root)
  ;; TODO:
  ;; * check JVM_OPTS
  ;; * JAVA_CMD, LEIN_JAVA_CMD
  ;; * allow lein version to be chosen
  ;; * honor .lein-classpath
  ;; * 
  )

(defun lein (command)
  ;; TODO: completion for commands
  (interactive "MCommand: ")
  ;; flet nrepl-current-session?
  (let ((nrepl-connection-buffer lein-nrepl-connection-buffer))
    (nrepl-send-string (apply 'lein-command-string
                              (lein-project-root)
                              (split-string command-string " "))
                       (lein-make-handler "*lein-out*"))))

(defun eshell/lein (&rest args)
  (eshell-print (plist-get (nrepl-send-string-sync
                            (apply 'lein-command-string
                                   (lein-project-root) args))
                           :stdout))
  nil)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

(provide 'lein)
;;; lein.el ends here

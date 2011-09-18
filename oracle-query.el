;;; oracle-query.el --- execute sql select using sqlplus. -*- coding:utf-8 -*-

;; Copyright (C) 2011 孤峰独秀

;; Author: 孤峰独秀 jixiuf@gmail.com
;; Keywords: sqlplus emacs sql

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; execute sql using sqlplus and return as list .
;;
;; (oracle-query "select empno,ename from emp where empno<=7499")
;; got : (("7369" "SMITH") ("7499" "ALLEN"))
;; using default connection ,not recommend.
;;
;; (defvar connection (oracle-query-create-connection "scott/tiger"))
;; (oracle-query "select empno from emp" connection)
;; recommended
;;
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `oracle-query-create-connection'
;;    create a connection to oracle using sqlplus ,and return the
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(defvar oracle-query-default-connection nil)

(defvar oq-timeout-wait-for-result 300
  "waiting 300s for sql result returned.")

(defvar oq-linesize 20000 "Default linesize for sqlplus")

(defun oq-parse-result-as-list (raw-result)
  (let (result row)
    (with-temp-buffer
      (insert raw-result)
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n]*[ \t\n]*" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (while (not (= (point-at-eol) (point-max)))
        (setq row (split-string (buffer-substring-no-properties
                                 (point-at-bol) (point-at-eol)) "" t))
        (setq result (append result (list row)))
        ;;      (add-to-list 'result row t)
        (forward-line) (beginning-of-line))
      )result ))

;; (defun oq-build-connection-string()
;;   " default:sqlplus scott/tiger@localhost:1521/orcl"
;;   (format "sqlplus %s/%s@%s:%s/%s %s"
;;           oq-username oq-password oq-server oq-port oq-dbname (if oq-as-sysdba "as sysdba" "")))
(defun oracle-query-read-connect-string ()
  (let( (connection-string  (read-string "Connect String:" "" nil)))
    (list connection-string)))

;; (oracle-query-create-connection "scott/tiger")
;; (oracle-query-create-connection "scott/tiger@localhost:1521/orcl")
;; (oracle-query-create-connection "system/root as sysdba")
;;;###autoload
(defun oracle-query-create-connection(connect-string)
  "create a connection to oracle using sqlplus ,and return the
created process"
  (interactive (oracle-query-read-connect-string))
  (let ((oracle-query-process (start-process-shell-command
                               "sqlplus"
                               (concat " *oracle-query" (number-to-string (random)) "*")
                               (concat "sqlplus " connect-string))))
    (process-send-string oracle-query-process "set heading off;\n")
    (process-send-string oracle-query-process (format "set linesize %d;\n" oq-linesize))
    (process-send-string oracle-query-process "set colsep '';\n");;column separater
    (process-send-string oracle-query-process "set null 'NULL';\n");;
    (process-send-string oracle-query-process "set wrap off;\n")
    (process-send-string oracle-query-process "set pagesize 0;\n")
    (process-send-string oracle-query-process "set feedback on;\n")
    (process-send-string oracle-query-process "set serveroutput on;\n")
    ;; (set-process-filter oracle-query-process 'oq-filter-fun)
    oracle-query-process))


;; (oracle-query "select empno from emp")
;; (oracle-query "select empno from emp" (oracle-query-create-connection "scott/tiger"))
;;;###autoload
(defun oracle-query (sql &optional oracle-query-process)
  "execute sql using `sqlplus' ,and return the result of it."
  (let ((process oracle-query-process))
    (when (or (not process)
              (not (equal (process-status process ) 'run)))
      (when (or (not oracle-query-default-connection)
                (not (equal (process-status oracle-query-default-connection ) 'run)))
        (setq oracle-query-default-connection (call-interactively   'oracle-query-create-connection)))
      (setq process oracle-query-default-connection))
    (when (string-match "\\(.*\\);[ \t]*" sql)
      (setq sql (match-string 1 sql)))
    (with-current-buffer (process-buffer process)
      (delete-region (point-min) (point-max))
      (let ((start (point-min)) end)
        (goto-char (point-max))
        (process-send-string process (format "%s ;\n" sql))
        (goto-char (point-min))
        (while (not (re-search-forward "^[0-9]+ rows? selected\\|no rows selected" nil t 1))
          (when (accept-process-output process oq-timeout-wait-for-result 0 nil)
            (goto-char (point-min))))
        (setq end (1- (match-beginning 0)))
        (when (re-search-backward "\\bSQL> " nil t 1) (setq start (match-end 0)))
        (oq-parse-result-as-list (buffer-substring start end))))  )
  )

(provide 'oracle-query)
;;; oracle-query.el ends here



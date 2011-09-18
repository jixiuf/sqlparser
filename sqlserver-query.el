;;; sqlserver-query.el --- execute sql select using sqlcmd.exe or osql.exe on SQL SERVER. -*- coding:utf-8 -*-

;; Copyright (C) 2011 孤峰独秀

;; Author: 孤峰独秀  jixiuf@gmail.com
;; Keywords: sqlserver emacs sql sqlcmd.exe osql.exe
;; Filename: sqlserver-query.el
;; Description:  execute sql select using sqlcmd.exe or osql.exe on SQL SERVER
;; Created: 2011年08月17日 星期三 22时11分54秒
;; Version: 0.1.0
;; URL:http://www.emacswiki.org/emacs/down/sqlserver-query.el
;;     https://github.com/jixiuf/sqlparser

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  execute sql using sqlcmd.exe or osql.exe and return as list .
;;  (sqlserver-query "select empno,ename from emp where empno<=7499")
;;  got : (("7369" "SMITH") ("7499" "ALLEN"))
;;
;; 1 make sure sqlcmd.exe or osql.exe are in your path .and sqlserver is started

;; 2. you should custom these variable
;;  `sqlserver-username'
;;  `sqlserver-password'
;;  `sqlserver-server-instance'
;;  `sqlserver-dbname'
;;  `sqlserver-cmd' ;sqlcmd or osql
;; for example
;;    (setq sqlserver-username "sa")
;;    (setq sqlserver-password "sa")
;;    (setq sqlserver-server-instance "localhost\\SQLEXPRESS")
;; or sometimes
;;   (setq sqlserver-server-instance "localhost")
;; is enough
;;   (setq sqlserver-dbname "master")
;;   (setq sqlserver-cmd' 'sqlcmd) or (setq sqlserver-cmd' 'osql)

;; 3. call function `sqlserver-query'
;;   (sqlserver-query "select * from sysobjects where type='u'")


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `sqlserver-query-create-connection'
;;    open connection with sqlcmd.exe or osql.exe.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `sqlserver-connection-info'
;;    sqlserver connection info .
;;    default = (quote ((username . "sa") (password . "sa") (server-instance . "localhost\\SQLEXPRESS") (dbname . "master")))
;;  `sqlserver-cmd'
;;    sqlserver-cmd  now  support sqlcmd.exe and osql.exe
;;    default = (quote sqlcmd)

;;; Code:

(require 'sql)

(defgroup sqlserver-query nil
  "SQL SERVER QUERY"
  :group 'SQL)
(defcustom sqlserver-connection-info
  '((username . "sa")
    (password . "sa")
    (server-instance . "localhost\\SQLEXPRESS")
    (dbname . "master"))
  "sqlserver connection info ."
  :group 'sqlserver-query
  :type 'alist)
(make-variable-buffer-local 'sqlserver-connection-info)

;; (defcustom sqlserver-username "sa"
;;   "sqlserver user name."
;;   :group 'sqlserver-query
;;   :type 'string)

;; (defcustom sqlserver-password "sa"
;;   "sqlserver user password."
;;   :group 'sqlserver-query
;;   :type 'string)

;; (defcustom sqlserver-server-instance "localhost\\SQLEXPRESS"
;;   "Default server or host."
;;   :type 'string
;;   :group 'sqlserver-query
;;   :safe 'stringp)

;; (defcustom sqlserver-dbname "master"
;;   "database name ."
;;   :type 'string
;;   :group 'sqlserver-query
;;   :safe 'stringp)

(defcustom sqlserver-cmd 'sqlcmd
  "sqlserver-cmd  now  support sqlcmd.exe and osql.exe
sqlserver 2005 add new cmd sqlcmd.exe. and osql.exe is not recommended."
  :type '(choice (const sqlcmd) (const osql))
  :group 'sqlserver-query)

(defvar sqlserver-timeout-wait-for-result 300
  "waiting 300s for sql result returned.")

(defvar sqlserver-query-process nil)
(defvar sqlserver-query-result nil)
(defvar sqlserver-query-buffer  " *sqlserver-query*")

(defun sqlserver-parse-result-as-list-4-osql (raw-result)
  (let  (result row line-count line)
    (with-temp-buffer
      (insert raw-result)
      (goto-char  (point-min))
      (when (re-search-forward "(.*\\(行受影响\\|rows affected\\))" nil t)
        (replace-match "" nil nil))
      (goto-char  (point-min))
      (while (re-search-forward "[ \t\n]*[ \t\n]*" nil t)
        (replace-match "" nil nil))
      (goto-char  (point-min))
      (while (re-search-forward "\\([ \t]+$\\|^[ \t]+\\)" nil t)
        (replace-match "" nil nil))
      (setq line-count (count-lines (point-min) (point-max)))
      (goto-char  (point-min))
      (while (< (line-number-at-pos)  line-count )
	(setq line  (buffer-substring-no-properties
		     (point-at-bol) (point-at-eol)))
	(unless (string-match "^[ \t]*$" line)
	  (setq row (split-string line "" t))
	  (when row (setq result (append result (list row)))))
	(forward-line))
      )result ))

(defun sqlserver-parse-result-as-list-4-sqlcmd (raw-result)
  (let  (result row line-count)
    (with-temp-buffer
      (insert raw-result)
      (setq line-count (count-lines (point-min) (point-max)))
      (goto-char  (point-min))
      (while (< (line-number-at-pos) (- line-count 1))
        (setq row (split-string (buffer-substring-no-properties
                                 (point-at-bol) (point-at-eol)) "" t))
        (setq result (append result (list row)))
        (forward-line))
      )result ))

(defun sqlserver-parse-result-as-list (raw-result)
  (if (equal sqlserver-cmd 'sqlcmd)
      (sqlserver-parse-result-as-list-4-sqlcmd raw-result)
    (sqlserver-parse-result-as-list-4-osql raw-result)))

;; (sqlserver-format-connect-string sqlserver-connection-info)
(defun sqlserver-format-connect-string(connection-info)
  "default:sqlcmd -S localhost\\SQLEXPRESS -U sa -P sa -d master -h-1 -w 65535   -s \"\^E\" -W"
  (if (equal sqlserver-cmd 'sqlcmd)
      (format "%s -S %s -U %s -P %s -d %s -h-1  -w 65535  -s \"\^E\" -W"
              (symbol-name sqlserver-cmd)
              (cdr (assoc 'server-instance connection-info))
              (cdr (assoc 'username connection-info))
              (cdr (assoc 'password connection-info))
              (cdr (assoc 'dbname connection-info)))
    (format "%s -S %s -U %s -P %s -d %s -h-1  -n -w 65535 -s \"\" "
            ;;            "%s -S %s -U %s -P %s -d %s -h-1  -n -w 65535  -s \"\^E\""
            (symbol-name sqlserver-cmd)
            (cdr (assoc 'server-instance connection-info))
            (cdr (assoc 'username connection-info))
            (cdr (assoc 'password connection-info))
            (cdr (assoc 'dbname connection-info)))))


(defun sqlserver-query-read-connect-string()
  "set server dbname username password interactive"
  (let ((connection-info (copy-alist sqlserver-connection-info)))
    (setcdr  (assoc 'username connection-info)
             (read-string (format  "username(default:%s):"  (cdr (assoc 'username connection-info)))
                          "" nil   (cdr (assoc 'username connection-info))))
    (setcdr  (assoc 'password connection-info)
             (read-passwd "password:"  nil ))
    (setcdr  (assoc 'username connection-info)
             (read-string (format  "server-instance(default:%s):"  (cdr (assoc 'server-instance connection-info)))
                          "" nil (cdr (assoc 'server-instance connection-info))))
    (setcdr  (assoc 'dbname connection-info)
             (read-string (format  "dbname(default:%s):"  (cdr (assoc 'dbname connection-info)))
                          "" nil (cdr (assoc 'dbname connection-info))))
    connection-info))

(defun sqlserver-query-create-connection(connection-info)
  "open connection with sqlcmd.exe or osql.exe."
  (interactive (list (sqlserver-query-read-connect-string)))
  (let ((process (start-process-shell-command
                  (symbol-name sqlserver-cmd)
                  (concat " *sqlserver-query-" (number-to-string (random)) "*")
                  (concat  (sqlserver-format-connect-string connection-info)))))

    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc change)
       (when (string-match "\\(finished\\|exited\\|exited abnormally with code\\)" change)
         (message  (concat  (process-name proc) " exited"))
         (kill-buffer  (process-buffer proc)))))
    (list process
          (process-buffer process)
          connection-info)
    ;; (set-process-filter process 'sqlserver-filter-fun)
    ))
;; (sqlserver-query-create-connection sqlserver-connection-info)

;;;###autoload
(defun sqlserver-query-close-connection(connection)
  "close connection.kill sqlplus process and buffer ."
  (kill-process (nth 0 connection))
  (kill-buffer (nth 1 connection))
  (setq connection nil))

(defvar sqlserver-query-default-connection nil)

;;(sqlserver-query "select empno from emp")
;; (sqlserver-query "select empno from emp" (sqlserver-query-create-connection "scott/tiger"))
;;;###autoload
(defun sqlserver-query (sql &optional sqlserver-query-connection)
  "execute sql using `sqlcmd' or `osql' ,and return the result of it."
  (let( (connection sqlserver-query-connection) process)
    (unless connection
      (unless sqlserver-query-default-connection
        (setq sqlserver-query-default-connection (call-interactively 'sqlserver-query-create-connection)))
      (setq connection sqlserver-query-default-connection)
      )
    (setq process (car connection))
    (when (string-match "\\(.*\\);[ \t]*" sql)
      (setq sql (match-string 1 sql)))
    (with-current-buffer (process-buffer process)
      (delete-region (point-min) (point-max))
      (let ((start (point-min)) end)
        (goto-char (point-max))
        (process-send-string process (format "%s ;\n" sql))
        (process-send-string process  "go\n")
        (goto-char (point-min))
        (while (not (re-search-forward "(.*\\(行受影响\\|rows affected\\))" nil t 1))
          (when (accept-process-output process  sqlserver-timeout-wait-for-result 0 nil)
            (goto-char (point-min))))
        (setq end (1- (match-beginning 0)))
        (when (re-search-backward "\\bSQL> " nil t 1) (setq start (match-end 0))) ;FIXME
        (sqlserver-parse-result-as-list  (buffer-substring-no-properties start end))
        ))))

;; ;;;###autoload
;; (defun sqlserver-query-rebuild-connection()
;;   "rebuild connection."
;;   (interactive)
;;   (if (and   sqlserver-query-process
;;              (equal (process-status sqlserver-query-process ) 'run))
;;       (progn
;;         (process-send-string sqlserver-query-process  "go\n")
;;         (process-send-string sqlserver-query-process  "exit\n")
;;         (set-process-sentinel sqlserver-query-process
;;                               (lambda (proc change)
;;                                 (when (string-match "\\(finished\\|exited\\)" change)
;;                                   (sqlserver-query-create-connection)))))
;;     (sqlserver-query-create-connection)))



;; (defun sqlserver-query (sql)
;;   "geta result from the function `sqlserver-query-result-function'
;; after you call `sqlserver-query'"
;;   (unless (and (buffer-live-p (get-buffer  sqlserver-query-buffer))
;;            sqlserver-query-process
;;            (equal (process-status sqlserver-query-process ) 'run))
;;       (sqlserver-query-rebuild-connection))
;;   (when (string-match "\\(.*\\);[ \t]*" sql)
;;     (setq sql (match-string 1 sql)))
;;   (process-send-string sqlserver-query-process  (format "%s ;\n" sql))
;;   (process-send-string sqlserver-query-process  "go\n")
;;   (if (accept-process-output sqlserver-query-process  sqlserver-timeout-wait-for-result 0 nil)
;;       sqlserver-query-result
;;     nil))

;; (defun sqlserver-filter-fun (process output)
;;   (setq  sqlserver-query-result  ( sqlserver-parse-result-as-list  output)))

(provide 'sqlserver-query)
;;; sqlserver-query.el ends here

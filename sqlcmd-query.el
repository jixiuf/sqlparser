;;; sqlserver-query.el --- execute sql select using sqlserver on SQL SERVER. -*- coding:utf-8 -*-

;; Copyright (C) 2011 孤峰独秀

;; Author: 孤峰独秀  jixiuf@gmail.com
;; Keywords: sqlplus emacs sql

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

;;  execute sql using sqlplus and return as list .
;;  (sqlserver-query "select empno,ename from emp where empno<=7499")
;;  got : (("7369" "SMITH") ("7499" "ALLEN"))
;;

;; 2. you should custom these variable
;;  `sqlserver-username'
;;  `sqlserver-password'
;;  `sqlserver-server'
;;  `sqlserver-dbname'
;;  `sqlserver-port'
;;  `sqlserver-as-sysdba'
;; 3. call (sqlserver-query-init) to start a background sqlplus process

;;    value as the function name .this function must accept one parameter
;;    actually the parameter is the result after you call (sqlserver-query sql)
;; 5. call function `sqlserver-query'
;;
;; for example
;; (setq sqlserver-username "scott")
;; (setq sqlserver-password "tiger")
;; (setq sqlserver-server "localhost")
;; (setq sqlserver-dbname "orcl")
;; (setq sqlserver-port "1521")
;; (setq sqlserver-as-sysdba nil)

;; (sqlserver-query "select 1 from dual")
;; (sqlserver-query "select * from user_tables")



;;; Commands:
;;
;; Below are complete command list:
;;
;;  `sqlserver-query-rebuild-connection'
;;    rebuild sqlplus connection.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `sqlserver-username'
;;    sqlserver user name.
;;    default = "sa"
;;  `sqlserver-password'
;;    sqlserver user password.
;;    default = "sa"
;;  `sqlserver-server'
;;    Default server or host.
;;    default = "localhost"
;;  `sqlserver-dbname'
;;    database name .
;;    default = "SQLEXPRESS"
;;  `sqlserver-cmd'
;;    sqlserver cmd.now it support sqlcmd and osql
;;    default = (quote sqlcmd)

;;; Code:

(require 'sql)

(defcustom sqlserver-username "sa"
  "sqlserver user name."
  :group 'sqlparser
  :type 'string)
(defcustom sqlserver-password "sa"
  "sqlserver user password."
  :group 'sqlparser
  :type 'string)
(defcustom sqlserver-server "localhost"
  "Default server or host."
  :type 'string
  :group 'sqlparser
  :safe 'stringp)
(defcustom sqlserver-dbname "SQLEXPRESS"
  "database name ."
  :type 'string
  :group 'sqlparser
  :safe 'stringp)

(defcustom sqlserver-cmd 'sqlcmd
  "sqlserver cmd.now it support sqlcmd and osql
sqlserver add new cmd sqlcmd.exe.and osql is not recommended."
  :type '(choice (const sqlcmd) (const osql))
  :group 'sqlparser)


(defvar sqlserver-timeout-wait-for-result 300
  "waiting 300s for sql result returned.")

(defvar sqlserver-query-process nil)
(defvar sqlserver-query-result nil)


(defun sqlserver-parse-result-as-list (raw-result)
  (let  (result row line-count)
    (with-temp-buffer
      (insert raw-result)
      (setq line-count (count-lines (point-min) (point-max)))
      (goto-char  (point-min))
      (while (< (line-number-at-pos) (- line-count 1))
        (setq row (split-string (buffer-substring-no-properties
                                 (point-at-bol) (point-at-eol)) "" t))
        (setq result (append result (list row)))
        ;;            (add-to-list 'result row t)
        (forward-line))
      )result ))

(defun sqlserver-conn-str()
  "default:sqlcmd -S localhost -U sa -P sa -d SQLEXPRESS -h-1 -w 65535   -s \"\^E\" -W"
  (format "%s -S %s -U %s -P %s -d %s -h-1  -w 65535  -s \"\^E\" -W"
          (symbol-name sqlserver-cmd) sqlserver-server sqlserver-username sqlserver-password sqlserver-dbname))

(defun sqlserver-query-init-interactive()
  (interactive)
  (setq sqlserver-server (read-string (format  "sqlserver-server(default:%s):" sqlserver-server) "" nil sqlserver-server))
  (setq sqlserver-dbname (read-string (format  "sqlserver-server(default:%s):" sqlserver-dbname) "" nil sqlserver-dbname))
  (setq sqlserver-username (read-string (format  "sqlserver-username(default:%s):" sqlserver-username) "" nil sqlserver-username))
  (setq sqlserver-password (read-passwd "sqlserver-password:"  nil sql-password))
  (sqlserver-query-init))

(defun sqlserver-query-init()
  (setq sqlserver-query-process
        (start-process-shell-command "sqlserver" " *sqlserver-query*" (sqlserver-conn-str)))
  (set-process-filter sqlserver-query-process 'sqlserver-filter-fun))

;; (sqlserver-query-init-interactive)
;; (sqlserver-query "select name from sysobjects")

(defun sqlserver-query-rebuild-connection()
  "rebuild sqlplus connection."
  (interactive)
  (kill-process sqlserver-query-process)
  (sqlserver-query-init))

(defun sqlserver-query (sql)
  "geta result from the function `sqlserver-query-result-function'
after you call `sqlserver-query'"
  (when (or (not sqlserver-query-process)  (not (equal (process-status sqlserver-query-process ) 'run)))
    (sqlserver-query-init))
  (when (string-match "\\(.*\\);[ \t]*" sql)
    (setq sql (match-string 1 sql)))
  (process-send-string sqlserver-query-process  (format "%s ;\n" sql))
  (process-send-string sqlserver-query-process  "go\n")
  (if (accept-process-output sqlserver-query-process  sqlserver-timeout-wait-for-result 0 nil)
      sqlserver-query-result
    nil))

(defun sqlserver-filter-fun (process output)
  (setq  sqlserver-query-result  ( sqlserver-parse-result-as-list  output)))

(provide 'sqlserver-query)
;;; sqlserver-query.el ends here



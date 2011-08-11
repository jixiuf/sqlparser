;;; oracle-query.el --- execute sql select using sqlplus. -*- coding:utf-8 -*-

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
;;  (oracle-query "select empno,ename from emp where empno<=7499")
;;  got : (("7369" "SMITH") ("7499" "ALLEN"))
;;
;;  how to using this file
;; 1. first you should have installed oracle ,and start the listener.
;;    on linux ,run :
;;            lsnrctl start

;; 2. you should custom these variable
;;  `oq-username'
;;  `oq-password'
;;  `oq-server'
;;  `oq-dbname'
;;  `oq-port'
;;  `oq-as-sysdba'
;; 3. call (oracle-query-init) to start a background sqlplus process

;; 4. define a function ,and custom `oracle-query-result-function'
;;    value as the function name .this function must accept one parameter
;;    actually the parameter is the result after you call (oracle-query sql)
;; 5. call function `oracle-query'
;;
;; for example
;; (setq oq-username "scott")
;; (setq oq-password "tiger")
;; (setq oq-server "localhost")
;; (setq oq-dbname "orcl")
;; (setq oq-port "1521")
;; (setq oq-as-sysdba nil)

;; (oracle-query-init)
;; (defun handle-result (result)
;;   "just print the result"
;;   (message "handle the result after `oracle-query'")
;;   (print result))

;; (setq oracle-query-result-function 'handle-result)

;; (oracle-query "select 1 from dual")
;; (oracle-query "select * from user_tables")



;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `oq-username'
;;    oracle user name.
;;    default = "scott"
;;  `oq-password'
;;    oracle user password.
;;    default = "tiger"
;;  `oq-server'
;;    Default server or host.
;;    default = "localhost"
;;  `oq-dbname'
;;    database name .
;;    default = "orcl"
;;  `oq-port'
;;    Default port.
;;    default = 1521
;;  `oq-as-sysdba'
;;    login as sysdba.
;;    default = nil
;;  `oracle-query-result-function'
;;    when you call `oracle-query' and the result is returned ,it will
;;    default = (quote oq-default-result-funciont)

;;; Code:

(require 'sql)
(defcustom oq-username "scott"
  "oracle user name."
  :group 'sqlparse
  :type 'string)
(defcustom oq-password "tiger"
  "oracle user password."
  :group 'sqlparse
  :type 'string)
(defcustom oq-server "localhost"
  "Default server or host."
  :type 'string
  :group 'SQL
  :safe 'stringp)
(defcustom oq-dbname "orcl"
  "database name ."
  :type 'string
  :group 'SQL
  :safe 'stringp)

(defcustom oq-port 1521
  "Default port."
  :type 'number
  :group 'SQL
  :safe 'numberp)

(defcustom oq-as-sysdba nil
  "login as sysdba."
  :type 'boolean
  :group 'SQL
  :safe 'booleanp)
(defcustom oracle-query-result-function 'oq-default-result-funciont
  "when you call `oracle-query' and the result is returned ,it will
call this function"
  :type 'symbol
  :group 'SQL
  :safe 'symbolp)


(defvar oq-linesize 2000
  "Default linesize for sqlplus")

(defvar oracle-query-process nil)


(defun oq-parse-result-as-list (raw-result)
  (let  (result row)
    (with-temp-buffer
      (insert raw-result)
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n]*[ \t\n]*" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+" nil t)
        (replace-match "" nil nil))
      (goto-char  (point-min))
      (while (not (= (point-at-eol) (point-max)))
        (setq row (split-string (buffer-substring-no-properties (point-at-bol) (point-at-eol)) "" t))
        (setq result (append result (list row)))
        ;;            (add-to-list 'result row t)
        (forward-line) (beginning-of-line))
      )result ))

(defun oq-conn-str()
  " default:sqlplus scott/tiger@localhost:1521/orcl"
  (if osq-as-sysdba
      (format "sqlplus  %s/%s@%s:%s/%s as sysdba"
              oq-username oq-password oq-server oq-port oq-dbname)
    (format "sqlplus  %s/%s@%s:%s/%s"
            oq-username oq-password oq-server oq-port oq-dbname)
    ))
(defun oracle-query-init()
  (setq oracle-query-process (start-process-shell-command "sqlplus" " *oracle-query-sqlplus*" (oq-conn-str)))
  (process-send-string oracle-query-process "set heading off;\n")
  (process-send-string oracle-query-process (format  "set linesize %d;\n" 11111))
  (process-send-string oracle-query-process "set colsep '';\n");;column separater
  (process-send-string oracle-query-process "set null 'NULL';\n");;
  (process-send-string oracle-query-process "set wrap off;\n")
  (process-send-string oracle-query-process "set pagesize 0;\n")
  (process-send-string oracle-query-process "set feedback off;\n")
  (process-send-string oracle-query-process "set serveroutput on;\n")
  (set-process-filter oracle-query-process 'oq-filter-fun)
  )
(defun oracle-query-rebuild-connection()
  (interactive)
  (kill-process oracle-query-process)
  (oracle-query-init))

(defun oracle-query (sql)
  "get result from the function `oracle-query-result-function' after you call `oracle-query'"
  (when (string-match "\\(.*\\);[ \t]*" sql)
    (setq sql (match-string 1 sql)))
  (process-send-string oracle-query-process  (format "%s ;\n" sql)))

(defun oq-default-result-funciont(result)
  "the default function when result is ok."
  (print result))

(defun oq-filter-fun (process output)
  (unless (string= "SQL> " output)
    (funcall oracle-query-result-function ( oq-parse-result-as-list  output)) ))
(provide 'oracle-query)
;;; oracle-query.el ends here



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

;; (oracle-query "select 1 from dual")
;; (oracle-query "select * from user_tables")



;;; Commands:
;;
;; Below are complete command list:
;;
;;  `oracle-query-setup-interactive'
;;    populate some usful variables ,like user ,passwd,dbname.
;;  `oracle-query-rebuild-connection'
;;    rebuild sqlplus connection.
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

;;; Code:

(require 'sql)
(defcustom oq-username "scott"
  "oracle user name."
  :group 'sqlparser
  :type 'string)
(defcustom oq-password "tiger"
  "oracle user password."
  :group 'sqlparser
  :type 'string)
(defcustom oq-server "localhost"
  "Default server or host."
  :type 'string
  :group 'sqlparser
  :safe 'stringp)
(defcustom oq-dbname "orcl"
  "database name ."
  :type 'string
  :group 'sqlparser
  :safe 'stringp)

(defcustom oq-port 1521
  "Default port."
  :type 'number
  :group 'sqlparser
  :safe 'numberp)

(defcustom oq-as-sysdba nil
  "login as sysdba."
  :type 'boolean
  :group 'sqlparser
  :safe 'booleanp)

(defvar oq-timeout-wait-for-result 300
  "waiting 300s for sql result returned.")
(defvar oq-linesize 20000 "Default linesize for sqlplus")
(defvar oracle-query-process nil)
(defvar oracle-query-result nil)

;;;###autoload
(defun oracle-query-setup-interactive()
  "populate some usful variables ,like user ,passwd,dbname."
  (interactive)
  (setq oq-username (read-string  (format "(build conn for completing)username:(default:%s)" oq-username) "" nil oq-username))
  (setq oq-password  (read-passwd (format  "(build conn for completing)passwd:(default:%s)" oq-password)  nil oq-password))
  (setq oq-server   (read-string (format  "(build conn for completing)server:(default:%s)" oq-server)  nil oq-server))
  (setq oq-dbname   (read-string (format  "(build conn for completing)dbname:(default:%s)" oq-dbname)  nil oq-dbname))
  (setq oq-port     (read-string (format  "(build conn for completing)port:(default:%s)" oq-port)  nil oq-port))
  (if  (y-or-n-p  "login as sysdba?")
      (setq oq-as-sysdba t)
    (setq oq-as-sysdba nil)))

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
        (setq row (split-string (buffer-substring-no-properties
                                 (point-at-bol) (point-at-eol)) "" t))
        (setq result (append result (list row)))
        ;;            (add-to-list 'result row t)
        (forward-line) (beginning-of-line))
      )result ))

(defun oq-conn-str()
  " default:sqlplus scott/tiger@localhost:1521/orcl"
  (if oq-as-sysdba
      (format "sqlplus  %s/%s@%s:%s/%s as sysdba"
              oq-username oq-password oq-server oq-port oq-dbname)
    (format "sqlplus  %s/%s@%s:%s/%s"
            oq-username oq-password oq-server oq-port oq-dbname)
    ))
(defun oracle-query-init()
  (setq oracle-query-process
        (start-process-shell-command "sqlplus" " *oracle-query-sqlplus*" (oq-conn-str)))
  (process-send-string oracle-query-process "set heading off;\n")
  (process-send-string oracle-query-process (format  "set linesize %d;\n" oq-linesize))
  (process-send-string oracle-query-process "set colsep '';\n");;column separater
  (process-send-string oracle-query-process "set null 'NULL';\n");;
  (process-send-string oracle-query-process "set wrap off;\n")
  (process-send-string oracle-query-process "set pagesize 0;\n")
  (process-send-string oracle-query-process "set feedback off;\n")
  (process-send-string oracle-query-process "set serveroutput on;\n")
  (set-process-filter oracle-query-process 'oq-filter-fun)
  )
;;;###autoload
(defun oracle-query-rebuild-connection()
  "rebuild sqlplus connection."
  (interactive)
  (kill-process oracle-query-process)
  (oracle-query-init))

(defun oracle-query (sql)
  "geta result from the function `oracle-query-result-function'
after you call `oracle-query'"
  (when (or (not oracle-query-process)  (not (equal (process-status oracle-query-process ) 'run)))
    (oracle-query-init))
  (when (string-match "\\(.*\\);[ \t]*" sql)
    (setq sql (match-string 1 sql)))
  (process-send-string oracle-query-process  (format "%s ;\n" sql))
  (if (accept-process-output oracle-query-process  oq-timeout-wait-for-result 0 nil)
      oracle-query-result
    nil))

(defun oq-filter-fun (process output)
  (if  (or  (string-match "SQL> " output)
               (string-match "Copyright (c) .* Oracle.  All rights reserved." output)
               (string-match "Oracle Database 11g Enterprise Edition Release 11.2.0.1.0 - Production" output)
               )
      (setq  oracle-query-result nil)
    (setq  oracle-query-result  ( oq-parse-result-as-list  output))))

(provide 'oracle-query)
;;; oracle-query.el ends here



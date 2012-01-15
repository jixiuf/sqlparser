;;; mysql-query.el --- execute sql select using mysql. -*- coding:utf-8 -*-

;; Copyright (C) 2011 纪秀峰(Joseph)

;; Last Updated: Joseph 2012-01-15 17:24:26 星期日
;; Created: 2012-01-12 10:52
;; Version: 0.1.0
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: mysql sql emacs
;; Filename: mysql-query.el
;; Description:execute sql select using mysql.
;; URL:http://www.emacswiki.org/emacs/download/mysql-query.el
;; https://github.com/jixiuf/sqlparser
;; Compatibility: Test on Linux

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

;; execute sql using mysql and return as list .
;;
;;       (mysql-query "select empno,ename from emp where empno<=7499")
;; got :     (("7369" "SMITH") ("7499" "ALLEN"))
;; or    (mysql-query-with-heading)
;; got :     (("EMPNO" "ENAME") ("7369" "SMITH") ("7499" "ALLEN"))
;; using default connection ,not recommended.
;;
;; (defvar connection (mysql-query-create-connection "scott/tiger"))
;; (mysql-query "select empno from emp" connection)
;; recommended
;;
;; the normal way to use mysql-query.el is :
;; 1:
;; (defvar mysql-connection nil)
;; (unless (mysql-query-connection-alive-p c)
;;   (setq mysql-connection (call-interactively 'mysql-query-create-connection)))
;;
;; 2:
;;   (mysql-query "select empno from emp" mysql-connection)
;; 3:
;;   (mysql-query-close-connection mysql-connection)

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `mysql-connection-info'
;;    default mysql connection info .
;;    default = (quote ((host . "localhost") (username . "root") (password . "root") (dbname . "mysql") (port . "3306")))
;;  `mysql-command'
;;    default mysql command ,.
;;    default = "mysql"
;;  `mysql-command-other-options'
;;    default mysql connection info .
;;    default = (quote ("--column-names" "-s" "--unbuffered"))

;;
;;; Code:

(defgroup mysql-query nil
  "mysql query"
  :group 'SQL)



(defcustom mysql-connection-info
  '((host . "localhost")
    (username . "root")
    (password . "root")
    (dbname . "mysql")
    (port . "3306")
    )
  "default mysql connection info ."
  :group 'mysql-query
  :type 'alist)

(defcustom mysql-command
  "mysql"
  "default mysql command ,."
  :group 'mysql-query
  :type 'string)


(defcustom mysql-command-other-options
  '("--column-names" "-s"  "--unbuffered")
  "default mysql connection info ."
  :group 'mysql-query)

(defvar mysql-query-default-connection nil)
(defvar mysql-timeout-wait-for-result 300
  "waiting 300s for sql result returned.")

(defun mysql-query-read-connect-string()
  "set hostname dbname username password interactive"
  (let ((connection-info (copy-alist mysql-connection-info)))
    (setcdr  (assoc 'host connection-info)
             (read-string (format  "host(default:%s):"  (cdr (assoc 'host connection-info)))
                          "" nil (cdr (assoc 'host connection-info))))
    (setcdr  (assoc 'port connection-info)
             (read-string (format  "port(default:%s):"  (cdr (assoc 'port connection-info)))
                          "" nil   (cdr (assoc 'port connection-info))))
    (setcdr  (assoc 'username connection-info)
             (read-string (format  "username(default:%s):"  (cdr (assoc 'username connection-info)))
                          "" nil   (cdr (assoc 'username connection-info))))
    (setcdr  (assoc 'password connection-info)
             (read-passwd "password:"  nil ))
    (setcdr  (assoc 'dbname connection-info)
             (read-string (format  "dbname(default:%s):"  (cdr (assoc 'dbname connection-info)))
                          "" nil (cdr (assoc 'dbname connection-info))))
    (setq-default mysql-connection-info connection-info) ;;update default info
    connection-info))

(defun mysql-format-command-args (connection-info)
  "Returns a list of all the arguments for the mysql  program.
  default: mysql -h localhost -u root -proot -s  --database=zaiko "
  (apply 'list "-h" (cdr (assoc 'host connection-info))
         "-u" (cdr (assoc 'username connection-info))
         (concat "-p" (cdr (assoc 'password connection-info)))
         "-P" (cdr (assoc 'port connection-info))
                                        ;use tab as column separator char
         (concat "--database=" (cdr (assoc 'dbname connection-info)))
         mysql-command-other-options))


(defun mysql-query-raw (connection-info sql)
  "Returns a list of all the arguments for the mysql  program.
  default: mysql -h localhost -u root -proot -s  --database=zaiko -e"
  (let((result-buf " *mysql-query-reslut*")
       (result)
       )
    (when (buffer-live-p (get-buffer result-buf)) (kill-buffer result-buf))
    (setq result (apply 'call-process
                        "mysql" nil result-buf nil
                        (append (mysql-format-command-args connection-info) (list "--batch" "-e" sql))
                        ;; (list "-h" "localhost" "-u" "root" "-proot" "-P" "3306" "--database=mysql" "--column-names" "-s" "--unbuffered"  "-e" "select now();")
                        ))
    (if (= result 0)                    ;success
      (with-current-buffer result-buf
        (buffer-string))
      nil
      )
    )
  )
;; (mysql-query mysql-connection-info "select user from mysql.user")











(provide 'mysql-query)
;;; mysql-query.el ends here

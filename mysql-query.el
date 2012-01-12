;;; mysql-query.el --- execute sql select using mysql. -*- coding:utf-8 -*-

;; Copyright (C) 2011 纪秀峰(Joseph)

;; Last Updated: Joseph 2012-01-12 15:30:31 星期四
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

;;
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `mysql-query-create-connection'
;;    create a connection to mysql ,and return the
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
;;    default = (quote ("--column-names" "-s" " --unbuffered"))

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
  '("--column-names" "-s"  " --unbuffered")
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
    (setcdr  (assoc 'username connection-info)
             (read-string (format  "username(default:%s):"  (cdr (assoc 'username connection-info)))
                          "" nil   (cdr (assoc 'username connection-info))))
    (setcdr  (assoc 'port connection-info)
             (read-string (format  "port(default:%s):"  (cdr (assoc 'port connection-info)))
                          "" nil   (cdr (assoc 'port connection-info))))
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



(defun mysql-format-command-args-4-shell (connection-info)
  "Returns a list of all the arguments for the mysql  program.
  default: mysql -h localhost -u root -proot -s  --database=zaiko -e"
  (append (mysql-format-command-args connection-info) (list "--batch" "-e")))

;; (mysql-format-command-args-4-shell mysql-connection-info)
;; ("-h" "localhost" "-u" "sa" "-p" "--database=mysql" "--column-names" "-s" "-e")
(defun mysql-query-create-connection(connection-info)
  "create a connection to mysql ,and return the
created process"
  (interactive (list (mysql-query-read-connect-string)))
  (let* ((random-str (number-to-string (random)))
         (process
          (apply 'start-process ;;
                 (concat "mysql-query-" random-str)
                 (concat " *mysql-query-" random-str "*")
                 mysql-command
                 (mysql-format-command-args mysql-connection-info))
          ))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc change)
       (when (string-match "\\(finished\\|exited\\|exited abnormally with code\\)" change)
         (kill-buffer (process-buffer proc) )
         (message  (concat  (process-name proc) " exited")))))
    (list process
          (process-buffer process)
          connection-info)))

;; (mysql-query-create-connection mysql-connection-info)


(defun mysql-query-connection-alive-p(connection)
  "test whether the connection is alive."
  (and connection
       (listp connection)
       (processp (car connection))
       (bufferp (nth 1 connection))
       (buffer-live-p (nth 1 connection))
       (equal (process-status (car connection)) 'run)))

;;;###autoload
(defun mysql-query-close-connection(connection)
  "close connection.kill mysql process and buffer ."
  (when (mysql-query-connection-alive-p connection)
    (process-send-string (car connection)  "exit\n"))
  (sleep-for 0.1)
  (when (mysql-query-connection-alive-p connection)
    (kill-process (car connection))
    (kill-buffer (nth 1 connection)))
  )


;;(mysql-query "select empno from emp")
;; (mysql-query "select empno from emp" (mysql-query-create-connection "scott/tiger"))
;;;###autoload
(defun mysql-query (sql &optional mysql-query-connection)
  "execute sql using `mysql' ,and return the result of it."
  (cdr (mysql-query-with-heading sql mysql-query-connection)))


;; (defun mysql-query-with-heading (sql &optional mysql-query-connection)
;;   "execute sql using `mysql' ,and return the result of it."
;;   (let( (connection mysql-query-connection) process)
;;     (unless (mysql-query-connection-alive-p mysql-query-connection)
;;       (setq-default mysql-query-default-connection (call-interactively 'mysql-query-create-connection))
;;       (setq mysql-query-connection mysql-query-default-connection))
;;     (setq process (car connection))
;;     (when (string-match "\\(.*\\);[ \t\n\r]*" sql)
;;       (setq sql (match-string 1 sql)))
;;     (setq sql (replace-regexp-in-string "[\n\r]" " " sql))
;;     (with-current-buffer (process-buffer process)
;;       (delete-region (point-min) (point-max))
;;       (let ((start (point-min)) end)
;;         (goto-char (point-max))
;;         (process-send-string process (format "%s ;\n" sql))
;;         (goto-char (point-min))
;;         ;; (while (not (re-search-forward "^\\([0-9]+\\|no\\) rows? \\(selected\\|updated\\|deleted\\)" nil t 1))
;;         ;;   (when (accept-process-output process mysql-timeout-wait-for-result 0 nil)
;;         ;;     (goto-char (point-min))))
;;         ;; (if (not  (string=  (match-string 2) "selected"))
;;         ;;     (match-string 0)
;;         ;;   ;; (setq end (1- (match-beginning 0)))
;;         ;;   ;; (when (re-search-backward "\\bSQL> " nil t 1) (setq start (match-end 0)))
;;         ;;   ;; (oq-parse-result-as-list (buffer-substring start end))
;;         ;;   )
;;         ))))

;; (defun oq-parse-result-as-list (raw-result)
;;   (let (result row index-of-result mysql-query-heading)
;;     (with-temp-buffer
;;       (insert raw-result)
;;       (goto-char (point-min))
;;       (while (re-search-forward "[ \t\n]*[ \t\n]*" nil t)
;;         (replace-match "" nil nil))
;;       (goto-char (point-min))
;;       (while (re-search-forward "^[ \t]+" nil t)
;;         (replace-match "" nil nil))
;;       (goto-char (point-min))
;;       (forward-line 1)
;;       (setq mysql-query-heading
;;             (split-string (buffer-substring-no-properties
;;                            (point-at-bol) (point-at-eol)) "" t))
;;       (forward-line 2) (setq index-of-result 3)
;;       (while (not (= (point-at-eol) (point-max)))
;;         (unless (or (= 1  (% index-of-result oq-pagesize))
;;                     (= 2  (% index-of-result oq-pagesize))
;;                     (= 0 (% index-of-result oq-pagesize)))
;;           (setq row (split-string (buffer-substring-no-properties
;;                                    (point-at-bol) (point-at-eol)) "" t))
;;           (setq result (append result (list row))))
;;         (forward-line) (beginning-of-line)
;;         (setq index-of-result (1+ index-of-result)))
;;       )(cons mysql-query-heading result)))




(provide 'mysql-query)
;;; mysql-query.el ends here

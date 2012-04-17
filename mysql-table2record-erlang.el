;;; mysql-table2record-4erlang.el --- mysql table2record for erlang

;; Description:mysql table2record for erlang
;; Last Updated: Joseph 2012-04-17 17:31:31 星期二
;; Created: 2012年04月03日 星期二 17时14分44秒
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: mysql erlang record
;; https://github.com/jixiuf/sqlparser
;;  call command : (mysql-table2record-4erlang-interactively)

;; Copyright (C) 2011~2012 纪秀峰(Joseph) all rights reserved.

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

;; require mysql-query.el
;;  call command :erlang-mysql-generate-records
;; it will connect to a mysql intance ,and export all the tables to erlang records

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `erlang-mysql-generate-records'
;;    generate records from mysql tables
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'mysql-query)
(require 'erlang)


(defun erlang-mysql-tablename2record-name(tablename)
  "use table name as erlang record name defaul"
  tablename)

(defun erlang-mysql-columnname2fieldname(columnname)
  "use mysql column name as the field name of generated erlang record."
  columnname)

;; (camelize "hello_world") =="Hello_World"
;; (camelize "hello_world" "_") =="HelloWorld"
;; (camelize "HELLO_WORLD" "_") =="HelloWorld"
;; (camelize "helloworld") =="Helloworld"
(defun camelize (s &optional separator )
  "Convert under_score string S to CamelCase string."
  (mapconcat 'idrecord (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (if separator (split-string s "_") (list s))
                        ) ""))

;; (camelize-method "hello_world_everyone") =="hello_world_everyone"
;; (camelize-method "HELLO_WORLD_EVERYONE") =="hello_world_everyone"
;; (camelize-method "hello_world_everyone" "_") =="helloWorldEveryone"
;; (camelize-method "HELLO_WORLD_EVERYONE" "_")== "helloWorldEveryone"
(defun camelize-method (s &optional separator)
  "Convert under_score string S to camelCase string."
  (mapconcat 'idrecord (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (if separator (split-string s "_") (list s))) ""))

;; (upcase-first-char "hello") "Hello"
;; (upcase-first-char "HELLO") "HELLO"
;; (upcase-first-char "helloWorld") "HelloWorld"
;; (upcase-first-char "hello_world") "Hello_world"
;; (capitalize "hello_world") "Hello_World"
;; (upcase-first-char "helloWorld" "set") "setHelloWorld"
(defun upcase-first-char (s &optional prefix)
  "make the first char `upcase' and return (concat prefix upcasedstring)"
  (when  (>  (length s) 0)
    (let ( (first-char (substring s 0 1 ))
           (rest  (substring s  1 )))
      (concat (or prefix "") (upcase first-char) rest))))

;; (un-camelcase-string "helloWorld") == "hello_world"
;; (un-camelcase-string "helloWorld" "") == "helloworld"
(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is a hyphen \"-\".

    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defvar mysql-connection-4-mysql-erlang nil)

 ;; (erlang-mysql-query-all-tablename-in-db mysql-connection-4-mysql-erlang)
(defun erlang-mysql-query-all-tablename-in-db(mysql-connection-4-mysql-erlang)
  "query all table name from connected mysql `mysql-query.el'"
   (mysql-query
                (format "select table_name,table_comment from information_schema.tables where table_schema='%s'"
                        (cdr (assoc 'dbname mysql-connection-4-mysql-erlang)))
                mysql-connection-4-mysql-erlang)
  ;; (mapcar 'car)
   )

 ;; (erlang-mysql-query-table "user" mysql-connection-4-mysql-erlang)
(defun erlang-mysql-query-table (tablename mysql-connection-4-mysql-erlang)
  "query all column name and data type ."
  (mysql-query
   (format
    "SELECT column_name ,column_comment ,COLUMN_TYPE FROM information_schema.columns c WHERE c.table_schema = '%s' AND c.table_name = '%s' AND column_name NOT IN ( SELECT k.column_name FROM information_schema.KEY_COLUMN_USAGE k WHERE c.table_schema = k.table_schema AND c.table_name = k.table_name )"
    ;; "select column_name,column_comment from information_schema.columns where table_schema ='%s' and  table_name='%s' "
           (cdr (assoc 'dbname mysql-connection-4-mysql-erlang)) tablename)
   mysql-connection-4-mysql-erlang)
)

;; (erlang-mysql-generate-record "user" mysql-connection-4-mysql-erlang)
(defun erlang-mysql-generate-record(tablename-tablecomment mysql-connection-4-mysql-erlang)
  "generate all setter getter of `tablename'"
  (let (
        (col-type-alist (erlang-mysql-query-table (car tablename-tablecomment) mysql-connection-4-mysql-erlang))
         field-name)
    (with-temp-buffer
      (insert "-record(" )
      (insert (format "%s " ( erlang-mysql-tablename2record-name (car tablename-tablecomment)) ))
      (insert (format ",{%% %s\n"  (nth 1 tablename-tablecomment)) )
      (dolist (colomnname-type-item (cdr  col-type-alist))
        (setq field-name (erlang-mysql-columnname2fieldname (car colomnname-type-item)))
        (insert (format "%s, %% %s \n" field-name (nth 1 colomnname-type-item)))
        )
        (setq field-name (erlang-mysql-columnname2fieldname (car (car col-type-alist))))
        (insert (format "%s %% %s \n" field-name (nth 1 (car col-type-alist))))
      (insert "}).\n")
      (buffer-string)
      )
    )
  )

;;;###autoload
(defun erlang-mysql-generate-records()
  "generate records from mysql tables"
  (interactive)
  (let ((mysql-connection-4-mysql-erlang (call-interactively 'mysql-query-create-connection)))
    (switch-to-buffer  "*erlang records*")
    (with-current-buffer "*erlang records*"
      (erase-buffer)
      (erlang-mode)
      (dolist (tablname-table-comment   (erlang-mysql-query-all-tablename-in-db mysql-connection-4-mysql-erlang))
        (insert (erlang-mysql-generate-record tablname-table-comment mysql-connection-4-mysql-erlang) "\n")
        )
      (indent-region (point-min) (point-max))
      )
    ))

(provide 'mysql-table2record-erlang)

;; Local Variables:
;; coding: utf-8
;; End:

;;; mysql-table2record-erlang.el ends here

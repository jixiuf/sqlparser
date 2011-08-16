;; -*- coding:utf-8 -*-
;;; sqlparser-sqlserver-complete.el --- complete tablename and columnname when editing sql

;; Copyright (C) 2011 孤峰独秀

;; Author: 孤峰独秀  jixiuf@gmail.com
;; Keywords: sql complete sqlserver

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

;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:
(defun sqlparser-sqlserver-tablename-schemaname-databasename-candidates ()
"complete table name or schemanmae or databasename ."
  (let* ((prefix (sqlparser-get-prefix)); maybe master.dbo.tablename ,dbo.tablename tablename [dbo].[tablename]
         (sub-prefix (split-string prefix "\\." nil))
         sql result)
    (cond ( (= (length sub-prefix) 1)
            (let* ( (tablefullname (sqlparser-sqlserver-split-three  (car sub-prefix)) )
                    ( tablename-prefix (car tablefullname))
                    (tablename-suffix  (nth 2 tablefullname))
                    (tablename  (nth 1 tablefullname)))
              (setq sql (format "select '%s'+  name  + '%s' from sys.tables where name like %s%%"
                                tablename-prefix  tablename-suffix tablename))
              ))


          ( (= (length sub-prefix) 2); maybe databasename.schename or schename.tablename
            (let* ((fullname1 (sqlparser-sqlserver-split-three (car sub-prefix)))
                    (name1-prefix (car fullname1))
                    (name1 (nth 1 fullname1))
                    (name1-suffix  (if (equal "[" name1-prefix) "]" "" ))
                    (fullname2 (sqlparser-sqlserver-split-three (nth 1 sub-prefix)))
                    (name2-prefix (car fullname2))
                    (name2 (nth 1 fullname2))
                    (name2-suffix  (if (equal "[" name2-prefix) "]" "" ))
                    (db-exists
                     (>  (caar (sqlserver-query
                                (format  "select count(name) from sys.databases where name='%s'" name1) )) 0)))
              (if db-exists
                  (setq sql (format
                             "(select dbname_tab.dbname + '.' + schemaname_tab.schemaname  + '.' from  (select '%s' + name + '%s' dbname from sys.databases where name ='%s') as dbname_tab, (select '%s' + name + '%s' schemaname from [%s].[sys].[schemas] where name like '%s%%') schemaname_tab) union all (select schemaname_tab2.schemaname + '.' + tablename_tab.tablename from (select '%s' + name + '%s' schemaname from sys.schemas where name = '%s' ) schemaname_tab2, (select '%s' + name +'%s' tablename from sys.tables where name like '%s%%' ) tablename_tab )"
                             name1-prefix name1-suffix name1 name2-prefix name2-suffix name1 name2
                             name1-prefix name1-suffix name1 name2-prefix name2-suffix name2))
                (setq sql (format
                           "select schemaname_tab2.schemaname + '.' + tablename_tab.tablename from (select '%s' + name + '%s' schemaname from sys.schemas where name = '%s' ) schemaname_tab2, (select '%s' + name +'%s' tablename from sys.tables where name like '%s%%' ) tablename_tab "
                           name1-prefix name1-suffix name1 name2-prefix name2-suffix name2))
                  )
              )
              )
          ( (= (length sub-prefix) 3)
            (let* ((fullname1 (sqlparser-sqlserver-split-three (car sub-prefix)))
                   (name1-prefix (car fullname1))
                   (name1 (nth 1 fullname1))
                   (name1-suffix  (if (equal "[" name1-prefix) "]" "" ))
                   (fullname2 (sqlparser-sqlserver-split-three (nth 1 sub-prefix)))
                   (name2-prefix (car fullname2))
                   (name2 (nth 1 fullname2))
                   (name2-suffix  (if (equal "[" name2-prefix) "]" "" ))
                   (name3-prefix (car fullname3))
                   (name3 (nth 2 fullname3))
                   (name3-suffix  (if (equal "[" name3-prefix) "]" "" ))
                   )
              (setq sql (format "select dbname_tab.dbname + '.' + schemaname_tab.schemaname  + '.' tablename_tab.tablename from ( SELECT '%s' + name + '%s' dbname FROM sys.databases WHERE name = '%s' ) AS dbname_tab, ( SELECT '%s' + name + '%s' schemaname FROM sys.schemas WHERE name = '%s' ) schemaname_tab ,( SELECT '%s' + name + '%s' tablename FROM sys.tables WHERE name LIKE '%s%%' ) tablename_tab"
                                name1-prefix name1-suffix name1 name2-prefix name2-suffix name2 name3-prefix name3-suffix name3
                                ))

              )
            )

          )

    (mapcar 'car (sqlserver-query sql))
    )
  )

;;(sqlparser-sqlserver-split-three "[abc]") return ("[" "abc" "]")
;;(sqlparser-sqlserver-split-three "[abc") return ("[" "abc" "")
;;(sqlparser-sqlserver-split-three "abc") return ("" "abc" "")
(defun sqlparser-sqlserver-split-three(str)
  "when str=[abc] ,then return '(\"[\" \"abc\" \"]\" )
   when str=[abc  ,then return '(\"[\" \"abc\" \"\")
   when str=abc   ,then return '(\"\" \"abc\" \"\")"
  (let ( (prefix "") ( suffix "") (middle str))
    (when (string-match "^\\[" middle)
      (setq prefix "[")
      (setq middle  (substring-no-properties middle 1)))
    (when (string-match "]" middle)
      (setq suffix "]")
      (setq middle (substring-no-properties str 1   (length middle))))
    (list prefix  middle suffix)
    )
  )

(defun sqlparser-sqlserver-all-tablename-candidates()
  (mapcar 'car (sqlserver-query "select name from sys.tables")))

(defun sqlparser-sqlserver-all-databasename-candidates()
  "for example :return (master msdb tempdb)"
  (mapcar 'car (sqlserver-query "select name from sys.databases")))

;; master.dbo.tablename  databasename.schemaname.tablename
(defun sqlparser-sqlserver-schemaname-candidates()
  "for example :return (dbo sys db_owner)."
  (mapcar 'car (sqlserver-query "  select name from sys.schemas")))

(defun sqlparser-get-prefix()
  "for example `tablename.col' `table.' `str'"
  (let ((init-pos (point)) prefix)
    (when (search-backward-regexp "[ \t,(;]+" (point-min) t)
      (setq prefix (buffer-substring (match-end 0) init-pos)))
    (goto-char init-pos)
    (or prefix "")
    ))

(defun sqlparser-word-before-point()
  "get word before current point or empty string."
  (save-excursion
    (let ((current-pos (point)))
      (if (search-backward-regexp "\s-\\|[ \t]+\\|\\.\\|," (point-at-bol) t )
          (buffer-substring-no-properties (match-end 0) current-pos )
        ""))))

;; (defun abcd ()
;;    "thisandthat."
;;    (interactive)
;;    (print   (sqlparser-word-before-point))
;;    )

(provide 'sqlparser-sqlserver-complete)
;;; sqlparser-sqlserver-complete.el ends here



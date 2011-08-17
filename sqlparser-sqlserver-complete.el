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
;;  `abcd'
;;    thisandthat.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'sqlserver-query)

;;( sqlparser-string-replace "abc_" "_" "[_]")
(defun sqlparser-string-replace(str from-string replace-string)
  "replace all `from-string' in `str' with `replace-string'"
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward from-string nil t)
      (replace-match replace-string nil t))
    (buffer-string)))

;; Test
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "")
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "d")
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "[d")
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "m")

;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "[master].[dbo")
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "[master].dbo")
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "master.db_o")
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "dbo.spt_f")
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "master.dbo.spt")
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "master.dbo.[spt")
;; ( sqlparser-sqlserver-tablename-schemaname-databasename-candidates "[master].dbo.[spt")

(defun sqlparser-sqlserver-tablename-schemaname-databasename-candidates (&optional prefix)
  "complete table name or schemanmae or databasename ."
  (let* ((prefix (or prefix  (sqlparser-get-prefix))); maybe master.dbo.tablename ,dbo.tablename tablename [dbo].[tablename]
         (sub-prefix (split-string prefix "\\." nil))
         sql result)
    (cond ( (= (length sub-prefix) 1)
            (let* ( (tablefullname (sqlparser-sqlserver-split-three  (car sub-prefix)) )
                    ( name-prefix (car tablefullname))
                    (name-suffix   (if (equal "[" name-prefix) "]" "" ))
                    (name  (nth 1 tablefullname)))
              (setq sql (format "select '%s'+  name  + '%s' from sys.tables where name like '%s%%' union all select '%s' + name + '%s.' from sys.databases where name like '%s%%' union all select '%s' + name + '%s.' from sys.schemas where name like '%s%%'"
                                name-prefix  name-suffix  (sqlparser-string-replace name "_" "[_]")
                                name-prefix  name-suffix (sqlparser-string-replace name "_" "[_]")
                                name-prefix  name-suffix (sqlparser-string-replace name "_" "[_]")))))

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
                    (>  (string-to-number (caar (sqlserver-query
                                                 (format  "select count(name) from sys.databases where name='%s'" name1) ))) 0)))
              (if db-exists
                  (setq sql (format
                             "(select dbname_tab.dbname + '.' + schemaname_tab.schemaname  + '.' from  (select '%s' + name + '%s' dbname from sys.databases where name ='%s') as dbname_tab, (select '%s' + name + '%s' schemaname from [%s].[sys].[schemas] where name like '%s%%') schemaname_tab) union all (select schemaname_tab2.schemaname + '.' + tablename_tab.tablename from (select '%s' + name + '%s' schemaname from sys.schemas where name = '%s' ) schemaname_tab2, (select '%s' + name +'%s' tablename from sys.tables where name like '%s%%' ) tablename_tab )"
                             name1-prefix name1-suffix name1 name2-prefix name2-suffix name1 (sqlparser-string-replace name2 "_" "[_]")
                             name1-prefix name1-suffix name1 name2-prefix name2-suffix (sqlparser-string-replace name2 "_" "[_]")))
                (setq sql (format
                           "select schemaname_tab2.schemaname + '.' + tablename_tab.tablename from (select '%s' + name + '%s' schemaname from sys.schemas where name = '%s' ) schemaname_tab2, (select '%s' + name +'%s' tablename from sys.tables where name like '%s%%' ) tablename_tab "
                           name1-prefix name1-suffix name1 name2-prefix name2-suffix (sqlparser-string-replace name2 "_" "[_]")))
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
                   (fullname3 (sqlparser-sqlserver-split-three (nth 2 sub-prefix)))
                   (name3-prefix (car fullname3))
                   (name3 (nth 1 fullname3))
                   (name3-suffix  (if (equal "[" name3-prefix) "]" "" ))
                   )
              (setq sql (format "select dbname_tab.dbname + '.' + schemaname_tab.schemaname  + '.' + tablename_tab.tablename from ( SELECT '%s' + name + '%s' dbname FROM sys.databases WHERE name = '%s' ) AS dbname_tab, ( SELECT '%s' + name + '%s' schemaname FROM sys.schemas WHERE name = '%s' ) schemaname_tab ,( SELECT '%s' + name + '%s' tablename FROM sys.tables WHERE name LIKE '%s%%' ) tablename_tab"
                                name1-prefix name1-suffix name1 name2-prefix name2-suffix name2 name3-prefix name3-suffix (sqlparser-string-replace name3 "_" "[_]")
                                )))))
    (mapcar 'car (sqlserver-query sql))))

;;(sqlparser-sqlserver-split-three "[abc]") return ("[" "abc" "]")
;;(sqlparser-sqlserver-split-three "[abc") return ("[" "abc" "")
;; (sqlparser-sqlserver-split-three "abc") return ("" "abc" "")
;; (sqlparser-sqlserver-split-three "") return ("" "" "")
(defun sqlparser-sqlserver-split-three(str)
  "when str=[abc] ,then return '(\"[\" \"abc\" \"]\" )
   when str=[abc  ,then return '(\"[\" \"abc\" \"\")
   when str=abc   ,then return '(\"\" \"abc\" \"\")"
  (if  (or (null str) (string= str ""))
      (list "" "" "")
    (let ( (prefix "") ( suffix "") (middle str))
      (when (string-match "^\\[" middle)
        (setq prefix "[")
        (setq middle  (substring-no-properties middle 1)))
      (when (string-match "]" middle)
        (setq suffix "]")
        (setq middle (substring-no-properties str 1   (length middle))))
      (list prefix  middle suffix)
      )))

;; Test:
;; (sqlparser-sqlserver-get-matched-columns "t3" "s")
;; (sqlparser-sqlserver-get-matched-columns "[t3]" "[s")
;; (sqlparser-sqlserver-get-matched-columns "[t3]" )
;; (sqlparser-sqlserver-get-matched-columns "dbo.t3" "[s" )
;; (sqlparser-sqlserver-get-matched-columns "dbo.t3" "s" )
;; (sqlparser-sqlserver-get-matched-columns "dbo.t3" "" )
;; (sqlparser-sqlserver-get-matched-columns "master.dbo.dt" "t" )
;; (sqlparser-sqlserver-get-matched-columns "master.[dbo].dt" "[t" )
(defun sqlparser-sqlserver-get-matched-columns( tablename &optional column-prefix)
  "tablename can be [master].[dbo].test ,or test or [dbo].test"
  (let* (;;("[master]" "dbo" "test") or ("dbo" "test") or ("test")
         (tablename-string-list (split-string tablename "[ \t\\.]" t))
         sql
         (column-prefix-string (nth 1 (sqlparser-sqlserver-split-three  (or  column-prefix ""))))
         (column-prefix-prefix (car  (sqlparser-sqlserver-split-three  (or  column-prefix ""))))
         (column-prefix-sufix    (if (equal "[" column-prefix-prefix) "]" "" ))
         )
    (cond ( (= (length tablename-string-list) 1);tablename
            (setq sql (format "select '%s' + col.name + '%s' from sys.objects obj, sys.tables tab , sys.columns col where tab.[object_id]=obj.[object_id] and col.[object_id]=obj.[object_id] and tab.name ='%s'  and col.name like '%s%%'"
                              column-prefix-prefix column-prefix-sufix
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)))
                              column-prefix-string))

            )
          ( (= (length tablename-string-list) 2); schema.tablename
            (setq sql (format "select '%s' + col.name + '%s' from sys.objects obj, sys.tables tab , sys.columns col where tab.[object_id]=obj.[object_id] and col.[object_id]=obj.[object_id] and SCHEMA_NAME(tab.schema_id)='%s' and tab.name ='%s'  and col.name like '%s%%'"
                              column-prefix-prefix column-prefix-sufix
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)))
                              (nth 1 (sqlparser-sqlserver-split-three  (nth 1  tablename-string-list)))
                              column-prefix-string))
            )
          ( (= (length tablename-string-list) 3); dbname.schema.tablename
            (setq sql (format "select '%s' + col.name + '%s' from %s.sys.objects obj, %s.sys.tables tab , %s.sys.columns col where tab.[object_id]=obj.[object_id] and col.[object_id]=obj.[object_id] and SCHEMA_NAME(tab.schema_id)='%s' and tab.name ='%s'  and col.name like '%s%%'"
                              column-prefix-prefix column-prefix-sufix
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)));db
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)));db
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)));db
                              (nth 1 (sqlparser-sqlserver-split-three  (nth 1 tablename-string-list)));schema
                              (nth 1 (sqlparser-sqlserver-split-three  (nth 2  tablename-string-list)));table
                              column-prefix-string))))
    (mapcar 'car (sqlserver-query sql))))

(defun  sqlparser-sqlserver-column-candidates ()
  "column name candidates of table in current sql "
  (let* ((sql "select column_name from user_tab_columns where 1=0")
         (prefix (sqlparser-get-prefix)) ;alias.columnname or columnname
         (sub-prefix (split-string prefix "\\." nil)) ;(alias columnname)
         result)
    (cond ( (= (length sub-prefix) 1);;columnname
            (let ((table-names (sqlparser-fetch-tablename-from-select-sql
                                (sqlparser-sql-sentence-at-point)))
                  )
              (dolist (tablename table-names)
                (setq result (append result (list  (sqlparser-sqlserver-get-matched-columns tablename-string (car sub-prefix)))))
                )))
          ((= (length sub-prefix) 2); alias.columnname
           (let ((tablename-string  (sqlparser-guess-table-name (car sub-prefix))) ;[master].dbo.test
                 )
             (setq result (sqlparser-sqlserver-get-matched-columns tablename-string (nth 1 sub-prefix)))))
          )
    result))

(defun sqlparser-sqlserver-all-tablename-candidates()
  (mapcar 'car (sqlserver-query "select name from sys.tables")))

(defun sqlparser-sqlserver-all-databasename-candidates()
  "for example :return (master msdb tempdb)"
  (mapcar 'car (sqlserver-query "select name from sys.databases")))

;; master.dbo.tablename  databasename.schemaname.tablename
(defun sqlparser-sqlserver-schemaname-candidates()
  "for example :return (dbo sys db_owner)."
  (mapcar 'car (sqlserver-query "  select name from sys.schemas")))

(defun sqlparser-guess-table-name (alias &optional sql1)
  "find out the true table name depends on the alias.
suppose the sql is `select * from user u where u.age=11'
then the `u' is `alias' and `user' is the true table name."
  (let ((sql  (or sql1 (sqlparser-sql-sentence-at-point)))
        (regexp (concat  "\\(\\([a-zA-Z0-9_\\$\\.]\\|\\[\\|]\\)+\\)[ \t\n\r]+\\(as[ \t]+\\)?" alias "[, \t\n\r]\\|$"))
        table-name)
    (setq sql (concat sql " "))
    (setq sql (sqlparser-string-replace sql "\n" " "))
    (if (and  sql (string-match regexp sql))
        (progn
          (setq table-name (match-string 1 sql))
          (if (string-equal "from" table-name) alias table-name))
      alias)))


(defun sqlparser-fetch-tablename-from-sql ( &optional sql1)
  "return a list of tablenames from a sql-sentence."
  (let ((sql (or sql1 (sqlparser-sql-sentence-at-point)))
        tablenames)
    (setq tablenames (sqlparser-fetch-tablename-from-select-sql sql))
    (unless tablenames
      (setq tablenames (append tablenames (list (sqlparser-fetch-tablename-from-insert-update-alter-sql sql)))))
    tablenames
    ))

(defun sqlparser-fetch-tablename-from-insert-update-alter-sql( &optional sql1)
  "fetch tablename ,or schema.tablename from a insert sentence or
update sentence or alter sentence."
  (let ((sql (or sql1 (sqlparser-sql-sentence-at-point)))
        tablename)
    (with-temp-buffer
      (insert sql)
      (goto-char (point-min))
      (when (search-forward-regexp "\\(\\binto\\|update\\|alter\\)[ \t\n\r]+\\(\\([a-zA-Z0-9_\\$\\.]\\|\\[\\|]\\)+\\)[ \t\n\r]*" (point-max ) t)
        (setq tablename (match-string 2))))))


(defun sqlparser-fetch-tablename-from-select-sql ( &optional sql1)
  "return a list of tablenames from a sql-sentence."
  (let* ((sql (or sql1 (sqlparser-sql-sentence-at-point)))
         (sql-stack (list sql)) ele pt result-stack tablename-stack )
    (while (> (length sql-stack) 0)
      (setq ele (pop sql-stack))
      (with-temp-buffer
        (insert ele)
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t\n\r]*(" (point-max) t)
          (forward-char -1)
          (setq pt (point))
          (forward-sexp)
          (push (buffer-substring (1+ pt)  (1- (point))) sql-stack)
          (delete-region  pt (point))
          (insert "table"))
        (push (buffer-substring (point-min) (point-max))  result-stack)
        ))
    (while (> (length result-stack) 0)
      (setq ele (pop result-stack))
      (with-temp-buffer
        (insert ele)
        (goto-char (point-min))
        (when  (search-forward-regexp "[ \t\n\r]+from[ \t\n\r]+" (point-max) t)
          (delete-region (point-min) (point))
          (when (search-forward-regexp "[ \t\n\r]+where[ \t\n\r]+" (point-max) t)
            (backward-word)
            (delete-region (point) (point-max)))
          (goto-char (point-min))
          (while (search-forward-regexp "," (point-max) t)
            (push (buffer-substring 1 (1- (point))) tablename-stack)
            (delete-region  1 (point))
            )
          (push (buffer-substring (point-min) (point-max)) tablename-stack)
          )
        )
      )
    (while (> (length tablename-stack) 0)
      (setq ele (pop tablename-stack))
      (with-temp-buffer
        (insert ele)
        (goto-char (point-min))
        (replace-regexp "\n" " ")
        (goto-char (point-min))
        (replace-regexp "[ \t]+as[ \t]+" " ")
        (goto-char (point-min))
        (delete-horizontal-space)
        (goto-char (point-max))
        (delete-horizontal-space)

        (if (= 1  (count-matches   "\\([a-zA-Z0-9_\\$\\.]\\|\\[\\|]\\)+" 1 (point-max)))
            (push (buffer-substring 1 (point-max)) result-stack)
          (goto-char 0)
          (when (search-forward-regexp "\\([a-zA-Z0-9_\\$\\.]\\|\\[\\|]\\)+" (point-max) t )
            (push (match-string 0) result-stack)
            )
          )
        )
      )
    (delete "table" result-stack)
    result-stack
    ))

(defun sqlparser-parse()
  "judge now need complete tablename or column name or don't complete .
it will return 'table' ,or 'column' ,or nil.
"
  (let* ((cur-pos (point))
         (sql-pos-info (bounds-of-sql-at-point))
         (sql-start-pos (car sql-pos-info ))
         (sql-end-pos (cdr sql-pos-info))
         map keyword returnVal)
    (when (search-backward-regexp "\\balter\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "alter") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bfrom\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "from") map))
    (when (search-backward-regexp "\\bdesc\\b\\|\\bdescribe\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "desc") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bupdate\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "update") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bselect\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "select") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bset\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "set") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bwhere\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "where") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bvalues\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "values") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\binto\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "into") map))
    (goto-char cur-pos)
    (setq map   (sort map (lambda (a b ) (when (< (car a ) (car b)) t))))
    (setq keyword  (car (cdar map)))
    (cond
     ( (null keyword)
       (setq returnVal nil)
       )
     ((string= "into" keyword)
      (progn
        ;; '(' between "into" and current position
        (if (search-backward-regexp (regexp-quote "(") (- cur-pos  (caar map)) t 1)
            (setq returnVal "column")
          (setq returnVal "table")
          )
        )
      )
     ((string-match "from\\|alter\\|update\\|desc\\|describe" keyword)
      (setq returnVal "table")
      )
     ((string-match "select\\|set\\|where\\|" keyword)
      (setq returnVal "column")
      )
     ((string-match "values" keyword)
      (setq returnVal nil.)
      )
     (t
      (setq returnVal nil)
      )
     )
    (goto-char cur-pos)
    returnVal
    ))

(defun sqlparser-sql-sentence-at-point()
  "get current sql sentence. "
  (let* ((bounds (bounds-of-sql-at-point))
         (beg (car bounds))
         (end (cdr bounds)))
    (buffer-substring-no-properties  beg end)
    ))

(defun bounds-of-sql-at-point()
  "get start and end point of current sql."
  (let ((pt (point))begin end empty-line-p empty-line-p next-line-included tail-p)
    (when (and
           (looking-at "[ \t]*\\(\n\\|\\'\\)")
           (looking-back "[ \t]*;[ \t]*" (beginning-of-line))
           )
      (search-backward-regexp "[ \t]*;[ \t]*" (beginning-of-line) t)
      )
    (save-excursion
      (skip-chars-forward " \t\n\r")
      (re-search-backward ";[ \t\n\r]*\\|\\`\\|\n[\r\t ]*\n[^ \t]" nil t)
      (setq begin (point)))
    (save-excursion
      (skip-chars-forward " \t\n\r")
      (re-search-forward "\n[\r\t ]*\n[^ \t]\\|\\'\\|[ \t\n\r]*;" nil t)
      (unless (zerop (length (match-string 0)))
        (backward-char 1))
      (skip-syntax-backward "-")
      (setq end   (point)))
    (goto-char pt)
    (cons begin end)
    )
  )

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

(defun abcd ()
  "thisandthat."
  (interactive)
  (print (   sqlparser-sqlserver-column-candidates ))
  )

;;( sqlparser-string-replace "abc_" "_" "[_]")
(defun sqlparser-string-replace(str from-string replace-string)
  "replace all `from-string' in `str' with `replace-string'"
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward from-string nil t)
      (replace-match replace-string nil t))
    (buffer-string)))

(provide 'sqlparser-sqlserver-complete)
;;; sqlparser-sqlserver-complete.el ends here



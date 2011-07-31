;;; sqlparser-oracle-complete.el --- completing tablename,column name for oracle. -*- coding:utf-8 -*-

;; Copyright (C) 2011 孤峰独秀

;; Author: 孤峰独秀  jixiuf@gmail.com
;; Keywords: sql parse oracle
;; Filename: sqlparser-oracle-complete.el
;; Description:  completing tablename column for oracle when editing
;; Created: 2011年07月31日 星期日 20时37分31秒
;; Version: 0.1.0
;; URL:http://www.emacswiki.org/emacs/down/sqlparser-oracle-complete.el
;;
;; screencast :
;; Compatibility: Test on Linux
;;
;; Features that might be required by this library:
;; `anything'
;;
;; Features  that be required by this library
;; `oracle-shell-query.el'
;; http://www.emacswiki.org/emacs/download/oracle-shell-query.el
;;

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
;; it can parsing current sql sentence ,it is smart enough to choose to
;; completing table name or column name depending on your position.
;; 1 after keyword 'use'   :complete schema name (on mysql)
;; 2 after keyword 'select' 'set' 'where'    :complete  columnname.
;; 3 after keyword 'alter', 'from' 'update' 'desc'  'show' : complete tablename
;; 4 after keyword 'into' and and there isn't a
;; "\\(" between 'into' and current postion :complete tablename
;; 4.1 after keyword 'into' but there is a "(" between 'into' and current
;; position  :complete columnname
;; 5 after keyword 'values'  :complete nothing.
;;

;;; Installation:
;;
;; 1 it required oracle.el you should download and add it to you load-path.
;; http://www.emacswiki.org/emacs/download/oracle-shell-query.el

;; 2 add sqlparser-oracle-complete.el to you load-path
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;; (eval-after-load 'sql
;;   '(progn
;;      (require 'sqlparser-oracle-complete)
;;      (defun sqlparser-setup-for-oracle()
;;        "initial some variable .some is defined in oracle.el.
;;         some is defined here."
;;        (interactive)
;;        (setq osq-username "scott")
;;        (setq osq-password "tiger")
;;        (setq osq-server   "localhost")
;;        (setq osq-dbname   "orcl")
;;        (setq osq-port   "1521")

;;        )
;;      (sqlparser-setup-for-oracle)
;;      (define-key sql-mode-map (quote [tab]) 'anything-oracle-complete)
;;      (define-key sql-interactive-mode-map  (quote [tab]) 'anything-oracle-complete)
;;      (define-key sqlplus-mode-map  (quote [tab]) 'anything-oracle-complete)
;;      )
;;   )
;; if you don't want to use this function
;; you can call (sqlparser-setup-for-oracle-interactive)
;;
;; 3 define key bindings for complete .you have two choice .
;;  1). if you using anything.el  you can binding it like this .

;;      (define-key sql-mode-map (quote [tab]) 'anything-oracle-complete)
;;      (define-key sql-interactive-mode-map  (quote [tab]) 'anything-oracle-complete)

;;  2). use Emacs default completing system.
;;
;;      (define-key sql-mode-map (quote [tab]) 'sqlparser-oracle-complete)
;;      (define-key sql-interactive-mode-map  (quote [tab]) 'sqlparser-oracle-complete)
;;


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `sqlparser-oracle-setup-interactive'
;;    populate some usful variables ,like user ,passwd,dbname.
;;  `sqlparser-oracle-complete'
;;    complete tablename or column name depending on current point
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:
(require 'sql)
(require 'oracle-shell-query)
(require 'anything nil t)

(defgroup sqlparser nil
  "SQL-PARSE"
  :group 'tools
  )

(defun sqlparser-oracle-setup-interactive()
  "populate some usful variables ,like user ,passwd,dbname."
  (interactive)
  (setq osq-username (read-string  (format "(build conn for completing)username:(default:%s)" osq-username) "" nil osq-username))
  (setq osq-password  (read-passwd (format  "(build conn for completing)passwd:(default:%s)" osq-password)  nil osq-password))
  (setq osq-server   (read-passwd (format  "(build conn for completing)server:(default:%s)" osq-server)  nil osq-server))
  (setq osq-dbname   (read-passwd (format  "(build conn for completing)dbname:(default:%s)" osq-dbname)  nil osq-dbname))
  (setq osq-port   (read-passwd (format  "(build conn for completing)port:(default:%s)" osq-port)  nil osq-port)))

(when (featurep 'anything)
  (defvar anything-c-source-oracle-candidates nil)
  (defvar anything-c-source-oracle
    '((name . "SQL Object:")
      (init (lambda() (setq anything-c-source-oracle-candidates ( sqlparser-oracle-context-candidates))))
      (candidates . anything-c-source-oracle-candidates)
      (action . (("Complete" . (lambda(candidate) (backward-delete-char (length (sqlparser-word-before-point))) (insert candidate)))))))

  (defun anything-oracle-complete()
    "call `anything' to complete tablename and column name for oracle."
    (interactive)
    (let ((anything-execute-action-at-once-if-one t)
          (anything-quit-if-no-candidate
           (lambda () (message "complete failed."))))
      (anything '(anything-c-source-oracle)
                ;; Initialize input with current symbol
                (sqlparser-word-before-point)  nil nil))))


(defun sqlparser-oracle-complete()
  "complete tablename or column name depending on current point
position ."
  (interactive)
  (let ((prefix  (sqlparser-word-before-point) )
        (init-pos (point))
        last-mark)
    (insert (completing-read "complete:" (  sqlparser-oracle-context-candidates) nil t prefix ))
    (setq last-mark (point-marker))
    (goto-char init-pos)
    (backward-delete-char (length prefix))
    (goto-char (marker-position last-mark))
    ))

(defun  sqlparser-oracle-context-candidates()
  "it will decide to complete tablename or columnname depend on
  current position."
  (let ((context (sqlparser-parse)) candidats)
    (cond
     ((string= "schema" context)
      (setq candidats (sqlparser-oracle-schemaname-candidates))
      )
     ((string= "table" context)
      (setq candidats (sqlparser-oracle-tablename-or-schemaname-candidates))
      )
     ((string= "column" context)
      (setq candidats (  sqlparser-oracle-column-candidates))
      )
     ((null context)))
    candidats))






;; elect view_name from all_views where owner = 'U1'
;; union all
;; select table_name from all_tables where owner = 'U1'
;; union all
;; select table_name from all_tab_privs where grantee = 'U1' and privilege = 'SELECT'
(defun  sqlparser-oracle-tablename-or-schemaname-candidates ()
  "all tablename viewname i can select."
  ;;-s means use TAB as separate char . -N means don't print column name.
  (mapcar 'car
          (oracle-shell-query
           (format " select view_name from all_views where upper(owner)=upper('%s') union all select table_name from all_tables where upper(owner)=upper('%s') union all  select table_schema||'.'||table_name from all_tab_privs where lower(grantee) = lower('%s') and privilege = 'SELECT' union all select table_name from dict"
                   osq-username osq-username osq-username))))

(defun sqlparser-oracle-schemaname-candidates ()
  "all schema-name in oracle database"
  ;;-s means use TAB as separate char . -N means don't print column name.
  (mapcar 'car
          (oracle-shell-query
           (format "select table_schema from all_tab_privs where lower(grantee) = lower('%s') and privilege = 'SELECT'"
            osq-username))))

(defun  sqlparser-oracle-column-candidates ()
  "column name candidates of table in current sql "
  (let* ((sql "select column_name from user_tab_columns where 1=0")
         (table-names (sqlparser-fetch-tablename-from-select-sql
                       (sqlparser-sql-sentence-at-point)))
         (prefix (sqlparser-get-prefix))
         (sub-prefix (split-string prefix "\\." nil))
         tablename tablenamelist schemaname )
    (if (> (length sub-prefix) 1);;alias.columnsname
        (progn
          (setq tablename (sqlparser-guess-table-name (car sub-prefix)))
          (setq tablenamelist (split-string tablename "[ \t\\.]" t))
          (if (= 1 (length tablenamelist)) ;;just tablename ,not dbname.tablename
              (progn
                (setq tablename (car tablenamelist))
                (setq schemaname nil)
                (setq sql (format " select column_name from all_tab_columns  where upper(table_name)=upper('%s') and upper(column_name) like upper('%s%%') "
                                  tablename (nth 1 sub-prefix) ) ))
            (setq schemaname (car tablenamelist))
            (setq tablename (cadr tablenamelist))
            (setq sql (format "select column_name from all_tab_columns where upper(owner)=upper('%s') and upper(table_name) =upper('%s') and upper(column_name) like upper('%s%%')"
                              schemaname tablename (nth 1 sub-prefix)))
            ))
      (while (> (length table-names) 0)
        (setq tablename (pop table-names))
        (setq tablenamelist (split-string tablename "[ \t\\.]" t))
        (if (= 1 (length tablenamelist))
            (progn
              (setq tablename (car tablenamelist))
              (setq schemaname nil)
              (setq sql (format "%s union select column_name from all_tab_columns where upper(table_name)=upper('%s') and upper(column_name) like upper('%s%%') " sql tablename prefix ))
              )
          (setq tablename (cadr tablenamelist))
          (setq schemaname (car tablenamelist))
          (setq sql (format "%s union select column_name from all_tab_columns where upper(table_name)=upper('%s') and upper(owner)=upper('%s') and upper(column_name) like upper('%s%%') "
                            sql tablename schemaname prefix)))))
    (mapcar 'car (oracle-shell-query sql))))

;; TEST :
;; (sqlparser-fetch-tablename-from-sql "select * from (select id from oracle.emp a , oracle.abc ad) ,abcd  as acd  where name=''")
;; (sqlparser-fetch-tablename-from-sql "update user set age=11 ")
;; (sqlparser-fetch-tablename-from-sql "alter  user add (tim datetime)")

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
      (when (search-forward-regexp "\\(\\binto\\|update\\|alter\\)[ \t]+\\([a-zA-Z0-9\\._]+\\)\\b" (point-max ) t)
        (setq tablename (match-string 2))
        )
      )))

(defun sqlparser-fetch-tablename-from-select-sql ( &optional sql1)
  "return a list of tablenames from a sql-sentence."
  (let* ((sql (or sql1 (sqlparser-sql-sentence-at-point)))
         (sql-stack (list sql)) ele pt result-stack tablename-stack )
    (while (> (length sql-stack) 0)
      (setq ele (pop sql-stack))
      (with-temp-buffer
        (insert ele)
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t]*(" (point-max) t)
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
        (when  (search-forward-regexp "[ \t]+from[ \t]+" (point-max) t)
          (delete-region (point-min) (point))
          (when (search-forward-regexp "[ \t]+where[ \t]+" (point-max) t)
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

        (if (= 1  (count-matches  "[a-zA-Z0-9_\\.]+" 1 (point-max)))
            (push (buffer-substring 1 (point-max)) result-stack)
          (goto-char 0)
          (when (search-forward-regexp "[a-zA-Z0-9_\\.]+" (point-max) t )
            (push (match-string 0) result-stack)
            )
          )
        )
      )
    (delete "table" result-stack)
    result-stack
    ))

;; TEST :
;; (sqlparser-fetch-tablename-from-select-sql "select * from (select id from oracle.emp a , oracle.abc ad) ,abcd  as acd  where name=''")


(defun sqlparser-guess-table-name (alias &optional sql1)
  "find out the true table name depends on the alias.
suppose the sql is `select * from user u where u.age=11'
then the `u' is `alias' and `user' is the true table name."
  (let ((sql  (or sql1 (sqlparser-sql-sentence-at-point)))
        (regexp (concat  "\\([a-zA-Z0-9_\\.]+\\)[ \t]+\\(as[ \t]+\\)?" alias "[, \t\n\r]\\|$"))
        table-name)
    (setq sql (concat sql " "))
    (if (and  sql (string-match regexp sql))
        (progn
          (setq table-name (match-string 1 sql))
          (if (string-equal "from" table-name) alias table-name))
      alias)
    ))
;; TEST :
;; (sqlparser-guess-table-name "a"   "select * from (select id from oracle.emp a , oracle.abc ad) ,abcd  as acd  where name=''")


;; (defun sql-mode-hook-fun()
;;   "change the `sentence-end'"
;;   (make-local-variable 'sentence-end)
;;   (make-local-variable 'sentence-end-without-space)
;;   (setq sentence-end nil)
;;   (setq sentence-end-without-space ";")

;;   )
;; (add-hook 'sql-mode-hook 'sql-mode-hook-fun)

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
      ;;(end-of-line)
      (re-search-backward ";[ \t\n\r]*\\|\\`\\|\n[\r\t ]*\n[^ \t]" nil t)
      (skip-syntax-forward "-")
      (setq begin (match-end 0)))
    (save-excursion
      (skip-chars-forward " \t\n\r")
      (re-search-forward "\n[\r\t ]*\n[^ \t]\\|\\'\\|[ \t\n\r]*;" nil t)
      (unless (zerop (length (match-string 0)))
        (backward-char 1))
      (skip-syntax-backward "-")
      (setq end   (match-beginning 0)))
    (goto-char pt)
    (cons begin end)
    )
  )
;; 1 after keyword 'use'   :complete schema name
;; 2 after keyword 'alter', 'from' 'update' 'desc'  'show' : complete tablename
;; 3 after keyword 'select' 'set' 'where'    :complete  columnname.
;; 4 after keyword 'into' and and there isn't a
;; "\\(" between 'into' and current postion :complete tablename
;; 4.1 after keyword 'into' but there is a "(" between 'into' and current
;; position  :complete columnname
;; 5 after keyword 'values'  :complete nothing.
(defun sqlparser-parse()
  "judge now need complete tablename or column name or don't complete .
it will return 'table' ,or 'column' ,or nil.
"
  (let* ((cur-pos (point))
         (sql-pos-info (bounds-of-sql-at-point))
         (sql-start-pos (car sql-pos-info ))
         (sql-end-pos (cdr sql-pos-info))
         map keyword returnVal)
    (when (search-backward-regexp "\\buse\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "use") map))
    (when (search-backward-regexp "\\balter\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "alter") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bfrom\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "from") map))
    (when (search-backward-regexp "\\bshow\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "show") map))
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
     ((string-match "use" keyword)
      (setq returnVal "schema")
      )
     ((string-match "from\\|alter\\|update\\|desc\\|describe\\|show" keyword)
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


(provide 'sqlparser-oracle-complete)
;;; sqlparser-oracle-complete.el ends here

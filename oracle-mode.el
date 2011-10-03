;;; oracle-mode.el --- oracle-mode   -*- coding:utf-8 -*-

;; Description: oracle-mode
;; Time-stamp: <Joseph 2011-10-03 11:43:12 星期一>
;; Created: 2011-10-03 01:19
;; Author: 孤峰独秀  jixiuf@gmail.com
;; Maintainer:  孤峰独秀  jixiuf@gmail.com
;; Keywords: oracle
;; URL: http://www.emacswiki.org/emacs/oracle-mode.el

;; Copyright (C) 2011, 孤峰独秀, all rights reserved.

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
;;  `oracle-mode-execute'
;;    insert result in current buffer
;;  `oracle-mode-execute-other-buffer'
;;    insert result in other buffer other window
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'oracle-query)

(defun oracle-mode-execute()
"insert result in current buffer"
  (interactive)
  (let* ((bounds  (bounds-of-sql-at-point-4-oracle))
        (result (oracle-query-with-heading (buffer-substring (car bounds) (cdr bounds)) )))
    (goto-char (cdr bounds))
    (insert (oracle-mode-format-result result))
    (goto-char (cdr bounds))
    )
  )

(defun oracle-mode-execute-other-buffer()
  "insert result in other buffer other window"
  (interactive)
  (let* ((sql (sql-sentence-at-point-4-oracle)) mark
         (result-buffer-name "*oracle-mode-result*")
         (result (oracle-query-with-heading sql)))
    (with-current-buffer (get-buffer-create result-buffer-name)
      (goto-char (point-max))
      (insert (concat "\n" sql "\n"))
      (setq mark (point-max))
      (insert (oracle-mode-format-result result))
      (switch-to-buffer-other-window result-buffer-name)
      (goto-char mark)
      (recenter 1)
      )
    )
  )
;; (oracle-mode-max-length-of-each-columns '(("a" "bbb" "c") ("aa" "bb" "cc")))= (2 3 2)
(defun oracle-mode-max-length-of-each-columns(result)
  (let ((head (car result))
        (maxlength 0)
        maxlength-list (index 0))
    (while (< index (length head))
      (setq maxlength 0)
      (dolist (column result)
        (when (>  (length (nth index  column)) maxlength)
          (setq maxlength (length (nth index  column)))))
      (setq maxlength-list (append maxlength-list (list maxlength)))
      (setq index (1+ index)))
    maxlength-list))

(defun oracle-mode-format-result(result)
  (let* ((maxlength-list (oracle-mode-max-length-of-each-columns result))
         (index 0) (index2 1) (separater " | ")
         (line ) (separater-line " |-"))
    (while (< index (length maxlength-list))
      (setq separater-line (concat separater-line (make-string (nth index maxlength-list) ?-) "-+-"))
      (setq index (1+ index)))
    (setq line (concat "\n"  separater-line "\n | "))
    (dolist (row result)
      (setq index 0)
      (while (< index (length maxlength-list))
        (setq line (concat line  (format (format "%%%ds" (nth index maxlength-list) ) (nth index row)) separater))
        (setq index (1+ index)))
      (when (= 1 index2 ) (setq line (concat line "\n" separater-line )))
      (when (<  index2 (length result)) (setq line (concat  line "\n" separater )))
      (setq index2 (1+ index2))
      )
    (concat line "\n"  separater-line "\n")
    )
  )

(defun sql-sentence-at-point-4-oracle()
  "get current sql sentence. "
  (let* ((bounds (bounds-of-sql-at-point-4-oracle))
         (beg (car bounds))
         (end (cdr bounds)))
    (buffer-substring-no-properties  beg end)
    ))

(defun bounds-of-sql-at-point-4-oracle()
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

(provide 'oracle-mode)
;;; oracle-mode.el ends here

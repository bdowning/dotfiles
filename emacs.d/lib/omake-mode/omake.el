;;; omake.el --- Editing mode for omake files

;; Copyright (C) 2007  David M. Cooke

;; Author: David M. Cooke <david.m.cooke@gmail.com>
;; Version: 1
;; Keywords: omake

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Uses some code from python-mode.el, which has the following licence:
;; Copyright (C) 1992,1993,1994  Tim Peters
;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.

;;; Code:

(require 'rx)

(defvar omake-hook nil)

(defvar omake-indent-offset 4
  "Amount of indent that TAB adds.")

;; Taken from python-mode.el
(defun omake-shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

;; Taken from python-mode.el
(defsubst omake-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently; its policy doesn't require us
  ;; to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

;; Taken from python-mode.el
(defun omake-shift-region-left (start end &optional count)
  "Shift region of Omake code to the left.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the left, by `omake-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, dedent only the current line.
You cannot dedent the region if any line is already at column zero."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (omake-shift-region start end (- (prefix-numeric-value
                                    (or count omake-indent-offset))))
  (omake-keep-region-active))

;; Taken from python-mode.el
(defun omake-shift-region-right (start end &optional count)
  "Shift region of Omake code to the right.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the right, by `omake-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, indent only the current line."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (omake-shift-region start end (prefix-numeric-value
                                 (or count omake-indent-offset)))
  (omake-keep-region-active))

(add-to-list 'auto-mode-alist
             '("\\(OMake\\(?:file\\|root\\)\\)\\|\\(\\.om\\)$"
               . omake-mode))

(defconst omake--re-identifier "[-@~_A-Za-z0-9]+")

(defconst omake--re-command
  (rx (seq line-start
           (* space)
           (group
            (or
             "case"     "catch"  "class"   "declare"  "default"
             "do"       "else"   "elseif"  "export"   "extends"
             "finally"  "if"     "import"  "include"  "match"
             "open"     "raise"  "return"  "section"  "switch"
             "try"      "value"  "when"    "while"))
           word-end)))

(defconst omake--re-block-opening-command
  (rx (group
       (or
        "if" "else" "elseif" "case" "when" "default" "try" "catch" "finally"
        "section" "while"))))
(defconst omake--re-block-closing-command
  (rx (group (or "return" "raise" "value"))))

(defconst omake--re-single-char-identifier "[][@~_A-Za-z0-9&*<^?-]")

(defconst omake--re-variable-ref
  (rx-to-string
   `(seq
     "$" (or (seq "(" (regexp ,omake--re-identifier) ")")
             (regexp ,omake--re-single-char-identifier)))))

(defconst omake--re-variable-def
  (rx-to-string
   `(seq line-start
         (* space)
         (group
          (regexp ,omake--re-identifier)
          (group (\? "." (regexp ,omake--re-identifier)))) ; for object members
         (group (\? "[]"))
         (* space)
         (group (\? "+"))
         "=")))

(defconst omake--re-object-def
  (rx-to-string
   `(seq line-start
         (* space)
         (group (regexp ,omake--re-identifier))
         "."
         (* space)
         (group (\? "+"))
         "=")))

(defconst omake--re-multiline-function-def
  (rx-to-string
   `(seq line-start
         (* space)
         (group (regexp ,omake--re-identifier)
                (\? "." (regexp ,omake--re-identifier)))
         (* space) "("
         ;; this should match id, ..., id, but we'll be sloppy
         (group (minimal-match (* nonl)))
         ")"
         (* space) "="
         (* space) line-end))
  "Regular expression that (loosely) matches the beginning of a multiline
function definintion")

(defconst omake--re-rule
  (rx (seq line-start
           (* space)
           (group (minimal-match (* nonl)))
           ":"
           (* nonl) line-end))
  "Regular expression that (loosely) matches the rule header")

(defconst omake--re-comment
  (rx (seq (* space)
           (group "#" (* not-newline)))))

(defconst omake--re-blank-or-comment
  (rx (seq (* space)
           (group (or line-start "#"))))
  "Regular expression matching a blank or comment line.")

(defconst omake-font-lock-keywords
  (list
   (cons omake--re-command font-lock-keyword-face)
   (cons (rx (group
              "$"
              (group (or (+ ?\") (+ ?\')))
              (minimal-match (* not-newline))
              (backref 2)))
         font-lock-string-face)
   (cons (rx-to-string `(group (regexp, omake--re-variable-ref)))
         font-lock-variable-name-face)
   (cons (rx-to-string `(seq word-start
                             (group (seq (regexp ,omake--re-identifier) "."))))
         font-lock-type-face)
   (cons (rx (group (seq "$" (in "*>@^+<&`,"))))
         font-lock-builtin-face)
))

(defun omake--alist (&rest body)
  (let (alist)
    (while body
      (if (not (keywordp (car body)))
          (error "Non-keyword in unexpected place")
        (push (cons (pop body) (pop body)) alist)))
    alist))

(defun omake--alist-from-match (&rest body)
  (let ((alist (list
                (cons :range (cons (match-beginning 0) (match-end 0)))))
        kw r)
    (while body
      (setq kw (pop body))
      (if (not (keywordp kw))
          (error "Non-keyword in unexpected place")
        (setq r (pop body))
        (push (cons kw (cons (match-beginning r) (match-end r))) alist)))
    alist))

(defun omake--match (alist symbol)
  (cdr (assq symbol alist)))
(defun omake--match-begin (alist symbol)
  (car (omake--match alist symbol)))
(defun omake--match-end (alist symbol)
  (cdr (omake--match alist symbol)))

(defmacro omake--def-point-bol (name pat &rest body)
  `(defun ,name ()
     (save-excursion
       (let ((here (point)))
         (beginning-of-line)
         (if (and (looking-at ,pat)
                  (>= (match-end 0) here))
             (omake--alist-from-match ,@body)
           nil)))))

(omake--def-point-bol omake-point-command omake--re-command
                      :command 1)
(omake--def-point-bol omake-point-variable-def omake--re-variable-def
                      :id 1 :member 2 :array 3 :add 4)
(omake--def-point-bol omake-point-object-def omake--re-object-def
                      :id 1 :add 2)
(omake--def-point-bol omake-point-multiline-function-def
                      omake--re-multiline-function-def
                      :params 1)
(omake--def-point-bol omake-point-rule omake--re-rule
                      :target 1)

(defun omake-point-comment ()
  (save-excursion
    (let ((here (point)))
      (beginning-of-line)
      (if (and (re-search-forward omake--re-comment (line-end-position) t)
               (<= (match-beginning 1) here))
          (omake--alist-from-match :comment 1)
        nil))))

(defun omake-point-variable-ref ()
  (save-excursion
    (let ((here (point)))
      (if (and (search-backward "$" (line-beginning-position))
               (looking-at omake--re-variable-ref)
               (>= (match-end 0) here))
          (omake--alist-from-match :id 1)
        nil))))

(defun omake--indent-to (n)
  (beginning-of-line)
  (delete-horizontal-space)
  (indent-to n))

(defun omake--round-down-to-indent-offset (col)
  (* (/ col omake-indent-offset) omake-indent-offset))

(defun omake-indent ()
  (interactive)
  (let* ((ci (current-indentation))
         (rci (omake--round-down-to-indent-offset ci))
         (cc (current-column))
         (move-to-indentation
          (if (<= cc ci) 'back-to-indentation 'ignore))
         (max (omake-compute-max-indentation)))
    (cond
     ((= ci 0)
      (omake--indent-to max))
     ((/= rci ci)
      (omake--indent-to (+ rci omake-indent-offset)))
     ;; dedent out a level if this was the previous command also,
     ;; unless we're in column 1
     ((equal last-command this-command)
      (if (/= cc 0)
          (omake--indent-to (omake--round-down-to-indent-offset (1- cc)))
        (omake--indent-to max)))
     ((< ci max)
      (omake--indent-to (+ rci omake-indent-offset)))
     ((> ci max)
      (omake--indent-to max))
     (t nil))
    (funcall move-to-indentation)))

(defun omake-compute-max-indentation ()
  (let ((ci (current-indentation)))
    (save-excursion
      (if (bobp)
          ci
        (forward-line -1)
        (while (looking-at "^\\s-*$")
          (forward-line -1))
        (cond
         ((and (omake-point-comment)
               (<= ci
                   (progn
                     (forward-comment (- (point-max)))
                     (current-indentation))))
          ci)
         ((omake-statement-opens-block-p)
          (+ (current-indentation) omake-indent-offset))
         ((omake-statement-closes-block-p)
          (- (current-indentation) omake-indent-offset))
         (t
          (current-indentation)))))))

(defun omake-statement-opens-block-p ()
  (save-excursion
    (beginning-of-line)
    (let (r)
      (cond
       ((setf r (omake-point-command))
        (goto-char (omake--match-begin r :command))
        (looking-at omake--re-block-opening-command))
       ((omake-point-object-def) t)
       ((setf r (omake-point-variable-def))
        (goto-char (omake--match-end r :range))
        (looking-at (rx (* space) line-end)))
       ((setf r (omake-point-multiline-function-def))
        (goto-char (omake--match-end r :range))
        (looking-at (rx (* space) line-end)))
       ((omake-point-rule) t)
       (t nil))
  )))

(defun omake-statement-closes-block-p ()
  (save-excursion
    (beginning-of-line)
    (let (r)
      (cond
       ((setf r (omake-point-command))
        (goto-char (omake--match-begin r :command))
        (looking-at omake--re-block-closing-command))
       (t nil)))))

(defvar omake-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\' "'" st)
    (modify-syntax-entry ?\" "." st)
    (modify-syntax-entry ?#  "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?_  "w" st)
    st))

(defun omake--set-local (v arg)
  (set (make-local-variable v) arg))

;;;###autoload
(define-derived-mode omake-mode
  nil "OMakefile"
  "A major mode for editing OMake files.
\\{omake-mode-map}"
  :syntax-table omake-syntax-table
  :abbrev-table nil
    (omake--set-local 'comment-start "#")
    (omake--set-local 'comment-end "")
    (omake--set-local 'comment-start-skip "#+\\s-*")
    (omake--set-local 'font-lock-defaults '(omake-font-lock-keywords))
    (omake--set-local 'indent-line-function #'omake-indent)
    (message "Omake mode")
    (message "indent-line-function = %s" indent-line-function)
)

(define-key omake-mode-map "\C-j" 'newline-and-indent)
;; indentation level modifiers. Same keys python-mode uses
(define-key omake-mode-map "\C-c\C-l"  'omake-shift-region-left)
(define-key omake-mode-map "\C-c\C-r"  'omake-shift-region-right)
(define-key omake-mode-map "\C-c<"     'omake-shift-region-left)
(define-key omake-mode-map "\C-c>"     'omake-shift-region-right)

(provide 'omake)
;;; omake.el ends here

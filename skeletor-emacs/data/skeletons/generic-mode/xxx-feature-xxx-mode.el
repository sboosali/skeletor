;;; xxx-feature-xxx-mode.el --- __SYNOPSIS__ -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Free Software Foundation, Inc
;; Copyright © 2019 Spiros Boosalis

;; Version: 0.0.0
;; Package-Requires: ((emacs "25") seq pcase)
;; Author:  Spiros Boosalis <samboosalis@gmail.com>
;; Homepage: https://github.com/__GitHubUser__/xxx-feature-xxx-mode
;; Keywords: __KEYWORDS__
;; Created: 01 May 2019

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; __SYNOPSIS__.
;;
;; Commands:
;;
;; • `xxx-feature-xxx-mode'
;;
;; Variables:
;;
;; • `xxx-feature-xxx-builtin-keywords-list'
;; • `xxx-feature-xxx-builtin-operators-list'
;;
;; Bugs: https://github.com/__GitHubUser__/issues
;;
;; History: https://github.com/__GitHubUser__/blob/master/CHANGELOG.md
;;
;; 

;;; Code:

;;----------------------------------------------;;
;; Imports -------------------------------------;;
;;----------------------------------------------;;

;; Builtins:

(eval-when-compile
  (require 'rx)
  (require 'cl-lib)
  ())

;;----------------------------------------------;;

(progn
  (require 'subr-x)
  (require 'pcase)
  (require 'seq)
  ())

;;----------------------------------------------;;
;; Variables -----------------------------------;;
;;----------------------------------------------;;

(defgroup xxx-feature-xxx

  nil

  "Customize `xxx-feature-xxx-mode'."

;;:link (url-link "")

;;:group '
  )

;;==============================================;;

(defvar xxx-feature-xxx-keywords

  '("imported" "exported")

  "Keywords for `xxx-feature-xxx-mode'.")

;;----------------------------------------------;;

(defvar xxx-feature-xxx-file-extensions

  '("\\.sapi\\'" "\\.sapi.py\\'")

  "File Extensions for `xxx-feature-xxx-mode'.
Override `python-mode'  for this compound-file-extension.")

;;----------------------------------------------;;

(defvar xxx-feature-xxx-

  '(
    ("^#.*"      . 'font-lock-comment-face)       ;; comments at start of line
    ("<dgn.*?>"  . 'font-lock-builtin-face)       ;; 
    ("^<.*?>"    . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>"     . 'font-lock-variable-name-face) ;; other nonterminals
    ("{.*?}"     . 'font-lock-variable-name-face) ;;
    ("="         . 'font-lock-constant-face)      ;; "goes-to" symbol
    (";"         . 'font-lock-constant-face)      ;; statement delimiter
    ("\|"        . 'font-lock-keyword-face)       ;; "OR" symbol
    ("\+"        . 'font-lock-keyword-face)       ;; 
    ("\["        . 'font-lock-keyword-face)       ;; 
    ("\]"        . 'font-lock-keyword-face)       ;; 
    )

  "AssociationList of RegularExpressions for syntax-highlighting `xxx-feature-xxx-mode'.")

;;==============================================;;

(defalias 'xxx-feature-xxx-mode-version #'pkg-info-package-version)

;;----------------------------------------------;;

(defun xxx-feature-xxx-mode-help ()

  "Open a Help Buffer for `xxx-feature-xxx-mode'."

  (interactive)

  )

;;==============================================;;

(defvar xxx-feature-xxx-mode-syntax-table nil

  "Syntax table for `xxx-feature-xxx-mode'. Natlink's BNF has Bash-style comment syntax.")

;;----------------------------------------------;;

(setq xxx-feature-xxx-mode-syntax-table

      (let ((*SyntaxTable* (make-syntax-table)))

        ;; Bash-style comment: “# …”

        (modify-syntax-entry ?#  "<" *SyntaxTable*)
        (modify-syntax-entry ?\n ">" *SyntaxTable*)

        *SyntaxTable*))

;;==============================================;;



;;----------------------------------------------;;



;;==============================================;;

(define-generic-mode 'natlink-grammar-simple-mode

  ()                                              ;; comment char: inapplicable because # must be at start of line

  '("imported" "exported")                        ;; keywords

  '(
    ("^#.*"      . 'font-lock-comment-face)       ;; comments at start of line
    ("<dgn.*?>"  . 'font-lock-builtin-face)       ;; 
    ("^<.*?>"    . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<.*?>"     . 'font-lock-variable-name-face) ;; other nonterminals
    ("{.*?}"     . 'font-lock-variable-name-face) ;;
    ("="         . 'font-lock-constant-face)      ;; "goes-to" symbol
    (";"         . 'font-lock-constant-face)      ;; statement delimiter
    ("\|"        . 'font-lock-keyword-face)       ;; "OR" symbol
    ("\+"        . 'font-lock-keyword-face)       ;; 
    ("\["        . 'font-lock-keyword-face)       ;; 
    ("\]"        . 'font-lock-keyword-face)       ;; 
   )

  '("\\.sapi\\'" "\\.sapi.py\\'")                 ;; filename suffixes
                                                  ;; override python-mode, only for this compound-file-extension.

  nil                                             ;; extra function hooks

  "Major mode for highlighting a NatLink/SAPI grammar.")

        ;; token can be '=', '|', '+', ';', '(', ')', '[', ']' (with value None)
        ;; or 'list' (value without {})
        ;; or 'rule' (value wihtout <>)
        ;; or 'sqword', 'dqword', 'word'  (a word, in single quotes, double quotes or unquoted)















;;----------------------------------------------;;
;; Commands ------------------------------------;;
;;----------------------------------------------;;

(define-derived-mode xxx-feature-xxx-mode prog-mode "__Feature__"

  "Major mode for ...

"

  (progn

    (setq font-lock-defaults (list nil nil))

    (set-syntax-table xxx-feature-xxx-mode-syntax-table)

    ()))

;;==============================================;;

(defun xxx-feature-xxx-mode-version ()

  "Returns the (currently-loaded) version of `xxx-feature-xxx-mode'.

Output:

• a `listp' of `numberp's."

  (interactive)

  (let ((ECHO-VERSION? (called-interactively-p 'any))
        )

  (pkg-info-package-version 'xxx-feature-xxx-mode ECHO-VERSION?)))

;;----------------------------------------------;;

(defun xxx-feature-xxx-mode-help ()

  "Open a (Help Buffer) tutorial for `xxx-feature-xxx-mode'."

  (interactive)

  )

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; 
;;
;;

;;----------------------------------------------;;
;; EOF -----------------------------------------;;
;;----------------------------------------------;;

(provide 'xxx-feature-xxx)

;;; xxx-feature-xxx-mode.el ends here
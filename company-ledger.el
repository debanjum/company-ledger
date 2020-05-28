;;; company-ledger.el --- Fuzzy Transaction Auto-Completion for Ledger & Friends

;; Copyright (C) 2018 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum AT gmail DOT com>
;; Description: Transaction auto-completion for ledger & friends
;; Keywords: abbrev, matching, auto-complete, beancount, ledger, company
;; Version: 0.1.0
;; Package-Requires: ((company "0.8.0"))
;; URL: https://github.com/debanjum/company-ledger

;; This file is NOT part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  Detailed Behavior
;; --------------------
;; - Provides in-place transaction auto-completion based on words on current line
;; - The words on the current line can be partial and in any order
;; - The candidate transactions are sorted by most recent transaction

;;; Acknowledgments

;; Sixty North's blog entry http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend.html
;; provided the push required to finally write this company mode

;;; Code:

(require 'cl-lib)
(require 'company)

(defun company-ledger--regexp-filter (regexp list)
  "Use REGEXP to filter LIST of strings."
  (let (new)
    (dolist (string list)
      (when (string-match regexp string)
        (setq new (cons string new))))
    new))

(defun company-ledger--get-all-postings ()
  "Get all paragraphs in buffer containing YYYY[-/]MM[-/]DD in them."
  (company-ledger--regexp-filter
   "[0-9][0-9][0-9][0-9][\-/][0-9][0-9][\-/][0-9][0-9]"
   (mapcar #'(lambda (s) (substring s 1))
           (split-string (buffer-string) "^$" t))))

(defun company-ledger--fuzzy-word-match (prefix candidate)
  "Return true if each (partial) word in PREFIX is also in CANDIDATE."
  (eq nil
      (memq nil
            (mapcar
             #'(lambda (pre) (string-match-p (regexp-quote pre) candidate))
             (split-string prefix)))))

(defun company-ledger--next-line-empty-p ()
  "Return true if next line empty else false."
  (save-excursion
    (beginning-of-line)
    (forward-line 1)
    (or (looking-at "[[:space:]]*$")
        (eolp)
        (eobp))))

;;;###autoload
(defun company-ledger-backend (command &optional arg &rest ignored)
  "Company back-end for ledger, beancount and other ledger-like modes.
  "Fuzzy company back-end for ledger, beancount and other ledger-like modes.
Provide completion info based on COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ledger-backend))

    (prefix (and (or (bound-and-true-p beancount-mode)
                     (derived-mode-p 'ledger-mode))
                 (company-ledger--next-line-empty-p)
                (thing-at-point 'line t)))

    (candidates
     (cl-remove-if-not
      (lambda (c) (company-ledger--fuzzy-word-match arg c))
      (company-ledger--get-all-postings)))
    (sorted t)))

(provide 'company-ledger)
;;; company-ledger.el ends here

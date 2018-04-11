(require 'cl-lib)
(require 'company)

(defun regexp-filter (regexp list)
  "Filter LIST of strings with REGEXP."
  (let (new)
    (dolist (string list)
      (when (string-match regexp string)
	(setq new (cons string new))))
    (nreverse new)))

(defun get-all-postings ()
  "Get all paragraphs containing YYYY-MM-DD in them"
  (regexp-filter "[0-9][0-9][0-9][0-9][\-/][0-9][0-9][\-/][0-9][0-9]"
		 (mapcar #'(lambda (s) (substring s 1))
			 (split-string (buffer-string) "^$" t))))

(defun fuzzy-match (prefix candidate)
  "returns true if each character is also in candidate"
  (cl-subsetp (string-to-list prefix)
	      (string-to-list candidate)))

(defun company-beancount-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-beancount-backend))
    (prefix (and (bound-and-true-p beancount-mode)
                (thing-at-point 'line t)))
    (candidates
    (cl-remove-if-not
      (lambda (c) (fuzzy-match arg c))
      (get-all-postings)))))

(provide 'company-beancount)

(require 'company)

(defun regexp-filter (regexp list)
  "Filter LIST of strings with REGEXP."
  (let (new)
    (dolist (string list)
      (when (string-match regexp string)
	(setq new (cons string new))))
    new))

(defun get-all-postings ()
  "Get all paragraphs containing YYYY-MM-DD in them"
  (regexp-filter "[0-9][0-9][0-9][0-9][\-/][0-9][0-9][\-/][0-9][0-9]"
		 (mapcar #'(lambda (s) (substring s 1))
			 (split-string (buffer-string) "^$" t))))

(defun fuzzy-word-match (prefix candidate)
  "returns true if each word in prefix is also in candidate"
  (eq nil (memq nil (mapcar #'(lambda (p) (string-match-p (regexp-quote p) candidate)) (split-string prefix)))))

(defun company-beancount-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-beancount-backend))
    (prefix (and (bound-and-true-p beancount-mode)
                (thing-at-point 'line t)))
    (candidates
     (remove-if-not
      (lambda (c) (fuzzy-word-match arg c))
      (get-all-postings)))))

(provide 'company-beancount)

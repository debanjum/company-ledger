==============
Company Ledger
==============

Transaction Auto-Completion for Beancount & other Ledger-Likes with Company Mode


Usage
-----

1. Open your ledger file or the `sample beancount`_ file
2. Enter text to narrow to previous transaction you want to base your new transaction on
3. Select the appropriate transaction from dropdown
4. Edit as desired and you're done

Demo
====

.. image:: ./examples/demo.gif

	   
Installation
------------

Just copy the `company-ledger.el`_ script to your Emacs LOAD_PATH


Sample Setup
------------

Install, Configure: Beancount, Company, Company-Ledger

.. code:: lisp
	  
    ;; Company mode for Completion
    (use-package company :ensure t :defer t :diminish company-mode)

    ;; Custom Beancount Company backend
    (use-package company-ledger
      :load-path "~/.emacs.d/lisp/company-ledger.el"
      :ensure company
      :init
      (with-eval-after-load 'company
	  (add-to-list 'company-backends 'company-ledger-backend)))

    ;; Beancount Minor Mode
    ;; Get beancount.el from https://bitbucket.org/blais/beancount
    (use-package beancount
      :load-path "~/.emacs.d/lisp/beancount.el"
      :config (progn (add-hook 'beancount-mode-hook 'company-mode)))

    (add-to-list 'auto-mode-alist '("\\.bean\\'" . beancount-mode))


Acknowledgment
--------------
`Sixty North's blog post`_ provided the required push to write this company mode


License
-------

Copyright (C) 2018 Debanjum Singh Solanky

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see `http://www.gnu.org/licenses/`


.. _sample beancount: ./examples/otzi.beancount
.. _usage demo: ./examples/demo.gif
.. _company-ledger.el: ./company-ledger.el
.. _Sixty North's blog post: http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend.html


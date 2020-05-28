==============
Company Ledger
==============

Fuzzy Auto-Completion for Beancount & other Ledger-Likes with Company Mode


Usage
-----

1. Open your ledger file or the `sample beancount`_ file
2. Enter some text for a new transaction
3. A dropdown with similar past transactions will appear
4. Select the most relevant transaction from dropdown
5. The selected transaction will be inserted at point
6. Update the inserted transaction as appropriate

Demo
====

.. image:: ./examples/demo.gif


Installation
------------

Just copy `company-ledger.el`_ to a valid location in your Emacs `load-path`_


Sample Setup
------------

Sets up `beancount <https://bitbucket.org/blais/beancount>`_, `company <https://company-mode.github.io/>`_ and `company-ledger <https://github.com/debanjum/company-ledger>`_

Sample setup assumes:
  1. :code:`~/.emacs.d/lisp/` is in your Emacs :code:`load-path`
  2. `beancount.el <https://bitbucket.org/blais/beancount/src/default/editors/emacs/beancount.el>`_ and `company-ledger.el`_ are in :code:`~/.emacs.d/lisp/`
  3. `use-package <https://jwiegley.github.io/use-package/>`_ is managing your emacs configuration

.. code:: lisp

    ;; Company mode for Completion
    (use-package company :ensure t :defer t :diminish company-mode)

    ;; Custom Beancount Company backend
    (use-package company-ledger
      :load-path "~/.emacs.d/lisp/company-ledger.el"
      :ensure company
      :init
      (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-ledger)))

    ;; Beancount Minor Mode
    ;; Get beancount.el from https://bitbucket.org/blais/beancount
    (use-package beancount
      :load-path "~/.emacs.d/lisp/beancount.el"
      :config (progn (add-hook 'beancount-mode-hook 'company-mode)))

    (add-to-list 'auto-mode-alist '("\\.bean\\'" . beancount-mode))


License
-------

Copyright (C) 2018-2020 Debanjum Singh Solanky

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

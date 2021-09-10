==============
Company Ledger
==============
.. image:: https://melpa.org/packages/company-ledger-badge.svg
  :target: https://melpa.org/#/company-ledger

Fuzzy auto-completion for Beancount & other Ledger-likes with Company Mode


Installation
------------

Install `company-ledger <https://melpa.org/#/company-ledger>`_ from MELPA


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


Setup
-----

1. Make sure `company-mode`_ is already installed
2. Though not strictly required, you'd mostly want `ledger-mode`_ or `beancount-mode`_ also setup

Minimal
=======

.. code:: lisp

 (with-eval-after-load 'company
   (add-to-list 'company-backends 'company-ledger))

Use-Package
===========

.. code:: elisp

 (use-package company-ledger
   :ensure company
   :init
   (with-eval-after-load 'company
     (add-to-list 'company-backends 'company-ledger)))


.. _sample beancount: ./examples/otzi.beancount
.. _company-mode: https://company-mode.github.io
.. _ledger-mode: https://github.com/ledger/ledger-mode
.. _beancount-mode: https://github.com/beancount/beancount-mode

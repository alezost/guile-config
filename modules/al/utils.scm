;;; utils.scm --- Miscellaneous utilities

;; Copyright © 2016–2026 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 6 Feb 2016

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides various procedures that do not suit any other
;; module.

;; The following procedures originate from Guix source code:
;;
;; - split-path: `search-path-as-string->list' from (guix build utils)
;; - memoize: from (guix combinators)
;; - remove-keywords: `strip-keyword-arguments' from (guix utils).

;;; Code:

(define-module (al utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (al let-macros)
  #:export (with-no-output
            remove-keywords
            define-lazy
            memoize
            push!
            set-locale
            scheme->lisp
            digits
            format-index
            split-path))

(define-syntax-rule (define-lazy1 name doc body ...)
  (define name
    (let ((value (delay (begin body ...))))
      (lambda () doc (force value)))))

(define-syntax define-lazy
  (lambda (x)
    "Define NAME thunk that will evaluate BODY once (on the first call).
NAME will return result of the first evaluation on subsequent calls
without re-evaluating BODY.

  (define-lazy NAME [DOC] BODY ...)"
    (syntax-case x ()
      ((_ name doc body ...)
       (string? (syntax->datum #'doc))
       #'(define-lazy1 name doc body ...))
      ((_ name body ...)
       #'(define-lazy1 name #f body ...)))))

(define (memoize proc)
  "Return a memoizing version of PROC."
  (let ((cache (make-hash-table)))
    (lambda args
      (if-let ((results (hash-ref cache args)))
        (apply values results)
        (let ((results (call-with-values
                           (lambda () (apply proc args))
                         list)))
          (hash-set! cache args results)
          (apply values results))))))

(define (remove-keywords args . keywords)
  "Remove keyword/value pairs from ARGS.

If KEYWORDS are specified, remove only these keywords.  Otherwise,
remove all KEYWORDS.

This function exists because when a procedure is defined using both
#:key and #:rest keywords, then the rest argument also contains all the
key/value pairs.  See `(guile) lambda* and define*' node in the Guile
info manual."
  (let loop ((args args)
             (result '()))
    (match args
      (() (reverse result))
      (((? keyword? keyword) value . rest)
       (loop rest
             (if (or (null? keywords)
                     (memq keyword keywords))
               result
               (cons* value keyword result))))
      ((arg . rest)
       (loop rest (cons arg result))))))

(define-syntax-rule (push! elt lst)
  "Add ELT to LST."
  (set! lst (cons elt lst)))

(define-syntax-rule (with-no-output body ...)
  "Do not display any output while running BODY."
  (let ((null (%make-void-port "w")))
    (parameterize ((current-output-port  null)
                   (current-error-port   null)
                   (current-warning-port null))
      body ...)))

(define* (set-locale #:optional (locale ""))
  "Call (setlocale LC_ALL LOCALE) and ignore errors."
  (catch #t
    (lambda _ (setlocale LC_ALL locale))
    (lambda (_ . args)
      (apply display-error #f (current-error-port) args))))

(define* (split-path #:optional (path (getenv "PATH")) (separator #\:))
  "Split PATH string into a list of substrings with SEPARATOR."
  (string-tokenize path (char-set-complement (char-set separator))))

(define (scheme->lisp value)
  "Convert Scheme value to Lisp value."
  (case value
    ((#t) 't)
    ((#f) '())
    (else value)))

(define (digits integer)
  "Return the number of digits in INTEGER."
  (cond
   ((positive? integer)
    (1+ (inexact->exact (floor (log10 integer)))))
   ((zero? integer) 1)
   (else (digits (- integer)))))

(define (format-index index total)
  (let ((fmt (string-append "[~"
                            (number->string (digits total))
                            "d of ~d]")))
    (format #f fmt index total)))

;;; utils.scm ends here

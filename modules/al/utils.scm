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
;; - split-path: "search-path-as-string->list" from (guix build utils)
;; - memoize: from (guix combinators)

;;; Code:

(define-module (al utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 and-let-star) ; or (srfi srfi-2)
  #:re-export ((and-let* . and-let))
  #:export (with-no-output
            if-let
            when-let
            remove-keywords
            define-delayed
            memoize
            push!
            set-locale
            mapconcat
            comma-separated
            build-file-name
            min-string
            replace
            split
            split-path))

(define-syntax if-let
  (syntax-rules ()
    ;; Single binding.
    ((_ ((var expr)) then else)
     (let ((var expr))
       (if var then else)))
    ;; Multiple bindings.
    ((_ ((var expr) rest ...) then else)
     (let ((var expr))
       (if var
         (if-let (rest ...) then else)
         else)))
    ;; No else clause.
    ((_ bindings then)
     (if-let bindings then #f))))

(define-syntax-rule (when-let bindings body ...)
  (if-let bindings
    (begin body ...)))

(define-syntax-rule (define-delayed name expression)
  "Define NAME thunk that will evaluate EXPRESSION, remember and return
its value on a first call and will return this value on subsequent
calls."
  (define name
    (let ((value (delay expression)))
      (lambda () (force value)))))

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
  ;; Originates from `strip-keyword-arguments' from (guix utils).
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

(define* (mapconcat proc lst #:optional (separator ""))
  "Apply PROC to each element of LST and concatenate the result strings
into a single string using SEPARATOR."
  (match lst
    (() "")
    ((elt . rest)
     (fold (lambda (elt res)
             (string-append res separator (proc elt)))
           (proc elt)
           rest))))

(define (comma-separated . strings)
  "Return string by concatenating STRINGS with commas."
  (mapconcat identity strings ","))

(define (build-file-name . file-parts)
  "Return file name by concatenating FILE-PARTS with slashes."
  (mapconcat identity file-parts "/"))

(define (min-string . strings)
  "Like 'min' but performed on STRINGS.
Return #f if STRINGS are not specified."
  (reduce (lambda (cur min)
            (if (string< cur min) cur min))
          #f
          strings))

(define (replace pred new lst)
  "Replace element of LST matching PRED with NEW element."
  (cons new (remove pred lst)))

(define (split lst elt)
  "Return two values, a list containing the elements of the list LST
that appear before the first occurence of the object ELT and a list
containing the elements after ELT."
  (let-values (((head tail)
                (break (cut string=? elt <>) lst)))
    (values head
            (match tail
              (() '())
              ((_ rest ...) rest)))))

(define* (split-path #:optional (path (getenv "PATH")) (separator #\:))
  "Split PATH string into a list of substrings with SEPARATOR."
  (string-tokenize path (char-set-complement (char-set separator))))

;;; utils.scm ends here

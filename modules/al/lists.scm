;;; lists.scm --- List utilities

;; Copyright © 2016–2026 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>

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

;; This file provides additional functionality to work with lists.

;;; Code:

(define-module (al lists)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (replace
            split
            map-indexed
            for-each-indexed))

(define (replace pred new lst)
  "Replace element of LST matching PRED with NEW element."
  (cons new (remove pred lst)))

(define* (split lst elt #:optional (test equal?))
  "Split LST into 2 lists by ELT.
Compare list elements with TEST function.
Return two values, a list containing the elements of the list LST
that appear before the first occurence of the object ELT and a list
containing the elements after ELT."
  (let-values (((head tail)
                (break (cut test elt <>) lst)))
    (values head
            (match tail
              (() '())
              ((_ rest ...) rest)))))

(define-syntax-rule (iterate-indexed iterate proc lists ...)
  "Helper for `map-indexed' and `for-each-indexed'."
  (let ((index -1))
    (iterate (lambda args
               (set! index (1+ index))
               (apply proc index args))
             lists ...)))

(define-syntax-rule (map-indexed proc lists ...)
  "Apply PROC to index and each element of LISTS.
This is the same as `map' except the first argument for PROC is
index (starting from 0) of the current LISTS elements."
  (iterate-indexed map proc lists ...))

(define-syntax-rule (for-each-indexed proc lists ...)
  "Apply PROC to index and each element of LISTS.
This is the same as `for-each' except the first argument for PROC is
index (starting from 0) of the current LISTS elements."
  (iterate-indexed for-each proc lists ...))

;;; lists.scm ends here

;;; records.scm --- Improved version of 'define-record-type'

;; Copyright (C) 2015 David Thompson <davet@gnu.org>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Immutable record type syntax with keyword argument constructor,
;; field inheritance, and default field values.

;; This file is taken from
;; <https://git.dthompson.us/sly.git/blob/HEAD:/sly/records.scm>.  Only
;; the module name is changed.

;;; Code:

(define-module (al records)
  #:use-module (srfi srfi-9)
  #:export (define-record-type*))

(define-syntax-rule (define-record-type* type
                      constructor keyword-constructor
                      predicate
                      (field getter default) ...)
  "Define a new, immutable record type named TYPE.  PREDICATE is the
name of the procedure that tests if an object is of TYPE.  A GETTER is
the name of the accessor for its respective FIELD.  CONSTRUCTOR is the
name of the record type's low-level constructor that uses positional
arguments corresponding to the order in which the fields are
specified.  KEYWORD-CONSTRUCTOR is the name of the high-level
constructor that uses keyword arguments for each FIELD whose values
correspond to the DEFAULT associated with that field.  Additionally, a
special keyword, #:inherit, can be used to copy values from another
instance of TYPE and only change the fields that are explicitly
overridden."
  (begin
    (define-record-type type
      (constructor field ...)
      predicate
      (field getter) ...)

    (define keyword-constructor
      (let ((default-record (constructor default ...)))
        (lambda* (#:key (inherit default-record)
                        (field (getter inherit)) ...)
          (constructor field ...))))))

;;; records.scm ends here

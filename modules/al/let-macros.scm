;;; let-macros.scm --- `if-let' syntax family

;; Copyright © 2026 Alex Kost

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

;; This file provides `if-let' and similar macros.

;;; Code:

(define-module (al let-macros)
  ;; #:use-module (ice-9 and-let-star)
  ;; #:re-export ((and-let* . and-let))
  #:export (
            if-let
            if-let1
            if-letn
            when-let
            when-let1
            when-letn))

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

(define-syntax if-let1
  (syntax-rules ()
    ;; Single binding.
    ((_ (binding) then ...)
     (if-let (binding) then ...))
    ;; Multiple bindings.
    ((_ (first rest ...) then else)
     (if-let (first)
       (let* (rest ...)
         then)
       else))
    ;; No else clause.
    ((_ bindings then)
     (if-let1 bindings then #f))))

(define-syntax if-letn
  (syntax-rules ()
    ;; Single binding.
    ((_ (binding) then ...)
     (if-let (binding) then ...))
    ;; Multiple bindings.
    ((_ (first rest ...) then ...)
     (let (first)
       (if-letn (rest ...)
         then ...)))))

(define-syntax-rule (when-let bindings body ...)
  (if-let bindings
    (begin body ...)))

(define-syntax-rule (when-let1 bindings body ...)
  (if-let1 bindings
    (begin body ...)))

(define-syntax-rule (when-letn bindings body ...)
  (if-letn bindings
    (begin body ...)))

;;; let-macros.scm ends here

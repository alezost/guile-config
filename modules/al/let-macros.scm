;;; let-macros.scm --- `let' syntax family

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

;; This file provides `let+' which is an augmented `let*' and
;; `if-let'-like macros.

;;; Code:

(define-module (al let-macros)
  #:use-module (al fp-utils)
  #:export (let+
            if-let
            if-let1
            if-letn
            when-let
            when-let1
            when-letn))

(define-syntax let+
  (syntax-rules (<= =>)
    ;; Base case: a single binding without extra clauses.
    ((let+ () body ...)
     (begin body ...))

    ;; Multiple bindings without extra clauses: reduce bindings.
    ((let+ ((var expr)
            rest-bindings ...)
       body ...)
     (let ((var expr))
       (let+ (rest-bindings ...)
         body ...)))

    ;; Multiple bindings with extra <= clause: reduce clauses.
    ((let+ ((var expr
                 (<= procedures ...))
            rest-bindings ...)
       body ...)
     (when-let ((var (and<= expr procedures ...)))
       (let+ (rest-bindings ...)
         body ...)))

    ((let+ ((var expr
                 (<= procedures ...)
                 rest-clauses ...)
            rest-bindings ...)
       body ...)
     (when-let ((var (and<= expr procedures ...)))
       (let+ ((var var rest-clauses ...)
              rest-bindings ...)
         body ...)))

    ;; Multiple bindings with extra => clause: reduce clauses.
    ((let+ ((var expr
                 (=> procedures ...)
                 rest-clauses ...)
            rest-bindings ...)
       body ...)
     (let+ ((var (and=> expr procedures ...)
                 rest-clauses ...)
            rest-bindings ...)
       body ...))))

(define-syntax if-let
  (syntax-rules (<= =>)
    ;; Base case: a single binding without extra clauses.
    ((_ ((var expr)) then else)
     (let ((var expr))
       (if var then else)))

    ;; Multiple bindings without extra clauses: reduce bindings.
    ((if-let ((var expr)
              rest-bindings ...)
       then else)
     (let ((var expr))
       (if var
         (if-let (rest-bindings ...) then else)
         else)))

    ;; Multiple bindings with extra <= clause: reduce clauses.
    ((if-let ((var expr
                   (<= procedures ...)
                   rest-clauses ...)
              rest-bindings ...)
       then else)
     (if-let ((var (and<= expr identity procedures ...)
                   rest-clauses ...)
              rest-bindings ...)
       then else))

    ;; Multiple bindings with extra => clause: reduce clauses.
    ((if-let ((var expr
                   (=> procedures ...)
                   rest-clauses ...)
              rest-bindings ...)
       then else)
     (if-let ((var (and=> expr procedures ...)
                   rest-clauses ...)
              rest-bindings ...)
       then else))

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
       (let+ (rest ...)
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
     (let+ (first)
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

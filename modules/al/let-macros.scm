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
;; `if-let'-like macros.  Along with usual (NAME EXPRESSION) bindings,
;; all these macros accept (NAME EXPRESSION CLAUSES ...) forms.  See
;; `let+' for details.

;;; Code:

(define-module (al let-macros)
  #:export (let+
            if-let
            if-let1
            if-letn
            when-let
            when-let1
            when-letn))

(define-syntax compose-call
  (syntax-rules ()
    "Call composed PROCEDURES on EXPR.
More precisely, (compose-call EXPR PROC1 PROC2 ... PROCn)
expands to (PROCn ... (PROC2 (PROC1 EXPR)))."
    ((_ expr)
     expr)
    ((_ expr proc1 rest ...)
     (compose-call (proc1 expr) rest ...))))

(define-syntax let+
  (syntax-rules (<= =>)
    "Augmented `let*'.

It has the following form:

  (let+ ((NAME EXPRESSION [CLAUSES ...])
         ...)
    BODY
    ...)

If CLAUSES are not specified, then `let+' is equivalent to `let*'.
Each clause from CLAUSES should have one of the following forms:

  (<= PROCEDURES ...)

    each procedure from PROCEDURES is called with NAME variable as a
    single argument.  If any of them returns #f, `let+' exits
    immediately with #f value without evaluating BODY.

  (=> PROCEDURES ...)

    PROCEDURES are composed from left to right (i.e., the leftmost
    procedure is called the first) and called on NAME variable.  The
    result is taken as the new value for NAME.

Example:

  (let+ ((file \"/tmp/foo\"
               (<= file-exists? symlink?)
               (=> canonicalize-path)
               (<= (cut string-suffix? \".scm\" <>))))
    (basename file))

Here, we check if FILE exists and is a symlink (there is no `symlink?'
function in Guile by the way), and if so, file name is canonicalized.
Finally, if the canonical name ends with \".scm\", its basename is
returned.  So the above `let+' expands to something like this:

  (let* ((file \"/tmp/foo\")
         (file (and (file-exists? file)
                    (symlink? file)
                    file)))
    (and file
         (let* ((file (canonicalize-path file))
                (file (and ((cut string-suffix? \".scm\" <>) file)
                           file)))
           (and file (basename file)))))"

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
     (when-letn ((var expr)
                 (var (and (procedures var) ... var)))
       (let+ (rest-bindings ...)
         body ...)))

    ((let+ ((var expr
                 (<= procedures ...)
                 rest-clauses ...)
            rest-bindings ...)
       body ...)
     (when-letn ((var expr)
                 (var (and (procedures var) ... var)))
       (let+ ((var var
                   rest-clauses ...)
              rest-bindings ...)
         body ...)))

    ;; Multiple bindings with extra => clause: reduce clauses.
    ((let+ ((var expr
                 (=> procedures ...)
                 rest-clauses ...)
            rest-bindings ...)
       body ...)
     (let+ ((var (compose-call expr procedures ...)
                 rest-clauses ...)
            rest-bindings ...)
       body ...))))

(define-syntax if-let
  (syntax-rules (<= =>)
    "Usual `if-let' construct with optional auxiliary CLAUSES.

It has the following form:

  (if-let ((NAME EXPRESSION [CLAUSES ...])
           ...)
    THEN
    ELSE)

See `let+' for the meaning of CLAUSES.

If all NAME variables pass all checks, evaluate THEN.  Otherwise (if
NAME is #f or if one of the CLAUSES returns #f), evaluate ELSE."

    ;; Base case: no bindings.
    ((_ () then else)
     then)

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
     (let ((var expr))
       (if-let ((var (and var (procedures var) ... var)
                     rest-clauses ...)
                rest-bindings ...)
         then else)))

    ;; Multiple bindings with extra => clause: reduce clauses.
    ((if-let ((var expr
                   (=> procedures ...))
              rest-bindings ...)
       then else)
     (if-let ((var expr)
              (var (procedures var))
              ...
              rest-bindings ...)
       then else))
    ((if-let ((var expr
                   (=> procedures ...)
                   rest-clauses ...)
              rest-bindings ...)
       then else)
     (if-let ((var expr)
              (var (procedures var))
              ...
              (var var
                   rest-clauses ...)
              rest-bindings ...)
       then else))

    ;; No else clause.
    ((_ bindings then)
     (if-let bindings then #f))))

(define-syntax if-let1
  (syntax-rules ()
    "Call `if-let' on the first binding and `let+' on the rest.

For example,

  (if-let1 ((a 1)
            (b 2)
            (c 3))
    (+ a b c)
    0)

expands to

  (if-let ((a 1))
    (let+ ((b 2)
           (c 3))
      (+ a b c))
    0)"
    ((_ () then else)
     then)
    ((_ (first rest ...) then else)
     (if-let (first)
       (let+ (rest ...)
         then)
       else))
    ((_ bindings then)
     (if-let1 bindings then #f))))

(define-syntax if-letn
  (syntax-rules ()
    "Call `if-let' on the last binding and `let+' on the rest.

For example,

  (if-letn ((a 1)
            (b 2)
            (c 3))
    (+ a b c)
    0)

expands to

  (let+ ((a 1)
         (b 2))
    (if-let ((c 3))
      (+ a b c)
      0))"
    ((_ () then else)
     then)
    ((_ (first rest ...) then else)
     (let+ (first)
       (if-letn (rest ...)
         then
         else)))
    ((_ bindings then)
     (if-let1 bindings then #f))))

(define-syntax-rule (when-let bindings body ...)
  "`if-let' without ELSE clause.
The difference between `when-let' and `let+' is the following.
`let+' does not evaluate its BODY only if \"<=\" clause returns #f,
`when-let' does not evaluate its BODY if any clause or variable value is
#f."
  (if-let bindings
    (begin body ...)))

(define-syntax-rule (when-let1 bindings body ...)
  "`if-let1' without ELSE clause."
  (if-let1 bindings
    (begin body ...)))

(define-syntax-rule (when-letn bindings body ...)
  "`if-letn' without ELSE clause."
  (if-letn bindings
    (begin body ...)))

;;; let-macros.scm ends here

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

;; This file provides augmented `let'-like macros.  Along with usual
;; (NAME EXPRESSION) bindings, all macros accept (NAME EXPRESSION
;; CLAUSES ...) forms.  See `if-let-' for details.

;;; Code:

(define-module (al let-macros)
  #:export (let-                ; alias for `when-let-'
            if-let              ; alias for `if-let+'
            if-let-
            if-let+
            if-let1
            if-letn
            when-let            ; alias for `when-let+'
            when-let-
            when-let+
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

(define-syntax if-let-
  (syntax-rules (<= =>)
    "Augmented `let'-like construct.

It has the following form:

  (if-let- ((NAME EXPRESSION [CLAUSES ...])
            ...)
    THEN
    ELSE)

If CLAUSES are not specified, then `if-let-' is equivalent to `let*'
with THEN body (ELSE is not evaluated).

Each clause from CLAUSES should have one of the following forms:

  (<= PROCEDURES ...)

    each procedure from PROCEDURES is called with NAME variable as a
    single argument.  If any of them returns #f, `if-let-' evaluates
    ELSE.  Otherwise, evaluate THEN.

  (=> PROCEDURES ...)

    PROCEDURES are composed from left to right (i.e., the leftmost
    procedure is called the first) and called on NAME variable.  The
    result is taken as the new value for NAME.

Example:

  (if-let- ((file \"/tmp/foo\"
                  (<= file-exists? symlink?)
                  (=> canonicalize-path)
                  (<= (cut string-suffix? \".scm\" <>))))
    (basename file)
    (begin
      (format #t \"~a is not a suitable file~%\" file)
      #f))

Here, we check if FILE exists and is a symlink (there is no `symlink?'
function in Guile by the way), and if so, file name is canonicalized.
Finally, if the canonical name ends with \".scm\", its basename is
returned.  If any check fails (i.e., if file does not exist or is not a
symlink or its canonical name does not end with \".scm\"), a message is
displayed and #f is returned.

So the above `if-let-' expands to this:

(let ((file \"/tmp/foo\"))
  (if (and (file-exists? file) (symlink? file))
      (let ((file (canonicalize-path file)))
        (if ((cut string-suffix? \".scm\" <>) file)
            (basename file)
            (begin (format #t \"~a is not a suitable file~%\" file) #f)))
      (begin (format #t \"~a is not a suitable file~%\" file) #f)))"

    ;; Base case: a single binding without extra clauses.
    ((_ () then else)
     then)

    ;; Multiple bindings without extra clauses: reduce bindings.
    ((if-let- ((var expr)
               rest-bindings ...)
       then else)
     (let ((var expr))
       (if-let- (rest-bindings ...)
         then else)))

    ;; Multiple bindings with extra <= clause: reduce clauses.
    ((if-let- ((var expr
                    (<= procedures ...))
               rest-bindings ...)
       then else)
     (let ((var expr))
       (if (and (procedures var) ...)
         (if-let- (rest-bindings ...)
           then else)
         else)))

    ((if-let- ((var expr
                    (<= procedures ...)
                    rest-clauses ...)
               rest-bindings ...)
       then else)
     (let ((var expr))
       (if (and (procedures var) ...)
         (if-let- ((var var
                        rest-clauses ...)
                   rest-bindings ...)
           then else)
         else)))

    ;; Multiple bindings with extra => clause: reduce clauses.
    ((if-let- ((var expr
                    (=> procedures ...)
                    rest-clauses ...)
               rest-bindings ...)
       then else)
     (if-let- ((var (compose-call expr procedures ...)
                    rest-clauses ...)
               rest-bindings ...)
       then else))

    ;; No else clause.
    ((_ bindings then)
     (if-let- bindings then #f))))

(define-syntax if-let+
  (syntax-rules (<= =>)
    "Usual `if-let' construct with optional auxiliary CLAUSES.

It has the following form:

  (if-let+ ((NAME EXPRESSION [CLAUSES ...])
            ...)
    THEN
    ELSE)

See `if-let-' for the meaning of CLAUSES.

If all NAME variables pass all checks, evaluate THEN.
Otherwise, evaluate ELSE.

The difference between `if-let-' and `if-let+' is the following:
`if-let-' evaluates ELSE if any \"<=\" clause returns #f;
`if-let+' evaluates ELSE if any \"<=\" clause or \"=>\" clause or NAME
value is #f."

    ;; Base case: no bindings.
    ((_ () then else)
     then)

    ;; Multiple bindings without extra clauses: reduce bindings.
    ((if-let+ ((var expr)
               rest-bindings ...)
       then else)
     (let ((var expr))
       (if var
         (if-let+ (rest-bindings ...)
           then else)
         else)))

    ;; Multiple bindings with extra <= clause: reduce clauses.
    ((if-let+ ((var expr
                    (<= procedures ...))
               rest-bindings ...)
       then else)
     (let ((var expr))
       (if (and var (procedures var) ...)
         (if-let+ (rest-bindings ...)
           then else)
         else)))

    ((if-let+ ((var expr
                    (<= procedures ...)
                    rest-clauses ...)
               rest-bindings ...)
       then else)
     (let ((var expr))
       (if (and var (procedures var) ...)
         (if-let+ ((var var
                        rest-clauses ...)
                   rest-bindings ...)
           then else)
         else)))

    ;; Multiple bindings with extra => clause: reduce clauses.
    ((if-let+ ((var expr
                    (=> procedures ...))
               rest-bindings ...)
       then else)
     (if-let+ ((var expr)
               (var (procedures var))
               ...
               rest-bindings ...)
       then else))

    ((if-let+ ((var expr
                    (=> procedures ...)
                    rest-clauses ...)
               rest-bindings ...)
       then else)
     (if-let+ ((var expr)
               (var (procedures var))
               ...
               (var var
                    rest-clauses ...)
               rest-bindings ...)
       then else))

    ;; No else clause.
    ((_ bindings then)
     (if-let+ bindings then #f))))

(define-syntax if-let1
  (syntax-rules ()
    "Call `if-let+' on the first binding and `if-let-' on the rest.

For example,

  (if-let1 ((a 1)
            (b 2)
            (c 3))
    (+ a b c)
    0)

expands to

  (if-let+ ((a 1))
    (if-let- ((b 2)
              (c 3))
      (+ a b c)
      0)
    0)"
    ((_ () then else)
     then)
    ((_ (first rest ...) then else)
     (if-let+ (first)
       (if-let- (rest ...)
         then else)
       else))
    ((_ bindings then)
     (if-let1 bindings then #f))))

(define-syntax if-letn
  (syntax-rules ()
    "Call `if-let+' on the last binding and `if-let-' on the rest.

For example,

  (if-letn ((a 1)
            (b 2)
            (c 3))
    (+ a b c)
    0)

expands to

  (if-let- ((a 1)
            (b 2))
    (if-let+ ((c 3))
      (+ a b c)
      0)
    0)"
    ((_ () then else)
     then)
    ((_ (binding) then else)
     (if-let+ (binding) then else))
    ((_ (first rest ...) then else)
     (if-let- (first)
       (if-letn (rest ...)
         then else)
       else))
    ((_ bindings then)
     (if-letn bindings then #f))))

(define-syntax-rule (when-let- bindings body ...)
  "`if-let-' without ELSE clause."
  (if-let- bindings
    (begin body ...)))

(define-syntax-rule (when-let+ bindings body ...)
  "`if-let+' without ELSE clause."
  (if-let+ bindings
    (begin body ...)))

(define-syntax-rule (when-let1 bindings body ...)
  "`if-let1' without ELSE clause."
  (if-let1 bindings
    (begin body ...)))

(define-syntax-rule (when-letn bindings body ...)
  "`if-letn' without ELSE clause."
  (if-letn bindings
    (begin body ...)))

(define-syntax-rule (let- args ...)
  "Alias for `when-let-'."
  (when-let- args ...))

(define-syntax-rule (if-let args ...)
  "Alias for `if-let+'."
  (if-let+ args ...))

(define-syntax-rule (when-let args ...)
  "Alias for `when-let+'."
  (when-let+ args ...))

;;; let-macros.scm ends here

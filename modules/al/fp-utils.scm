;;; fp-utils.scm --- Basic functional programming utilities

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

;; This file provides some additional functional programming utilities
;; that I find useful.

;;; Code:

(define-module (al fp-utils)
  #:use-module (ice-9 match)
  #:export (and=>
            and<=))

(define (and=> value . procedures)
  "Return result of consecutive applying PROCEDURES to VALUE.

More precisely, for a given list of PROCEDURES, (PROC1 PROC2 ... PROCn),
return #f if:

  VALUE is #f or
  (PROC1 VALUE) is #f or
  (PROC2 (PROC1 VALUE)) is #f or
  ...
  (PROCn ... (PROC2 (PROC1 VALUE))) is #f.

Otherwise, return (PROCn ... (PROC2 (PROC1 VALUE)))."
  (and value
       (match procedures
         (() value)
         ((proc . rest)
          (apply and=> (proc value) rest)))))

(define-syntax-rule (and<= value procedures ...)
  "Return VALUE if (PROC VALUE) is not false for all PROCEDURES.

More precisely, for a given list of PROCEDURES, (PROC1 PROC2 ... PROCn),
return #f if:

  (PROC1 VALUE) is #f or
  (PROC2 VALUE) is #f or
  ...
  (PROCn VALUE) is #f.

Otherwise, return VALUE."
  (let ((val value))
    (and (procedures val)
         ...
         val)))

;;; fp-utils.scm ends here

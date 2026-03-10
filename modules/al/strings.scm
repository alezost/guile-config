;;; strings.scm --- String utilities

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

;; This file provides additional functionality to work with strings.

;;; Code:

(define-module (al strings)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (string->bool
            string-trim-left    ; alias for `string-trim'
            mapconcat
            comma-separated
            min-string))

(define (string->bool string)
  "Convert STRING into the boolean value.
If STRING is \"y\"/\"yes\"/\"true\", return #t.
If STRING is \"n\"/\"no\"/\"false\", return #f."
  (cond
   ((member string '("y" "yes" "true"))
    #t)
   ((member string '("n" "no" "false"))
    #f)
   (else
    (format (current-error-port)
            "'~a' should be a string with boolean value~%"
            string)
    (raise-exception &error))))

(define string-trim-left string-trim)

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

(define (min-string . strings)
  "Like `min' but performed on STRINGS.
Return #f if STRINGS are not specified."
  (reduce (lambda (cur min)
            (if (string< cur min) cur min))
          #f
          strings))

;;; strings.scm ends here

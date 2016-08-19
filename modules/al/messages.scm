;;; messages.scm --- Procedures for displaying messages

;; Copyright © 2015 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created:  6 Mar 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides several procedures for displaying strings that may
;; be indented by some spaces.

;;; Code:

(define-module (al messages)
  #:use-module (srfi srfi-26)
  #:export (message
            message-proc
            message0
            message1
            print-output
            print-error
            leave))

(define* (indent-string level #:optional (step 3))
  "Return string of LEVEL * STEP spaces."
  (make-string (* level step) #\space))

(define (message indent-level dest str . args)
  "Print message indented to INDENT-LEVEL."
  (let ((str (string-append (indent-string indent-level)
                            str "~%")))
    (apply format dest str args)))

(define* (message-proc #:key (indent-level 0) (destination #t))
  "Return procedure for displaying a message."
  (cut message indent-level destination <> <...>))

(define message0 (message-proc #:indent-level 0))
(define message1 (message-proc #:indent-level 1))

(define print-output message0)
(define print-error (message-proc #:destination (current-error-port)))

(define (leave format-string . args)
  "Print message to STDERR and exit."
  (print-error format-string args)
  (exit 1))

;;; messages.scm ends here

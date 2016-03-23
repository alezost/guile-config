;;; display.scm --- Utilities related to $DISPLAY environment

;; Copyright © 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 18 Mar 2016

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

;; This file provides procedures to find used/unused $DISPLAY and other
;; related utilities.

;;; Code:

(define-module (al display)
  #:use-module (al processes)
  #:export (with-display
            display-number->string
            display-string->number
            display-used?
            display-unused?
            first-used-display
            first-unused-display))

(define-syntax-rule (with-display display body ...)
  "Add DISPLAY to the current environment and run BODY."
  (environment-excursion
   (lambda () (setenv "DISPLAY" display))
   (lambda () body ...)))

(define (display-number->string number)
  "Return $DISPLAY string by NUMBER."
  (string-append ":" (number->string number)))

(define (display-string->number string)
  "Return $DISPLAY number by STRING."
  (string->number
   (substring string
              (+ 1 (string-index string #\:)))))

(define (display-used? display)
  "Return #t if $DISPLAY is used and available for the current user."
  (and (not (display-unused? display))
       (with-display display
         ;; 'xset' errors if DISPLAY is not available.  Is there a
         ;; better way to find available DISPLAY?
         (zero? (system*-no-output "xset" "q")))))

(define (display-unused? display)
  "Return #t if $DISPLAY is unused."
  ;; Checking /tmp/.X<num>-lock is unreliable: for example, there is no
  ;; lock file in /tmp (where is it?) for X server started with GDM.
  ;; Running "DISPLAY=:0 xset q" or alike (as advised at
  ;; <http://stackoverflow.com/questions/637005/x-server-running>) also
  ;; doesn't work as you get "unable to open display" for both an unused
  ;; display and a display used by GDM's X server.

  ;; Is checking a socket file in "/tmp/.X11-unix/" reliable?
  (let* ((num (substring display
                         (+ 1 (string-index display #\:))))
         (socket (string-append "/tmp/.X11-unix/X" num)))
    (not (file-exists? socket))))

(define* (first-matching-display pred #:optional (count +inf.0))
  "Return the first $DISPLAY matching PRED.
Return #f if there is no matching display among the first COUNT ones."
  (let loop ((num 0))
    (if (>= num count)
        #f
        (let ((str (display-number->string num)))
          (if (pred str)
              str
              (loop (1+ num)))))))

(define* (first-used-display #:optional (count +inf.0))
  "Return the first used $DISPLAY.
Return #f if there is no used display among the first COUNT ones."
  (first-matching-display display-used? count))

(define (first-unused-display)
  "Return the first unused $DISPLAY."
  (first-matching-display display-unused?))

;;; display.scm ends here

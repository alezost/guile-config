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
  #:export (display-unused?
            first-unused-display))

(define (display-unused? number)
  "Return #t if $DISPLAY ':NUMBER' is unused."
  ;; Checking /tmp/.X<num>-lock is unreliable: for example, there is no
  ;; lock file in /tmp (where is it?) for X server started with GDM.
  ;; Running "DISPLAY=:0 xset q" or alike (as advised at
  ;; <http://stackoverflow.com/questions/637005/x-server-running>) also
  ;; doesn't work as you get "unable to open display" for both an unused
  ;; display and a display used by GDM's X server.

  ;; Is checking a socket file in "/tmp/.X11-unix/" reliable?
  (let ((socket (string-append "/tmp/.X11-unix/X"
                               (number->string number))))
    (not (file-exists? socket))))

(define (first-unused-display)
  "Return the first unused $DISPLAY."
  (let loop ((num 0))
    (if (display-unused? num)
        (string-append ":" (number->string num))
        (loop (1+ num)))))

;;; display.scm ends here

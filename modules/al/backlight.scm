;;; backlight.scm --- Screen backlight utilities

;; Copyright © 2018 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 14 Dec 2018

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

;; This file provides code related to screen backlight (currently, it is
;; just a wrapper for 'xbacklight').

;;; Code:

(define-module (al backlight)
  #:use-module (ice-9 regex)
  #:use-module (al processes)
  #:export (call-xbacklight
            get-backlight
            set-backlight
            increase-backlight
            decrease-backlight))

(define (call-xbacklight . args)
  "Call 'xbacklight' with ARGS."
  (apply system* "xbacklight" args))

(define get-backlight
  (let ((num-rx (make-regexp "[0-9.]+")))
    (lambda ()
      "Return percentage of the current screen backlight."
      (let ((output (system*-output "xbacklight" "-get")))
        (and=> (regexp-exec num-rx output)
               (compose string->number match:substring))))))

(define (set-backlight percentage)
  "Set screen backlight to PERCENTAGE."
  (call-xbacklight "-set" (number->string percentage)))

(define (increase-backlight percentage)
  "Increase screen backlight by PERCENTAGE."
  (call-xbacklight "-inc" (number->string percentage)))

(define (decrease-backlight percentage)
  "Decrease screen backlight by PERCENTAGE."
  (call-xbacklight "-dec" (number->string percentage)))

;;; backlight.scm ends here

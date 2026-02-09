;;; backlight.scm --- Screen backlight utilities

;; Copyright © 2018–2026 Alex Kost

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

;; This file provides code related to screen backlight.  Getting/setting
;; backlight brightness is performed by reading/writing to files from
;; "/sys/class/backlight/*/" directory.  One way to make
;; "/sys/class/backlight/*/brightness" file writable by a user is to
;; make a udev rule.  See <https://wiki.archlinux.org/title/Backlight>.

;;; Code:

(define-module (al backlight)
  #:use-module (ice-9 ftw)
  #:use-module (al files)
  #:use-module (al utils)
  #:use-module (al let-macros)
  #:export (backlight-available?
            get-backlight
            set-backlight
            increase-backlight
            decrease-backlight))

(define-lazy backlight-directory
  "Return system directory with backlight device.
Return #f if no such directory found."
  (let ((root-dir "/sys/class/backlight"))
    (when-let ((subdirs (scandir root-dir))
               (devices (filter (lambda (name)
                                  (not (member name '("." ".."))))
                                subdirs))
               (device (and (not (null? devices))
                            (car devices))))
      (and (file-exists? (build-file-name root-dir device "brightness"))
           (build-file-name root-dir device)))))

(define (backlight-available?)
  "Return #t if backlight is available for the current machine."
  (->bool (backlight-directory)))

(define (backlight-directory-file name)
  "Return file NAME from `backlight-directory'."
  (build-file-name (backlight-directory) name))

(define-lazy max-brightness
  (read-number-from-file (backlight-directory-file "max_brightness")))

(define (current-brightness)
  (read-number-from-file (backlight-directory-file "actual_brightness")))

(define (get-backlight)
  "Return percentage of the current screen backlight.
Percentage is an integer from 0 to 100."
  (round (* 100
            (/ (current-brightness)
               (max-brightness)))))

(define (set-backlight value)
  "Set screen backlight according to VALUE.

VALUE should be either:

  - an integer from 0 to 100,

  - a string in \"sN\" format, where \"N\" is an integer and \"s\" is an
    optional sign i.e., \"s\" is either:

    \"+\": to increase backlight by N,
    \"-\": to decrease backlight by N,
    empty string: to set backlight to N.

Return percentage of the current actual backlight if it was set
successfully.  Return #f otherwise."
  (define (backlight val)
    (cond
     ((integer? val)
      (inexact->exact
       (round (* (/ (max-brightness) 100)
                 val))))
     ((string? val)
      (let ((first-char (car (string->list val))))
        (case first-char
          ((#\+)
           (backlight (+ (get-backlight)
                         (string->number (substring val 1)))))
          ((#\-)
           (backlight (- (get-backlight)
                         (string->number (substring val 1)))))
          (else
           (backlight (string->number val))))))
     (else #f)))

  (when-let ((bl (backlight value)))
    (with-output-to-file (backlight-directory-file "brightness")
      (lambda () (write bl)))
    (get-backlight)))

(define (increase-backlight percentage)
  "Increase screen backlight by PERCENTAGE.
Return percentage of the current actual backlight."
  (set-backlight (+ (get-backlight) percentage)))

(define (decrease-backlight percentage)
  "Decrease screen backlight by PERCENTAGE.
Return percentage of the current actual backlight."
  (increase-backlight (- percentage)))

;;; backlight.scm ends here

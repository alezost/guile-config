;;; sound.scm --- Sound utilities

;; Copyright © 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 14 Jul 2016

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

;; This file provides code related to sound (currently: calling 'amixer'
;; and parsing its output).

;;; Code:

(define-module (al sound)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-2)
  #:use-module (al records)
  #:export (make-sound
            sound*
            sound?
            sound-control
            sound-volume
            sound-muted?
            get-sound
            set-sound
            call-amixer
            parse-amixer-output))

(define-record-type* <sound>
  make-sound sound*
  sound?
  (control sound-control #f)    ; name of a simple control
  (volume  sound-volume #f)     ; number from 0 to 100
  (muted?  sound-muted? #f))    ; on or off

(define (call-amixer . args)
  "Call 'amixer' with ARGS and return its output."
  (let* ((port (apply open-pipe* OPEN_READ "amixer" args))
         (out  (read-string port)))
    (close-pipe port)
    out))

(define parse-amixer-output
  (let ((control-rx  (make-regexp "Simple mixer control '([^']+)"))
        (playback-rx (make-regexp
                      "Playback [0-9]+ \\[([0-9]+)%\\].* \\[(on|off)\\]")))
    (lambda (output)
      "Parse 'amixer' OUTPUT and return <sound> record.
Return #f if OUTPUT cannot be parsed."
      (and-let* ((control-match  (regexp-exec control-rx output))
                 (playback-match (regexp-exec playback-rx output
                                              (match:end control-match))))
        (sound*
         #:control (match:substring control-match 1)
         #:volume (string->number (match:substring playback-match 1))
         #:muted? (string=? "off" (match:substring playback-match 2)))))))

(define (get-sound . args)
  "Call 'amixer sget ARGS ...', parse its outputs and return <sound>
structure."
  (parse-amixer-output (apply call-amixer "sget" args)))

(define (set-sound . args)
  "Call 'amixer sset ARGS ...'."
  (apply system* "amixer" "sset" args))

;;; sound.scm ends here

;;; files.scm --- Procedures for working with files

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

;; This file provides several additional procedures to perform various
;; actions on files.

;;; Code:

(define-module (al files)
  #:export (symlink?
            file-exists??
            unique-filename))

(define (symlink? file)
  "Return #t if FILE is a symbolic link."
  (eq? (stat:type (lstat file)) 'symlink))

(define (file-exists?? filename)
  "Check if FILENAME exists in the file system.
This procedure is similar to 'file-exists?' except it returns #t on a
broken link (i.e., on a file that is a symlink pointed to a non-existing
target)."
  (or (file-exists? filename)
      (catch 'system-error
        (lambda () (->bool (readlink filename)))
        (const #f))))

(define* (unique-filename basename #:optional (i 1))
  "Return unique filename based on BASENAME and a number I."
  (let ((filename (string-append basename (number->string i))))
    (if (file-exists? filename)
        (unique-filename basename (+ i 1))
        filename)))

;;; files.scm ends here

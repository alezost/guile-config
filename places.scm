;;; places.scm --- Where my files are placed

;; Copyright © 2015 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 14 Feb 2015

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

;; This file provides procedures returning paths to my various files.

;;; Code:

(define-module (al places)
  #:export (home-file
            config-file))

(define home-file
  (let ((home (getenv "HOME")))
    (lambda* (#:optional filename)
      "Return file path by appending FILENAME to a home directory.
If FILENAME is not specified, return the home directory."
      (if filename
          (string-append home "/" filename)
          home))))

(define* (config-file #:optional filename)
  "Return file path by appending FILENAME to a config directory.
If FILENAME is not specified, return the config directory."
  (let* ((config-dir "config")
         (rel-name (if filename
                       (string-append config-dir "/" filename)
                       config-dir)))
    (home-file rel-name)))

;;; places.scm ends here

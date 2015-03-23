;;; links.scm --- Procedures for working with symlinks

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

;; This file provides several procedures build around <link> record to
;; simplify a work with symlinks.

;;; Code:

(define-module (al links)
  #:use-module (srfi srfi-9)
  #:use-module (al files)
  #:export (make-link
            link?
            link-target
            link-filename
            link-string
            link-exists?
            create-link))

(define-record-type <link>
  (make-link filename target)
  link?
  (filename link-filename)
  (target   link-target))

(define (link-string link)
  "Return a string with printable representation of LINK record."
  (let ((filename (link-filename link))
        (target   (link-target link)))
    (string-append filename " → " target)))

(define (link-exists? link)
  "Return #t if symlink defined by LINK record exists in the file system."
  (let ((filename (link-filename link))
        (target   (link-target link)))
    (and (file-exists? filename)
         (symlink? filename)
         (equal? (readlink filename) target))))

(define (create-link link)
  "Create symlink defined by LINK record."
  (symlink (link-target link)
           (link-filename link)))

;;; links.scm ends here

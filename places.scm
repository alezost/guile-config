;;; places.scm --- Where my files are placed

;; Copyright © 2015, 2016 Alex Kost

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
  #:use-module (al utils)
  #:export (home-file
            bin-file
            config-file
            guix-config-file
            guix-script-file
            guix-system-file
            guix-manifest-file
            guix-profile))

(define (home-file . file-parts)
  "Return file name from my home directory."
  (apply build-file-name (getenv "HOME") file-parts))

(define (bin-file . file-parts)
  "Return file name from my bin directory."
  (apply home-file "bin" file-parts))

(define (config-file . file-parts)
  "Return file name from my config directory."
  (apply home-file "config" file-parts))

(define (guix-config-file . file-parts)
  "Return file name from my Guix config directory."
  (apply config-file "guix" file-parts))

(define* (guix-script-file #:optional name)
  "Return file name of my Guix script NAME."
  (apply guix-config-file "scripts"
         (if name
             (list name)
             '())))

(define* (guix-system-file #:optional name)
  "Return file name of my Guix system NAME."
  (apply guix-config-file "system-config"
         (if name
             (list (string-append "os-" name ".scm"))
             '())))

(define* (guix-manifest-file #:optional name)
  "Return file name of my Guix manifest file for profile NAME."
  (apply guix-config-file "user-config"
         (if name
             (list (string-append "manifest-" name ".scm"))
             '())))

(define (guix-profile name)
  "Return file name of my Guix profile NAME."
  (home-file ".guix-profiles" name name))

;;; places.scm ends here

;;; sources.scm --- Procedures for working with sources

;; Copyright © 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 31 Jan 2016

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

;; This file provides several procedures build around <source> record.
;; Source has an URI of the remote origin, and a local directory where
;; it should be put.  Currently a source is a git source (it is easy to
;; support other type of sources, but I only need git).

;;; Code:

(define-module (al sources)
  #:use-module (al records)
  #:export (make-source
            source*
            source?
            source-uri
            source-directory
            fetch-source))

(define-record-type* <source>
  make-source source*
  source?
  (uri       source-uri #f)
  (directory source-directory #f))

(define (git-clone repository directory)
  "Clone git REPOSITORY to DIRECTORY."
  (system* "git" "clone" repository directory))

(define (git-pull directory)
  "Run 'git pull' in DIRECTORY."
  (system* "git"
           (string-append "--work-tree=" directory)
           (string-append "--git-dir=" directory "/.git")
           "pull" "--verbose"))

(define (fetch-source source)
  "Fetch SOURCE from remote to local place.
Initialize the local directory or update it if it already exists."
  (let ((uri (source-uri source))
        (dir (source-directory source)))
    (if (file-exists? dir)
        (git-pull dir)
        (git-clone uri dir))))

;;; sources.scm ends here

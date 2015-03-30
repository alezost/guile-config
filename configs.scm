;;; configs.scm --- Procedures for working with configs

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

;; This file provides several procedures build around <config> record to
;; simplify a work with configurations (list of symlinks).

;;; Code:

(define-module (al configs)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (al files)
  #:use-module (al links)
  #:use-module (al messages)
  #:export (make-config
            config?
            config-name
            config-links
            show-config
            deploy-config))

(define-record-type <config>
  (make-config name links)
  config?
  (name  config-name)
  (links config-links))

(define (show-link link)
  "Display information about LINK record."
  (define message* (message-proc #:indent-level 1))
  (message* (link-string link)))

(define* (deploy-link link #:optional (name-proc unique-filename))
  "Deploy LINK record if needed.
If LINK's target file already exists, it will be renamed into a name
returned by NAME-PROC procedure."
  (define message* (message-proc #:indent-level 1))

  (define (rename-file-unique filename)
    (let ((new-name (name-proc filename)))
      (rename-file filename new-name)
      (message* "Old file has been renamed into '~a'." new-name)))

  (let ((filename (link-filename link)))
    (if (link-exists? link)
        (message* "A proper link '~a' already exists."
                  (link-string link))
        (let ((dir (dirname filename)))
          (unless (file-exists? dir)
            (mkdir-with-parents dir))
          (when (file-exists?? filename)
            (rename-file-unique filename))
          (create-link link)
          (message* "Link '~a' has been created."
                    (link-string link))))))

(define (show-config config)
  "Show info about CONFIG record."
  (define message* (message-proc #:indent-level 0))
  (let ((name  (config-name config))
        (links (config-links config)))
    (message* "'~a' configuration:" name)
    (map show-link links)))

(define* (deploy-config config #:optional (name-proc unique-filename))
  "Deploy (create symlinks) CONFIG record.
See 'deploy-link' for the meaning of NAME-PROC."
  (define message* (message-proc #:indent-level 0))
  (let ((name  (config-name config))
        (links (config-links config)))
    (message* "Deploying '~a' configuration..." name)
    (map (cut deploy-link <> name-proc) links)))

;;; config.scm ends here

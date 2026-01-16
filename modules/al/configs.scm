;;; configs.scm --- Procedures for working with configs

;; Copyright © 2015–2026 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 6 Mar 2015

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

;; This file provides several procedures build around <config> record to
;; simplify a work with configurations (list of symlinks).

;;; Code:

(define-module (al configs)
  #:use-module (srfi srfi-26)
  #:use-module (al let-macros)
  #:use-module (al files)
  #:use-module (al links)
  #:use-module (al messages)
  #:use-module (al records)
  #:use-module (al sources)
  #:export (make-config
            config*
            config?
            config-name
            config-source
            config-links
            show-config
            fetch-config
            deploy-config))

(define-record-type* <config>
  make-config config*
  config?
  (name   config-name #f)
  (source config-source #f)
  (links  config-links '()))

(define (show-source source)
  "Display information about SOURCE record."
  (message1 "Source URI: ~a" (source-uri source))
  (message1 "Directory:  ~a" (source-directory source)))

(define (show-link link)
  "Display information about LINK record."
  (message1 (link-string link)))

(define* (deploy-link link #:optional (name-proc unique-filename))
  "Deploy LINK record if needed.
If LINK's target file already exists, it will be renamed into a name
returned by NAME-PROC procedure."
  (define (rename-file-unique filename)
    (let ((new-name (name-proc filename)))
      (rename-file filename new-name)
      (message1 "Old file has been renamed to '~a'." new-name)))

  (if (link-exists? link)
      (message1 "A proper link already exists: '~a'."
                (link-string link))
      (let* ((filename (link-filename link))
             (target   (link-target link))
             (dir      (dirname filename)))
        (unless (file-exists? dir)
          (mkdir-with-parents dir))
        (if (with-directory-excursion dir
              (file-exists? target))
            (begin
              (when (file-exists?? filename)
                (rename-file-unique filename))
              (create-link link)
              (message1 "Link has been created: '~a'."
                        (link-string link)))
            (message1 "Target does not exists: '~a'."
                      target)))))

(define (show-config config)
  "Show info about CONFIG record."
  (let ((name   (config-name config))
        (source (config-source config))
        (links  (config-links config)))
    (message0 "'~a' configuration:" name)
    (when source (show-source source))
    (map show-link links)))

(define (fetch-config config)
  "Fetch source of CONFIG record."
  (when-let ((source (config-source config)))
    (message0 "Fetching '~a' configuration source..."
              (config-name config))
    (fetch-source source)))

(define* (deploy-config config #:optional (name-proc unique-filename))
  "Deploy (create symlinks) CONFIG record.
See 'deploy-link' for the meaning of NAME-PROC."
  (when-let ((links (config-links config))
             (not-empty (not (null? links)))
             (name (config-name config)))
    (message0 "Deploying '~a' configuration..." name)
    (map (cut deploy-link <> name-proc) links)))

;;; config.scm ends here

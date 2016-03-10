;;; files.scm --- Procedures for working with files

;; Copyright © 2015, 2016 Alex Kost

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

;; The following procedures originate from (guix build utils) module:
;;
;; - symlink? (from "symbolic-link?");
;; - executable? (from "executable-file?");
;; - mkdir-with-parents (from "mkdir-p");
;; - with-directory-excursion;
;; - which;
;; - find-files;
;; - delete-file-recursively.

;;; Code:

(define-module (al files)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (al utils)
  #:export (symlink?
            executable?
            file-exists??
            canonicalize-file-name
            parent-directory
            mkdir-with-parents
            which
            which-but
            program-exists?
            first-existing-program
            first-existing-file
            with-directory-excursion
            unique-filename
            find-files
            find-matching-files
            delete-file-recursively
            read-file))

(define (symlink? file)
  "Return #t if FILE is a symbolic link."
  (eq? (stat:type (lstat file)) 'symlink))

(define (executable? file)
  "Return #t if FILE exists and is executable (not a directory)."
  (let ((s (stat file #f)))
    (and s
         (not (eq? (stat:type s) 'directory))
         (not (zero? (logand (stat:mode s) #o100))))))

(define (file-exists?? filename)
  "Check if FILENAME exists in the file system.
This procedure is similar to 'file-exists?' except it returns #t on a
broken link (i.e., on a file that is a symlink pointed to a non-existing
target)."
  (or (file-exists? filename)
      (catch 'system-error
        (lambda () (->bool (readlink filename)))
        (const #f))))

(define (canonicalize-file-name file-name)
  "Return the canonical name of FILE-NAME.
This procedure is similar to 'canonicalize-path' except it returns #f
instead of raising an error."
  (catch 'system-error
    (lambda () (canonicalize-path file-name))
    (const #f)))

(define (parent-directory file)
  "Return FILE's parent directory.
FILE should be an absolute file name."
  ;; Trim trailing slash as FILE may be "/tmp/foo" or "/tmp/foo/".
  (let* ((file       (string-trim-right file #\/))
         (last-slash (string-rindex file #\/))
         (parent     (and last-slash
                          (substring file 0 last-slash))))
    ;; For "/foo" and "/" the parent is "/".
    (if (or (not parent)
            (string=? "" parent))
        "/"
        parent)))

(define (mkdir-with-parents dir)
  "Create directory DIR and all its ancestors."
  (let ((not-slash (char-set-complement (char-set #\/))))
    (let loop ((components (string-tokenize dir not-slash))
               (root ""))
      (match components
        ((head tail ...)
         (let ((file (string-append root "/" head)))
           (unless (file-exists? file)
             (mkdir file))
           (loop tail file)))
        (_ #t)))))

(define (which program)
  "Return full file name of PROGRAM found in $PATH.
Return #f if PROGRAM is not found."
  (search-path (split-path) program))

(define (which-but program . files)
  "Return full file name of the first PROGRAM found in $PATH which is
not one of FILES.
Return #f if PROGRAM is not found."
  (let ((files (filter-map canonicalize-file-name files)))
    (let loop ((dirs (split-path)))
      (match dirs
        (() #f)
        ((dir rest-dirs ...)
         (let* ((prog (build-file-name dir program))
                (prog (and (executable? prog)
                           (canonicalize-file-name prog))))
           (if (and prog (not (member prog files)))
               prog
               (loop rest-dirs))))))))

(define (program-exists? program)
  "Check if program exists in $PATH."
  (->bool (which program)))

(define (first-existing-program . programs)
  "Return the first program from PROGRAMS found in $PATH.
Return #f if none of the PROGRAMS is available."
  (find program-exists? programs))

(define (first-existing-file . files)
  "Return the first existing file from FILES.
Return #f if none of the FILES exists."
  (find file-exists? files))

(define-syntax-rule (with-directory-excursion dir body ...)
  "Run BODY with DIR as the process's current directory."
  (let ((init (getcwd)))
   (dynamic-wind
     (lambda ()
       (chdir dir))
     (lambda ()
       body ...)
     (lambda ()
       (chdir init)))))

(define* (unique-filename basename #:optional (i 1))
  "Return unique filename based on BASENAME and a number I."
  (let ((filename (string-append basename (number->string i))))
    (if (file-exists? filename)
        (unique-filename basename (+ i 1))
        filename)))

(define* (find-files dir regexp #:key enter-tree? follow-links?)
  "Return list of files in DIR whose basenames match REGEXP.
If ENTER-TREE? is #f, scan DIR only (without subdirs), otherwise
traverse the whole DIR tree.
If FOLLOW-LINKS? is #f, do not follow symbolic links."
  (define rx
    (if (regexp? regexp)
        regexp
        (make-regexp regexp)))

  (define (enter? file stat result)
    (or (string= file dir)
        enter-tree?))

  (define (leaf-or-skip file stat result)
    (if (regexp-exec rx (basename file))
        (cons file result)
        result))

  (define (up-or-down file stat result)
    result)

  (define (error file stat errno result)
    (format (current-error-port)
            "Warning: ~a: ~a~%"
            file (strerror errno))
    result)

  (let ((stat (if follow-links? stat lstat)))
    (file-system-fold enter? leaf-or-skip up-or-down
                      up-or-down leaf-or-skip error
                      '() dir stat)))

(define (find-matching-files filename-part)
  "Return list of files whose full names begin with FILENAME-PART.
For example, (find-matching-files \"/foo/bar\") finds \"/foo/bar\",
\"/foo/bar.scm\", \"/foo/barman\", etc."
  (let ((dir (dirname filename-part)))
    (if (file-exists? dir)
        (let ((dir (if (symlink? dir)
                       (canonicalize-path dir)
                       dir))
              (rx (string-append "\\`"
                                 (regexp-quote
                                  (basename filename-part)))))
          (find-files dir rx))
        (begin
          (format (current-error-port)
                  "Warning: No such directory: ~a~%" dir)
          '()))))

(define* (delete-file-recursively dir)
  "Delete DIR recursively, like `rm -rf', without following symlinks."
  (define ok (const #t))

  (define (del-file file stat result)
    (delete-file file)
    (format #t "File '~a' has been deleted.~%" file))

  (define (del-dir dir stat result)
    (rmdir dir)
    (format #t "Directory '~a' has been deleted.~%" dir))

  (define (error file stat errno result)
    (format (current-error-port)
            "Warning: failed to delete ~a: ~a~%"
            file (strerror errno)))

  ;; Use lstat to make sure that symlinks are not followed.
  (file-system-fold ok del-file ok del-dir ok error
                    #t dir lstat))

(define (read-file filename)
  "Return a string with the contents of FILENAME."
  (call-with-input-file filename
    (lambda (port)
      (peek-char port)
      (drain-input port))))

;;; files.scm ends here

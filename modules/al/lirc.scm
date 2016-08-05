;;; lirc.scm --- LIRC client utilities

;; Copyright © 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 1 Aug 2016

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

;; This file provides some code that can be used to make a custom LIRC
;; client (written in Guile!) instead of the default 'irexec'
;; facilities.
;;
;; So instead of writing a dumb (non-programmable) "~/.lircrc"
;; configuration file, I simply use this module (in my Guile-Daemon
;; config) to connect to the lircd socket and to handle the keys I press
;; on my remote control.
;;
;; LIRC: <http://www.lirc.org>
;; irexec: <http://www.lirc.org/html/irexec.html>
;; My Guile-Daemon config: <https://github.com/alezost/guile-daemon-config>

;;; Code:

(define-module (al lirc)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-26)
  #:use-module (al records)
  #:export (make-lirc-connection
            lirc-connection*
            lirc-connection?
            lirc-connection-port
            lirc-connection-thread
            lirc-connect
            lirc-disconnect))

(define-record-type* <lirc-connection>
  make-lirc-connection lirc-connection*
  lirc-connection?
  (port   lirc-connection-port #f)
  (thread lirc-connection-thread #f))

(define key-specification->name
  (let ((rx (make-regexp "[^ ]+ [^ ]+ ([^ ]+)")))
    (lambda (specification)
      "Return key name from the key SPECIFICATION line.
Lines sent by lircd to its socket file look like this (see also the
output of 'irw'):

  0000000080010160 00 KEY_OK devinput

This function returns \"KEY_OK\" string from this specification."
      (and=> (regexp-exec rx specification)
             (cut match:substring <> 1)))))

(define* (lirc-connect key-handler
                       #:key (socket-file "/var/run/lirc/lircd"))
  "Connect to the SOCKET-FILE created by 'lircd' process.
For each key signal read from the SOCKET-FILE, call:

  (KEY-HANDLER KEY-NAME)

where KEY-NAME is a string with the name of the key.

Return <lirc-connection> structure."
  (define port (socket PF_UNIX SOCK_STREAM 0))

  (define (handle-keys-loop)
    (let ((line (read-line port)))
      (unless (eof-object? line)
        (key-handler (key-specification->name line))
        (handle-keys-loop))))

  (connect port AF_UNIX socket-file)
  (lirc-connection* #:thread (call-with-new-thread handle-keys-loop)
                    #:port port))

(define (lirc-disconnect connection)
  "Break the CONNECTION returned by 'lirc-connect'."
  (let ((port   (lirc-connection-port   connection))
        (thread (lirc-connection-thread connection)))
    (when (and thread (not (thread-exited? thread)))
      (cancel-thread thread))
    (when port
      (close-port port))))

;;; lirc.scm ends here

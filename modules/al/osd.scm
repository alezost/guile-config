;;; osd.scm --- Auxiliary code for working with OSDs

;; Copyright © 2016–2026 Alex Kost <alezost@gmail.com>

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

(define-module (al osd)
  #:use-module (xosd)
  #:use-module (al utils)
  #:export (show-osds
            hide-osds
            register-osd
            define-osd))

(define %osds '())

(define (register-osd osd)
  "Register OSD for such procedures as 'hide-osds'."
  (push! osd %osds))

(define-syntax-rule (define-osd name args ...)
  "Define NAME thunk that will return OSD object made with ARGS.

ARGS are passed to 'make-osd' procedure.

OSD object is not defined immediately, it will be created on the first
'name' call and will be returned on subsequent calls of NAME.

Created OSD object is also registered with 'register-osd'."
  (define-lazy name
    (let ((osd (make-osd args ...)))
      (register-osd osd)
      osd)))

(define (show-osds)
  "Show all registered OSDs."
  (for-each show-osd %osds))

(define (hide-osds)
  "Hide all registered OSDs."
  (for-each hide-osd %osds))

;;; osd.scm ends here

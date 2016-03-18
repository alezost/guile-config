;;; processes.scm --- Utilities for working with processes

;; Copyright © 2016 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 19 Mar 2016

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

;; This file provides code related to processes.

;;; Code:

(define-module (al processes)
  #:export (environment-excursion
            with-environment-excursion))

(define (environment-excursion env-thunk body-thunk)
  "Run BODY-THUNK with the current environment set by ENV-THUNK."
  (let ((old-env (environ)))
    (dynamic-wind
      env-thunk
      body-thunk
      (lambda () (environ old-env)))))

(define-syntax-rule (with-environment-excursion env body ...)
  "Run BODY with ENV as the process's current environment."
  (environment-excursion
   (lambda () (environ env))
   (lambda () body ...)))

;;; processes.scm ends here

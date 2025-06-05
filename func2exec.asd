;;; func2exec.asd --- ASDF definition of func2exec. 

;; File:        func2exec.asd
;; Description: ASDF definition of func2exec. 
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2025, 凉凉, all rights reserved
;; Created: 2025-06-04 03:41
;; Version: 0.1
;; Last-Updated: 2025-06-05 16:52
;;           By: 凉凉
;; URL: https://li-yiyang.github.io
;; Keywords: Image dump
;; Compatibility: 
;; 
;; 

;;; License
;; 
;; this package is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;; 
;; this package is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this package. If not, see <https://www.gnu.org/licenses/>.
;;

(asdf:defsystem #:func2exec
  :author ("凉凉")
  :version "0.1"
  :description "Turn function to executable. "
  :depends-on (:SB-INTROSPECT)
  :serial t
  :components
  ((:file "func2exec")))

;;; func2exec.asd ends here

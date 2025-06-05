;;; func2exec.lisp --- Function -> Executable. 

;; File:        func2exec.lisp
;; Description: Function -> Executable. 
;; Author:      凉凉
;; Maintainer:  凉凉
;; Copyright (c) 2025, 凉凉, all rights reserved
;; Created: 2025-06-04 03:41
;; Version: 0.1
;; Last-Updated: 2025-06-05 17:10
;;           By: 凉凉
;; URL: https://github.com/li-yiyang/func2exec
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

(defpackage #:func2exec
  (:use :cl)
  (:export
   ;; Configure
   #:*executable*
   #:*dynamic-space-size*
   #:*control-stack-size*
   #:*parse-hint*
   #:*default-parse-hint*
   #:*flag-nicknames*
   #:*help-flags*

   #:f2e
   #:func2exec))

(in-package :func2exec)

(defparameter *executable* "executable"
  "Default `func2exec' executable name fallback. ")

(defparameter *dynamic-space-size* nil
  "Runtime option --dynamic-space-size for external SBCL.
Set to be `nil' will use current image runtime option. ")

(defparameter *control-stack-size* nil
  "Runtime option --control-stack-size for external SBCL.
Set to be `nil' will use current image runtime option. ")

(defparameter *flag-nicknames* '((:h . :help))
  "Flag nicknames as fallback. ")

(defparameter *parse-hint* '((:help . :flag))
  "Parse hint fallbacks. ")

(defparameter *default-parse-hint* :read
  "Default parse hint type. ")

(defparameter *help-flags* '(:help)
  "A list of keys that will be used to print help message.  ")

#+sbcl
(require :sb-introspect)

(defun function-lambda-list (function)
  "Return `function' lambda list. "
  #+sbcl (sb-introspect:function-lambda-list function))

(defun command-line-arguments ()
  #+sbcl (rest sb-ext:*posix-argv*))

(defun lisp-runtime-arguments ()
  #+sbcl
  (list "--dynamic-space-size"
        (format nil "~d"
                (or *dynamic-space-size*
                    (/ (sb-ext:dynamic-space-size)
                       1024 1024)))
        "--control-stack-size"
        (format nil "~d"
                (or *control-stack-size*
                    (/ (- sb-vm:*control-stack-end*
                          sb-vm:*control-stack-start*)
                       1024 1024)))
        "--non-interactive"))

(defun symbol->keyword (symbol)
  (declare (type symbol symbol))
  (intern (symbol-name symbol) :keyword))

(defun normalize-parse-hint (parse-hint)
  (loop for (var* . type) in parse-hint
        for var = (etypecase var*
                    (keyword var*)
                    (symbol  (symbol->keyword var*))
                    (string  (intern (string-upcase var*) :keyword)))
        do (assert (member type '(:stdin :stdin* :read :eval :plain :flag)))
        collect (cons var type)))

(defun parse-lambda-list (lambda-list &optional parse-hint)
  "Parse `lambda-list' with `parse-hint' annotated.
Return values are normal, optional, key, rest-p, other-keys-p.

Parameters:
+ `lambda-list': should having the syntax

      <lambda-list> ::= <normal>*
                        (&optional <option>*)?
                        (&key <key>* &allow-other-keys?)?

+ `parse-hint': the alist of variable type hint
  See `func2exec:func2exec'.
"
  (loop with stat         = :normal
        with skip         = nil
        with rest-p       = nil
        with other-keys-p = nil
        with parse-hint   = (normalize-parse-hint
                             (append parse-hint *parse-hint*))

        for var* in lambda-list
        for sym  = (if (listp var*) (first var*) var*)
        for var  = (symbol->keyword sym)
        for type = (or (cdr (assoc var parse-hint))
                       *default-parse-hint*)

        do (cond ((eq sym '&optional)
                  (setf skip :optional
                        stat nil))
                 ((eq sym '&key)
                  (setf skip :key
                        stat nil))
                 ((eq sym '&rest)
                  (setf skip   :rest
                        stat   nil
                        rest-p t))
                 ((eq sym '&allow-other-keys)
                  (setf stat         nil
                        skip         nil
                        other-keys-p t)))

        if (eq stat :normal)
          collect (cons var type) into normal
        if (eq stat :optional)
          collect (cons var type) into optional
        if (eq stat :key)
          collect (cons var type) into key

        if skip
          do (shiftf stat skip nil)

        finally (return (values normal optional key rest-p other-keys-p))))

(defun function-docstring (function exec &key parse-hint)
  "Return the documentation string of `function'"
  (with-output-to-string (*standard-output*)
    (multiple-value-bind (normal optional keys rest-p other-key-p)
        (parse-lambda-list (function-lambda-list function) parse-hint)

      ;; exec args [optional args] { --key ... } (ref:docstr.first)
      (format t "~A ~{~A~^ ~}" exec (mapcar #'car normal))
      (when optional (format t " [~{~A~^ ~}]" (mapcar #'car optional)))
      (when rest-p (format t " ... "))
      (when keys   (format t " { --key ... }"))
      (format t "~%~%")

      ;; list of keys                            (ref:docstr.rest)
      (let ((lines ())
            (types ()))
        (loop for (var . type) in normal
              do (push (format nil "  ~A" var)         lines)
              do (push (format nil " [~:@(~A~)]" type) types))
        (when optional
          (push "  &optional" lines)
          (push ""            types)
          (loop for (var . type) in optional
                do (push (format nil "  ~A" var)         lines)
                do (push (format nil " [~:@(~A~)]" type) types)))
        (loop for (var . type) in keys
              do (push (format nil "  --~A"      var)  lines)
              do (push (format nil " [~:@(~A~)]" type) types))
        (when lines
          (let ((max (reduce #'max (mapcar #'length lines))))
            (loop for line in (reverse lines)
                  for type in (reverse types)
                  do (format t "~vA ~A~%" (1+ max) line type))))
        (when other-key-p
          (format t "  ... allow other keys~%"))

        ;; Lisp function docstrings                (ref:docstr.last)
        (format t "~&~%~A~&" (or (documentation function 'function) ""))))))

(defun key-arg-p (arg)
  (and (> (length arg) 2)
       (char= (aref arg 0) #\-)
       (char= (aref arg 1) #\-)))

(defun read-arg (type arg)
  (declare (type (member :stdin :stdin* :read :eval :plain :flag) type))
  (ecase type
    (:stdin  *standard-input*)
    (:stdin* (with-output-to-string (in)
               (loop for line = (read-line *standard-input* nil nil)
                     while line
                     do (write-line line in))))
    (:read   (read-from-string arg))
    (:eval   (eval (read-from-string arg)))
    (:plain  arg)
    (:flag   t)))

(defun parse-argv (lambda-list argv
                   &key parse-hint flag-nicknames
                     (default-parse-hint *default-parse-hint*))
  "Parse ARGV and return the calling form. "
  (let ((*default-parse-hint* default-parse-hint)
        (flag-nicknames       (append flag-nicknames *flag-nicknames*)))
    (multiple-value-bind (normal optional keys rest-p other-key-p)
        (parse-lambda-list lambda-list parse-hint)
      (loop with key*    = ()
            with normal* = ()
            with help?   = nil

            while (not (endp argv))

            do (let ((arg (pop argv)))
                 (if (key-arg-p arg)
                     ;; Parse keys
                     (let* ((key  (symbol->keyword
                                   (read-from-string arg t nil :start 2)))
                            (key  (or (cdr (assoc key flag-nicknames)) key))
                            (help (find key *help-flags*))
                            (type (or (cdr (assoc key keys))
                                      (and other-key-p default-parse-hint)
                                      help
                                      (error "Unknown key ~S" key)))
                            (arg  (read-arg (if help :flag type)
                                            (unless (or (eq type :flag) help)
                                              (pop argv)))))
                       (when help (setf help? t))
                       (push arg key*)
                       (push key key*))
                     ;; Parse normal and optional arguments
                     (let* ((type (if (endp normal)
                                      (if (endp optional)
                                          (if rest-p
                                              :plain
                                              (error "Too many input arguments. "))
                                          (cdr (pop optional)))
                                      (cdr (pop normal)))))
                       (push (read-arg type arg) normal*))))

            finally (progn
                      (unless (or (endp normal) help?)
                        (error "Too few input arguments. "))
                      (return (values (nconc (nreverse normal*) key*))))))))

(defun func2exec-here (executable function
                       &key
                         (default-parse-hint *default-parse-hint*)
                         parse-hint
                         flag-nicknames
                         compression
                         result)
  (let* ((lambda-list (function-lambda-list function))
         (exec        (file-namestring executable))
         (document    (function-docstring function exec
                                          :parse-hint parse-hint))
         (print-fn    (ecase result
                        (:none   (lambda (res) (declare (ignore res))))
                        (:plain  #'print)
                        (:pretty #'pprint)))
         (toplevel    (lambda ()
                        (let ((args (parse-argv
                                     lambda-list
                                     (command-line-arguments)
                                     :default-parse-hint default-parse-hint
                                     :parse-hint     parse-hint
                                     :flag-nicknames flag-nicknames)))
                          (cond ((find :help args)
                                 (write-string document)
                                 (format t "~&~%Input: ~%")
                                 (format t "  ~{~S~^ ~}~%" args))
                                (t
                                 (funcall print-fn (apply function args))))))))
    #+sbcl
    (sb-ext:save-lisp-and-die executable
                              :toplevel             toplevel
                              :compression          compression
                              :executable           t
                              :save-runtime-options t)))

(defun func2exec-external (executable function
                           &key
                             (default-parse-hint *default-parse-hint*)
                             parse-hint
                             flag-nicknames
                             compression
                             result
                             depends-on
                             loads
                             no-evaluate)
  (unless (symbolp function)
    (error "Building externally should provide function as symbol. "))
  (let* ((*print-pretty* nil)
         (cmd
           `("sbcl"
             ,@(lisp-runtime-arguments)
             "--eval" "(ql:quickload :func2exec)"
             ,@(when depends-on
                 (list "--eval" (format nil "(ql:quickload '~S)" depends-on)))
             ,@(loop for load in loads
                     for path = (truename load)
                     for dir  = (format nil "/~{~A~^/~}/"
                                        (cdr (pathname-directory path)))
                     for src  = (file-namestring path)
                     collect "--eval"
                     collect (format nil
                                     "(uiop:with-current-directory (~S) (load ~S))"
                                     dir src))
             "--eval"
             ,(format nil
                      (concatenate 'string
                                   "(func2exec:f2e #'~A::~A "
                                   ":executable '~S "
                                   ":parse-hint '~S "
                                   ":default-parse-hint '~S "
                                   ":flag-nicknames '~S "
                                   ":no-compression '~S "
                                   ":result '~S)")
                      (package-name (symbol-package function)) (symbol-name function)
                      executable
                      parse-hint
                      default-parse-hint
                      flag-nicknames
                      (not compression)
                      result))))
    (if no-evaluate
        cmd
        (uiop:run-program cmd :ignore-error-status t
                              :output              t
                              :error-output        t))))

(defun func2exec (function &key (executable (if (symbolp function)
                                                (format nil "~A" function)
                                                *executable*))
                             (no-compression nil)
                             (result :none)
                             (default-parse-hint *default-parse-hint*)
                             parse-hint
                             flag-nicknames
                             external
                             depends-on
                             loads)
  "Turn lisp `function' into `executable'.
Return the path to executable.

Parameters:
+ `function': a symbol for function or function itself
+ `executable': name to output executale file
+ `compression': non `nil' to compress the output executable (SBCL)
+ `documentation': documentation string of `function'

  if not provided, the documentation will be evaluated by
  `function-docstring'.
+ `result': how to output (serialize) function return values

  the `result' could be:
  + `:none'   for not output result
  + `:plain'  just print the result
  + `:pretty' pretty print the result
+ `parse-hint': an alist
+ `flag-nicknames': an alist
+ `external': non-nil for using external SBCL to build executable
+ `depends-on': system dependence
+ `loads': loading scripts
"
  (declare (type (or symbol function) function))
  (let ((*default-parse-hint* default-parse-hint))
    (if external
        (func2exec-external executable function
                            :parse-hint parse-hint
                            :flag-nicknames flag-nicknames
                            :compression    (not no-compression)
                            :result         (or result :none)
                            :depends-on     depends-on
                            :loads          loads)
        (func2exec-here executable function
                        :parse-hint     parse-hint
                        :flag-nicknames flag-nicknames
                        :compression    (not no-compression)
                        :result         (or result :none)))
    executable))

(setf (fdefinition 'f2e) #'func2exec)

;;; func2exec.lisp ends here

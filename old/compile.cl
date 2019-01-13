(ql:quickload :parenscript)

(defpackage :test
  (:use #:cl :parenscript))

(in-package :test)


(with-open-file (str "main.js"
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
    (write-string (PS-COMPILE-FILE "main.cl") str))


;;; generic utilties ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: Made up on the fly - need to research to see what kind of reusable stuff my fellow clers have implemented! 
; alexandria and cl-containers come to mind

(defun map-up (keys values &key (test 'eql))
  (let ((a-table (make-hash-table :test test)))
	(loop 
	   for k in keys 
	   for v in values 
       do (setf (gethash k a-table) v))
	a-table))

(defun shallow-print-hash (tbl)
  (format t "~A:~%" tbl)
  (maphash #'(lambda (key value)
               (format t "[~A] ~A~%" key value))
           tbl))

(defun get-hash-values (tbl)
  (loop for k being the hash-keys in tbl 
     using (hash-value v)
     collect v))

(defun get-hash-keys (tbl)
  (loop for k being the hash-keys in tbl
       using (hash-value v)
       collect k))

; TODO: 1. reprogram MOP to not signal an error when reading nil slots 
;       2. hashtable literals


(defvar *output-file* "C:\\Users\\Gurp\\Desktop\\bundle\\first-project\\ltk-lst-symbols.lisp")

(setf *output-file* "C:\\Users\\Gurp\\Desktop\\bundle\\first-project\\ltk-lst-symbols.lisp")

(output-exported-symbols :ltk :to-file *output-file*)
(output-exported-symbols :sdraw :to-file *output-file*)

(defun exported-symbols (package-name &key (to-file nil))
  (when (not (null package-name))
    (if to-file
        (with-open-file (s (concatenate 'string (string package-name) "_package.lisp") 
                           :direction :output 
                           :if-exists :supersede)
          (output-exported-symbols-to-stream package-name s))
        (output-exported-symbols-to-stream package-name t))))

(defun output-exported-symbols-to-stream (package-name s)
  "inspired by the common lisp cookbook"
  (let ((coll))
    (do-external-symbols (symb (find-package package-name))
      (push (string symb) coll))
    (setf coll (sort coll #'string<))
    (loop for item in coll
       do (format s ";~A~%" item))))


(defun doctor-path (str)
  "adds a slash to a windows path"
  ())

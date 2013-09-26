; lst ;

; first rest list 

(defmacro left (cns)
  `(car ,cns))

(defmacro right (cns)
  `(cdr ,cns))

; stack

(defvar *stk* nil)
(push 1 *stk*) ;=> (1)
(push 2 *stk*) ;=> '(2 1)
(pop *stk*)    ;=> '(1)

; circular lst #1(1 2 3 #1#)

(defvar *circulo* nil)
;(setf *circulo* #1(1 2 3 #1#))

; map (hash tables)

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

; objects (clos)
(defclass sample-object ()
  ((fielda :accessor fielda :initarg :fielda)
   (fieldb :accessor fieldb :initarg :fieldb))
  (:default-initargs
   :fielda nil
    :fieldb nil))

(defmethod initialize-instance :after ((o sample-object) &key)
  (format t "This where you put stuff after the object is made an intialized"))

; queue

;(defclass my-queue ()
;  ((qwee :accessor qwee :initarg :qwee)
;   (last-one :accessor last-one :initarg :last-one))
;  (:default-initargs
;   :qwee (cons 'empty-queue nil)
;   :last-one nil))

;(defmethod initialize-instance :after ((q my-queue) &key)
;  (setf (last-one q) (qwee            


(setf *q* nil)

(enqueue! 1 *q*)
(enqueue! 2 *q*)
(enqueue! 3 *q*)
(dequeue! *q*)

;implemented as macros b/c setf of the argument is really a setf in the local environment
(defmacro enqueue! (obj q)
  `(if (null ,q)
       (push ,obj ,q)
       (setf (cdr (last ,q))
             (cons ,obj nil))))
       
(defmacro dequeue! (q)
  `(pop ,q))


;;; graph data structure primitives  ;;;

(defclass node ()
  ((name :accessor name :initarg :name)
   (val :accessor val :initarg :val)
   (visited? :accessor visited? :initarg :visited?)
   (neighbors :accessor neighbors :initarg :neighbors))
  (:default-initargs
    :name nil
    :val nil
    :visited? nil
    :neighbors nil))

(defclass graph ()
  ((allnodes :accessor allnodes :initarg :allnodes)
   (sdot-output :accessor sdotout :initarg :sdotout))
  (:default-initargs
   :allnodes nil
    :sdotout nil))

(setf *node-specs*
      '((A B C)
        (B C A)
        (C B A)))

(defun get-name (node-spec)
  (first node-spec))

(defun get-edge-names (node-spec)
  (rest node-specs))

;;; Basic graph data structure construction ;;;

(defun construct-graph (node-specs)
  (let* ((cache (make-hash-table :test 'equal))
         (grph (make-instance 'graph)))
    (loop for nspec in node-specs
       for (make-node-based-on nspec cache))))

(defun make-node-based-on (nspec cache)
  (let (cur (get-node! cache (get-name nspec)))
    (setf (neighbors cur)
          (get-edges! cache (get-edge-names nspec)))))

(defun get-edges! (cache edge-names)
  (loop for edge-name in edge-names
     collect 
       (get-node! cache edge-name)))

(defun get-node! (cache nme)
  (let ((existing-node (gethash nme cache)))
    (if existing-node
        existing-node
        (let ((new-node (make-instance 'node :name nme)))
          (setf (gethash nme cache) new-node)
          new-node))))
                            
(defmethod reset-graph ((g graph))
  (loop for n being the hash-values in (allnodes g)
       do (setf (visited? n) nil)))

;cc,cb

(defmethod make-visual-graph ((g graph) nde)
  (setf (visited? nde) t)
  (loop for neigh in (neighbors nde)
       do (output-neighbor neigh :for-node nde)))

(defun output-neighbor (nde &key for-node)
  (if (null (visited? neigh))
      (output-graph-traversal neigh)))

 
; tree (s-draw)
; regexp (cl-ppcre)
; file io (pcl)
; ui (ltk)
; sorting
; searching
; looping (loop macro, do)
; array
; clrw writer design

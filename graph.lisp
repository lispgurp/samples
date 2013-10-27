(defclass node ()
  ((name :accessor name :initarg :name)
   (val :accessor val :initarg :val)
   (visited? :accessor visited? :initarg :visited?)
   (neighbors :accessor neighbors :initarg :neighbors)
   (sdot-rep :accessor sdot-rep :initarg :sdot-rep))
  (:default-initargs
    :name nil
    :val nil
    :visited? nil
    :neighbors nil
    :sdot-rep nil))

(defclass graph ()
  ((allnodes :accessor allnodes :initarg :allnodes)
   (sdot-rep :accessor sdot-rep :initarg :sdot-rep))
  (:default-initargs
   :allnodes nil
    :sdot-rep nil))


(defun get-name (node-spec)
  (first node-spec))

(defun get-edge-names (node-spec)
  (rest node-spec))


(defmethod output-all-nodes ((g graph))
  (loop for n being the hash-values of (allnodes g)
       do (output-node n)))

(defmethod output-node ((n node))
  (format t "Node ~A, Visited? ~A, Value ~A" (name n) (visited? n) (val n))
  (format t ",neighbors: ~A~%" (output-neighbors n)))

(defmethod output-neighbors ((n node))
  (when (not (null (neighbors n)))
    (loop for neigh in (neighbors n)
       collect (name neigh))))

;;; graph data structure construction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun construct-graph (node-specs)
  (let* ((cache (make-hash-table :test 'equal))
         (grph (make-instance 'graph :allnodes cache)))
    (loop for nspec in node-specs
       do (make-node! cache nspec))
    grph))

(defun make-node! (cache nspec)
  (let ((cur (get-node! cache (get-name nspec))))
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

; types = 'real or 'sdot                            
(defmethod reset-graph ((g graph) &key (node-type 'real))
  (loop for n being the hash-values in (allnodes g)
     do (if (eq node-type 'real)
            (setf (visited? n) nil)
            (setf 
             (sdot-visited? 
              (sdot-rep n)) nil))))

(defmethod graph-search ((n node) nme)
  (cond ((visited? n) nil)
        ((equal nme (name n)) n)
        (t
         (setf (visited? n) t)
         (recur-over-neighbors n nme))))

(defmethod recur-over-neighbors ((n node) nme)
  (loop 
     for edge-node in (neighbors n)
     if (not (visited? edge-node))
     do (let ((result (graph-search edge-node nme)))
          (when (not (null result))
            (return result)))))

;;; sdot representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass sdot-node ()
  ((sdot-visited? :accessor sdot-visited? :initarg :sdot-visited?)
   (node-decl :accessor node-decl :initarg :node-decl)
   (edge-decls :accessor edge-decls :initarg :edge-decls))
  (:default-initargs
   :sdot-visited? nil
   :node-decl nil
   :edge-decls nil))

(defclass sdot-graph ()
  ((node-decls :accessor node-decls :initarg :node-decls)
   (edge-decls :accessor edge-decls :initarg :edge-decls)
   (graph-header :accessor graph-header :initarg :graph-header)
   (default-font :accessor default-font :initarg :default-font))
  (:default-initargs
   :node-decls '()
   :edge-decls '()
   :graph-header '(graph ((rankdir "LR")))
   :default-font '((fontname "Arial") (fontsize "9"))))

(defmethod initialize-sdot-rep! ((g graph))
  (setf (sdot-rep g) (make-instance 'sdot-graph))
  (loop for n being the hash-values in (allnodes g)
       do (init-whole-sdot-node! n (sdot-rep g))))

(defmethod init-whole-sdot-node! ((n node) (g sdot-graph))
  (init-raw-sdot-node! n g)
  (init-sdot-node-edges! n g))

(defmethod init-raw-sdot-node! ((n node) (g sdot-graph))
    (setf (sdot-rep n) (make-instance 'sdot-node))
    (setf (node-decl (sdot-rep n))
          (make-sdot-node n g))
    (push (node-decl (sdot-rep n))
          (node-decls g)))

(defmethod make-sdot-node ((n node) (g sdot-graph))
  (let* ((name-str (string (name n))))
    (list 
     'node 
     (append
      (list
       (list 'id name-str)
       (list 'label (concatenate 'string "node" name-str)))
      (default-font g)))))

(defmethod init-sdot-node-edges! ((n node) (g sdot-graph))
  (let ((sdot-edges
         (loop 
            for neigh in (neighbors n)
            collect (make-sdot-edge n neigh g))))
    (setf (edge-decls (sdot-rep n))
          (push 'Neighbors sdot-edges))
    (push (edge-decls (sdot-rep n))
          (edge-decls g))))

(defmethod make-sdot-edge ((l node) (r node) (g sdot-graph))
  (let ((left-name (string (name l)))
        (right-name (string (name r))))
    (list 'edge 
          (append
           (list (list 'from left-name)
                 (list 'to right-name))
           (default-font g)))))

(defmethod splice-together-pieces-of-sdot-graph ((sdg sdot-graph))
  (append
   (graph-header sdg)
   (node-decls sdg)
   (loop for neighborhood in (edge-decls sdg)
      append (rest neighborhood))))

; pure means as strings!
(defun generate-pure-sdot-output (sdg)
  (with-open-file (s "C:/Users/Gurp/Desktop/bundle/tst-graph.dot"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (s-dot->dot s sdg)))

(defun consume-sdot-strings (afile)
  (with-open-file (s afile)
    ('look-for-consumer)))


; First load in s-dot, and get into the package

; TODO test
;(defmacro initialize-sdot-session ()
;  '(ql:quickload 's-dot)
;  '(in-package :s-dot))

;;; drivers ;;;

(defvar *node-specs* nil)
(defvar *g* nil)
(defvar *sdg* nil)

(defun test-simple-graph ()
  (let* ((nspecs '((A B C)
                  (B C A)
                  (C B A)))
         (g (construct-graph nspecs)))
    (initialize-sdot-rep! g)
    (setf *g* g)
    (setf *sdg* (sdot-rep *g*))
    (setf *node-specs* nspecs)
  ))

;;; sdot experiments ;;;

(defvar *sdg1* nil)
(setf *sdg1*
      '(graph ((rankdir "LR"))
        (node (	(id "a") (label "node1") (fontsize "9") (fontname "Arial")))
        (node (	(id "b") (label "node2") (fontsize "9") (fontname "Arial")))
        (node (	(id "c") (label "node3") (fontsize "9") (fontname "Arial")))
        (node (	(id "d") (label "node4") (fontsize "9") (fontname "Arial")))
        (edge (	(from "a") (to "b") (fontname "Arial") (fontsize "9") (label "edge1")))
        (edge (	(from "a") (to "c") (fontname "Arial") (fontsize "9") (label "edge2")))
        (edge (	(from "b") (to "c") (fontname "Arial") (fontsize "9") (label "edge3")))
        (edge (	(from "b") (to "d") (fontname "Arial") (fontsize "9") (label "edge4")))
        (edge (	(from "c") (to "d") (fontname "Arial") (fontsize "9") (label "edge5")))))

(defvar *sdg2* nil)
(setf *sdg2*
      '(graph ((rankdir "LR"))
        (node (	(id "a") (label "nodeA") (fontname "Arial") (fontsize "9")))
        (node (	(id "b") (label "nodeB") (fontname "Arial") (fontsize "9")))
        (node (	(id "c") (label "nodeC") (fontname "Arial") (fontsize "9") ))
        (node (	(id "d") (label "nodeD") (fontname "Arial") (fontsize "9") ))
        (edge (	(from "a") (to "b") (fontname "Arial") (fontsize "9") (label "edge1")))
        (edge (	(from "b") (to "a") (fontname "Arial") (fontsize "9") (label "edge2")))
        (edge (	(from "a") (to "c") (fontname "Arial") (fontsize "9") (label "edge3")))
        (edge (	(from "b") (to "c") (fontname "Arial") (fontsize "9") (label "edge4")))
        (edge (	(from "b") (to "d") (fontname "Arial") (fontsize "9") (label "edge5")))
        (edge (	(from "c") (to "d") (fontname "Arial") (fontsize "9") (label "edge6")))
        )
      )

; graph = '(graph <header-decl-lst> <node-lst> <edge-lst>)
; <node> = 

;"C:/Program Files (x86)/Graphviz2.30/bin/tst-graph.dot"
;"C:/Users/Gurp/Desktop/bundle/tst-graph.dot"

(defun output-sdot (sdg)
  (with-open-file (s "C:/Users/Gurp/Desktop/bundle/tst-graph.dot"
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (s-dot->dot s sdg)))


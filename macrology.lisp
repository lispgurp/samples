
;;; let over lambda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun sleep-units (value unit)
  (sleep
    (* value
       (case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600)
         ((d) 86400)
         ((ms) 1/1000)
         ((us) 1/1000000)))))

(sleep-units 2 'm)
(trace sleep-units)

(defmacro sleep-units (value unit)
  `(sleep
     (* ,value
        ,(case unit
           ((s) 1)
           ((m) 60)
           ((h) 3600)
           ((d) 86400)
           ((ms) 1/1000)
           ((us) 1/1000000)))))

(defmacro nlet (aname letargs &rest body)
  `(labels ((,aname ,(mapcar #'car letargs)
              ,@body))
     (,aname ,@(mapcar #'cadr letargs))))

(macroexpand
    '(nlet fact ((n n))
       (if (zerop n)
         1
         (* n (fact (- n 1))))))

;;; on lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ME: I believe that in order to understand variable capture we need to understand some prerequisite vocabulary
; 1. "Use" of variables = establishing a binding or referring to a binding

; *variable capture*

; PAUL: "Variable capture occurs when macroexpansion causes a name clash: when some symbol ends up referring to a variable from another context."
; ME: This is essentially a form of inadvetant semantic ambigioutiy introduced by names looking the same but meaning something different. Surely
; this can be automated? 

; *macro argument capture*
; PAUL:  "In argument capture, a symbol passed as an argument in the macro call inadvertently refers to a variable established by the macro expansion itself"
; ME: Macro argument capture: When the "use" of an argument in a macro clashes with a binding established by the macro itself
; 
;

;ME: What if you pass "limit" in? It is already established by the macro itself and the "use" of it in the macro will clash with the "use" if it out of the macro 
(defmacro for ((var start stop) &body body) ; wrong:                                           
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

(for (limit 1 5)
     (princ limit))

(macroexpand
 '(let ((limit 5))
   (for (i 1 10)
     (when (> i limit)
       (princ i)))))

; 2. Free Symbol Capture: Macro expansion causes a name clash in the surrounding environment, I have always understood this intuitively
;
; Paul is going to tell us how to avoid variable capture:
;  Free: The use of symbols that do not have any bindings in a specific lexical scope (not specific to macroexpansion!)
;  Skeleton: A macro expansion without the arguments in it (but the lst structure is intact
;  Capturable: A symbol is capturable 
;      1. It appears in macro expansion
;      2. It is *free* in the skeleton of the macroexpansion
;      3. It is *bound* in the tree of the macro expansion in which arguments are used (e.g. bound or evaluated)
;
; if name shadowingmacros that expand into othe same name
;However, if there is no (lexical) context in which the binding of x and the variable passed as an argument will both be visible, as in
	
(defmacro safe1 (var)
  `(progn (let ((x 1))
	    (print x))
	  (let ((,var 1))
	    (print ,var))))

(defmacro before (x y seq)
  "takes two objects and a sequence, and returns true iff the first object occurs before the second in the sequence"
  `(let ((xval ,x) (yval ,y) (seq ,seq))
     (< (position xval seq)
	(position yval seq))))










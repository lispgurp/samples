

(defclass plane-of-existence ()
  ((x-bound :accessor x-bound :initarg :x-bound)
   (y-bound :accessor x-bound :initarg :y-bound)
   (plane :accessor plane :initarg :plane))
  (:default-initargs
   :x-bound nil
   :y-bound nil
   :plane nil))
  
(defclass point ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y))
  (default-initargs
   :x nil
    :y nil))

(defclass simulation-object ()
  ((location :accessor location :initarg :location))
  (default-initargs
   :location nil))

; Visual Representation
; fly-trap - String = "T", Color = BLACK
; food - String = "F", Color = BLACK
; bear - String = "/" "\": every other move | Color = WHITE for Polar, BROWN for Brown
; Lion - String = "L" | Color = RGB, Randomly pick one of Every Three Moves
; giant - String = "Fee","Fie","Foe","Fum", Linearly pick one of Every Three Moves, Color = GRAY

(defmethod out-of-bounds? ((pl plane-of-existence) x-pt y-pt)
  (not (and
        (>= x-pt 0) 
        (< x-pt (x-bound pl))
        (>= y-pt 0)
        (< y-pt (y-bound pl)))))

(defmethod set-plane! ((pl plane-of-existence) x-pt y-pt an-object)
  (when (out-of-bounds? x-pt y-pt)
    (error (format nil "Fool!, you tried to set an object outside the plane of existence @x: ~A @y: ~A, GTFO" x-pt y-pt)))
  (setf (aref (plane pl)
              x-pt
              y-pt)
        an-object))
  
(defmethod find-free-random-pt ((pl plane-of-existence))
  "if the plane of existence gets more dense this algorithm will run longer!"
  (loop 
     for x-val = (random (1- (x-bound pl)))
     for y-val = (random (1- (y-bound pl)))
     until (null (aref (plane pl) x-val y-val))
       finally (make-instance 'point :x x-val :y y-val)))
       
(defmethod populate-plane ((pl plane-of-existence) objname ntimes)
  (loop for i from 0 to ntimes
       for newobj = (make-instance objname)
       for pt = (find-random pl)
       do (set-plane! pl (x pt) (y pt) newobj)))



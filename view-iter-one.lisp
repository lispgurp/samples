; we will be using ltk for all of the GUI elements

(ql:quickload "ltk")


(defun construct-ui ()
  (with-ltk ()
    (let* ((w 100)
           (h 200)
           (root (make-instance 'frame
                                :master nil))
           (fld (make-simulation-field root w h))
           (ctrls (make-controls root fld)))
      (construct-layout root ctrls fld))))
      
(defun make-controls (par cnvs)
  (let* ((lbl-slow (make-instance 'label
                                :master par
                                :text "slow"))
         (s (make-instance 'scale
                           :master par
                           :length 100))
         (lbl-fast (make-instance 'label
                                  :master par
                                  :text "fast"))
         (strt-b (make-instance 'button
                               :master par
                               :text "start"))
         (stop-b (make-instance 'button
                               :master par
                               :text "stop"))
         (dbg-b (make-instance 'button
                               :master par
                               :text "debug"))                               
         (stp-b (make-instance 'button
                               :master par
                               :text "step"
                               :command #'(lambda () (redraw-simulation-field cnvs par))))
         (nxt-b (make-instance 'button
                               :master par
                               :text "next 100")))
    (list lbl-slow s lbl-fast strt-b stop-b stp-b dbg-b nxt-b)))

(defun construct-layout (root ctrls fld)
  (grid root 0 0 :rowspan 2 :columnspan (length ctrls))
  (grid fld 0 1)
  (loop for c in ctrls
       for i from 0 to (1- (length ctrls))
       do (grid c 1 i))
  (format t "@ end of construct-layout parent height: ~A, width: ~A~%" 
          (window-height root)
          (window-width root)))
;  (configure fld :height (- (window-height root) 30))
;  (configure fld :width  (- (window-width root) 35)))

(defun make-simulation-field (parent w h)
  (let ((c (make-instance 'canvas
                          :width w
                          :height h
                          :master parent)))
    (configure c :background 'cyan)))


(defun redraw-simulation-field (cvs par)
  (widget-delete-all cvs)
  (redraw-grid-on cvs par))

(defun redraw-grid-on (cvs par)
  (format t "Width,Height of Canvas: ~A,~A~%" 
          (window-width cvs)
          (window-height cvs))
  (format t "Width, Height of Parent: ~A,~A~%"
          (window-width par)
          (window-height par)))

;  (let ((home-base (make-instance 'point :x 0 :y 0)))
;    (tile-pt home-base 2 cvs)))

(defclass point ()
  ((x :accessor x :initarg x)
   (y :accessor y :initarg y))
  (:default-initargs
   :x nil
    :y nil)) 

(defun tile-pt (pt dimension cvs)
  (let* ((x2 (+ dimension (x pt)))
         (y2 (+ dimension (y pt))))
    (create-rectangle cvs 
                      (x pt)
                      (y pt)
                      x2 y2)))
                      

(defclass visual-simulation-field ()
  ((width :accessor width :initarg width)
   (height :accessor height :initarg height))
  (:default-initargs
   :width nil
    :height nil))

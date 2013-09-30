(in-package #:plant-example)


(defvar *points* ())
(defvar *triangles* ())


(defun demo (num-points)
  "Generate a random array of `num-points' points, generate the tesselation, 
and display it with cl-opengl and cl-glut."
  (let* ((*points* (lofi.tri:sort-by-x
                    (lofi.tri:random-point-array num-points)))
         (*triangles* (lofi.tri:triangulate *points*))
         (w (make-instance 'viz-window)))
    (glut:display-window w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that perform the actual drawing.

(defun draw-points ()
  "Draws the currently bound *points*"
  (gl:with-primitive :points
    (loop for i from 0 upto (- (length *points*) 1)
       do (let* ((p (aref *points* i))
                 (x (aref p 0))
                 (y (aref p 1)))
            (gl:vertex x y 1)))))

(defun draw-circle (center radius)
  "Draws a circle given a center and a radius"
  (let* ((step 5)
         (seg-max (- 360 step))
         (trig-step 0.0174))
    (gl:with-primitive :line-loop
      (loop for i from 0 upto seg-max by step
         do (let ((x (+ (aref center 0) (* radius (sin (* trig-step i)))))
                  (y (+ (aref center 1) (* radius (cos (* trig-step i))))))
              (gl:vertex x y 0))))))

(defun draw-circumcircle (tri)
  "Draws the circumcircle of tri"
  (let ((cir (slot-value tri 'lofi.tri:circumcircle)))
    (draw-circle (slot-value cir 'lofi.tri:center)
                 (slot-value cir 'lofi.tri:radius))))

(defun draw-circumcircles ()
  "Draws all of the circumcircles of the currently bound *triangles*"
  (loop for tri in *triangles*
     do (draw-circumcircle tri)))

(defun draw-triangle (tri)
  "Draws a lofi.tr:triangle struct"
  (let ((verts (slot-value tri 'lofi.tri:verts)))
    (loop for i from 0 upto 2
       do (let* ((vi (aref verts i))
                 (v (aref *points* vi)))
            (gl:vertex (aref v 0) (aref v 1) (aref v 2))))))

(defun draw-triangles ()
  "Draws the currently bound *triangles*"
  (gl:with-primitive :triangles
    (loop for tri in *triangles*
       do (draw-triangle tri))))

(defun draw-triangulation ()
  "Draws the currently bound triangulation. 
Meant to be called from a glut window display callback."
  ;; Draw a unit rect to act as a frame of reference
  (gl:color 1 0.25 0.25 0.25)
  (gl:line-width 2)
  (gl:rect -1 -1 1 1)

  ;; lofi.tri point arrays are generated from 0-1
  ;; so we just offset them a bit so that they
  ;; are centered when drawn
  (gl:translate -0.5 -0.5 0.0)

  (gl:color 0.15 0.05 0.15 0.05)
  (gl:line-width 1)
  (draw-circumcircles)

  (gl:color 0 0.25 1 0.25)
  (gl:line-width 2)
  (draw-triangles)

  ;; Draw the points
  (gl:color 1 1 1 1)
  (gl:point-size 5)  
  (draw-points))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLUT windowing

(defclass viz-window (glut:window)
  ((zoom :initform 0.0025)
   (offset :initform #(0.0 0.0))
   (last-mouse :initform ())
   (pan-throttle :initform 5)
   (zoom-throttle :initform 0.00025)
   (update-mode :initform nil))
  (:default-initargs :pos-x 100 :pos-y 100
                     :width 800 :height 800
                     :mode '(:double :rgba)
                     :title "plant-example"))
 
(defmethod glut:display-window :before ((w viz-window))
  (gl:clear-color 0.15 0.15 0.15 0)
  (gl:enable :blend)
  (gl:blend-func :one :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :line))
 
(defmethod glut:mouse ((w viz-window) button state x y)
  (with-slots (update-mode last-mouse) w
    (setf update-mode
          (when (eq state :down)
            (setf last-mouse (list x y))
            (case button
              (:right-button :zoom)
              (:middle-button :pan))))))
 
(defmethod glut:motion ((w viz-window) x y)
  (with-slots (offset zoom last-mouse update-mode zoom-throttle pan-throttle) w
    (let* ((dx (- (car last-mouse) x))
           (dy (- (cadr last-mouse) y))
           (modx (* (/ dx (glut:width w)) 100))
           (mody (* (/ dy (glut:height w)) 100))
           (effective-pan-speed (* pan-throttle zoom)))
      (case update-mode
        (:zoom
         (setf zoom (max 0.0001 (+ zoom (* zoom-throttle modx)))))
        (:pan
         (setf (aref offset 0) (+ (aref offset 0) (* effective-pan-speed modx))
               (aref offset 1) (- (aref offset 1) (* effective-pan-speed mody))))))
    (setf last-mouse (list x y)))
  (glut:post-redisplay))
 
(defmethod glut:keyboard ((w viz-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))
 
(defmethod glut:reshape ((w viz-window) width height)
  (setf (glut:width w) width
        (glut:height w) height)
  (gl:viewport 0 0 width height)
  (glut:post-redisplay))
 
 
(defmethod glut:display ((w viz-window))
  (gl:clear :color-buffer)
 
  (with-slots (offset zoom) w
 
    ;; Calculate the view
    (gl:matrix-mode :projection)
    (let* ((width (glut:width w))
           (height (glut:height w))
           (hx (* (* width zoom) 0.5))
           (hy (* (* height zoom) 0.5))
           (xoffset (aref offset 0))
           (yoffset (aref offset 1)))
      (gl:load-identity)
      (gl:ortho (- xoffset hx) (+ xoffset hx)
                (- yoffset hy) (+ yoffset hy)
                -5 5))
 
    (gl:matrix-mode :modelview)
    (gl:load-identity)
 
    ;; This is where the drawing functions are called from.
    (draw-triangulation)
    
    (gl:flush))
  
  (glut:swap-buffers))

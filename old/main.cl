
(defvar g-canvas (chain document (get-element-by-id "main-canvas")))
(defvar g-ctx (chain g-canvas (get-context "2d")))

(defvar g-width 640)
(defvar g-height 480)

(defvar g-neurons (array))

(defvar g-dragging Nil)
(defvar g-drag-initial (create x 0 y 0))

(defun -Neuron (x y)
  (setf (@ this x) x)
  (setf (@ this y) y))

(setf (@ -Neuron prototype radius) 20)

(chain g-neurons (push (new (-Neuron 100 100))))

(defun get-mouse-pos (canvas e)
  (let ((rect (chain canvas (get-bounding-client-rect))))
    (create x (- (@ e client-x) (@ rect left))
            y (- (@ e client-y) (@ rect top)))))


(defun hit-mouse (mouse-pos n)
  (with-slots (x y radius) n
      (< (dist-sqr x y (@ mouse-pos x) (@ mouse-pos y)) (* radius radius))))

(defun dist-sqr (x-1 y-1 x-2 y-2)
  (+ (* (- x-2 x-1) (- x-2 x-1))
     (* (- y-2 y-1) (- y-2 y-1))))

(defun vec-sub (a b)
  (create x (- (@ b x) (@ a x))
          y (- (@ b y) (@ a y))))

(defun mouse-down-handler (e)
  (let* ((mouse-pos (get-mouse-pos g-canvas e))
        (hit (chain g-neurons (find
           (lambda (n)
             (hit-mouse mouse-pos n))))))
    (if (null hit)
      NIL
      (progn 
        (setf g-drag-initial mouse-pos)
        (setf g-dragging hit)
        NIL))))

(defun mouse-move-handler (e)
  (let* ((mouse-pos (get-mouse-pos g-canvas e))
         (delta (vec-sub mouse-pos g-drag-initial)))
    (if (null g-dragging)
      NIL
      (progn
        (chain console (log delta))))))

(defun mouse-up-handler (e)
  (setf g-dragging NIL)
  NIL)

(setf (@ g-canvas onmousedown) mouse-down-handler)
(setf (@ g-canvas onmousemove) mouse-move-handler)
(setf (@ g-canvas onmouseup) mouse-up-handler)
                  
        
(defun sim-clear-canvas ()
    (setf (@ g-ctx fill-style) "#D0E7F9")
    (chain g-ctx (begin-path))
    (chain g-ctx (rect 0 0 g-width g-height))
    (chain g-ctx (close-path))
    (chain g-ctx (fill)))

(defun sim-draw ()
  ; setup for drawing neurons
  (setf (@ g-ctx stroke-style) "#000000")
  (setf (@ g-ctx fill-style) "#FFFFFF")
  (chain g-neurons (map (lambda (n) 
         (draw-neuron n 20))))
  (setf (@ g-ctx fill-style) "#000000")
  (chain g-neurons (map (lambda (n) 
         (draw-neuron-text n 20)))))
 

(defun sim-loop () 
  (sim-clear-canvas)
  (sim-draw))

(defun draw-neuron (n r)
  (chain g-ctx (begin-path))
  (chain g-ctx (arc (@ n x) (@ n y) r 0 (* 2.0 (@ -Math -P-I)) F))
  (chain g-ctx (fill))
  (chain g-ctx (stroke)))

(defun draw-neuron-text (n)
  (chain g-ctx (fill-text (-String 3) (@ n x) (@ n y))))

(sim-loop)

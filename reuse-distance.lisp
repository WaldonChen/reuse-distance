;;;;
;;;; reuse-distance.lisp
;;;;
;;;; Measure Reuse Distance.
;;;;

;;; TRACE - a list of memory references, like '(a b b a a c b a)
(defun reuse-distance-histogram (trace)
  "Return reuse distance histogram and last-accesses of a memory reference trace. If TRACE is NIL, return NIL."
  (if (null trace)
    (values nil nil)
    (let ((elem (car (last trace)))     ; ELEM - the last element in TRACE
          (cycle (1- (length trace))))  ; CYCLE - the position of ELEM in TRACE
      (multiple-value-bind
        (histogram          ; HISTOGRAM - reuse distance histogram of TRACE
          last-accesses)    ; LAST-ACCESS - previous emergence of ELEM
        (reuse-distance-histogram (butlast trace))
        (if (null (assoc elem last-accesses))
          ;; ELEM never appeared
          (values
            (append histogram `((,elem nil)))
            (append last-accesses `((,elem ,cycle))))
          ; LAST-ACCESS - previous position of ELEM
          (let ((last-access (cadr (assoc elem last-accesses))))
            ; update the lastest position of ELEM
            (setf (cadr (assoc elem last-accesses)) cycle)
            (values
              (append histogram
                      `((,elem
                          ; reuse distance from the last occurence of ELEM
                          ,(1- (length (remove-duplicates
                                         (subseq trace
                                                 last-access
                                                 cycle)))))))
              last-accesses)))))))

;;; Runtime version of measuring reuse distance
(defun make-reuse-distance ()
  "Return a function with a memory reference as argument and reuse distance histogram as return value"
  (let ((trace nil)
        (histogram nil)
        (last-accesses nil))
    #'(lambda (elem)
        (let ((cycle (length trace)))
          (setf trace (append trace `(,elem)))
          (if (null (assoc elem last-accesses))
            (progn
              (push `(,elem ,cycle) last-accesses)
              (push `(,elem nil) histogram)
              (values histogram trace last-accesses))
            (let ((last-access (cadr (assoc elem last-accesses))))
              (push
                `(,elem ,(1- (length (remove-duplicates (subseq
                                                          trace
                                                          last-access
                                                          cycle)))))
                histogram)
              (setf (cadr (assoc elem last-accesses)) cycle)
              (values histogram trace last-accesses)))))))


(defun reuse-distance-histogram-runtime (trace)
  (defun reuse-intern (reuse-fn trace)
    (case (length trace)
      ((0) nil)
      ((1) (funcall reuse-fn (car trace)))
      (otherwise
        (progn (funcall reuse-fn (car trace))
               (reuse-intern reuse-fn (rest trace))))))
  (let ((reuse (make-reuse-distance)))
    (reuse-intern reuse trace)))

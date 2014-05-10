;;;
;;; reuse-distance.lisp
;;;
;;; Measure Reuse Distance
;;;

(defparameter *trace* '(a b b c c c d c a b c b a b c d d c c a))

(defun reuse-distance-histogram (trace)
  (if (null trace)
    (values nil nil)
    (let ((elem (car (last trace)))     ; ELEM - the last element in TRACE
          (cycle (1- (length trace))))  ; CYCLE - the position of ELEM in TRACE
      (multiple-value-bind
        (histogram          ; HISTOGRAM - reuse distance histogram of TRACE
          last-accesses)    ; LAST-ACCESS - previous emergence of ELEM
        (reuse-distance-histogram (subseq trace 0 cycle))
        (if (null (assoc elem last-accesses))
          ; ELEM never appeared
          (values
            (append histogram (list (list elem nil)))
            (append last-accesses (list (list elem cycle))))
          ; LAST-ACCESS - previous position of ELEM
          (let ((last-access (cadr (assoc elem last-accesses))))
            ; update the lastest position of ELEM
            (setf (cadr (assoc elem last-accesses)) cycle)
            (values
              (append histogram
                      (list (list elem
                                  ; reuse distance from the last occurence of ELEM
                                  (length (remove-duplicates (subseq trace last-access cycle))))))
              last-accesses)))))))

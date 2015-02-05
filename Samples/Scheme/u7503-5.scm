;;; $Id$
(let ((x 3))
  ((let ((f (lambda (y) (+ x y))))
     f )
   7500 ) )

;;; end of u7503-5.scm

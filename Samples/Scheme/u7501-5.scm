;;; $Id$
(comment "lambda enclosant une variable et appliquee")
(let ((x 1))
  ((lambda (y) (+ x y))
   7500 ) )

;;; end of u7501-5.scm

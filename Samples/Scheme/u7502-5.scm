;;; $Id$
(comment "lambda enclosant une variable appliquee hors de portee")
((let ((x 2))
   (lambda (y) (+ x y)) )
 7500 )

;;; end of u7502-5.scm

(defpackage dom-test
  (:use cl dom)
  (:export run))

(in-package dom-test)

(defun run ()
  (let ((doc (html
	      (head)
	       (body))))
    (format t "DOC: ~a~%" doc)))

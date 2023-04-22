(defpackage dom-test
  (:use cl dom)
  (:export run))

(in-package dom-test)

(defun run ()
  (let ((doc (<html>
	      (<head>)
	       (<body>
		(<a> (:href "foo")
		     (text "bar"))))))
    (assert (string= "<HTML><HEAD/><BODY><A href=\"foo\">bar</A></BODY></HTML>"
	     (with-output-to-string (out)
	       (write-html doc :out out :pretty? nil))))))

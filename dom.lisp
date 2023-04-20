(defpackage dom
  (:use cl)
  (:export *node* body head html new-node node))

(in-package dom)

(defvar *node* nil)

(defstruct node
  (tag (error "Missing tag") :type keyword)
  (children nil :type list)
  (attrs nil :type list))

(defun new-node (tag &rest attrs)
  (make-node :tag tag :attrs attrs))

(defun new-child-node (tag &rest attrs)
  (let ((n (apply #'new-node tag attrs)))
    (push n (node-children *node*))))

(defmacro html (&body body)
  `(macrolet ((head (&body body)
		`(let ((*node* (new-child-node :head)))
		   ,@body))
	      (body (&body body)
		`(let ((*node* (new-child-node :body)))
		   ,@body)))
     (let ((*node* (new-node :htmtl)))
       ,@body
       *node*)))

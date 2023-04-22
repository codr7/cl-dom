(defpackage dom
  (:use cl)
  (:export *node* <a> <body> <head> <html> <new-node> <node> text write-html))

(in-package dom)

(defvar *node* nil)

(defstruct node
  (tag (error "Missing tag") :type keyword)
  (children nil :type list)
  (attrs nil :type list))

(defun new-node (tag &rest attrs)
  (labels ((parse-attrs (in out)
	     (if in
		 (let ((k (pop in)) (v (pop in)))
		   (parse-attrs in (cons (cons k v) out)))
		 (nreverse out))))
    (make-node :tag tag :attrs (parse-attrs attrs nil))))

(defun indent-html (level out)
  (dotimes (i level)
    (write-string "  " out)))

(defmethod write-html ((val node) &key (level 0) (out *standard-output*) (pretty? t))
  (with-slots (tag children attrs) val
    (when pretty?
      (indent-html level out))
    
    (format out "<~a" tag)
    
    (unless (null attrs)
      (dolist (a attrs)
	(format out " ~a=\"~a\"" (string-downcase (symbol-name (first a))) (rest a))))
    
    (if (null children)
	(progn
	  (format out "/>")

	  (when pretty?
	    (terpri out)))
	(progn
	  (format out ">")
	  
	  (when pretty?
	    (terpri out))
	  
	  (dolist (cn (reverse children))
	    (write-html cn :level (1+ level) :out out :pretty? pretty?))
	  
	  (when pretty?
	    (indent-html level out))
	  
	  (format out "</~a>" tag)
	  
	  (when pretty?
	    (terpri out))))))

(defmethod write-html ((val string) &key (level 0) (out *standard-output*) (pretty? t))
  (when pretty?
    (indent-html level out))

  (write-string val out)

  (when pretty?
    (terpri out)))

(defmethod print-object ((val node) out)
  (write-html val :out out))

(defun text (child)
  (push child (node-children *node*)))

(defmacro with-node ((&rest args) &body body)
  `(let ((p *node*)
	 (*node* (new-node ,@args)))
     (when p
       (push *node* (node-children p)))
     ,@body
     *node*))

(defmacro <html> (&body body)
  `(macrolet ((<head> (&body body)
		`(with-node (:head)
		   ,@body))
	      (<body> (&body body)
		`(with-node (:body)
		   (macrolet ((<a> ((&rest attrs &key href) &body body)
				`(with-node (:a ,@attrs)
				   ,@body)))
		     ,@body))))
     (with-node (:html)
       ,@body)))

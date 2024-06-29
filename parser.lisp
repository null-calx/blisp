(in-package :parser)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym ,(format nil "~A-" n))))
     ,@body))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
	nconc (direct-slots super)
	nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',name 'slots) ',(mapcar #'first slots))
	 (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
	 ,(mapcar #'slot->defclass-slot slots))

       ,read-method

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
	 (declare (ignorable ,streamvar))
	 (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	   ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
	 (declare (ignorable ,streamvar))
	 (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	   ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
	 (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
	   (let ((,objectvar
		   (make-instance
		    ,@(or (cdr (assoc :dispatch options))
			  (error "Must supply :dispatch form."))
		    ,@(mapcan #'slot->keyword-arg slots))))
	     (read-object ,objectvar ,streamvar)
	     ,objectvar))))))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))

(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (typevar streamvar valuevar)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
	 `(progn
	    (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key ,@args)
	      (read-value ',derived-from ,streamvar ,@derived-args))
	    (defmethod write-value ((,typevar (eql ',name)) ,streamvar ,valuevar &key ,@args)
	      (write-value ',derived-from ,streamvar ,valuevar ,@derived-args))))))
    (2
     (with-gensyms (typevar)
       `(progn
	  ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
	     `(defmethod read-value ((,typevar (eql ',name)) ,in &key ,@args)
		,@body))
	  ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
	     `(defmethod write-value ((,typevar (eql ',name)) ,out ,value &key ,@args)
		,@body)))))))

(defvar *in-progress-objects* nil)

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defun current-binary-object ()
  (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

(defmacro define-binary-enum (name (type &rest args) items)
  (let ((enum-item-list (loop for i from 0
			      for item in items
			      if (listp item)
				do (progn
				     (setf i (second item))
				     (setf item (first item)))
			      collect (cons i item))))
    (with-gensyms (streamvar valuevar)
      `(define-binary-type ,name ()
	 (:reader (,streamvar)
          (ecase (read-value ',type ,streamvar ,@args)
	    ,@(loop for (i . item) in enum-item-list
		    collect (list i `',item))))
	 (:writer (,streamvar ,valuevar)
          (write-value ',type ,streamvar
		       (ecase ,valuevar
			 ,@(loop for (i . item) in enum-item-list
				 collect (list item i)))
		       ,@args))))))

(defmacro define-binary-literal (name (type &rest args) value)
  (with-gensyms (streamvar valuevar)
    `(define-binary-type ,name ()
       (:reader (,streamvar)
        (assert (eql (read-value ',type ,streamvar ,@args) ,value))
	,value)
       (:writer (,streamvar ,valuevar)
        (assert (eql ,valuevar ,value))
	(write-value ',type ,streamvar ,valuevar ,@args)))))

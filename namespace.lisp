;;;-*- Mode: common-lisp; syntax: common-lisp; package: ns; base: 10 -*-
;;;
;;;; Namespace Module
;;;
;;; This module is developed for the purpose of removing net.uri module of Allegro from 
;;; Franz Inc. So, this is expected to work on not only Allegro but also other lisp systems.
;;;
;;; ----------------------------------------------------------------------------------
;;; Copyright (c) 2014-2019 Seiji Koide <koide@ontolonomy.co.jp>
;;; Released under the MIT license
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.
;;; ----------------------------------------------------------------------------------
;; History
;; -------
;; 2015/06/25  Modified for IRIstructure and dtree
;; 2014/04/25  File created.
;;; ==================================================================================


(cl:provide :namespace)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (cl:require :iri)
  ) ; end of eval-when

(cl:defpackage :namespace
  (:nicknames :ns)
  (:use :common-lisp :utils)
  (:export #:*namespaces* #:*namespace* #:namespace #:set-namespace #:namespace-iri 
           #:namespace-node #:namespace-dictionary #:namespace-dtree
           #:list-all-namespaces #:list-all-localnames-in #:namespace-p
           #:create-namespace #:find-namespace #:find-namespace-by-prefix 
           #:intern-iri-p #:intern-iri #:intern-namespace
           #:compute-localname-from-iri #:intern-local-name #:list-all-iris-in
           #:find-iri #:find-imports #:find-ontology #:primary-prefix
           #:export-as-QName
   ))

(in-package :ns)

;;;
;;; A namespace is a memory for localnames of IRIs that share all part of an IRI but localname.
;;;

(defparameter *namespaces* (make-hash-table :test #'equalp)
  "Hash table that includes all namespaces in the system.")

(defclass namespace ()
  ((prefix :initarg :prefix :initform nil :accessor namespace-prefix)
   (iri    :initarg :iri    :initform nil :accessor namespace-iri)
   (node   :initarg :node   :initform nil :accessor namespace-node)
   (dictionary :initform (make-hash-table :test #'equal :size 30) :accessor namespace-dictionary)
   (dtree  :initarg :dtree  :initform nil :accessor namespace-dtree)
   )
  (:documentation "A namespace has an IRI, a prefix, etc."))

#+:ndtree
(defmethod ndtree:get-dtree ((predicate ns:namespace))
  (error "Check it!")
  (cond ((ns:namespace-dtree predicate))
        (t (pushnew predicate ndtree:*predicates* :test #'equal)
           (setf (ns:namespace-dtree predicate) (ndtree:make-dtree)))))

(defun namespace-p (x)
  (typep x 'namespace))

(defun primary-prefix (namespace)
  (let ((prefix (slot-value namespace 'prefix)))
    (if (atom prefix) prefix (first prefix))))

(defmethod print-object ((obj namespace) stream)
  (cond ((and (slot-boundp obj 'prefix) (slot-value obj 'prefix))
         (print-unreadable-object (obj stream :type nil :identity t)
           (princ (primary-prefix obj) stream)))
        (t (call-next-method))))

(defmethod find-namespace ((namespace string))
  "returns a namespace."
  (if (string= namespace "http://www.w3.org/2002/07/owl")
      (find-namespace (concatenate 'string namespace "#"))
    (gethash namespace *namespaces*)))

(defmethod find-namespace ((namespace iri:iri))
  "returns a namespace by using iri-string of namespace."
  (find-namespace (iri:iri-string namespace)))

(defmethod find-imports ((imports string))
  "returns a namespace for import."
  (cond ((gethash imports *namespaces*))
        ((and (not (nc:ends-with-char #\# imports))
              (gethash (concatenate 'string imports "#") *namespaces*)))
        ((and (not (nc:ends-with-char #\/ imports))
              (gethash (concatenate 'string imports "/") *namespaces*)))
        (t (cond ((nc:ends-with-char #\# imports) (create-namespace imports))
                 ((nc:ends-with-char #\/ imports) (create-namespace imports))
                 (t (error "Not Yet!"))))))

(defmethod find-ontology ((ontology string))  ; ontology should be a namespace node
  "returns a namespace for ontology."
  (cond ((gethash ontology *namespaces*))
        ((and (not (nc:ends-with-char #\# ontology))
              (gethash (concatenate 'string ontology "#") *namespaces*)))
        ((and (not (nc:ends-with-char #\/ ontology))
              (gethash (concatenate 'string ontology "/") *namespaces*)))
        (t (cond ((nc:ends-with-char #\# ontology) (create-namespace ontology))
                 ((nc:ends-with-char #\/ ontology) (create-namespace ontology))
                 (t (error "Not Yet!"))))))

(defun find-namespace-by-prefix (prefix)
  "returns an interned namespace associated to <prefix>."
  (setq prefix (string prefix))
  (with-hash-table-iterator (ns-gen *namespaces*)
    (labels ((do-loop (exists &optional key namespace)
               (declare (ignore key))
               (when exists
                 (cond ((member (string prefix) (namespace-prefix namespace) :test #'equalp)
                        namespace)
                       (t (multiple-value-call #'do-loop (ns-gen)))))))
      (multiple-value-call #'do-loop (ns-gen)))))

(defmethod create-namespace ((namespace iri:iri) &optional (prefix nil))
  "makes a namespace for iri-string of <namespace>."
  (create-namespace (iri:iri-string namespace) prefix))

(defmethod create-namespace ((namespace string) &optional (prefix nil))
  "makes a namespace for <namespace> in string. If optional <prefix> is given, 
   it is used for the prefix to <prefix-iri>, otherwise nil for the prefix."
  (let ((ns (gethash namespace *namespaces*)))
    (cond ((null ns)
           (setq ns (if prefix 
                        (make-instance 'namespace :prefix (list (string prefix)))
                      (make-instance 'namespace)))
           (setf (gethash namespace *namespaces*) ns)
           (setf (namespace-iri ns) (intern-namespace namespace)) ; selfish registration with localname = "" 
           ns)
          (t (cond ((null prefix))                                              ; nothing done
                   ((null (namespace-prefix ns))
                    (setf (namespace-prefix ns) (list (string prefix))))
                   ((member prefix (namespace-prefix ns) :test #'string-equal)) ; nothing done
                   (t (warn "Alternative prefix ~S designated to ~S." prefix namespace)
                      (setf (namespace-prefix ns)
                        (append (namespace-prefix ns) (list (string prefix))))))
             ns))))
#|
(ns:create-namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf")
(ns:create-namespace "http://www.w3.org/2000/01/rdf-schema#" "rdfs")
(ns:create-namespace "http://www.w3.org/2002/07/owl#" "owl")
(ns:find-namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(ns:find-namespace "http://www.w3.org/2000/01/rdf-schema#")
(ns:find-namespace-by-prefix "rdf")
(ns:find-namespace-by-prefix "rdfs")

(create-namespace "http://xmlns.com/foaf/0.1/" "foaf")
(find-namespace "http://xmlns.com/foaf/0.1/")
(find-namespace-by-prefix "foaf")

(create-namespace "http://www.ipadic.jp/270/schema#" "ipa")

(create-namespace "http://rdf.freebase.com/ns/" "freebase")
|#

(defun list-all-namespaces ()
  "returns a list of namespaces registered in system."
  (let ((lst nil))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (push val lst))
             *namespaces*)
    (nreverse lst)))

(defun list-all-localnames-in (namespace)
  "returns a list of localnames in <namespace>."
  (loop for localname being each hash-key in (namespace-dictionary namespace)
      collect localname))

(defun list-all-iris-in (namespace)
  "returns a list of entries in <namespace> by IRI."
  (loop for iri being each hash-value in (namespace-dictionary namespace)
      collect iri))

(defmethod compute-localname-from-iri ((iri iri:iri))
  (compute-localname-from-iri (iri:iri-string iri)))
(defmethod compute-localname-from-iri ((iri string))
  (let ((ns-string
         (car
          (sort 
           (loop for ns being each hash-key in *namespaces*
               when (nc:match ns iri)
               collect ns)
           #'> :key #'(lambda (x) (length x))))))
    (when ns-string
      (values (subseq iri (length ns-string)) ns-string))))

#|
(in-package :ns)
(ns:create-namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf")
(ns:create-namespace "http://rdf.freebase.com/ns/" "freebase")
(ns:create-namespace "http://example.com/OK/" "ex")
(ns:compute-localname-from-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property")
(ns:compute-localname-from-iri "http://rdf.freebase.com/ns/type.object.name")
(ns:compute-localname-from-iri "http://example.com/OK/Studio_4°C")    ; This module do not care DEGREE SIGN
(ns:compute-localname-from-iri "http://example.com/OK/Studio_°C") ; do not care DEGREE SIGN (U+00B0)
(ns:compute-localname-from-iri "http://example.com/OK/Studio_℃")   ; U+2103
(ns:compute-localname-from-iri "http://example.com/OK/Studio_?")   ; U+2109 https://www.fileformat.info/info/unicode/char/2109/index.htm
|#

;;;
;;;
;;;

(defvar *namespace* nil "current namespace")

(defun in-namespace (prefix)
  "changes the current namespace to that for <prefix>."
  (setq *namespace* (find-namespace-by-prefix prefix)))

;;;
;;; Note that null string local name "" comes up from the statement of ontologies,
;;; like "<http://www.w3.org/1999/02/22-rdf-syntax-ns#> a owl:Ontology ."
;;;

(defun intern-iri (iristring &optional (start 0) (end (length iristring)))
  "interns <iristring> and returns an IRI."
  (let ((iristr (subseq iristring start end)))
    (multiple-value-bind (localname namespace) (compute-localname-from-iri iristr)
      (if namespace
          (let ((*namespace* (find-namespace namespace)))
            (let ((dictionary (namespace-dictionary *namespace*)))
              (or (gethash localname dictionary)
                  (setf (gethash localname dictionary) (iri:iri iristr)))))
        (let ((iri (iri:iri iristr)))
          (multiple-value-setq (localname namespace) (iri:decompose-iri iri))
          (if namespace
              (let ((*namespace* (create-namespace namespace)))
                (let ((dictionary (namespace-dictionary *namespace*)))
                  (setf (gethash localname dictionary) iri)))
            ;; no namespace or namespace iri itself
            (let ((namespace (find-namespace iristr)))
              (cond (namespace (namespace-iri namespace))
                    (t (warn "IRI ~S is refered." iristr)
                       (iri:iri iristr))))))))))
#|
(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
|#

(defun intern-namespace (nsstring &optional (start 0) (end (length nsstring)))
  "interns <nsstring> and returns an IRI. Just only for suppressing warning in decomposition."
  (let ((nsstr (subseq nsstring start end)))
    (multiple-value-bind (localname namespace) (compute-localname-from-iri nsstr)
      (if namespace
          (let ((*namespace* (find-namespace namespace)))
            (let ((dictionary (namespace-dictionary *namespace*)))
              (or (gethash localname dictionary)
                  (setf (gethash localname dictionary) (iri:iri nsstr 0 (length nsstr) nil))))) ; suppress warning
        (let ((iri (iri:iri nsstr 0 (length nsstr) nil)))  ; suppress warning
          (multiple-value-setq (localname namespace) (iri:decompose-iri iri))
          (if namespace
              (let ((*namespace* (create-namespace namespace)))
                (let ((dictionary (namespace-dictionary *namespace*)))
                  (setf (gethash localname dictionary) iri)))
            ;; no namespace or namespace iri itself
            (let ((namespace (find-namespace nsstr)))
              (cond (namespace (namespace-iri namespace))
                    (t (warn "IRI ~S is refered." nsstr)
                       (iri:iri nsstr))))))))))
#|
(ns:intern-namespace "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
|#

#|
(in-package :ns)

(defun iri-test-index ()
  (let ((p (ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")))
    (let ((props `((,p a b) (,p a c) (,p a ?x) (,p b c)
                   (,p b (f c)) (,p a (f . ?x))
                   (,p
                    ,(iri:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property")
                    ,(iri:iri "http://www.w3.org/2000/01/rdf-schema#Resource")))))
      (ndtree:clear-dtrees)
      (mapc #'ndtree:index props)
      (write (list props (ndtree:get-dtree p))
             :circle t :array t :pretty t)
      (values))))

(iri-test-index)
(ndtree:fetch `(,(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") ?x ?y))
(ndtree:fetch `(,(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") ?x c))
(ndtree:fetch `(,(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") a ?y))
(ndtree:fetch `(,(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") ?x ,(iri:iri "http://www.w3.org/2000/01/rdf-schema#Resource")))
(ndtree:fetch `(,(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") ,(iri:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property") ?y))
|#

(defun find-iri (iristr &optional (start 0) (end (length iristr)))
  "finds iri that iri string equals <iristr> and returns the IRI."
  (let ((iristr (subseq iristr start end)))
    (multiple-value-bind (localname namespace) (compute-localname-from-iri iristr)
      (when namespace
          (let ((*namespace* (find-namespace namespace)))
            (let* ((dictionary (namespace-dictionary *namespace*))
                   (iri (gethash localname dictionary)))    ; localname may be "".
              iri))
        ))))
#|
(eq (ns:intern-iri "http://www.w3.org/2000/01/rdf-schema#Resource")
    (ns:intern-iri "http://www.w3.org/2000/01/rdf-schema#Resource"))
|#
#|
(ns:find-iri "http://rdf.freebase.com/ns/automotive.engine.engine_type")
|#

(defun intern-local-name (local-name &optional (namespace *namespace*))
  "interns <local-name> into <namespace> and returns the relevant interned IRI."
  (let ((dictionary (namespace-dictionary namespace))
        (iristr (concatenate 'string (iri:iri-string (ns:namespace-iri namespace)) local-name)))
    (setf (gethash local-name dictionary)
      (ns:intern-iri iristr 0 (length iristr)))))

;;;
;;; Any IRI must be unique in systems, because any IRI is unique in the WWW. The 
;;; uniqueness of IRI is assured by interning it in lisp systems like lisp symbols. 
;;;

#|
(in-package :ns)
(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property")
(ns:intern-iri "http://www.w3.org/2000/01/rdf-schema#Resource")
(ns:intern-iri "http://www.w3.org/2002/07/owl#Thing")
(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(ns:intern-iri "http://www.ipadic.jp/270/schema#Noun")
(ns:intern-iri "http://www.ipadic.jp/270/schema#Noun-固有名詞")
(ns:intern-iri "http://ja.dbpedia.org/resource/人工知能")
(ns:intern-iri "http://rdf.freebase.com/ns/type.object.name")
(ns:intern-iri "http://example.com/OK/Studio_4°C")    ; This module do not care DEGREE SIGN
|#
#+:common-graphics
(defun ask-user-for-string (prompt string1 option1 option2 prompt2)
  (cg:ask-user-for-string prompt string1 option1 option2 prompt2))
#-:common-graphics
(defun ask-user-for-string (prompt string1 option1 option2 prompt2)
  (declare (ignore option1 option2))
  (format t "~%~A ~A:" prompt prompt2)
  (let ((str (read-line t)))
    (if (zerop (length str)) (values str nil "" nil)
      (values str nil "" t))))

(defun ask-user-prefix-name (ns)
  "asks user a prefix name associated to <ns> iri."
  (multiple-value-bind (prefix str2 button enter)
      (ask-user-for-string "QName prefix" "prefix" "Enter" "Cancel" (format nil "as ~A" ns))
    (declare (ignore str2 button))
    (cond (enter (unless (nc:null-string-p prefix) prefix))
          (t nil))))

;;; In this module, the character encoding is UNICODE in NFC, and it is assumed that 
;;; strings or character sequences in external format is in UTF-8.

;;;
;;; % encoding and deconding
;;;
#|
(defun %-encode-for-iri (string)
  "<string> is a string of iri parts, i.e., hier-part, query, and fragment. "
  (multiple-value-bind (colon-pos ?-pos number-sign-pos)
      (iri-decompose string)
    (let ((scheme (subseq string 0 colon-pos))
          (hier-part (%-encode-for-hier-part (subseq string (1+ colon-pos) ?-pos)))
          (query (%-encode-for-query (subseq string (1+ ?-pos) number-sign-pos)))
          (fragment (%-encode-for-fragment (subseq string (1+ number-sign-pos)))))
      (let ((iristr (concatenate 'string scheme ":" hier-part)))
        (when query
          (setq iristr (concatenate 'string iristr "?" query)))
        (when fragment
          (setq iristr (concatenate 'string iristr "#" fragment)))))))

(defun %-encode-for-hier-part (string)
  )

(defun %-encode-for-query (string)
  )

(defun %-encode-for-fragment (string)
  )

(defun %-decode-for-iri (string)
  string
  )
|#
(defun find-iri-from-symbol (sym)
  (let ((namespace (find-namespace-by-prefix (package-name (symbol-package sym)))))
    (when namespace
      (let ((dictionary (namespace-dictionary namespace)))
        (when dictionary
          (gethash (symbol-name sym) dictionary))))))

(defun iri-same-p (x y)
  (cond ((null x) nil)
        ((symbolp x)
         (cond ((symbolp y) (eq x y))
               ((iri:iri-p y) (iri:iri= (find-iri-from-symbol x) y))
               ((stringp y) (string= (iri:iri-string (find-iri-from-symbol x)) y))))
        ((iri:iri-p x)
         (cond ((symbolp y) (iri:iri= x (find-iri-from-symbol y)))
               ((iri:iri-p y) (iri:iri= x y))
               ((stringp y) (string= x (iri:iri-string (find-iri-from-symbol y))))))
        ((stringp x)
         (cond ((symbolp y) (string= x (iri:iri-string (find-iri-from-symbol y))))
               ((iri:iri-p y) (string= x (iri:iri-string y)))
               ((stringp y) (string= x y))))))

(defun export-as-QName (symbol)
  "export this <symbol> as QName. The symbol-package of <symbol> is stored 
   into the related uri namespace."
  (proclaim `(special ,symbol))
  (let ((pkg (symbol-package symbol)))
    (when pkg
      (export symbol pkg)
      (let ((ns (or (find-namespace-by-prefix (package-name pkg))
                    (documentation pkg t))))
        (when (and (not (string= "rdf" (package-name pkg))) (null ns))
          (warn "No namespace for package ~S for ~S" pkg symbol))))))

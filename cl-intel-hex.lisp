;;;; cl-intel-hex.lisp
;;;;
;;;; Copyright (c) 2015 Siddharth Heroor

(in-package #:cl-intel-hex)


;; Constants

(defconstant +data-record+ #x00)
(defconstant +eof-record+ #x01) 
(defconstant +extended-segment-address-record+ #x02)
(defconstant +start-segment-address-record+ #x03)
(defconstant +extended-linear-address-record+ #x04)
(defconstant +start-linear-address-record+ #x05)


;; Class Definitions

(defclass record ()
  ((byte-count
    :initarg :byte-count
    :initform #x0
    :reader byte-count
    :documentation "Length of Data in 2 Hex Digits")
   (address
    :initarg :address
    :initform #x0
    :accessor address
    :documentation "16 Bit Address Offset in 4 Hex Digits")
   (record-type
    :initarg :record-type
    :initform +data-record+
    :accessor record-type
    :documentation "Type of data in 2 Hex Digits")
   (data
    :initarg :data
    :initform #()
    :accessor data
    :documentation "Sequence of n bytes in 2n Hex Digits")
   (checksum 
    :initarg :checksum
    :initform #x0
    :reader checksum
    :documentation "Computed checkum in 2 Hex Digits"))
  (:documentation "Represents a record in a Hex file."))


(define-condition invalid-parameter (error)
  ((text :initarg :text :reader text)))

(define-condition malformed-hex-string (error)
  ((text :initarg :text :reader text)))

(define-condition checksum-failure (error)
  ((text :initarg :text :reader text)))

;; Class Methods

(defmethod initialize-instance :after ((r record) &key)
  "Automatically calculate byte-count and checksum"
  (setf (slot-value r 'byte-count) (length (slot-value r 'data)))
  (setf (slot-value r 'checksum) (calculate-checksum r)))

(defmethod byte-count ((r record))
  "Get the Byte Count of the record"
  (slot-value r 'byte-count))


(defmethod address ((r record))
  "Get the Address of the record"
  (slot-value r 'address))

(defmethod (setf address) (value (r record))
  "Set the Address of the record"
  (if (> value #xFFFF)
      (error 'invalid-parameter
	     :text "Invalid Address. Value out of range"))
  (setf (slot-value r 'address) value)
  (setf (slot-value r 'checksum) (calculate-checksum r)))

(defmethod record-type ((r record))
  "Get the Record Type of the record"
  (slot-value r 'record-type))

(defmethod (setf record-type) (value (r record))
  "Set the Record Type of the record"
  (if (or (< value +data-record+) 
	   (> value +start-linear-address-record+))
      (error 'invalid-parameter 
	     :text "Invalid record-type. Value out of range."))
  (setf (slot-value r 'record-type) value)
  (setf (slot-value r 'checksum) (calculate-checksum r)))


(defmethod data ((r record))
  "Get the Data of the record"
  (slot-value r 'data))

(defmethod (setf data) (value (r record))
  "Set the Data of the record"
  (if (not (typep value '(vector (unsigned-byte 8))))
      (error 'invalid-parameter 
	     :text "Invalid data type. Need an array of unsigned 8 bit integers"))
  (setf (slot-value r 'byte-count) (length value))
  (setf (slot-value r 'data) value)
  (setf (slot-value r 'checksum) (calculate-checksum r)))


(defmethod checksum ((r record))
  "Get the Checksum of the record"
  (slot-value r 'checksum))


(defun make-record-from-string (input-string)
  "make a record from a hex-string representation"
  (check-type input-string string)
  (if (not (eq (char input-string 0) #\:))
      (error 'malformed-hex-string :text "Invalid record string header"))
  (macrolet ((parse-integer-hex (string start end)
	       `(parse-integer (subseq ,string ,start ,end) :radix 16)))
    (flet ((parse-header-from-string (str)
	     (let ((minimum-length-string 11))
	       (if (< (length str) minimum-length-string)
		   (error 'malformed-hex-string :text "Insufficient record string length"))
	       (values (parse-integer-hex str 1 3)
		       (parse-integer-hex str 3 7)
		       (parse-integer-hex str 7 9))))
	   (parse-data-from-string (substr)
	     (loop for i below (length substr) by 2
		collect (parse-integer-hex substr i (+ i 2))))
	   (parse-checksum-from-string (str data-length)
	     (let ((start (+ 9 data-length)))
	       (parse-integer-hex str start (+ start 2)))))
      (multiple-value-bind
	    (byte-length-value address-value record-type-value)
	  (parse-header-from-string input-string)
	(if (or (< record-type-value +data-record+)
		(> record-type-value +start-linear-address-record+))
	    (error 'malformed-hex-string :text "Invalid Record Type"))
	(let ((r (make-instance 'record
		     :address address-value
		     :record-type record-type-value
		     :data (coerce (parse-data-from-string
				    (subseq input-string 9 (+ 9 (* byte-length-value 2))))
				   '(vector (unsigned-byte 8))))))
	  (if (/= (parse-checksum-from-string input-string (* 2 byte-length-value))
		  (checksum r))
	      (error 'checksum-failure :text "Invalid Checksum")) 
	  r)))))


(defmethod calculate-checksum ((r record))
  "Calculate the 16 Bit Checksum"
  (with-slots (byte-count address record-type data) r
    (loop for i across data
       sum i into checksum
       finally (return (+ 1 (logandc1
			     (+ checksum
				byte-count
				(ldb (byte 8 8) address)
				(ldb (byte 8 0) address)
				record-type)
			     #xFF))))))

	


(defmethod print-object ((r record) stream)
  (print-unreadable-object (r stream :type t :identity t))
  (format stream "~%byte-count: ~2,'0X" (byte-count r))
  (format stream "~%address: ~4,'0X" (address r))
  (format stream "~%record-type: ~2,'0X" (record-type r))
  (format stream "~%data: ~2,'0X" (data r))
  (format stream "~%checksum: ~2,'0X" (checksum r)))

(defmethod format-record ((r record) stream)
  "Serialize the record into the stream"
  (with-slots (byte-count address record-type data checksum) r
    ;; :          - Print :
    ;; ~2,'0X     - Print 2 Bytes with Padding of 0
    ;; ~4,'0X     - Print 4 Bytes with Padding of 0
    ;; ~{~2,'0X~} - Print a List with each element as 2 Bytes with a Padding of 0
    (format stream ":~2,'0X~4,'0X~2,'0X~{~2,'0X~}~2,'0X"
	    byte-count
	    address
	    record-type
	    (coerce data 'list)
	    checksum)))

;http://www.gigamonkeys.com/book/practical-parsing-binary-files.html
;http://golang-challenge.com/go-challenge1/
;http://www.lispworks.com/documentation/lw51/CLHS/Body/f_rd_seq.htm
;http://www.lispworks.com/documentation/lw60/CLHS/Body/f_rd_by.htm

(defconstant +null+ (code-char 0))

(defun read-entire-file (in)
  (loop for byte = (read-byte in nil 'eof)
     until (eq byte 'eof)
     do (format t "~S" byte)
     collect byte))

(defun read-a-little (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (loop for byte = (read-byte in nil 'eof)
          until (eq byte 'eof)
          do (format t "~S" (read-u2 byte))
          collect (read-u2 byte))))

(defun read-u2 (byte)
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) byte)
    (setf (ldb (byte 8 0) u2) byte)
    u2))

(defparameter *fixture-1* (read-entire-file (open "./fixtures/pattern_1.splice" :element-type 'unsigned-byte)))
(defparameter *fixture-2* (read-entire-file (open "./fixtures/pattern_2.splice" :element-type 'unsigned-byte)))
(defparameter *fixture-3* (read-entire-file (open "./fixtures/pattern_3.splice" :element-type 'unsigned-byte)))
(defparameter *fixture-4* (read-entire-file (open "./fixtures/pattern_4.splice" :element-type 'unsigned-byte)))
(defparameter *fixture-5* (read-entire-file (open "./fixtures/pattern_5.splice" :element-type 'unsigned-byte)))

(defun read-splice (fixture)
  (loop for item in (subseq fixture 0 6)
        for char = (code-char item)
     collect char))

(defun read-version (fixture)
 (loop  for item in (subseq fixture 14 25)
        for char = (code-char item)
        collect char))

;; this should return:
;; (0) kick     |x---|x---|x---|x---|

(defun read-first-track (fixture)
  (loop for item in (subseq fixture 55 79)
     collect (parse-track-item item)))

(defun read-second-track (fixture)
  (loop for item in (subseq fixture 80 101)
     collect (parse-track-item item)))



(defun parse-track-item (item)
  (cond ((= item 0) "-")
        ((= item 1) "x")
        (t (code-char item))))


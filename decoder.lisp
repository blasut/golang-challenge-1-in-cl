(with-open-file (s "/Users/jite/code/golang-challenge-1/fixtures/pattern_1.splice" :element-type 'unsigned-byte)
  (format t "~S ~S" (read-byte s) (read-byte s nil 'eof)))

; First 6 bytes should be read.o
(READ-NULL-TERMINATED-ASCII (open "/Users/jite/code/golang-challenge-1/fixtures/pattern_1.splice" :element-type '(unsigned-byte 8)))

(defconstant +null+ (code-char 0))

(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte in))
       until (char= char +null+) do (write-char char s))))

(defun write-null-terminated-ascii (string out)
  (loop for char across string
        do (write-byte (char-code char) out))
  (write-byte (char-code +null+) out))
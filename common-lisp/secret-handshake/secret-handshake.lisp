(defpackage :secret-handshake
  (:use :cl)
  (:export :commands))

(in-package :secret-handshake)

(defmacro pop-bit (n)
    `(let ((bit (mod ,n 2)))
         (setf ,n (truncate ,n 2))
         (not (zerop bit))))

(defun commands (number)
    (let ((actions nil))
        (if (pop-bit number) (push "wink" actions))
        (if (pop-bit number) (push "double blink" actions))
        (if (pop-bit number) (push "close your eyes" actions))
        (if (pop-bit number) (push "jump" actions))
        (unless (pop-bit number) (setf actions (reverse actions)))
        actions
        ))

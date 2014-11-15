c
(defun add (x y) 
       (if (eq x 0) y (add (- x 1) y)))

(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

;binds each enry in *db* to cd variable to perform the function given
;~10t tells the printer to shift 10 columns over for printing the next variable
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;call to force-output is required in some cl implmentations to force it to not wait for a newline
;*query-io* is a global variable that contains input stream from terminal
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*)) 

;combine functions to make interactive prompt to make cd's
;y-or-n-p will re-prompt the user if they don't enter y/n valid value
(defun prompt-for-cd ()
  (make-cd 
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped (y/n)")))



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
;~10t tells the printer to 
;shift 10 columns over for printing the next variable
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;call to force-output is required in some cl implmentations to force it to not wait for a newline
;*query-io* is a global variable that contains input stream from terminal
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;combine functions to make interactive prompt to make cd's
;y-or-n-p will re-prompt the user if they don't enter y/n valid value
(defun prompt-for-cd ()
  (make-cd 
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped (y/n)")))


(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;setf is main assignment operator
;warning-this currently clobbers current value of *db*
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;first pass at selection
; #' says give me function named by - in case of lambda it points to lamba function definition
(defun select-by-artist1 (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

;more general selector
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;wrap up the creation of the anonymous function
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))


;first pass at where clause...
(defun where1 (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) title) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;ex (update1 where1 :artist "Dixie Chicks' :rating 11)
(defun update1 (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title));set a field on the row being returned
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     ;mapcar creates a row from the above fields and puts it into db
	     row) 
	 *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


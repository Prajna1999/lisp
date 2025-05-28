(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)
; add items to the *db* list
(defun add-record (cd) (push cd *db*))

; prettify the list
(defun dump-db ()

  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%~%" cd)))


(defun prompt-read (prompt)

  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))


(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [y/N")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))


; save to database
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

; load the database from db
(defun load-db (filename)
  (with-open-file (in filename)

    (with-standard-io-syntax
      (setf *db* (read in)))))


; query the database and select by artist
(defun select-by-artist (artist)

  (lambda (cd) (equal (getf cd :artist) artist)))

; query the database by title
(defun select-by-title (title)

  (lambda (cd) (equal (getf cd :title) title)))

; select by title and artist
(defun select-by-title-and-artist (title artist)
  (remove-if-not
      (lambda (cd)
        (and (equal (getf cd :title) title)
             (equal (getf cd :artist) artist)))
      *db*))

; generic select query
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

; general selector function generator
(defun where (&key title artist rating (ripped nil ripped-p))
  (lambda (cd)

    (and

     (if title (equal (getf cd :title) title) t)
     (if artist (equal (getf cd :artist) artist) t)
     (if rating (equal (getf cd :rating) rating) t)
     ;  if the argument ripped is actually passed ripped-p=t
     (if ripped-p (equal (getf cd :ripped) ripped) t))))

; general updater function 

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
    (mapcar
        (lambda (row)
          (when (funcall selector-fn row)
                (if title (setf (getf row :title) title))
                (if artist (setf (getf row :artist) artist))
                (if rating (setf (getf row :rating) rating))
                (if ripped-p (setf (getf row :ripped) ripped)))
          row) *db*)))

; add a function to delete rows from a database
(defun delete-rows (selector-fn)

  (setf *db* (remove-if selector-fn *db*)))
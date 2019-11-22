;;;; -wallpapers.lisp
(in-package #:wallpapers)

(defparameter *wallpapers-path*  "wallpapers/unsplash/")
(defparameter *modules-path* stumpwm::*module-dir*)
(defparameter *full-path* (format nil "~a~a" *modules-path* *wallpapers-path*))


;; PATH -> STRING
;; Produces a string  "PATH/*.*"
;;(wallpapers::files-in-path wallpapers::*full-path*)
(defun files-in-path (path)
  (format nil "~a*.*" path))

;; PATH-TO-FILE -> MIMETYPE
;;(wallpapers::mimetype (car (directory (wallpapers::files-in-path wallpapers::*full-path*))))
(defun mimetype (file)
  (string-right-trim
   '(#\newline)
   (stumpwm:run-shell-command
    (format nil "file -b --mime-type ~a" file) t)))


;; FILE -> BOOLEAN
;; Tells if file is image
;;(wallpapers::imagep (car (directory (wallpapers::files-in-path wallpapers::*full-path*))))
(defun wallpaperp (file)
  (let ((m (mimetype file))) ;;get mimetype so we only pass images
    (member m '("image/jpeg"
                "image/jpg"
                "image/png"
                "image/tiff"
                "image/bmp"
                "image/gif")
            :test #'equal)))

;; PATH -> LIST
;; Outputs a list of wallpapers in path
;; (wallpapers::list-of-wallpapers wallpapers::*full-path*)
(defun list-of-wallpapers (path)
  (let ((l (directory (files-in-path path))))
    (remove-if-not #'wallpaperp l)))

;; PATH -> STATE
;; (wallpapers::set-random-wallpaper wallpapers::*full-path*)
;; WARNING: GIF ONLY FOR STATIC
(defun set-random-wallpaper (path)
  (sb-thread:make-thread
   (lambda ()
     (let* ((d1 (list-of-wallpapers path)) ; path to wallpapers
            (rs (make-random-state t)) ; generate a random state
            (r (random (list-length d1) rs))) ; generate random number of length of d1
       (run-shell-command
        (format nil "feh --bg-fill ~a" (nth r d1)))))))


;; (wallpapers::wallpapers-timer 10 10 wallpapers::*full-path*)
(defun wallpapers-timer (secs repeat path)
  (stumpwm:run-with-timer secs repeat #'set-random-wallpaper path))

;; for stumpwm
(defcommand random-wallpaper ()
  ((:variable nil))
  (progn (kill-wallpaper-timer)
         (set-random-wallpaper *full-path*)))

(defcommand multiple-wallpapers
  (&optional (delay 0) (change 120))
  ((:number "give a delay ")
   (:number "give a change "))
  (progn (kill-wallpaper-timer)
         (wallpapers-timer delay change *full-path*)))

;; kills all the timers of set-random-wallpaper
(defun kill-wallpaper-timer ()
  (loop for timer in stumpwm::*timer-list*
        if (equal #'set-random-wallpaper ;; this is the function passed to wallpapers-timer
                  (stumpwm::timer-function timer))
        do (stumpwm::cancel-timer timer)))

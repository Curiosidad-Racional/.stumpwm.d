(in-package #:stumpwm)

(export '(*transparency-p*
          *transparency-focus*
          *transparency-focus-default*
          *transparency-unfocus*
          *transparency-unfocus-default*))

(set-prefix-key (kbd "s-SPC"))
;;; Functions
(defun run-or-raise-prefer-group (cmd win-cls)
  "If there are windows in the same class, cycle in those. Otherwise call
run-or-raise with group search t."
  (let ((windows (group-windows (current-group))))
    (if (member win-cls (mapcar #'window-class windows) :test #'string-equal)
        (run-or-raise cmd `(:class ,win-cls) nil T)
      (run-or-raise cmd `(:class ,win-cls) T T))))

(defun set-window-transparency (window opacity)
  (run-shell-command
   (format nil "type transset && transset --id ~d ~d"
           (xlib:window-id (window-xwin window))
           opacity)))

(defun set-focus-window-transparency (window)
  (set-window-transparency
   window
   (or (cdr (assoc (window-class window) *transparency-focus* :test 'string-equal))
       *transparency-focus-default*)))

(defun set-unfocus-window-transparency (window)
  (set-window-transparency
   window
   (or (cdr (assoc (window-class window) *transparency-unfocus* :test 'string-equal))
       *transparency-unfocus-default*)))

(defun focus-window-transparency (current-window last-window)
  (when *transparency-p*
    (set-focus-window-transparency current-window)
    (if last-window (set-unfocus-window-transparency last-window))))
(add-hook *focus-window-hook* 'focus-window-transparency)

;;; Commands
;; Monitors
(defcommand monitor-left () ()
  (run-shell-command "type setup && setup monitor left")
  (refresh-heads))
(defcommand monitor-rigth () ()
  (run-shell-command "type setup && setup monitor right")
  (refresh-heads))
;; Transparency
(defcommand transparency-window (opacity)
  ((:number "Enter transparency: "))
  (cond
   ((= 0 opacity)
    (set-window-transparency (current-window) 1))
   ((< 1 opacity)
    (throw 'error "Big numbers not allowed."))
   ((< opacity 0)
    (throw 'error "Negative numbers not allowed."))
   (t (set-window-transparency (current-window) opacity))))

(defcommand transparency-enable () ()
  (setf *transparency-p* t)
  (let ((cur-win (current-window)))
    (dolist (group (sort-groups (current-screen)))
      (dolist (window (group-windows group))
        ;; Don't include the current window in the list
        (unless (eq window cur-win)
          (set-unfocus-window-transparency window))))
    (set-focus-window-transparency cur-win)))

(defcommand transparency-disable () ()
  (setf *transparency-p* nil)
  (dolist (group (sort-groups (current-screen)))
    (dolist (window (group-windows group))
      (set-window-transparency window 1))))

;; exec
(defcommand ec () ()
  (run-or-raise-prefer-group "emacsclient -n -c" "Emacs"))
(defcommand firefox () ()
  (run-or-raise-prefer-group "firefox" "Firefox"))
(defcommand google-chrome () ()
  (run-or-raise-prefer-group "google-chrome" "Google-chrome"))
(defcommand xterm () ()
  (run-or-raise-prefer-group "xterm" "XTerm"))
(defcommand workbench () ()
  (run-or-raise-prefer-group "mysql-workbench" "Mysql-workbench"))

(defcommand resize-width (width-inc)
  ((:number "Enter width increment: "))
  (resize width-inc 0))

(defcommand resize-height (height-inc)
  ((:number "Enter width increment: "))
  (resize 0 height-inc))

(defcommand xscreensaver-command (arg)
  ((:string "Enter argument: "))
  (run-shell-command (concatenate 'string "xscreensaver-command " arg)))

(defcommand menu () ()
  (labels ((pick (options)
                 (let ((selection (stumpwm::select-from-menu (current-screen) options "")))
                   (cond
                    ((null selection)
                     (throw 'error "Abort."))
                    ((stringp (second selection))
                     (second selection))
                    (t
                     (pick (cdr selection)))))))
    (let ((choice (pick *app-menu*)))
      (run-shell-command choice))))

;;; Keys
;; (load (merge-pathnames "bindings.lisp" *load-truename*))

(define-key *root-map* (kbd "e") "ec")
(define-key *root-map* (kbd "f") "firefox")
(define-key *root-map* (kbd "c") "google-chrome")
(define-key *root-map* (kbd "t") "xterm")
(define-key *root-map* (kbd "q") "workbench")

(define-key *top-map* (kbd "s-l") "xscreensaver-command -lock")
(define-key *top-map* (kbd "s-x") "colon")
(define-key *top-map* (kbd "s-.") "eval")
(define-key *top-map* (kbd "s-!") "exec")
(define-key *top-map* (kbd "s-a") "toggle-modeline")
(define-key *top-map* (kbd "C-s-g") "grouplist")
(define-key *top-map* (kbd "s-n") "next-in-frame")
(define-key *top-map* (kbd "s-N") "next") ;; group
(define-key *top-map* (kbd "s-p") "prev-in-frame")
(define-key *top-map* (kbd "s-P") "prev") ;; group
(define-key *top-map* (kbd "s-o") "fother")
(define-key *top-map* (kbd "s-Tab") "fnext")
(define-key *top-map* (kbd "s-ISO_Left_Tab") "fprev")
(define-key *top-map* (kbd "s-r") "iresize")
(define-key *top-map* (kbd "s-R") "remove")
(define-key *top-map* (kbd "s-v") "vsplit")
(define-key *top-map* (kbd "s-s") "fselect")
(define-key *top-map* (kbd "s-h") "hsplit")

(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-c") "fclear")
(define-key *top-map* (kbd "s-+") "balance-frames")
(define-key *top-map* (kbd "s--") "only")
(define-key *top-map* (kbd "s-w") "windowlist")
(define-key *top-map* (kbd "Menu") "menu")

(define-key *top-map* (kbd "s-Up") "move-focus up")
(define-key *top-map* (kbd "s-Down") "move-focus down")
(define-key *top-map* (kbd "s-Left") "move-focus left")
(define-key *top-map* (kbd "s-Right") "move-focus right")
(define-key *top-map* (kbd "S-s-Up") "move-window up")
(define-key *top-map* (kbd "S-s-Down") "move-window down")
(define-key *top-map* (kbd "S-s-Left") "move-window left")
(define-key *top-map* (kbd "S-s-Right") "move-window right")
(define-key *top-map* (kbd "C-s-Up") "resize 0 10")
(define-key *top-map* (kbd "C-s-Down") "resize 0 -10")
(define-key *top-map* (kbd "C-s-Left") "resize -10 0")
(define-key *top-map* (kbd "C-s-Right") "resize 10 0")

(define-key *top-map* (kbd "s-W") "resize-width")
(define-key *top-map* (kbd "s-H") "resize-height")

(dotimes (i 10)
  (define-key *group-root-map* (kbd (write-to-string i)) nil)
  (define-key *top-map* (kbd (format nil "s-~a" i)) (format nil "select-window-by-number ~a" i)))

(define-key *top-map* (kbd "s-C") "gnew")
(define-key *top-map* (kbd "s-K") "gkill")
(dotimes (i 12)
  (define-key *root-map* (kbd (format nil "F~a" (1+ i))) nil)
  (define-key *top-map* (kbd (format nil "s-F~a" (1+ i))) (format nil "gselect ~a" (1+ i))))

(define-key *top-map* (kbd "s-m") "mark")

;;; Remaps
;; (define-remapped-keys
;;     `((,(lambda (win)
;;           (string-equal "Firefox" (window-class win)))
;;        ("C-k"   . ("C-S-End" "C-x")))))
(define-remapped-keys
    '(("(Firefox|[Cc]hrome)"
       ("C-n"   . "Down")
       ("C-p"   . "Up")
       ("C-f"   . "Right")
       ("C-b"   . "Left")
       ("C-v"   . "Next")
       ("M-v"   . "Prior")
       ("M-w"   . "C-c")
       ("C-w"   . "C-x")
       ("C-y"   . "C-v")
       ("M-<"   . "Home")
       ("M->"   . "End")
       ("C-s"   . "C-f")
       ("C-M-b" . "M-Left")
       ("C-M-f" . "M-Right")
       ("C-k"   . ("C-S-End" "C-x")))
      ("XTerm"
       ("C-y"   . "S-Insert"))))


;;; Menu
(defparameter *app-menu* '(("Google Chrome" "google-chrome")
                           ("Mozilla Firefox" "firefox")
                           ("Emacs Client" "emacsclient -c -n")
                           ("XTerm" "xterm")
                           ("File Manager" "nautilus")
                           ("MySQL Workbench" "mysql-workbench")))

;;; Options
(defparameter *transparency-p* t)
(defparameter *transparency-focus-default* 0.8)
(defparameter *transparency-unfocus-default* 0.65)
(defparameter *transparency-focus* '(("Google-chrome" . 0.85)
                                     ("Firefox" . 0.85)))
(defparameter *transparency-unfocus* '(("Google-chrome" . 0.75)
                                       ("Firefox" . 0.75)))
(set-focus-color "cyan")

(setf *maxsize-border-width* 3
      *transient-border-width* 3
      *normal-border-width* 3
      *window-border-style* :tight
      ;; *default-group-name* "Base"  ;; ignored
      *input-window-gravity* :center
      *message-window-input-gravity* :center
      *message-window-gravity* :center
      *mouse-focus-policy* :click
      *mode-line-timeout* 9.0
      *timeout-wait* 30
      *window-format* "%m%n%s%10c"
      *time-modeline-string* "%a %b %e %H:%M"
      *screen-mode-line-format* "%h:%g^4>^]%w^>^2%d^]        %T")

;; (toggle-mode-line (current-screen)
;;                   (current-head))
;; (defparameter *screen-mode-line-format*
;;       (list '(:eval (run-shell-command "date '+%R, %F %a'|tr -d [:cntrl:]" t)) " | %t | %c| %l | [^B%n^b] %w"))
;; (mode-line)

;;; Modes
(which-key-mode)

;;; Modules
(set-module-dir "~/.stumpwm.d/modules")
;; [ True Type fonts
;; dependency
;; * (ql:quickload "clx-truetype")
;; first time:
;; * (xft:cache-fonts)
;; user fonts directory: `~/.local/share/fonts/`
;; list system fonts: `fc-list`
;; update system fonts: `fc-cache -f` (force) or `fc-cache -r` (really force, clean and force)
(load-module "ttf-fonts")
(set-font (make-instance 'xft:font :family "Iosevka Term" :subfamily "Regular" :size 10))
;; (set-font "-xos4-terminus-medium-r-normal--9-140-72-72-c-80-iso8859-15")
;; ]
;; [ Stumpwm Tray
;; dependency
;; * (ql:quickload "xembed")
(load-module "stumptray")
;; ]
;; [ Background
;; require `feh`
(load-module "wallpapers")
(wallpapers::multiple-wallpapers 0 (* 5 60))
;; ]
;; [ Global windows
(load-module "globalwindows")
(define-key *top-map* (kbd "s-g") "global-windowlist")
(define-key *top-map* (kbd "s-G") "global-pull-windowlist")
;; ]
;; [ Gaps between windows
(load-module "swm-gaps")
;; Head gaps run along the 4 borders of the monitor(s)
(setf swm-gaps:*head-gaps-size* 20
;; Inner gaps run along all the 4 borders of a window
      swm-gaps:*inner-gaps-size* 5
;; Outer gaps add more padding to the outermost borders of a window (touching
;; the screen border)
      swm-gaps:*outer-gaps-size* 0)
;; ]


;;; Programs
(run-shell-command "type emacs && emacs --daemon")
(run-shell-command "xscreensaver -no-splash")
(run-shell-command "type setup && setup monitor left")
(run-shell-command "type compton && compton")
(toggle-mode-line (current-screen)
                  (current-head))
(refresh-heads)
;; [ Stumpwm Tray
(stumptray::stumptray)
;; ]
;; [ Gaps between windows
(swm-gaps::toggle-gaps)
;; ]

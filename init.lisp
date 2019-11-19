(in-package #:stumpwm)

(export '(*app-menu*
          *transparency-p*
          *transparency-focus*
          *transparency-focus-default*
          *transparency-unfocus*
          *transparency-unfocus-default*))

(setf *maxsize-border-width* 3
      *transient-border-width* 3
      *normal-border-width* 3
      *window-border-style* :tight
      *default-group-name* "Def"  ;; ignored
      *input-window-gravity* :center
      *message-window-input-gravity* :center
      *message-window-gravity* :center
      *mouse-focus-policy* :click
      *mode-line-timeout* 9.0
      *timeout-wait* 30
      *window-format* "%m%n%s%10c"
      *time-modeline-string* "%a %b %e %H:%M"
      *screen-mode-line-format* "%h:%g^4>^]%w^>^2%d^]    %T")
;;(restart-soft)

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

(defcommand transparency-toggle () ()
  (if *transparency-p*
      (transparency-disable)
    (transparency-enable)))

;; exec
(defcommand ec () ()
  (run-or-raise-prefer-group "emacsclient -c -n" "Emacs"))
(defcommand firefox () ()
  (run-or-raise-prefer-group "firefox" "Firefox"))
(defcommand chromium-browser () ()
  (run-or-raise-prefer-group "chromium-browser" "Chromium-browser"))
(defcommand google-chrome () ()
  (run-or-raise-prefer-group "google-chrome" "Google-chrome"))
(defcommand xterm () ()
  (run-or-raise-prefer-group "xterm" "XTerm"))
(defcommand workbench () ()
  (run-or-raise-prefer-group "mysql-workbench" "Mysql-workbench"))
(defcommand wireshark () ()
  (run-or-raise-prefer-group "wireshark" "Wireshark"))
(defcommand nautilus () ()
  (run-or-raise-prefer-group "nautilus" "Nautilus"))
(defcommand top () ()
  (run-or-raise-prefer-group "tilda -c 'gotop -pbc solarized'" "Tilda"))
(defcommand pull-top () ()
  (run-shell-command "pkill -x tilda"))

;; frames
(defcommand resize-width (width-inc)
  ((:number "Enter width increment: "))
  (resize width-inc 0))

(defcommand resize-height (height-inc)
  ((:number "Enter width increment: "))
  (resize 0 height-inc))

(defcommand xscreensaver-command (arg)
  ((:string "Enter argument: "))
  (run-shell-command (concatenate 'string "xscreensaver-command " arg)))

(defcommand layout-3 () ()
  (let ((g (current-group (current-screen))))
    (split-frame-in-dir g :row 0.75)
    (split-frame-in-dir g :column 0.15))
  ;; (move-focus-and-or-window :right)
  ;; (let* ((g (current-group (current-screen)))
  ;;        (f (tile-group-current-frame g)))
  ;;   (resize-frame g f (round (* (frame-height f) 1.5)) :height)
  ;;   (resize-frame g f (round (* (frame-width f) 1.7)) :width))
  )

(defcommand hide () ()
  (hide-window (current-window)))

;;; Keys
;; (load (merge-pathnames "bindings.lisp" *load-truename*))
(define-key *top-map* (kbd "s-ESC") "xscreensaver-command -lock")
(define-key *top-map* (kbd "s-x") "colon")
(define-key *top-map* (kbd "s-:") "eval")
(define-key *top-map* (kbd "s-!") "exec")
(define-key *top-map* (kbd "s-a") "mode-line")
(define-key *top-map* (kbd "C-s-g") "grouplist")
(define-key *top-map* (kbd "s-n") "next-in-frame")
(define-key *top-map* (kbd "s-N") "next") ;; group
(define-key *top-map* (kbd "s-p") "prev-in-frame")
(define-key *top-map* (kbd "s-P") "prev") ;; group
(define-key *top-map* (kbd "s-o") "fother")
(define-key *top-map* (kbd "s-Tab") "gnext")
(define-key *top-map* (kbd "s-ISO_Left_Tab") "gprev")
(define-key *top-map* (kbd "s-r") "iresize")
(define-key *top-map* (kbd "s-R") "remove")
(define-key *top-map* (kbd "s-v") "vsplit")
(define-key *top-map* (kbd "s-s") "fselect")
(define-key *top-map* (kbd "s-h") "hsplit")

(define-key *top-map* (kbd "s-f") "fullscreen-gaps")
(define-key *top-map* (kbd "s-F") "fullscreen")
(define-key *top-map* (kbd "s-c") "fclear")
(define-key *top-map* (kbd "s-+") "balance-frames")
(define-key *top-map* (kbd "s--") "only")
(define-key *top-map* (kbd "s-w") "windowlist")
(define-key *top-map* (kbd "s-z") "hide")
(define-key *top-map* (kbd "Menu") "show-menu")

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
(define-key *top-map* (kbd "s-i") "move-focus up")
(define-key *top-map* (kbd "s-k") "move-focus down")
(define-key *top-map* (kbd "s-j") "move-focus left")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-I") "move-window up")
(define-key *top-map* (kbd "s-K") "move-window down")
(define-key *top-map* (kbd "s-J") "move-window left")
(define-key *top-map* (kbd "s-L") "move-window right")
(define-key *top-map* (kbd "C-s-i") "resize 0 10")
(define-key *top-map* (kbd "C-s-k") "resize 0 -10")
(define-key *top-map* (kbd "C-s-j") "resize -10 0")
(define-key *top-map* (kbd "C-s-l") "resize 10 0")

(define-key *top-map* (kbd "s-W") "resize-width")
(define-key *top-map* (kbd "s-H") "resize-height")

(define-key *top-map* (kbd "s-m") "mark")
(define-key *top-map* (kbd "s-C") "gnew")

(define-key *root-map* (kbd "s-r") "refresh-heads")
(define-key *root-map* (kbd "w") "pull-from-windowlist")

(define-key *root-map* (kbd "C-SPC") nil)
(define-key *root-map* (kbd "C-a") nil)
(define-key *root-map* (kbd "C-b") nil)
(define-key *root-map* (kbd "C-c") nil)
(define-key *root-map* (kbd "C-e") nil)
(define-key *root-map* (kbd "C-k") nil)
(define-key *root-map* (kbd "C-m") nil)
(define-key *root-map* (kbd "C-n") nil)
(define-key *root-map* (kbd "C-p") nil)
(define-key *root-map* (kbd "e") "expose")
(define-key *root-map* (kbd "c") nil)

(define-key *group-root-map* (kbd "C-w") nil)
(define-key *group-root-map* (kbd "w") nil)

(dotimes (i 10)
  (define-key *group-root-map* (kbd (write-to-string i)) nil)
  (define-key *top-map* (kbd (format nil "s-F~a" (if (= i 0) 10 i)))
    (format nil "select-window-by-number ~a" i)))

(dotimes (i 12)
  (define-key *root-map* (kbd (format nil "F~a" (1+ i))) nil))
(dotimes (i 9)
  (define-key *top-map* (kbd (format nil "s-~a" (1+ i))) (format nil "gselect ~a" (1+ i))))

(defparameter *toggle-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") "toggle-gaps")
    (define-key m (kbd "f") "toggle-always-on-top")
    (define-key m (kbd "s") "toggle-always-show")
    (define-key m (kbd "m") "mode-line")
    (define-key m (kbd "k") "which-key-mode")
    (define-key m (kbd "t") "transparency-toggle")
    m))
(define-key *top-map* (kbd "s-t") *toggle-map*)

(defparameter *app-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") "ec")
    (define-key m (kbd "E") "exec emacsclient -c -n")
    (define-key m (kbd "f") "firefox")
    (define-key m (kbd "F") "exec firefox")
    (define-key m (kbd "g") "google-chrome")
    (define-key m (kbd "G") "exec google-chrome")
    (define-key m (kbd "c") "chromium-browser")
    (define-key m (kbd "C") "exec chromium-browser")
    (define-key m (kbd "t") "xterm")
    (define-key m (kbd "T") "exec xterm")
    (define-key m (kbd "q") "workbench")
    (define-key m (kbd "Q") "exec workbench")
    (define-key m (kbd "w") "wireshark")
    (define-key m (kbd "W") "exec wireshark")
    (define-key m (kbd "n") "nautilus")
    (define-key m (kbd "N") "exec nautilus")
    (define-key m (kbd "m") "top")
    (define-key m (kbd "M") "pull-top")
    m))
(define-key *top-map* (kbd "s-q") *app-map*)

;;; Remaps
;; (define-remapped-keys
;;     `((,(lambda (win)
;;           (string-equal "Firefox" (window-class win)))
;;        ("C-k"   . ("C-S-End" "C-x")))))
(define-remapped-keys
    '(("(Firefox|[Cc]hrom[ei])"
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
(defparameter *app-menu*
  '(("Aplications"
     ("Chromium Browser"          "chromium-browser")
     ("Google Chrome"             "google-chrome")
     ("Mozilla Firefox"           "firefox")
     ("Emacs Client"              "emacsclient -c -n")
     ("XTerm"                     "xterm")
     ("File Manager"              "nautilus")
     ("MySQL Workbench"           "mysql-workbench")
     ("NetworkManager"            "nm-applet")
     ("System Monitor"            "gnome-system-monitor")
     ("Audacity"                  "audacity")
     ("PulseAudio Volume Control" "pavucontrol")
     ("Gimp"                      "gimp")
     ("Simple Scan"               "simple-scan")
     ("Boot usb creator"          "usb-creator-gtk")
     ("KeePassXC"                 "keepassxc"))
    ("Screen"
     ("Restart conky"         "pkill -x conky; conky -d")
     ("Blank Screen"          "xset s activate")
     ("Standby On"            "xset +dpms s on")
     ("Standby Off"           "xset +dpms s off")
     ("Set transparency"      "transparency-window")
     ("Enable transparency"   "compton")
     ("Disable transparency"  "pkill -x compton"))
    ("Monitors"
     ("Configuration"         "arandr")
     ("Detect Left"           "setup monitor left -w -k")
     ("Detect Right"          "setup monitor right -w -k"))
    ("Screensaver"
     ("Enable screensaver"    "/usr/bin/xscreensaver -no-splash")
     ("Disable screensaver"   "/usr/bin/xscreensaver-command -exit")
     ("Lock screen"           "/usr/bin/xscreensaver-command -lock")
     ("Configure screensaver" "/usr/bin/xscreensaver-command -prefs"))))

;;; Options
(defparameter *transparency-p* t)
(defparameter *transparency-focus-default* 0.85)
(defparameter *transparency-unfocus-default* 0.75)
(defparameter *transparency-focus* '(("Google-chrome" . 0.9)
                                     ("Firefox" . 0.9)
                                     ("Mysql-workbench-bin" . 0.9)
                                     ("Tilda" . 1.0)
                                     ("Totem" . 1.0)))
(defparameter *transparency-unfocus* '(("Google-chrome" . 0.8)
                                       ("Firefox" . 0.8)
                                       ("Mysql-workbench-bin" . 0.8)
                                       ("Tilda" . 1.0)
                                       ("Totem" . 1.0)))
(set-focus-color "cyan")

;; (toggle-mode-line (current-screen)
;;                   (current-head))
;; (defparameter *screen-mode-line-format*
;;       (list '(:eval (run-shell-command "date '+%R, %F %a'|tr -d [:cntrl:]" t)) " | %t | %c| %l | [^B%n^b] %w"))
;; (mode-line)

;;; Modes
(which-key-mode)

;;; Modules
(set-module-dir "~/.stumpwm.d/modules")
;; [ Menu
(load-module "app-menu")
;; ]
;; https://www.oreilly.com/library/view/x-window-system/9780937175149/Chapter05.html
;; (set-font "-misc-fixed-medium-r-normal--10-100-75-75-c-60-iso8859-1") ;; 6x10 small
;; (set-font "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1") ;; 7x13
;; (set-font "-misc-fixed-medium-r-normal--14-130-75-75-c-70-iso8859-1") ;; 7x14
;; [ True Type fonts
;; dependency
;; * (ql:quickload "clx-truetype")
;; first time:
;; * (xft:cache-fonts)
;; user fonts directory: `~/.local/share/fonts/`
;; list system fonts: `fc-list`
;; update system fonts: `fc-cache -f` (force) or `fc-cache -r` (really force, clean and force)
(load-module "ttf-fonts")
(handler-case
    (set-font (make-instance 'xft:font :family "Iosevka Term" :subfamily "Regular" :size 10))
  (error (c)
         (set-font "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1")))
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
(defcommand fullscreen-gaps () ()
  (swm-gaps::toggle-gaps)
  (fullscreen))
;; ]


;;; Programs
(run-shell-command "type emacs && emacs --daemon")
(run-shell-command "type xscreensaver && xscreensaver -no-splash")
(run-shell-command "type setup && setup monitor left" t)
(run-shell-command "type compton && compton")
(run-shell-command "type tilda && type gotop && tilda -c 'gotop -pbc solarized'")
(toggle-mode-line (current-screen)
                  (current-head))
(refresh-heads)
;; [ Stumpwm Tray
(stumptray::stumptray)
;; ]
;; [ Gaps between windows
(swm-gaps::toggle-gaps)
;; ]

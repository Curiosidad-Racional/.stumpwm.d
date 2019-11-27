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
      ;; *default-group-name* "Def"  ;; ignored
      *input-window-gravity* :center
      *message-window-input-gravity* :center
      *message-window-gravity* :center
      *mouse-focus-policy* :click
      *mode-line-timeout* 9.0
      *timeout-wait* 30
      *window-format* "%m%n%s%10c"
      *time-modeline-string* "%a %b %e %H:%M"
      *screen-mode-line-format* "%h:%g^4>^]%w^>^2%d^]    %T")
;; (restart-soft)  ;; bug

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

;; Mouse keyboard
(setf *rat-mode-delta* 16
      *rat-widths* nil
      *rat-heights* nil)

(defun draw-regions (screen)
  "Draw the number of each frame in its corner. Return the list of
windows used to draw the numbers in. The caller must destroy them."
  (let ((w (screen-width screen))
        (h (screen-height screen))
        (x0 0) (y0 0))
    (dolist (d (reverse *rat-widths*))
      (setf w (round w 5)
            x0 (+ x0 (* d w))))
    (setf w (round w 5))
    (dolist (d (reverse *rat-heights*))
      (setf h (round h 3)
            y0 (+ y0 (* d h))))
    (setf h (round h 3))
    (mapcar (lambda (x y l)
              (let ((win (xlib:create-window
                          :parent (screen-root screen)
                          :x (+ x0 (* x w)) :y (+ y0 (* y h)) :width 1 :height 1
                          :background (screen-fg-color screen)
                          :border (screen-border-color screen)
                          :border-width 1
                          :event-mask '())))
                (xlib:map-window win)
                (setf (xlib:window-priority win) :above)
                (echo-in-window win (screen-font screen)
                                (screen-fg-color screen)
                                (screen-bg-color screen)
                                l)
                (xlib:display-finish-output *display*)
                (dformat 3 "mapped ~S~%" l)
                win))
            '(0 1 2 3 4
                0 1 2 3 4
                0 1 2 3 4)
            '(0 0 0 0 0
                1 1 1 1 1
                2 2 2 2 2)
            '("q" "w" "e" "r" "t"
              "a" "s" "d" "f" "g"
              "z" "x" "c" "v" "b"))))

(defun rat-pointer (group screen)
  (let ((w (screen-width screen))
        (h (screen-height screen))
        (x 0) (y 0))
    (dolist (d (reverse *rat-widths*))
      (setf w (round w 5)
            x (+ x (* d w))))
    (dolist (d (reverse *rat-heights*))
      (setf h (round h 3)
            y (+ y (* d h))))
    (warp-pointer screen x y)
    (let ((winner))
      (dolist (f (group-frames group))
        (when (and (> x (frame-x f)) (> y (frame-y f)))
          (if winner
              (when (or (> (frame-x f) (frame-x winner))
                        (> (frame-y f) (frame-y winner)))
                (setf winner f))
            (setf winner f))))
      (if winner (focus-frame group winner)))))

(defcommand rat-choose-region () ()
  "show a number in the corner of each frame and wait for the user to
select one. Returns the selected frame or nil if aborted."
  (let* ((continue t)
         (break-next nil)
         (button 1)
         (group (current-group))
         (screen (group-screen group))
         (wins (draw-regions screen)))
    (unwind-protect
        (loop while continue
              do 
              (multiple-value-bind (has-click ch x y)
                  (read-one-char-or-click group)
                (cond
                 (has-click
                  (setf continue nil))
                 ((null ch)
                  (setf continue nil))
                 ((char= ch #\<)
                  (pop *rat-widths*)
                  (pop *rat-heights*)
                  (mapc #'xlib:destroy-window wins)
                  (setf wins (draw-regions screen)))
                 ((char= ch #\Space)
                  (setf break-next t))
                 ((char= ch #\Escape)
                  (setf button 0
                        continue nil))
                 ((char= ch #\-)
                  (setf *rat-mode-delta* (round *rat-mode-delta* 2)))
                 ((char= ch #\+)
                  (setf *rat-mode-delta* (* 2 *rat-mode-delta*)))
                 ((let ((d (digit-char-p ch)))
                    (if d (setf button d))))
                 ((let ((mov (cdr (assoc ch
                                         '((#\h -1 . 0)
                                           (#\j 0 . 1)
                                           (#\k 0 . -1)
                                           (#\l 1 . 0)
                                           (#\H -2 . 0)
                                           (#\J 0 . 2)
                                           (#\K 0 . -2)
                                           (#\L 2 . 0))))))
                    (if mov
                        (rat-mode-move (car mov) (cdr mov)))
                    mov))
                 ((let ((click (cdr (assoc ch
                                         '((#\m . 1)
                                           (#\, . 2)
                                           (#\. . 3)
                                           (#\p . 4)
                                           (#\LATIN_SMALL_LETTER_N_WITH_TILDE . 5))))))
                    (if click
                        (send-fake-click (current-window) click))
                    click))
                 ((let ((pos (cdr (assoc (char-downcase ch)
                                         '((#\q 0 . 0)
                                           (#\w 1 . 0)
                                           (#\e 2 . 0)
                                           (#\r 3 . 0)
                                           (#\t 4 . 0)
                                           (#\a 0 . 1)
                                           (#\s 1 . 1)
                                           (#\d 2 . 1)
                                           (#\f 3 . 1)
                                           (#\g 4 . 1)
                                           (#\z 0 . 2)
                                           (#\x 1 . 2)
                                           (#\c 2 . 2)
                                           (#\v 3 . 2)
                                           (#\b 4 . 2)) :test 'char=))))
                    (if pos
                        (progn
                          (push (car pos) *rat-widths*)
                          (push (cdr pos) *rat-heights*)
                          (rat-pointer group screen)
                          (mapc #'xlib:destroy-window wins)
                          (setf wins (draw-regions screen))
                          (if (or
                               break-next
                               (char= (char-upcase ch) ch))
                              (setf continue nil)))
                      (message "Unknown key: ~a" ch)))))))
      (mapc #'xlib:destroy-window wins))
    (when (and (< 0 button) (current-window))
      (send-fake-click (current-window) button))))

(defcommand rat-mode-double () ()
  (setf *rat-mode-delta* (* 2 *rat-mode-delta*))
  (message "Mouse delta: ~a" *rat-mode-delta*))

(defcommand rat-mode-half () ()
  (setf *rat-mode-delta* (round *rat-mode-delta* 2))
  (message "Mouse delta: ~a" *rat-mode-delta*))

(defcommand rat-mode-pop-fracs () ()
  (pop *rat-mode-width-fracs*)
  (pop *rat-mode-height-fracs*))

(defcommand rat-mode-frac (width-frac height-frac)
  ((:number "Enter width fraction: ")
   (:number "Enter height fraction: "))
  (push width-frac *rat-mode-width-fracs*)
  (push height-frac *rat-mode-height-fracs*)
  (let* ((cur (current-screen))
         (w (screen-width cur))
         (h (screen-height cur)))
    (dolist (frac *rat-mode-width-fracs*)
      (setf w (round (* frac w) 6)))
    (dolist (frac *rat-mode-height-fracs*)
      (setf h (round (* frac h) 4)))
    (warp-pointer cur w h)))

(defcommand rat-mode-move (fx fy)
  ((:number "Times deltax: ")
   (:number "Times deltay: "))
  (warp-pointer-relative (* fx *rat-mode-delta*) (* fy *rat-mode-delta*)))

(defun rat-mode-enter ()
  (setf *rat-mode-delta* 16
        *rat-mode-width-fracs* nil
        *rat-mode-height-fracs* nil
        *rat-mode-line-background-color* *mode-line-background-color*
        *mode-line-background-color* "red")
  (toggle-mode-line *primary-screen* *primary-head*)
  (toggle-mode-line *primary-screen* *primary-head*))

(defun rat-mode-exit ()
  (setf *mode-line-background-color* *rat-mode-line-background-color*)
  (toggle-mode-line *primary-screen* *primary-head*)
  (toggle-mode-line *primary-screen* *primary-head*))

(define-interactive-keymap rat-mode
  (:on-enter #'rat-mode-enter
             :on-exit #'rat-mode-exit)
  ((kbd "q") "rat-mode-frac 1 1")
  ((kbd "w") "rat-mode-frac 2 1")
  ((kbd "e") "rat-mode-frac 3 1")
  ((kbd "r") "rat-mode-frac 4 1")
  ((kbd "t") "rat-mode-frac 5 1")

  ((kbd "a") "rat-mode-frac 1 2")
  ((kbd "s") "rat-mode-frac 2 2")
  ((kbd "d") "rat-mode-frac 3 2")
  ((kbd "f") "rat-mode-frac 4 2")
  ((kbd "g") "rat-mode-frac 5 2")

  ((kbd "z") "rat-mode-frac 1 3")
  ((kbd "x") "rat-mode-frac 2 3")
  ((kbd "c") "rat-mode-frac 3 3")
  ((kbd "v") "rat-mode-frac 4 3")
  ((kbd "b") "rat-mode-frac 5 3")

  ((kbd "-") "rat-mode-half")
  ((kbd "+") "rat-mode-double")
  ((kbd "<") "rat-mode-pop-fracs")

  ((kbd "h") "rat-mode-move -2 0")
  ((kbd "j") "rat-mode-move 0 2")
  ((kbd "k") "rat-mode-move 0 -2")
  ((kbd "l") "rat-mode-move 2 0")

  ((kbd "H") "rat-mode-move -4 0")
  ((kbd "J") "rat-mode-move 0 4")
  ((kbd "K") "rat-mode-move 0 -4")
  ((kbd "L") "rat-mode-move 4 0")

  ((kbd "C-h") "rat-mode-move -1 0")
  ((kbd "C-j") "rat-mode-move 0 1")
  ((kbd "C-k") "rat-mode-move 0 -1")
  ((kbd "C-l") "rat-mode-move 1 0")

  ((kbd "m") "ratclick 1")
  ((kbd ",") "ratclick 2")
  ((kbd ".") "ratclick 3")
  ((kbd "p") "ratclick 4")
  ((kbd "ntilde") "ratclick 5"))

;; Layouts
(define-interactive-keymap layout-mode ()
  ((kbd "k") "resize 0 10")
  ((kbd "j") "resize 0 -10")
  ((kbd "h") "resize -10 0")
  ((kbd "l") "resize 10 0")

  ((kbd "V") "vsplit")
  ((kbd "H") "hsplit")
  ((kbd "R") "remove")

  ((kbd "M-w") "resize-width")
  ((kbd "M-h") "resize-height")

  ((kbd "3") "layout-3"))

;;; Keys
(define-key *top-map* (kbd "s-,") "rat-choose-region")
(define-key *top-map* (kbd "s-.") "rat-mode")
(define-key *top-map* (kbd "s-a") "layout-mode")
;; (load (merge-pathnames "bindings.lisp" *load-truename*))
(define-key *top-map* (kbd "s-ESC") "xscreensaver-command -lock")
(define-key *top-map* (kbd "s-x") "colon")
(define-key *top-map* (kbd "s-:") "eval")
(define-key *top-map* (kbd "s-!") "exec")
(define-key *top-map* (kbd "C-s-m") "lastmsg")
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
(define-key *top-map* (kbd "s-s") "fselect")

(define-key *top-map* (kbd "s-f") "fullscreen-gaps")
(define-key *top-map* (kbd "s-F") "fullscreen")
(define-key *top-map* (kbd "s-+") "balance-frames")
(define-key *top-map* (kbd "s--") "only")
(define-key *top-map* (kbd "s-w") "windowlist")
(define-key *top-map* (kbd "s-z") "hide")
(define-key *top-map* (kbd "s-Z") "fclear")
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

(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-L") "move-window right")
(define-key *top-map* (kbd "C-s-k") "resize 0 10")
(define-key *top-map* (kbd "C-s-j") "resize 0 -10")
(define-key *top-map* (kbd "C-s-h") "resize -10 0")
(define-key *top-map* (kbd "C-s-l") "resize 10 0")
(define-key *top-map* (kbd "M-s-k") "rat-mode-move 0 -1")
(define-key *top-map* (kbd "M-s-j") "rat-mode-move 0 1")
(define-key *top-map* (kbd "M-s-h") "rat-mode-move -1 0")
(define-key *top-map* (kbd "M-s-l") "rat-mode-move 1 0")

(define-key *top-map* (kbd "M-s--") "rat-mode-half")
(define-key *top-map* (kbd "M-s-+") "rat-mode-double")

(define-key *top-map* (kbd "M-s-m") "ratclick 1")
(define-key *top-map* (kbd "M-s-,") "ratclick 2")
(define-key *top-map* (kbd "M-s-.") "ratclick 3")
(define-key *top-map* (kbd "M-s-p") "ratclick 4")
(define-key *top-map* (kbd "M-s-ntilde") "ratclick 5")

(define-key *top-map* (kbd "s-m") "mark")
(define-key *top-map* (kbd "s-C") "gnew")

(define-key *root-map* (kbd "s-r") "refresh-heads")
(define-key *root-map* (kbd "w") "pull-from-windowlist")

(define-key *root-map* (kbd "ESC") "abort")
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
(define-key *root-map* (kbd "m") "mode-line")

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

(defparameter *modes-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "r") "rat-mode")
    (define-key m (kbd "l") "layout-mode")
    m))
(define-key *top-map* (kbd "s-c") *modes-map*)

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
;; [ Wallpapers
;; require `feh`
(load-module "wallpapers")
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
(setf *primary-screen* (current-screen)
      *primary-head* (current-head))
(toggle-mode-line *primary-screen*
                  *primary-head*)
(refresh-heads)
;; [ Wallpapers
(wallpapers::multiple-wallpapers 0 (* 5 60))
;; ]
;; [ Stumpwm Tray
(stumptray::stumptray)
;; ]
;; [ Gaps between windows
(swm-gaps::toggle-gaps)
;; ]

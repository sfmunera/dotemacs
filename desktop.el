;;; EXWM
(defun sm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun sm/exwm-update-title ()
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ("Chromium-browser" (exwm-workspace-rename-buffer (format "Chromium: %s" exwm-title)))))

(defun sm/configure-window-by-class ()
  (interactive)
  (message "Window '%s' appeared!" exwm-class-name)
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-move-window 2))
    ;; Use the exwm-floating-toggle-floating command (C-c C-t C-f)
    ("mpv" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))))

(defun sm/update-displays ()
  (sm/run-in-background "autorandr --change --force")
  (sm/set-wallpaper)
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(defun sm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun sm/set-wallpaper ()
  (interactive)
  (start-process-shell-command "feh" nil "feh --bg-scale /usr/share/backgrounds/System76-Robot-by_Kate_Hazen_of_System76.png"))

;; Configure polybar
;; # Install dependencies on Ubuntu 20.04
;; sudo apt update
;; sudo apt install build-essential git cmake cmake-data pkg-config \
;;       python3-sphinx libcairo2-dev libxcb1-dev libxcb-util0-dev \
;;       libxcb-randr0-dev libxcb-composite0-dev python3-xcbgen xcb-proto \
;;       libxcb-image0-dev libxcb-ewmh-dev libxcb-icccm4-dev

;; # Clone the repo and compile version
;; git clone --recursive https://github.com/polybar/polybar
;; cd polybar
;; git checkout 3.6.3
;; ./build.sh

;;  Also install some icon fonts:

;; sudo apt install fonts-font-awesome fonts-material-design-icons-iconfont

;; polybar config goes in ~/.config/polybar/config.ini

(defvar sm/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun sm/kill-panel ()
  (interactive)
  (when sm/polybar-process
    (ignore-errors
      (kill-process sm/polybar-process)))
  (setq sm/polybar-process nil))

(defun sm/start-panel ()
  (interactive)
  (sm/kill-panel)
  (setq sm/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun sm/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

(defun sm/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun sm/send-polybar-exwm-workspace ()
  (sm/send-polybar-hook "exwm-workspace" 1))

; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'sm/send-polybar-exwm-workspace)

(defun sm/exwm-init-hook ()
  ;; Make workspace 1 the one where we land on startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;; (eshell)
  ;; Show battery status in mode line
  ;; (display-battery-mode 1)

  ;; Show time and date in mode line
  ;; (setq display-time-day-and-date t)
  ;; (display-time-mode 1)
  
  ;; Start the Polybar panel
  (sm/start-panel)

  ;;Launch apps that will run in the background
  ;; sudo apt install blueman pavucontrol pasystray nm-applet
  (sm/run-in-background "dunst")
  (sm/run-in-background "nm-applet")
  (sm/run-in-background "pasystray")
  (sm/run-in-background "blueman-applet"))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'sm/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'sm/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'sm/configure-window-by-class)

  ;; Extra configuration When EXWM starts up
  (add-hook 'exwm-init-hook #'sm/exwm-init-hook)

  ;; (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")
  ;; This screwed up my keyboard configuration. To fix it, ran setxkbmap -option in the terminal

  ;; Automatically move EXWM buffer to current workspace when selected
  (setq exwm-layout-show-all-buffers t)

  ;; Display all EXWM buffers in every workspace buffer list
  (setq exwm-workspace-show-all-buffers t)

  ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
  ;; (setq exwm-workspace-minibuffer-position 'bottom)

  ;; Set the screen resolution
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --mode 1920x1080 --pos 0x1080 --rotate normal --output DP-1 --off --output HDMI-1 --off --output HDMI-2 --primary --mode 3840x2160 --pos 1920x0 --rotate normal")

  (setq exwm-randr-workspace-monitor-plist '(4 "eDP-1"))

  ;; sudo apt install autorandr
  ;; autorandr --save mobile
  ;; autorandr --save mobile
  ;; React to display connectivity changes, do initial display update
  (add-hook 'exwm-randr-screen-change-hook #'sm/update-displays)
  (sm/update-displays)
  
  ;; Set background
  ;; sudo apt install compton
  ;; compton &
  ;; sudo apt install feh
  ;; feh --bg-scale /usr/share/backgrounds/System76-Robot-by_Kate_Hazen_of_System76.png
  (sm/set-wallpaper)
  
  ;; Load system tray
  ;; (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 32)
  ;; (exwm-systemtray-enable)

  (setq exwm-workspace-warp-cursor t)
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  
  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "> ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive)
                       (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                     (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

  (exwm-enable))

;; sudo apt install scrot brightnessctl playerctl
(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

;; Locking the screen
;; sudo apt install slock xss-lock

;; Notifications with Dunst
;; sudo apt install dunst

;; Dunst config goes in ~/.config/dunst/dunstrc
(defun sm/dunstctl (command)
  (start-process-shell-command "dunstctl" nil (concat "dunstctl " command)))

(exwm-input-set-key (kbd "s-n") (lambda () (interactive) (sm/dunstctl "history-pop")))
(exwm-input-set-key (kbd "s-N") (lambda () (interactive) (sm/dunstctl "close-all")))

(defun sm/disable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun sm/enable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(defun sm/toggle-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))


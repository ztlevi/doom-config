;;; ~/.doom.d/+os.el -*- lexical-binding: t; -*-

;; /////////////////////////// PATH ///////////////////////////
;; Load path from zsh login shell
(let* ((zshpath (shell-command-to-string
                 "/usr/bin/env zsh -lc 'printenv PATH'"))
       (pathlst (split-string zshpath ":")))
  (setq exec-path pathlst)
  (setq eshell-path-env zshpath)
  (setenv "PATH" zshpath))

;; ////////////////////////// MACOS ////////////////////////////
(defconst IS-EMACS-PLUS (file-exists-p "/usr/local/opt/emacs-plus"))
(defconst IS-EMACS-MAC (file-exists-p "/usr/local/opt/emacs-mac"))

(when IS-MAC
  (defvar mac-apps '("Clion" "IntelliJ IDEA" "Visual Studio Code")
    "MacOS applications collection used for `+macos!open-with' method")
  (defun ivy--read-apps ()
    (ivy-read "Select Applications:" mac-apps))

  (when (file-directory-p "/Applications/ForkLift.app")
    (+macos!open-with reveal-in-finder "forklift" default-directory)
    (+macos!open-with reveal-project-in-finder "forklift"
                      (or (doom-project-root) default-directory)))

  (+macos!open-with reveal-in-apps (ivy--read-apps) default-directory)
  (+macos!open-with reveal-project-in-apps (ivy--read-apps)
                    (or (doom-project-root) default-directory))

  (+macos!open-with reveal-in-terminal "iterm" default-directory)
  (+macos!open-with reveal-project-in-terminal "iterm"
                    (or (doom-project-root) default-directory))

  (+macos!open-with reveal-in-typora "Typora" buffer-file-name))

;; ////////////////////////// LINUX /////////////////////////////
(when IS-LINUX
  ;; Add executable: Clion -> Tools -> Create Command Line Launcher
  (defvar linux-apps '("clion" "intellij idea" "code")
    "Linux applications collection used for `+shell!open-with' method")
  (defun ivy--read-apps ()
    (ivy-read "Select Applications:" linux-apps))

  (defvar linux-terminal (if (file-exists-p "/usr/bin/konsole")
                             "/usr/bin/konsole"
                           "/usr/bin/gnome-terminal"))

  (defvar linux-finder (if (file-exists-p "/usr/bin/xdg-open")
                           "/usr/bin/xdg-open"
                         "/usr/bin/gvfs-open"))

  (+shell!open-with open-in-default-program linux-finder)

  (+shell!open-with reveal-in-finder linux-finder default-directory)
  (+shell!open-with reveal-project-in-finder linux-finder
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-apps (ivy--read-apps) default-directory)
  (+shell!open-with reveal-project-in-apps (ivy--read-apps)
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-terminal linux-terminal default-directory)
  (+shell!open-with reveal-project-in-terminal linux-terminal
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-typora "typora" buffer-file-name))

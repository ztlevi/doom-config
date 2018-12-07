;;; ~/.doom.d/+os.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load path from zsh login shell
(let* ((zshpath (shell-command-to-string
                 "/usr/bin/env zsh -lc 'printenv PATH'"))
       (pathlst (split-string zshpath ":")))
  (setq exec-path pathlst)
  (setq eshell-path-env zshpath)
  (setenv "PATH" zshpath))

;; use zsh as default shell
(setenv "SHELL" "zsh")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LINUX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when IS-LINUX
  ;; Add executable: Clion -> Tools -> Create Command Line Launcher
  (defvar linux-apps '("clion" "intellij idea" "code")
    "Linux applications collection used for `+shell!open-with' method")
  (defun ivy--read-apps ()
    (ivy-read "Select Applications:" linux-apps))

  (defvar linux-terminal (cond ((executable-find "terminator") "terminator")
                               ((executable-find "konsole") "konsole")
                               ((executable-find "gnome-terminal") "gnome-terminal")))

  (defun linux-terminal-args (dir)
    (cond ((executable-find "terminator") (list "--working-directory" dir))
          ((executable-find "konsole") (list "--workdir" dir))
          ((executable-find "gnome-terminal") (list "--working-directory" dir))))

  (defvar linux-finder (cond ((executable-find "xdg-open") "xdg-open")
                             ((executable-find "gvfs-open") "gvfs-open")))

  (+shell!open-with open-in-default-program linux-finder)

  (+shell!open-with reveal-in-finder linux-finder default-directory)
  (+shell!open-with reveal-project-in-finder linux-finder
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-apps (ivy--read-apps) default-directory)
  (+shell!open-with reveal-project-in-apps (ivy--read-apps)
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-terminal linux-terminal nil (linux-terminal-args default-directory))
  (+shell!open-with reveal-project-in-terminal linux-terminal nil
                    (linux-terminal-args (or (doom-project-root) default-directory)))

  (+shell!open-with reveal-in-typora "typora" buffer-file-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete to trash
(setq delete-by-moving-to-trash t)

;; using trash over rm
(when (executable-find "trash")
  (os--trash-setup))

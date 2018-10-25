;;; ~/.doom.d/+os.el -*- lexical-binding: t; -*-

;; ////////////////////////// MACOS ////////////////////////////
(defconst IS-EMACS-PLUS (file-exists-p "/usr/local/opt/emacs-plus"))
(defconst IS-EMACS-MAC (file-exists-p "/usr/local/opt/emacs-mac"))

(when IS-MAC
  (when (file-directory-p "/Applications/ForkLift.app")
    (+macos!open-with reveal-in-finder "forklift" default-directory)
    (+macos!open-with reveal-project-in-finder "forklift"
                      (or (doom-project-root) default-directory)))

  ;; Add executable: Clion -> Tools -> Create Command Line Launcher
  (+macos!open-with reveal-project-in-clion"clion"
                    (or (doom-project-root) default-directory))

  (+macos!open-with reveal-in-vscode "Visual Studio Code" default-directory)
  (+macos!open-with reveal-project-in-vscode "Visual Studio Code"
                    (or (doom-project-root) default-directory))

  (+macos!open-with reveal-in-terminal "iterm" default-directory)
  (+macos!open-with reveal-project-in-terminal "iterm"
                    (or (doom-project-root) default-directory))

  (+macos!open-with reveal-in-typora "Typora" buffer-file-name))

;; ////////////////////////// LINUX /////////////////////////////
(when IS-LINUX
  ;; Load path from zsh login shell
  (let* ((zshpath (shell-command-to-string
                   "/usr/bin/env zsh -lc 'printenv PATH'"))
         (pathlst (split-string zshpath ":")))
    (setq exec-path pathlst)
    (setq eshell-path-env zshpath)
    (setenv "PATH" zshpath))

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

  (+shell!open-with reveal-project-in-clion "clion"
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-vscode "code" default-directory)
  (+shell!open-with reveal-project-in-vscode "code"
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-terminal linux-terminal default-directory)
  (+shell!open-with reveal-project-in-terminal linux-terminal
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-typora "typora" buffer-file-name))

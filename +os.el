;;; ~/.doom.d/+os.el -*- lexical-binding: t; -*-

;; ////////////////////////// MACOS ////////////////////////////
(defconst IS-EMACS-PLUS (file-exists-p "/usr/local/opt/emacs-plus"))
(defconst IS-EMACS-MAC (file-exists-p "/usr/local/opt/emacs-mac"))

(if IS-MAC
    (progn
      (if (file-directory-p "/Applications/ForkLift.app")
          (progn
            (+macos!open-with reveal-in-finder "forklift" default-directory)
            (+macos!open-with reveal-project-in-finder "forklift"
                              (or (doom-project-root) default-directory))))

      (+macos!open-with reveal-in-vscode "Visual Studio Code" default-directory)
      (+macos!open-with reveal-project-in-vscode "Visual Studio Code"
                        (or (doom-project-root) default-directory))

      (+macos!open-with reveal-in-iterm "iterm" default-directory)
      (+macos!open-with reveal-project-in-iterm "iterm"
                        (or (doom-project-root) default-directory))

      (+macos!open-with reveal-in-typora "Typora" buffer-file-name)))

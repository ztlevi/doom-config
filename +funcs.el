;;; ~/.doom.d/+funcs.el -*- lexical-binding: t; -*-

(defconst IS-EMACS-PLUS (file-exists-p "/usr/local/opt/emacs-plus"))
(defconst IS-EMACS-MAC (file-exists-p "/usr/local/opt/emacs-mac"))

;; ////////////////////////// BUFFER ///////////////////////////
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (save-buffer)
  (revert-buffer :ignore-auto :noconfirm))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

;; ////////////////////////// MACOS ////////////////////////////
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

(+macos!open-with reveal-in-typora "Typora" buffer-file-name)

;; ////////////////////// UTILIES ////////////////////////////
(defun zt/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun zt/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;;add count for chinese, mainly used for writing chinese blog post
;; http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun zt/word-count-for-chinese ()
  "「較精確地」統計中/日/英文字數。
- 文章中的註解不算在字數內。
- 平假名與片假名亦包含在「中日文字數」內，每個平/片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。

※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ; 去掉 org 文件的 OPTIONS（以#+開頭）
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                        ; 把註解行刪掉（不把註解算進字數）。
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字數（不含標點）：%s
中日文字數（包含標點）：%s
英文字數（不含標點）：%s
=======================
中英文合計（不含標點）：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))

(defun zt/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (replace-regexp-in-string "/" "\\/" (regexp-quote (buffer-substring-no-properties beg end)) t t)))
      (setq command-string (format "1,$s /%s/%s/g" selection selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

;; "http://xuchunyang.me/Opening-iTerm-From-an-Emacs-Buffer/"
(defun zt/iterm-shell-command (command &optional prefix)
  "cd to `default-directory' then run COMMAND in iTerm.
With PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (zt/git-project-root)
                default-directory))
         ;; if COMMAND is empty, just change directory
         (cmd (format "cd %s ;%s" dir command)))
    (do-applescript
     (format
      "
  tell application \"iTerm2\"
       activate
       set _session to current session of current window
       tell _session
            set command to get the clipboard
            write text \"%s\"
       end tell
  end tell
  " cmd))))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

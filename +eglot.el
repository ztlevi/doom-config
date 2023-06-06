;;; +eglot.el -*- lexical-binding: t; -*-

(add-hook 'java-mode-local-vars-hook #'lsp! 'append)

(use-package! breadcrumb
  :defer t
  :when (modulep! :tools lsp +eglot)
  :hook
  (prog-mode . breadcrumb-local-mode)
  (text-mode . breadcrumb-local-mode)
  )

(after! eglot
  ;; set-eglot-client set the eglot-server-programs variable
  ;; CC
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))

  ;; JS & TS
  ;; https://www.reddit.com/r/emacs/comments/11bqzvk/emacs29_and_eglot_inlay_hints/
  ;; https://github.com/microsoft/TypeScript/blob/main/src/server/protocol.ts#L3410-L3539
  (set-eglot-client! '(typescript-mode js-mode js-ts-mode tsx-ts-mode typescript-ts-mode)
                     '("typescript-language-server" "--stdio" :initializationOptions
                       (:preferences (:importModuleSpecifierPreference "relative"
                                      :allowRenameOfImportPath t
                                      ))))

  ;; Java
  ;; https://w.amazon.com/bin/view/Bemol/#HeglotpluginforEmacs
  (set-eglot-client! '(java-mode java-ts-mode)
                     '("jdtls" ;; This should be the script to run jdtls. You can create it yourself, or use the script provided by jdt (requires python 3.9.)
                       ;; Alternatively, you can paste the command here (e.g. "java" "-Declipse.application=org.eclipse.jdt.ls.core.id1" ...)
                       ;; See https://github.com/eclipse/eclipse.jdt.ls for more info.
                       :initializationOptions (:extendedClientCapabilities (:classFileContentsSupport t ))))

  ;; The jdt server sometimes returns jdt:// scheme for jumping to definition
  ;; instead of returning a file. This is not part of LSP and eglot does not
  ;; handle it. The following code enables eglot to handle jdt files.
  ;; See https://github.com/yveszoundi/eglot-java/issues/6 for more info.
  (defun jdt-file-name-handler (operation &rest args)
    "Support Eclipse jdtls `jdt://' uri scheme."
    (let* ((uri (car args))
           (cache-dir "/tmp/.eglot")
           (source-file
            (expand-file-name
             (file-name-concat
              cache-dir
              (save-match-data
                (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri))
                (message "URI:%s" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (message "content:%s" content)
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file)

  (add-to-list 'file-name-handler-alist '("\\`jdt://" . jdt-file-name-handler))

  (defun jdthandler--wrap-legacy-eglot--path-to-uri (original-fn &rest args)
    "Hack until eglot is updated.
ARGS is a list with one element, a file path or potentially a URI.
If path is a jar URI, don't parse. If it is not a jar call ORIGINAL-FN."
    (let ((path (file-truename (car args))))
      (if (equal "jdt" (url-type (url-generic-parse-url path)))
          path
        (apply original-fn args))))

  (defun jdthandler--wrap-legacy-eglot--uri-to-path (original-fn &rest args)
    "Hack until eglot is updated.
ARGS is a list with one element, a URI.
If URI is a jar URI, don't parse and let the `jdthandler--file-name-handler'
handle it. If it is not a jar call ORIGINAL-FN."
    (let ((uri (car args)))
      (if (and (stringp uri)
               (string= "jdt" (url-type (url-generic-parse-url uri))))
          uri
        (apply original-fn args))))


  (defun jdthandler-patch-eglot ()
    "Patch old versions of Eglot to work with Jdthandler."
    (interactive) ;; TODO, remove when eglot is updated in melpa
    (unless (or (and (advice-member-p #'jdthandler--wrap-legacy-eglot--path-to-uri 'eglot--path-to-uri)
                     (advice-member-p #'jdthandler--wrap-legacy-eglot--uri-to-path 'eglot--uri-to-path))
                (<= 29 emacs-major-version))
      (advice-add 'eglot--path-to-uri :around #'jdthandler--wrap-legacy-eglot--path-to-uri)
      (advice-add 'eglot--uri-to-path :around #'jdthandler--wrap-legacy-eglot--uri-to-path)
      (message "[jdthandler] Eglot successfully patched.")))

  ;; invoke
  (jdthandler-patch-eglot)
  )

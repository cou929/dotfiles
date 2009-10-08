;; .emacs

;; add ~/elisp/ to load-path
(setq load-path
      (append
       (list
    (expand-file-name "~/.emacs.d/elisp")
    )
       load-path))

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; call compile command from key shortcat "c-c c".
(define-key mode-specific-map "c" 'compile)

;;bind ctrl-h to BackSpace
(global-set-key "\C-h" 'delete-backward-char)

;; flymake
(require 'flymake)
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" local-file))))

(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook
          '(lambda ()
             (flymake-mode t)))

;; highlight ()
(show-paren-mode 1)

;; hide menu bar
(menu-bar-mode -1)

;; bind help command to C-x C-h
(define-key global-map "\C-x\C-h" 'help-command)

;; bind dabbrev-expand (auto complete) to C-o
(global-set-key "\C-o" 'dabbrev-expand)
(setq dabbrev-case-fold-search nil)

;; php mode
(require 'php-mode)
(add-hook 'php-mode-hook
      '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;; setting of scheme mode using gauche environment
(setq scheme-program-name "gosh")
(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)

;; for mac
(when (eq window-system 'mac)
  (add-hook 'window-setup-hook
            (lambda ()
;;              (setq mac-autohide-menubar-on-maximize t)
              (set-frame-parameter nil 'fullscreen 'fullboth)
              )))

(defun mac-toggle-max-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

;; Carbon Emacs, hide menu, etc
(custom-set-variables
 '(display-time-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
 )

;; Color
(if window-system (progn
   (set-background-color "Black")
   (set-foreground-color "LightGray")
   (set-cursor-color "Gray")
   (set-frame-parameter nil 'alpha 80)
   ))

;; install-elisp
;; http://www.emacswiki.org/cgi-bin/wiki/download/install-elisp.el
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

;; auto-save-buffers.el 
(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers)

;; simple-hatena-mode.el
(require 'simple-hatena-mode)
(setq simple-hatena-bin "~/projects/pl/hatenaDiaryWriter/hw.pl")
(setq simple-hatena-use-timestamp-permalink-flag nil)
(setq simple-hatena-time-offset 6)

;; in c mode, use flyspell only for comemnt and string
(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; enable flyspell-prog-mode
             (flyspell-prog-mode)
))

;; global (gtags) setting
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))
(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))

;; gdb-many-windows
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t) ; If you don't need "IO buffer", set it to nil.

;; anything.el
;; bind anything to C-x b
(require 'anything)
(require 'anything-config)
;(add-to-list 'anything-sources 'anything-c-source-emacs-commands)
(setq anything-sources
      '(anything-c-source-buffers+
	anything-c-source-colors
	anything-c-source-recentf
	anything-c-source-man-pages
	anything-c-source-emacs-commands
	anything-c-source-emacs-functions
	anything-c-source-files-in-current-dir
	))
(define-key global-map (kbd "C-x b") 'anything)

;; compilation window size
(setq compilation-window-height 8)

;; close compilation window if it ends with no problem
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "compilation errors, press C-x ` to visit")
          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))


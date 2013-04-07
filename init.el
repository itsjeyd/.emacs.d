(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;
;;; Paths ;;;
;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/")


;;;;;;;;;;;;;;;
;;; Buffers ;;;
;;;;;;;;;;;;;;;

; Key Bindings
(global-set-key (kbd "M-s r b") 'revert-buffer)

; Variables
(setq confirm-nonexistent-file-or-buffer nil)
(setq revert-without-query (quote (".*")))


;;;;;;;;;;;;;;;;;;;;
;;; Byte-Compile ;;;
;;;;;;;;;;;;;;;;;;;;
(defun auto-recompile-emacs-file ()
  (interactive)
  (when (and buffer-file-name (string-match "\\.el" buffer-file-name))
    (let ((byte-file (concat buffer-file-name "\\.elc")))
      (if (or (not (file-exists-p byte-file))
              (file-newer-than-file-p buffer-file-name byte-file))
          (byte-compile-file buffer-file-name)))))

(add-hook 'after-save-hook 'auto-recompile-emacs-file)


;;;;;;;;;;;;;
;;; Dired ;;;
;;;;;;;;;;;;;

; Hidden Files
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

; Movement (source: whattheemacsd.com)
(defun dired-jump-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-jump-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

; Variables
(setq dired-recursive-copies (quote always))


;;;;;;;;;;;;;;;
;;; Editing ;;;
;;;;;;;;;;;;;;;

; Ace Jump
(global-set-key (kbd "s-SPC") 'ace-jump-mode)

; Functions
(put 'narrow-to-region 'disabled nil)

(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR. CHAR
is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Key Bindings
(global-set-key (kbd "M-g c") 'goto-char)
(global-unset-key (kbd "M-g g"))
(global-unset-key (kbd "M-g M-g"))
(global-set-key (kbd "M-g l") 'goto-line)
(global-set-key (kbd "M-s t t") 'toggle-truncate-lines)

; Variables
(setq sentence-end-double-space nil)
(setq set-mark-command-repeat-pop t)
(setq tab-width 4)


;;;;;;;;;;;;;
;;; Fixes ;;;
;;;;;;;;;;;;;

;; NOTE: The following fix becomes obsolete when using python-mode.el
;; instead of python.el!

;; (defun python-reinstate-current-directory ()
;;     "When running Python, add the current directory ('') to the
;;     head of sys.path. For security reasons, run-python passes
;;     arguments to the interpreter that explicitly remove '' from
;;     sys.path. This means that, for example, using
;;     `python-send-buffer' in a buffer visiting a module's code
;;     will fail to find other modules in the same directory.

;;     Adding this function to `inferior-python-mode-hook'
;;     reinstates the current directory in Python's search path."
;;     (python-send-string "sys.path[0:0] = ['']"))
;; (add-hook 'inferior-python-mode-hook 'python-reinstate-current-directory)


;;;;;;;;;;;;;
;;; Fonts ;;;
;;;;;;;;;;;;;
(set-face-attribute 'default nil :font "Monaco-10")


;;;;;;;;;;;
;;; Ido ;;;
;;;;;;;;;;;

(ido-mode (quote both))
(setq ido-use-virtual-buffers t)

; Key Bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; Ubiquitous
(add-to-list 'load-path "~/.emacs.d/elpa/ido-ubiquitous-1.6/")
(require 'ido-ubiquitous)
(ido-ubiquitous-mode)
(setq ido-ubiquitous-command-exceptions
      (quote (sclang-dump-interface sclang-dump-full-interface)))

; Variables
(setq ido-create-new-buffer (quote always))
(setq ido-enable-flex-matching t)


;;;;;;;;;;;;;;;;;
;;; Interface ;;;
;;;;;;;;;;;;;;;;;
(set-scroll-bar-mode nil)
(tool-bar-mode 0)
(tooltip-mode 0)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

; cl
(add-to-list 'load-path "~/.emacs.d/elpa/cl-lib-0.2/")
(require 'cl-lib)

; Eclim
(add-to-list 'load-path "~/.emacs.d/elpa/emacs-eclim-20130310.1237/")
(require 'eclim)
(global-eclim-mode)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

; Eclimd
(require 'eclimd)

; Company
(add-to-list 'load-path "~/.emacs.d/elpa/company-0.6/")
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)


;;;;;;;;;;;;;;;;;;
;;; Minibuffer ;;;
;;;;;;;;;;;;;;;;;;

; Prompts
(fset 'yes-or-no-p 'y-or-n-p)

; Variables
(savehist-mode t)
(setq history-delete-duplicates t)
(setq history-length t)


;;;;;;;;;;;;
;;; MISC ;;;
;;;;;;;;;;;;

; Line Numbering
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'lisp-mode-hook 'linum-mode)
(add-hook 'octave-mode-hook 'linum-mode)


;;;;;;;;;;;;;;;;
;;; Modeline ;;;
;;;;;;;;;;;;;;;;

; Unique Buffer Names
(require 'uniquify)
(setq uniquify-buffer-name-style (quote forward))

; Variables
(column-number-mode t)


;;;;;;;;;;;;;;;;
;;; Org Mode ;;;
;;;;;;;;;;;;;;;;

; Hooks
(add-hook 'org-mode-hook 'linum-mode)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

; Key Bindings
(global-set-key (kbd "C-c a") 'org-agenda)

; Variables
(setq org-agenda-files (quote
                        ("/storage/ORG/school.org"
                         "/storage/ORG/job.org"
                         "/storage/ORG/life.org"
                         "/storage/ORG/read.org")))
(setq org-agenda-include-diary t)
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-use-speed-commands t)


;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Manager ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defadvice package-compute-transaction
  (before package-compute-transaction-reverse
          (package-list requirements) activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

;;;;;;;;;;;;;;;;;;;
;;; Permissions ;;;
;;;;;;;;;;;;;;;;;;;
(require 'tramp)
; Usage: C-x C-f /sudo::/path/to/file


;;;;;;;;;;;;;;;;;;;
;;; Programming ;;;
;;;;;;;;;;;;;;;;;;;

; Auto-complete
(add-to-list 'load-path "~/.emacs.d/elpa/popup-0.5/")
(require 'popup)
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-1.4/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

; Indentation
(setq-default indent-tabs-mode nil)

; Parens
(show-paren-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Project Managment ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(filesets-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Django
(add-to-list 'load-path "~/.emacs.d/pony-mode/src")
(require 'pony-mode)

; Flymake
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'python-mode-hook
          (lambda ()
            (unless (eq buffer-file-name nil) (flymake-mode t))))

; Key Bindings
(require 'python)
(define-key python-mode-map (kbd "M-s f n") 'flymake-goto-next-error)
(define-key python-mode-map (kbd "M-s f p") 'flymake-goto-prev-error)
(define-key python-mode-map (kbd "M-s f d") 'flymake-display-err-menu-for-current-line)

; Subword Mode
(add-hook 'python-mode-hook (lambda () (subword-mode 1)))


;;;;;;;;;;;;;;;
;;; Recentf ;;;
;;;;;;;;;;;;;;;
(require 'recentf)
(recentf-mode t)

; Functions
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

; Key Bindings
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
; NOTE: C-x C-r is bound to `find-file-read-only' by default

; Variables
(setq recentf-max-saved-items 50)


;;;;;;;;;;;;;;;;;
;;; Scrolling ;;;
;;;;;;;;;;;;;;;;;

; Functions
(put 'scroll-left 'disabled nil)


;;;;;;;;;;;;;;
;;; Server ;;;
;;;;;;;;;;;;;;
(server-start)


;;;;;;;;;;;;;;;;;
;;; Utilities ;;;
;;;;;;;;;;;;;;;;;

; Functions
(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(defun startpage ()
  "Startpages a query or region if any."
  (interactive)
  (browse-url
   (concat
    "https://startingpage.com/do/metasearch.pl?query="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "StartPage: ")))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control ;;;
;;;;;;;;;;;;;;;;;;;;;;;

; Packages
(global-set-key (kbd "M-s g s") 'magit-status)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows + Frames ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

; Macros
(fset 'kill-other-buffer-close-window
   (lambda (&optional arg) "Keyboard macro. Kills the next buffer
   in line and closes the associated window. I.e., if there are
   two windows, the active one stays intact, the inactive one is
   closed. If there are several windows, the one that would be
   reached by issuing C-x o once is closed, all others stay
   intact. Should only be used if the frame is displaying more
   than one window." (interactive "p") (kmacro-exec-ring-item (quote ([24
   111 24 107 return 24 48] 0 "%d")) arg)))
(global-set-key (kbd "C-S-o k") 'kill-other-buffer-close-window)

(fset 'ver-to-hor-split
   (lambda (&optional arg) "Keyboard macro. Go from a horizontal
   bar splitting two windows to a vertical one, preserving the
   buffers shown in the two windows" (interactive "p") (kmacro-exec-ring-item (quote ([24
   98 18 19 return 24 98 return 24 49 24 51 24 111 24 98 return
   24 111] 0 "%d")) arg)))
(global-set-key (kbd "C-x C-3") 'ver-to-hor-split)

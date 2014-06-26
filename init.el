(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
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
(add-to-list 'load-path "~/.emacs.d/mark-lines/")


;;;;;;;;;;;;;;;
;;; Backups ;;;
;;;;;;;;;;;;;;;

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


;;;;;;;;;;;;;;;
;;; Buffers ;;;
;;;;;;;;;;;;;;;

; Functions
(setq temp-buffer-count 0)

(defun make-temp-buffer ()
  (interactive)
  (let ((temp-buffer-name (format "*tempbuf-%d*" temp-buffer-count)))
    (switch-to-buffer temp-buffer-name)
    (message "New temp buffer (%s) created." temp-buffer-name))
  (setq temp-buffer-count (1+ temp-buffer-count)))

; Key Bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-s r b") 'revert-buffer)
(global-set-key (kbd "M-s t b") 'make-temp-buffer)

; Variables
(setq confirm-nonexistent-file-or-buffer nil)
(setq revert-without-query (quote (".*")))


;;;;;;;;;;;;;;;;;;;;
;;; Byte-Compile ;;;
;;;;;;;;;;;;;;;;;;;;

(defun auto-recompile-elisp-file ()
  (interactive)
  (when (and buffer-file-name (string-match "\\.el" buffer-file-name))
    (let ((byte-file (concat buffer-file-name "\\.elc")))
      (if (or (not (file-exists-p byte-file))
              (file-newer-than-file-p buffer-file-name byte-file))
          (byte-compile-file buffer-file-name)))))

(add-hook 'after-save-hook 'auto-recompile-elisp-file)


;;;;;;;;;;;;;;;;;;;
;;; Common Lisp ;;;
;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)


;;;;;;;;;;;;;;;;;;;;
;;; Custom Stuff ;;;
;;;;;;;;;;;;;;;;;;;;

(load "wenote")


;;;;;;;;;;;;;
;;; Dired ;;;
;;;;;;;;;;;;;

; File Associations
(require 'openwith)
(openwith-mode t)
(setq openwith-associations
      (quote (("\\.\\(?:pdf\\|ps\\)\\'" "okular" (file))
              ("\\.\\(?:mp3\\|wav\\|flac\\)\\'" "gmusicbrowser" (file))
              ("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\|mov\\|mp4\\)\\'" "vlc" (file))
              ("\\.\\(?:jpe?g\\|png\\|bmp\\)\\'" "gwenview" (file))
              ("\\.chm\\'" "kchmviewer" (file))
              ("\\.\\(?:odt\\|doc\\|docx\\)\\'" "libreoffice" ("--writer" file))
              ("\\.\\(?:ods\\|xls\\|xlsx\\)\\'" "libreoffice" ("--calc" file))
              ("\\.\\(?:odp\\|pps\\|ppt\\|pptx\\)\\'" "libreoffice" ("--impress" file))
              ("\\.dia\\'" "dia" (file)))))

; Hidden Files
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

; Layout (source: whattheemacsd.com)
(require 'dired-details)
(setq-default dired-details-hidden-string "> ")
(dired-details-install)

; Movement (source: whattheemacsd.com)
(defun dired-jump-to-top ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-jump-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

; Variables
(setq dired-isearch-filenames "dwim")
(setq dired-recursive-copies (quote always))


;;;;;;;;;;;;;;;
;;; Editing ;;;
;;;;;;;;;;;;;;;

; Browse Kill Ring
(require 'browse-kill-ring)
(global-set-key (kbd "M-s b k") 'browse-kill-ring)

; Functions
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice zap-to-char (after zap-to-char-keep-char (arg char) activate)
  "Kill up to but not including the ARG'th occurence of CHAR.
Put point before CHAR."
  (insert char)
  (forward-char -1))

; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Key Bindings
(global-set-key (kbd "RET") 'newline-and-indent)

; Mark Lines
(require 'mark-lines)
(global-set-key (kbd "M-s m") 'mark-lines-next-line)

; Move Text
(global-set-key (kbd "M-s u") 'move-text-up)
(global-set-key (kbd "M-s d") 'move-text-down)

; Parens
(require 'smartparens-config)
(smartparens-global-mode t)

(global-rainbow-delimiters-mode t)

; Variables
(setq cua-enable-cua-keys nil)
(setq save-interprogram-paste-before-kill t)
(setq sentence-end-double-space nil)
(setq set-mark-command-repeat-pop t)
(setq tab-width 4)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; Hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

; Key Bindings
(define-key emacs-lisp-mode-map (kbd "M-s e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "M-s e r") 'eval-region)


;;;;;;;;;;;;;
;;; Fonts ;;;
;;;;;;;;;;;;;

(set-face-attribute 'default nil :font "Monaco-10")


;;;;;;;;;;;;
;;; Help ;;;
;;;;;;;;;;;;

(setq guide-key/guide-key-sequence '("C-c" "C-x r" "C-x v" "C-x 4"))
(guide-key-mode t)


;;;;;;;;;;;
;;; Ido ;;;
;;;;;;;;;;;

(ido-mode (quote both))

; Key Bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; Ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous-mode)
(setq ido-ubiquitous-command-exceptions
      (quote (sclang-dump-interface sclang-dump-full-interface)))

; Variables
(setq ido-create-new-buffer (quote always))
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)


;;;;;;;;;;;;;;;;;
;;; Interface ;;;
;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen t)

; Controls
(set-scroll-bar-mode nil)
(tool-bar-mode 0)

; Theme
(load-theme 'wombat t)
(set-cursor-color "#FF5A0E")

; Tooltips
(tooltip-mode 0)

; Writeroom
(defun turn-off-git-gutter ()
  (if (not git-gutter-mode)
      (git-gutter-mode t)
    (git-gutter-mode -1)))

(add-hook 'writeroom-mode-hook 'turn-off-git-gutter)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

; Eclim
(require 'eclim)
(global-eclim-mode)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(add-hook
 'eclim-mode-hook
 (lambda ()
   (remove-hook 'before-save-hook 'delete-trailing-whitespace)))

; Eclimd
(require 'eclimd)

; Company
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(add-hook 'eclim-mode-hook (lambda () (company-mode t)))

; Functions
(defun java-goto-class ()
  (interactive)
  (goto-char (point-min))
  (search-forward "class")
  (beginning-of-line)
  (recenter-top-bottom 0))

(defun java-class-to-top ()
  (if (and (eq major-mode 'java-mode)
           (looking-at "^public\\|private\\|protected\\|class"))
      (recenter-top-bottom 0)))

; Hooks
(add-hook 'java-mode-hook 'ensime-scala-mode-hook)
(add-hook 'java-mode-hook 'java-goto-class)
(add-hook 'window-configuration-change-hook 'java-class-to-top)

; Variables
(defun set-indentation-behavior ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'set-indentation-behavior)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JavaScript Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq js-indent-level 2)


;;;;;;;;;;;;;
;;; LaTeX ;;;
;;;;;;;;;;;;;

; AUCTeX
(require 'tex)
(setq-default TeX-master nil)

; BibTeX
(setq bibtex-maintain-sorted-entries t)

; Hooks
(add-hook 'LaTeX-mode-hook (lambda () (TeX-PDF-mode t)))


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

; Source:
; https://code.google.com/p/dea/source/browse/trunk/my-lisps/ahei-misc.el

(defun add-to-hooks (hooks function &optional append local)
  "Call `add-hook' on HOOKS using arguments FUNCTION, APPEND, and LOCAL.

HOOKS can be a list of hooks or just a single hook."
  (if (listp hooks)
      (mapc
       `(lambda (hook)
          (add-hook hook ',function append local))
       hooks)
    (add-hook hooks function append local)))


;;;;;;;;;;;;;;;;
;;; Modeline ;;;
;;;;;;;;;;;;;;;;

; Unique Buffer Names
(require 'uniquify)
(setq uniquify-buffer-name-style (quote forward))

; Variables
(column-number-mode t)


;;;;;;;;;;;;;
;;; Modes ;;;
;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("routes$" . conf-space-mode))


;;;;;;;;;;;;;;;;
;;; Movement ;;;
;;;;;;;;;;;;;;;;

; Ace Jump
(global-set-key (kbd "s-SPC") 'ace-jump-mode)

; Key Bindings
(global-set-key (kbd "M-g c") 'goto-char)
(global-set-key (kbd "M-g l") 'goto-line)


;;;;;;;;;;;;;;;;
;;; Org Mode ;;;
;;;;;;;;;;;;;;;;

(require 'org)

; Exports
(require 'ox-md)

; Babel
(require 'ob-plantuml)
(setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
(setq plantuml-jar-path "/opt/plantuml/plantuml.jar")

; Hooks
(add-hook 'org-mode-hook 'turn-on-auto-fill)

; Key Bindings
(define-key org-mode-map (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "M-n") 'org-next-item)
(define-key org-mode-map (kbd "M-p") 'org-previous-item)
(define-key org-mode-map (kbd "M-s t l") 'org-toggle-link-display)

; Variables
(setq org-agenda-include-diary t)
(setq org-enforce-todo-dependencies t)
(setq org-list-allow-alphabetical t)
(setq org-list-use-circular-motion t)
(setq org-special-ctrl-a/e t)
(setq org-track-ordered-property-with-tag t)
(setq org-use-speed-commands
      (lambda () (or (looking-at org-outline-regexp) (looking-at "^#\+"))))
(add-to-list 'org-structure-template-alist
             '("o" "#+BEGIN_COMMENT\n?\n#+END_COMMENT") t)


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
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

; Hide/Show
(defun configure-hs ()
  "Configure Hide/Show"
  (setq hs-isearch-open t)
  (define-key hs-minor-mode-map (kbd "M-s a h") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "M-s a s") 'hs-show-all)
  (define-key hs-minor-mode-map (kbd "M-s b h") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "M-s b s") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "M-s l h") 'hs-hide-level)
  (define-key hs-minor-mode-map (kbd "M-s t h") 'hs-toggle-hiding))

(defun turn-on-hs ()
  (hs-minor-mode 1)
  (configure-hs))

(add-to-hooks '(emacs-lisp-mode-hook
                haml-mode-hook
                html-mode-hook
                java-mode-hook
                js-mode-hook
                php-mode-hook
                python-mode-hook)
              'turn-on-hs)

; Indentation
(setq-default indent-tabs-mode nil)

; Parens
(show-paren-mode t)

; Subword Mode
(add-to-hooks `(java-mode-hook
                js-mode-hook
                php-mode-hook
                python-mode-hook)
              (lambda () (subword-mode 1)))

; yasnippet
(require 'yasnippet)
(yas-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project Management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setq recentf-max-saved-items 100)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scala Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; Ensime
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq ensime-ac-override-settings nil)


;;;;;;;;;;;;;;;;;
;;; Scrolling ;;;
;;;;;;;;;;;;;;;;;

; Functions
(put 'scroll-left 'disabled nil)

; Variables
(setq scroll-preserve-screen-position 1)


;;;;;;;;;;;;;;
;;; Search ;;;
;;;;;;;;;;;;;;

; Functions
(defun occur-rename-buffer-after-search-string ()
  "Uniquify name of *Occur* buffer by appending search string to it."
  (let* ((beg-end (match-data (string-match "\".+\"" (buffer-string))))
         (beg (+ (car beg-end) 2))
         (end (cadr beg-end))
         (search-string (buffer-substring-no-properties beg end)))
    (rename-buffer (format "*Occur-%s*" search-string))))

; Hooks
(add-hook 'occur-hook 'occur-rename-buffer-after-search-string)

; Key Bindings
(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)


; Smartscan
(global-smartscan-mode t)
(define-key smartscan-map (kbd "M-n") nil)
(define-key smartscan-map (kbd "M-p") nil)
(define-key smartscan-map (kbd "s-n") 'smartscan-symbol-go-forward)
(define-key smartscan-map (kbd "s-p") 'smartscan-symbol-go-backward)



;;;;;;;;;;;;;;
;;; Server ;;;
;;;;;;;;;;;;;;

(require 'server)
(or (server-running-p)
    (server-start))


;;;;;;;;;;;;;;;;;
;;; Utilities ;;;
;;;;;;;;;;;;;;;;;

; Functions
(defun search-service (name url)
  (browse-url
   (concat url (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (read-string (format "%s: " name))))))

(defun google ()
  "Googles a query or region if any."
  (interactive)
  (search-service "Google"
                  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="))

(defun startpage ()
  "Startpages a query or region if any."
  (interactive)
  (search-service "StartPage"
                  "https://startpage.com/do/metasearch.pl?query="))

(defun urbandictionary ()
  (interactive)
  (search-service "Urbandictionary"
                  "http://www.urbandictionary.com/define.php?term="))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(require 'magit)

; Git Gutter
(require 'git-gutter)
(add-to-list 'git-gutter:update-hooks 'magit-revert-buffer-hook)

; Hooks
(add-hook 'emacs-lisp-mode-hook 'git-gutter-mode)
(add-hook 'java-mode-hook 'git-gutter-mode)
(add-hook 'js-mode-hook 'git-gutter-mode)
(add-hook 'haml-mode-hook 'git-gutter-mode)
(add-hook 'html-mode-hook 'git-gutter-mode)
(add-hook 'org-mode-hook 'git-gutter-mode)
(add-hook 'php-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
(add-hook 'sh-mode-hook 'git-gutter-mode)
(add-hook 'css-mode-hook 'git-gutter-mode)
(add-hook 'git-commit-mode-hook 'turn-on-orgstruct)

; Key Bindings
(global-set-key (kbd "M-s g s") 'magit-status)
(global-set-key (kbd "M-s n h") 'git-gutter:next-hunk)
(global-set-key (kbd "M-s p h") 'git-gutter:previous-hunk)
(global-set-key (kbd "M-s s h") 'git-gutter:stage-hunk)
(global-set-key (kbd "M-s r h") 'git-gutter:revert-hunk)
(define-key magit-mode-map (kbd "M-s") nil)
(define-key magit-mode-map (kbd "M-S") nil)

; Variables
(setq magit-diff-refine-hunk t)


;;;;;;;;;;;;;;;;;;
;;; Visibility ;;;
;;;;;;;;;;;;;;;;;;

; Functions
(put 'narrow-to-region 'disabled nil)

(defun narrow-to-region-indirect-buffer (start end)
  "Create indirect buffer based on current buffer and narrow it
to currently active region. Instead of using arbitrary numbering,
incorporate line numbers of point and mark into buffer name for
indirect buffer. This command makes it easy to quickly generate
multiple views of the contents of any given buffer.

Adapted from: http://paste.lisp.org/display/135818."
  (interactive "r")
  (with-current-buffer
      (clone-indirect-buffer
       (generate-new-buffer-name
        (concat (buffer-name)
                "-indirect-L"
                (number-to-string (line-number-at-pos start))
                "-L"
                (number-to-string (line-number-at-pos end))))
       'display)
    (narrow-to-region start end)
    (deactivate-mark)
    (goto-char (point-min))))

(defun toggle-truncate-lines-on ()
  (toggle-truncate-lines 1))

; Hooks
(add-hook 'conf-space-mode-hook 'toggle-truncate-lines-on)
(add-hook 'occur-mode-hook 'toggle-truncate-lines-on)

; Key Bindings
(global-set-key (kbd "C-x n i") 'narrow-to-region-indirect-buffer)
(global-set-key (kbd "M-s t t") 'toggle-truncate-lines)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows + Frames ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

; Ace Window
(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i))

; Functions
(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         ; set-window-dedicated-p returns FLAG that was passed as
         ; second argument, thus can be used as COND for if:
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(defun change-split (&optional arg)
  "Change arrangement of two windows from 'stacked' to 'side-by-side'.

With a prefix arg, change arrangement from 'side-by-side' to 'stacked'."
  (interactive "P")
  (let ((split-function (progn
                          (if arg
                              (lambda () (split-window-below))
                            (lambda () (split-window-right)))))
        (current-buf (current-buffer))
        (other-buf (progn
                     (other-window 1)
                     (current-buffer))))
    (delete-other-windows)
    (funcall split-function)
    (switch-to-buffer current-buf)))

(defun kill-other-buffer-and-window ()
  "Kill the next buffer in line and closes the associated window.
I.e., if there are two windows, the active one stays intact, the
inactive one is closed. If there are several windows, the one
that would be reached by issuing C-x o once is closed, all others
stay intact. Should only be used if the frame is displaying more
than one window."
  (interactive)
  (other-window 1)
  (kill-buffer-and-window))

(defun swap-windows ()
  (interactive)
  (let ((current-buf (current-buffer))
        (other-buf (progn
                     (other-window 1)
                     (current-buffer))))
    (switch-to-buffer current-buf)
    (other-window -1)
    (switch-to-buffer other-buf)))

; Key Bindings
(global-set-key (kbd "M-s t d") 'toggle-window-dedicated)
(global-set-key (kbd "M-s c s") 'change-split)
(global-set-key (kbd "M-s k o") 'kill-other-buffer-and-window)
(global-set-key (kbd "M-s s w") 'swap-windows)

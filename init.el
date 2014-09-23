(package-initialize)
(setq idle-require-idle-delay 10)

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

; Dired Details (source: whattheemacsd.com)
(require 'dired-details)
(setq-default dired-details-hidden-string "> ")
(dired-details-install)

; Hidden Files
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

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

; Openwith
(openwith-mode t)
(setq openwith-associations
      (quote (("\\.\\(?:pdf\\|ps\\)\\'" "okular" (file))
              ("\\.\\(?:mp3\\|wav\\|flac\\)\\'" "gmusicbrowser" (file))
              ("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\|mov\\|mp4\\|ogg\\)\\'" "vlc" (file))
              ("\\.\\(?:jpe?g\\|png\\|bmp\\)\\'" "gwenview" (file))
              ("\\.chm\\'" "kchmviewer" (file))
              ("\\.\\(?:odt\\|doc\\|docx\\)\\'" "libreoffice" ("--writer" file))
              ("\\.\\(?:ods\\|xls\\|xlsx\\)\\'" "libreoffice" ("--calc" file))
              ("\\.\\(?:odp\\|pps\\|ppt\\|pptx\\)\\'" "libreoffice" ("--impress" file))
              ("\\.odg\\'" "libreoffice" ("--draw" file))
              ("\\.dia\\'" "dia" (file)))))

; Variables
(setq dired-isearch-filenames "dwim")
(setq dired-listing-switches "-alh")
(setq dired-recursive-copies (quote always))


;;;;;;;;;;;;;;;
;;; Editing ;;;
;;;;;;;;;;;;;;;

; Anchored Transpose
(global-set-key (kbd "M-s a t") 'anchored-transpose)

; Browse Kill Ring
(global-set-key (kbd "M-s b k") 'browse-kill-ring)

; Change Inner
(global-set-key (kbd "M-s c i") 'change-inner)
(global-set-key (kbd "M-s c o") 'change-outer)

; Expand Region
(global-set-key (kbd "M-s x r") 'er/expand-region)

; Functions
(autoload 'zap-up-to-char "misc")

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

(defadvice set-mark-command
  (before record-current-position (arg) activate compile)
  (when arg (push-mark)))

(defun flush-empty-lines ()
  "Remove empty lines from buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^$")))

(defun ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((before (downcase (or (thing-at-point 'word) "")))
        after)
    (call-interactively 'ispell-word)
    (setq after (downcase (or (thing-at-point 'word) "")))
    (unless (string= after before)
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table) before after))
      (message "\"%s\" now expands to \"%s\" %sally."
               before after (if p "loc" "glob"))))

(defun sort-lines-and-uniquify ()
  "Sort lines alphabetically (in ascending order) and remove duplicates."
  (interactive)
  (sort-lines nil (point-min) (point-max))
  (shell-command-on-region (point-min) (point-max) "uniq" nil t))

(defun zap-to-string (arg str)
  "Kill up to but not including ARG'th occurrence of STR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if STR not found."
  (interactive "p\nsZap to string: ")
  (save-excursion
    (let* ((start (point))
           (len (length str))
           (end (if (< arg 0)
                    (+ (search-forward str nil nil arg) len)
                  (- (search-forward str nil nil arg) len))))
      (kill-region start end))))

; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Key Bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-s f e") 'flush-empty-lines)
(global-set-key (kbd "M-s i a") 'ispell-word-then-abbrev)
(global-set-key (kbd "M-s s u") 'sort-lines-and-uniquify)
(global-set-key (kbd "M-s z") 'zap-to-string)
(global-set-key (kbd "M-z") 'zap-up-to-char)

; Mark Lines
(require 'mark-lines)
(global-set-key (kbd "M-s m") 'mark-lines-next-line)

; Move Text
(global-set-key (kbd "M-s u") 'move-text-up)
(global-set-key (kbd "M-s d") 'move-text-down)

; Multiple Cursors
(global-set-key (kbd "M-s n l") 'mc/mark-next-like-this)
(global-set-key (kbd "M-s a l") 'mc/mark-all-like-this)
(global-set-key (kbd "M-s a d") 'mc/mark-all-dwim)

; Rainbow Delimiters
(add-hook 'org-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

(defun sp-wrap-with-parens (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "("))

(defun sp-wrap-with-double-quotes (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "\""))

(defun sp-wrap-with-single-quotes (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "'"))

(define-key sp-keymap (kbd "M-s (") 'sp-wrap-with-parens)
(define-key sp-keymap (kbd "M-s \"") 'sp-wrap-with-double-quotes)
(define-key sp-keymap (kbd "M-s '") 'sp-wrap-with-single-quotes)

; Variables
(setq cua-enable-cua-keys nil)
(setq require-final-newline t)
(setq save-abbrevs t)
(setq save-interprogram-paste-before-kill t)
(setq sentence-end-double-space nil)
(setq set-mark-command-repeat-pop t)
(setq tab-width 4)
(setq-default abbrev-mode t)

; Whitespace
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column nil)
(add-hook 'org-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; Hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

; Key Bindings
(define-key emacs-lisp-mode-map (kbd "M-s e b") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "M-s e r") 'eval-region)

; Slime Nav
(defadvice turn-on-elisp-slime-nav-mode
  (after configure-slime-nav activate compile)
  (modeline-remove-lighter 'elisp-slime-nav-mode))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

; Variables
(setq eldoc-minor-mode-string "")


;;;;;;;;;;;;;
;;; Fonts ;;;
;;;;;;;;;;;;;

(set-face-attribute 'default nil :font "Monaco-10")

(require 'unicode-fonts)
(unicode-fonts-setup)


;;;;;;;;;;;;
;;; Help ;;;
;;;;;;;;;;;;

(find-function-setup-keys)

; Guide Key
(setq guide-key/guide-key-sequence '("C-c" "C-x r" "C-x v" "C-x 4"))
(guide-key-mode t)


;;;;;;;;;;;
;;; Ido ;;;
;;;;;;;;;;;

(ido-mode (quote both))
(ido-everywhere)

; Flx
(flx-ido-mode 1)

; Key Bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

; Ubiquitous
(ido-ubiquitous-mode)
(setq ido-ubiquitous-command-exceptions
      (quote (sclang-dump-interface sclang-dump-full-interface)))

; Variables
(add-to-list 'ido-ignore-buffers "\*Compile-Log\*")
(setq ido-create-new-buffer (quote always))
(setq ido-enable-flex-matching t)
(setq ido-use-virtual-buffers t)


;;;;;;;;;;;;;;;;;
;;; Interface ;;;
;;;;;;;;;;;;;;;;;

; Controls
(set-scroll-bar-mode nil)
(tool-bar-mode 0)

; Cursor
(blink-cursor-mode -1)

; Functions
(defun customize-scratch-buffer ()
  "Display custom message in *scratch* buffer."
  (erase-buffer)
  (insert ";; Parentheses are just *hugs* for your function calls!")
  (newline 2))

; Theme
(defun customize-enabled-theme ()
  (let ((enabled-theme (car custom-enabled-themes)))
    (cond ((eq enabled-theme 'sanityinc-tomorrow-night)
           (fringe-mode 0))
          ((eq enabled-theme 'ample)
           (fringe-mode 0))
          ((eq enabled-theme 'base16-default)
           (set-cursor-color "#FF5A0E")
           (fringe-mode 0))
          ((eq enabled-theme 'tronesque)
           (set-face-attribute 'dired-directory nil :foreground "#2872b2")
           (set-face-attribute 'info-header-xref nil :foreground "#2872b2"))
          ((eq enabled-theme 'wombat)
           (set-cursor-color "#FF5A0E")))))

(defadvice load-theme
  (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

(defadvice load-theme
  (after load-custom-theme-settings
         (theme &optional no-confirm no-enable)
         activate)
  (customize-enabled-theme))

(load-theme 'sanityinc-tomorrow-night t)

; Tooltips
(tooltip-mode 0)

; Variables
(setq inhibit-startup-screen t)

; Writeroom
(defun turn-off-git-gutter ()
  (if (not git-gutter-mode)
      (git-gutter-mode t)
    (git-gutter-mode -1)))

(add-hook 'writeroom-mode-hook 'turn-off-git-gutter)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

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

; Functions
(defun json-reformat-buffer ()
  (interactive)
  (json-reformat-region (point-min) (point-max)))

; Variables
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
(setq echo-keystrokes 0.3)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)
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

; Lighters
(defun modeline-set-lighter (minor-mode lighter)
  (when (assq minor-mode minor-mode-alist)
    (setcar (cdr (assq minor-mode minor-mode-alist)) lighter)))

(defun modeline-remove-lighter (minor-mode)
  (modeline-set-lighter minor-mode ""))

(modeline-set-lighter 'auto-fill-function (string 32 #x23ce))

; Modes
(column-number-mode t)

; Nyan
(nyan-mode t)
(setq nyan-bar-length 16)

; Unique Buffer Names
(require 'uniquify)
(setq uniquify-buffer-name-style (quote forward))

; Variables
(setf (nth 5 mode-line-modes)
      '(:eval (if (buffer-narrowed-p) (string 32 #x27fa) "")))

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

; Emphasis
(setcar org-emphasis-regexp-components " \t('\"{-")
(setcar (nthcdr 1 org-emphasis-regexp-components) "\[[:alpha:]- \t.,:!?;'\")}\\")
(setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n\"'")
(setcar (nthcdr 4 org-emphasis-regexp-components) 2)
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

; Exports
(idle-require 'ox-md)

; Functions
(defadvice org-display-inline-images
  (around handle-openwith
          (&optional include-linked refresh beg end) activate compile)
  (openwith-mode -1)
  ad-do-it
  (openwith-mode 1))

(defun org-at-outline-or-file-header ()
  (or (looking-at org-outline-regexp)
      (looking-at "^#\+")))

; Babel
(idle-require 'ob-plantuml)
(eval-after-load 'ob-plantuml
  '(progn
     (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
     (setq plantuml-jar-path "/opt/plantuml/plantuml.jar")))

; Hooks
(add-hook 'org-mode-hook 'turn-on-auto-fill)

; Key Bindings
(define-key org-mode-map (kbd "<C-tab>") 'pcomplete)
(define-key org-mode-map (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-c t") 'org-toggle-link-display)
(define-key org-mode-map (kbd "M-n") 'org-next-item)
(define-key org-mode-map (kbd "M-p") 'org-previous-item)
;; Mnemonics: (i)tem, (t)ree
(define-key org-mode-map (kbd "M-s i u") 'org-metaup)
(define-key org-mode-map (kbd "M-s i d") 'org-metadown)
(define-key org-mode-map (kbd "M-s i l") 'org-metaleft)
(define-key org-mode-map (kbd "M-s i r") 'org-metaright)
(define-key org-mode-map (kbd "s-l") 'org-shiftleft)
(define-key org-mode-map (kbd "s-r") 'org-shiftright)
(define-key org-mode-map (kbd "M-s t u") 'org-shiftmetaup)
(define-key org-mode-map (kbd "M-s t d") 'org-shiftmetadown)
(define-key org-mode-map (kbd "M-s t l") 'org-shiftmetaleft)
(define-key org-mode-map (kbd "M-s t r") 'org-shiftmetaright)
(define-key org-mode-map (kbd "M-s t h") 'org-insert-todo-heading)
(define-key org-mode-map (kbd "M-s t s") 'org-insert-todo-subheading)
(global-set-key (kbd "C-c l") 'org-store-link)

; Variables
(setq org-agenda-include-diary t)
(setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
(setq org-completion-use-ido t)
(setq org-enforce-todo-dependencies t)
(setq org-export-copy-to-kill-ring nil)
(setq org-footnote-define-inline t)
(setq org-footnote-auto-label 'random)
(setq org-list-allow-alphabetical t)
(setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "-")))
(setq org-list-use-circular-motion t)
(setq org-M-RET-may-split-line
      '((headline . nil) (item . t) (table . t)))
(setq org-return-follows-link t)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-track-ordered-property-with-tag t)
(setq org-use-speed-commands 'org-at-outline-or-file-header)
(add-to-list 'org-structure-template-alist
             '("o" "#+BEGIN_COMMENT\n?\n#+END_COMMENT") t)


;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Manager ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

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
(ac-flyspell-workaround)
(setq ac-use-menu-map t)
(setq ac-auto-show-menu 0.3)
(setq ac-ignore-case nil)

; Hide/Show
(require 'hideshow-org)

(defun configure-hs ()
  "Configure Hide/Show"
  (setq hs-isearch-open t))

(defun turn-on-hs-org ()
  (hs-org/minor-mode 1)
  (configure-hs))

(add-to-hooks '(emacs-lisp-mode-hook
                haml-mode-hook
                html-mode-hook
                java-mode-hook
                js-mode-hook
                php-mode-hook
                python-mode-hook)
              'turn-on-hs-org)

; Indentation
(setq-default indent-tabs-mode nil)

; Parens
(show-paren-mode t)

; Smart Semicolon
(defun tim/electric-semicolon ()
  (interactive)
  (end-of-line)
  (when (not (looking-back ";"))
    (insert ";")))

(defun tim/enable-electric-semicolon ()
  (interactive)
  (local-set-key (kbd ";") 'tim/electric-semicolon))

(add-hook 'java-mode-hook 'tim/enable-electric-semicolon)
(add-hook 'js-mode-hook 'tim/enable-electric-semicolon)
(add-hook 'php-mode-hook 'tim/enable-electric-semicolon)

; Subword Mode
(defun turn-on-subword-mode ()
  (subword-mode 1))

(add-to-hooks `(java-mode-hook js-mode-hook php-mode-hook python-mode-hook)
              'turn-on-subword-mode)

; Which Function
(which-function-mode)

; yasnippet
(yas-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project Management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Projectile
(projectile-global-mode)
(add-to-list 'projectile-globally-ignored-directories "doxygen")
(setq projectile-mode-line
      '(:eval (format " %s[%s]"
                      (string #x1f5c0) (projectile-project-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(idle-require 'python)
(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "M-s f n") 'flymake-goto-next-error)
     (define-key python-mode-map (kbd "M-s f p") 'flymake-goto-prev-error)))

; Variables
(setq python-shell-interpreter "ipython")


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

; Loccur
(require 'loccur)
(global-set-key (kbd "M-s l o") 'loccur-current)

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


;;;;;;;;;;;;;;;;
;;; Speedbar ;;;
;;;;;;;;;;;;;;;;

; Variables
(setq speedbar-use-images nil)


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
  "Google a query or region if any."
  (interactive)
  (search-service "Google"
                  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="))

(defun startpage ()
  "Startpage a query or region if any."
  (interactive)
  (search-service "StartPage"
                  "https://startpage.com/do/metasearch.pl?query="))

(defun thesaurus ()
  "Look up synonyms for query or region if any."
  (interactive)
  (search-service "Thesaurus" "http://thesaurus.com/browse/"))

(defun urbandictionary ()
  "Look up Urbandictionary definition(s) for query or region if any."
  (interactive)
  (search-service "Urbandictionary"
                  "http://www.urbandictionary.com/define.php?term="))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(require 'magit)

; Functions
(defun magit-ls-files ()
  "List tracked files of current repository."
  (interactive)
  (if (derived-mode-p 'magit-mode)
      (magit-git-command "ls-files" default-directory)
    (message "Not in a Magit buffer.")))

; Git Gutter
(require 'git-gutter)
(add-to-list 'git-gutter:update-hooks 'magit-revert-buffer-hook)

; git-wip
(load "~/git-wip/emacs/git-wip.el")
(require 'git-wip-timemachine)

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
(define-key magit-mode-map (kbd "K") 'magit-ls-files)

; Variables
(setq magit-diff-refine-hunk t)
(setq magit-auto-revert-mode-lighter "")


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

(defun tim/set-selective-display (&optional arg)
  "Use selective display to hide lines below current column.
With a prefix arg, clear selective display."
  (interactive "P")
  (if arg
      (set-selective-display -1)
    (set-selective-display (+ (current-column) 1))))

(defun toggle-truncate-lines-on ()
  (toggle-truncate-lines 1))

; Hooks
(add-hook 'conf-space-mode-hook 'toggle-truncate-lines-on)
(add-hook 'occur-mode-hook 'toggle-truncate-lines-on)

; Key Bindings
(global-set-key (kbd "C-x n i") 'narrow-to-region-indirect-buffer)
(global-set-key (kbd "C-x $") 'tim/set-selective-display)
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

; Modes
(winner-mode)
(global-set-key (kbd "C-c u") 'winner-undo)
(global-set-key (kbd "C-c r") 'winner-redo)

; Variables
(setq ediff-split-window-function 'split-window-horizontally)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(idle-require-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(modeline-remove-lighter 'auto-complete-mode)
(modeline-remove-lighter 'git-gutter-mode)
(modeline-remove-lighter 'guide-key-mode)
(modeline-remove-lighter 'hs-minor-mode)
(modeline-remove-lighter 'smartparens-mode)
(modeline-remove-lighter 'whitespace-mode)
(modeline-remove-lighter 'yas-minor-mode)
(modeline-set-lighter 'abbrev-mode " Abbr")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(customize-scratch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(split-window-horizontally)

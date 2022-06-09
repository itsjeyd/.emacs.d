(package-initialize)
(setq package-enable-at-startup nil)

(require 'use-package)
(require 'bind-key)



;;;;;;;;;;;;;;
;;; Custom ;;;
;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)



;;;;;;;;;;;;;
;;; Paths ;;;
;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/lisp/")



;;;;;;;;;;;;
;;; Keys ;;;
;;;;;;;;;;;;

(defvar custom-keys-mode-map (make-sparse-keymap)
  "Keymap for custom-keys-mode.")

(defvar custom-keys-mode-prefix-map (lookup-key global-map (kbd "M-s"))
  "Keymap for custom key bindings starting with M-s prefix.")

(define-key custom-keys-mode-map (kbd "M-s") custom-keys-mode-prefix-map)

(define-minor-mode custom-keys-mode
  "A minor mode for custom key bindings."
  :group 'keys
  :lighter ""
  :keymap 'custom-keys-mode-map
  :global t)

(defun prioritize-custom-keys
    (file &optional noerror nomessage nosuffix must-suffix)
  "Try to ensure that custom key bindings always have priority."
  (unless (eq (caar minor-mode-map-alist) 'custom-keys-mode)
    (let ((custom-keys-mode-map (assq 'custom-keys-mode minor-mode-map-alist)))
      (assq-delete-all 'custom-keys-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist custom-keys-mode-map))))

(advice-add 'load :after #'prioritize-custom-keys)



;;;;;;;;;;;;;;;
;;; Backups ;;;
;;;;;;;;;;;;;;;

(setq backup-directory-alist '(("_EDITMSG\\'")
                               ("-autoloads.el\\'")
                               ("-loaddefs.el\\'")
                               ("." . "~/.emacs.d/backups/")))

(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))


;;;;;;;;;;;;;;;
;;; Buffers ;;;
;;;;;;;;;;;;;;;

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Functions
  (defun ibuffer-group-buffers ()
    (ibuffer-switch-to-saved-filter-groups "Default"))

  ;; Hooks
  (add-hook 'ibuffer-mode-hook #'ibuffer-group-buffers)
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

  ;; Variables
  (setq-default ibuffer-saved-filter-groups
                '(("Default"
                   ("Dired" (mode . dired-mode))
                   ("Magit" (name . "\*magit.+*"))
                   ("Org" (mode . org-mode))
                   ("Grep" (name . "\*h?grep-?.*"))
                   ("Helm" (name . "\*helm .*\*"))
                   ("Temporary" (name . "\*.*\*")))))
  (setq-default ibuffer-sorting-mode 'major-mode))

; Commands
(defvar temp-buffer-count 0)

(defun make-temp-buffer ()
  (interactive)
  (let ((temp-buffer-name (format "*temp-%d*" temp-buffer-count)))
    (switch-to-buffer temp-buffer-name)
    (message "New temp buffer (%s) created." temp-buffer-name))
  (setq temp-buffer-count (1+ temp-buffer-count)))

; Key Bindings
(global-set-key (kbd "C-c t") #'make-temp-buffer)
(define-key custom-keys-mode-prefix-map (kbd "r b") #'revert-buffer)

; Variables
(setq confirm-nonexistent-file-or-buffer nil)
(setq revert-without-query '(".*"))



;;;;;;;;;;;;;;;;;;;;
;;; Byte-Compile ;;;
;;;;;;;;;;;;;;;;;;;;

; Commands
(defun recompile-elisp-file ()
  (interactive)
  (when (and buffer-file-name (string-match "\\.el" buffer-file-name))
    (let ((byte-file (concat buffer-file-name "\\.elc")))
      (if (or (not (file-exists-p byte-file))
              (file-newer-than-file-p buffer-file-name byte-file))
          (byte-compile-file buffer-file-name)))))

(add-hook 'after-save-hook #'recompile-elisp-file)



;;;;;;;;;;;;;;;;;;;
;;; Common Lisp ;;;
;;;;;;;;;;;;;;;;;;;

;; (use-package cl-lib)



;;;;;;;;;;;;;
;;; Dired ;;;
;;;;;;;;;;;;;

(use-package dired
  :ensure nil
  :config

  ;; Commands
  (defun dired-jump-to-top ()
    (interactive)
    (goto-char (point-min))
    (if dired-hide-details-mode
        (dired-next-line 3)
      (dired-next-line 4)))

  (defun dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))

  ;; Commands
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Hooks
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  ;; Key Bindings
  (bind-keys :map dired-mode-map
             (")" . dired-hide-details-mode)
             ((vector 'remap 'beginning-of-buffer) . dired-jump-to-top)
             ((vector 'remap 'end-of-buffer) . dired-jump-to-bottom))

  ;; Variables
  (setq dired-dwim-target t)
  (setq dired-isearch-filenames "dwim")
  (setq dired-listing-switches "-alh")
  (setq dired-recursive-copies 'always)
  (setq dired-use-ls-dired nil))

(use-package dired-x
  :ensure nil
  :bind ("C-x C-j" . dired-jump)
  :config
  ;; Hooks
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  ;; Key Bindings
  (bind-key "M-o" #'dired-omit-mode dired-mode-map)

  ;; Variables
  (setq dired-omit-files "^\\...+$"))



;;;;;;;;;;;;;;;
;;; Editing ;;;
;;;;;;;;;;;;;;;

;; (use-package anchored-transpose
;;   :commands anchored-transpose)

(use-package auto-complete-config
  :ensure auto-complete
  :config
  (ac-config-default)
  (ac-flyspell-workaround)

  ;; Key Bindings
  (bind-keys :map ac-completing-map
             ("C-h" . ac-help)
             ("C-v" . ac-quick-help-scroll-down)
             ("M-v" . ac-quick-help-scroll-up))
  (bind-key "C-f" #'ac-stop ac-menu-map)

  ;; Variables
  (setq ac-auto-show-menu 0.2)
  (setq ac-quick-help-delay 0.5)
  (setq ac-use-menu-map t))

(use-package avy-zap
  :bind ("M-z" . avy-zap-up-to-char-dwim))

(use-package caps-lock
  :commands caps-lock-mode
  :init
  (bind-key "c l" #'caps-lock-mode custom-keys-mode-prefix-map))

(use-package change-inner
  :ensure t
  :bind (("C-c i" . change-inner)
         ("C-c o" . change-outer)))

(use-package comment-dwim-2
 :ensure t
 :bind ("M-;" . comment-dwim-2))

(use-package elec-pair
  :ensure nil
  :init
  (electric-pair-mode 1)

  ;; Pairs
  (defvar single-backticks '(?\` . ?\`)))

(use-package expand-region
  :commands er/expand-region
  :init
  (bind-key "@" #'er/expand-region custom-keys-mode-prefix-map))

(use-package iso-transl
  :ensure nil
  :defer t
  :config
  (bind-keys :map iso-transl-ctl-x-8-map
             ("a" . "⟶")
             ("l" . "⚡" )))

;; (use-package move-text
;;   :commands (move-text-up move-text-down))

(use-package multiple-cursors
  :config

;;   (use-package mc-hide-unmatched-lines-mode
;;     :ensure multiple-cursors
;;     :commands mc-hide-unmatched-lines-mode
;;     :config
;;     (setq hum/lines-to-expand 1))

  ;; Hydra
  (defhydra hydra-mc ()
    "MC"
    ("n" mc/mark-next-like-this "next")
    ("p" mc/mark-previous-like-this "prev")
;;     ("s" hydra-mc-symbols/body "symbols" :color blue)
;;     ("w" hydra-mc-words/body "words" :color blue)
;;     ("S" hydra-mc-skip/body "skip" :color blue)
;;     ("U" hydra-mc-unmark/body "unmark" :color blue)
;;     ("o" hydra-mc-operate/body "operate" :color blue)
;;     ("C-'" mc-hide-unmatched-lines-mode "hide unmatched")
    ("q" nil "exit" :color blue))

;;   (defhydra hydra-mc-symbols ()
;;     "MC (symbols)"
;;     ("n" mc/mark-next-symbol-like-this "next")
;;     ("p" mc/mark-previous-symbol-like-this "prev")
;;     ("q" hydra-mc/body "exit" :color blue))

;;   (defhydra hydra-mc-words ()
;;     "MC (words)"
;;     ("n" mc/mark-next-word-like-this "next")
;;     ("p" mc/mark-previous-word-like-this "prev")
;;     ("q" hydra-mc/body "exit" :color blue))

;;   (defhydra hydra-mc-skip ()
;;     "MC (skip)"
;;     ("n" mc/skip-to-next-like-this "next")
;;     ("p" mc/skip-to-previous-like-this "prev")
;;     ("q" hydra-mc/body "exit" :color blue))

;;   (defhydra hydra-mc-unmark ()
;;     "MC (unmark)"
;;     ("n" mc/unmark-next-like-this "next")
;;     ("p" mc/unmark-previous-like-this "prev")
;;     ("q" hydra-mc/body "exit" :color blue))

;;   (defhydra hydra-mc-operate ()
;;     "MC (operate)"
;;     ("n" mc/insert-numbers "number")
;;     ("s" mc/sort-regions "sort")
;;     ("r" mc/reverse-regions "reverse")
;;     ("q" hydra-mc/body "exit" :color blue))

;;   (defhydra hydra-mc-all ()
;;     "MC (all)"
;;     ("d" mc/mark-all-dwim "dwim")
;;     ("f" mc/mark-all-like-this-in-defun "defun")
;;     ("l" mc/mark-all-like-this "like this")
;;     ("r" mc/mark-all-in-region "region")
;;     ("R" mc/mark-all-in-region-regexp "region (regexp)")
;;     ("s" hydra-mc-symbols-all/body "symbols" :color blue)
;;     ("w" hydra-mc-words-all/body "words" :color blue))

;;   (defhydra hydra-mc-symbols-all ()
;;     "MC (all symbols)"
;;     ("l" mc/mark-all-symbols-like-this "like this")
;;     ("f" mc/mark-all-symbols-like-this-in-defun "defun")
;;     ("q" hydra-mc-all/body "exit" :color blue))

;;   (defhydra hydra-mc-words-all ()
;;     "MC (all words)"
;;     ("l" mc/mark-all-words-like-this "like this")
;;     ("f" mc/mark-all-words-like-this-in-defun "defun")
;;     ("q" hydra-mc-all/body "exit" :color blue))

;;   (defhydra hydra-mc-edit (:color blue)
;;     "MC (edit)"
;;     ("l" mc/edit-lines "lines")
;;     ("b" mc/edit-beginnings-of-lines "beginnings")
;;     ("e" mc/edit-ends-of-lines "ends"))

  ;; Key Bindings
  (bind-keys :map custom-keys-mode-prefix-map
             ("m" . hydra-mc/body)
;;              ("a" . hydra-mc-all/body)
;;              ("e" . hydra-mc-edit/body)
  ))

(use-package utils
  :ensure nil
  :commands (flush-empty-lines sort-lines-and-uniquify unfill-paragraph))

; Advice
(defun record-current-position (arg)
  (when arg (push-mark)))

(advice-add 'set-mark-command :before #'record-current-position)

(defun determine-scope (beg end &optional region)
  "Determine scope for next invocation of `kill-region' or
`kill-ring-save': When called interactively with no active
region, operate on a single line. Otherwise, operate on region."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-region :before #'determine-scope)
(advice-add 'kill-ring-save :before #'determine-scope)

; Commands
(defun kill-region-with-arg (arg)
  (interactive "P")
  (if arg
      (let ((beg (line-beginning-position))
            (end (line-beginning-position (+ arg 1))))
        (kill-region beg end)
        (message "Killed %d lines." arg))
    (call-interactively #'kill-region)))

(defun kill-ring-save-with-arg (arg)
  (interactive "P")
  (if arg
      (let ((beg (line-beginning-position))
            (end (line-beginning-position (+ arg 1))))
        (kill-ring-save beg end)
        (message "Copied %d lines." arg))
    (call-interactively #'kill-ring-save)))

; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; (defhydra hydra-move-text ()
;;   "Move text"
;;   ("u" move-text-up "up")
;;   ("d" move-text-down "down"))

; Key Bindings
(global-set-key (kbd "C-w") #'kill-region-with-arg)
(global-set-key (kbd "M-w") #'kill-ring-save-with-arg)
(global-set-key (kbd "M-=") #'count-words)
;; (define-key custom-keys-mode-prefix-map (kbd "u") #'hydra-move-text/body)
;; (define-key custom-keys-mode-prefix-map (kbd "d") #'hydra-move-text/body)

; Variables
(setq cua-enable-cua-keys nil)
(setq require-final-newline t)
(setq save-interprogram-paste-before-kill t)
(setq sentence-end-double-space nil)
(setq set-mark-command-repeat-pop t)
(setq tab-width 4)



;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eldoc
  :commands eldoc-mode
  :config
  (setq eldoc-minor-mode-string ""))

; Hooks
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File Associations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations
        (list (list (openwith-make-extension-regexp '("pdf" "ps"))
                    "open" '(file))
              (list (openwith-make-extension-regexp '("flac" "mp3" "wav"))
                    "open" '(file))
              (list (openwith-make-extension-regexp '("avi" "flv" "mov" "mp4"
                                                      "mpeg" "mpg" "ogg" "wmv"))
                    "open" '(file))
              (list (openwith-make-extension-regexp '("bmp" "jpeg" "jpg" "png"))
                    "open" '(file))
              (list (openwith-make-extension-regexp '("doc" "docx" "odt"))
                    "open" '(file))
              (list (openwith-make-extension-regexp '("ods" "xls" "xlsx"))
                    "open" '(file))
              (list (openwith-make-extension-regexp '("odp" "pps" "ppt" "pptx"))
                    "open" '(file)))))



;;;;;;;;;;;;;
;;; Fonts ;;;
;;;;;;;;;;;;;

(set-face-attribute 'default nil :font "Monaco-11")

; Unicode Fonts
;; (use-package unicode-fonts
;;   :config
;;   (unicode-fonts-setup))



;;;;;;;;;;;;
;;; Helm ;;;
;;;;;;;;;;;;

;; (use-package helm
;;   :config

;;   (use-package helm-config
;;     :ensure helm)

;;   (use-package helm-firefox
;;     :commands helm-firefox-bookmarks
;;     :config
;;     (defun helm-get-firefox-user-init-dir ()
;;       "Return name of Firefox profile to list bookmarks for."
;;       "~/.mozilla/firefox/29x5sgi7.default/"))


;;   (helm-adaptive-mode 1)

;;   ;; Key Bindings
;;   (bind-key "C-c k" #'helm-show-kill-ring)
;;   (bind-key "C-c SPC" #'helm-all-mark-rings)
;;   (bind-key "B" #'helm-firefox-bookmarks helm-command-map)
;;   (bind-key "M-o" #'helm-previous-source helm-map)

;;   ;; Variables
;;   (setq helm-truncate-lines t)
;;   (setq helm-grep-save-buffer-name-no-confirm t))

;; (use-package helm-flx
;;   :config
;;   (helm-flx-mode 1))



;;;;;;;;;;;;
;;; Help ;;;
;;;;;;;;;;;;

(find-function-setup-keys)

(use-package which-key
  :config
  (which-key-mode t)) ; Seems to be messing with frame/window configuration?

;; (use-package cheatsheet
;;   :config
;;   ;; Cheats

;;   ;; Editing
;;   (cheatsheet-add :group 'Editing
;;                   :key "M-- M-^"
;;                   :description "Join line to next line (no need for point to be at EOL)")
;;   (cheatsheet-add :group 'Editing
;;                   :key "M-s @"
;;                   :description "Expand region")
;;   ;; Ibuffer
;;   (cheatsheet-add :group 'Ibuffer
;;                   :key "M-x ibuffer-do-isearch"
;;                   :description "Incremental search in marked buffers")
;;   (cheatsheet-add :group 'Ibuffer
;;                   :key "M-DEL"
;;                   :description "Unmark all marked buffers")
;;   (cheatsheet-add :group 'Ibuffer
;;                   :key "O"
;;                   :description "Run occur on marked buffers")
;;   (cheatsheet-add :group 'Ibuffer
;;                   :key "* s"
;;                   :description "Mark temporary buffers")
;;   (cheatsheet-add :group 'Ibuffer
;;                   :key "="
;;                   :description "Diff changes with file on disk")
;;   (cheatsheet-add :group 'Ibuffer
;;                   :key ","
;;                   :description "Rotate between sorting modes")
;;   ;; Helm
;;   (cheatsheet-add :group 'Helm
;;                   :key "C-x c B"
;;                   :description "Access Firefox bookmarks")
;;   ;; Movement
;;   (cheatsheet-add :group 'Movement
;;                   :key "M-s a"
;;                   :description "Hand Isearch matches over to Avy")
;;   (cheatsheet-add :group 'Movement
;;                   :key "M-s C-w"
;;                   :description "Move line/region here")
;;   (cheatsheet-add :group 'Movement
;;                   :key "M-s M-w"
;;                   :description "Copy line/region here")
;;   (cheatsheet-add :group 'Movement
;;                   :key "M-g w"
;;                   :description "Go to word")
;;   ;; Org
;;   (cheatsheet-add :group 'Org
;;                   :key "M-x org-copy-link"
;;                   :description "Copy link at point")
;;   ;; Programming
;;   (cheatsheet-add :group 'Programming
;;                   :key "C-c y"
;;                   :description "Browse snippets with Helm")
;;   ;; Projects
;;   (cheatsheet-add :group 'Projects
;;                   :key "C-c p v"
;;                   :description "View status buffer for project")
;;   (cheatsheet-add :group 'Projects
;;                   :key "C-c p p"
;;                   :description "Switch project")
;;   (cheatsheet-add :group 'Projects
;;                   :key "C-c p o"
;;                   :description "Run occur on project buffers")
;;   (cheatsheet-add :group 'Projects
;;                   :key "C-c p k"
;;                   :description "Kill project buffers")
;;   (cheatsheet-add :group 'Projects
;;                   :key "C-c p h"
;;                   :description "Launch helm-projectile (lists project buffers, project files, projects)")
;;   (cheatsheet-add :group 'Projects
;;                   :key "C-c p I"
;;                   :description "Ibuffer for current project")
;;   (cheatsheet-add :group 'Projects
;;                   :key "C-c p F"
;;                   :description "Find file in known projects")
;;   ;; Python
;;   (cheatsheet-add :group 'Python
;;                   :key "M-x helm-pydoc"
;;                   :description "View documentation/source or import module")
;;   (cheatsheet-add :group 'Python
;;                   :key "C-x D"
;;                   :description "View definitions in file as tree")
;;   (cheatsheet-add :group 'Python
;;                   :key "C-c ?"
;;                   :description "View documentation for thing at point")
;;   ;; Search
;;   (cheatsheet-add :group 'Search
;;                   :key "C-c s"
;;                   :description "Launch dispatcher for web search")
;;   (cheatsheet-add :group 'Search
;;                   :key "C-M-s"
;;                   :description "Isearch with flex matching (forward)")
;;   (cheatsheet-add :group 'Search
;;                   :key "C-M-r"
;;                   :description "Isearch with flex matching (backward)")
;;   ;; Utilities
;;   (cheatsheet-add :group 'Utilities
;;                   :key "M-x quick-calc"
;;                   :description "Do a quick calculation in the minibuffer")
;;   ;; Version Control
;;   (cheatsheet-add :group 'VC
;;                   :key "M-x browse-at-remote/kill"
;;                   :description "Copy GitHub URL of current file (includes reference to current line)")
;;   (cheatsheet-add :group 'VC
;;                   :key "M-x browse-at-remote/browse"
;;                   :description "View current file on GitHub")
;;   (cheatsheet-add :group 'VC
;;                   :key "M-s g f"
;;                   :description "View file at specific revision")
;;   (cheatsheet-add :group 'VC
;;                   :key "M-s g b"
;;                   :description "Blame file being visited in current buffer")
;;   (cheatsheet-add :group 'VC
;;                   :key "e"
;;                   :description "Compare, stage, or *resolve* using Ediff")
;;   ;; Windows
;;   (cheatsheet-add :group 'Windows
;;                   :key "C-c 3"
;;                   :description "Split root window below")
;;   (cheatsheet-add :group 'Windows
;;                   :key "C-c 2"
;;                   :description "Split root window right")

;;   ;; Functions
;;   (defun cheatsheet-show-cheats-in-separate-frame ()
;;     (interactive)
;;     (let ((cheatsheet-frame (make-frame '((minibuffer . nil)))))
;;       (with-selected-frame cheatsheet-frame
;;         (toggle-frame-maximized)
;;         (cheatsheet-show)
;;         (delete-other-windows)
;;         (split-window-right)
;;         (follow-mode)
;;         (goto-char (point-min))
;;         (text-scale-adjust 2))))

;;   ;; Key Bindings
;;   (bind-key "<f1>" #'cheatsheet-show-cheats-in-separate-frame))

; Functions
(defun info-display-topic (topic)
  "Create command that opens up a separate *info* buffer for TOPIC."
  (let* ((bufname (format "*%s Info*" (capitalize topic)))
         (cmd-name (format "info-display-%s" topic))
         (cmd (intern cmd-name)))
    (if (fboundp cmd)
        cmd
      (eval `(defun ,cmd ()
               ,(format "Jump to %s info buffer, creating it if necessary.\nThis is *not* the buffer \\[info] would jump to, it is a separate entity." topic)
               (interactive)
               (if (get-buffer ,bufname)
                   (switch-to-buffer ,bufname)
                 (info ,topic ,bufname)))))))

; Hydra
;; (defhydra hydra-apropos (:color blue)
;;   "Apropos"
;;   ("a" apropos "apropos")
;;   ("c" apropos-command "cmd")
;;   ("d" apropos-documentation "doc")
;;   ("e" apropos-value "val")
;;   ("l" apropos-library "lib")
;;   ("o" apropos-user-option "opt")
;;   ("v" apropos-variable "var")
;;   ("i" info-apropos "info")
;;   ("t" xref-find-apropos "tags")
;;   ("z" hydra-customize-apropos/body "customize"))

;; (defhydra hydra-customize-apropos (:color blue)
;;   "Apropos (customize)"
;;   ("a" customize-apropos "apropos")
;;   ("f" customize-apropos-faces "faces")
;;   ("g" customize-apropos-groups "groups")
;;   ("o" customize-apropos-options "options"))

;; (defhydra hydra-info (:color blue)
;;   "Info"
;;   ("e" (funcall (info-display-topic "emacs")) "Emacs")
;;   ("l" (funcall (info-display-topic "elisp")) "Elisp")
;;   ("m" (funcall (info-display-topic "magit")) "Magit")
;;   ("o" (funcall (info-display-topic "org")) "Org Mode")
;;   ("S" (funcall (info-display-topic "sicp")) "SICP"))

;; ; Key Bindings
;; (global-set-key (kbd "C-h a") #'hydra-apropos/body)
;; (define-key custom-keys-mode-prefix-map (kbd "i") #'hydra-info/body)

; Variables
(setq help-window-select t)



;;;;;;;;;;;;
;;; HTML ;;;
;;;;;;;;;;;;

;; (use-package sgml-mode
;;   :commands sgml-mode
;;   :config
;;   (bind-key "C-c b" #'web-beautify-html sgml-mode-map))



;;;;;;;;;;;;;
;;; Hydra ;;;
;;;;;;;;;;;;;

(use-package hydra
  :defer t)



;;;;;;;;;;;
;;; Ido ;;;
;;;;;;;;;;;

(use-package ido
  :config

  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1))

  (use-package ido-completing-read+
    :ensure t
    :config
    ;; Variables
    (setq ido-cr+-max-items 50000)

    (ido-ubiquitous-mode 1))

  (use-package ido-vertical-mode
    :ensure t
    :config
    ;; Variables
    (setq ido-vertical-show-count t)
    (ido-vertical-mode 1))

  (use-package recentf
    :commands recentf-mode
    :config
    ;; Advice
    (defun recentf-discard-autoloads (orig file)
      (if (not (string-match-p "-autoloads" (file-name-nondirectory file)))
          (funcall orig file)
        nil))

    (advice-add 'recentf-keep-default-predicate :around #'recentf-discard-autoloads)

    (defun recentf-set-buffer-file-name (orig)
      (if (eq major-mode 'dired-mode)
          (progn (setq buffer-file-name default-directory)
                 (funcall orig)
                 (setq buffer-file-name nil))
        (funcall orig)))

    (advice-add 'recentf-track-opened-file :around #'recentf-set-buffer-file-name)
    (advice-add 'recentf-track-closed-file :around #'recentf-set-buffer-file-name)

    ;; Commands
    (defun ido-recentf-open ()
      "Use `ido-completing-read' to \\[find-file] a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))

    ;; Key Bindings
    (bind-key "C-x C-r" #'ido-recentf-open)

    ;; Variables
    (add-to-list 'recentf-used-hooks
                 '(dired-after-readin-hook recentf-track-opened-file))
    (setq recentf-max-saved-items 150)
    (setq recentf-save-file "~/.emacs.d/.recentf"))

  (ido-mode 'both)
  (ido-everywhere 1)
  (recentf-mode t)

  ;; Commands
  (defun ido-find-file-as-root ()
    "Like `ido-find-file', but automatically edit file with
  root-privileges if it is not writable by user."
    (interactive)
    (let ((file (ido-read-file-name "Edit as root: ")))
      (unless (file-writable-p file)
        (setq file (concat "/su:root@localhost:" file)))
      (find-file file)))

  ;; Key Bindings
  (bind-key "C-c f" #'ido-find-file-as-root)

  ;; Variables
  (add-to-list 'ido-ignore-buffers "\*Compile-Log\*")
  (add-to-list 'ido-ignore-buffers "\*Messages\*")
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-create-new-buffer 'always)
  (setq ido-enable-flex-matching t)
  (setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
  (setq ido-use-virtual-buffers t))

(use-package smex
 :ensure t
 :bind (("M-x" . smex))
 :config
 (setq smex-save-file "~/.emacs.d/.smex-items"))

; Variables
(setq gc-cons-threshold 7000000)



;;;;;;;;;;;;;;;;;
;;; Interface ;;;
;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package whitespace
  :commands whitespace-mode
  :config
  (modeline-remove-lighter 'whitespace-mode)

  ;; Hooks
  (add-hook 'prog-mode-hook #'whitespace-mode)

  ;; Variables
  (setq whitespace-line-column nil)
  (add-to-list 'whitespace-style 'lines-tail t))

; Controls
(set-scroll-bar-mode nil)
(tool-bar-mode 0)

; Cursor
(blink-cursor-mode 0)

(defvar default-cursor-color "#F2777A")
(defvar expandable-thing-before-point-color "#00FF7F")

; Lines
(global-hl-line-mode)

; Theme
;; (defun customize-enabled-theme ()
;;   (let ((enabled-theme (car custom-enabled-themes))
;;         (cursor-preferred-color "#FF5A0E"))
;;     (cond ((eq enabled-theme 'tronesque)
;;            (let ((fallback-color
;;                   (face-attribute 'show-paren-match :background)))
;;              (set-face-attribute
;;               'dired-directory nil :foreground fallback-color)
;;              (set-face-attribute
;;               'info-header-xref nil :foreground fallback-color))))))

;; (defun customize-theme ()
;;   (let ((default-background-color (face-attribute 'default :background)))
;;     (set-face-attribute 'fringe nil :background default-background-color)))

(defun disable-custom-themes (theme &optional no-confirm no-enable)
  (mapc 'disable-theme custom-enabled-themes))

(advice-add 'load-theme :before #'disable-custom-themes)

;; (defun load-custom-theme-settings (theme &optional no-confirm no-enable)
;;   (customize-theme)
;;   (customize-enabled-theme))

;; (advice-add 'load-theme :after #'load-custom-theme-settings)

(load-theme 'kaolin-valley-light t)

; Tooltips
(tooltip-mode nil)

; Variables
(setq inhibit-startup-screen t)
(setq initial-scratch-message
      ";; Parentheses are just *hugs* for your function calls!\n\n")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JavaScript Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package json-mode
  :config
  (bind-key "C-c b" #'web-beautify-js json-mode-map))



;;;;;;;;;;;;;;;;;;
;;; Minibuffer ;;;
;;;;;;;;;;;;;;;;;;

; Modes
(minibuffer-depth-indicate-mode 1)
(savehist-mode t)

; Prompts
(fset 'yes-or-no-p 'y-or-n-p)

; Variables
(setq echo-keystrokes 0.3)
(setq enable-recursive-minibuffers t)
(setq history-delete-duplicates t)
(setq history-length t)
(setq minibuffer-prompt-properties
      (append minibuffer-prompt-properties
              '(point-entered minibuffer-avoid-prompt)))



;;;;;;;;;;;;;;;;
;;; Modeline ;;;
;;;;;;;;;;;;;;;;

(use-package uniquify
  :ensure nil
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix nil))

; Functions
(defun modeline-set-lighter (minor-mode lighter)
  (when (assq minor-mode minor-mode-alist)
    (setcar (cdr (assq minor-mode minor-mode-alist)) lighter)))

(defun modeline-remove-lighter (minor-mode)
  (modeline-set-lighter minor-mode ""))

; Modes
(column-number-mode t)

; Variables
(setf (nth 5 mode-line-modes)
      '(:eval (if (buffer-narrowed-p) (string 32 #x27fa) "")))



;;;;;;;;;;;;;
;;; Modes ;;;
;;;;;;;;;;;;;

; Variables
(add-to-list 'auto-mode-alist '("\.env$" . conf-mode))



;;;;;;;;;;;;;;;;
;;; Movement ;;;
;;;;;;;;;;;;;;;;

(use-package avy
  :defer t
  :config
  ;; Commands
;;   (defun avy-move-region (arg)
;;     "Select two lines and move the text between them to point.

;;   The window scope is determined by `avy-all-windows' or
;;   `avy-all-windows-alt' when ARG is non-nil."
;;     (interactive "P")
;;     (let ((initial-window (selected-window)))
;;       (avy-with avy-copy-region
;;         (let* ((beg (avy--line arg))
;;                (end (avy--line arg))
;;                (str (buffer-substring-no-properties
;;                      beg
;;                      (save-excursion
;;                        (goto-char end)
;;                        (line-end-position)))))
;;           (select-window initial-window)
;;           (cond ((eq avy-line-insert-style 'above)
;;                  (beginning-of-line)
;;                  (save-excursion
;;                    (insert str "\n")))
;;                 ((eq avy-line-insert-style 'below)
;;                  (end-of-line)
;;                  (newline)
;;                  (save-excursion
;;                    (insert str)))
;;                 (t
;;                  (user-error "Unexpected `avy-line-insert-style'")))
;;           (kill-region beg end)))))

;;   ;; Hydra
;;   (defhydra hydra-avy-jump (:color blue)
;;     "Avy jump"
;;     ("c" avy-goto-char "char")
;;     ("w" avy-goto-word-0 "word")
;;     ("l" avy-goto-line "line")
;;     ("s" avy-goto-subword-0 "subword")
;;     ("C" goto-char "goto char")
;;     ("L" goto-line "goto line"))

;;   (defhydra hydra-avy-copy (:color blue)
;;     "Avy copy"
;;     ("l" avy-copy-line "line")
;;     ("r" avy-copy-region "region"))

;;   (defhydra hydra-avy-move (:color blue)
;;     "Avy move"
;;     ("l" avy-move-line "line")
;;     ("r" avy-move-region "region"))

;;   ;; Key Bindings
;;   (bind-key "M-g" #'hydra-avy-jump/body)
;;   (bind-keys :map custom-keys-mode-prefix-map
;;              ("C-w" . hydra-avy-move/body)
;;              ("M-w" . hydra-avy-copy/body))
;;   (bind-key "M-s a" #'avy-isearch isearch-mode-map)

  ;; Variables
  (setq avy-background t)
  (setq avy-case-fold-search nil)
  ;; (setq avy-keys (number-sequence ?a ?z))
  )

;; (defhydra hydra-move-by-page ()
;;   "Move by page"
;;   ("[" backward-page "prev page")
;;   ("]" forward-page "next page"))

;; (global-set-key (kbd "C-x [") #'hydra-move-by-page/body)
;; (global-set-key (kbd "C-x ]") #'hydra-move-by-page/body)



;;;;;;;;;;;;;;;;
;;; Org Mode ;;;
;;;;;;;;;;;;;;;;

(use-package org
  :commands org-mode
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :config

  ; Introduce keymap for additional key bindings
  (defvar org-mode-extra-keys-map (lookup-key org-mode-map (kbd "C-c C-x")))

  ;; (use-package org-ac
  ;;   :disabled
  ;;   :config
  ;;   (org-ac/config-default)
  ;;   (setq org-ac/ac-trigger-command-keys '("\\" "SPC" ":" "[" "+")))

  (use-package org-capture
    :ensure nil
    :bind ("C-c c" . org-capture)
    :config
  ;;   ;; Functions
  ;;   (defun format-quote (selection)
  ;;     (if (= (length selection) 0)
  ;;         ""
  ;;       (format "#+BEGIN_QUOTE\n  %s\n  #+END_QUOTE\n\n  " selection)))

    ;; Variables
    (setq org-capture-templates
          '(
            ("t" "New item" entry (file+headline "~/OpenCraft/tickets.org" "Inbox (tasks)")
             "** TODO %?" :prepend t :jump-to-captured t)
            ("n" "New note" entry (file+headline "~/OpenCraft/tickets.org" "Inbox (notes)")
             "** %U\n   %?" :prepend t :jump-to-captured t)
            )
          ))

  (use-package org-clock
    :ensure nil
    :bind (("C-c C-x C-d" . org-clock-display)
           ("C-c C-x C-j" . org-clock-goto)
           ("C-c C-x C-o" . org-clock-out)
           ("C-c C-x C-q" . org-clock-cancel-save-buffer)
           ("C-c C-x C-x" . org-clock-in-last-save-buffer))
    :config
    ;; Advice
    (defun org-save-task-buffer ()
      (let ((working-buffer (current-buffer)))
        (call-interactively #'org-clock-goto)
        (call-interactively #'save-buffer)
        (switch-to-buffer working-buffer)))

    (advice-add 'org-clock-in :after #'save-buffer)
    (advice-add 'org-store-log-note :after #'org-save-task-buffer)

    ;; Commands
    (defun org-clock-cancel-save-buffer ()
      (interactive)
      (with-current-buffer (org-clocking-buffer)
        (org-clock-cancel)
        (call-interactively #'save-buffer)))

    (defun org-clock-in-last-save-buffer ()
      (interactive)
      (let ((working-buffer (current-buffer)))
        (call-interactively #'org-clock-goto)
        (call-interactively #'org-clock-in) ; No need to call save-buffer explictly here, org-clock-in is advised
        (switch-to-buffer working-buffer)))

    ;; Key Bindings
    (bind-keys :map org-mode-extra-keys-map
               ("C-q" . org-clock-cancel-save-buffer)
               ("C-x" . org-clock-in-last-save-buffer)
               ("n" . org-clock-entry-new-day))

    ;; Keyboard Macros
    (fset 'org-clock-entry-new-day
          [?\C-n tab ?\C-n ?\C-n ?\C-o ?\C-p ?\C-p tab ?p ?\C-x ?\C-s])

    ;; Variables
    (setq org-clock-mode-line-total 'today)
    (setq org-clock-persist 'history)

    (org-clock-persistence-insinuate))

  (use-package org-footnote
    :ensure nil
    :config
    (setq org-footnote-define-inline t)
    (setq org-footnote-auto-label 'random))

  (use-package org-list
    :ensure nil
    :config
    ;; Advice
    (swap-args 'org-toggle-checkbox)

    ;; Key Bindings
    (bind-keys :map org-mode-map
               ("M-n" . org-next-item)
               ("M-p" . org-previous-item))

    ;; Variables
    (setq org-cycle-include-plain-lists t)
    (setq org-list-allow-alphabetical t)
    (setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "-")))
    (setq org-list-use-circular-motion t))

  (use-package org-tempo)

  (use-package ox
    :ensure nil
    :commands org-export-dispatch
    :config

    ;; Variables
    (setq org-export-copy-to-kill-ring nil)
    (setq org-export-dispatch-use-expert-ui nil))

    (use-package ox-gfm)

    (use-package ox-jira)

  ;; Babel
  (add-to-list 'org-babel-load-languages '(python . t) t)

  (org-babel-do-load-languages
   'org-babel-load-languages org-babel-load-languages)

  ;; Commands
  (defvar org-generic-drawer-regexp "^ +:[[:alpha:]]+:")

  (defun org-copy-link ()
    "Copy `org-mode' link at point."
    (interactive)
    (when (org-in-regexp org-link-bracket-re 1)
      (let ((link (org-link-unescape (match-string-no-properties 1))))
        (kill-new link)
        (message "Copied link: %s" link))))

  (defun org-next-drawer (arg)
    (interactive "p")
    (org-next-block arg nil org-generic-drawer-regexp))

  (defun org-previous-drawer (arg)
    (interactive "p")
    (org-previous-block arg org-generic-drawer-regexp))

  ;; Faces
  (set-face-attribute 'org-headline-done nil :strike-through t)

  ;; ;; Functions
  ;; (defvar org-bold-markup '(?\* . ?\*))
  ;; (defvar org-italics-markup '(?/ . ?/))
  ;; (defvar org-verbatim-markup '(?= . ?=))
  ;; (defvar org-code-markup '(?~ . ?~))

  ;; (defun org-add-electric-pairs ()
  ;;   (let ((org-electric-pairs `(,org-code-markup
  ;;                               ,org-verbatim-markup
  ;;                               ,org-italics-markup
  ;;                               ,org-bold-markup)))
  ;;     (setq-local electric-pair-pairs
  ;;                 (append electric-pair-pairs org-electric-pairs))
  ;;     (setq-local electric-pair-text-pairs electric-pair-pairs)))

  (defun org-point-in-speed-command-position-p ()
    (when (not (equal (point) (point-max)))
      (if (equal (point) 1)
          (or (looking-at org-outline-regexp)
              (looking-at "^#\+"))
        (or (looking-at org-outline-regexp)
            (looking-at "^#\+")
            (looking-at "^[[:blank:]]\\{2,\\}")
            (looking-at "^$")))))

  ;; Hooks
  (add-hook 'org-mode-hook #'enable-which-function-mode)
  ;; (add-hook 'org-mode-hook #'org-add-electric-pairs)

  ;; Key Bindings
  (bind-keys :map org-mode-extra-keys-map
             ("c" . org-table-copy-down)
             ("d" . org-metadown)
             ("l" . org-metaleft)
             ("r" . org-metaright)
             ("u" . org-metaup)
             ("D" . org-shiftmetadown)
             ("L" . org-shiftmetaleft)
             ("R" . org-shiftmetaright)
             ("U" . org-shiftmetaup))
  (bind-keys :map org-mode-map
             ("C-M-h" . org-mark-element)
             ("M-h" . mark-paragraph)
             ("M-s t h" . org-insert-todo-heading)
             ("M-s t l" . org-toggle-link-display)
             ("M-s t s" . org-insert-todo-subheading)
             ("s-d" . org-shiftdown)
             ("s-l" . org-shiftleft)
             ("s-r" . org-shiftright)
             ("s-u" . org-shiftup))

  ;; Variables
  (setq org-agenda-files
        '("~/OpenCraft/tickets.org"))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-todo-ignore-deadlines 'all)
  (setq org-agenda-todo-ignore-scheduled 'all)
  (setq org-babel-python-command "/usr/bin/python3")
  (setq org-catch-invisible-edits 'error)
  (setq org-confirm-babel-evaluate nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-hide-emphasis-markers t)
  (setq org-log-into-drawer t)
  ;; (setq org-M-RET-may-split-line '((headline . nil) (item . t) (table . t)))
  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets
        '((("~/OpenCraft/tickets.org") . (:maxlevel . 3))))
  (setq org-refile-use-outline-path t)
  ;; (setq org-return-follows-link t)
  (setq org-reverse-note-order t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  ;; (setq org-todo-repeat-to-state "RECURRING")
  (setq org-track-ordered-property-with-tag t)
  (setq org-use-speed-commands 'org-point-in-speed-command-position-p)
  (add-to-list 'org-speed-commands '("d" . org-next-drawer) t)
  (add-to-list 'org-speed-commands '("P" . org-previous-drawer) t))



;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Manager ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package package
  :commands list-packages
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/") t))



;;;;;;;;;;;;;;;;;;;
;;; Permissions ;;;
;;;;;;;;;;;;;;;;;;;

(use-package tramp
  :commands ido-find-file)

; Usage: C-x C-f /sudo::/path/to/file



;;;;;;;;;;;;;;;;;;;
;;; Programming ;;;
;;;;;;;;;;;;;;;;;;;

;; (use-package flycheck
;;   :commands flycheck-mode
;;   :config

;;   (use-package helm-flycheck)

;;   ;; Functions
;;   (defun flycheck-mode-line-status-text (&optional status)
;;     "Get a text describing STATUS for use in the mode line.

;;   STATUS defaults to `flycheck-last-status-change' if omitted or nil."
;;     (let ((text (pcase (or status flycheck-last-status-change)
;;                   (`not-checked "")
;;                   (`no-checker "-")
;;                   (`running "*")
;;                   (`errored "!")
;;                   (`finished
;;                    (if flycheck-current-errors
;;                        (let-alist (flycheck-count-errors flycheck-current-errors)
;;                          (format ":%s/%s" (or .error 0) (or .warning 0)))
;;                      ""))
;;                   (`interrupted "-")
;;                   (`suspicious "?"))))
;;       (concat " ⚡" text)))

;;   (defun flycheck-setup ()
;;     (bind-key "f" #'hydra-flycheck/body custom-keys-mode-prefix-map))

;;   ;; Hooks
;;   (add-hook 'flycheck-mode-hook #'flycheck-setup)

;;   ;; Hydra
;;   (defhydra hydra-flycheck ()
;;     "Flycheck"
;;     ("l" helm-flycheck "list errors")
;;     ("n" flycheck-next-error "next error")
;;     ("p" flycheck-previous-error "previous error")
;;     ("q" nil "quit" :color blue)))

(use-package yasnippet
  :commands yas-minor-mode
  :init
  ;; (add-hook 'css-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config

  ;; (use-package helm-c-yasnippet
  ;;   :bind ("C-c y" . helm-yas-complete)
  ;;   :config
  ;;   (setq helm-yas-not-display-dups nil)
  ;;   (setq helm-yas-display-key-on-candidate t))

  (yas-reload-all)
  (modeline-remove-lighter 'yas-minor-mode)

  ;; Functions
  (defun change-cursor-color-when-can-expand ()
    (set-cursor-color (if (last-thing-expandable-p)
			  expandable-thing-before-point-color
			default-cursor-color)))

  (defun last-thing-expandable-p ()
    (or (abbrev--before-point) (yasnippet-can-fire-p)))

  (defun yasnippet-can-fire-p (&optional field)
    (setq yas--condition-cache-timestamp (current-time))
    (let (relevant-snippets)
      (unless (and yas-expand-only-for-last-commands
		   (not (member last-command yas-expand-only-for-last-commands)))
	(setq relevant-snippets (if field
				    (save-restriction
				      (narrow-to-region (yas--field-start field)
							(yas--field-end field))
				      (yas--templates-for-key-at-point))
				  (yas--templates-for-key-at-point)))
	(and relevant-snippets (first relevant-snippets)))))

  ;; Hooks
  (add-hook 'post-command-hook #'change-cursor-color-when-can-expand)

  ;; Variables
;;  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (setq yas-prompt-functions '(yas-ido-prompt yas-x-prompt yas-no-prompt)))

; Commands
(defun tim/electric-semicolon ()
  (interactive)
  (let ((limit (save-excursion
                 (beginning-of-line)
                 (point))))
    (end-of-line)
    (when (not (looking-back ";" limit))
      (insert ";"))))

;; (defun tim/enable-electric-semicolon ()
;;   (local-set-key (kbd ";") #'tim/electric-semicolon))

; Functions
(defun subword-setup ()
  (subword-mode 1)
  (modeline-remove-lighter 'subword-mode))

(defun enable-which-function-mode ()
  "Conditionally enable which-function mode (skip for remote files)"
  (let ((file-name (buffer-file-name)))
    (when (or (not file-name) ; Enable for buffers that are not visiting file
              (not (file-remote-p file-name)))
      (make-local-variable 'which-function-mode)
      (which-function-mode 1))))

; Hooks
(add-hook 'prog-mode-hook #'enable-which-function-mode)
;; (add-hook 'java-mode-hook #'tim/enable-electric-semicolon)
;; (add-hook 'js2-mode-hook #'tim/enable-electric-semicolon)
(add-hook 'prog-mode-hook #'subword-setup)

; Variables
(setq-default indent-tabs-mode nil)
(show-paren-mode t)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project Management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :config

  ;; (use-package helm-projectile
  ;;   :config
  ;;   (helm-projectile-on))

  (projectile-mode 1)

  ;; Key Bindings
  (define-key custom-keys-mode-map (kbd "C-c p") projectile-command-map)

  ;; Variables
  (add-to-list 'projectile-globally-ignored-directories "bin")
  (add-to-list 'projectile-globally-ignored-directories "doxygen")
  (add-to-list 'projectile-globally-ignored-directories "include")
  (add-to-list 'projectile-globally-ignored-directories "man")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (setq projectile-cache-file "~/.emacs.d/.projectile.cache")
  ;; (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  (setq projectile-enable-idle-timer nil)
  (setq projectile-find-dir-includes-top-level t)
  (setq projectile-known-projects-file "~/.emacs.d/.projectile.projects")
  (setq projectile-mode-line
        '(:eval (format " %s[%s]"
                        (string #x1f5c0) (projectile-project-name))))
  (setq projectile-switch-project-action #'projectile-dired))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package python
;;   :commands python-mode
;;   :config

;;   (use-package helm-pydoc)

;;   (use-package jedi
;;     :config

;;     (use-package jedi-direx
;;       :commands jedi-direx:pop-to-buffer)

;;     ;; Functions
;;     (defun jedi:get-project-root ()
;;       "Use `vc-find-root' function to figure out the project root."
;;       (let* ((buf (current-buffer))
;;              (buf-dir (expand-file-name (file-name-directory (buffer-file-name (current-buffer)))))
;;              (project-root (vc-find-root buf-dir ".git")))
;;         (when project-root
;;           (expand-file-name project-root))))

;;     (defun jedi:server-args-setup ()
;;       ;; Define server args
;;       (let* ((project-root (jedi:get-project-root))
;;              (venv-root (concat project-root "venv")))
;;         (make-local-variable 'jedi:server-args)
;;         (when project-root
;;           (message (format "Adding project root to sys.path: %s" project-root))
;;           (add-args jedi:server-args "--sys-path" project-root))
;;         (when (file-exists-p venv-root)
;;           (message (format "Adding virtualenv: %s" venv-root))
;;           (add-args jedi:server-args "--virtual-env" venv-root))))

;;     ;; Hooks
;;     (add-hook 'jedi-mode-hook #'jedi-direx:setup)

;;     ;; Key Bindings
;;     (bind-keys :map jedi-mode-map
;;                ("C-(" . jedi:get-in-function-call)
;;                ("C-)" . jedi:get-in-function-call)
;;                ("C-c /" . helm-jedi-related-names)
;;                ("C-c ?" . jedi:show-doc)
;;                ("C-x D" . jedi-direx:pop-to-buffer))

;;     ;; Macros
;;     (defmacro add-args (arg-list arg-name arg-value)
;;       `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))

;;     ;; Variables
;;     (setq jedi:complete-on-dot t)
;;     (setq jedi:get-in-function-call-delay 200)
;;     (setq jedi:tooltip-method nil)
;;     (setq jedi:use-shortcuts t))

;;   (use-package pony-mode
;;     :commands pony-mode
;;     :config
;;     ;; Commands
;;     (defun pony-shell-switch-to-shell ()
;;       (interactive)
;;       (let ((pony-shell-buffer (get-buffer "*ponysh*")))
;;         (if pony-shell-buffer
;;             (pop-to-buffer pony-shell-buffer)
;;           (call-interactively #'pony-shell))))

;;     ;; Key Bindings
;;     (bind-key "C-c C-p s" #'pony-shell-switch-to-shell pony-minor-mode-map))

;;   (use-package wenote
;;     :ensure nil)

;;   ;; Functions
;;   (defun python-add-electric-pairs ()
;;     (setq-local electric-pair-pairs
;;                 (cons single-backticks electric-pair-pairs)))

;;   (defun python-enable-flycheck-mode ()
;;     "Conditionally enable flycheck-mode (skip for remote files)"
;;     (when (not (file-remote-p (buffer-file-name)))
;;       (flycheck-mode 1)))

;;   (defun python-enable-jedi ()
;;     "Conditionally enable jedi (skip for remote files)"
;;     (when (not (file-remote-p (buffer-file-name)))
;;       (jedi:setup)
;;       (jedi:server-args-setup)))

;;   ;; Hooks
;;   (add-hook 'python-mode-hook #'python-enable-flycheck-mode)
;;   (add-hook 'python-mode-hook #'python-enable-jedi)
;;   (add-hook 'python-mode-hook #'python-add-electric-pairs)

;;   ;; Key Bindings
;;   (unbind-key "C-c C-f" python-mode-map)
;;   (unbind-key "C-c C-j" python-mode-map)
;;   (unbind-key "C-c C-l" python-mode-map)
;;   (unbind-key "C-c C-p" python-mode-map)
;;   (unbind-key "C-c C-s" python-mode-map)
;;   (unbind-key "C-c C-z" python-mode-map)
;;   (bind-keys :map python-mode-map
;;              ("M-a" . python-nav-backward-block) ; Default
;;              ("M-e" . python-nav-forward-block) ; Default
;;              ("C-M-u" . python-nav-backward-up-list) ; Default
;;              ("H-a" . python-nav-beginning-of-statement)
;;              ("H-b" . python-nav-backward-statement)
;;              ("H-e" . python-nav-end-of-statement)
;;              ("H-f" . python-nav-forward-statement)
;;              ("C-c RET" . python-nav-if-name-main)
;;              ("C-c C-c" . python-shell-switch-to-shell)
;;              ("C-c C-d" . python-eldoc-at-point)
;;              ("C-c C-r" . run-python)
;;              ("C-c C-s b" . python-shell-send-buffer)
;;              ("C-c C-s d" . python-shell-send-defun)
;;              ("C-c C-s f" . python-shell-send-file)
;;              ("C-c C-s r" . python-shell-send-region)
;;              ("C-c C-s s" . python-shell-send-string))

;;   ;; Variables
;;   (setq python-environment-virtualenv (list "/usr/local/bin/virtualenv" "--system-site-packages" "--quiet"))
;;   (setq python-fill-docstring-style 'django)
;;   (setq python-shell-completion-native-enable nil))



;;;;;;;;;;;;;;;;;
;;; Scrolling ;;;
;;;;;;;;;;;;;;;;;

; Commands
(put 'scroll-left 'disabled nil)

;; ; Hydra
;; (defhydra hydra-scroll ()
;;   "Scroll"
;;   ("<" scroll-left "left")
;;   (">" scroll-right "right"))

;; ; Key Bindings
;; (global-set-key (kbd "C-x <") #'hydra-scroll/body)
;; (global-set-key (kbd "C-x >") #'hydra-scroll/body)

; Variables
(setq recenter-positions '(top middle bottom))
(setq scroll-preserve-screen-position 'always)



;;;;;;;;;;;;;;
;;; Search ;;;
;;;;;;;;;;;;;;

(use-package anzu
  :config
  (global-anzu-mode 1)

  ;; Faces
  (when (eq (car custom-enabled-themes) 'base16-hopscotch)
    (set-face-attribute 'anzu-mode-line nil :foreground "#ff7f50"))

  ;; Variables
  (setq anzu-mode-lighter ""))

;; (use-package helm-ag
;;   :config
;;   (setq helm-ag-base-command "/usr/local/bin/ag --nocolor --nogroup")
;;   (setq helm-ag-insert-at-point 'symbol))

;; (use-package helm-swoop
;;   :bind ("C-c h" . helm-swoop)
;;   :config
;;   (bind-key "M-h" #'helm-swoop-from-isearch isearch-mode-map))

;; (use-package flx-isearch
;;   :bind (("C-M-r" . flx-isearch-backward)
;;          ("C-M-s" . flx-isearch-forward)))

(use-package grep
  :ensure nil
  :bind ("C-c g" . rgrep)
  :config
  (defun rgrep-rename-buffer-after-search-string
      (orig regexp &optional files dir confirm)
    (funcall orig regexp files dir confirm)
    (with-current-buffer grep-last-buffer
      (rename-buffer (format "*grep-%s*" regexp))))

  (advice-add 'rgrep :around #'rgrep-rename-buffer-after-search-string))

(use-package rg
  :defer t
  :config
  ;; Variables
  (setq rg-executable "/opt/homebrew/bin/rg"))

;; (use-package smartscan
;;   :config
;;   (global-smartscan-mode t)
;;   (unbind-key "M-n" smartscan-map)
;;   (unbind-key "M-p" smartscan-map)
;;   (bind-keys :map smartscan-map
;;              ("s-n" . smartscan-symbol-go-forward)
;;              ("s-p" . smartscan-symbol-go-backward)))

;; Advice
(defun occur-rename-buffer-after-search-string
    (orig regexp &optional nlines region)
  (funcall orig regexp nlines region)
  (with-current-buffer "*Occur*"
    (rename-buffer (format "*Occur-%s*" regexp))))

(advice-add 'occur :around #'occur-rename-buffer-after-search-string)

(defun isearch-fake-success (orig)
  (let ((isearch-success t)
        (isearch-error nil))
    (funcall orig)))

(advice-add 'isearch-abort :around #'isearch-fake-success)

; Commands
(defun isearch-toggle-lazy-highlight-cleanup ()
  "Toggle `lazy-highlight-cleanup'.
- If `t' (ON), Isearch will *not* leave highlights around.
- If `nil' (OFF), matches will stay highlighted until the next
invocation of an Isearch command."
  (interactive)
  (setq lazy-highlight-cleanup (not lazy-highlight-cleanup))
  (message "Lazy highlight cleanup is now %s."
           (if lazy-highlight-cleanup "ON" "OFF")))

(defun isearch-hungry-delete ()
  "Delete the failed portion of the search string, or the last
char if successful."
  (interactive)
  (if (isearch-fail-pos)
      (while (isearch-fail-pos)
        (isearch-delete-char))
    (isearch-delete-char)))

;; Hooks
(add-hook 'occur-mode-hook #'next-error-follow-minor-mode)

; Key Bindings
(define-key isearch-mode-map (kbd "<backspace>") #'isearch-hungry-delete)
(define-key occur-mode-map "n" #'occur-next)
(define-key occur-mode-map "p" #'occur-prev)

; Variables
(setq isearch-allow-scroll t)



;;;;;;;;;;;;;;
;;; Server ;;;
;;;;;;;;;;;;;;

(use-package server
  :config
  (or (server-running-p)
      (server-start)))



;;;;;;;;;;;;;;;;;
;;; Utilities ;;;
;;;;;;;;;;;;;;;;;

; Advice
(defun swap-args (fun)
  (if (not (equal (interactive-form fun) '(interactive "P")))
      (error "Can only swap args if interactive spec is (interactive \"P\").")
    (advice-add
     fun
     :around
     (lambda (orig &rest args)
       "Swap default behavior with \\[universal-argument] behavior."
       (if (car args)
           (apply orig (cons nil (cdr args)))
         (apply orig (cons '(4) (cdr args))))))))

(swap-args 'quit-window)

; Functions
(defun define-search-service (name url)
  "Create command for looking up query using a specific service."
  (eval `(defun ,(intern (downcase name)) ()
           ,(format "Look up query or contents of region (if any) on %s." name)
           (interactive)
           (let ((query (if mark-active
                            (buffer-substring (region-beginning) (region-end))
                          (read-string (format "%s: " ,name)))))
             (browse-url (concat ,url query))))))

(define-search-service
  "Google" "http://www.google.com/search?ie=utf-8&oe=utf-8&q=")
;; (define-search-service
;;   "StartPage" "https://startpage.com/do/metasearch.pl?query=")
(define-search-service
  "Thesaurus" "http://thesaurus.com/browse/")
(define-search-service
  "Urbandictionary" "http://www.urbandictionary.com/define.php?term=")
(define-search-service
  "Wiktionary" "https://en.wiktionary.org/wiki/")
(define-search-service
  "Wikipedia" "https://en.wikipedia.org/wiki/")

;; ; Hydra
;; (defhydra hydra-search (:color blue)
;;   "Search"
;;   ("g" google "Google")
;;   ("s" startpage "StartPage")
;;   ("t" thesaurus "Thesaurus")
;;   ("u" urbandictionary "Urbandictionary")
;;   ("d" wiktionary "Wiktionary")
;;   ("w" wikipedia "Wikipedia"))

;; ; Key Bindings
;; (global-set-key (kbd "C-c s") #'hydra-search/body)



;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package browse-at-remote
  :commands browse-at-remote/browse browse-at-remote/kill)

;; (use-package diffview
;;   :commands diffview-current)

;; (use-package git-gutter
;;   :commands git-gutter-mode
;;   :init
;;   (add-hook 'css-mode-hook #'git-gutter-mode)
;;   (add-hook 'html-mode-hook #'git-gutter-mode)
;;   (add-hook 'org-mode-hook #'git-gutter-mode)
;;   (add-hook 'prog-mode-hook #'git-gutter-mode)
;;   :config

;;   (use-package git-gutter-fringe
;;     :config
;;     ;; Functions
;;     (defun git-gutter-fringe-change-fringe ()
;;      (if linum-mode
;;          (setq-local git-gutter-fr:side 'right-fringe)
;;        (setq-local git-gutter-fr:side 'left-fringe)))

;;     ;; Hooks
;;     (add-hook 'linum-mode-hook #'git-gutter-fringe-change-fringe))

;;   (modeline-remove-lighter 'git-gutter-mode)

;;   ;; Hydra
;;   (defhydra hydra-git-gutter (:color pink)
;;     "Git Gutter"
;;     ("SPC" git-gutter:mark-hunk "mark")
;;     ("n" git-gutter:next-hunk "next")
;;     ("p" git-gutter:previous-hunk "prev")
;;     ("d" git-gutter:popup-hunk "diff")
;;     ("s" git-gutter:stage-hunk "stage")
;;     ("r" git-gutter:revert-hunk "revert")
;;     ("m" magit-status "magit" :color blue))

;;   ;; Key Bindings
;;   (bind-key "g g" #'hydra-git-gutter/body custom-keys-mode-prefix-map))

;; (use-package git-messenger
;;   :config
;;   ;; Commands
;;   (defun git-messenger:popup-message-with-details ()
;;     (interactive)
;;     (let ((current-prefix-arg (not current-prefix-arg)))
;;       (git-messenger:popup-message)))

;;   ;; Key Bindings
;;   (bind-key "p" #'git-messenger:popup-message-with-details vc-prefix-map))

;; (use-package git-wip-timemachine
;;   :ensure nil
;;   :load-path "lisp/git-wip-timemachine"
;;   :commands git-wip-timemachine)

;; (use-package helm-github-stars
;;   :commands helm-github-stars
;;   :config
;;   (setq helm-github-stars-username "itsjeyd"))

;; (use-package helm-open-github
;;   :commands (helm-open-github-from-commit
;;              helm-open-github-from-file
;;              helm-open-github-from-issues
;;              helm-open-github-from-pull-requests)
;;   :config
;;   (defun helm-open-github-from-issues (arg)
;;     (interactive "P")
;;     (let ((host (helm-open-github--host))
;;           (url (helm-open-github--remote-url)))
;;       (when arg
;;         (remhash url helm-open-github--issues-cache))
;;       (if (not (string= host "github.com"))
;;           (helm-open-github--from-issues-direct host)
;;         (helm :sources '(helm-open-github--from-issues-source)
;;               :buffer "*open github*")))))

(use-package magit
  :commands magit-status
  :init
  (bind-keys :map custom-keys-mode-prefix-map
             ("g b" . magit-blame)
             ("g d" . magit-dispatch)
             ("g f" . magit-find-file)
             ("g s" . magit-status))
  :config

  ;; (use-package git-commit
  ;;   :config
  ;;   ;; Functions
  ;;   (defun git-commit-add-electric-pairs ()
  ;;     (setq-local electric-pair-pairs
  ;;                 (cons single-backticks electric-pair-pairs)))

  ;;   ;; Hooks
  ;;   (add-hook 'git-commit-mode-hook #'git-commit-add-electric-pairs)
  ;;   (add-hook 'git-commit-mode-hook #'turn-on-orgstruct)

  ;;   ;; Variables
  ;;   (setq git-commit-finish-query-functions nil))

  ;; (use-package magit-autorevert
  ;;   :ensure magit
  ;;   :config

  ;;   (magit-auto-revert-mode 1))

  ;; ;; Advice
  ;; (defadvice Info-follow-nearest-node (around gitman activate)
  ;;   "When encountering a cross reference to the `gitman' info
  ;;    manual, then instead of following that cross reference show
  ;;    the actual manpage using the function `man'."
  ;;   (let ((node (Info-get-token
  ;;                (point) "\\*note[ \n\t]+"
  ;;                "\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
  ;;     (if (and node (string-match "^(gitman)\\(.+\\)" node))
  ;;         (progn (require 'man)
  ;;                (man (match-string 1 node)))
  ;;       ad-do-it)))

  (advice-add 'magit-discard :after #'magit-refresh)
  (advice-add 'magit-stage :after #'magit-refresh)
  (advice-add 'magit-unstage :after #'magit-refresh)

  ;; Commands
  (defun magit-ls-files ()
    "List tracked files of current repository."
    (interactive)
    (if (derived-mode-p 'magit-mode)
        (magit-git-command "ls-files")
      (message "Not in a Magit buffer.")))

  ;; Key Bindings
  (unbind-key "M-s" magit-mode-map)
  (unbind-key "M-S" magit-mode-map)
  (bind-key "K" #'magit-ls-files magit-mode-map)

  ;; Variables
  (setq magit-completing-read-function #'magit-ido-completing-read)
  (setq magit-diff-refine-hunk t)
  (setq magit-status-show-hashes-in-headers t))

;; ; git-wip
;; (load "~/git-wip/emacs/git-wip.el")



;;;;;;;;;;;;;;;;;;
;;; Visibility ;;;
;;;;;;;;;;;;;;;;;;

; Commands
(put 'narrow-to-page 'disabled nil)
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

;; (defun hide-lines-below-current-column (orig &optional arg)
;;   "Use selective display to hide lines below current column.
;; With a prefix arg, clear selective display."
;;   (interactive "P")
;;   (if arg
;;       (funcall orig -1)
;;     (funcall orig (+ (current-column) 1))))

;; (advice-add 'set-selective-display :around #'hide-lines-below-current-column)

; Key Bindings
(global-set-key (kbd "C-x n i") #'narrow-to-region-indirect-buffer)
(define-key custom-keys-mode-prefix-map (kbd "t t") #'toggle-truncate-lines)

; Variables
(setq-default truncate-lines t)



;;;;;;;;;;;;;;
;;; Webdev ;;;
;;;;;;;;;;;;;;

;; (use-package web-beautify
;;   :commands (web-beautify-css web-beautify-html web-beautify-js))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows + Frames ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 2.0)
  (setq aw-keys (number-sequence ?a ?i))
  (setq aw-scope 'frame))

(use-package winner
  :config
  (winner-mode 1)
  (bind-key "C-c r" #'winner-redo)
  (bind-key "C-c u" #'winner-undo))

; Commands
(defun change-split (&optional arg)
  "Change arrangement of current window and `other-window' from 'stacked'
to 'side-by-side'. With a prefix arg, change arrangement from 'side-by-side'
to 'stacked'."
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

(defun split-root-window-below (&optional size)
  "Split root window vertically.
Optional argument SIZE specifies height of window that will be
added to the current window layout."
  (interactive "P")
  (split-root-window 'below size))

(defun split-root-window-right (&optional size)
  "Split root window horizontally.
Optional argument SIZE specifies width of window that will be
added to the current window layout."
  (interactive "P")
  (split-root-window 'right size))

(defun swap-windows ()
  "Call `ace-window' with a single prefix arg to swap arbitrary
window with current window."
  (interactive)
  (ace-window 4))

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

; Functions
(defun split-root-window (direction size)
  "Split root window of current frame.
DIRECTION specifies how root window will be split; possible
values are 'below and 'right. SIZE specifies height or width of
window that will be added to the current window layout."
  (split-window (frame-root-window)
                (and size (prefix-numeric-value size))
                direction))

;; ; Hydra
;; (defhydra hydra-resize-window ()
;;   "Make window(s)"
;;   ("}" enlarge-window-horizontally "wider")
;;   ("{" shrink-window-horizontally "narrower")
;;   ("^" enlarge-window "taller")
;;   ("v" shrink-window "shorter")
;;   ("+" balance-windows "balanced")
;;   ("-" shrink-window-if-larger-than-buffer "fit"))

; Key Bindings
(global-set-key (kbd "C-c 2") #'split-root-window-below)
(global-set-key (kbd "C-c 3") #'split-root-window-right)
;; (global-set-key (kbd "C-x {") #'hydra-resize-window/body)
;; (global-set-key (kbd "C-x }") #'hydra-resize-window/body)
;; (global-set-key (kbd "C-x ^") #'hydra-resize-window/body)
(define-key custom-keys-mode-prefix-map (kbd "c s") #'change-split)
(define-key custom-keys-mode-prefix-map (kbd "k o") #'kill-other-buffer-and-window)
(define-key custom-keys-mode-prefix-map (kbd "k t") #'kill-buffer-and-window)
(define-key custom-keys-mode-prefix-map (kbd "s w") #'swap-windows)
(define-key custom-keys-mode-prefix-map (kbd "t d") #'toggle-window-dedicated)

; Variables
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)



;;;;;;;;;;;;;;;
;;; Writing ;;;
;;;;;;;;;;;;;;;

;; (use-package flyspell
;;   :commands (flyspell-mode flyspell-prog-mode)
;;   :config
;;   (defun flyspell-buffer-again (&optional no-query force-save)
;;     (flyspell-buffer))

;;   (advice-add 'ispell-pdict-save :after #'flyspell-buffer-again))

(use-package markdown-mode
  :commands markdown-mode
  :config

  ;; (use-package gh-md
  ;;   :commands (gh-md-render-region gh-md-render-buffer))

  ;; ;; Functions
  ;; (defun markdown-add-electric-pairs ()
  ;;   (setq-local electric-pair-pairs
  ;;               (cons single-backticks electric-pair-pairs)))

  ;; ;; Hooks
  ;; (add-hook 'markdown-mode-hook #'markdown-add-electric-pairs)
  )

;; (use-package writeroom-mode
;;   :commands writeroom-mode)

;; ; Commands
;; (defun ispell-word-then-abbrev (local)
;;   "Call `ispell-word'. Then create an abbrev for the correction made.
;; With prefix P, create local abbrev. Otherwise it will be global."
;;   (interactive "P")
;;   (let ((before (downcase (or (thing-at-point 'word) "")))
;;         after)
;;     (call-interactively #'ispell-word)
;;     (setq after (downcase (or (thing-at-point 'word) "")))
;;     (unless (string= after before)
;;       (define-abbrev
;;         (if local local-abbrev-table global-abbrev-table) before after)
;;       (message "\"%s\" now expands to \"%s\" %sally."
;;                before after (if local "loc" "glob")))))

;; ; Key Bindings
;; (define-key custom-keys-mode-prefix-map (kbd "s a") #'ispell-word-then-abbrev)

; Variables
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
(setq-default abbrev-mode t)



;;;;;;;;;;;;
;;; YAML ;;;
;;;;;;;;;;;;

(use-package yaml-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-keys-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(modeline-remove-lighter 'auto-complete-mode)
(modeline-remove-lighter 'which-key-mode)
(modeline-set-lighter 'abbrev-mode " Abbr")
(modeline-set-lighter 'auto-fill-function (string 32 #x23ce))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(toggle-frame-maximized)
(semantic-mode)

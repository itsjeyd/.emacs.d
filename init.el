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

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/mark-lines/")



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

(setq backup-directory-alist '(("-autoloads.el\\'")
                               ("-loaddefs.el\\'")
                               ("." . "~/.emacs.d/backups")))


;;;;;;;;;;;;;;;
;;; Buffers ;;;
;;;;;;;;;;;;;;;

; Functions
(defvar temp-buffer-count 0)

(defun ibuffer-group-buffers ()
  (ibuffer-switch-to-saved-filter-groups "Default"))

(defun ibuffer-turn-on-auto-refresh ()
  (ibuffer-auto-mode 1))

(defun make-temp-buffer ()
  (interactive)
  (let ((temp-buffer-name (format "*temp-%d*" temp-buffer-count)))
    (switch-to-buffer temp-buffer-name)
    (message "New temp buffer (%s) created." temp-buffer-name))
  (setq temp-buffer-count (1+ temp-buffer-count)))

; Hooks
(add-hook 'ibuffer-mode-hook 'ibuffer-group-buffers)
(add-hook 'ibuffer-mode-hook 'ibuffer-turn-on-auto-refresh)

; Key Bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key custom-keys-mode-prefix-map (kbd "r b") 'revert-buffer)
(define-key custom-keys-mode-prefix-map (kbd "t b") 'make-temp-buffer)

; Variables
(setq confirm-nonexistent-file-or-buffer nil)
(setq revert-without-query (quote (".*")))
(setq-default ibuffer-saved-filter-groups
              '(("Default" ("Dired" (mode . dired-mode))
                           ("Org" (mode . org-mode))
                           ("Temporary" (name . "\*.*\*")))))



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

; Functions
(put 'dired-find-alternate-file 'disabled nil)

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

(defun dired-setup ()
  (dired-hide-details-mode 1)
  (dired-omit-mode 1))

; Hidden Files
(require 'dired-x)
(setq dired-omit-files "^\\...+$")

; Hooks
(add-hook 'dired-mode-hook 'dired-setup)

; Key Bindings
(define-key dired-mode-map (kbd ")") 'dired-hide-details-mode)
(define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-jump-to-top)
(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

; Openwith
(openwith-mode t)
(setq openwith-associations
      (list (list (openwith-make-extension-regexp '("pdf" "ps"))
                  "okular" '(file))
            (list (openwith-make-extension-regexp '("flac" "mp3" "wav"))
                  "gmusicbrowser" '(file))
            (list (openwith-make-extension-regexp '("avi" "flv" "mov" "mp4"
                                                    "mpeg" "mpg" "ogg" "wmv"))
                  "vlc" '(file))
            (list (openwith-make-extension-regexp '("bmp" "jpeg" "jpg" "png"))
                  "gwenview" '(file))
            (list (openwith-make-extension-regexp '("chm"))
                  "kchmviewer" '(file))
            (list (openwith-make-extension-regexp '("doc" "docx" "odt"))
                  "libreoffice" '("--writer" file))
            (list (openwith-make-extension-regexp '("ods" "xls" "xlsx"))
                  "libreoffice" '("--calc" file))
            (list (openwith-make-extension-regexp '("odp" "pps" "ppt" "pptx"))
                  "libreoffice" '("--impress" file))
            (list (openwith-make-extension-regexp '("odg"))
                  "libreoffice" '("--draw" file))
            (list (openwith-make-extension-regexp '("dia"))
                  "dia" '(file))))

; Variables
(setq dired-dwim-target t)
(setq dired-isearch-filenames "dwim")
(setq dired-listing-switches "-alh --time-style=long-iso")
(setq dired-recursive-copies (quote always))



;;;;;;;;;;;;;;;
;;; Editing ;;;
;;;;;;;;;;;;;;;

; Advice
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
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

(defun goto-beginning-of-word (arg)
  (unless (looking-back "\\b")
    (backward-word)))

(advice-add 'capitalize-word :before #'goto-beginning-of-word)
(advice-add 'downcase-word :before #'goto-beginning-of-word)
(advice-add 'upcase-word :before #'goto-beginning-of-word)

; Anchored Transpose
(define-key custom-keys-mode-prefix-map (kbd "a t") 'anchored-transpose)

; Browse Kill Ring
(define-key custom-keys-mode-prefix-map (kbd "b k") 'browse-kill-ring)

; Caps Lock
(define-key custom-keys-mode-prefix-map (kbd "c l") 'caps-lock-mode)

; Change Inner
(global-set-key (kbd "C-c i") 'change-inner)
(global-set-key (kbd "C-c o") 'change-outer)

; Electric Pair Mode
(electric-pair-mode 1)

(defvar single-backticks '(?\` . ?\`))
(defvar single-quotes '(?\' . ?\'))
(defvar org-bold-markup '(?\* . ?\*))
(defvar org-italics-markup '(?/ . ?/))
(defvar org-verbatim-markup '(?= . ?=))
(defvar org-electric-pairs
  `(,single-quotes ,org-verbatim-markup ,org-italics-markup ,org-bold-markup))

(defun git-commit-add-electric-pairs ()
  (setq-local electric-pair-pairs
              (cons single-backticks electric-pair-pairs)))

(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs
              (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'git-commit-mode-hook 'git-commit-add-electric-pairs)
(add-hook 'org-mode-hook 'org-add-electric-pairs)

; Expand Region
(define-key custom-keys-mode-prefix-map (kbd "@") 'er/expand-region)

; Functions
(autoload 'zap-up-to-char "misc")

(defun flush-empty-lines ()
  "Remove empty lines from buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^$")))

(defun kill-ring-save-with-arg (arg)
  (interactive "P")
  (if arg
      (let ((beg (line-beginning-position))
            (end (line-beginning-position (+ arg 1))))
        (kill-ring-save beg end)
        (message "Copied %d lines." arg))
    (call-interactively 'kill-ring-save)))

(defun kill-region-with-arg (arg)
  (interactive "P")
  (if arg
      (let ((beg (line-beginning-position))
            (end (line-beginning-position (+ arg 1))))
        (kill-region beg end)
        (message "Killed %d lines." arg))
    (call-interactively 'kill-region)))

(defun sort-lines-and-uniquify ()
  "Sort lines alphabetically (in ascending order) and remove duplicates."
  (interactive)
  (sort-lines nil (point-min) (point-max))
  (delete-duplicate-lines (point-min) (point-max) nil nil nil t))

(defun ucs-rightwards-arrow ()
  "Insert unicode symbol: →"
  (interactive)
  (insert-char #x2192))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and turns it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

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
(require 'iso-transl)
(global-set-key (kbd "C-w") 'kill-region-with-arg)
(global-set-key (kbd "M-w") 'kill-ring-save-with-arg)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(define-key custom-keys-mode-prefix-map (kbd "f e") 'flush-empty-lines)
(define-key custom-keys-mode-prefix-map (kbd "s u") 'sort-lines-and-uniquify)
(define-key custom-keys-mode-prefix-map (kbd "z") 'zap-to-string)
(define-key iso-transl-ctl-x-8-map (kbd "a") 'ucs-rightwards-arrow)

; Mark Lines
(require 'mark-lines)
(define-key custom-keys-mode-prefix-map (kbd "m") 'mark-lines-next-line)

; Move Text
(define-key custom-keys-mode-prefix-map (kbd "u") 'move-text-up)
(define-key custom-keys-mode-prefix-map (kbd "d") 'move-text-down)

; Multiple Cursors
(define-key custom-keys-mode-prefix-map (kbd "e l") 'mc/edit-lines)
(define-key custom-keys-mode-prefix-map (kbd "n l") 'mc/mark-next-like-this)
(define-key custom-keys-mode-prefix-map (kbd "a l") 'mc/mark-all-like-this)
(define-key custom-keys-mode-prefix-map (kbd "a d") 'mc/mark-all-dwim)
(define-key custom-keys-mode-prefix-map (kbd "r a") 'set-rectangular-region-anchor)

; Rainbow Delimiters
(add-hook 'org-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Variables
(setq cua-enable-cua-keys nil)
(setq require-final-newline t)
(setq save-interprogram-paste-before-kill t)
(setq sentence-end-double-space nil)
(setq set-mark-command-repeat-pop t)
(setq tab-width 4)

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
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

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
;;; Helm ;;;
;;;;;;;;;;;;

(require 'helm-config)
(setq helm-buffers-fuzzy-matching t)



;;;;;;;;;;;;
;;; Help ;;;
;;;;;;;;;;;;

(find-function-setup-keys)

; Guide Key
(setq guide-key/guide-key-sequence
      '("C-c" "C-x r" "C-x v" "C-x 4" "C-c p" "C-x c"))
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode t)

; Variables
(setq help-window-select t)



;;;;;;;;;;;
;;; Ido ;;;
;;;;;;;;;;;

(ido-mode (quote both))
(ido-everywhere 1)

; Flx
(flx-ido-mode 1)
(setq gc-cons-threshold 7000000)

; Functions
(defun ido-find-file-as-root ()
  "Like `ido-find-file, but automatically edit file with
root-privileges if it is not writable by user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/su:root@localhost:" file)))
    (find-file file)))

; Key Bindings
(global-set-key (kbd "C-c f") 'ido-find-file-as-root)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex-major-mode-commands)

; Ubiquitous
(ido-ubiquitous-mode 1)
(push '(disable exact "unhighlight-regexp") ido-ubiquitous-command-overrides)
(push '(disable prefix "sclang-dump-") ido-ubiquitous-command-overrides)

; Smex
(setq smex-save-file "~/.emacs.d/.smex-items")

; Variables
(add-to-list 'ido-ignore-buffers "\*Compile-Log\*")
(add-to-list 'ido-ignore-buffers "\*Messages\*")
(setq ido-create-new-buffer (quote always))
(setq ido-enable-flex-matching t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-use-filename-at-point 'guess)
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
(defun toggle-transparency ()
  (interactive)
  (let ((opacity (frame-parameter nil 'alpha)))
    (if (or (not opacity) (= opacity 100))
        (set-frame-parameter nil 'alpha 80)
      (set-frame-parameter nil 'alpha 100))))

; Hooks
(add-hook 'linum-mode-hook 'git-gutter-fringe+-change-fringe)

; Linum Relative
(require 'linum-relative)

; Rainbow
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-turn-on)

; Theme
(defun customize-enabled-theme ()
  (let ((enabled-theme (car custom-enabled-themes))
        (cursor-preferred-color "#FF5A0E"))
    (cond ((eq enabled-theme 'base16-default)
           (set-cursor-color cursor-preferred-color))
          ((eq enabled-theme 'tronesque)
           (let ((fallback-color
                  (face-attribute 'show-paren-match :background)))
             (set-face-attribute
              'dired-directory nil :foreground fallback-color)
             (set-face-attribute
              'info-header-xref nil :foreground fallback-color)))
          ((eq enabled-theme 'wombat)
           (set-cursor-color cursor-preferred-color)))))

(defun customize-theme ()
  (let ((default-background-color (face-attribute 'default :background))
        (linum-background-color (face-attribute 'linum :background)))
    (set-face-attribute 'fringe nil :background default-background-color)
    (set-face-attribute 'linum nil :background default-background-color)
    (set-face-attribute
     'linum-relative-current-face nil :background linum-background-color)))

(defadvice load-theme
  (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

(defadvice load-theme
  (after load-custom-theme-settings
         (theme &optional no-confirm no-enable)
         activate)
  (customize-theme)
  (customize-enabled-theme))

(load-theme 'sanityinc-tomorrow-eighties t)

; Tooltips
(tooltip-mode 0)

; Variables
(setq inhibit-startup-screen t)
(setq initial-scratch-message
      ";; Parentheses are just *hugs* for your function calls!\n\n")



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

(defun set-indentation-behavior ()
  (c-set-offset 'arglist-intro '+))

; Hooks
(add-hook 'java-mode-hook 'ensime-scala-mode-hook)
(add-hook 'java-mode-hook 'java-goto-class)
(add-hook 'java-mode-hook 'set-indentation-behavior)
(add-hook 'window-configuration-change-hook 'java-class-to-top)



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

; Functions
(defun configure-tex ()
  (TeX-PDF-mode t)
  (outline-minor-mode))

; Hooks
(add-hook 'LaTeX-mode-hook 'configure-tex)



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

; Variables
(setq ace-jump-mode-submode-list
      '(ace-jump-char-mode ace-jump-word-mode ace-jump-line-mode))
(setq ace-jump-word-mode-use-query-char nil)



;;;;;;;;;;;;;;;;
;;; Org Mode ;;;
;;;;;;;;;;;;;;;;

(require 'org)

; Advice
(defadvice org-display-inline-images
  (around handle-openwith
          (&optional include-linked refresh beg end) activate compile)
  (openwith-mode -1)
  ad-do-it
  (openwith-mode 1))

(defun org-add-tags (property value)
  (let* ((props (org-entry-properties))
         (unnumbered (assoc "UNNUMBERED" props))
         (tags-entry (assoc "TAGS" props))
         (tags (if tags-entry (cdr tags-entry) "")))
    (when (and unnumbered (not (string-match-p ":notoc:" tags)))
      (org-set-tags-to (concat tags "notoc")))))

(advice-add 'org-set-property :after #'org-add-tags)

(defun org-export-unnumbered (orig headline info)
  (and (funcall orig headline info)
       (not (org-element-property :UNNUMBERED headline))))

(advice-add 'org-export-numbered-headline-p :around #'org-export-unnumbered)

(defun org-remove-tags (property)
  (let* ((props (org-entry-properties))
         (unnumbered (assoc "UNNUMBERED" props))
         (tags-entry (assoc "TAGS" props))
         (tags (if tags-entry (cdr tags-entry) "")))
    (when (and (not unnumbered) (string-match-p ":notoc:" tags))
      (org-set-tags-to (replace-regexp-in-string ":notoc:" "" tags)))))

(advice-add 'org-delete-property :after #'org-remove-tags)

; Babel
(require 'ob-dot)

(idle-require 'ob-ditaa)
(eval-after-load 'ob-ditaa
  '(progn
     (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")))

(idle-require 'ob-plantuml)
(eval-after-load 'ob-plantuml
  '(progn
     (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
     (setq plantuml-jar-path "/opt/plantuml/plantuml.jar")))

(add-to-list 'org-babel-load-languages '(dot . t) t)
(add-to-list 'org-babel-load-languages '(ditaa . t) t)
(add-to-list 'org-babel-load-languages '(plantuml . t) t)

; Drill
(require 'org-drill)
(setq org-drill-scope 'directory)
(setq org-drill-hide-item-headings-p t)

; Emphasis
(setcar org-emphasis-regexp-components " \t('\"`{-")
(setcar (nthcdr 1 org-emphasis-regexp-components) "\[[:alpha:]- \t.,:!?;'\")}\\")
(setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n\"'")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

; Exports
(idle-require 'ox-md)

; Functions
(defun org-point-in-speed-command-position-p ()
  (or (looking-at org-outline-regexp)
      (looking-at "^#\+")
      (looking-at "^[[:blank:]]\\{2,\\}")
      (looking-at "^$")))

(defun org-back-to-item ()
  (interactive)
  (re-search-backward "^ *[-+*]\\|^ *[1-9]+[)\.] " nil nil 1))

(defun org-export-all ()
  "Export all subtrees that are *not* tagged with :noexport:
or :subtree: to separate files.

Note that subtrees must have the :EXPORT_FILE_NAME: property set
to a unique value for this to work properly."
  (interactive)
  (org-map-entries (lambda () (org-html-export-to-html nil t))
                   "-noexport-subtree"))

(defun org-fill-paragraph-handle-lists (&optional num-paragraphs)
  (interactive "p")
  (save-excursion
    (let ((bound (if mark-active
                     (- (region-end) 2)
                   (progn
                     (org-back-to-item)
                     (while (>= num-paragraphs 0)
                       (call-interactively 'org-mark-element)
                       (setq num-paragraphs (1- num-paragraphs)))
                     (- (region-end) 2)))))
      (while (search-forward "\n" bound t)
        (replace-match " ")))
    (org-fill-paragraph)))

(defvar org-generic-drawer-regexp "^ +:[[:alpha:]]+:")

(defun org-next-drawer (arg)
  (interactive "p")
  (org-next-block arg nil org-generic-drawer-regexp))

(defun org-previous-drawer (arg)
  (interactive "p")
  (org-previous-block arg org-generic-drawer-regexp))

(fset 'org-wrap-in-comment-block
   [?\C-o tab ?< ?o tab ?\C-w ?\C-w ?\C-u ?\C-x ?q ?\C-y ?\C-p ?\C-p ?\C-w ?\C-e ?\C-f])

; Hooks
(add-hook 'org-mode-hook 'org-hide-block-all)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

; Key Bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "RET") 'org-return-indent)
(define-key org-mode-map (kbd "<C-tab>") 'pcomplete)
(define-key org-mode-map (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-c c") 'org-wrap-in-comment-block)
(define-key org-mode-map (kbd "C-c d") 'org-toggle-link-display)
(define-key org-mode-map (kbd "C-M-q") 'org-fill-paragraph-handle-lists)
(define-key org-mode-map (kbd "M-n") 'org-next-item)
(define-key org-mode-map (kbd "M-p") 'org-previous-item)
(define-key org-mode-map (kbd "M-s TAB") 'org-force-cycle-archived)
(define-key org-mode-map (kbd "M-s t h") 'org-insert-todo-heading)
(define-key org-mode-map (kbd "M-s t s") 'org-insert-todo-subheading)
(define-key org-mode-map (kbd "s-d") 'org-shiftdown)
(define-key org-mode-map (kbd "s-l") 'org-shiftleft)
(define-key org-mode-map (kbd "s-r") 'org-shiftright)
(define-key org-mode-map (kbd "s-u") 'org-shiftup)

; Variables
(setq org-agenda-include-diary t)
(setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
(setq org-catch-invisible-edits 'error)
(setq org-completion-use-ido t)
(setq org-confirm-babel-evaluate nil)
(setq org-cycle-include-plain-lists 'integrate)
(setq org-export-dispatch-use-expert-ui t)
(setq org-enforce-todo-dependencies t)
(setq org-export-copy-to-kill-ring nil)
(setq org-footnote-define-inline t)
(setq org-footnote-auto-label 'random)
(setq org-latex-table-caption-above nil)
(setq org-list-allow-alphabetical t)
(setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "-")))
(setq org-list-use-circular-motion t)
(setq org-M-RET-may-split-line
      '((headline . nil) (item . t) (table . t)))
(setq org-return-follows-link t)
(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-src-fontify-natively t)
(setq org-track-ordered-property-with-tag t)
(setq org-use-extra-keys t)
(setq org-use-speed-commands 'org-point-in-speed-command-position-p)
(add-to-list 'org-speed-commands-user '("d" . org-next-drawer) t)
(add-to-list 'org-speed-commands-user '("P" . org-previous-drawer) t)
(add-to-list 'org-structure-template-alist
             '("o" "#+BEGIN_COMMENT\n?\n#+END_COMMENT") t)



;;;;;;;;;;;;;;;;
;;; Overtone ;;;
;;;;;;;;;;;;;;;;

(require 'cider)

; Hooks
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

; Variables
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-use-pretty-printing t)
(setq nrepl-buffer-name-show-port t)



;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Manager ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

; Paradox
(setq paradox-automatically-star nil)
(setq paradox-execute-asynchronously nil)



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
(require 'org-ac)
(org-ac/config-default)
(require 'ac-cider)
(add-to-list 'ac-modes 'cider-mode)

(defadvice ac-quick-help
    (around turn-off-line-truncation (&optional force) activate compile)
  (toggle-truncate-lines -1)
  ad-do-it
  (toggle-truncate-lines 1))

(define-key ac-completing-map (kbd "C-h") 'ac-help)
(define-key ac-completing-map (kbd "C-v") 'ac-quick-help-scroll-down)
(define-key ac-completing-map (kbd "M-v") 'ac-quick-help-scroll-up)
(define-key ac-menu-map (kbd "C-f") 'ac-stop)

(setq ac-auto-show-menu 0.3)
(setq ac-comphist-file "~/.emacs.d/.ac-comphist.dat")
(setq ac-ignore-case nil)
(setq ac-quick-help-delay 1.0)
(setq ac-use-menu-map t)
(add-to-list 'ac-sources 'ac-source-yasnippet)

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
(which-function-mode 1)

; yasnippet
(yas-global-mode 1)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project Management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Projectile
(projectile-global-mode 1)
(add-to-list 'projectile-globally-ignored-directories "doxygen")
(setq projectile-enable-caching t)
(setq projectile-known-projects-file "~/.emacs.d/.projectile-bookmarks.eld")
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

; Functions
(defun python-setup ()
  (unless (eq buffer-file-name nil)
    (flymake-mode t)))

; Hooks
(add-hook 'python-mode-hook 'python-setup)

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

; Functions
(defadvice recentf-keep-default-predicate
    (around recentf-discard-autoloads (file) activate compile)
  (if (not (string-match-p "-autoloads" (file-name-nondirectory file)))
      ad-do-it
    nil))

(defadvice recentf-track-opened-file (around set-buffer-file-name activate compile)
  (if (eq major-mode 'dired-mode)
      (progn (setq buffer-file-name default-directory)
             ad-do-it
             (setq buffer-file-name nil))
    ad-do-it))

(defadvice recentf-track-closed-file (around set-buffer-file-name activate compile)
  (if (eq major-mode 'dired-mode)
      (progn (setq buffer-file-name default-directory)
             ad-do-it
             (setq buffer-file-name nil))
    ad-do-it))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

; Key Bindings
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

; Variables
(add-to-list 'recentf-used-hooks '(dired-after-readin-hook recentf-track-opened-file))
(setq recentf-max-saved-items 150)
(setq recentf-save-file "~/.emacs.d/.recentf")

; Launch
(recentf-mode t)



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
(setq recenter-positions '(top middle bottom))
(setq scroll-preserve-screen-position 1)



;;;;;;;;;;;;;;
;;; Search ;;;
;;;;;;;;;;;;;;

; Functions
(defadvice occur (around occur-rename-buffer-after-search-string
                         (regexp &optional nlines)
                         activate compile)
  ad-do-it
  (with-current-buffer "*Occur*"
    (rename-buffer (format "*Occur-%s*" regexp))))

(defadvice rgrep (around rgrep-rename-buffer-after-search-string
                         (regexp &optional files dir confirm)
                         activate compile)
  ad-do-it
  (with-current-buffer grep-last-buffer
    (rename-buffer (format "*grep-%s*" regexp))))

; Hooks
(add-hook 'occur-mode-hook 'next-error-follow-minor-mode)

; Key Bindings
(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)

; Loccur
(require 'loccur)
(define-key custom-keys-mode-prefix-map (kbd "l o") 'loccur-current)

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
(defun magit-log-all ()
  (interactive)
  (magit-key-mode-popup-logging)
  (magit-key-mode-toggle-option (quote logging) "--all"))

(defun magit-ls-files ()
  "List tracked files of current repository."
  (interactive)
  (if (derived-mode-p 'magit-mode)
      (magit-git-command "ls-files" default-directory)
    (message "Not in a Magit buffer.")))

; git-wip
(load "~/git-wip/emacs/git-wip.el")
(require 'git-wip-timemachine)

; Git Gutter
(require 'git-gutter-fringe+)

(defun set-up-git-gutter+ ()
  (setq-local git-gutter-fr+-side 'left-fringe)
  (local-set-key (kbd "M-s n h") 'git-gutter+-next-hunk)
  (local-set-key (kbd "M-s p h") 'git-gutter+-previous-hunk)
  (local-set-key (kbd "M-s s d") 'git-gutter+-show-hunk)
  (local-set-key (kbd "M-s s h") 'git-gutter+-stage-hunks)
  (local-set-key (kbd "M-s r h") 'git-gutter+-revert-hunks))

(defun git-gutter-fringe+-change-fringe ()
  (if linum-mode
      (setq-local git-gutter-fr+-side 'right-fringe)
    (setq-local git-gutter-fr+-side 'left-fringe))
  (git-gutter+-refresh))

(add-hook 'git-gutter+-mode-hook 'set-up-git-gutter+)
(add-hook 'magit-revert-buffer-hook 'git-gutter+-refresh)

; Hooks
(add-hook 'css-mode-hook 'git-gutter+-mode)
(add-hook 'html-mode-hook 'git-gutter+-mode)
(add-hook 'org-mode-hook 'git-gutter+-mode)
(add-hook 'prog-mode-hook 'git-gutter+-mode)
(add-hook 'git-commit-mode-hook 'turn-on-orgstruct)
(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)

; Key Bindings
(define-key custom-keys-mode-prefix-map (kbd "g s") 'magit-status)
(define-key magit-mode-map (kbd "M-s") nil)
(define-key magit-mode-map (kbd "M-S") nil)
(define-key magit-mode-map (kbd "K") 'magit-ls-files)
(define-key magit-mode-map (kbd "l") 'magit-log-all)

; Variables
(setq magit-diff-refine-hunk t)
(setq magit-auto-revert-mode-lighter "")
(setq magit-use-overlays nil)



;;;;;;;;;;;;;;;;;;
;;; Visibility ;;;
;;;;;;;;;;;;;;;;;;

; Functions
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

(defun tim/set-selective-display (&optional arg)
  "Use selective display to hide lines below current column.
With a prefix arg, clear selective display."
  (interactive "P")
  (if arg
      (set-selective-display -1)
    (set-selective-display (+ (current-column) 1))))

; Key Bindings
(global-set-key (kbd "C-x n i") 'narrow-to-region-indirect-buffer)
(global-set-key (kbd "C-x $") 'tim/set-selective-display)
(define-key custom-keys-mode-prefix-map (kbd "t t") 'toggle-truncate-lines)

; Variables
(setq-default truncate-lines t)



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows + Frames ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

; Ace Window
(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i))
(setq aw-scope 'frame)

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
(define-key custom-keys-mode-prefix-map (kbd "c s") 'change-split)
(define-key custom-keys-mode-prefix-map (kbd "k o") 'kill-other-buffer-and-window)
(define-key custom-keys-mode-prefix-map (kbd "s w") 'swap-windows)
(define-key custom-keys-mode-prefix-map (kbd "t d") 'toggle-window-dedicated)

; Modes
(winner-mode 1)
(global-set-key (kbd "C-c r") 'winner-redo)
(global-set-key (kbd "C-c u") 'winner-undo)

; Variables
(setq ediff-split-window-function 'split-window-horizontally)



;;;;;;;;;;;;;;;
;;; Writing ;;;
;;;;;;;;;;;;;;;

; Functions
(defadvice ispell-pdict-save
  (after flyspell-buffer-again (&optional no-query force-save)
         activate compile)
  (flyspell-buffer))

(defun ispell-word-then-abbrev (local)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((before (downcase (or (thing-at-point 'word) "")))
        after)
    (call-interactively 'ispell-word)
    (setq after (downcase (or (thing-at-point 'word) "")))
    (unless (string= after before)
      (define-abbrev
        (if local local-abbrev-table global-abbrev-table) before after))
      (message "\"%s\" now expands to \"%s\" %sally."
               before after (if local "loc" "glob"))))

; Key Bindings
(define-key custom-keys-mode-prefix-map (kbd "i a") 'ispell-word-then-abbrev)

; Variables
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
(setq save-abbrevs t)
(setq-default abbrev-mode t)

; Writeroom
(defun turn-off-git-gutter+ ()
  (if (not git-gutter+-mode)
      (git-gutter+-mode t)
    (git-gutter+-mode -1)))

(add-hook 'writeroom-mode-hook 'turn-off-git-gutter+)



;;;;;;;;;;;;;
;;; Ztree ;;;
;;;;;;;;;;;;;

(idle-require 'ztree-dir)
(eval-after-load 'ztree-dir
  '(progn
     (define-key ztree-mode-map (kbd "n") 'next-line)
     (define-key ztree-mode-map (kbd "p") 'previous-line)))

; Key Bindings
(global-set-key (kbd "C-x C-d") 'ztree-dir)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(idle-require-mode 1)
(custom-keys-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(modeline-remove-lighter 'auto-complete-mode)
(modeline-remove-lighter 'git-gutter+-mode)
(modeline-remove-lighter 'guide-key-mode)
(modeline-remove-lighter 'hs-minor-mode)
(modeline-remove-lighter 'whitespace-mode)
(modeline-remove-lighter 'yas-minor-mode)
(modeline-set-lighter 'abbrev-mode " Abbr")
(modeline-set-lighter 'auto-fill-function (string 32 #x23ce))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(split-window-horizontally)
(toggle-frame-maximized)

(package-initialize)
(load-theme 'wombat t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
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
;;; Buffers ;;;
;;;;;;;;;;;;;;;

; Key Bindings
(global-set-key (kbd "M-s r b") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
(setq dired-recursive-copies (quote always))


;;;;;;;;;;;;;;;
;;; Editing ;;;
;;;;;;;;;;;;;;;

; Ace Jump
(global-set-key (kbd "s-SPC") 'ace-jump-mode)

; Browse Kill Ring
(require 'browse-kill-ring)
(global-set-key (kbd "M-s b k") 'browse-kill-ring)

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

(define-key occur-mode-map "n" 'occur-next)
(define-key occur-mode-map "p" 'occur-prev)

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

; Smartscan
(global-smartscan-mode t)

; Variables
(setq sentence-end-double-space nil)
(setq set-mark-command-repeat-pop t)
(setq tab-width 4)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


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

(setq inhibit-startup-screen t)
(set-scroll-bar-mode nil)
(tool-bar-mode 0)
(tooltip-mode 0)
(set-cursor-color "#FF5A0E")


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

; Hooks
(add-hook 'java-mode-hook 'ensime-scala-mode-hook)
(add-hook 'java-mode-hook (lambda () (subword-mode 1)))

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
(setq-default TeX-master nil)

; BibTeX
(setq bibtex-maintain-sorted-entries t)


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

(require 'org)

; Hooks
(add-hook 'org-mode-hook 'turn-on-auto-fill)

; Key Bindings
(define-key org-mode-map (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "M-s t l") 'org-toggle-link-display)

; Variables
(setq org-agenda-include-diary t)
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-use-speed-commands t)
(setq org-structure-template-alist
      (quote (("s" "#+BEGIN_SRC ?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>")
              ("e" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE" "<example>\n?\n</example>")
              ("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE" "<quote>\n?\n</quote>")
              ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
              ("V" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM" "<verbatim>\n?\n</verbatim>")
              ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER" "<center>\n?\n</center>")
              ("l" "#+BEGIN_LaTeX\n?\n#+END_LaTeX" "<literal style=\"latex\">\n?\n</literal>")
              ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
              ("h" "#+BEGIN_HTML\n?\n#+END_HTML" "<literal style=\"html\">\n?\n</literal>")
              ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
              ("a" "#+BEGIN_ASCII\n?\n#+END_ASCII")
              ("A" "#+ASCII: ")
              ("i" "#+INDEX: ?" "#+INDEX: ?")
              ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">")
              ("o" "#+BEGIN_COMMENT\n?\n#+END_COMMENT"))))


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

; Indentation
(setq-default indent-tabs-mode nil)

; Parens
(show-paren-mode t)

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
(setq recentf-max-saved-items 100)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scala Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; Ensime
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;;;;;;;;;;;;;;;;;
;;; Scrolling ;;;
;;;;;;;;;;;;;;;;;

; Functions
(put 'scroll-left 'disabled nil)

; Variables
(setq scroll-preserve-screen-position 1)


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

; Key Bindings
(global-set-key (kbd "M-s g s") 'magit-status)
(global-set-key (kbd "M-s n h") 'git-gutter:next-hunk)
(global-set-key (kbd "M-s p h") 'git-gutter:previous-hunk)
(global-set-key (kbd "M-s s h") 'git-gutter:stage-hunk)
(global-set-key (kbd "M-s r h") 'git-gutter:revert-hunk)


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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(desktop-save-mode t)
 '(tags-revert-without-query t))
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
  (when (and buffer-file-name (string-match "\\.emacs" buffer-file-name))
    (let ((byte-file (concat buffer-file-name "\\.elc")))
      (if (or (not (file-exists-p byte-file))
              (file-newer-than-file-p buffer-file-name byte-file))
          (byte-compile-file buffer-file-name)))))

(add-hook 'after-save-hook 'auto-recompile-emacs-file)


;;;;;;;;;;;;;;;;;;;
;;; Color Theme ;;;
;;;;;;;;;;;;;;;;;;;
(require 'color-theme)

; Get rid of "Wrong type argument" error thrown by <M-x
; color-theme-select RET> (NOTE that if color-theme-zenburn is
; selected at startup, the error persists!):
    (defun color-theme-face-attr-construct (face frame)
       (if (atom face)
           (custom-face-attributes-get face frame)
         (if (and (consp face) (eq (car face) 'quote))
             (custom-face-attributes-get (cadr face) frame)
           (custom-face-attributes-get (car face) frame))))

; Add a couple more themes
(load "color-theme-colorful-obsolescence")
(load "color-theme-zenburn")

; Set up list of favourite themes to cycle through
(setq my-color-themes (list
  'color-theme-colorful-obsolescence
  'color-theme-zenburn
  'color-theme-charcoal-black
  'color-theme-comidia
  'color-theme-late-night
))
(defun my-theme-set-default ()
      (interactive)
      (setq theme-current my-color-themes)
      (funcall (car theme-current)))
    (defun my-describe-theme ()
      (interactive)
      (message "%s" (car theme-current)))
    (defun my-theme-cycle ()
      (interactive)
      (setq theme-current (cdr theme-current))
      (if (null theme-current)
      (setq theme-current my-color-themes))
      (funcall (car theme-current))
      (message "%S" (car theme-current)))
    (setq theme-current my-color-themes)
    (my-theme-set-default)
    (global-set-key [f8] 'my-theme-cycle)


;;;;;;;;;;;;;
;;; Dired ;;;
;;;;;;;;;;;;;

; File Associations
(require 'openwith)
(openwith-mode t)
(setq openwith-associations (quote (("\\.\\(?:pdf\\|ps\\)\\'" "evince" (file))
                                    ("\\.djvu\\'" "djview4" (file))
                                    ("\\.\\(?:mp3\\|wav\\|flac\\)\\'" "clementine" (file))
                                    ("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\|mov\\|mp4\\)\\'" "vlc" (file))
                                    ("\\.\\(?:jpe?g\\|png\\|bmp\\)\\'" "viewnior" (file))
                                    ("\\.chm\\'" "chmsee" (file))
                                    ("\\.\\(?:odt\\|doc\\|docx\\)\\'" "ooffice" ("-writer" file))
                                    ("\\.\\(?:ods\\|xls\\|xlsx\\)\\'" "ooffice" ("-calc" file))
                                    ("\\.\\(?:odp\\|pps\\|ppt\\|pptx\\)\\'" "ooffice" ("-impress" file))
                                    ("\\.odb\\'" "ooffice" ("-base" file)))))

; Hidden Files
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

; Variables
(setq dired-recursive-copies (quote always))


;;;;;;;;;;;;;;;
;;; Editing ;;;
;;;;;;;;;;;;;;;

; Functions
(put 'narrow-to-region 'disabled nil)

; Key Bindings
(global-set-key (kbd "M-g c") 'goto-char)
(global-unset-key (kbd "M-g g"))
(global-unset-key (kbd "M-g M-g"))
(global-set-key (kbd "M-g l") 'goto-line)
(global-set-key (kbd "M-s t t") 'toggle-truncate-lines)

; Mark Lines
(require 'mark-lines)
(global-set-key (kbd "M-s m") 'mark-lines-next-line)

; Move Text
(require 'move-text)
(global-set-key (kbd "M-s u") 'move-text-up)
(global-set-key (kbd "M-s d") 'move-text-down)

; Variables
(setq indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(setq set-mark-command-repeat-pop t)
(setq tab-width 4)
(setq x-select-enable-clipboard t)

; Wrap Region
(add-to-list 'load-path "~/.emacs.d/wrap-region")
(require 'wrap-region)
(wrap-region-add-wrapper "*" "*")
(wrap-region-add-wrapper "/" "/")
(wrap-region-add-wrapper "=" "=")


;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Goodies ;;;
;;;;;;;;;;;;;;;;;;;;;

; Browse Kill Ring
(require 'browse-kill-ring)

; Minibuffer Completion Cycling
(require 'minibuffer-complete-cycle)
(setq minibuffer-complete-cycle t)

; Shell Command Completion
(require 'shell-command)
(shell-command-completion-mode)


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

; Functions
(defun mx-ido()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (all-completions "" obarray 'commandp)))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

; Key Bindings
(global-set-key (kbd "M-x") 'mx-ido)
(global-set-key (kbd "<menu>") 'mx-ido)

; Ubiquitous
(add-to-list 'load-path "~/.emacs.d/ido-ubiquitous/")
(require 'ido-ubiquitous)
(ido-ubiquitous)

; Variables
(setq ido-create-new-buffer (quote always))
(setq ido-enable-flex-matching t)
(ido-mode (quote both))


;;;;;;;;;;;;;;;;;
;;; Interface ;;;
;;;;;;;;;;;;;;;;;
(scroll-bar-mode nil)
(tool-bar-mode nil)
(setq tooltip-use-echo-area t)


;;;;;;;;;;;
;;; LKB ;;;
;;;;;;;;;;;
   (let ((root (getenv "DELPHINHOME")))
     (if (file-exists-p (format "%s/lkb/etc/dot.emacs" root))
       (load (format "%s/lkb/etc/dot.emacs" root) nil t t)))


;;;;;;;;;;;;;
;;; LaTeX ;;;
;;;;;;;;;;;;;

; Variables
(setq TeX-command-list (quote (("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t"
                                TeX-run-TeX
                                nil
                                (plain-tex-mode texinfo-mode ams-tex-mode)
                                :help "Run plain TeX")
                               ("LaTeX" "%`%l%(mode)%' %t"
                                TeX-run-TeX
                                nil
                                (latex-mode doctex-mode)
                                :help "Run LaTeX")
                               ("Makeinfo" "makeinfo %t"
                                TeX-run-compile
                                nil
                                (texinfo-mode)
                                :help "Run Makeinfo with Info output")
                               ("Makeinfo HTML" "makeinfo --html %t"
                                TeX-run-compile
                                nil
                                (texinfo-mode)
                                :help "Run Makeinfo with HTML output")
                               ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t"
                                TeX-run-TeX
                                nil
                                (ams-tex-mode)
                                :help "Run AMSTeX")
                               ("ConTeXt" "texexec --once --texutil %(execopts)%t"
                                TeX-run-TeX
                                nil
                                (context-mode)
                                :help "Run ConTeXt once")
                               ("ConTeXt Full" "texexec %(execopts)%t"
                                TeX-run-TeX
                                nil
                                (context-mode)
                                :help "Run ConTeXt until completion")
                               ("BibTeX" "bibtex %s"
                                TeX-run-BibTeX
                                nil
                                t
                                :help "Run BibTeX")
                               ("View" "%V"
                                TeX-run-discard
                                t
                                t
                                :help "Run Viewer")
                               ("Print" "%p"
                                TeX-run-command
                                t
                                t
                                :help "Print the file")
                               ("Queue" "%q"
                                TeX-run-background
                                nil
                                t
                                :help "View the printer queue"
                                :visible TeX-queue-command)
                               ("File" "%(o?)dvips %d -o %f "
                                TeX-run-command
                                t
                                t
                                :help "Generate PostScript file")
                               ("Index" "makeindex %s"
                                TeX-run-command
                                nil
                                t
                                :help "Create index file")
                               ("Check" "lacheck %s"
                                TeX-run-compile
                                nil
                                (latex-mode)
                                :help "Check LaTeX file for correctness")
                               ("Spell" "(TeX-ispell-document \"\")"
                                TeX-run-function
                                nil
                                t
                                :help "Spell-check the document")
                               ("Clean" "TeX-clean"
                                TeX-run-function
                                nil
                                t
                                :help "Delete generated intermediate files")
                               ("Clean All" "(TeX-clean t)"
                                TeX-run-function
                                nil
                                t
                                :help "Delete generated intermediate and output files")
                               ("Other" ""
                                TeX-run-command
                                t
                                t
                                :help "Run an arbitrary command")
                               ("XeLaTeX" "%`xelatex%(mode)%' %t"
                                TeX-run-TeX
                                nil
                                t))))
(setq TeX-electric-sub-and-superscript t)


;;;;;;;;;;;;;;;;;;;;;;;
;;; Library Loading ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/idle-require/")
(require 'idle-require)
(idle-require-mode t)
(setq idle-require-idle-delay "10")
(setq idle-require-load-break "0.5")


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
(add-hook 'org-mode-hook 'autopair-mode)
(add-hook 'org-mode-hook 'linum-mode)
(add-hook 'org-mode-hook 'show-paren-mode)

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
(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.mm\\'" . default)
                            ("\\.x?html?\\'" . default)
                            ("\\.pdf\\'" . default)
                            ("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\|mov\\|mp4\\)\\'" . "vlc %s")
                            ("\\.\\(?:jpe?g\\|png\\|bmp\\)\\'" . "viewnior %s")
                            ("\\.\\(?:mp3|wav\\|flac\\)\\'" . "clementine %s")
                            ("\\.chm\\'" . default) ("\\.ps\\'" . default)
                            ("\\.\\(?:odt\\|doc\\|docx\\)\\'" . default)
                            ("\\.\\(?:ods\\|xls\\|xlsx\\)\\'" . default)
                            ("\\.\\(?:odp\\|pps\\|ppt\\|pptx\\)\\'" . default)
                            ("\\.odb\\'" . default))))
(setq org-track-ordered-property-with-tag t)
(setq org-use-speed-commands t)


;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Manager ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("marmalade"
. "http://marmalade-repo.org/packages/"))


;;;;;;;;;;;;;;;;;;;
;;; Programming ;;;
;;;;;;;;;;;;;;;;;;;

; Autopair
(require 'autopair)
(add-hook 'emacs-lisp-mode-hook 'autopair-mode)
(add-hook 'lisp-mode-hook 'autopair-mode)
(add-hook 'octave-mode-hook 'autopair-mode)

; Parens
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'lisp-mode-hook 'show-paren-mode)
(add-hook 'octave-mode-hook 'show-paren-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python Development ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Anything
;; (require 'anything)
;; (require 'anything-ipython)
;; (add-hook 'python-mode-hook #'(lambda ()
;;                                 (define-key py-mode-map (kbd "C-c c") 'anything-ipython-complete)))
;; (add-hook 'py-shell-hook #'(lambda ()
;;                              (define-key py-shell-map (kbd "C-c c") 'anything-ipython-complete)))
;; (when (require 'anything-show-completion nil t)
;;   (use-anything-show-completion 'anything-ipython-complete
;;                                 '(length initial-pattern)))
;; (require 'anything-config)

; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(define-key ac-complete-mode-map [tab] 'ac-expand)

; Autopair
(add-hook 'python-mode-hook 'autopair-mode)

; IPython
(require 'ipython)
(setq py-python-command-args nil)

; Macros
(fset 'python-hide-class-body
   "\261\C-x$")
(global-set-key (kbd "C-c s d c") 'python-hide-class-body) ; "Selective Display: Classes"

(fset 'python-hide-function-bodies
   "\265\C-x$")
(global-set-key (kbd "C-c s d f") 'python-hide-function-bodies) ; "Selective Display: Functions"

(fset 'python-show-all
   "\C-x$")
(global-set-key (kbd "C-c s d a") 'python-show-all) ; "Selective Display: All"

; Parens
(add-hook 'python-mode-hook 'show-paren-mode)

; Pylint
(require 'python-pylint)

; Pylookup
(setq pylookup-dir "~/.emacs.d/pylookup")
(add-to-list 'load-path pylookup-dir)

(eval-when-compile (require 'pylookup))

(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML index." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(global-set-key (kbd "C-c h") 'pylookup-lookup)

; Ropemacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

; Subword Mode
(add-hook 'python-mode-hook 'subword-mode)

; Virtualenv (https://github.com/aculich/virtualenv.el)
(add-to-list 'load-path "~/.emacs.d/virtualenv.el/")
(load "virtualenv")

; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; YaSnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "/usr/share/emacs/site-lisp/yasnippet/snippets/")


;;;;;;;;;;;;;;;
;;; Recentf ;;;
;;;;;;;;;;;;;;;
(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)


;;;;;;;;;;;;;;;;;
;;; Scrolling ;;;
;;;;;;;;;;;;;;;;;
(setq hscroll-margin 1)
(setq scroll-margin 1)
(setq scroll-preserve-screen-position t)
(setq scroll-step 1)


;;;;;;;;;;;;;;
;;; Server ;;;
;;;;;;;;;;;;;;
(server-start)


;;;;;;;;;;;;;;;;;
;;; Utilities ;;;
;;;;;;;;;;;;;;;;;

; Functions
(defun count-words-buffer ()
  "Count the number of words in the current buffer;
echo the result to the minibuffer"
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "Buffer contains %d words. That's a lot of words!" count))))

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

; Packages
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "bk" 'browse-kill-ring)


;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control ;;;
;;;;;;;;;;;;;;;;;;;;;;;

; Packages
(require 'magit)
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

; Swapping windows
(defun swap-windows ()
 "If you have 2 windows, this function swaps them."
 (interactive)
 (cond ((not (= (count-windows) 2))
        (message "You need exactly 2 windows to do this."))
       (t
        (let* ((w1 (first (window-list)))
               (w2 (second (window-list)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)))))

(global-set-key (kbd "M-s s w") 'swap-windows)

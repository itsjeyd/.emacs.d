;;;;;;;;;;;;;;
;;; Thesis ;;;
;;;;;;;;;;;;;;

(idle-require 'ox-odt)

(defun org-export-all ()
  "Export all subtrees that are *not* tagged with :noexport:
or :subtree: to separate files.

Note that subtrees must have the :EXPORT_FILE_NAME: property set
to a unique value for this to work properly."
  (interactive)
  (org-map-entries (lambda () (org-html-export-to-html nil t))
                   "-noexport-subtree"))

(defvar org-custom-drawer-regexp
  "^ +:\\(ALTERNATIVES\\|NOTES\\|PROPOSAL\\|FEEDBACK\\):")

(defun org-next-drawer (arg)
  (interactive "p")
  (org-next-block arg nil org-custom-drawer-regexp))

(defun org-previous-drawer (arg)
  (interactive "p")
  (org-previous-block arg org-custom-drawer-regexp))

(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("scrbook" "\\documentclass[11pt]{scrbook}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-speed-commands-user
             '("d" . org-next-drawer) t)
(add-to-list 'org-speed-commands-user
             '("P" . org-previous-drawer) t)
(setq org-confirm-babel-evaluate nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-html-postamble nil)
(setq org-html-table-default-attributes nil)
(setq org-link-search-must-match-exact-headline t)
(add-hook 'org-mode-hook 'org-hide-block-all)

(defun insert-spaces ()
  (interactive)
  (while (re-search-forward "\\(About\\|ChildName\\|ChildGender\\|CurrentGame\\|Encounter\\|Familiarity\\|GameQuiz\\|SpeechAct\\)" nil t)
    (replace-match "\\1 ")))

(fset 'format-lhs
   [?\C-n ?\C-  ?\C-c ?\C-n ?\C-p ?\C-c ?| ?\M-h ?\C-x ?\C-x ?\C-p tab ?\C-c ?\C-n])

(global-set-key (kbd "M-s i s") 'insert-spaces)
(global-set-key (kbd "M-s f l") 'format-lhs)

(require 'bibtex)
(bibtex-set-dialect)

(require 'org-bibtex)

(require 'ox-bibtex)
(setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))


;;;;;;;;;;;;;;;;;;
;;; Colloquium ;;;
;;;;;;;;;;;;;;;;;;

(require 'ox-beamer)

(defun org-export-unnumbered (orig headline info)
  (and (funcall orig headline info)
       (not (org-element-property :UNNUMBERED headline))))

(advice-add 'org-export-numbered-headline-p :around #'org-export-unnumbered)

(defun org-add-tags (property value)
  (let* ((props (org-entry-properties))
         (unnumbered (assoc "UNNUMBERED" props))
         (tags-entry (assoc "TAGS" props))
         (tags (if tags-entry (cdr tags-entry) "")))
    (when (and unnumbered (not (string-match-p ":notoc:" tags)))
      (org-set-tags-to (concat tags "notoc")))))

(advice-add 'org-set-property :after #'org-add-tags)

(defun org-remove-tags (property)
  (let* ((props (org-entry-properties))
         (unnumbered (assoc "UNNUMBERED" props))
         (tags-entry (assoc "TAGS" props))
         (tags (if tags-entry (cdr tags-entry) "")))
    (when (and (not unnumbered) (string-match-p ":notoc:" tags))
      (org-set-tags-to (replace-regexp-in-string ":notoc:" "" tags)))))

(advice-add 'org-delete-property :after #'org-remove-tags)

;;; java.el --- Customizations for Java development.

(use-package cc-mode
  :commands java-mode
  :config

  ;; Commands
  (defun java-goto-class ()
    (interactive)
    (goto-char (point-min))
    (search-forward "class")
    (beginning-of-line)
    (recenter-top-bottom 0))

  ;; Functions
  (defun java-class-to-top ()
    (if (and (eq major-mode 'java-mode)
             (looking-at "^public\\|private\\|protected\\|class"))
        (recenter-top-bottom 0)))

  (defun java-set-indentation-behavior ()
    (c-set-offset 'arglist-intro '+))

  ;; Hooks
  (add-hook 'java-mode-hook #'java-goto-class)
  (add-hook 'java-mode-hook #'java-set-indentation-behavior)
  (add-hook 'window-configuration-change-hook #'java-class-to-top))

(provide 'java)

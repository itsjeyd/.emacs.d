(defun python-split-args (arg-string)
  "Split a Python argument string into ((name, default)..) tuples."
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-args-to-docstring ()
  "Return docstring format for Python arguments in `yas-text'."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                (lambda (x)
                   (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                           (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                args
                indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Arguments:" formatted-args) indent))))

(defun python-set-snippet-indentation ()
  (set (make-local-variable 'yas-indent-line) 'fixed))

(add-hook 'python-mode-hook #'python-set-snippet-indentation)

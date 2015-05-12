;;; wenote.el --- Custom elisp for wenote development.

;;;###autoload
(defun hamlpy-regenerate-html ()
  "Run hamlpy on file in current buffer.

  Input file: File we are currently looking at, obtained using
  `buffer-file-name`

  Output file: File with same name but different
  extension (.html), obtained using a combination of
  `file-name-sans-extension` and `file-name-nondirectory` to get the
  file name without path and extension[1], and
  concatenating the result with the appropriate extension.

  [1] Note that starting with Emacs 24.3, there is a built-in
  function called `file-name-base` that can be used for the same
  purpose.

  Folder of target file: Folder called `html` that lives next to the
  `haml` folder that holds the file we are currently looking at

  `default-directory` is the current directory, of which we want
  the parent. In order to achieve this, we first have to convert
  the current directory as given by `default-directory` from a
  *directory name* into a *file name* using
  `directory-file-name`. We can then call `file-name-directory`
  on the result, which gives us the parent directory of the
  current directory."
  (interactive)
  (when (equal (file-name-extension (buffer-file-name)) "haml")
    (call-process
      "hamlpy" nil nil t
      (buffer-file-name)
      (concat
        (file-name-directory (directory-file-name default-directory))
        "html/"
        (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
        ".html"))))

(add-hook 'after-save-hook 'hamlpy-regenerate-html)

(provide 'wenote)

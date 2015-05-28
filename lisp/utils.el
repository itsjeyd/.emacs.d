;;; utils.el --- Useful commands and functions that I don't need frequently.



;;;;;;;;;;;;;;;
;;; Editing ;;;
;;;;;;;;;;;;;;;

; Commands
(defun flush-empty-lines ()
  "Remove empty lines from buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^$")))

(defun sort-lines-and-uniquify ()
  "Sort lines alphabetically (in ascending order) and remove duplicates."
  (interactive)
  (sort-lines nil (point-min) (point-max))
  (delete-duplicate-lines (point-min) (point-max) nil nil nil t))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and turns it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))



;;;;;;;;;;;;;;;;;
;;; Interface ;;;
;;;;;;;;;;;;;;;;;

(defun toggle-transparency ()
  (interactive)
  (let ((opacity (frame-parameter nil 'alpha)))
    (if (or (not opacity) (= opacity 100))
        (set-frame-parameter nil 'alpha 80)
      (set-frame-parameter nil 'alpha 100))))



;;;;;;;;;;;;;;;;;;;;
;;; Key Bindings ;;;
;;;;;;;;;;;;;;;;;;;;

; Source: http://emacs.stackexchange.com/a/654/504

(defun locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret (list (key-binding-at-point key)
                   (minor-mode-key-binding key)
                   (local-key-binding key)
                   (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

(defun key-binding-at-point (key)
  (mapcar (lambda (keymap) (lookup-key keymap key))
          (cl-remove-if-not #'keymapp
                            (append (mapcar (lambda (overlay)
                                              (overlay-get overlay 'keymap))
                                            (overlays-at (point)))
                                    (get-text-property (point) 'keymap)
                                    (get-text-property (point) 'local-map)))))



;;;;;;;;;;;;
;;; MISC ;;;
;;;;;;;;;;;;

; Functions
(defun print-chars (chars)
  (dolist (char chars)
    (insert (prin1-char char) " ")))



;;;;;;;;;;;;;;;;
;;; Security ;;;
;;;;;;;;;;;;;;;;

;; Easiest symmetric crypto for region of text.
;;
;; Mark start of region, move cursor to end of region, then
;; hit either M-F11 to encrypt, or M-F12 to decrypt.
;; Try alternating between both (without moving cursor) to
;; see the text morph from cleartext to encrypted text back
;; and forth.
;;
;; After encrypting you can copy or kill without moving cursor.
;; Or after pasting encrypted into buffer, hit M-F12 to decrypt
;; since pasted region is already marked.  Short, simple, easy.

;; Needed for below functions
(require 'epg)

;; Encrypt region with default password
(defun symmetric-encrypt-region (&optional n)
  "Encrypt region using symmetric crypto."
  (interactive "P")
  (let ((plain (delete-and-extract-region (point) (mark))))
    (insert (epg-encrypt-string (epg-make-context nil t) plain nil))))

;; Decrypt region with default password
(defun symmetric-decrypt-region (&optional n)
  "Decrypt region using symmetric crypto."
  (interactive "P")
  (let ((cipher (delete-and-extract-region (point) (mark))))
    (insert (epg-decrypt-string (epg-make-context nil t) cipher))))

; Adapted from: http://pastebin.com/M1MAaaT6

(provide 'utils)

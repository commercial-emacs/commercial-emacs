;;; mode-overlay.el --- One buffer, multiple major modes  -*- lexical-binding:t -*-

(defgroup mode-overlay nil
  "Multiple language modes in buffer."
  :version "31.1"
  :group 'convenience)

(defface mode-overlay '((t :inherit highlight :extend t))
  "Default face for `mode-overlay-face'."
  :version "31.1"
  :group 'mode-overlay)

(defcustom mode-overlay-face-alist nil
  "Demarcate mode overlay."
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type (symbol :tag "Face"))
  :group 'mode-overlay)

(defsubst mode-overlay-p (overlay)
  (overlay-get overlay 'mode-overlay-p))

;;;###autoload
(defun delete-all-mode-overlays ()
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (mode-overlay-p ov)
      (delete-mode-overlay (overlay-start ov)))))

;;;###autoload
(defun delete-mode-overlay (pos)
  "Remove all mode overlays at POS."
  (interactive "d")
  (dolist (ov (overlays-at pos))
    (when (mode-overlay-p ov)
      (let ((font-lock-extra-managed-props
             `(display fontified . ,font-lock-extra-managed-props))
            (beg (overlay-start ov))
            (end (overlay-end ov))
            (modified (buffer-modified-p)))
        ;; Eschew `with-silent-modifications' since precision required.
        (unwind-protect
            (progn
              (delete-overlay ov)
              (font-lock-unfontify-region beg end))
          (when (memq modified '(nil autosaved))
            (restore-buffer-modified-p modified)))))))

;;;###autoload
(defun make-mode-overlay (beg end mode)
  "Return new overlay corresponding to indirect buffer of major MODE."
  (interactive
   (list (if (region-active-p) (region-beginning) (point))
         (if (region-active-p) (region-end) (point))
         (intern-soft
          (completing-read
           "Mode: "
           (let (modes)
             (mapatoms
              (lambda (sym)
                (when (provided-mode-derived-p sym '(prog-mode))
                  (push sym modes)))) modes)
           nil t))))
  (if (or (null mode) (string-empty-p (symbol-name mode)))
      (keyboard-quit)
    ;; Eschew `with-silent-modifications' since precision required.
    (let ((modified (buffer-modified-p))
          (font-lock-extra-managed-props
           `(display fontified . ,font-lock-extra-managed-props)))
      (unwind-protect
          (progn
            (font-lock-unfontify-region beg end)
            (make-mode--overlay beg end mode))
        (when (memq modified '(nil autosaved))
          (restore-buffer-modified-p modified))))))

(defalias 'mode-overlay-on-switch-to-buffer
  (lambda (_window)
    (mapc (lambda (ov) (funcall (or (overlay-on-enter ov) #'ignore) ov))
          (overlays-at (point))))
  "A hook in `window-buffer-change-functions' to immediately switch
to the appropriate indirect buffer.")

(defalias 'mode-overlay-filter-buffer-substring-function
  (lambda ()
    (add-function :filter-return
                  (local 'filter-buffer-substring-function)
                  (lambda (s)
                    (prog2 (remove-list-of-text-properties
                            0 (length s) '(read-only rear-nonsticky) s)
                        s))))
  "Don't paste a read-only bumpguard user then can't delete.")

(provide 'mode-overlay)

;;; mode-overlay.el ends here

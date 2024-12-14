;;; multi-lang.el --- One buffer, multiple major modes  -*- lexical-binding:t -*-

(defgroup multi-lang nil
  "Multiple language modes in buffer."
  :version "31.1"
  :group 'convenience)

(defface multi-lang '((t :inherit highlight :extend t))
  "Default face for `multi-lang-face'."
  :version "31.1"
  :group 'multi-lang)

(defcustom multi-lang-face 'multi-lang
  "Demarcate multi-lang overlay."
  :type 'face
  :group 'multi-lang
  :set (lambda (symbol value)
	 (set symbol value)
	 (dolist (b (buffer-list))
	   (with-current-buffer b
             ;; (mapc (lambda (ov) (overlay-put ov 'face multi-lang-face)) overlays)
             ))))

(defsubst multi-lang-p (overlay)
  (overlay-get overlay 'multi-lang-p))

(defun delete-multi-lang-overlay (beg)
  "Remove all multi-lang overlays at BEG."
  (interactive "d")
  (dolist (ov (overlays-at beg))
    (when (multi-lang-p ov)
      (font-lock-unfontify-region (overlay-start ov) (overlay-end ov))
      (with-silent-modifications
        (delete-overlay ov)))))

(defun make-multi-lang-overlay (beg end mode)
  "Return indirect buffer with major mode MODE."
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
    (with-silent-modifications
      (make-multi-lang--overlay beg end mode))))

(defalias 'multi-lang-on-switch-to-buffer
  (lambda (_window)
    (mapc (lambda (ov) (funcall (or (overlay-on-enter ov) #'ignore) ov))
          (overlays-at (point))))
  "A hook in `window-buffer-change-functions' to immediately switch
to the appropriate indirect buffer.")

(provide 'multi-lang)

;;; multi-lang.el ends here

;;; window-border.el --- where we at???  -*- lexical-binding:t -*-

;; Copyright (C) 2025 commandlinesystems.com

(defcustom window-border-width (min 2 (frame-char-height))
  "Width of the border around the selected window in pixels."
  :type 'natnum
  :group 'windows
  :set (lambda (sym val)
         (let ((max-height (max 1 (/ (frame-char-height) 2))))
           (when (or (< val 1) (> val max-height))
             (user-error "window-border-width must be between 1 and %d" max-height))
           (set-default sym val)
           (when (bound-and-true-p window-border-mode)
             (window-border-mode -1)
             (window-border-mode 1)))))

;;;###autoload
(define-minor-mode window-border-mode
  "Highlight the selected window with a rectangular border."
  :global t
  :group 'windows
  (unless (eq window-system 'x)
    (setq window-border-mode nil)
    (user-error "window-border-mode only implemented for X"))
  (if window-border-mode
      (if (or (not (integerp window-border-width))
              (<= window-border-width 0))
          (progn
            (setq window-border-mode nil)
            (user-error "window-border-width %s not a positive integer"
                        window-border-width))
        (window-border--primitive window-border-width))
    (window-border--primitive 0))
  (redraw-display))

(provide 'window-border)

;;; window-border.el ends here

;;; window-border.el --- where we at???  -*- lexical-binding:t -*-

;; Copyright (C) 2025 commandlinesystems.com

(defcustom window-border-width 2
  "Width of the border around the selected window in pixels."
  :type 'natnum
  :group 'windows)

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
            (user-error "window-border-width %s not a positive integer"
                        window-border-width)
            (setq window-border-mode nil))
        (window-border--primitive window-border-width))
    (window-border--primitive 0))
  (redraw-display))

(provide 'window-border)

;;; window-border.el ends here

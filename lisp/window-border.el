;;; window-border.el --- where we at???  -*- lexical-binding:t -*-

;; Copyright (C) 2025 commandlinesystems.com

(defcustom window-border-width 2
  "Width of the border around the selected window in pixels."
  :type 'natnum
  :group 'windows)

(defcustom window-border-color "#0000ff"
  "Color for the selected window's border."
  :type 'color
  :group 'windows)

;;;###autoload
(define-minor-mode window-border-mode
  "Highlight the selected window with a rectangular border."
  :global t
  :group 'windows
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

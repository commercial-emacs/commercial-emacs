;;; window-border-mode.el --- Highlight selected window with border  -*- lexical-binding:t -*-

;; Copyright (C) 2025 commandlinesystems.com

(defcustom window-border-width 2
  "Width of the border around the selected window in pixels."
  :type 'integer
  :group 'windows)

(defcustom window-border-color "#0000ff"
  "Color for the selected window's border."
  :type 'color
  :group 'windows)

;;;###autoload
(define-minor-mode window-border-mode
  "Highlight the selected window with a rectangular border."
  :global t
  :group 'windows)

(provide 'window-border-mode)

;;; window-border-mode.el ends here

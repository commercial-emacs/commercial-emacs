;;; window-border-mode.el --- Highlight selected window with border  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Claude Code
;; Keywords: convenience, frames

;;; Commentary:

;; This package draws a rectangular border around the selected window,
;; making it very obvious which window has focus.

;;; Code:

(defcustom window-border-width 1
  "Width of the border around the selected window in pixels."
  :type 'integer
  :group 'windows)

(defcustom window-border-color "#0000ff"
  "Color for the selected window's border."
  :type 'color
  :group 'windows)

(defvar window-border--last-window nil
  "Last window that had a border.")

(defun window-border--update ()
  "Update window borders - add border to selected window, remove from others."
  (let ((current (selected-window)))
    ;; Clear border from last window and force complete redisplay
    (when (and window-border--last-window
               (window-live-p window-border--last-window)
               (not (eq window-border--last-window current)))
      (set-window-border window-border--last-window 0)
      ;; Force complete window redisplay to clear border artifacts
      (with-selected-window window-border--last-window
        (redraw-display)))

    ;; Set border on current window
    (set-window-border current window-border-width)

    (setq window-border--last-window current)))

;;;###autoload
(define-minor-mode window-border-mode
  "Highlight the selected window with a rectangular border."
  :global t
  :group 'windows
  (if window-border-mode
      (progn
        (add-hook 'post-command-hook #'window-border--update)
        (add-hook 'window-configuration-change-hook #'window-border--update)
        (add-hook 'buffer-list-update-hook #'window-border--update)
        (window-border--update))
    (remove-hook 'post-command-hook #'window-border--update)
    (remove-hook 'window-configuration-change-hook #'window-border--update)
    (remove-hook 'buffer-list-update-hook #'window-border--update)
    ;; Remove all borders
    (walk-windows
     (lambda (w)
       (set-window-border w 0))
     'no-minibuf t)))

(provide 'window-border-mode)

;;; window-border-mode.el ends here

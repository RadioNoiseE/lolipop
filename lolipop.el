;;; lolipop-mode.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jing Huang <rne.kou@icloud.com>

(defvar lolipop--timer nil
  "Timer used to rate-limit cursor animations.")

(defvar lolipop-filter-modes nil
  "List of major and minor modes that need spacial care.")

(defvar lolipop-filter-commands nil
  "List of commands that need special care.")

(defun lolipop-savor ()
  "Determine whether the animation should be rendered."
  (when (timerp lolipop--timer)
    (cancel-timer lolipop--timer))
  (if (or (memq major-mode lolipop-filter-modes)
          (seq-intersection local-minor-modes lolipop-filter-modes)
          (memq this-command lolipop-filter-commands))
      (lolipop-lick nil)
    (setq lolipop--timer
          (run-with-idle-timer 0.01 nil #'lolipop-unwrap))))

(defun lolipop-unwrap ()
  "Calculate current cursor position, size and color, then call
`lolipop-lick' which does all of the heavy lifting."
  (when-let* ((visible (or (pos-visible-in-window-p)
                           (and (redisplay)
                                (pos-visible-in-window-p))))
              (cursor (posn-at-point))
              (coordinate (posn-x-y cursor))
              (window (posn-window cursor))
              (edges (window-inside-pixel-edges window)))
    (apply #'lolipop-lick
           (+ (car coordinate) (nth 0 edges))
           (+ (cdr coordinate) (nth 1 edges)) ; TODO: handle descent
           (if-let* ((cursor (point))
                     (glyph (and (< cursor (point-max))
                                 (aref (font-get-glyphs
                                        (font-at cursor)
                                        cursor
                                        (1+ cursor)) 0))))
               (aref glyph 4)
             (frame-char-width))
           (if-let* ((cursor (point))
                     (font (and (not (equal cursor (line-end-position)))
                                (font-info
                                 (font-at cursor)))))
               (aref font 3)
             (line-pixel-height))
           (color-name-to-rgb (frame-parameter nil 'cursor-color)))))

;;;###autoload
(define-minor-mode lolipop-mode
  "Toggle rendering of cursor animations."
  :global t
  (if lolipop-mode
      (progn
        (unless (functionp 'lolipop-lick)
          (load "lolipop-core"))
        (add-hook 'post-command-hook #'lolipop-savor))
    (remove-hook 'post-command-hook #'lolipop-savor)))

(provide 'lolipop-mode)

;;; lolipop-mode.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jing Huang <rne.kou@icloud.com>

(defvar lolipop--timer nil
  "Timer used to rate-limit cursor animations.")

(defun lolipop-savor ()
  "Cancel exisiting timer and schedule a new one."
  (when (timerp lolipop--timer)
    (cancel-timer lolipop--timer))
  (setq lolipop--timer
        (run-with-idle-timer 0.02 nil 'lolipop-unwrap)))

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
    (apply 'lolipop-lick
           (list
            (+ (car coordinate) (nth 0 edges))
            (+ (cdr coordinate) (nth 1 edges))
            (if-let* ((cursor (point))
                      (glyph (and (< cursor (point-max))
                                  (aref (font-get-glyphs
                                         (font-at cursor)
                                         cursor
                                         (1+ cursor)) 0))))
                (aref glyph 4)
              (frame-char-width))
            (line-pixel-height)
            (frame-parameter nil 'cursor-color)))))

;;;###autoload
(define-minor-mode lolipop-mode
  "Toggle rendering of cursor animations."
  :init-value nil
  (if lolipop-mode
      (progn
        (unless (functionp 'lolipop-lick)
          (load "lolipop-core"))
        (add-hook 'post-command-hook 'lolipop-savor nil t))
    (remove-hook 'post-command-hook 'lolipop-savor t)))

(provide 'lolipop-mode)

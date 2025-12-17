;;; lolipop-mode.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jing Huang <rne.kou@icloud.com>

(defun lolipop-crush ()
  (when-let* ((visible (or (pos-visible-in-window-p)
                           (and (redisplay)
                                (pos-visible-in-window-p))))
              (cursor (posn-at-point))
              (coordinate (posn-x-y cursor))
              (window (posn-window cursor))
              (edges (window-inside-pixel-edges window)))
    (apply 'lolipop-chew
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
  "Toggle trace for your cursor."
  :global t
  (if lolipop-mode
      (progn
        (load "lolipop-core")
        (add-hook 'post-command-hook 'lolipop-crush))
    (remove-hook 'post-command-hook 'lolipop-crush)))

(provide 'lolipop-mode)

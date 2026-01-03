;;; lolipop-mode.el -*- lexical-binding: t -*-

;; Copyright (C) 2025, 2026 Jing Huang <rne.kou@icloud.com>

(defvar lolipop--timer nil
  "Throttle timer used to rate-limit cursor animations.

This timer is canceled and rescheduled on each command in order to block
animation updates and reduce flickering.")

(defvar lolipop-filter-modes nil
  "List of modes for which cursor animations are suppressed.

If the current major mode or any enabled minor mode is a member of this
list, cursor animations will not be rendered.")

(defvar lolipop-filter-commands nil
  "List of commands for which cursor animations are suppressed.

If `this-command' is a member of this list, cursor animations will not
be rendered.  See also `post-command-hook'.")

(defun lolipop-savor ()
  "Determine whether and when to update the cursor animation.

If the current context matches any entry in `lolipop-filter-modes' or
`lolipop-filter-commands', the cursor state is updated immediately
with no cursor animation rendered.

Otherwise, update and render the cursor animation immediately on the
first eligible invocation, then suppress further animation updates for a
short, fixed interval.  During this interval, subsequent invocations do
nothing.

The throttle state is maintained by `lolipop--timer'."
  (cond
   ((or (memq major-mode lolipop-filter-modes)
        (seq-intersection local-minor-modes lolipop-filter-modes)
        (memq this-command lolipop-filter-commands))
    (lolipop-unwrap nil))
   ((not (and lolipop--timer
              (timerp lolipop--timer)))
    (lolipop-unwrap t)
    (setq lolipop--timer
          (run-with-idle-timer
           0.02 nil
           (lambda ()
             (cancel-timer lolipop--timer)
             (setq lolipop--timer nil)))))
   (t (lolipop-unwrap nil))))

(defun lolipop-unwrap (render)
  "Collect cursor geometry and color, then invoke `lolipop-lick'.

This function computes the pixel position, size and color of the cursor.
The coordinates supplied are relative to the top-left corner of the
window; any further coordinate transformation is handled elsewhere.

The argument RENDER determines whether the cursor animation is rendered.
If nil, only the internal cursor state is updated."
  (when-let* ((status (redisplay))
              (cursor (window-cursor-info))
              (edges (window-inside-pixel-edges)))
    (let* ((point (and (pos-visible-in-window-p)
                       (point)))
           (glyph (and point
                       (< point (point-max))
                       (font-info (font-at point))))
           (short (and glyph
                       (< (aref glyph 3) (aref cursor 4)))))
      (apply #'lolipop-lick
             render
             (+ (aref cursor 1) (nth 0 edges))
             (+ (aref cursor 2) (nth 1 edges)
                (if short (- (aref cursor 5) (aref glyph 8)) 0))
             (aref cursor 3)
             (if short (aref glyph 3) (aref cursor 4))
             (color-name-to-rgb (frame-parameter nil 'cursor-color))))))

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

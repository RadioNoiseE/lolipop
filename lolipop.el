;;; lolipop-mode.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jing Huang <rne.kou@icloud.com>

(defvar lolipop--timer nil
  "Idle timer used to rate-limit cursor animations.

This timer is canceled and rescheduled on each command in order to delay
animation updates until Emacs becomes idle.")

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

Otherwise, schedule an idle timer `lolipop--timer' to update and render
the cursor animation after a short delay.  Any previously scheduled
timer is canceled before scheduling a new one."
  (when (timerp lolipop--timer)
    (cancel-timer lolipop--timer))
  (if (or (memq major-mode lolipop-filter-modes)
          (seq-intersection local-minor-modes lolipop-filter-modes)
          (memq this-command lolipop-filter-commands))
      (lolipop-unwrap t)
    (setq lolipop--timer
          (run-with-idle-timer 0.02 nil #'lolipop-unwrap))))

(defun lolipop-unwrap (&optional hide)
  "Collect cursor geometry and color, then invoke `lolipop-lick'.

This function computes the pixel position, size and color of the cursor.
The coordinates supplied are relative to the top-left corner of the
window; any further coordinate transformation is handled by
`lolipop-lick'.

If the optional argument HIDE is non-nil, the cursor animation is not
rendered.  In this case, only the internal cursor state is updated."
  (when-let* ((cursor (window-cursor-info))
              (edges (window-inside-pixel-edges)))
    (apply #'lolipop-lick
           (if hide nil t)
           (+ (aref cursor 1) (nth 0 edges))
           (+ (aref cursor 2) (nth 1 edges))
           (aref cursor 3)
           (aref cursor 4)
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

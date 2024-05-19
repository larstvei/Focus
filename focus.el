;;; focus.el --- Dim the font color of text in surrounding sections  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Lars Tveito

;; Author: Lars Tveito <larstvei@ifi.uio.no>
;; URL: http://github.com/larstvei/Focus
;; Created: 11th May 2015
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Focus provides `focus-mode` that dims the text of surrounding sections,
;; similar to [iA Writer's](https://ia.net/writer) Focus Mode.
;;
;; Enable the mode with `M-x focus-mode'.

;;; Code:

(require 'cl-lib)
(require 'thingatpt)

(defgroup focus ()
  "Dim the font color of text in surrounding sections."
  :group 'font-lock
  :prefix "focus-")

(defcustom focus-mode-to-thing '((prog-mode . defun)
                                 (text-mode . paragraph)
                                 (org-mode . org-element))
  "An associated list between mode and thing.

A thing is defined in thingatpt.el; the thing determines the
narrowness of the focused section.

Note that the order of the list matters.  The first mode that the
current mode is derived from is used, so more modes that have
many derivatives should be placed by the end of the list.

Things that are defined include `symbol', `list', `sexp',
`defun', `filename', `url', `email', `word', `sentence',
`whitespace', `line', and `page'.

In order for changes to take effect, reenable `focus-mode'."
  :type '(alist :key-type symbol :valye-type symbol)
  :group 'focus)

(defcustom focus-read-only-blink-seconds 1
  "The duration of a cursor blink in `focus-read-only-mode'."
  :type 'number
  :group 'focus)

(defcustom focus-update-idle-delay nil
  "Delay (in seconds) before updating the focus after each command.
The default value of nil results in an immediate update.
Increase this value if you experience performance issues."
  :type '(choice (const  :tag "Immediate update" nil)
                 (const  :tag "Delayed update (0.1s)" 0.1)
                 (number :tag "Custom delay"))
  :group 'focus)

(defface focus-unfocused
  '((t :inherit shadow))
  "The face that overlays the unfocused area."
  :group 'focus)

(defface focus-focused nil
  "The face that overlays the focused area."
  :group 'focus)

(defvar focus-cursor-type cursor-type
  "Used to restore the users `cursor-type'.")

(defvar-local focus-current-thing nil
  "Overrides the choice of thing dictated by `focus-mode-to-thing' if set.")

(defvar-local focus-current-thing-cache nil
  "Caches the current thing to focus.")

(defvar-local focus-buffer nil
  "Local reference to the buffer focus functions operate on.")

(defvar-local focus-pre-overlay nil
  "The overlay that dims the text prior to the current-point.")

(defvar-local focus-mid-overlay nil
  "The overlay that surrounds the text of the current-point.")

(defvar-local focus-post-overlay nil
  "The overlay that dims the text past the current-point.")

(defvar-local focus-read-only-blink-timer nil
  "Timer started from `focus-read-only-cursor-blink'.
The timer calls `focus-read-only-hide-cursor' after
`focus-read-only-blink-seconds' seconds.")

(defvar-local focus-update-timer nil
  "Timer started from `focus-update'")

(defun focus-get-thing ()
  "Return the current thing, based on `focus-mode-to-thing'.

This also sets `focus-current-thing-cache' to the current thing."
  (or focus-current-thing
      focus-current-thing-cache
      (setq focus-current-thing-cache
            (let* ((modes (mapcar 'car focus-mode-to-thing))
                   (mode  (or (cl-find major-mode modes)
                              (apply #'derived-mode-p modes))))
              (if mode (cdr (assoc mode focus-mode-to-thing)) 'sentence)))))

(defun focus-bounds ()
  "Return the current bounds, based on `focus-get-thing'."
  (let ((thing (focus-get-thing)))
    (cond ((eq thing 'org-element)
           (let* ((elem (org-element-at-point))
                  (beg (org-element-property :begin elem))
                  (end (org-element-property :end elem)))
             (cons beg end)))
          (t (bounds-of-thing-at-point thing)))))

(defun focus-move-focus (buffer)
  "Move the focused section according to `focus-bounds'.

If `focus-mode' is enabled, this command fires after each
command."
  (with-current-buffer buffer
    (setq focus-update-timer nil)
    (let* ((bounds (focus-bounds)))
      (when bounds
        (focus-move-overlays (car bounds) (cdr bounds))))))

(defun focus-update ()
  "Trigger an update of the focus.

When `focus-update-idle-delay' is non-nil, start update after the
specified idle delay."
  (if focus-update-idle-delay
      (unless focus-update-timer
        (setq focus-update-timer
              (run-with-idle-timer focus-update-idle-delay nil
                                   #'focus-move-focus focus-buffer)))
    (focus-move-focus focus-buffer)))

(defun focus-move-overlays (low high)
  "Move the overlays to highlight the region between LOW and HIGH."
  (move-overlay focus-pre-overlay (point-min) low)
  (move-overlay focus-mid-overlay low high)
  (move-overlay focus-post-overlay high (point-max)))

(defun focus-init ()
  "This function is run when command `focus-mode' is enabled.

It sets the `focus-pre-overlay', `focus-min-overlay', and
`focus-post-overlay' to overlays; these are invisible until
`focus-update' is run.  It adds `focus-update' to
`post-command-hook'."
  (unless (or focus-pre-overlay focus-post-overlay)
    (setq focus-pre-overlay  (make-overlay (point-min) (point-min))
          focus-mid-overlay  (make-overlay (point-min) (point-max))
          focus-post-overlay (make-overlay (point-max) (point-max))
          focus-buffer (current-buffer))
    (overlay-put focus-mid-overlay 'face 'focus-focused)
    (mapc (lambda (o) (overlay-put o 'face 'focus-unfocused))
          (list focus-pre-overlay focus-post-overlay))
    (setq focus-current-thing-cache nil
          focus-update-timer nil)
    (add-hook 'post-command-hook 'focus-update nil t)
    (add-hook 'change-major-mode-hook 'focus-terminate nil t)))

(defun focus-terminate ()
  "This function is run when command `focus-mode' is disabled.

The overlays pointed to by `focus-pre-overlay',
`focus-mid-overlay' and `focus-post-overlay' are deleted, and
`focus-update' is removed from `post-command-hook'."
  (when (and focus-pre-overlay focus-post-overlay)
    (mapc 'delete-overlay
          (list focus-pre-overlay focus-mid-overlay focus-post-overlay))
    (remove-hook 'post-command-hook 'focus-update t)
    (when focus-update-timer
      (cancel-timer focus-update-timer))
    (setq focus-current-thing-cache nil
          focus-update-timer nil
          focus-pre-overlay nil
          focus-mid-overlay nil
          focus-post-overlay nil)))

(defun focus-goto-thing (bounds)
  "Move point to the middle of BOUNDS."
  (when bounds
    (goto-char (/ (+ (car bounds) (cdr bounds)) 2))
    (recenter nil)))

(defun focus-change-thing ()
  "Adjust the narrowness of the focused section for the current buffer.

The variable `focus-mode-to-thing' dictates the default thing
according to major-mode.  If `focus-current-thing' is set, this
default is overwritten.  This function simply helps set the
`focus-current-thing'."
  (interactive)
  (let* ((candidates '(defun line list org-element paragraph sentence sexp symbol word))
         (thing (completing-read "Thing: " candidates)))
    (setq focus-current-thing (intern thing))))

(defun focus-pin ()
  "Pin the focused section to its current location or the region, if active."
  (interactive)
  (when (bound-and-true-p focus-mode)
    (when (region-active-p)
      (focus-move-overlays (region-beginning) (region-end)))
    (when focus-update-timer
      (cancel-timer focus-update-timer))
    (setq focus-update-timer nil)
    (remove-hook 'post-command-hook 'focus-update t)))

(defun focus-unpin ()
  "Unpin the focused section."
  (interactive)
  (when (bound-and-true-p focus-mode)
    (add-hook 'post-command-hook 'focus-update nil t)))

(defun focus-next-thing (&optional n)
  "Move the point to the middle of the Nth next thing."
  (interactive "p")
  (let ((current-bounds (focus-bounds))
        (thing (focus-get-thing)))
    (forward-thing thing n)
    (when (equal current-bounds (focus-bounds))
      (forward-thing thing (cl-signum n)))
    (focus-goto-thing (focus-bounds))))

(defun focus-prev-thing (&optional n)
  "Move the point to the middle of the Nth previous thing."
  (interactive "p")
  (focus-next-thing (- n)))

(defun focus-read-only-hide-cursor ()
  "Hide the cursor.
This function is triggered by the `focus-read-only-blink-timer',
when `focus-read-only-mode' is activated."
  (with-current-buffer focus-buffer
    (when (and (bound-and-true-p focus-read-only-mode)
               (not (null focus-read-only-blink-timer)))
      (setq focus-read-only-blink-timer nil)
      (setq cursor-type nil))))

(defun focus-read-only-cursor-blink ()
  "Make the cursor visible for `focus-read-only-blink-seconds'.
This is added to the `pre-command-hook' when
`focus-read-only-mode' is active."
  (with-current-buffer focus-buffer
    (when (and (bound-and-true-p focus-read-only-mode)
               (not (member last-command '(focus-next-thing focus-prev-thing))))
      (when focus-read-only-blink-timer (cancel-timer focus-read-only-blink-timer))
      (setq cursor-type focus-cursor-type)
      (setq focus-read-only-blink-timer
            (run-at-time focus-read-only-blink-seconds nil
                         'focus-read-only-hide-cursor)))))

(defun focus-read-only-init ()
  "Run when `focus-read-only-mode' is activated.
Enables `read-only-mode', hides the cursor and adds
`focus-read-only-cursor-blink' to `pre-command-hook'.
Also `focus-read-only-terminate' is added to the `kill-buffer-hook'."
  (read-only-mode 1)
  (setq cursor-type nil
        focus-buffer (current-buffer))
  (add-hook 'pre-command-hook 'focus-read-only-cursor-blink nil t)
  (add-hook 'kill-buffer-hook 'focus-read-only-terminate nil t))

(defun focus-read-only-terminate ()
  "Run when `focus-read-only-mode' is deactivated.
Disables `read-only-mode' and shows the cursor again.
It cleans up the `focus-read-only-blink-timer' and hooks."
  (read-only-mode -1)
  (setq cursor-type focus-cursor-type)
  (when focus-read-only-blink-timer
    (cancel-timer focus-read-only-blink-timer))
  (setq focus-read-only-blink-timer nil)
  (remove-hook 'pre-command-hook 'focus-read-only-cursor-blink t)
  (remove-hook 'kill-buffer-hook 'focus-read-only-terminate t))

(defun focus-turn-off-focus-read-only-mode ()
  "Turn off `focus-read-only-mode'."
  (interactive)
  (focus-read-only-mode -1))

;;;###autoload
(define-minor-mode focus-mode
  "Dim the font color of text in surrounding sections."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-q") 'focus-read-only-mode)
            map)
  (if focus-mode (focus-init) (focus-terminate)))

;;;###autoload
(define-minor-mode focus-read-only-mode
  "A read-only mode optimized for `focus-mode'."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "n") 'focus-next-thing)
            (define-key map (kbd "SPC") 'focus-next-thing)
            (define-key map (kbd "p") 'focus-prev-thing)
            (define-key map (kbd "S-SPC") 'focus-prev-thing)
            (define-key map (kbd "i") 'focus-turn-off-focus-read-only-mode)
            (define-key map (kbd "q") 'focus-turn-off-focus-read-only-mode)
            map)
  (when cursor-type
    (setq focus-cursor-type cursor-type))
  (if focus-read-only-mode (focus-read-only-init) (focus-read-only-terminate)))

(provide 'focus)
;;; focus.el ends here

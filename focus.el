;;; focus.el --- Dim the font color of text in surrounding sections  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Lars Tveito

;; Author: Lars Tveito <larstvei@ifi.uio.no>
;; URL: http://github.com/larstvei/Focus
;; Created: 11th May 2015
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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

(defcustom focus-dimness 0
  "Amount of dimness in out of focus sections is determined by this integer.

A positive value increases the dimness of the sections.
A negative value decreases the dimness.

The default is 0 which means a 50/50 mixture of the background
and foreground color."
  :type '(integer)
  :group 'focus)

(defcustom focus-mode-to-thing '((prog-mode . defun) (text-mode . sentence))
  "An associated list between mode and thing.

A thing is defined in thingatpt.el; the thing determines the
narrowness of the focused section.

Note that the order of the list matters. The first mode that the
current mode is derived from is used, so more modes that have
many derivatives should be placed by the end of the list.

Things that are defined include `symbol', `list', `sexp',
`defun', `filename', `url', `email', `word', `sentence',
`whitespace', `line', and `page'."
  :type '(repeat symbol)
  :group 'focus)

(defvar focus-pre-overlay nil
  "The overlay that dims the text prior to the current-point.")

(defvar focus-post-overlay nil
  "The overlay that dims the text past the current-point.")

;; Use make-local-variable for backwards compatibility.
(dolist (var '(focus-pre-overlay
               focus-post-overlay))
  (make-local-variable var))

;; Changing major-mode should not affect Focus mode.
(dolist (var '(focus-pre-overlay
               focus-post-overlay
               post-command-hook))
  (put var 'permanent-local t))

(defun focus-any (f lst)
  "Apply F to each element of LST and return first NON-NIL."
  (when lst
    (let ((v (funcall f (car lst))))
      (if v v (focus-any f (cdr lst))))))

(defun focus-get-thing ()
  "Return the current thing, based on `focus-mode-to-thing'."
  (let* ((modes (mapcar 'car focus-mode-to-thing))
         (mode  (focus-any 'derived-mode-p modes)))
    (if mode (cdr (assoc mode focus-mode-to-thing)) 'sentence)))

(defun focus-bounds ()
  "Return the current bounds, based on `focus-get-thing'."
  (bounds-of-thing-at-point (focus-get-thing)))

(defun focus-average-colors (color &rest colors)
  "Takes an average of the colors given by argument.
Argument COLOR is a color name, and so are the COLORS; COLOR is
there to ensure that the the function receives at least one
argument."
  (let* ((colors (cons color colors))
         (colors (mapcar 'color-name-to-rgb colors))
         (len    (length colors))
         (sums   (apply 'cl-mapcar '+ colors))
         (avg    (mapcar (lambda (v) (/ v len)) sums)))
    (apply 'color-rgb-to-hex avg)))

(defun focus-make-dim-color ()
  "Return a dimmed color relative to the current theme."
  (let ((background (face-attribute 'default :background))
        (foreground (face-attribute 'default :foreground))
        (backgrounds (if (> focus-dimness 0)    focus-dimness  1))
        (foregrounds (if (< focus-dimness 0) (- focus-dimness) 1)))
    (apply 'focus-average-colors
           (append (make-list backgrounds background)
                   (make-list foregrounds foreground)))))

(defun focus-move-focus ()
  "Move `focus-pre-overlay' and `focus-post-overlay'.

If function `focus-mode' is enabled, this command fires after
each command."
  (let* ((bounds (focus-bounds)))
    (when bounds
      (move-overlay focus-pre-overlay  (point-min) (car bounds))
      (move-overlay focus-post-overlay (cdr bounds) (point-max)))))

(defun focus-init ()
  "This function is run when command `focus-mode' is enabled.

 It sets the `focus-pre-overlay' and `focus-post-overlay' to
overlays; these are invisible until `focus-move-focus' is run. It
adds `focus-move-focus' to `post-command-hook'."
  (setq focus-pre-overlay  (make-overlay (point-min) (point-min))
        focus-post-overlay (make-overlay (point-max) (point-max)))
  (let ((color (focus-make-dim-color)))
    (mapc (lambda (o) (overlay-put o 'face (cons 'foreground-color color)))
          (list focus-pre-overlay focus-post-overlay)))
  (add-hook 'post-command-hook 'focus-move-focus nil t))

(defun focus-terminate ()
  "This function is run when command `focus-mode' is disabled.

The overlays pointed to by `focus-pre-overlay' and `focus-post-overlay' are
deleted, and `focus-move-focus' is removed from `post-command-hook'."
  (progn (mapc 'delete-overlay (list focus-pre-overlay focus-post-overlay))
         (remove-hook 'post-command-hook 'focus-move-focus t)))

(defun focus-next-thing (&optional n)
  "Moves the point to the middle of the Nth next thing."
  (interactive "p")
  (forward-thing (focus-get-thing) (+ 1 n))
  (let ((bounds (focus-bounds)))
    (goto-char (/ (+ (car bounds) (cdr bounds)) 2)))
  (recenter nil))

(defun focus-prev-thing (&optional n)
  "Moves the point to the middle of the Nth previous thing."
  (interactive "p")
  (focus-next-thing (- (+ 2 n))))
;;;###autoload
(define-minor-mode focus-mode
  "Dim the font color of text in surrounding sections."
  :init-value nil
  (if focus-mode (focus-init) (focus-terminate)))

(provide 'focus)
;;; focus.el ends here

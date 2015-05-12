(require 'cl-lib)

(defvar-local focus-pre-overlay nil
  "The overlay that dims the text prior to the current-point.")

(defvar-local focus-post-overlay nil
  "The overlay that dims the text past the current-point.")

(defun focus-search-backward (regex)
  "A wrapper for re-search-backward, where the point does not move,
and if the search fails, it returns NIL."
  (save-excursion (re-search-backward regex nil t)))

(defun focus-search-forward (regex)
  "A wrapper for re-search-backward, where the point does not move,
and if the search fails, it returns NIL."
  (save-excursion (re-search-forward regex nil t)))

(defun focus-average-colors (color &rest colors)
  "This function takes one or more colors and returns the average
of RGB values of the given colors."
  (let* ((colors (cons color colors))
         (colors (mapcar 'color-name-to-rgb colors))
         (len    (length colors))
         (sums   (apply 'cl-mapcar '+ colors))
         (avg    (mapcar (lambda (v) (/ v len)) sums)))
    (apply 'color-rgb-to-hex avg)))

(defun focus-move-focus ()
  "If `focus-mode' is enabled, this command fires after each
command, and moves the dimming overlays."
  (let* ((pre  (or (focus-search-backward "^\n") (point-min)))
         (post (or (focus-search-forward  "^\n") (point-max))))
    (move-overlay focus-pre-overlay  (point-min) pre)
    (move-overlay focus-post-overlay post (point-max))))

(defun focus-init ()
  "This function is run when focus-mode is enabled. It sets the
`focus-pre-overlay' and `focus-post-overlay' to overlays; these
are invisible until `focus-move-focus' is run. It adds
focus-move-focus to `post-command-hook'."
  (setq focus-pre-overlay  (make-overlay (point-min) (point-min))
        focus-post-overlay (make-overlay (point-max) (point-max)))
  (let ((color (focus-average-colors
                (face-attribute 'default :foreground)
                (face-attribute 'default :background))))
    (mapc (lambda (o) (overlay-put o 'face (cons 'foreground-color color)))
          (list focus-pre-overlay focus-post-overlay)))
  (add-hook 'post-command-hook 'focus-move-focus nil t))

(defun focus-terminate ()
  "When `focus-mode' is disabled the overlays pointed to by
`focus-pre-overlay' and `focus-post-overlay' are deleted, and
`focus-move-focus' is removed from `post-command-hook'."
  (progn (mapc 'delete-overlay (list focus-pre-overlay focus-post-overlay))
         (remove-hook 'post-command-hook 'focus-move-focus t)))

;;;###autoload
(define-minor-mode focus-mode
  "Dim the font color text in surrounding paragraphs."
  :init-value nil
  (if focus-mode (focus-init) (focus-terminate)))

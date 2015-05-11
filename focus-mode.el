(require 'cl-lib)

(defvar-local focus-pre-overlay nil)
(defvar-local focus-post-overlay nil)

(defun focus-search-backward (regex)
  (save-excursion (re-search-backward regex nil t)))

(defun focus-search-forward (regex)
  (save-excursion (re-search-forward regex nil t)))

(defun focus-average-colors (color &rest colors)
  (let* ((colors (cons color colors))
         (colors (mapcar 'color-name-to-rgb colors))
         (len    (length colors))
         (sums   (apply 'cl-mapcar '+ colors))
         (avg    (mapcar (lambda (v) (/ v len)) sums)))
    (apply 'color-rgb-to-hex avg)))

(defun focus-move-focus ()
  (let* ((pre  (or (focus-search-backward "^\n") (point-min)))
         (post (or (focus-search-forward  "^\n") (point-max))))
    (move-overlay focus-pre-overlay  (point-min) pre)
    (move-overlay focus-post-overlay post (point-max))))

(defun focus-init ()
  (setq focus-pre-overlay  (make-overlay (point-min) (point-min))
        focus-post-overlay (make-overlay (point-max) (point-max)))
  (let ((color (focus-average-colors
                (face-attribute 'default :foreground)
                (face-attribute 'default :background))))
    (mapc (lambda (o) (overlay-put o 'face (cons 'foreground-color color)))
          (list focus-pre-overlay focus-post-overlay)))
  (add-hook 'post-command-hook 'focus-move-focus nil t))

(defun focus-terminate ()
  (progn (mapc 'delete-overlay (list focus-pre-overlay focus-post-overlay))
         (remove-hook 'post-command-hook 'focus-move-focus t)))

;;;###autoload
(define-minor-mode focus-mode
  "Dim the font color text in surrounding paragraphs."
  :init-value nil
  (if focus-mode (focus-init) (focus-terminate)))

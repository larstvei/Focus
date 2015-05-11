(defvar-local focus-pre-overlay nil)
(defvar-local focus-post-overlay nil)

(defun focus-hexstr-to-int (str)
  (read (concat "#x" (substring str 1))))

(defun focus-average-colors (hstr1 hstr2 &rest hstrs)
  (let* ((strs (cons hstr1 (cons hstr2 hstrs)))
         (avg (/ (apply '+ (mapcar 'focus-hexstr-to-int strs)) 2)))
    (format "#%X" avg)))

(defun focus-move-focus ()
  (save-excursion
    (let* ((pre  (or (re-search-backward "^\n" nil t) 1))
           (post (1- (or (re-search-forward "^\n" nil t 2)
                         (1+ (point-max))))))
      (move-overlay focus-pre-overlay  (point-min) pre)
      (move-overlay focus-post-overlay post (point-max)))))

;;;###autoload
(define-minor-mode focus-mode
  "Dim the font color text in surrounding paragraphs."
  :init-value nil
  (if (not focus-mode)
      (progn (mapc 'delete-overlay (list focus-pre-overlay focus-post-overlay))
             (remove-hook 'post-command-hook 'focus-move-focus t))
    (setq focus-pre-overlay  (make-overlay (point-min) (point-min))
          focus-post-overlay (make-overlay (point-max) (point-max)))
    (let ((color (focus-average-colors
                  (face-attribute 'default :foreground)
                  (face-attribute 'default :background))))
      (mapc (lambda (o) (overlay-put o 'face (cons 'foreground-color color)))
            (list focus-pre-overlay focus-post-overlay)))
    (add-hook 'post-command-hook 'focus-move-focus nil t)))

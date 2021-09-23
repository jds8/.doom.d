;; https://jingsi.space/post/2017/04/05/organizing-a-complex-directory-for-emacs-org-mode-and-deft/
(defun my-deft/deft-file-relative-directory (filename)
  (file-name-directory (file-relative-name filename deft-directory)))

(defun my-deft/title-prefix-from-file-name (filename)
  (let ((reldir (my-deft/deft-file-relative-directory filename)))
    (if reldir
        (concat (directory-file-name reldir) " > "))))

(defun my-deft/parse-title-with-directory-prepended (orig &rest args)
  (let ((filename (car args)))
    (concat
      (my-deft/title-prefix-from-file-name filename)
            (rm-leading-chars "-" (underscore-to-space-string (rm-digits-string (file-name-nondirectory filename)))))))

(provide 'my-deft)

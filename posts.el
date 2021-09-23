;;; posts.el -*- lexical-binding: t; -*-

(require 'ox-publish)
(setq org-publish-project-alist
    '(
    ("org-posts"
     :base-directory "~/OneDrive - UBC/Notes/"
     :base-extension "org"
     :publishing-directory "~/Desktop/Things/Grad School/UBC/jekyll-faculty/_posts/"
     :recursive t
     :publishing-function org-html-publish-to-html
     :headline-levels 4             ; Just the default for this project.
     :auto-preamble t
     :body-only t ;; Only export section between <body> </body>
    )
    ("org-static"
     :base-directory "~/OneDrive - UBC/Notes/"
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
     :publishing-directory "~/Desktop/Things/Grad School/UBC/jekyll-faculty/_posts/"
     :recursive t
     :publishing-function org-publish-attachment
    )
    ("org" :components ("org-posts" "org-static")))
)

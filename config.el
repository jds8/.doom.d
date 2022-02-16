;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Justice Sefas"
      user-mail-address "jsefas@cs.ubc.ca")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-badger)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; evil
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "H") 'evil-first-non-blank))
(setq yas-triggers-in-field t)

(after! evil
  (map! :nv "g`" (evil-define-operator my/evil-titlecase-operator (beg end)
                   (interactive "<r>")
                   (save-excursion
                     (set-mark beg)
                     (goto-char end)
                     (titlecase-dwim)))))

;; elisp functions
(load! "string-lib")
(defun join (delim ls) (if (eq nil ls) nil (concat (car ls) delim (join delim (cdr ls)))))
(defun head (str) (substring str 0 1))
(defun tail (str) (substring str 1 nil))
(defun removesuffix (ch str) (if (string= "" str) str (if (string= ch (head str)) "" (concat (head str) (removesuffix ch (tail str))))))
(defun circumpend (a ls) (if (eq nil ls) ls (cons (concat a (string-trim (removesuffix "=" (car ls))) " = " (removesuffix ":" (car ls))) (circumpend a (cdr ls)))))
(defun copy-buffer-name () (interactive) (kill-new (buffer-file-name)))
(defun copy-relative-file-name () (interactive) (kill-new (file-name-nondirectory buffer-file-name)))
(defun get-level (str) (downcase (replace-regexp-in-string ".national.*" "national" (replace-regexp-in-string ".*femaregion.*" "femaregion" (replace-regexp-in-string ".*censusregion.*" "censusregion" (replace-regexp-in-string ".*state.*" "state" (replace-regexp-in-string ".*county.*" "county" (replace-regexp-in-string ".*cbsa.*" "cbsa" str))))))))
(defun name-to-prk (str) (concat "PrimaryKeyColumns." (let ((level (get-level str))) (cond ((string= level "county") "FIPS") ((string= level "cbsa") "CBSAFP") ((string= level "state") "STATEABBR") ((string= level "censusregion") "CENSUS_REGION_NUM") ((string= level "femaregion") "FEMA_REGION_NUM") ((string= level "national") "NATIONAL") (t level)) )))
(defun to-spaces (str) (make-string (length str) ?\ ))

   ;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun insert-current-time ()
  (interactive)
  (org-insert-time-stamp (current-time) t))

   ;; soure: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; Define Macros
(fset 'lively-macro
   (kmacro-lambda-form [?a ?  escape ?  ?: ?l ?i ?v ?e ?l ?y return] 0 "%d"))
(fset 'pdf-to-note-and-view
   (kmacro-lambda-form [?  ?z ?P ?  ?f ?z ?\C-w ?V ?\C-w ?h ?  ?d ?/ ?N ?o ?t ?e ?s return return ?/ ?R ?e ?s ?e ?r ?a backspace backspace ?a ?r ?c ?h return return ?/ ?\s-v return return] 0 "%d"))
(fset 'pdf-to-note
   (kmacro-lambda-form [?  ?f ?z ?  ?z ?c ?\s-v ?- ?n ?o ?t ?e ?s return ?r escape ?\C-w ?k ?  ?f ?y ?\C-w ?j ?i ?\[ ?\[ ?\s-v ?\] ?\[ ?p ?a ?p ?e ?r escape ?\C-c ?\C-c] 0 "%d"))
(fset 'org-export-to-posts
   (kmacro-lambda-form [?  ?m ?e ?P ?f] 0 "%d"))
(fset 'usebox
   (kmacro-lambda-form [?  ?f ?f ?~ ?/ ?\C-? ?. ?m ?i ?n ?i ?o ?a ?n ?a ?l ?y ?s ?t ?/ ?c ?o ?n ?f ?. ?y ?m ?l ?\C-m ?: ?2 ?\C-m ?f ?  ?d ?$ ?a ?  ?T ?r ?u ?e escape ?: ?w ?\C-m ?\C-x ?\C-\[ ?O ?D] 0 "%d"))
(fset 'uses3
   (kmacro-lambda-form [?  ?f ?f ?~ ?. ?m ?i ?n ?i ?o ?a ?n ?a ?l ?y ?s ?t ?/ ?c ?o ?n ?f ?. ?y ?m ?l ?\C-m ?: ?2 ?\C-m ?f ?: ?d ?$ ?a ?: ?  ?F ?a ?l ?s ?e escape ?: ?w ?\C-m ?\C-x ?\C-\[ ?O ?D] 0 "%d"))
(fset 'safeshell
   (kmacro-lambda-form [?\C-\[ ?x ?u ?s ?e ?s ?3 ?\C-m ?\C-\[ ?x ?s ?h ?e ?l ?l ?\C-m ?i] 0 "%d"))
(fset 'cdu
   (kmacro-lambda-form [?\C-\[ ?x ?u ?s ?e ?s ?3 ?\C-m ?\C-\[ ?x ?s ?h ?e ?l ?l ?\C-m ?i ?c ?d ?  ?~ ?/ ?D ?e ?s ?k ?t ?o ?p ?/ ?W ?H ?\C-i ?c ?u ?r ?r ?\C-i ?\C-m ?c ?o ?v ?i ?d ?o ?p ?s ?/ ?p ?r ?o ?d ?u ?c ?t ?\C-i ?c ?o ?v ?i ?d ?_ ?d ?a ?i ?l ?y ?_ ?u ?p ?d ?\C-i ?\C-m ?d ?t ?  ?= ?  ?$ ?\( ?d ?a ?t ?e ?  ?\C-\[ ?\[ ?O ?\C-\[ ?\[ ?I ?- ?v ?  ?- ?1 ?d ?  ?\' ?+ ?% ?Y ?% ?m ?% ?d ?\C-m ?\C-\[ ?O ?A ?\C-\[ ?b ?\C-\[ ?b ?\C-\[ ?b ?\C-\[ ?b ?\C-\[ ?O ?D ?\C-\[ ?O ?D ?\C-? ?\C-\[ ?O ?D ?\C-m ?\C-\[ ?O ?A ?\C-\[ ?b ?\C-\[ ?b ?\C-\[ ?b ?\C-\[ ?b ?\C-\[ ?b ?\C-\[ ?O ?D ?\C-? ?\C-m ?p ?y ?t ?h ?o ?n ?  ?c ?o ?v ?\C-i ?i ?d ?_ ?d ?a ?i ?l ?y ?_ ?u ?p ?d ?a ?t ?e ?. ?p ?y ?\C-m ?  ?- ?d ?  ?$ ?d ?t ?\C-m] 0 "%d"))
(global-set-key (kbd "C-c C-d C-u") 'cdu)
(fset 'cpr
   (kmacro-lambda-form [?\C-\[ ?x ?s ?h ?\C-? ?a ?f ?s ?\C-? ?e ?s ?h ?\C-m ?d ?d ?= ?$ ?\( ?d ?a ?t ?e ?  ?- ?v ?  ?- ?1 ?d ?  ?\' ?+ ?% ?Y ?% ?m ?% ?d ?\' ?\) ?\C-m ?c ?d ?  ?~ ?/ ?D ?e ?s ?\C-? ?\C-? ?\C-? ?\C-? ?/ ?D ?e ?s ?k ?t ?o ?p ?/ ?c ?u ?r ?r ?e ?n ?t ?- ?o ?p ?e ?r ?a ?t ?i ?o ?n ?s ?/ ?\C-\[ ?b ?\C-\[ ?b ?W ?H ?T ?F ?/ ?\C-\[ ?f ?\C-\[ ?f ?\C-\[ ?f ?c ?o ?v ?i ?d ?o ?p ?s ?/ ?p ?r ?o ?d ?u ?c ?t ?/ ?c ?p ?r ?/ ?c ?p ?r ?s ?l ?i ?d ?e ?s ?\C-m ?l ?s ?\C-m ?p ?y ?t ?h ?o ?n ?  ?c ?p ?r ?. ?p ?y ?  ?- ?d ?  ?$ ?d ?d ?\C-m] 0 "%d"))
(global-set-key (kbd "C-c C-p C-r") 'cpr)
(fset 'gov
   (kmacro-lambda-form [?\C-\[ ?x ?u ?s ?e ?b ?o ?x ?\C-m ?  ?f ?f ?~ ?/ ?D ?e ?\C-? ?\C-? ?\C-? ?D ?e ?s ?k ?t ?o ?p ?/ ?W ?H ?T ?F ?/ ?c ?u ?r ?r ?e ?n ?t ?- ?o ?p ?e ?r ?a ?t ?i ?o ?n ?s ?/ ?G ?o ?v ?e ?r ?n ?o ?r ?S ?l ?i ?d ?e ?s ?/ ?c ?o ?n ?f ?i ?g ?. ?y ?m ?l ?\C-m ?: ?1 ?0 ?\C-m ?f ?  ?d ?$ ?a ?  ?T ?r ?u ?e escape ?: ?w ?\C-m ?\C-\[ ?x ?s ?h ?e ?l ?l ?\C-m ?i ?\C-m ?c ?d ?  ?~ ?/ ?D ?e ?s ?k ?t ?o ?p ?/ ?c ?u ?r ?r ?\C-? ?\C-? ?\C-? ?\C-? ?W ?H ?T ?F ?/ ?c ?u ?r ?r ?e ?n ?t ?- ?o ?p ?e ?r ?a ?t ?i ?o ?n ?s ?/ ?G ?o ?v ?e ?r ?n ?o ?r ?S ?l ?i ?d ?e ?s ?\C-m ?j ?u ?p ?y ?t ?e ?r ?  ?n ?o ?t ?e ?b ?o ?o ?k ?  ?M ?a ?\C-? ?a ?k ?e ?G ?o ?v ?\C-i ?\C-m ?\C-m] 0 "%d"))
(fset 'vax
   (kmacro-lambda-form [?  ?f ?d ?~ ?/ ?D ?e ?s ?\C-? ?\C-? ?\C-? ?\C-? ?\C-d ?\C-d ?D ?e ?s ?k ?t ?o ?p ?/ ?c ?u ?r ?\C-? ?\C-? ?\C-? ?W ?H ?T ?F ?/ ?c ?u ?r ?r ?e ?n ?t ?\C-i ?/ ?c ?o ?v ?i ?d ?o ?p ?s ?/ ?v ?a ?c ?c ?\C-? ?\C-? ?\C-? ?\C-? ?d ?a ?t ?a ?/ ?v ?a ?c ?c ?i ?n ?a ?t ?i ?o ?n ?/ ?p ?i ?p ?e ?l ?i ?n ?e ?\C-m] 0 "%d"))
(fset 'pln
   (kmacro-lambda-form [?\C-\[ ?x ?s ?a ?f ?e ?\C-m ?\C-? ?p ?y ?t ?h ?o ?n ?  ?\C-? ?\C-? ?\C-? ?\C-? ?\C-? ?\C-? ?\C-? ?d ?d ?= ?$ ?\( ?d ?a ?t ?e ?  ?- ?v ?  ?- ?1 ?d ?  ?\' ?+ ?% ?Y ?% ?m ?% ?d ?\C-m ?p ?y ?t ?h ?o ?n ?  ?- ?d ?  ?$ ?d ?d ?\C-\[ ?O ?D ?\C-\[ ?O ?D ?\C-\[ ?O ?D ?\C-\[ ?O ?D ?\C-\[ ?O ?D ?\C-\[ ?O ?D ?\C-\[ ?O ?D ?  ?~ ?/ ?D ?e ?s ?k ?t ?o ?p ?/ ?c ?u ?r ?r ?\C-? ?\C-? ?\C-? ?\C-? ?W ?H ?T ?\C-i ?c ?u ?r ?r ?e ?\C-i ?\C-m ?c ?o ?v ?i ?\C-i ?\C-m ?d ?a ?t ?a ?/] 0 "%d"))
(global-set-key (kbd "C-c C-p C-l C-n") 'pln)
(fset 'runf
   (kmacro-lambda-form [?\C-\[ ?x ?s ?a ?f ?e ?\C-m ?\C-m escape ?\C-w ?h ?\C-c ?\C-b ?\C-w ?l ?p ?0 ?i ?p ?y ?t ?h ?o ?n ?  ?$ ?\C-? escape ?$ ?\C-\[ ?\[ ?O ?\C-\[ ?\[ ?I ?\C-\[ ?\[ ?O ?\C-\[ ?\[ ?I ?\C-\[ ?\[ ?O ?\C-\[ ?\[ ?I ?a ?  ?- ?d ?  ?\C-\[ ?\[ ?O ?\C-\[ ?\[ ?I ?\C-\[ ?\[ ?O ?\C-\[ ?\[ ?I ?$ ?\( ?d ?a ?t ?e ?  ?- ?v ?  ?- ?1 ?d ?  ?\' ?+ ?% ?Y ?% ?m ?% ?d ?\C-m] 0 "%d"))
(global-set-key (kbd "C-c C-f") 'runf)
(fset 'run-test
   (kmacro-lambda-form [?\' ?T ?  ?m ?t ?t ?\C-o] 0 "%d"))
(global-set-key (kbd "C-c t") 'run-test)
(fset 'm_macro
   (kmacro-lambda-form [?d ?d ?` ?m ?p ?$ ?m ?m] 0 "%d"))
(evil-set-register ?m [?d ?d ?` ?m ?p ?$ ?m ?m])
(fset 'wrap_fun
   (kmacro-lambda-form [?y ?s ?i ?o ?\) ?i ?\C-r ?. escape] 0 "%d"))
(global-set-key (kbd "C-c f") 'wrap_fun)
(fset 'range
   (kmacro-lambda-form [?c ?i ?W ?r ?a ?n ?g ?e ?\( ?\C-r ?\" ?\) escape] 0 "%d"))
(global-set-key (kbd "C-c r") 'range)

;; Keybindings
(global-set-key (kbd "C-c <right>") 'next-buffer)
(global-set-key (kbd "C-c <left>") 'previous-buffer)
(global-set-key (kbd "C-x t t") 'tab-new)
(global-set-key (kbd "C-x t n") 'tab-next)
(global-set-key (kbd "C-x t p") 'tab-previous)
(global-set-key (kbd "C-x t r") 'tab-rename)
(global-set-key (kbd "C-x t x") 'tab-close)
(global-set-key (kbd "C-x C-k") '(lambda ()(interactive) (kill-buffer nil)))
(global-set-key (kbd "C-l") 'org-latex-preview)

;; org ref
(map! :leader
      :desc "insert ref"
      "n i" 'org-ref-helm-insert-cite-link)

;; org export
(map! :leader
      (:prefix ("e" . "export"))
      :desc "Export Org to LaTeX"
      "e l" #'org-latex-export-to-pdf)
(map! :leader
      (:prefix ("e" . "export"))
      :desc "Export Org to Beamer"
      "e b" #'org-beamer-export-to-pdf)
(map! :leader
      (:prefix ("e" . "export"))
      :desc "Export Org to Posts"
      "e p" #'org-export-to-posts)

;; dates
(map! :leader
      :desc "Insert current date (%Y-%m-%d)"
      "i d" #'insert-current-date)
(map! :leader
      :desc "Insert current date (%Y-%m-%d)"
      "i t" #'insert-current-time)
(map! :leader
      :desc "Insert digraph"
      "i k" #'evil-insert-digraph)

;; Zettelkasten
(map! :leader
      (:prefix ("z" . "Zettelkasten"))
      :desc "Org Roam Alias"
      "z a" #'org-roam-alias-add)
(map! :leader
      :desc "Org Roam Capture"
      "z c" #'org-roam-capture)
(map! :leader
      :desc "Org Roam Insert"
      "z i" #'org-roam-node-insert)
(map! :leader
      :desc "Org Roam Insert Immediate"
      "z I" #'org-roam-node-insert-immediate)
(map! :leader
      :desc "Org Roam Id Get Create"
      "z d" #'org-id-get-create)
(map! :leader
      :desc "Org Roam Ref"
      "z r" #'org-roam-ref-add)
(map! :leader
      :desc "Org Roam Ref Find"
      "z R" #'org-roam-ref-find)
(map! :leader
      :desc "Org Add Link"
      "z l" #'org-insert-link)
(map! :leader
      :desc "Org Roam Find"
      "z f" #'org-roam-node-find)
(map! :leader
      :desc "Org Roam Toggle"
      "z t" #'org-roam-buffer-toggle)
(map! :leader
      :desc "Create Note from PDF"
      "z P" #'pdf-to-note)
(map! :leader
      :desc "Create Note from PDF and display"
      "z p" #'pdf-to-note-and-view)
(map! :leader
      :desc "Org Roam Today's Journal"
      "z j" #'org-roam-dailies-goto-today)
(map! :leader
      :desc "Org Roam Find Journal"
      "z J" #'org-roam-dailies-find-date)
(map! :leader
      :desc "Org Mind Map"
      "z m" #'org-mind-map-write)

;; misc
(map! :leader
      :desc "Yank file name base"
      "f z" #'(lambda () (interactive) (let ((base (file-name-base buffer-file-name))) (progn (kill-new base) (message "%s" (concat "Copied base name to clipboard: " base))))))
(map! :leader
      :desc "Yank relative filepath"
      "f Y" #'copy-relative-file-name)
(map! :leader
      :desc "Avy"
      "j" '(lambda (&rest _)
            (interactive)
            (let ((current-prefix-arg t)) (evil-avy-goto-char-timer))))
(map! :leader
      :desc "Dired of snippets"
      "s y" #'(lambda ()(interactive) (dired "~/.doom.d/snippets/")))
(map! :leader
      :desc "Dired of One Drive - UBC"
      "d" #'(lambda ()(interactive) (dired "~/OneDrive - UBC")))

;; org noter
(map! :leader
      (:prefix ("k" . "Org-Noter"))
      :desc "start session"
      "k s" 'org-noter)
(map! :leader
      :desc "create skeleton"
      "k k" 'org-noter-create-skeleton)
(map! :leader
      :desc "insert note"
      "k i" 'org-noter-insert-note)
(map! :leader
      :desc "add highlight"
      "k h" 'pdf-annot-add-highlight-markup-annotation)
(map! :leader
      :desc "delete annotation"
      "k d" 'pdf-annot-delete)


;; Yapf on save
;; (add-hook 'python-mode-hook 'yapf-mode)

;; Julia hook
(after! julia-mode
  (add-hook! 'julia-mode-hook
    (setq-local lsp-enable-folding t
                lsp-folding-range-limit 100)))

;; LaTeX config
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; For mac set option to meta
(setq mac-right-option-modifier 'meta)

;; TeX Pdf
(setq TeX-PDF-mode t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(pdf-loader-install)

;; Org
(setq org-directory "~/OneDrive - UBC")
(setq agenda-directory (file-name-as-directory org-directory))
(setq notes-directory (concat (file-name-as-directory org-directory) "Notes"))
(setq dailies-directory (concat (file-name-as-directory org-directory) "Dailies"))
(setq org-agenda-files (directory-files-recursively agenda-directory "\\.org$"))
(setq org-priority-default 10)
(setq org-priority-highest 1)
(setq org-priority-lowest 10)
(load! "posts")

;; Org Roam config
(setq org-roam-directory notes-directory)
(setq org-roam-db-location (concat (file-name-as-directory org-roam-directory) "org-roam.db"))
(setq org-roam-completion-everywhere t)
(setq completion-ignore-case t)
(setq org-roam-capture-templates
      `(("d" "Default" plain
         "%?"
         :if-new (file+head "%<%Y-%m-%d>-${slug}.org"
               ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: Justice Sefas\n"
                        "#+OPTIONS: toc:nil num:nil tex:t html-postamble:nil\n\n"
                        "#+LATEX_HEADER: \\usepackage{amsfonts}\n"
                        "#+LATEX_HEADER: \\usepackage{physics}"
                        "#+LATEX_HEADER: \\usepackage{bbm}"
                        "#+LATEX_HEADER: \\setlength{\parindent}{0pt}\n\n"))
         :unnarrowed t)
        ("r" "Research" plain
         "%?"
         :if-new (file+head "Research/%<%Y-%m-%d>-${slug}.org"
               ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: Justice Sefas\n"
                        "#+OPTIONS: toc:nil num:nil tex:t html-postamble:nil\n\n"
                        "#+LATEX_HEADER: \\usepackage{amsfonts}\n"
                        "#+LATEX_HEADER: \\usepackage{physics}\n\n"
                        "#+LATEX_HEADER: \\usepackage{bbm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\parindent}{0pt}\n\n"
                        "* Valuable Terminology\n\n"
                        "* Notes"))
         :unnarrowed t)
        ("c" "Class" plain
         "%?"
         :if-new (file+head "Class/%<%Y-%m-%d>-${slug}.org"
               ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: Justice Sefas\n"
                        "#+OPTIONS: toc:nil num:nil tex:t html-postamble:nil\n\n"
                        "#+LATEX_HEADER: \\usepackage{amsfonts}\n"
                        "#+LATEX_HEADER: \\usepackage{physics}\n\n"
                        "#+LATEX_HEADER: \\usepackage{bbm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\parindent}{0pt}\n\n"
                        "* Valuable Terminology\n\n"
                        "* Notes"))
         :unnarrowed t)
        ("n" "Running" plain
         "%?"
         :if-new (file+head "../Running/%<%Y-%m-%d>-${slug}.org"
               ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: Justice Sefas\n"
                        "#+OPTIONS: toc:nil num:nil tex:t html-postamble:nil\n\n"
                        "#+LATEX_HEADER: \\usepackage{amsfonts}\n"
                        "#+LATEX_HEADER: \\usepackage{physics}\n\n"
                        "#+LATEX_HEADER: \\usepackage{bbm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\parindent}{0pt}\n"
                        "* Today's Workout\n\n"
                        "* Goals"))
         :unnarrowed t)
        ("h" "History" plain
         "%?"
         :if-new (file+head "History/%<%Y-%m-%d>-${slug}.org"
               ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: Justice Sefas\n"
                        "#+OPTIONS: toc:nil num:nil tex:t html-postamble:nil\n\n"
                        "#+LATEX_HEADER: \\usepackage{amsfonts}\n"
                        "#+LATEX_HEADER: \\usepackage{physics}\n\n"
                        "#+LATEX_HEADER: \\setlength{\parindent}{0pt}\n"
                        "* Title\n"))
         :unnarrowed t)
        ("b" "Beamer" plain
         "%?"
         :if-new (file+head "Beamer/%<%Y-%m-%d>-${slug}.org"
               ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: Justice Sefas\n"
                        "#+OPTIONS: toc:nil num:nil tex:t html-postamble:nil\n\n"
                        "#+LATEX_HEADER: \\usepackage{amsfonts}\n"
                        "#+LATEX_HEADER: \\usepackage{physics}\n\n"
                        "#+LATEX_HEADER: \\usepackage{bbm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\parindent}{0pt}\n\n"
                        "#+KEYWORDS:\n"
                        "#+LANGUAGE:  en\n"
                        "#+OPTIONS:   H:1 num:t toc:nil \\n:nil @:t ::t |:t ^:t -:t f:t *:t <:t\n"
                        "#+OPTIONS:   TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc\n"
                        "#+INFOJS_OPT: view:nil toc:nil ltoc:t mouse:underline buttons:0 path:https://orgmode.org/org-info.js\n"
                        "#+EXPORT_SELECT_TAGS: export\n"
                        "#+EXPORT_EXCLUDE_TAGS: noexport\n"
                        "#+HTML_LINK_UP:\n"
                        "#+HTML_LINK_HOME:\n"
                        "#+STARTUP: beamer\n"
                        "#+LaTeX_CLASS: beamer\n"
                        "#+COLUMNS: %40ITEM %10BEAMER_env(Env) %9BEAMER_envargs(Env Args) %4BEAMER_col(Col) %10BEAMER_extra(Extra)\n\n"
                        "* ${title}\n"))
         :unnarrowed t)
         ("p" "Post" plain
         "%?"
         :if-new (file+head "Research/%<%Y-%m-%d>-${slug}.org"
               ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: Justice Sefas\n"
                        "#+OPTIONS: toc:nil num:nil tex:t html-postamble:nil\n\n"
                        "#+LATEX_HEADER: \\usepackage{amsfonts}\n"
                        "#+LATEX_HEADER: \\usepackage{physics}\n\n"
                        "#+LATEX_HEADER: \\usepackage{bbm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\parindent}{0pt}\n\n"
                        "#+BEGIN_EXPORT html\n"
                        "---\n"
                        "layout: post\n"
                        "title: ${title}\n"
                        "---\n"
                        "#+END_EXPORT\n\n"
                        "* ${title}\n"))
         :unnarrowed t)
       ))

(setq org-roam-dailies-directory dailies-directory)
(setq org-roam-dailies-capture-templates
      '(("d" "Daily" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n"))))
(org-roam-setup)

; Deft note-taking
(load! "my-deft")
(setq deft-directory org-directory)
(setq deft-extensions '("org" "txt" "md"))
(setq deft-default-extension "org")
(setq deft-recursive t)
;; (setq deft-use-filename-as-title nil)
;; (setq deft-use-filter-string-for-filename t)
;; (setq deft-file-naming-rules
;;       '((noslash . "-")
;;         (nospace . "-")
;;         (case-fn . downcase)))
(advice-add 'deft-parse-title :around #'my-deft/parse-title-with-directory-prepended)


;; Don't make Projectile try to figure out project names
(setq projectile-mode-line "Projectile")

;; Tramp
(setq tramp-default-method "ssh")

;; lively
(load! "lively")
(map! :leader
      :desc "lively"
      "l" #'lively-macro)

;; Use BibLaTex
(setq bibtex-dialect 'biblatex)

;; ivy-bibtex
(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(setq papers-directory (concat (file-name-as-directory org-directory) "Papers"))
(setq bibtex-completion-library-path papers-directory)

(setq bib-directory (concat (file-name-as-directory org-directory) "Bibliographies"))
(setq bibtex-completion-bibliography (concat (file-name-as-directory bib-directory) "references.bib"))
(setq bibtex-completion-library-path `("/Users/MacMag/Zotero/storage/5EA58VE6" ,(concat (file-name-as-directory bib-directory) "bibtex-pdfs")))
(setq bibtex-completion-pdf-field "file")

;; org-ref
(setq org-ref-completion-library 'org-ref-ivy-cite)
(setq org-ref-default-bibliography (list bibtex-completion-bibliography))
(setq org-ref-default-cite-link org-ref-default-bibliography)

;; org-noter
(setq research-directory (concat (file-name-as-directory notes-directory) "Research"))
(setq org-noter-notes-search-path (list research-directory))

;; org-mind-map
(require 'ox-org)
(setq org-mind-map-engine "dot")       ; Default. Directed Graph
;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
;; (setq org-mind-map-engine "twopi")  ; Radial Layout
;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
;; (setq org-mind-map-engine "twopi")  ; Radial layouts
;; (setq org-mind-map-engine "circo")  ; Circular Layout

;; org-inline-image
(defun org-inline-image--get-current-image ()
  "Return the overlay associated with the image under point."
  (car (--select (eq (overlay-get it 'org-image-overlay) t) (overlays-at (point)))))

(defun org-inline-image--get (prop)
  "Return the value of property PROP for image under point."
  (let ((image (org-inline-image--get-current-image)))
    (when image
      (overlay-get image prop))))

(defun org-inline-image-animate ()
  "Animate the image if it's possible."
  (interactive)
  (let ((image-props (org-inline-image--get 'display)))
    (when (image-multi-frame-p image-props)
      (image-animate image-props))))

(defun org-inline-image-animate-auto ()
  (interactive)
  (when (eq 'org-mode major-mode)
    (while-no-input
      (run-with-idle-timer 0.3 nil 'org-inline-image-animate))))

(setq org-inline-image--get-current-image (byte-compile 'org-inline-image--get-current-image))
(setq org-inline-image-animate  (byte-compile 'org-inline-image-animate ))
(add-hook 'post-command-hook 'org-inline-image-animate-auto)

;; org scale images
(setq org-image-actual-width nil)

;; compute jobs
(setq jobs-file (concat (file-name-as-directory org-directory) "Notes/Research/jobs.org"))
(map! :leader
      :desc "List of compute jobs"
      "n ;" #'(lambda ()(interactive) (find-file jobs-file)))

;; taxonomy of jobs
(require 'taxy)

(defvar jobs-taxy
  (make-taxy
   :name "Jobs"
   :description "Taxonomy of jobs"
   :taxys (list (make-taxy
                 :name "Running"
                 :predicate (lambda (j) (string-match "(R)" j)))
                (make-taxy
                 :name "Pending"
                 :predicate (lambda (j) (string-match "(PD)" j)))
                (make-taxy
                 :name "Finished"
                 :predicate (lambda (j) (string-match "(F)" j)))
                (make-taxy
                 :name "Cancelled"
                 :predicate (lambda (j) (string-match "(CG)" j)))
                 )))

;; iterates through headers that contain job statuses in jobs.org and then taxonomizes them
;; inserts the output into a new buffer called jobs-temp
(defun jobs-taxonomy ()
  (switch-to-buffer (make-temp-name "jobs-temp"))
  (insert
    (replace-regexp-in-string ")\\([^)]\\)" ")\\1\n"
      (format "%s" (let ((jobs (-filter '(lambda (x) (string-match "([A-z]+)" x)) (with-temp-buffer
  (org-mode)
  (insert-file-contents jobs-file)
  (goto-char (point-min))
  (org-map-entries '(lambda () (org-entry-get nil "ITEM"))))))
      ;; Since `numbery' is stored in a variable, we use an emptied
      ;; copy of it to avoid mutating the original taxy.
      (taxy (taxy-emptied jobs-taxy)))
                       (taxy-plain (taxy-fill jobs taxy)))))))

(map! :leader
      :desc "Insert jobs taxonomy"
      "n :" #'(lambda ()(interactive) (jobs-taxonomy)))

;; run current file
(defvar xah-run-current-file-before-hook nil "Hook for `xah-run-current-file'. Before the file is run.")
(defvar xah-run-current-file-after-hook nil "Hook for `xah-run-current-file'. After the file is run.")
(defvar xah-run-current-file-map nil "A association list that maps file extension to program path, used by `xah-run-current-file'. First element is file suffix, second is program name or path. You can add items to it.")
(setq
 xah-run-current-file-map
 '(
   ("php" . "php")
   ("pl" . "perl")
   ("py" . "python")
   ("py2" . "python2")
   ("py3" . "python3")
   ("rb" . "ruby")
   ("go" . "go run")
   ("hs" . "runhaskell")
   ("js" . "deno run")
   ("ts" . "deno run") ; TypeScript
   ("tsx" . "tsc")
   ("mjs" . "node --experimental-modules ")
   ("sh" . "bash")
   ("clj" . "java -cp ~/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
   ("rkt" . "racket")
   ("ml" . "ocaml")
   ("vbs" . "cscript")
   ("tex" . "pdflatex")
   ("latex" . "pdflatex")
   ("java" . "javac")
   ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
   ))
(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
Output is printed to buffer “*xah-run output*”.
File suffix is used to determine which program to run, set in the variable `xah-run-current-file-map'.

If the file is modified or not saved, save it automatically before run.

URL `http://xahlee.info/emacs/emacs/elisp_run_current_file.html'
Version 2020-09-24 2021-01-21"
  (interactive)
  (let (
        ($outBuffer "*xah-run output*")
        (resize-mini-windows nil)
        ($suffixMap xah-run-current-file-map )
        $fname
        $fSuffix
        $progName
        $cmdStr)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $fname (buffer-file-name))
    (setq $fSuffix (file-name-extension $fname))
    (setq $progName (cdr (assoc $fSuffix $suffixMap)))
    (setq $cmdStr (concat $progName " \""   $fname "\" &"))
    (run-hooks 'xah-run-current-file-before-hook)
    (cond
     ((string-equal $fSuffix "el")
      (load $fname))
     ((string-equal $fSuffix "go")
      (xah-run-current-go-file))
     ((string-equal $fSuffix "java")
      (progn
        (shell-command (format "javac %s" $fname) $outBuffer )
        (shell-command (format "java %s" (file-name-sans-extension
                                          (file-name-nondirectory $fname))) $outBuffer )))
     (t (if $progName
            (progn
              (message "Running")
              (shell-command $cmdStr $outBuffer ))
          (error "No recognized program file suffix for this file."))))
    (run-hooks 'xah-run-current-file-after-hook)))

(map! :leader
      :desc "Xah Run Current File"
      "f x" #'xah-run-current-file)

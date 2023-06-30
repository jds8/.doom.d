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

;; exec-path
(setq dm-path "/home/jsefas/driving-models/venv/bin/")
(setq exec-path (append exec-path `(,dm-path)))
(setq python-shell-interpreter (concat dm-path "python3"))

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

;; standard emacs bindings in evil insert mode
(define-key evil-insert-state-map (kbd "C-n") 'next-line)      ;; was 'evil-complete-next
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)  ;; was 'evil-complete-previous
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)  ;; was 'evil-complete-previous
(define-key evil-insert-state-map (kbd "C-=") 'text-scale-increase)  ;; was 'evil-complete-previous
(define-key evil-insert-state-map (kbd "C--") 'text-scale-decrease)  ;; was 'evil-complete-previous

;; window bindings
(define-key evil-window-map (kbd "z") '(lambda ()(interactive) (evil-window-bottom-right) (evil-window-left 10)))
(define-key evil-window-map (kbd "d") '(lambda ()(interactive) (evil-window-top-left) (evil-window-right 10)))  ;; was 'evil-window-delete
(define-key evil-window-map (kbd "M-h") 'flip-window-left)
(define-key evil-window-map (kbd "M-l") 'flip-window-right)

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

;; buffer functions
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

    ;; find buffer whose name contains with "needle"
(defun find-buffer-helper (needle haystack)
  (if (not haystack) nil
    (let ((buff (buffer-name (car haystack))))
    (let ((match (string-match needle buff)))
    (if match buff (find-buffer-helper needle (cdr haystack)))))))
(defun find-buffer (needle) (find-buffer-helper needle (buffer-list)))

    ;; remove new line characters from str
(defun remove-new-lines (str)
  (let ((value (replace-regexp-in-string "\n" " " str)))
    (replace-regexp-in-string " +" " " value)))

    ;; send the current region to the shell buffer
(defun send-flattened-region-to-shell () (interactive)
        (let ((value (buffer-substring (region-beginning) (region-end))))
          (progn
             (pop-to-buffer (find-buffer "shell"))
             (insert (remove-new-lines value))
             )))

    ;; send the current region to a user-specified buffer
(defun send-flattened-region-to-buffer (buffer flatten) (interactive "bSend to which buffer?\nnJoin lines? (0/1) ")
       (let ((value (buffer-substring (region-beginning) (region-end))))
         (let ((val (if (= 1 flatten) (remove-new-lines value) value)))
          (progn
             (pop-to-buffer (find-buffer buffer))
             (insert val)
             ))))

;; date and time functions
(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun insert-current-time ()
  (interactive)
  (forward-char)
  (insert " ")
  (org-insert-time-stamp (current-time) t))

   ;; soure: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; move current line up https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/ (Bozhidar Batsov)
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;; move current line down https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/ (Bozhidar Batsov)
(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; duplicate line: https://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

;; http://xahlee.info/emacs/emacs/emacs_move_by_paragraph.html
(defun xah-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://xahlee.info/emacs/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))

;; http://xahlee.info/emacs/emacs/emacs_move_by_paragraph.html
(defun xah-backward-block (&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://xahlee.info/emacs/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        ($i 1))
    (while (<= $i n)
      (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq $i n)))
      (setq $i (1+ $i)))))

;; Define Macros
(fset 'lively-macro
   (kmacro-lambda-form [?a ?  escape ?  ?: ?l ?i ?v ?e ?l ?y return] 0 "%d"))
(fset 'pdf-to-note-and-view
   (kmacro-lambda-form [?  ?z ?P ?  ?f ?z ?\C-w ?V ?\C-w ?h ?  ?d ?/ ?N ?o ?t ?e ?s return return ?/ ?R ?e ?s ?e ?r ?a backspace backspace ?a ?r ?c ?h return return ?/ ?\s-v return return] 0 "%d"))
(fset 'pdf-to-note
   (kmacro-lambda-form [?  ?f ?z ?  ?z ?c ?\s-v ?- ?n ?o ?t ?e ?s return ?r escape ?\C-w ?k ?  ?f ?y ?\C-w ?j ?i ?\[ ?\[ ?\s-v ?\] ?\[ ?p ?a ?p ?e ?r escape ?\C-c ?\C-c] 0 "%d"))
(fset 'org-export-to-posts
   (kmacro-lambda-form [?  ?m ?e ?P ?f] 0 "%d"))
(fset 'uses3
   (kmacro-lambda-form [?  ?f ?f ?~ ?. ?m ?i ?n ?i ?o ?a ?n ?a ?l ?y ?s ?t ?/ ?c ?o ?n ?f ?. ?y ?m ?l ?\C-m ?: ?2 ?\C-m ?f ?: ?d ?$ ?a ?: ?  ?F ?a ?l ?s ?e escape ?: ?w ?\C-m ?\C-x ?\C-\[ ?O ?D] 0 "%d"))
(fset 'run-test
   (kmacro-lambda-form [?\' ?T ?  ?m ?t ?t ?\C-o] 0 "%d"))
(fset 'm_macro
   (kmacro-lambda-form [?d ?d ?` ?m ?p ?$ ?m ?m] 0 "%d"))
(fset 'wrap_fun
   (kmacro-lambda-form [?y ?s ?i ?o ?\) ?i ?\C-r ?f escape] 0 "%d"))
(fset 'range
   (kmacro-lambda-form [?c ?t ?: ?r ?a ?n ?g ?e ?\( ?\C-r ?\" ?\) escape] 0 "%d"))
(fset 'insert-pdb
   (kmacro-lambda-form [escape ?H ?D ?a ?t ?r ?y ?: return ?\C-r ?\" return ?e ?x ?c ?e ?p ?t ?: return escape ? ?i ?s ?p ?d ?b return escape ?= ?= ?o ?\C-r ?\" escape] 0 "%d"))
(fset 'self
   (kmacro-lambda-form [escape ?o ?s ?e backspace backspace ?s ?e ?l ?f ?. ?\C-r ?0 ?  ?= ?  ?\C-r ?0 escape ?H] 0 "%d"))
(fset 'wrap_symbol_python
   (kmacro-lambda-form [escape ?y ?s ?i ?W ?\) ?i] 0 "%d"))
(fset 'wrap_symbol_org
   (kmacro-lambda-form [escape ?y ?s ?i ?o ?\} ?i ?\\] 0 "%d"))
(fset 'remove-function-call
   (kmacro-lambda-form [escape ?F ?\( ?h ?v ?i ?o ?d ?d ?s ?\(] 0 "%d"))
(fset 'boldify
   (kmacro-lambda-form [escape ?y ?s ?i ?W ?* ?e ?e ?a] 0 "%d"))
(fset 'find-method
   (kmacro-lambda-form [escape ?\C-\M-a ?f ?\( ?h ?*] 0 "%d"))
(fset 'flip-window-right
   (kmacro-lambda-form [escape ?\C-w ?L ?\C-w ?h] 0 "%d"))
(fset 'flip-window-left
   (kmacro-lambda-form [escape ?\C-w ?H ?\C-w ?l] 0 "%d"))
(fset 'increment-number
   (kmacro-lambda-form [escape ?V ?g ?= ?a] 0 "%d"))
(fset 'decrement-number
   (kmacro-lambda-form [escape ?V ?g ?- ?a] 0 "%d"))
(fset 'increase-text-size-other-window
   (kmacro-lambda-form [?\C-x ?o ?\C-= ?\C-x ?o] 0 "%d"))
(fset 'decrease-text-size-other-window
   (kmacro-lambda-form [?\C-x ?o ?\C-- ?\C-x ?o] 0 "%d"))
(fset 'python-argument
   (kmacro-lambda-form [escape ?y ?i ?w ?e ?a ?= ?\C-r ?\"] 0 "%d"))
(fset 'underbrace
   (kmacro-lambda-form [?c ?\\ ?u ?n ?d ?e ?r ?b ?r ?a ?c ?e ?\{ ?\C-r ?\" ?\} ?\_ ?{ ?} ?\C-b] 0 "%d"))
(fset 'find-string
   (kmacro-lambda-form [escape ?y ?i ?\" ?\\ ?\C-r ?\" return] 0 "%d"))

;; Keybindings
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c r") 'range)
  (define-key python-mode-map (kbd "C-c f") 'wrap_fun)
  (define-key python-mode-map (kbd "C-c d") 'insert-pdb)
  (define-key python-mode-map (kbd "C-c t") 'run-test)
  (define-key python-mode-map (kbd "C-c s") 'self)
  (define-key python-mode-map (kbd "C-;") 'er/expand-region)
  (define-key python-mode-map (kbd "C-:") 'er/contract-region)
  (define-key python-mode-map (kbd "C-c w") 'wrap_symbol_python)
  (define-key python-mode-map (kbd "C-c x") 'remove-function-call)
  (define-key python-mode-map (kbd "C-c v") 'send-flattened-region-to-shell)
  (define-key python-mode-map (kbd "C-c V") 'send-flattened-region-to-buffer)
  (define-key python-mode-map (kbd "C-c /") 'find-method)
  (define-key python-mode-map (kbd "M-n") 'move-line-down)
  (define-key python-mode-map (kbd "M-p") 'move-line-up)
  (define-key python-mode-map (kbd "C-c =") 'increment-number)
  (define-key python-mode-map (kbd "C-c -") 'decrement-number)
  (define-key python-mode-map (kbd "C-c a") 'python-argument)
  (define-key python-mode-map (kbd "C-c q") 'chatgpt-query)
  (define-key python-mode-map (kbd "C-c \"") 'find-string)
)
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-c r") 'range)
  (define-key comint-mode-map (kbd "C-c f") 'wrap_fun)
  (define-key comint-mode-map (kbd "C-c t") 'run-test)
  (define-key comint-mode-map (kbd "C-;") 'er/expand-region)
  (define-key comint-mode-map (kbd "C-:") 'er/contract-region)
  (define-key comint-mode-map (kbd "C-c w") 'wrap_symbol_python)
  (define-key comint-mode-map (kbd "C-c x") 'remove-function-call)
  (define-key comint-mode-map (kbd "C-c V") 'send-flattened-region-to-buffer)
  (define-key comint-mode-map (kbd "C-c =") 'increment-number)
  (define-key comint-mode-map (kbd "C-c -") 'decrement-number)
)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c w") 'wrap_symbol_org)
  (define-key org-mode-map (kbd "C-c b") 'boldify)
  (define-key org-mode-map (kbd "C-c u") 'underbrace)
  (define-key org-mode-map (kbd "C-c q") 'chatgpt-query)
)

(global-set-key (kbd "C-x C-k") '(lambda ()(interactive) (kill-buffer nil)))
(global-set-key (kbd "C-l") 'org-latex-preview)
(global-set-key (kbd "C-,")
                '(lambda ()(interactive)
                   (let ((value (replace-regexp-in-string ",\\([-?A-z0-9\.:\"\']\\)" ", \\1" (buffer-substring (region-beginning) (region-end)))))
                     (delete-region (region-beginning) (region-end)) (insert value) (save-buffer))))
(global-set-key (kbd "<f3>") 'shell)
(global-set-key (kbd "M-[") 'scroll-down)
(global-set-key (kbd "M-]") 'scroll-up)
(global-set-key (kbd "M-z") 'recenter)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "C-M-<return>") 'duplicate-line)
(global-set-key (kbd "C-}") 'xah-forward-block)
(global-set-key (kbd "C-{") 'xah-backward-block)
(global-set-key (kbd "M-s-f") #'forward-whitespace)
(global-set-key (kbd "s-=") #'increase-text-size-other-window)
(global-set-key (kbd "s--") #'decrease-text-size-other-window)

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
(map! :leader
      :desc "Org Add Tag"
      "z T" #'org-roam-tag-add)

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

;; transpose
(map! :leader
      (:prefix ("d" . "Transposes"))
      :desc "Transpose Chars"
      "d c" #'transpose-chars)
(map! :leader
      :desc "Transpose Words"
      "d w" #'transpose-words)
(map! :leader
      :desc "Transpose Paragraphs"
      "d p" #'transpose-paragraphs)
(map! :leader
      :desc "Transpose Lines"
      "d l" #'transpose-lines)

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

;; cdlatex
(add-hook 'latex-mode-hook 'turn-on-cdlatex)
;; (add-hook 'org-mode-hook 'turn-on-cdlatex)

;; For mac set option to meta
(setq mac-right-option-modifier 'meta)

;; TeX Pdf
(setq TeX-PDF-mode t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(pdf-loader-install)

(print exec-path)

;; Org
;; (setq org-directory "~/OneDrive - UBC")
(setq agenda-directory (concat (file-name-as-directory org-directory) "Agenda"))
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
                        "#+LATEX_HEADER: \\usepackage{physics}\n\n"
                        "#+LATEX_HEADER: \\usepackage{bbm}\n\n"
                        "#+LATEX_HEADER: \\usepackage{amsthm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\\parindent}{0pt}\n\n"))
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
                        "#+LATEX_HEADER: \\usepackage{amsthm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\\parindent}{0pt}\n\n"
                        "* Definitions and Valuable Terminology\n\n"
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
                        "#+LATEX_HEADER: \\usepackage{amsthm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\\parindent}{0pt}\n\n"
                        "* Definitions and Valuable Terminology\n\n"
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
                        "#+LATEX_HEADER: \\usepackage{amsthm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\\parindent}{0pt}\n"
                        "* Today's Workout\n\n"
                        "* Goals\n\n"
                        "* Weekly Mileage since last "))
         :unnarrowed t)
        ("o" "Other" plain
         "%?"
         :if-new (file+head "Other/%<%Y-%m-%d>-${slug}.org"
               ,(concat "#+TITLE: ${title}\n"
                        "#+AUTHOR: Justice Sefas\n"
                        "#+OPTIONS: toc:nil num:nil tex:t html-postamble:nil\n\n"
                        "#+LATEX_HEADER: \\usepackage{amsfonts}\n"
                        "#+LATEX_HEADER: \\usepackage{physics}\n\n"
                        "#+LATEX_HEADER: \\setlength{\\parindent}{0pt}\n"
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
                        "#+LATEX_HEADER: \\usepackage{amsthm}\n\n"
                        "#+LATEX_CLASS: beamer\n"
                        "#+BEAMER_THEME: Frankfurt\n"
                        "#+BEAMER_INNER_THEME: rounded\n"
                        "#+LATEX_HEADER: \\setlength{\\parindent}{0pt}\n\n"
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
                        "#+LATEX_HEADER: \\usepackage{amsthm}\n\n"
                        "#+LATEX_HEADER: \\setlength{\\parindent}{0pt}\n\n"
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
(setq org-noter-always-create-frame 0)

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
   ("jl" . "julia")
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

(defun xah-run-other-file ()
  "Execute the other file"
  (interactive)
  (other-window 1)
  (xah-run-current-file)
  (other-window 1))

(map! :leader
      :desc "Xah Run Current File"
      "f x" #'xah-run-current-file)
(map! :leader
      :desc "Xah Run Other File"
      "f X" #'xah-run-other-file)

;; https://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
;; Gordon Gustafson
(defmacro define-and-bind-text-object (key start-regex end-regex name)
  (let ((inner-name (make-symbol name))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; from regex "=" up to regex "$", bound to r (invoke with "vil" or "val"):
(define-and-bind-text-object "l" "=" "$" "rhs of =")
(define-and-bind-text-object "h" "^" "=" "lhs of =")

;; between dollar signs
(define-and-bind-text-object "$" "\\$" "\\$" "between $ signs")

;; from space to colon
(define-and-bind-text-object ":" " " ":" "space to colon")

;; blink cursor
(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")
(set-cursor-color "#D4AF37")
(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Zarza wrote this cyberpunk variant of timer `blink-cursor-timer'.
Warning: overwrites original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count))
    )
  (internal-show-cursor nil (not (internal-show-cursor-p)))
  )
(blink-cursor-mode)

;; save upon exiting insert mode
(add-hook 'evil-insert-state-exit-hook (lambda () (if (buffer-file-name) (save-buffer))))

;; Focus/Beacon mode
(beacon-mode 1)
(setq beacon-color "#FF0000")

;; tells you which function you're in
(which-function-mode)

;; chatgpt.el
(use-package! chatgpt
  :defer t
  :config
  (unless (boundp 'python-interpreter)
    (defvaralias 'python-interpreter 'python-shell-interpreter))
  (setq chatgpt-repo-path (expand-file-name "~/.emacs.d/.local/straight/repos/ChatGPT.el/" doom-local-dir))
  (set-popup-rule! (regexp-quote "*ChatGPT*")
    :side 'bottom :size .5 :ttl nil :quit t :modeline nil)
  :bind ("C-c q" . chatgpt-query))

;;
(defun my-paste-squeue-output (server-name)
  "SSH into SERVER-NAME and paste 'squeue -u j' output at point in the current buffer."
  (interactive "sEnter server name: ")
  (let ((output-buffer (generate-new-buffer "*squeue-output*")))
    (shell-command (concat "ssh " server-name " squeue -u jsefas1") output-buffer)
    (switch-to-buffer output-buffer)
    (goto-char (point-min))
    (kill-ring-save (point-min) (point-max))
    (kill-buffer output-buffer)
    (yank)))

(defun my-ssh-and-open-file (word)
  (interactive)
  (let* ((server-options '("borg" "cedar" "narval" "beluga" "idk"))
         (server (completing-read "Select a server: " server-options)))
    (if (string-empty-p server)
        (message "No server selected.")
      (if (equal server "idk")
          (progn
            (dolist (option server-options)
              (unless (equal option "idk")
                (let ((ssh-command (format "ssh %s" option))
                      (tramp-path (format (if (equal option "borg") "/ssh:%s:/ubc/cs/research/plai-scratch/jsefas" "/ssh:%s:~/scratch") server))
                      (matching-files (directory-files (format (if (equal option "borg") "/ssh:%s:/ubc/cs/research/plai-scratch/jsefas" "/ssh:%s:~/scratch") server) t (concat ".*" word ".*"))))
                  (when matching-files
                    (shell-command ssh-command)
                    (find-file (car matching-files))
                    (setq-local remote-file-name (format tramp-path option)))
                  (unless (string-empty-p (buffer-name))
                    (return))))))
        (let ((ssh-command (format "ssh %s" server))
              (tramp-path (format (if (equal server "borg") "/ssh:%s:/ubc/cs/research/plai-scratch/jsefas" "/ssh:%s:~/scratch") server))
              (matching-files (directory-files (format (if (equal server "borg") "/ssh:%s:/ubc/cs/research/plai-scratch/jsefas" "/ssh:%s:~/scratch") server) t (concat ".*" word ".*"))))
          (shell-command ssh-command)
          (when matching-files
            (find-file (car matching-files))
            (setq-local remote-file-name (format tramp-path server))))))))

;; (defil TYPE START-DELIM END-DELIM TEXT-REGEXP LINK-EXPR &optional
;; START-REGEXP-FLAG END-REGEXP-FLAG DOC)
;; manual: https://www.gnu.org/software/hyperbole/man/hyperbole.pdf
(require 'hyperbole)
(defil my-ssh-open-file-button " " "" "[:digit:]*" '(lambda (x) (my-ssh-and-open-file x)))
(defil wandb-code-button " " "" "[:alnum:]*[a-z][:alnum:]*" '(lambda (x)
                                               (browse-url
                                                (format "https://wandb.ai/iai/itra/runs/%s" x))))

(defun find-longest-substring (delimiter)
  "Search for the longest substring between two instances of DELIMITER on the current line and perform an Evil search with the longest substring as the search pattern."
  (interactive "cDelimiter character: ")
  (save-excursion
    (beginning-of-line)
    (let* ((start (point))
           (end (progn (end-of-line) (point)))
           (line (buffer-substring-no-properties start end))
           (substrings (split-string line (string delimiter))))
      (if (> (length substrings) 1)
          (progn
            (setq substrings (cdr (reverse  (cdr substrings))))
            (setq longest-substring (car substrings))
            (setq longest-length (length longest-substring))
            (dolist (substring (cdr (reverse (cdr substrings))))
              (let ((substring-length (length substring)))
                (when (> substring-length longest-length)
                  (setq longest-substring substring)
                  (setq longest-length substring-length)))))
        (message "No substring found between '%c' characters on the current line."))
      (when (and (> longest-length 0) (stringp longest-substring))
        (evil-search (format "\\<%s\\>" (regexp-quote longest-substring)) 2 t))
      (evil-search-next)
      longest-substring)))

(search-forward "asdf")

asdfasdfasdf"longest"asdf

(defun my-evil-search-last-word-on-line ()
  "Invoke Evil search for the last word on the current line across the entire file."
  (interactive)
  (let ((last-word (save-excursion
                     (end-of-line)
                     (backward-word)
                     (thing-at-point 'word))))
    (evil-search (format "\\<%s\\>" (regexp-quote last-word)) 2 t))
  (evil-search-next)
  )



asd wu to notify this person of what append

notysothis person wi

asdf append fdsa

sdflkjasd append append

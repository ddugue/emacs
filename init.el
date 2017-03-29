;; Initialize packages to install use-package
(require 'package)
(setq package-enable-at-startup nil)


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)
;; (package-refresh-contents)
;; Install use-package only if it's not there
(unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package))

(setq use-package-verbose t) ;; Show loaded packages in *messages*

;; Install evil only if it's not there
(unless (package-installed-p 'evil)
   (package-refresh-contents)
   (package-install 'evil))

;; Enable evil globally
(require 'evil)
(evil-mode 1)

;; Unsetting some default evil commands, that I
;; plan to use
(require 'evil)
(evil-mode 1)

(define-key evil-motion-state-map (kbd "SPC") nil)
(define-key evil-motion-state-map "," nil)
(define-key evil-motion-state-map "zz" nil)
(define-key evil-motion-state-map "za" nil)
(define-key evil-normal-state-map (kbd "g,") nil)

(use-package evil-commentary
    :ensure t
    :commands (evil-commentary))

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode t))

(customize-set-variable 'evil-want-Y-yank-to-eol t)

;; Install general only if it's not there
(unless (package-installed-p 'general)
   (package-refresh-contents)
   (package-install 'general))
(require 'general)

(setq general-default-keymaps 'evil-motion-state-map)
(setq default-leader-key "SPC")
(setq application-leader-key ",")
(setq general-default-non-normal-prefix "C-SPC")

;; Install keychord only if it's not there
(unless (package-installed-p 'key-chord)
   (package-refresh-contents)
   (package-install 'key-chord))
(require 'key-chord)

(setq key-chord-two-keys-delay 0.1) ;; default 0.1
(setq key-chord-one-key-delay 0.2) ;; default 0.2

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)

;; Install hydra only if it's not there
(unless (package-installed-p 'hydra)
   (package-refresh-contents)
   (package-install 'hydra))
(require 'hydra)
;; Since we use the minibuffer in a separate frame. No help
;; for us
(setq hydra-is-helpful nil)

(defun my/ivy-get-selection ()
    "Returns the selected ivy text"
    (expand-file-name ivy--current ivy--directory))

(defun my/ivy-append-yank ()
    "Append the current line to the current kill-ring (via a register"
    (interactive)
    (set-register 300 (concat (get-register 300) (my/ivy-get-selection) "\n")))

(defun my/ivy-override-yank ()
    "Override the latest kill-ring"
    (interactive)
    (kill-new (my/ivy-get-selection)))

(defun my/ivy-mark ()
    "Append the current line to the current kill-ring (via a register"
    (interactive)
    (set-register 400 (concat (get-register 400) ivy--current "\n")))

(defun my/ivy-mark-display-transformer (str)
    "Transform string -> string"
    (let ((reg (get-register 400)))
    (if (member str (when reg (split-string reg "\n")))
     (concat "* " str)
     str)))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  (:map ivy-minibuffer-map
    ("C-S-J" . ivy-scroll-down-command)
    ("C-j" . ivy-next-line)
    ("C-S-K" . ivy-scroll-up-command)
    ("C-k" . ivy-previous-line)
    ("C-l" . ivy-alt-done)
    ("<C-return>" . ivy-immediate-done)
    ("C-h" . ivy-backward-kill-word))
  :init
  (progn
    ;; Set default regex matching
    (setq ivy-re-builders-alist
          '((ivy-switch-buffer . ivy--regex-fuzzy)
            (counsel-find-file . ivy--regex-fuzzy)
            (t . ivy--regex-plus)))
    (setq ivy-wrap t)
    ;; Remove ../ and ./ from files selection t)
    (setq ivy-extra-directories nil)
    (setq ivy-height 25)
    ;; Enable globally
    (ivy-mode 1))

  :config
  (progn
    ;; Define a custom hydra
    (key-chord-define ivy-minibuffer-map "jk"
                      (defhydra hydra-ivy/body
                                (:post (when
                                   (get-register 300)
                                   (kill-new (get-register 300))
                                   (set-register 300 nil)))
                                "ivy"
                                ("j" ivy-next-line "down")
                                ("k" ivy-previous-line "up")
                                ("l" ivy-alt-done "forward")
                                ("h" ivy-backward-kill-word "back")
                                ("y" my/ivy-append-yank "yank")
                                ("m" my/ivy-mark "mark")
                                ("Y" my/ivy-override-yank "override")
                                ("i" nil "insert")))))

(use-package counsel
    :ensure t
    :commands (counsel-find-file locate-file counsel-describe-function ivy-switch-buffer swiper)
    :bind
    (("C-x C-f" . counsel-find-file)
     ("C-x f"   . counsel-find-file))
    :config
    (progn
    (ivy-set-display-transformer 'counsel-find-file 'my/ivy-mark-display-transformer)
    (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"
         ;; File names ending in .pyc
         "\\|\\(?:\\`.+?\\.pyc\\'\\)"
         ))))

(defun get-candidates-function (str pred _)
)
(defun insert-selection (s)
  (insert (s-chop-prefix company-prefix s))
)
(defun my/max-candidate-length (candidates)
  (--reduce-from (max acc (length it)) 15 candidates))

(defun my/format-candidate (s)

 (let ((msg (concat
              (format "[%s] " (or (get-text-property 0 :symbol s)"_"))
              (propertize company-prefix 'face 'ivy-minibuffer-match-face-2)
              (s-pad-right (- (1+ (my/max-candidate-length company-candidates)) (length company-prefix))
                " "
                (s-chop-prefix company-prefix s))
              (propertize (s-truncate 50 (get-text-property 0 :description s)) 'face 'minibuffer-prompt)
              "\n")))
   (when (equal (nth company-selection company-candidates) s)
       (add-face-text-property 0 (length msg) 'highlight t msg)
   )
   msg))

(defun start-selection ()
  (message (mapconcat 'my/format-candidate company-candidates ""))
)

(defun hide-ivy ()
  (message "")
)
(defun counsel-company-frontend (command)
  (case command
    (post-command (start-selection))
    (hide (hide-ivy))))

(use-package company
  :ensure t
  :commands (company-mode)
  :bind
  (:map company-active-map
    ("C-k" . company-select-previous)
    ("C-j" . company-select-next))
  :config
  (setq company-idle-delay .3)
  (setq company-minimum-prefix-length 2)
  (setq company-frontends
        '(company-preview-frontend counsel-company-frontend))
  (setq company-require-match 'never))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
;; Linum relative mode configuration
(use-package linum-relative
   :ensure t
   :diminish linum-relative-mode
   :config
   (setq linum-relative-current-symbol "")
   (add-hook 'text-mode-hook 'linum-relative-mode) ;; global mode seems to enable it in the minibuffer
   (add-hook 'prog-mode-hook 'linum-relative-mode) ;; global mode seems to enable it in the minibuffer
   (add-hook 'text-mode-hook 'linum-mode)
   (add-hook 'prog-mode-mode-hook 'linum-mode))

;; Enable highlighting current line for all modes
(global-hl-line-mode 1)

(use-package whitespace
  :config
  (setq whitespace-style '(space-mark tab-mark newline-mark))
  (setq whitespace-display-mappings
        '((space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [8594 9] [92 9]) ; tab
          )))

(setq-default fill-column 80)
(setq column-number-mode t)
(use-package fill-column-indicator
    :ensure t
    :commands (fci-mode)
    :config
    (setq fci-rule-width 3)
    (setq fci-rule-color "#A16946"))

;; Mode to highlight numbers
(use-package highlight-numbers
    :commands (highlight-numbers-mode)
    :ensure t)

(setq custom-theme-directory "~/.emacs.d/themes")
(setq custom-safe-themes t)
(load-theme 'yesterday-glow t)
;; Force initial frame to not have any minibuffer
(setq initial-frame-alist '((name . "editor") (minibuffer . nil)))
(add-to-list 'default-frame-alist '(minibuffer . nil))

(defun my/matchframe (frame)
  (when (equal "help" (frame-parameter frame 'name)) frame))
;; For help buffers
;; TODO: Shorten fn
(add-to-list 'display-buffer-alist
   '("^\\*[hH]elp.*$" .
       ((display-buffer-reuse-window display-buffer-use-some-frame display-buffer-pop-up-frame)
        . ((reusable-frames . t)
          (frame-predicate . my/matchframe)
          (pop-up-frame-parameters . ((name . "help")
                                      (minibuffer . nil)
                                      (unsplittable . t)))))))

;; For messages buffers
(add-to-list 'display-buffer-alist
   '("^\\*[Mm]essages.*$" .
       ((display-buffer-reuse-window display-buffer-use-some-frame display-buffer-pop-up-frame)
        . ((reusable-frames . t)
          (frame-predicate . my/matchframe)
          (pop-up-frame-parameters . ((name . "help")
                                      (minibuffer . nil)
                                      (unsplittable . t)))))))


(add-to-list 'display-buffer-alist
   '("^\\*[Mm]agit.*$" .
       ((display-buffer-reuse-window display-buffer-use-some-frame display-buffer-pop-up-frame)
        . ((reusable-frames . t)
          (frame-predicate . my/matchframe)
          (pop-up-frame-parameters . ((name . "help")
                                      (minibuffer . nil)
                                      (unsplittable . t)))))))

(add-to-list 'display-buffer-alist
   '("^\\*[Ff]lycheck.*$" .
       ((display-buffer-reuse-window display-buffer-use-some-frame display-buffer-pop-up-frame)
        . ((reusable-frames . t)
          (frame-predicate . my/matchframe)
          (pop-up-frame-parameters . ((name . "help")
                                      (minibuffer . nil)
                                      (unsplittable . t)))))))


;; Change default location of temporary files
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
(fset 'yes-or-no-p 'y-or-n-p) ;; Yes or no questions become Y or n questions
;; Remove trailing whitespaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(defun my/toggle-tab-mode ()
  "Toggle visual tab and whitespace mode"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)))


(setq scroll-step 1)

;; Configuration and installation of which-key
(use-package which-key
    :ensure t
    :diminish which-key-mode
    :config
    (which-key-setup-minibuffer)
    (which-key-mode 1))

;; Install projectile
(use-package projectile
  :ensure t
  :commands (projectile-mode projectile-project-p))

(defun my/git-ag (&optional initial-input)
  "Search with ag on the git root if possible"
  (interactive)
  (counsel-ag initial-input
    (when (projectile-project-p) (projectile-project-root))))

(defun evil-magit/toggle (&optional intent)
  "Toggle the stage instead of moving it manually"
  (interactive "P")
  (pcase (magit-diff-type)
    ('unstaged (magit-stage intent))
    ('commited (magit-unstage))
    ('untracked (magit-stage intent))
    ('staged (magit-unstage))
    ('undefined (user-error "Cannot toggle"))))

(use-package magit
  :ensure t
  :commands (magit-status)
  :general
  (:states '(normal visual)
   :keymaps 'magit-status-mode-map
   "j" 'magit-section-forward
   "k" 'magit-section-backward
   "J" 'magit-section-forward-sibling
   "K" 'magit-section-backward-sibling
   "v" 'evil-magit/toggle
   "zz" 'magit-section-toggle
   "d" 'magit-discard
   )
  (:states '(normal visual)
   :keymaps 'magit-status-mode-map
   :prefix application-leader-key
   "m"  'magit-merge
   "c"  'magit-commit
   "a"  'magit-commit-amend
   "C"  'magit-commit-popup
   "P"  'magit-push-popup
   "pp" 'magit-push-current-to-upstream
   "F"  'magit-pull-popup
   "ff" 'magit-pull-from-upstream
   "bb" 'magit-checkout
   "bc" 'magit-branch-and-checkout
   "B"  'magit-branch-popup
   "r"  'magit-refresh
   "i"  'magit-gitignore
   )
  (:keymaps 'with-editor-mode-map
   "<C-return>" 'with-editor-finish)
   :config
   (evil-set-initial-state 'git-commit-mode 'normal)
   (evil-set-initial-state 'magit-mode 'normal)
   (evil-set-initial-state 'magit-status-mode 'normal)
   (setq magit-commit-show-diff nil))

(use-package smerge-mode
   :general
  (:keymaps 'smerge-mode-map
   :states '(normal visual)
   :prefix application-leader-key
   "RET" 'smerge-keep-current
   "SPC" 'smerge-keep-other
   "d"   'smerge-keep-base
   "a"   'smerge-keep-all
   "r"   'smerge-resolve
   "n"   'smerge-next
   "N"   'smerge-prev))
(defun my/override-flycheck-fn ()
(defconst flycheck-error-list-format
  `[("Line" 4 flycheck-error-list-entry-< :right-align t)
    ("ID" 15 t)
    (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]
  "Table format for the error list.")

(defun flycheck-error-list-make-entry (error)
  "Make a table cell for the given ERROR.

Return a list with the contents of the table cell."
  (let* ((level (flycheck-error-level error))
         (level-face (flycheck-error-level-error-list-face level))
         (line (flycheck-error-line error))
         (column (flycheck-error-column error))
         (message (or (flycheck-error-message error)
                      (format "Unknown %s" (symbol-name level))))
         (flushed-msg (flycheck-flush-multiline-message message))
         (id (flycheck-error-id error))
         (id-str (if id (format "%s" id) ""))
         (checker (flycheck-error-checker error))
         (msg-and-checker (flycheck-error-list-make-last-column flushed-msg checker))
         (explainer (flycheck-checker-get checker 'error-explainer)))
    (list error
          (vector (flycheck-error-list-make-number-cell
                   line level-face)
                  ;; Error ID use a different face when an error-explainer is present
                  (flycheck-error-list-make-cell
                   id-str (if explainer 'flycheck-error-list-id-with-explainer
                            'flycheck-error-list-id)
                   id-str 'flycheck-error-list-explain-error)
                  (flycheck-error-list-make-cell
                   msg-and-checker nil msg-and-checker))))))
(use-package flycheck
  :commands (flycheck-mode)
  :ensure t
  :init
    (setq-default flycheck-disabled-checkers '(python-flake8))
  :bind
    (:map flycheck-error-list-mode-map
             ("j" . flycheck-error-list-next-error)
             ("k" . flycheck-error-list-previous-error))
  :config
    (my/override-flycheck-fn)
    (setq flycheck-check-syntax-automatically '(save new-line idle-change))
    (setq flycheck-display-errors-delay 30)
  )

;; TODO: Make this a toggle
(defun my/enable-flycheck ()
  (interactive)
  (flycheck-mode t)
  (flycheck-list-errors))
    (defun my/touch-file (filename)
      "Create a file into the current directory"
      (interactive "sName of the file:")
      (shell-command (concat "touch " (shell-quote-argument filename)))
      (revert-buffer)
    )

    (defun my/dired-toggle-mark ()
      "Toggle a mark"
      (interactive)
      (save-restriction
        (narrow-to-region (point-at-bol) (point-at-eol))
        (dired-toggle-marks))
    )

    (defun my/wdired-commit ()
      "Commit edits and come back in wdired mode"
      (interactive)
      (wdired-finish-edit)
      (revert-buffer)
      (wdired-change-to-wdired-mode)
      (evil-normal-state)
    )

    (defun my/enter-wdired-and-change ()
      (interactive)
      (wdired-change-to-wdired-mode)
      (evil-normal-state))


    (defun my/enter-wdired-and-delete ()
      (interactive)
      (wdired-change-to-wdired-mode)
      (evil-normal-state)
      (evil-delete))
    (defun my/setup-dired (fun &rest args)
       (message "Dired started")
       (let ((res (apply fun args)))
          (message "Dired stopped")
           res))

    (use-package dired-ranger
      :ensure t
      :commands (dired-ranger-move dired-ranger-paste dired-ranger-copy))

    (use-package wdired
      :ensure t)
    (use-package dired
      :commands (dired)
      :bind
      (:map dired-mode-map
       ("SPC" . nil))
      :general
      (:states '(normal visual)
       :keymaps 'wdired-mode-map
       "<C-return>" 'my/wdired-commit
       "<return>" 'dired-find-file
       "m" 'my/dired-toggle-mark
       "dd" 'dired-do-delete
       "zz" 'dired-maybe-insert-subdir
      )
      (:states '(normal visual)
       :keymaps '(dired-mode-map wdired-mode-map)
       :prefix application-leader-key
       "!"  'dired-do-shell-command
       "i"  'dired-create-directory
       "a"  'my/touch-file
       "y" 'dired-ranger-copy
       "p" 'dired-ranger-paste
       "m" 'dired-ranger-move
       "%" 'dired-mark-files-regexp)
      :config
      (add-hook 'dired-after-readin-hook
                (lambda ()
                        (unless (member 'wdired-mode (mapcar #'car minor-mode-alist))
                                (my/enter-wdired-and-change)
                         ))))
(use-package ledger-mode
  :mode ("\\.dat\\'" . ledger-mode)
  :ensure t)
(setq org-hide-leading-stars t) ;; Ensure that we hide the number of stars before the first one
(setq org-startup-indented t) ;; Ensure we indent all the content
(use-package org
    :config
    (use-package org-bullets
        :ensure t
        :config
        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
)
;; Ensure syntax of the language is used inside source blocks
(setq org-src-fontify-natively t)

;; Ensure tabs work properly inside source blocks
(setq org-src-tab-acts-natively t)
(use-package org
   :general
   (:state '(insert normal visual)
    :keymaps 'org-mode-map
    "M-h" 'org-metaleft
    "M-l" 'org-metaright))
(defun my/shell-open ()
  "Open a shell in root of project"
  (interactive)
   (let ((project-root (if (projectile-project-p) (projectile-project-root) "~")))
         (progn
           (message project-root)
           (pop-to-buffer "*ansi-term*")
           (ansi-term "zsh" "ansi-term")
           (end-of-buffer)
           (insert (concat "cd " project-root))
           (term-send-input)
           (end-of-buffer)
           (insert "clear")
           (term-send-input))))
;; Make ansi-term lazy-load
(use-package ansi-term
    :commands (ansi-term))

;; For help buffers
(add-to-list 'display-buffer-alist
   '("^\\*[Aa]nsi.*$" .
       ((display-buffer-pop-up-frame)
        . ((pop-up-frame-parameters . ((name . "ansi-terminal")
                                      (minibuffer . nil)
                                      (unsplittable . t))
          ))
       )
    )
)
(use-package eww-lnum
  :ensure t)

(use-package eww
  :general
  (:keymaps 'eww-mode-map
   :prefix application-leader-key
           "r" 'eww-reload
           "<return>" 'eww-browse-with-external-browser
           "z" 'eww-readable
           "v" 'eww-view-source
           "y" 'eww-copy-page-url
           "b" 'eww-add-bookmark)
  (:keymaps 'eww-mode-map
   :states '(normal)
   "<C-return>" 'eww-submit
   "f" 'eww-lnum-follow
   ";" 'eww-lnum-universal
   "g" 'eww
   "b" 'eww-back-url
   "J" 'evil-scroll-down
   "K" 'evil-scroll-up
  )
  :config
  (setq eww-search-prefix "https://www.google.com/search?q="))

;; from magnars
(defun spacemacs/sudo-edit (&optional arg)
  "Open file in sudo mode"
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

(defun my/goto-default-mark ()
  "Go to the bookmark 'm'"
  (interactive)
  (evil-goto-mark ?m))

(defun my/kill-other-buffers ()
  "Kill all other buffers"
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

;; Function to reload editor
(defun my/reload-emacs ()
    "Reload emacs config"
    (interactive)
    (load-file "~/.emacs.d/init.el"))

;; Function to open this config file
(defun my/open-config ()
    "Open emacs config"
    (interactive)
    (find-file "~/.emacs.d/init.org"))

(defun my/set-venv ()
  (interactive)
  (require 'projectile)
  (when (projectile-project-p)
    (progn
      (venv-set-location (projectile-project-root))
      (setq python-environment-directory venv-location)
      (venv-workon "venv")
      (setenv "PYTHONPATH" (concat
                             (getenv "PYTHONPATH")
                              ":"
                             (concat (projectile-project-root) "src/")))
)))

(use-package company-jedi
  :ensure t)

(use-package virtualenvwrapper
  :ensure t
  :commands (venv-set-location venv-workon)
  :config
  (add-hook 'venv-postactivate-hook
            (lambda () (progn
                         (shell-command "pip install nose pylint pylint-django")
                         (jedi:install-server)
                         (flycheck-disable-checker 'python-pylint t)))))
;; When we jedi pop marker, we should close the buffer for SPC TAB
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :general
  (:keymaps 'python-mode-map
   :states '(normal)
   "g." 'jedi:goto-definition
   "g," 'jedi:goto-definition-pop-marker)
  :config
   (general-define-key
    :states '(normal)
    :keymaps 'python-mode-map
    :prefix application-leader-key
    "vv" 'my/set-venv)
   (add-hook 'python-mode-hook
     (lambda ()
       (progn
         (set (make-local-variable 'company-backends) '(company-jedi))
         (company-mode t)
         (flycheck-mode t)
         (highlight-numbers-mode t)
         ))))

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (add-hook 'web-mode-hook 'turn-off-fci-mode)
  :general
  (:keymaps 'web-mode-map
   :states '(normal)
   "zz" 'web-mode-fold-or-unfold)
  (:keymaps 'web-mode-map
   :states '(normal)
   :prefix application-leader-key
   "=" 'web-mode-buffer-indent))
(use-package conf-mode
  :mode "\\.pylintrc\\'")
(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode)
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
    (lambda ()
      (progn (highlight-numbers-mode)
      (face-remap-add-relative 'font-lock-variable-name-face '(:foreground "#E7C547"))
      (face-remap-add-relative 'default '(:foreground "#FF8100"))))))
(global-unset-key (kbd "C-SPC"))

;; leader key prefix shortcuts
(general-define-key
  :prefix default-leader-key
  ;; Help
  "h"  '(:ignore t :which-key "Help")
  "hf" 'counsel-describe-function
  "hv" 'counsel-describe-variable
  "hh" 'counsel-describe-function
  "hk" 'describe-key

  ;; Project
  "g"  '(:ignore t :which-key "Projects")
  "gg" 'magit-status
  "/"  'my/git-ag

  ;; Buffers
  "b"  '(:ignore t :which-key "Buffers")
  "bb" 'ivy-switch-buffer
  "bd" 'evil-delete-buffer
  "bc" 'my/kill-other-buffers
  "TAB" 'spacemacs/alternate-buffer

  ;; Files
  "f"  '(:ignore t :which-key "Files")
  "ff" 'counsel-find-file
  "fl" 'locate-file
  "f!" 'spacemacs/sudo-edit
  "fs" 'save-buffer

  ;; Editor
  "E"  '(:ignore t :which-key "Editor")
  "Er" 'my/reload-emacs
  "Ef" 'my/open-config

  ;; Windows
  "w"  '(:ignore t :which-key "Windows")
  "wd" 'delete-other-windows
  "wc" 'delete-other-windows
  "wa" 'make-frame-command

  ;; Error management
  "e"  '(:ignore t :which-key "Errors")
  "ee" 'my/enable-flycheck

  ;; Toggles
  "t"  '(:ignore t :which-key "Toggles")
  "tn" 'highlight-numbers-mode
  "te" 'flycheck-mode
  "tw" 'whitespace-mode
  "ta" 'my/toggle-tab-mode

  ;; Applications
  "RET" 'my/shell-open
  "a"  '(:ignore t :which-key "Applications")
  "aw" 'eww
)


(general-define-key
  ";" 'evil-commentary
  "/" 'swiper)

(general-define-key "`"
  (general-key-dispatch 'evil-goto-mark
    "`" 'my/goto-default-mark
  ))

(general-define-key
  :keymaps '(evil-normal-state-map evil-motion-state-map)
  ;; Folding
  "za" 'evil-close-folds
  "zz" 'evil-toggle-fold)

(add-hook 'prog-mode-hook 'fci-mode)
;; Utilities functions

(defun my/tangle-init ()
  "Tangle an init file while ignoring DISABLED headers and :tangle nil"
  (let ((body-list ()) (output-file "~/.emacs.d/init.el"))
    (org-babel-map-src-blocks "~/.emacs.d/init.org"
      (add-to-list 'body-list (unless (string= (org-get-todo-state) "DISABLED") body)))
    (with-temp-file output-file
    (insert (apply 'concat (reverse body-list)))
    (message (format "Wrote %d code blocks to init.el" (length body-list))))))


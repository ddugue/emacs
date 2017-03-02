(deftheme yesterday-glow
  "Theme that emulates the good old looking of old orange terminals")

(let* ((class '((class color) (min-colors 89)))
       (white "#FFFFFF")
       (orange "#FF8100")
       (light-orange "#E78C45")
       (dark-orange "#994D00")
       (lime "#B9CA4A")
       (teal "#66CCCC")
       (red "#D54E53")
       (yellow "#E7C547")
       (purple "#CC99CC")
       (dark "#0F0900")
       (even-less-dark "#1A0F00")
       (less-dark "#0D0800")
       (lesser-dark "#0A0300")
       (light "#38280B")
       (brown "#A16946")
       (blue "#7AA6DA")
       ;;
       (foreground orange)
       (background dark)
       (comment brown)
       (highlight light)
       ;; Support for spacemacs
       (bg1 dark)
       (bg2 even-less-dark)
       (bg3 less-dark)
       (bg4 lesser-dark)
       (highlight light)
       (base foreground)
       (act1 dark)
       (act2 foreground)
       (cursor orange)
       (lnum orange)
       (func blue)
       (err red)
       (str teal)
       (suc lime)
       (const red)
       (border orange)
       (type blue)
       (keyword yellow)
       (var orange)
       (mat yellow)
       (head1 blue)
       (head2 orange)
       (head3 red)
       (head4 yellow)
       (comp purple)
       (war yellow)
       (warn light-orange)
       )

  (custom-theme-set-faces
   'yesterday-glow

   ;; Defaults
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(italic ((,class (:slant italic))))
   `(shadow ((,class (:foreground ,brown))))
   `(success ((,class (:foreground ,lime))))
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,yellow))))
   `(outline-4 ((,class (:slant normal :foreground ,comment))))
   `(header-line ((,class :background ,lesser-dark)))
   `(match ((,class (:background ,light :foreground ,yellow))))
   `(page-break-lines ((,class (:foreground ,orange))))

   ;; Emacs interface
   `(cursor ((,class (:background ,orange))))
   `(fringe ((,class (:background ,background :foreground ,foreground))))
   `(custom-button ((,class :background ,less-dark :foreground ,foreground :box (:line-width 2 :style released-button))))
   `(linum ((,class (:foreground ,orange :background ,even-less-dark))))
   `(linum-relative-current-face ((,class (:foreground ,blue))))
   `(border ((,class (:background ,orange))))
   `(vertical-border ((,class (:foreground ,lesser-dark))))

   `(hl-line ((,class (:background ,even-less-dark))))
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,purple :underline t))))
   `(highlight ((,class (:foreground ,foreground :background ,light))))
   `(minibuffer-prompt ((,class (:inherit bold :foreground ,blue))))
   `(region ((,class (:background ,light))))
   `(secondary-selection ((,class (:background ,less-dark))))

   ;; Mode line
   `(mode-line ((,class (:foreground ,background :background ,orange :box (:color ,orange :line-width 1)))))
   `(mode-line-inactive ((,class (:foreground ,foreground :background ,background  :box (:color ,dark-orange :line-width 1)))))
   `(mode-line-buffer-id ((,class (:inherit bold :foreground ,white))))

   ;; Web mode
   `(web-mode-block-control-face ((t (:foreground ,blue :weight bold))))
   `(web-mode-block-delimiter-face ((t (:foreground ,blue))))
   `(web-mode-block-string-face ((t (:foreground ,teal))))
   `(web-mode-html-attr-name-face ((t (:foreground ,orange :weight normal))))
   `(web-mode-html-attr-value-face ((t (:foreground ,lime :weight normal :slant italic))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,foreground))))
   `(web-mode-html-tag-face ((t (:foreground ,orange :weight bold))))
   `(web-mode-variable-name-face ((t (:foreground ,red))))

   ;; Font-lock
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,red))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:inherit bold :foreground ,light-orange))))
   `(font-lock-builtin-face ((,class (:foreground ,keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,red))))
   `(font-lock-preprocessor-face ((,class (:foreground ,blue))))
   `(font-lock-reference-face ((,class (:foreground ,red))))
   `(font-lock-string-face ((,class (:foreground ,teal))))
   `(font-lock-type-face ((,class (:foreground ,blue :inherit bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange))))
   `(font-lock-warning-face ((,class (:foreground ,yellow :background ,background))))

   ;; Ivy
   `(ivy-current-match ((,class (:background ,highlight :inherit bold))))
   `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,yellow :underline t))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,yellow :underline t))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,yellow :underline t))))
   `(ivy-remote ((,class (:foreground ,teal))))
   `(ivy-modified-buffer ((,class (:foreground ,teal))))
   `(ivy-virtual ((,class (:inherit italic))))

   ;; Flycheck
   `(flycheck-warning ((,class (:underline (:style wave :color ,light-orange)))))
   `(flycheck-error ((,class (:underline (:style wave :color ,err)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,blue)))))
   `(flycheck-fringe-error ((,class (:foreground ,err :inherit bold))))
   `(flycheck-fringe-warning ((,class (:foreground ,light-orange :inherit bold))))
   `(flycheck-fringe-info ((,class (:foreground ,blue :inherit bold))))
   `(flycheck-error-list-error ((,class (:foreground ,err :inherit bold))))
   `(flycheck-error-list-warning ((,class (:foreground ,light-orange :inherit bold))))
   `(flycheck-error-list-info ((,class (:foreground ,blue :inherit bold))))
   ;; Org
   `(org-agenda-clocking ((,class (:background ,highlight :foreground ,comp))))
   `(org-agenda-date ((,class (:foreground ,var))))
   `(org-agenda-date-today ((,class (:foreground ,keyword :inherit bold ))))
   `(org-agenda-date-weekend ((,class (:inherit bold :foreground ,var))))
   `(org-agenda-done ((,class (:foreground ,suc ))))
   `(org-agenda-structure ((,class (:inherit bold :foreground ,comp))))
   `(org-block ((,class (:foreground ,dark-orange))))
   `(org-block-begin-line ((,class (:foreground ,dark-orange))))
   `(org-block-end-line ((,class (:foreground ,dark-orange))))
   `(org-clock-overlay ((,class (:foreground ,comp))))
   `(org-code ((,class (:foreground ,teal))))
   `(org-column ((,class (:background ,highlight))))
   `(org-column-title ((,class (:background ,highlight))))
   `(org-date ((,class (:underline t :foreground ,var))))
   `(org-date-selected ((,class (:background ,func :foreground ,bg1))))
   `(org-document-info-keyword ((,class (:foreground ,comment))))
   `(org-document-title ((,class (:foreground ,func :inherit bold  :underline t))))
   `(org-done ((,class (:foreground ,suc :inherit bold :background ,even-less-dark))))
   `(org-ellipsis ((,class (:foreground ,keyword))))
   `(org-footnote  ((,class (:underline t :foreground ,base))))
   `(org-hide ((,class (:foreground ,base))))
   `(org-kbd ((,class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
   `(org-level-1 ((,class (:inherit bold :foreground ,head1))))
   `(org-level-2 ((,class (:inherit bold :foreground ,head2))))
   `(org-level-3 ((,class (:bold nil :foreground ,head3))))
   `(org-level-4 ((,class (:bold nil :foreground ,head4))))
   `(org-level-5 ((,class (:bold nil :foreground ,head1))))
   `(org-level-6 ((,class (:bold nil :foreground ,head2))))
   `(org-level-7 ((,class (:bold nil :foreground ,head3))))
   `(org-level-8 ((,class (:bold nil :foreground ,head4))))
   `(org-link ((,class (:underline t :foreground ,blue))))
   `(org-meta-line ((,class (:foreground ,comment))))
   `(org-mode-line-clock-overrun ((,class (:foreground ,err))))
   `(org-priority ((,class (:foreground ,war :inherit bold))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-scheduled ((,class (:foreground ,comp))))
   `(org-scheduled-today ((,class (:foreground ,func ))))
   `(org-sexp-date ((,class (:foreground ,base))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-table ((,class (:foreground ,base :background ,even-less-dark))))
   `(org-time-grid ((,class (:foreground ,str))))
   `(org-todo ((,class (:foreground ,war :inherit bold))))
   `(org-verbatim ((,class (:foreground ,keyword))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-warning ((,class (:foreground ,err))))
   ))


(provide-theme 'yesterday-glow)

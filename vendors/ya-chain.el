;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;; (package-refresh-contents)
;; (package-install 'yasnippet)

(require 'yasnippet)
(require 's)
(require 'dash)
(yas-global-mode 1)

(setq template-1 "\n$1 = $0;")
(setq template-2 "if $1 then $2")
(setq template
      "$1 isn't the $0 bomb. $19 This is $ 1 and 1$. Oh yeah. really $13 is the ${2:WHAT} and really 
${15:$(downcase yas-test)}. ${  das}")


                                        ;;(yas-expand-snippet template)
;;(lambda (s) (match-string 1))
(setq reb-re-syntax 'string)
(setq simple-field-regex "\\(\\$\\)\\([0-9]+\\)")
(setq complex-field-regex"\\(${\\)\\([0-9]+\\)\\(:[^}]*}\\)")

(setq simple-exit-field-regex "$\\(0\\)")
(setq complex-exit-field-regex"${\\(0\\):\\([^}]*\\)}")

(defun s-insert (old new position)
  "Insert string new into old position and returns"
  (concat
   (substring old 0 position)
   new
   (substring old position (length old))))

(defun my/get-fields (template-str)
   (-concat
    (s-match-strings-all simple-field-regex template-str)
    (s-match-strings-all complex-field-regex template-str)))

(defun my/get-exit-fields (template-str)
   (-concat
    (s-match-strings-all simple-exit-field-regex template-str)
    (s-match-strings-all complex-exit-field-regex template-str)))

(defun my/get-number (match)
  (string-to-number (nth 2 match)))

(defun my/add-x-to-template (n template-str)
  "Adds N offset to every occurrence of a yas-snippet-field"
  (s-replace-all
   (--map
    (cons (nth 0 it)
          (concat (nth 1 it)
                  (number-to-string (+ (my/get-number it) n))
                  (nth 3 it)))
    (my/get-fields template-str))
   template-str))

(defun my/set-0-to-latest (template-str latest)
  "Transform 0 into the latest number field"
  (s-replace-all
   (--map
    (cons (nth 0 it)
          (concat (nth 1 it)
                  (number-to-string (latest))
                  (nth 3 it)))
    (my/get-exit-fields template-str))
   template-str))
  
(defun my/get-last-position (regex str)
  "Returns the ending position of first match of regex in str"
  (cdr
   (-first-item
    (s-matched-positions-all regex template-str 1))))

(defun my/get-exit-position (template-str)
  "Returns the exit position (after 0 or end of string)"
  (or
   (or
    (my/get-last-position simple-exit-field-regex template-str)
    (my/get-last-position complex-exit-field-regex template-str))
   ;; If no exit point, returns end of str
   (length template-str)))

  ;;(replace-regexp-in-string 
   ;;(lambda (s)
    ;; (concat (match-string 1 s)
     ;;        (number-to-string (+ (string-to-number (match-string 2 s)) n))
      ;;       ))
  ;; template-str))

(defun my/yas-biggest (template-str)
  "Returns the biggest yas-snippet number"
  (--reduce-from
   (max (my/get-number it) acc) 0 (my/get-fields template-str)))

(defun my/yas-inser ()
  (interactive)
  (insert (number-to-string (my/yas-biggest template)))
  (insert
   (s-insert
    template-1
    template-2
    (my/get-exit-position template-1))))

(general-define-key :prefix default-leader-key
                    "tt" 'my/yas-inser)
19

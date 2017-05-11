;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;; (package-refresh-contents)
;; (package-install 'yasnippet)

(require 'yasnippet)
(require 's)
(require 'dash)
(require 'cl-lib)
(yas-global-mode 1)

(setq template-1 "\n$1 = $0;")
(setq template-2 "if $1 then $2")
(setq template
      "$1 isn't the $0 bomb. $19 This is $ 1 and 1$. Oh yeah. really $13 is the ${2:WHAT} and really
${20:$(downcase yas-test)}. ${  das}")


                                        ;;(yas-expand-snippet template)
;;(lambda (s) (match-string 1))
(setq reb-re-syntax 'string)
(setq simple-field-regex "\\(\\$\\)\\([0-9]+\\)")
(setq complex-field-regex"\\(${\\)\\([0-9]+\\)\\(:[^}]*}\\)")

(setq simple-exit-field-regex "$\\(0\\)")
(setq complex-exit-field-regex"${\\(0\\):\\([^}]*\\)}")


;; String manipulation functions
(defun s-insert (old new position)
  "Insert string new into old position and returns"
  (concat
   (substring old 0 position)
   new
   (substring old position (length old))))
(cl-assert (string= (s-insert "David est gentil" "n'" 6) "David n'est gentil"))

(defun s-substitute (old start end new)
  "Insert string new into old between start and end"
  (concat
   (substring old 0 start)
   new
   (substring old end (length old))))
(cl-assert (string= (s-substitute "David est gentil." 10 16 "evil") "David est evil."))


;; Regex functions to work with yassnippet
(defun chain-get-yas-fields (template-str)
  "Return all the yasnippet fields in a template"
   (-concat
    (s-match-strings-all simple-field-regex template-str)
    (s-match-strings-all complex-field-regex template-str)))

(cl-assert (equal (chain-get-yas-fields "$1 and $2") '(("$1" "$" "1") ("$2" "$" "2"))))
(cl-assert (equal
            (chain-get-yas-fields "${20:$(downcase)}")
            '(("${20:$(downcase)}" "${" "20" ":$(downcase)}"))))


(defun chain-filter-0-field (fields)
  "Return a list of the fields without the special meaning 0 field"
  (--remove
   (equal (nth 2 it) "0")
   fields))
(cl-assert (equal (chain-filter-0-field
                   (chain-get-yas-fields "$1 and $0"))
                  '(("$1" "$" "1"))))


(defun chain-max-field (fields)
  "Returns the biggest yas-snippet number in the fields"
  (--reduce-from
   (max (string-to-number (nth 2 it)) acc) 0 fields))
(cl-assert (equal (chain-max-field (chain-get-yas-fields "$1 and $22")) 22))


(defun chain-increment-yas-fields (n template-str fields)
  "Increment the number for all yasnippet fields"
  (if fields
    (s-replace-all
     (--map
      (cons (nth 0 it) ;; the full expression
            (concat
             (nth 1 it) ;; The prefix
             (number-to-string (+ (string-to-number (nth 2 it)) n))
             (nth 3 it))) ;; The last part or nil
      fields)
     template-str)
    ;; else
    template-str))

(cl-assert (equal
            (chain-increment-yas-fields 2 "$1 and $2"
                                        (chain-get-yas-fields "$1 and $2"))
            "$3 and $4"))
(cl-assert (equal
            (chain-increment-yas-fields 2 "$0"
                                        '())
            "$0"))
(cl-assert (equal
            (chain-increment-yas-fields 2 "${20:$(downcase)}"
                                        (chain-get-yas-fields "${20:$(downcase)}"))
            "${22:$(downcase)}"))


;; Functions that will serve as my api
(defun chain-nest (template-parent template-child)
  "Nest a child template inside the parent template"
  (let ((computed-child-template (chain-increment-yas-fields
        (chain-max-field (chain-get-yas-fields template-parent))
        template-child
        (chain-filter-0-field (chain-get-yas-fields template-child)))))

    (if (s-contains? "$0" template-parent)
        (s-replace
         "$0"
         computed-child-template
         template-parent)
      (s-append
       computed-child-template
       template-parent))))
(cl-assert (equal (chain-nest "if $1 then $0;" "print($1)") "if $1 then print($2);"))
(cl-assert (equal (chain-nest "if $1 then $0;" "print($0)") "if $1 then print($0);"))
(cl-assert (equal (chain-nest "if $1 then $2;" "print($1)") "if $1 then $2;print($3)"))
(cl-assert (equal (chain-nest "if $22 then $0;" "print($1)") "if $22 then print($23);"))
(cl-assert (equal (chain-nest "if $1 or $1 then $0;" "print($1 and $1)")
                  "if $1 or $1 then print($2 and $2);"))
(cl-assert (equal (chain-nest "if $1 then $0;" "print(${1:$(downcase)})")
                  "if $1 then print(${2:$(downcase)});"))
;; We should be able to chain the result of another chain
(cl-assert (equal
            (chain-nest (chain-nest "$1 = $0;" "if $1 then $0") "show($1)")
            "$1 = if $2 then show($3);"))
;; Can we reduce it? Yup.
(cl-assert (equal
            (-reduce 'chain-nest '("$1 = $0;" "if $1 then $0" "show($1)"))
            "$1 = if $2 then show($3);"))

(defun chain-append (template-parent template-child)
  "Append the template-child after the template-parent on a newline"
  (let* ((computed-parent-template
         (if (s-contains? "$0" template-parent)
             (s-replace
              "$0"
              (concat "$" (number-to-string
                           (+ (chain-max-field
                               (chain-get-yas-fields template-parent)) 1)))
              template-parent)
           template-parent))
         ;; Computing the child based on the parent
         (computed-child-template
          (chain-increment-yas-fields
           (chain-max-field (chain-get-yas-fields computed-parent-template))
           template-child
           (chain-filter-0-field (chain-get-yas-fields template-child)))))

    (s-append
     computed-child-template
     (s-append "/n" computed-parent-template))))

(cl-assert (equal (chain-append "$1 and $2;" "if $1") "$1 and $2;/nif $3"))
(cl-assert (equal (chain-append "$1 and $0;" "if $1") "$1 and $2;/nif $3"))

;; Parsing functions
(defun chain-parse-tokens (str)
  "Parse a string into the different tokens we want"
  )

;; Shunting yard algorithm
(setq chain-precedences
      '(("APPEND" . 3) ("NEST" . 4) ("MUL" . 5) ("SOFTNEST" . 2) ("SOFTAPPEND" . 1) ("(" . 999)
        ))
(defun chain-has-precedence-p (operator1 operator2)
  "Return t if operator1 has precedence over operator2"
  (> (cdr (assoc operator1 chain-precedences)) (cdr (assoc operator2 chain-precedences))))
(cl-assert (equal (chain-has-precedence-p "APPEND" "NEST") nil))
(cl-assert (equal (chain-has-precedence-p "NEST" "APPEND") t))
(cl-assert (equal (chain-has-precedence-p "NEST" "NEST") nil))
(cl-assert (equal (chain-has-precedence-p "APPEND" "(") nil))

(defun chain-pop-operators (operator outputStack operatorStack)
  "Function that will pop higher or equal precedence operators from operatorStack"
  ;; It will pop it into outputStack whic will be returned
  (let ((popped (car operatorStack)))
    (cond
     ((equal popped nil) `(,outputStack ,(cons operator nil)))
     ((equal (chain-has-precedence-p operator popped) nil)
      (chain-pop-operators operator (cons popped outputStack) (cdr operatorStack)))
     (t `(,outputStack ,(cons operator operatorStack))))))

(cl-assert (equal (chain-pop-operators "NEST" nil nil) '(nil ("NEST"))))
(cl-assert (equal (chain-pop-operators "NEST" '(1 2) nil) '((1 2) ("NEST"))))
(cl-assert (equal (cdr (chain-pop-operators "NEST" '(1 2) nil)) '("NEST")))
(cl-assert (equal (chain-pop-operators "NEST" '(1) nil) '((1) ("NEST"))))
(cl-assert (equal
            (chain-pop-operators "NEST" '(1 2) '("MUL" "APPEND"))
            '(("MUL" 1 2) ("NEST" "APPEND"))))
(cl-assert (equal
            (chain-pop-operators "NEST" '(1 2) '("NEST" "MUL" "APPEND"))
            '(("MUL" "NEST" 1 2) ("NEST" "APPEND"))))


(defun chain-pop-parenthesis (outputStack operatorStack)
  "Function that will pop operators from the stack until left parens"
  (let ((popped (car operatorStack)))
    (if (equal popped "(")
        ;; Alright we return!
        `(,outputStack ,(cdr operatorStack))
      (when popped (chain-pop-parenthesis (cons popped outputStack) (cdr operatorStack))))))

(cl-assert (equal
            (chain-pop-parenthesis '("APPEND" 1 2) '("MUL" "(" "APPEND"))
            '(("MUL" "APPEND" 1 2) ("APPEND"))))
(cl-assert (equal
            (chain-pop-parenthesis '("APPEND" 1 2) '("MUL" "("))
            '(("MUL" "APPEND" 1 2) nil)))

(defun chain-parenp (token)
  "Return true if token is a parenthesis"
  (member token '("LEFT" "RIGHT")))

(defun chain-operatorp (token)
  "Return true if token is an operator"
  (member token '("APPEND" "NEST" "MUL" "SOFTAPPEND" "SOFTNEST")))

(defun chain-rpn-analyze (tokens outputStack operatorStack)
  "Recursive function that analyze the next token on the stack"
  (let ((token (car tokens)))
    (cond
     ((equal token nil)
      (if (equal (car operatorStack) nil)
          ;; We have the final answer
          ;; We will reverse the list so it is in rpn format
          (reverse outputStack)
        ;; We recursively pop the operator stack onto the output stack
        (chain-rpn-analyze tokens
                           (cons (car operatorStack) outputStack)
                           (cdr operatorStack))))
     ((equal token "(")
      ;; Left parenthesis go on the operator stack
      (chain-rpn-analyze (cdr tokens) outputStack (cons token operatorStack)))
     ((equal token ")")
      ;; We pop the operator stack until we found the left parenthesis
      (let ((result (chain-pop-parenthesis outputStack operatorStack)))
        (chain-rpn-analyze (cdr tokens) (car result) (car (cdr result)))))
     ((chain-operatorp token)
      ;; If it is an operator we need to pop operators
      ;; until the operator stack has a lower precedence
      (let ((result (chain-pop-operators token outputStack operatorStack)))
        (chain-rpn-analyze (cdr tokens) (car result) (car (cdr result)))))
     (t
      ;; It is a value we append to the outputstack
      (chain-rpn-analyze (cdr tokens) (cons token outputStack) operatorStack)
      ))))

(cl-assert (equal
           (chain-rpn-analyze '(1 "APPEND" 2) nil nil)
           '(1 2 "APPEND")))
(cl-assert (equal
           (chain-rpn-analyze '(1 "APPEND" 2 "APPEND" 3) nil nil)
           '(1 2 "APPEND" 3 "APPEND")))
(cl-assert (equal
           (chain-rpn-analyze '(1 "APPEND" 2 "MUL" 3) nil nil)
           '(1 2 3 "MUL" "APPEND")))
(cl-assert (equal
           (chain-rpn-analyze '(1 "MUL" 2 "APPEND" 3) nil nil)
           '(1 2 "MUL" 3 "APPEND")))
(cl-assert (equal
           (chain-rpn-analyze '("(" 1 "APPEND" 2 ")" "MUL" 3) nil nil)
           '(1 2 "APPEND" 3 "MUL")))

(defun chain-rpn (tokens)
  "Parse tokens and transform it into the rpn (reverse polish notation)"
  )

(defun chain-eval-rpn (rpn-stack)
  "Evaluate the RPN stack sent in params"
  )


(defun my/get-exit-fields (template-str)
   (-concat
    (s-match-strings-all simple-exit-field-regex template-str)
    (s-match-strings-all complex-exit-field-regex template-str)))

(defun chain-increment-template (n template-str)
  "Adds N offset to every occurrence of a yas-snippet-field"
  (s-replace-all
   (--map
    (cons (nth 0 it)
          (concat (nth 1 it)
                  (number-to-string (+ (my/get-number it) n))
                  (nth 3 it)))
    (my/get-fields template-str))
   template-str))

(defun my/get-number (match)
  (string-to-number (nth 2 match)))


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
$1 = $0if $1 then $2;


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
(setq chain-token-operators '(">" "," "*" "+" "_" ","))
(setq chain-token-precedences
      '(("+" . 3) (">" . 4) ("*" . 5) ("_" . 2) ("," . 1) ("(" . 0)))

(defun chain-token-operatorp (operator)
  "Return truthy if token is an operator"
  (-contains-p chain-token-operators operator))
(cl-assert (equal (chain-token-operatorp ">") t))
(cl-assert (equal (chain-token-operatorp "<") nil))

(defun chain-token-parenthesisp (parenthesis)
  "Return truthy if token is a parenthesis"
  (or (equal parenthesis ")") (equal parenthesis "(")))
(cl-assert (equal (chain-token-parenthesisp "(") t))
(cl-assert (equal (chain-token-parenthesisp 2) nil))

(defun chain-token-symbolp (symbol)
  "Return truthy if token is a symbol"
  (unless (equal symbol nil)
    (unless (chain-token-operatorp symbol)
      (unless (chain-token-parenthesisp symbol)
        (when (equal (string-to-number symbol) 0) t)))))

(cl-assert (equal (chain-token-symbolp ">") nil))
(cl-assert (equal (chain-token-symbolp ")") nil))
(cl-assert (equal (chain-token-symbolp "2") nil))
(cl-assert (equal (chain-token-symbolp "david") t))
(cl-assert (equal (chain-token-symbolp nil) nil))


(defun chain-recur-queue-token (input output)
  "Recursively check for tokens, transform them if necessary and checks for error"
  (let ((next (car input))
        (remain (cdr input))
        (last (car output)))
    ;; Checking for possible errors
    (if (equal last nil)
        (when (chain-token-operatorp next)
          (user-error "Cannot start chain with an operator!"))
      (when (and (chain-token-operatorp next) (chain-token-operatorp (car output)))
        (user-error "Invalid syntax: Two operators following each other")))

    (cond
     ;; Empty input queue
     ((equal next nil) (reverse output))

     ;; Parenthesis and operators are simply added to output stack
     ((chain-token-operatorp next) (chain-recur-queue-token remain (cons next output)))
     ((chain-token-parenthesisp next) (chain-recur-queue-token remain (cons next output)))

     ;; Number (only for multiplicator)
     ((not (equal (string-to-number next) 0))
      (if (equal last "*")
          (chain-recur-queue-token remain (cons (string-to-number next) output))
        (user-error "Invalid syntax: Use numbers only for the multiplicator(*) operator")))

     ;; If we are here we have a symbol, we check if previous was symbol
     ;; and we insert the default _ operator (softnesting)
     ((chain-token-symbolp last) (chain-recur-queue-token input (cons "_" output)))

     ((equal last "*") (user-error "Invalid syntax: cannot multiply two templates together"))

     (t (chain-recur-queue-token remain (cons next output))))))
(cl-assert (equal (chain-recur-queue-token '("a") nil) '("a")))
(cl-assert (equal (chain-recur-queue-token '("a" ">" "b") nil) '("a" ">" "b")))
(cl-assert (equal (chain-recur-queue-token '("a" ">" "b" ">" "c") nil) '("a" ">" "b" ">" "c")))
(cl-assert (equal (chain-recur-queue-token '("a" "b") nil) '("a" "_" "b")))
(cl-assert (equal (chain-recur-queue-token '("a" "*" "8") nil) '("a" "*" 8)))

(defun chain-parse-tokens (str)
  "Parse a string into the different tokens we want"
  ;; Pad our special characters with space
  (chain-recur-queue-token
   (split-string
    (s-replace-all
     (--map (cons it (s-center 3 it)) (append '("(" ")") chain-token-operators)) str)) nil))
(cl-assert (equal (chain-parse-tokens "d > a > e") '("d" ">" "a" ">" "e")))
(cl-assert (equal (chain-parse-tokens "d>a>e") '("d" ">" "a" ">" "e")))
(cl-assert (equal (chain-parse-tokens "d a > e") '("d" "_" "a" ">" "e")))

;; Shunting yard algorithm
(defun chain-has-not-precedence-p (operator1 operator2)
  "Return t if operator1 has precedence over operator2"
  (<= (cdr (assoc operator1 chain-token-precedences)) (cdr (assoc operator2 chain-token-precedences))))
(cl-assert (equal (chain-has-not-precedence-p "+" ">") t))
(cl-assert (equal (chain-has-not-precedence-p ">" "+") nil))
(cl-assert (equal (chain-has-not-precedence-p ">" ">") t))
(cl-assert (equal (chain-has-not-precedence-p "+" "(") nil))

(defun chain-take-while (predicate input)
  "Variation of the dash.el take while that returns elements matching predicate and the remainder"
  (let ((result (-take-while predicate input)))
    `(,result ,(-slice input (length result)))))
(cl-assert (equal (chain-take-while (lambda (item) (> item 2)) '(4 5 2 5 6)) '((4 5) (2 5 6))))
(cl-assert (equal (chain-take-while (lambda (item) (> item 2)) '(4 5 3 5 6)) '((4 5 3 5 6) nil)))

(defun chain-rpn-analyze (input output stack)
  "Recursive function that analyze the next token on the stack"
  (let ((next (car input))
        (remain (cdr input))
        (last-operator (car stack)))
    (cond
     ;; We have the final answer we append stack to output
     ((equal next nil) (reverse (append (reverse stack) output)))
     ((equal next "(") (chain-rpn-analyze remain output (cons next stack)))
     ;; When we have a right parenthesis, we pop the operator stack until
     ;; we found the left parenthesis
     ((equal next ")")
      (let ((result
             (chain-take-while (lambda (item) (not (equal item "("))) stack)))
        (chain-rpn-analyze remain
                           (append (reverse (car result)) output)
                           (-slice (car (cdr result)) 1))))

     ;; If it is an operator we need to pop operators
     ;; until the operator stack has a lower precedence
     ((chain-token-operatorp next)
      (let ((result
             (chain-take-while (lambda (item) (chain-has-not-precedence-p next item)) stack)))
        ;; You may notice we often reverse the result before doing append
        ;; The reason is we use the lists as stacks
        (chain-rpn-analyze remain (append (reverse (car result)) output) (cons next (car (cdr result))))))
     (t (chain-rpn-analyze remain (cons next output) stack)))))

(cl-assert (equal
           (chain-rpn-analyze '(1 "+" 2) nil nil)
           '(1 2 "+")))
(cl-assert (equal
           (chain-rpn-analyze '(1 "+" 2 "+" 3) nil nil)
           '(1 2 "+" 3 "+")))
(cl-assert (equal
           (chain-rpn-analyze '(1 "+" 2 "*" 3) nil nil)
           '(1 2 3 "*" "+")))
(cl-assert (equal
            (chain-rpn-analyze '(1 "+" 2 "*" 3 "+" 4 "*" 3) nil nil)
           '(1 2 3 "*" "+" 4 3 "*" "+")))
(cl-assert (equal
           (chain-rpn-analyze '(1 "*" 2 "+" 3) nil nil)
           '(1 2 "*" 3 "+")))
(cl-assert (equal
           (chain-rpn-analyze '("(" 1 "+" 2 ")" "*" 3) nil nil)
           '(1 2 "+" 3 "*")))



(setq templates
      '(("i" . "if $1 then $0;")
        ("v" . "const $1 = $0;")
        ("v:i" . "$1 ? $2 : $0")
        ("c" . "class $1 extends $2: $0")
        ("c:f"   . "function $1(self, $2):")
        ("f" . "function $1($2):")))

;; Not sure if it is the best way to do what I want
;; TODO: Refactor when better understanding of lexical binding
(defun chain-find-template (template-keys)
  "Recursively try to find a template"
  (let ((result (assoc (string-join template-keys ":") templates)))
    (if result (cdr result) (chain-find-template (cdr template-keys)))))
(cl-assert (equal (chain-find-template '("v" "i")) "$1 ? $2 : $0"))
(cl-assert (equal (chain-find-template '("v" "c")) "class $1 extends $2: $0"))

;; Represents a template that is wrapped for lazy evaluation
(defclass wrapped-template ()
  ((name :initarg :name
         :type string)
   (value :initarg :value)))

(defun chain-eval-template (wrapped parents)
  "Evaluate a wrapped template"
  (funcall (oref wrapped value) parents))

(defun chain-wrap-template (template-key)
  "Wrap a template-key into a lazy accessor"
  (wrapped-template :name template-key
                    :value `(lambda (parents)
                              (chain-find-template (append parents '(,template-key))))))

(defun chain-nest-wrapped-template (wrapped-1 wrapped-2)
  "Nest wrapped-2 inside wrapped-1 and returns a wrapped template"
  (wrapped-template :name (oref wrapped-1 name)
                    :value `(lambda (parents)
                              (chain-nest
                               (funcall ,(oref wrapped-1 value) parents)
                               (funcall ,(oref wrapped-2 value)
                                        (append parents '(,(oref wrapped-1 name))))))))
(cl-assert (equal
            (chain-eval-template
             (chain-nest-wrapped-template
              (chain-wrap-template "v")
              (chain-wrap-template "i")) nil)
            "const $1 = $2 ? $3 : $0;"))

(defun chain-append-wrapped-template (wrapped-1 wrapped-2)
  "Append wrapped-2 after wrapped-1"
  (wrapped-template :name (oref wrapped-2 name)
                    :value `(lambda (parents)
                              (chain-append
                               (funcall ,(oref wrapped-1 value) parents)
                               (funcall ,(oref wrapped-2 value) parents)))))
(cl-assert (equal
            (chain-eval-template
             (chain-append-wrapped-template
              (chain-wrap-template "v")
              (chain-wrap-template "i")) nil)
            "const $1 = $2;/nif $3 then $0"))

(defun chain-eval-rpn (input output)
  "Evaluate the RPN stack sent in params"
  (let ((next (car input)) (remain (cdr input)))
    (cond
     ((equal next nil) (chain-eval-template (car output) nil))
     ;; Nest and soft nesting
     ((or (equal next ">") (equal next "_"))
      (chain-eval-rpn remain (cons
                              (chain-nest-wrapped-template
                               (nth 1 output)
                               (nth 0 output))
                              (nthcdr 2 output))))

     ;; Append and soft appending
     ((or (equal next "+") (equal next ","))
      (chain-eval-rpn remain (cons
                              (chain-append-wrapped-template
                               (nth 1 output)
                               (nth 0 output))
                              (nthcdr 2 output))))

     ;;Factoring
     ((equal next "*") nil)

     ;; Simple term
     (t (chain-eval-rpn remain (cons (chain-wrap-template next) output)))
     )))

(cl-assert (equal (chain-eval-rpn '("v" "i" ">") nil) "const $1 = $2 ? $3 : $0;"))
(cl-assert (equal (chain-eval-rpn '("v" "i" "+") nil) "const $1 = $2;/nif $3 then $0"))

(defun chain-rpn (expression)
  "Parses a zen expression and return a yasnippet template"
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

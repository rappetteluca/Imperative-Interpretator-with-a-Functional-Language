#lang Scheme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts a scheme-expression into a string
;; INPUT: a scheme-expression EXP
;; OUTPUT: a SCHEME String corresponding to EXP
(define (exp->string exp)
  (cond ((number? exp) (number->string exp))
        ((symbol? exp) (symbol->string exp))
        ((list? exp) (exp->string (car exp)))))

;; INPUT: a list of lists
;; OUTPUT: a list containing all elements of the first-level lists
(define (flatten list-of-lists)
  (cond ((null? list-of-lists) '())
        (else (append (car list-of-lists) (flatten (cdr list-of-lists))))))

;; this is for all error handling.
;; programmers don't use this function but
;; the interpreter calls this function to
;; signal some type of programmer error
(define (error msg)
  (display "ERROR: ")
  (display msg)
  (newline))

;; THERE ARE TWO SUPPORTED TYPES: 'int and 'boolean
;; INPUT: an element of the ART-C language
;; OUTPUT: the type of that element
(define (type-of val)
  (cond ((number? val) 'int)
        ((boolean? val) 'boolean)))

;; A MAP is a list of key-value pairs
;; INPUT: a MAP and a KEY
;; OUTPUT: The value associated with the key or 'error
(define (map-get map x)
  (cond ((null? map) 'error)
        ((equal? (car (car map)) x) (cadr (car map)))
        (else (map-get (cdr map) x))))

;; INPUT : A MAP AND KEY
;; OUTPUT : true if the key is in the map and false otherwise
(define (map-contains map x)
  (cond ((null? map) #f)
        ((equal? (car (car map)) x) #t)
        (else (map-contains (cdr map) x))))

;; INPUT : A MAP, KEY and VALUE
;; OUTPUT: The map that results from replacing the key with the new value.  If
;; the map doesn't contain KEY, then 'error is returned
(define (map-replace map key val)
  (cond ((null? map) 'error)
        ((equal? (car (car map)) key)
         (cons (list key val) (cdr map)))
        (else
         (cons (car map) (map-replace (cdr map) key val)))))

;; INPUT : A MAP, Key and Value
;; OUTPUT : The map that results from adding a key-value pair.  This
;; allows for duplicate keys (the most-recently added is nearer the front of the list
(define (map-add map key val)
  (cons (list key val) map))

;; INPUT: A MAP and KEY
;; OUTPUT: The map that results from deleting the key.  No errors occur if the map
;; doesn't contain the key
(define (map-delete map key)
  (cond ((null? map) map)
        ((equal? (car (car map)) key) (cdr map))
        (else (cons (car map)
                    (map-delete (cdr map) key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPEMAP : A SEMANTIC DOMAIN DATA TYPE
;; A typemap is a list of block-level declarations.
;; FORM: ((var1 type1) (var2 type2) (var3 type3) ... )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT: NONE
;; OUTPUT: AN empty typemap
(define (typemap-create-empty) '())

;; INPUT: A TYPEMAP
;; OUTPUT: The type of variable x
(define (typemap-type-of tm x)
  (map-get tm x))

;; INPUT: A TYPEMAP
;; OUTPUT: THE TYPEMAP THAT RESULTS FROM INSERTING A DECLARATIONS
(define (typemap-add tm decl)
  (map-add tm (car decl) (cadr decl)))

(define (typemap-delete tm key)
  (map-delete tm key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE : A SEMANTIC DOMAIN DATA TYPE
;; A LIST OF (VAR, VALUE) pairs
;; FORM :  ( (var1 val1) (var2 val2) ... )
;; NOTE: A map can contain duplicate keys but innermost KEYS occur
;;       before outermost KEYS and hide them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INPUT : NONE
;; OUTPUT: AN EMPTY STATE
(define (state-create-empty) '())
  
;; INPUT: STATE and ID
;; OUTPUT: a new state such that the innermost scope now contains a
;;         new binding for the specified ID.  The bindings value is 'undefined.
(define (state-add state id)
  (map-add state id 'undefined))

;; INPUT : STATE and ID
;; OUTPUT: A new state such that the innermost id is removed
(define (state-delete state id)
  (map-delete state id))

;; INPUT: STATE and ID
;; OUTPUT: The value associated with the specified ID in the given state
(define (state-get-value state id)
  (map-get state id))

;; INPUT: STATE and ID
;; OUTPUT: A new state that results from changing the mapping from id->value in
;;         the specified state
(define (state-update state id value)
  (map-replace state id value))

;; INPUT: STATE and LIST-OF-IDS (VARIABLES)
;; OUTPUT: A new state that results from deleting all ids (the variables) from
;;         the specified state
(define (state-delete-all state variables)
  (cond ((null? variables) state)
        (else (state-delete-all (state-delete state (car variables)) (cdr variables)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THESE CLASSES CORRESPOND TO THE ABSTRACT SYNTAX SUCH THAT A "PROGRAM"
;; REPRESENTS A PARSE-TREE.  THESE FUNCTIONS OPERATE AT THE 'SYNTACTIC' LEVEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (PROGRAM BODY)
(define (program-get-body stmt)
  (cadr stmt))

;; (BLOCK S1...SN)
(define (block-get-body stmt)
  (filter (lambda (x) (not (is-declaration? x))) (cdr stmt)))
  
(define (block-get-declarations stmt)
  (filter (lambda (x) (is-declaration? x)) (cdr stmt)))

;; (DECLARE TYPE VAR)
(define (declaration-get-type stmt)
  (cadr stmt))

(define (declaration-get-var stmt)
  (caddr stmt))

(define (is-declaration? stmt)
  (and (list? stmt) 
       (equal? (car stmt) 'declare)))

;; (:= VAR EXP)
(define (assignment-get-var stmt)
  (cadr stmt))

(define (assignment-get-exp stmt)
  (caddr stmt))

;; (IF TEST THEN [ELSE])
(define (if-get-test stmt)
  (cadr stmt))

(define (if-get-then stmt)
  (caddr stmt))

(define (if-has-else? stmt)
  (= (length stmt) 4))

(define (if-get-else stmt)
  (cadddr stmt))

;; (WHILE TEST BODY)
(define (while-get-test stmt)
  (cadr stmt))

(define (while-get-body stmt)
  (caddr stmt))

;; (SPRINT LABEL EXP)
(define (sprint-has-exp? stmt)
  (and (list? stmt)
       (= (length stmt) 3)))

(define (sprint-get-label? stmt)
  (cadr stmt))

(define (sprint-get-exp stmt)
  (caddr stmt))

;; INPUT: an expression EXP
;; OUTPUT: the operator of EXP (an element of ART-C)
(define (exp-get-operator exp)
  (car exp))

;; INPUT: an expression EXP
;; OUTPUT: the left-operand (an expression) of EXP
(define (exp-get-left-operand exp)
  (car (cdr exp)))

;; INPUT: an expression EXP
;; OUTPUT: the exp-get-right-operand (an expression) of EXP
(define (exp-get-right-operand exp)
  (car (cdr (cdr exp))))

;; INPUT: an expression EXP
;; OUTPUT: #t if the expression is a boolean literal and #f otherwise
(define (bool? exp)
  (or (equal? exp 'true)
      (equal? exp 'false)))

;; INPUT: a symbol
;; OUTPUT: #t if the symbol is 'true and #f if it is 'false and 'void' if neither
(define (symbol->bool sym)
  (cond ((equal? sym 'true) #t)
        ((equal? sym 'false) #f)))


;; INPUT: A PROGRAM
;; A PROGRAM has syntactic structure (program stmt)
;; OUTPUT: THE STATE that results from executing the program
;;         in an empty state.
(define (interpret-program pgm)
  (interpret (program-get-body pgm) (state-create-empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the INTERPRETER class
;; An INTERPRETER is simply a collection of functions that
;; operates on TYPES, STATES, BINDING, SCOPES and PROGRAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT: STATEMENT and STATE
;; OUTPUT: The state that results from executing STATEMENT in STATE
(define reservedWords '(program block declare := if while sprint ~ + - * / @ < > = <= >= & % boolean int true false))

(define (interpret stmt state)
  (display stmt) (newline) (display state) (newline)
  (cond ((null? stmt) state))
  (let ((kind (car stmt)))
    (cond ((equal? kind 'block) (interpret-block stmt state))
          ((equal? kind 'declare) (interpret-declaration stmt state))
          ((equal? kind ':=) (interpret-assignment stmt state))
          ((equal? kind 'if) (interpret-if stmt state))
          ((equal? kind 'sprint) (interpret-sprint stmt state))
          ((equal? kind 'while) (interpret-while stmt state))       
          (else (error (string-append "statement expected but saw (" (exp->string stmt) "...) instead."))))))

(define pgm '(program 
              (block
               (declare int n)
               (declare boolean error)
               (declare int result)   
               (:= error false)
               (:= result 1)
               (block 
                (declare int local)
                (:= n 5)
                (:= local n)
                (while (> local 0)
                       (block
                        (:= result (* result local))
                        (:= local (- local 1)))))
              (sprint "result: " result)
              (if (! error) (sprint "a") (sprint "b")))))

(define (is-program-valid? pgm)
  (cond ((equal? (interpret-program pgm) '()) #f)
        (else #t)))

(define (interpret-block block state)
  (cond ((null? block) state))
  (block-iterate (cdr block) state))

(define (block-iterate block state)
  (cond ((null? block) state)
        ((void? state) '())
        (else (block-iterate (cdr block) (interpret (car block) state)))))
  
(define (interpret-declaration decl state)
  (cond ((eqv? (validate-declaration decl state) #f) (error decl))
        (else (state-update (state-add state (declaration-get-var decl)) (declaration-get-var decl) (declaration-get-type decl))  
        )))

(define (validate-declaration decl state)
  (cond ((eqv? (map-contains state (declaration-get-var decl)) #t) #f)
        ((member (declaration-get-var decl) reservedWords) #f)
        (else #t)))

(define (interpret-assignment assign state)
  (cond ((eqv? (validate-assignment assign state) #f) (error assign))
        (else (state-update state (assignment-get-var assign) (interpret-expression (assignment-get-exp assign) state)))))

(define (validate-assignment assign state)
  (cond ((equal? (type-of (state-get-value state (assignment-get-var assign))) (type-of (interpret-expression (assignment-get-exp assign) state))) #t)
        ((equal? (state-get-value state (assignment-get-var assign)) (type-of (interpret-expression (assignment-get-exp assign) state))) #t)
        (else #f)))

(define (interpret-if conditional state)
  (cond ((eqv? (validate-if conditional state) #f) (error conditional))
        ((eqv? (interpret-expression (if-get-test conditional) state) #t) (interpret (if-get-then conditional) state))
        ((if-has-else? conditional) (interpret (if-get-else conditional) state))
        (else state)))

(define (validate-if conditional state)
  (cond ((boolean? (interpret-expression (if-get-test conditional) state)) #t)
        (else #f)))


(define (interpret-while conditional state)
  (cond ((eqv? (validate-while conditional state) #f) (error conditional))
        (else (iterate-while conditional state))))

(define (validate-while conditional state)
  (cond ((boolean? (interpret-expression (while-get-test conditional) state)) #t)
        (else #f)))

(define (iterate-while conditional state)
  (cond ((eqv? (interpret-expression (while-get-test conditional) state) #f) state)
        (else (iterate-while conditional (interpret (while-get-body conditional) state)))))

(define (interpret-sprint expression state)
  (cond ((sprint-has-exp? expression) (display (sprint-get-label? expression)) (print-sprint (interpret-expression (sprint-get-exp expression) state) state))
        (else (print-sprint state state))))

(define (print-sprint exp state)
  (display exp) (newline)
  state)

(define (validate-expression exp state)
  (cond ((and (not (list? exp)) (equal? (state-get-value state exp) 'int)) #f)
        ((and (not (list? exp)) (equal? (state-get-value state exp) 'boolean)) #f)
        ((and (not (list? exp)) (equal? (type-of (state-get-value state exp)) 'int)) (validate-int (state-get-value state exp)))
        (else #t)))

(define (validate-int int)
  (cond ((not (integer? int)) #f)
        ((> int 2147483647) #f)
        ((< int -2147483648) #f)
        (else #t)))
   

(define (expTree-create root left right)
  (cons root (cons left (cons right '()))))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (interpret-expression exp state)
  (cond ((eqv? (validate-expression exp state) #f) (error exp))
        ((and (list? exp) (or (equal? (exp-get-operator exp) '~) (equal? (exp-get-operator exp) '!))) (not (interpret-expression (cadr exp) state)))
        ((and (and (list? exp) (= 2 (length exp))) (equal? (exp-get-operator exp) '-)) (eval-exp (* -1 (interpret-expression (cadr exp) state)) state))
        ((list? exp) (eval-exp (expTree-create (exp-get-operator exp) (interpret-expression (exp-get-left-operand exp) state) (interpret-expression (exp-get-right-operand exp) state)) state))
        (else (eval-exp exp state))))

(define (eval-exp exp state)
  (cond ((and (bool? exp) (equal? exp 'false)) #f)
        ((and (bool? exp) (equal? exp 'true)) #t)
        ((eqv? (map-contains state exp) #t) (state-get-value state exp))
        ((void? exp) '())
        ((and (list? exp) (or (void? (exp-get-left-operand exp)) (void? (exp-get-right-operand exp)))) '())
        ((number? exp) exp)
        ((and (list? exp) (not (equal? (type-of (exp-get-left-operand exp)) (type-of (exp-get-right-operand exp))))) (error exp))
        ((and (list? exp) (equal? (exp-get-operator exp) '+)) (+ (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '-)) (- (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '*)) (* (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '/)) (/ (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '@)) (expo (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '?)) (remainder (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '<)) (< (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '>)) (> (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '=)) (= (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '<=)) (<= (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '>=)) (>= (exp-get-left-operand exp) (exp-get-right-operand exp)))
        ((and (list? exp) (equal? (exp-get-operator exp) '&)) (and (eqv? (exp-get-left-operand exp) #t) (eqv? (exp-get-right-operand exp) #t)))
        ((and (list? exp) (equal? (exp-get-operator exp) '%)) (or (eqv? (exp-get-left-operand exp) #t) (eqv? (exp-get-right-operand exp) #t)))
        (else (eval exp ns))))

(define (expo n i)
  (cond ((= i 1) n)
        ((= i 0) 1)
        (else (* n (expo n (- i 1))))))

(is-program-valid? pgm)
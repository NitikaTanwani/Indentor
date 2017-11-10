(require rackunit)
(require "extras.rkt")
(require racket/string)
(require racket/list)
;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: GarterSnake program and a width
;; RETURNS: a representation of the program as a sequence of lines,
;;         following the formatting rules described below.

;; A Program is a ListOfDefinition.

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

(define-struct varexp (name))
(define-struct appexp (fn args))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; A Variable is a Symbol.
;;Your task is to write

;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: GarterSnake program and a width
;; RETURNS: a representation of the program as a sequence of lines,
;;         following the formatting rules described below.

(define (program-to-strings prog w)
  (cond
    [(empty? prog) prog]
    [else (append (def-arrange(first  prog) w)
                  (program-to-strings (rest  prog) w))]))
;;Take the make-def one by one from given program and returns the list of strings corresponding to it.

(define (def-arrange p w)
  (append
   (header-arrange (def-name p) (def-args p) w)
   (body-arrange (def-body p) w)))

;;Takes the body of the def structure and returns the corresponding list of strings
(define (body-arrange b w)
  (if (empty? (appexp-args b))
      (list (string-append "    " (symbol->string(appexp-fn b)) "(" ")"))
      ;; (if (check-varexp? (appexp-args b))
      (handle-varexp b w)
      ))

(define (handle-both b)
  b)
;;Returns true if the entire list contains varexp structures else false.
(define (check-varexp? l)
  (cond
    [(empty? l) false]
    [(and(empty? (rest l)) (varexp? (first l))) true]
    [else (check-varexp? ( rest l))]))
;;It returns the string or list of strings if the args of body contains only structures of varexps
(define (handle-varexp b w)
  (cond
    ;; [(appexp?(first (appexp-args b))) (handle-varexp (first (appexp-args b)) 20)]  
    [(=(length (appexp-args b))1) (list(one-arg/line b))]
    [(if(list?(extract-var (appexp-args b)))
        (<=(string-length(string-append(string-append* "    " (symbol->string(appexp-fn b)) "("
                                                       
                                                       (extract-var (appexp-args b))) ")")) w)
        (<=(string-length(string-append(string-append "    " (symbol->string(appexp-fn b)) "("
                                                      
                                                      (extract-var (appexp-args b))) ")")) w))
     
     (list(one-arg/line b))]
    [(or(>= (string-length(symbol->string(appexp-fn b))) w) (>= (+ 4(string-length(symbol->string(appexp-fn b)))) w))
     (all-arg-seperate-line b)]
    [else (arg-seperate-line b)]))
;;It takes list of structures and converts them to list of strings 
(define (structname-to-list b)
  (cond
    [(empty? b) b]
    [else ( cons (symbol->string(varexp-name (first b))) (structname-to-list (rest b)))]))
;;all-arg-seperate-line is means the body is arranged such that every argument is on a different list.
;;arg-seperate-line,except the first every arg is on the first line.
(define (one-arg/line b)
  (if (list? (extract-var (appexp-args b)))
      (string-append (string-append* "    " (symbol->string(appexp-fn b)) "(" (extract-var (appexp-args b))) ")")
      (string-append "    "(symbol->string (appexp-fn b)) "(" (extract-var (appexp-args b)) ")")))

(define (all-arg-seperate-line b)
  (cons(string-append "    " (symbol->string(appexp-fn b))) (print-seperate (appexp-args b)
                                                                            (string-length (string-append "    " (symbol->string(appexp-fn b))) ))))
;;added another parameter to print seperate whichis length of first line.
(define (print-seperate a l)
  (cond
    [(empty? a) a]
    [(appexp? (first a)) (append (handle-varexp (first a) 20) (print-seperate-1 (rest a) l))]
    [else (cons (string-append (spaces l) "(" (symbol->string (varexp-name (first a)))
                               (if (=(length a)1)
                                   ")"
                                   ","))
                (print-seperate-1 (rest a) l))]))

(define (print-seperate-1 r l)
  (cond   [(empty? r) r ];;(list (string-append (spaces (+ l 1))")"))]
    [(appexp? (first r)) (append (handle-varexp (first r) 20) (print-seperate-1 (rest r) l))]
    [else (cons(string-append (spaces (+ l 1)) (symbol->string(varexp-name (first r))) (if (=(length r)1)
                                                                                           ")"
                                                                                           ","))
               (print-seperate-1 (rest r) l))]))

(define (arg-seperate-line b)
  (cond
    [(appexp? (first (appexp-args b)))(cons(string-append "" (symbol->string (appexp-fn b)) "(")   (append (handle-varexp (first (appexp-args b)) 20)
                                                                                                                               (print-first(rest (appexp-args b)) (string-length(symbol->string(appexp-fn b))) )))]
    [else(cons(string-append "   " (symbol->string (appexp-fn b)) "(" (symbol->string(varexp-name (first (appexp-args b)))))
              (print-first(rest (appexp-args b)) (string-length(symbol->string(appexp-fn b))) ))]))

(define (print-first a s)
  (cond
    [(empty? a) a]
    [(appexp? (first a)) (append (handle-varexp (first a) 20) (print-first(rest a) s))]
    [else (cons (string-append (spaces (+ s 1))
                               (symbol->string(varexp-name(first a))))
                (print-first (rest a) s))]))


;;Takes the header of the make-def i.e name and variables and returns indented string
;;or list of strings.
(define (header-arrange n a w)
  (cond
    [(= (length a) 1) (single-line n a w)]
    [(>(+ 4 (string-length (symbol->string n)) (string-length (extract-var a))) w) (seperate-line n a w)]
    [(or (>=(string-length (symbol->string n)) w) (>= (+ 4 (string-length (symbol->string n))) w))
     (seperate-line n a w)]
    [(or (<=(string-length (symbol->string n)) w) (<= (+ 4 (string-length (symbol->string n))) w))
     (single-line n a w)]
    [else (single-line n a w)]))
;;The header is written in multiple lines ,takes header name,list of args and w and
;;returns a list of string
(define (seperate-line n a w)
  (cons (string-append "def " (symbol->string n) "(" (symbol->string(first a)) ",")
        (line-creator (rest a) w (string-length (string-append "def " (symbol->string n)
                                                               "(" )))))
;;The header is short enough to be arranged in a single line
(define (single-line n a w)
  (list (string-append "def " (symbol->string n) "(" (extract-var a) ")")))
;;line creator,creates line/strings with proper identation 
(define (line-creator l w size)
  (cond
    [(empty? l) l]
    
    [else (cons (string-append (spaces size) ;;(if  (<(- size (string-length (symbol->string (first l))))0)
                               ;;    (* -1 (- size (string-length (symbol->string (first l)))))
                               ;;   (- size (string-length (symbol->string (first l))))))
                               (symbol->string (first l)) (if (string=? (symbol->string(last l))
                                                                        (symbol->string(first l)))
                                                              ")" ","))
                (line-creator (rest l) w size ))]))
;;converts a list of symbols into a string
(define (extract-var l)
  (cond
    [(empty? l) ""]
    [(appexp? (first l)) (append  (handle-varexp (first l) 20) (if (list?(extract-var (rest l)))
                                                                   (extract-var (rest l))
                                                                   (list (extract-var (rest l)))))]
    [(=(length l)1) (extract (first l))]
    [else(string-append  (extract (first l)) "," (if (list?(extract-var (rest l)))
                                                     (first (extract-var (rest l)))
                                                     (extract-var (rest l))))]))
;;returns v after converting it into string
(define (extract v)
  (if (varexp? v)
      (symbol->string (varexp-name v))
      (symbol->string v)))
;;creates no of spaces as given by num
(define (spaces num)
  (cond
    [(= num 0) ""]
    [else
     (build-string num (lambda (i) (integer->char ( + 32))))]))

(define (insert t)
  " ")



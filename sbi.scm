#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;;Amit Khatri (akhatri@ucsc.edu)
;;ID: 1398993
;;
;;Marcos Chabolla (mchaboll@ucsc.edu)
;;ID: 1437530
;;
;;
;;
;;
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define *function-table* (make-hash)) ;Function hash table
(define *label-table* (make-hash))    ;Label hash table
(define *variable-table*(make-hash))  ;Variable hash table
(define *addr-table*(make-hash))      ;Hold line number

(define (symbol-get key)
  (hash-ref *function-table* key))
(define (symbol-put! key value)
  (hash-set! *function-table* key value))

(for-each
   ( lambda (item) (hash-set! *variable-table* (car item) (cadr item)))
    `(
         (pi      3.1415)
         (e       2.7182)
     )
)

(for-each
    (lambda (item) (hash-set! *function-table* (car item) (cadr item)))
    `(
         (log10_2 0.301029995663981195213738894724493026768189881)
         (sqrt_2  1.414213562373095048801688724209698078569671875)
         (e       2.718281828459045235360287471352662497757247093)
         (pi      3.141592653589793238462643383279502884197169399)
         (/ ,/) (abs ,abs)
         (<= ,<=) (>= , >=) (= ,=) (> ,>) (< , <) (sin ,sin) (cos ,cos) 
         (tan ,tan) (atan , atan) (ceil ,ceiling) 
         (exp ,exp) (floor ,floor)
         (^ ,expt)
         (asin ,asin) (acos ,acos) (round ,round) 
         (log ,log) (sqrt ,sqrt)
         (div     ,(lambda (x y) (floor (/ x y))))
         (log10   ,(lambda (x) (/ (log x) (log 10.0))))
         (mod     ,(lambda (x y) (- x (* (div x y) y))))
         (quot    ,(lambda (x y) (truncate (/ x y))))
         (rem     ,(lambda (x y) (- x (* (quot x y) y))))
         (<>      ,(lambda (x y) (not (= x y))))
         (+ ,(lambda (x y) (+ x y)))
         (- ,(lambda (x y) (- x y)))
         (* ,(lambda (x y) (* x y)))
     )
)


(define (evaluate input) ; Evaluates given expressions
  (cond
    ((string? input) input)
    ((number? input) input)
    ((hash-has-key? *function-table* input)
      (hash-ref *function-table* input))
    ((list? input)
      (if (hash-has-key? *function-table* (car input))
        (let((first (hash-ref *function-table*  (car input))))
          (cond 
            ((procedure? first)
             (apply first (map (lambda (x) (evaluate x)) (cdr input))))
            ((vector? first)
             (vector-ref first (cadr input)))
            ((number? first) first)
            (else
              (die "Issue with function-table!."))))
        (die (list "Error: " 
                   (car input) " does not exist!\n")))))
)


(define (exec-line inst program lineNum)

    (when (eq? (car inst) 'print) ;print case 
         (printFunc (cdr inst))
         (parse-prog-list program(+ lineNum 1)))

    (when (eq? (car inst) 'goto) ;goto case
     (parse-prog-list program (hash-ref *addr-table* (cadr inst))))
   
    (when (eq? (car inst) 'let) ;let case
       (letFunc inst)
       (parse-prog-list program(+ lineNum 1)))

    (when (eq? (car inst) 'if) ;if case
        (when (evaluate  (cadr inst))
            (parse-prog-list  program 
            (hash-ref *addr-table* (caddr inst)))
            ))

    (when (eq? (car inst) 'dim) ;dim case 
       (dimFunc inst)
       (parse-prog-list program(+ lineNum 1)))

    (when (eq? (car inst) 'input) ;input case (not working)
    ((inputFunc inst)
       (parse-prog-list program(+ lineNum 1)) 
    ))  
)
        
(define (printFunc input) ;print function
 (map (lambda (x) (display (evaluate x))) input)
  (newline)
)

(define (dimFunc input) ; makes an array with a given size
  (let((arr (make-vector (evaluate (cadadr input)) (caadr input))))
  (symbol-put! (caadr input) (+ (evaluate (cadadr input)) 1))
))


(define (inputFunc input) ;input attempt
  (symbol-put! 'count 0)
  (if (null? (car input))
    (symbol-put! 'count -1)
    (begin
    (symbol-put! 'count (inputFunc input 0)))))


(define (letFunc input)
  (symbol-put! (cadr input) (evaluate (caddr input)))
)

(define (populate-labels program) ; populates the label table
    (map (lambda (line)
        (when (not (null? line))
            (when (or (= 3 (length line))
                (and (= 2 (length line))
                    (not (list? (cadr line)))))
          (hash-set! *label-table* (cadr line ) (cddr line))
          (hash-set! *addr-table* (cadr line) (- (car line) 1))
          ))) program)
 )
  
(define (parse-prog-list program lineNum) ; Parses the program.
   (when (> (length program) lineNum)
    (let((line (list-ref program lineNum)))
    (cond
      ((= (length line) 3)
       (set! line (cddr line))
       (exec-line (car line) program lineNum))
      ((and (= (length line) 2) (list? (cadr line)))
       (set! line (cdr line))
       (exec-line (car line) program lineNum))
      (else 
        (parse-prog-list program (+ lineNum 1)))
    )))
)

(define length
   (lambda (ls)
     (if (null? ls) 0
         (+ (length (cdr ls)) 1)))
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
              (program (readlist-from-inputfile sbprogfile)))
              (populate-labels program)
              (parse-prog-list program 0)
        )
    )
)

(main (vector->list (current-command-line-arguments)))

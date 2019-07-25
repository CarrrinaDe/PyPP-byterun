#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack null)

(define (make-stack) empty-stack)

(define (push element stack)
  (cons element stack))

(define (top stack)
  (if (null? stack)
      null
      (car stack)))

(define (pop stack)
  (if (null? stack)
      null
      (cdr stack)))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (hash 0 stack 1 co-varnames 2 co-consts 3 co-names 4 co-code 5 IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (hash-ref stack-machine 1))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (hash-ref stack-machine 2))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (hash-ref stack-machine 3))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (hash-ref stack-machine 4))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (hash-ref stack-machine 0))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (hash-ref stack-machine 5))


(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (let get ((L symbols) (index 0))
    (if (equal? symbol (car L))
        index
        (get (cdr L) (add1 index)))))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine symbol item stack-machine)
  (hash-set stack-machine (get-symbol-index symbol) item))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine 'STACK (push value (get-stack stack-machine)) stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (update-stack-machine 'STACK (pop (get-stack stack-machine)) stack-machine))

(define (next-instr stack-machine)
  (update-stack-machine 'INSTRUCTION-COUNTER (add1 (get-IC stack-machine)) stack-machine))

(define (jump-to-instr stack-machine index)
  (update-stack-machine 'INSTRUCTION-COUNTER index stack-machine))

(define current car)
(define next cdr)

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (let ((instr-name (car (list-ref (get-code stack-machine) (get-IC stack-machine)))) (arg (cdr (list-ref (get-code stack-machine) (get-IC stack-machine)))))
   (cond
    ((equal? 'RETURN_VALUE instr-name) stack-machine)
    ((equal? 'POP_TOP instr-name) (run-stack-machine (next-instr (pop-exec-stack stack-machine))))
    ((equal? 'LOAD_CONST instr-name) (run-stack-machine (next-instr (push-exec-stack (hash-ref (get-consts stack-machine) arg) stack-machine))))
    ((equal? 'LOAD_GLOBAL instr-name) (run-stack-machine (next-instr (push-exec-stack (hash-ref (get-names stack-machine) arg) stack-machine))))
    ((equal? 'STORE_FAST instr-name) (run-stack-machine (next-instr (update-stack-machine 'CO-VARNAMES
                                                                      (hash-set (get-varnames stack-machine) arg (top (get-stack stack-machine))) (pop-exec-stack stack-machine)))))
    ((equal? 'LOAD_FAST instr-name) (run-stack-machine (next-instr (push-exec-stack (hash-ref (get-varnames stack-machine) arg) stack-machine))))
    ((equal? 'BINARY_MODULO instr-name) (run-stack-machine (next-instr (let ((TOS (top (get-stack stack-machine))) (TOS1 (top (pop (get-stack stack-machine)))))
                                                                          (push-exec-stack (modulo TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))))
    ((equal? 'BINARY_ADD instr-name) (run-stack-machine (next-instr (let ((TOS (top (get-stack stack-machine))) (TOS1 (top (pop (get-stack stack-machine)))))
                                                                          (push-exec-stack (+ TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))))
    ((equal? 'BINARY_SUBTRACT instr-name) (run-stack-machine (next-instr (let ((TOS (top (get-stack stack-machine))) (TOS1 (top (pop (get-stack stack-machine)))))
                                                                          (push-exec-stack (- TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))))
    ((equal? 'INPLACE_ADD instr-name) (run-stack-machine (next-instr (let ((TOS (top (get-stack stack-machine))) (TOS1 (top (pop (get-stack stack-machine)))))
                                                                          (push-exec-stack (+ TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))))
    ((equal? 'INPLACE_SUBTRACT instr-name) (run-stack-machine (next-instr (let ((TOS (top (get-stack stack-machine))) (TOS1 (top (pop (get-stack stack-machine)))))
                                                                          (push-exec-stack (- TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))))
    ((equal? 'INPLACE_MODULO instr-name) (run-stack-machine (next-instr (let ((TOS (top (get-stack stack-machine))) (TOS1 (top (pop (get-stack stack-machine)))))
                                                                          (push-exec-stack (modulo TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))))
    ((equal? 'JUMP_ABSOLUTE instr-name) (run-stack-machine (jump-to-instr stack-machine (quotient arg 2))))
    ((equal? 'COMPARE_OP instr-name) (run-stack-machine (next-instr (let ((TOS (top (get-stack stack-machine))) (TOS1 (top (pop (get-stack stack-machine)))))
                                                                          (push-exec-stack ((get-cmpop arg) TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))))
    ((equal? 'POP_JUMP_IF_FALSE instr-name)
     (if (not (top (get-stack stack-machine)))
         (run-stack-machine (jump-to-instr (pop-exec-stack stack-machine) (quotient arg 2)))
         (run-stack-machine (next-instr (pop-exec-stack stack-machine)))))
    ((equal? 'POP_JUMP_IF_TRUE instr-name)
     (if (top (get-stack stack-machine))
         (run-stack-machine (jump-to-instr (pop-exec-stack stack-machine) (quotient arg 2)))
         (run-stack-machine (next-instr (pop-exec-stack stack-machine)))))
    ((equal? 'GET_ITER instr-name) (run-stack-machine (next-instr stack-machine)))
    ((equal? 'FOR_ITER instr-name) (let ((ITERATOR (top (get-stack stack-machine))))
                                     (if (null? ITERATOR)
                                         (run-stack-machine (jump-to-instr (pop-exec-stack stack-machine) (+ (quotient (+ arg 2) 2) (get-IC stack-machine))))
                                         (run-stack-machine (next-instr (push-exec-stack (current ITERATOR) (push-exec-stack (next ITERATOR) (pop-exec-stack stack-machine))))))))
    ((equal? 'CALL_FUNCTION instr-name) (run-stack-machine (next-instr (let take-args ((argc arg) (argv null) (stack-machine stack-machine))
                                                                         (if (zero? argc)
                                                                             (push-exec-stack ((get-function (top (get-stack stack-machine))) argv) (pop-exec-stack stack-machine))
                                                                             (take-args (sub1 argc) (cons (top (get-stack stack-machine)) argv) (pop-exec-stack stack-machine)))))))
    (else (run-stack-machine (next-instr stack-machine)))))) ;; ignore SETUP_LOOP and POP_BLOCK

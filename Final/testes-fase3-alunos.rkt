#lang scheme
(require (lib "misc.ss" "swindle")
         (lib "list.ss")
         (lib "lset.ss" "srfi" "1"))

;;=======================================
;; 1) Colocar este ficheiro na mesma directoria que o ficheiro com o código do projecto;
;; 2) Colocar o ficheiro "solucao-parte2.rkt" na mesma directoria que o ficheiro com o código do projecto;
;; 3) Definir número do grupo a seguir;
;; 4) Executar a acção "Correr".
;;=======================================
(define *numero-do-grupo* "14")

;; TAI teste
(define (cria-teste nome forma erro?)
  (cons erro? (cons nome forma)))

;; Nome do teste
(define (nome-teste teste)
  (cadr teste))

;; Código a ser avaliado
(define (forma-teste teste)
  (cddr teste))

;; Indica se a avaliação do código deve gerar um erro de execução
(define (erro?-teste teste)
  (car teste))


;; Definição dos testes
(define (cria-testes)
  (list 
   ;; TAI info-no 
   (cria-teste "Teste info-no 01"
               '(novo-info-no 'A)
               #f)
   (cria-teste "Teste info-no 02"
               '(equal? (no-info-no (novo-info-no 'A)) 'A)
               #f)
   (cria-teste "Teste info-no 03"
               '(= (dist-info-no (novo-info-no 'A)) +inf.0)
               #f)
   (cria-teste "Teste info-no 04"
               '(null? (ant-info-no (novo-info-no 'A)))
               #f)
   (cria-teste "Teste info-no 05"
               '(let ((ifno (novo-info-no 'A)))
                  (modifica-dist! ifno 99)
                  (= (dist-info-no ifno) 99))
               #f)
   (cria-teste "Teste info-no 06"
               '(let ((ifno (novo-info-no 'A)))
                  (modifica-ant! ifno 'B)
                  (equal? (ant-info-no ifno) 'B))
               #f)
   ;; TAI info-nos
   (cria-teste "Teste info-nos 01"
               '(cria-info-inicial (faz-conj 'A 'B 'C))
               #f)
   (cria-teste "Teste info-nos 02"
               '(and (procedure? cria-info-inicial)
                     (cria-info-inicial 'A))
               #t)
   (cria-teste "Teste info-nos 03"
               '(let ((ifnos (cria-info-inicial (faz-conj 'A 'B 'C))))
                  (and (equal? (no-info-no (obtem-info-no ifnos 'A)) 'A)
                       (equal? (no-info-no (obtem-info-no ifnos 'B)) 'B)
                       (equal? (no-info-no (obtem-info-no ifnos 'C)) 'C)
                       (= (dist-info-no (obtem-info-no ifnos 'A)) +inf.0)
                       (= (dist-info-no (obtem-info-no ifnos 'B)) +inf.0)
                       (= (dist-info-no (obtem-info-no ifnos 'C)) +inf.0)
                       (null? (ant-info-no (obtem-info-no ifnos 'A)))
                       (null? (ant-info-no (obtem-info-no ifnos 'B)))
                       (null? (ant-info-no (obtem-info-no ifnos 'C)))))
               #f)
   (cria-teste "Teste info-nos 04"
               '(let* ((nos (faz-conj 'A 'B 'C))
                       (ifnos (cria-info-inicial nos)))
                  (modifica-dist! (obtem-info-no ifnos 'A) 5)
                  (modifica-ant! (obtem-info-no ifnos 'A) 'B)
                  (modifica-dist! (obtem-info-no ifnos 'B) 10)
                  (modifica-ant! (obtem-info-no ifnos 'B) 'C)
                  (modifica-dist! (obtem-info-no ifnos 'C) 15)
                  (modifica-ant! (obtem-info-no ifnos 'C) 'A)
                  (equal? 'A (no<dist ifnos nos)))
               #f)
   (cria-teste "Teste info-nos 05"
               '(let* ((nos (faz-conj 'A 'B 'C))
                       (ifnos (cria-info-inicial nos)))
                  (modifica-dist! (obtem-info-no ifnos 'A) 5)
                  (modifica-ant! (obtem-info-no ifnos 'A) 'B)
                  (modifica-dist! (obtem-info-no ifnos 'B) 10)
                  (modifica-ant! (obtem-info-no ifnos 'B) 'C)
                  (modifica-dist! (obtem-info-no ifnos 'C) 15)
                  (modifica-ant! (obtem-info-no ifnos 'C) 'A)
                  (equal? 'B (no<dist ifnos (faz-conj 'B 'C))))
               #f)
   (cria-teste "Teste info-nos 06"
               '(let ((ifnos (cria-info-inicial (faz-conj 'A 'B 'C))))
                  (modifica-dist! (obtem-info-no ifnos 'A) 5)
                  (modifica-ant! (obtem-info-no ifnos 'A) 'B)
                  (modifica-dist! (obtem-info-no ifnos 'B) 10)
                  (modifica-ant! (obtem-info-no ifnos 'B) 'C)
                  (modifica-dist! (obtem-info-no ifnos 'C) 15)
                  (modifica-ant! (obtem-info-no ifnos 'C) 'A)
                  (actualiza-dist-ant-no! ifnos 'B 12 'A)
                  (and (= (dist-info-no (obtem-info-no ifnos 'B)) 12)
                       (equal? (ant-info-no (obtem-info-no ifnos 'B)) 'A)))
               #f)
   (cria-teste "Teste info-nos 07"
               '(let ((ifnos (cria-info-inicial (faz-conj 'A 'B 'C))))
                  (modifica-dist! (obtem-info-no ifnos 'A) 5)
                  (modifica-ant! (obtem-info-no ifnos 'A) 'B)
                  (modifica-dist! (obtem-info-no ifnos 'B) 10)
                  (modifica-ant! (obtem-info-no ifnos 'B) 'C)
                  (modifica-dist! (obtem-info-no ifnos 'C) 15)
                  (modifica-ant! (obtem-info-no ifnos 'C) 'A)
                  (actualiza-dist-ant-no! ifnos 'B 12 null)
                  (and (= (dist-info-no (obtem-info-no ifnos 'B)) 12)
                       (null? (ant-info-no (obtem-info-no ifnos 'B)))))
               #f)
   ;; Procedimento caminho-mais-curto 
   (cria-teste "Teste caminho-mais-curto 01"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos)))
                  (equal? '((A 1 B) . 1)
                          (caminho-mais-curto 'A 'B grafo)))
               #f)
   (cria-teste "Teste caminho-mais-curto 02"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos)))
                  (equal? '((A 2 C 1 D) . 3)
                          (caminho-mais-curto 'A 'D grafo)))
               #f)
   (cria-teste "Teste caminho-mais-curto 03"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos)))
                  (equal? '((A 2 C 3 E) . 5)
                          (caminho-mais-curto 'A 'E grafo)))
               #f)
   (cria-teste "Teste caminho-mais-curto 04"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos)))
                  (equal? '((E 3 C 2 A) . 5)
                          (caminho-mais-curto 'E 'A grafo)))
               #f)
   (cria-teste "Teste caminho-mais-curto 05"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos)))
                  (equal? '(() . +inf.0)
                          (caminho-mais-curto 'A 'F grafo)))
               #f)

   ;; TAI cria-proc-caminhos-mais-curtos
   (cria-teste "Teste cria-proc-caminhos-mais-curtos 01"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos)))
                  (procedure? (cria-proc-caminhos-mais-curtos grafo)))
               #f)
  (cria-teste "Teste cria-proc-caminhos-mais-curtos 02"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos))
                       (g1 (cria-proc-caminhos-mais-curtos grafo)))
                  (equal? '((A 1 B) . 1)
                          (g1 'A 'B)))
               #f)
   (cria-teste "Teste cria-proc-caminhos-mais-curtos 03"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos))
                       (g1 (cria-proc-caminhos-mais-curtos grafo)))
                  (equal? '((A 2 C 1 D) . 3)
                          (caminho-mais-curto 'A 'D grafo)))
               #f)
   (cria-teste "Teste cria-proc-caminhos-mais-curtos 04"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos))
                       (g1 (cria-proc-caminhos-mais-curtos grafo)))
                  (equal? '((A 2 C 3 E) . 5)
                          (caminho-mais-curto 'A 'E grafo)))
               #f)
   (cria-teste "Teste cria-proc-caminhos-mais-curtos 05"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos))
                       (g1 (cria-proc-caminhos-mais-curtos grafo)))
                  (equal? '((E 3 C 2 A) . 5)
                          (caminho-mais-curto 'E 'A grafo)))
               #f)
   (cria-teste "Teste cria-proc-caminhos-mais-curtos 06"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos))
                       (g1 (cria-proc-caminhos-mais-curtos grafo)))
                  (equal? '(() . +inf.0)
                          (caminho-mais-curto 'A 'F grafo)))
               #f)
   (cria-teste "Teste cria-proc-caminhos-mais-curtos 07"
               '(let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                       (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 2) (faz-ramo 'B 'D 3)
                                             (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                       (grafo (faz-grafo conj-nos conj-ramos))
                       (g1 (cria-proc-caminhos-mais-curtos grafo)))
                  (let* ((conj-nos (faz-conj 'A 'B 'C 'D 'E 'F))
                         (conj-ramos (faz-conj (faz-ramo 'A 'B 1) (faz-ramo 'A 'C 4) (faz-ramo 'B 'D 3)
                                               (faz-ramo 'C 'D 1)(faz-ramo 'C 'E 3)(faz-ramo 'D 'E 5)))
                         (grafo (faz-grafo conj-nos conj-ramos))
                         (g2 (cria-proc-caminhos-mais-curtos grafo)))
                    (and (equal? '((A 1 B 3 D) . 4)
                                 (g2 'A 'D))
                         (equal? '((A 2 C 1 D) . 3) 
                                 (g1 'A 'D)))))
               #f)

   ))

(define (testa testes)
  (dolist (teste testes)
          (let ((res (no-errors* (eval (forma-teste teste)))))
            (newline)
            (display (nome-teste teste))
            (cond ((struct? res) (if (erro?-teste teste)
                                     (display " - Passou.")
                                     (display " - FALHOU.")))
                  ((erro?-teste teste) (display " - FALHOU."))
                  (res (display " - Passou."))
                  (else (display " - FALHOU."))))))



(parameterize ([current-namespace (make-base-empty-namespace)])
  (namespace-require 'scheme)
  (namespace-require (string-append "fp1112-parte3-grupo" *numero-do-grupo* ".rkt"))
  (eval '(require (lib "misc.ss" "swindle")
                  (lib "list.ss")
                  (lib "lset.ss" "srfi" "1")
                  (lib "trace.ss")
                  "solucao-parte2.rkt"
                  ))
  
 (testa (cria-testes)))

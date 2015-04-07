#lang scheme

(provide (all-defined-out))
(require (lib "trace.ss"))
;         (lib "misc.ss" "swindle"))

;;;;;;;;;;;;;;;TESTE DE COMPARACAO DE ELEMENTOS DE LISTAS E CONJUNTOS;;;;;;
;;;;;;;;no contexto presente os elementos de todos os tipos,;;;;;;;;;;;;;;;
;;;;;;;;excepto os do tipo ramo, podem ser comparados com equal?;;;;;;;;;;;
;;;;;;;;os elementos do tipo ramo devem ser comparados com ramos=?;;;;;;;;;
;;;elem=?: universal x universal -> logico
;;;elem=?(e1,e2) devolve ramos=?(e1,e2), se e1 e e2 forem ramos;
;;;              em caso contrario devolve equal?(e1,e2)
(define (elem=? e1 e2)
  (if (and (ramo? e1) (ramo? e2))
      (ramos=? e1 e2)
      (equal? e1 e2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI LISTA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; REP.INTERNA: Uma lista vazia e' represntada por null.     ;;;;;;;;
;;;;;;;; Uma lista nao vazia e' representada por um par, em        ;;;;;;;;
;;;;;;;; que o primeiro elemento e' o primeiro elemento da lista,  ;;;;;;;;
;;;;;;;; e o segundo elemento do par e' o resto da lista.          ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; OPERACOES BASICAS

;;; Construtores
;;; 
;;; nova-lista : {} -> lista 
;;; nova-lista() tem como valor uma lista vazia. 
(define (nova-lista) 
  '())

;;; insere : universal x lista -> lista 
;;; insere(elem, lst) tem como valor a lista que resulta de inserir o 
;;; elemento elem na primeira posicao da lista lst. 
(define (insere elem lst) 
  (cons elem lst)) 

;;; Selectores
;;; 
;;; primeiro : lista -> universal 
;;; primeiro(lst) tem como valor o elemento que se encontra na primeira
;;; posicao da lista lst. Se a lista nao tiver elementos, o valor desta
;;; operacao e' indefinido. 
(define (primeiro lst) 
  (if (null? lst) 
      (error "primeiro: a lista nao tem elementos") 
      (car lst)))

;;; resto : lista -> lista 
;;; resto(lst) tem como valor a lista que resulta de remover o primeiro
;;; elemento da lista lst. Se a lista nao tiver elementos, o valor 
;;; desta operacao  e' indefinido.
(define (resto lst) 
  (if (null? lst) 
      (error "resto: a lista nao tem elementos") 
      (cdr lst)))

;;; Reconhecedores
;;; 
;;; lista? : universal -> logico 
;;; lista?(arg) tem o valor verdadeiro se arg  e' uma lista e tem o valor
;;; falso em caso contrario. 
(define (lista? x) 
  (cond ((null? x) #t) 
        ((pair? x) (lista? (cdr x))) 
        (else #f))) 

;;; lista-vazia? : lista -> logico 
;;; lista-vazia?(lst) tem o valor verdadeiro se a lista lst  e' a lista 
;;; vazia e tem o valor falso em caso contrario. 
(define (lista-vazia? lst) 
  (null? lst))

;;; Testes
;;; 
;;; listas=? : lista x lista -> logico 
;;; listas=?(lst1, lst2) tem o valor verdadeiro se a lista lst1 e'
;;; igual a lista lst2, comparando os seus elementos com elem=?, e tem o 
;;; valor falso em caso contrario. 
(define (listas=? lst1 lst2)
  (cond ((null? lst1) (null? lst2))
        ((null? lst2) #f)
        ((elem=? (car lst1) (car lst2)) 
         (listas=? (cdr lst1) (cdr lst2)))
        (else #f)))

;;;; OPERACOES ALTO NIVEL

;;; comprimento : lista -> inteiro
;;; comprimento(lst) tem como valor o inteiro que corresponde ao numero
;;; de elementos da lista lst.
(define (comprimento lst)
  (if (lista-vazia? lst)
      0
      (+ 1 (comprimento (resto lst)))))

;;; todos-elementos-lista-satisfazem? : lista x predicado -> lógico
;;; todos-elementos-lista-satisfazem?(lst,p) tem  valor verdadeiro se  
;;; todos os elementos da lista lst satisfazem o predicado p, e tem o 
;;; valor falso em caso contrario.
(define (todos-elementos-lista-satisfazem? lst teste)
  (cond ((lista-vazia? lst) #t)
        ((teste (primeiro lst))
         (todos-elementos-lista-satisfazem? (resto lst) teste))
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI CONJUNTO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; REP.INTERNA: Lista com os elementos dos conjunto              ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; OPERACOES BASICAS

;;; Construtores
;;; 
;;; novo-conj : {} -> conjunto 
;;; novo-conj() tem como valor o conjunto vazio. 
(define (novo-conj) (nova-lista))

;;; insere-conj : universal x conjunto -> conjunto 
;;; insere-conj(elem, c) tem como valor o conjunto que resulta de inserir o 
;;; elemento elem no conjunto c; se elem ja pertencer a c tem como valor c. 
(define (insere-conj e c)
  (define (insere-conj-aux e c)
    (cond ((lista-vazia? c) (insere e c))
          ((elem=? e (primeiro c)) c)
          (else (insere (primeiro c) 
                        (insere-conj e (resto c))))))
  (if (conjunto? c)
      (insere-conj-aux e c)
      (error "insere-conj: o segundo argumento tem de ser um conjunto.")))

;;; faz-conj : universal^n -> conjunto 
;;; faz-conj(elem1,..., elemn) tem como valor o conjunto de elementos elem1,..., elemn;
;;; a ordem é irrelevante; o resultado nao contem elementos repetidos.
(define (faz-conj . elementos)

  (define (faz-conj-aux  elementos)
    (if (lista-vazia? elementos) 
        (novo-conj)
        (insere-conj (primeiro elementos) 
                     (faz-conj-aux (resto elementos)))))
                
  (faz-conj-aux elementos))

;;; Selector
;;; 
;;; retira-conj : universal x conjunto -> conjunto 
;;; retira-conj(elem, c) tem como valor o conjunto que resulta de retirar o 
;;; elemento elem do conjunto c; se elem nao pertencer a c tem como valor c. 
(define (retira-conj e c)
  (cond ((lista-vazia? c) (nova-lista))
        ((elem=? e (primeiro c)) (resto c))
        (else (insere (primeiro c) 
                      (retira-conj e (resto c))))))

;;; Reconhecedores
;;; 
;;; conjunto? : universal -> logico 
;;; conjunto?(arg) tem o valor verdadeiro se arg  e' um conjunto e tem o valor
;;; falso em caso contrario. 
(define (conjunto? x)
  (define (nao-tem-elementos-repetidos? x)
    (cond ((lista-vazia? x) #t)
          ((pertence? (primeiro x) (resto x)) #f)
          (else (nao-tem-elementos-repetidos? 
                  (resto x)))))
  (and (lista? x) 
       (nao-tem-elementos-repetidos? x)))

;;; conj-vazio? : conjunto -> logico 
;;; conj-vazio?(c) tem o valor verdadeiro se o conjunto c  e' o conjunto
;;; vazio e tem o valor falso em caso contrario. 
(define (conj-vazio? c)
  (lista-vazia? c))

;;; Testes
;;; 
;;; conjuntos=? : conjunto x conjunto -> logico 
;;; conjuntos=?(c1, c2) tem o valor verdadeiro se o conjunto c1 e'
;;; igual ao conjunto c2, e tem o 
;;; valor falso em caso contrario. 
(define (conjuntos=? c1 c2)
  (cond ((lista-vazia? c1) (lista-vazia? c2))
        ((lista-vazia? c2) #f)
        ((pertence? (primeiro c1)  c2)
         (conjuntos=? (resto c1) 
                      (retira-conj (primeiro c1) c2)))
        (else #f)))

;;; pertence? : universal x conjunto -> logico 
;;; pertence?(elem, c) tem o valor verdadeiro se o elemento elem pertence 
;;; ao conjunto c, e tem o valor falso em caso contrario;
;;; os elementos sao comparados com elem=?.
(define (pertence? e c)
  (cond ((lista-vazia? c) #f)
        ((elem=? e (primeiro c)) #t)
        (else (pertence? e (resto c)))))

;;; todos-elementos-conjunto-satisfazem? : conjunto x predicado -> lógico
;;; todos-elementos-conjunto-satisfazem?(c,p) tem  valor verdadeiro se  
;;; todos os elementos do conjunto c satisfizerem o predicado p, e tem o 
;;; valor falso em caso contrario.
(define (todos-elementos-conjunto-satisfazem? c teste)
  (todos-elementos-lista-satisfazem? c teste))

;;; Transformador
;;; 
;;; conj->lista : conjunto -> lista 
;;; conj->lista(c) tem como valor uma lista com os elementos do conjunto c;
;;; a ordem dos elementos da lista nao e' relevante. 
(define (conj->lista conj)
  conj)

;;; Operacao adicional
;;; 
;;; interseccao : conjunto x conjunto -> conjunto 
;;; interseccao(c1, c2) tem como valor o conjunto interseccao
;;; dos conjuntos c1 e c2. 
(define (interseccao c1 c2)
  (cond ((lista-vazia? c1) (nova-lista))
        ((pertence? (primeiro c1) c2)
         (insere (primeiro c1)
                 (interseccao (resto c1) c2)))
        (else (interseccao (resto c1) c2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI RAMO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; REP.INTERNA: Par cujo primeiro elemento e' o conjunto     ;;;;;;;;
;;;;;;;; cujos elementos sao os extremos do ramo, e cujo segundo   ;;;;;;;;
;;;;;;;; elemento e' o rotulo do ramo.                             ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; OPERACOES BASICAS

;;; Construtor
;;; 
;;; faz-ramo : no x no x inteiro -> ramo 
;;; faz-ramo(n1, n2, r) tem como valor o ramo de extremos n1 e n2, 
;;; e rotulo r.
(define (faz-ramo n1 n2 rot)
  (if (integer? rot)
      (cons (faz-conj n1 n2) rot)
      (error "faz-ramo: o terceiro argumento tem de ser um inteiro")))

;;; Selectores
;;; 
;;; extremos-ramo : ramo -> conjunto de nos
;;; extremos-ramo(r) tem como valor o conjunto cujos elementos
;;; sao os extremos do ramo r.
(define (extremos-ramo r)
  (car r))

;;; rotulo-ramo : ramo -> inteiro
;;; rotulo-ramo(r) tem como valor o rotulo do ramo r. 
(define (rotulo-ramo r)
  (cdr r))

;;; outro-no-ramo : no x ramo -> no
;;; outro-no-ramo(n, r), sendo n um dos extremos do ramo r, tem como valor  
;;; o outro extremo do ramo r.
(define (outro-no-ramo n r)
  (primeiro (conj->lista 
             (retira-conj n (extremos-ramo r)))))

;;; Reconhecedor
;;; 
;;; ramo? : universal -> logico 
;;; ramo?(arg) tem o valor verdadeiro se arg  e' um ramo e tem o valor
;;; falso em caso contrario. 
(define (ramo? x)
  (and (pair? x)
       (conjunto? (car x))
       (integer? (cdr x))))

;;; Testes
;;; 
;;; ramos=? : ramo x ramo -> logico 
;;; ramos=?(r1, r2) tem o valor verdadeiro se o ramo r1 e'
;;; igual ao ramo r2, e tem o valor falso em caso contrario. 
(define (ramos=? r1 r2)
  (and (= (rotulo-ramo r1) (rotulo-ramo r2))
       (conjuntos=? (extremos-ramo r1) (extremos-ramo r2))))

;;; extremo-ramo? : no x ramo -> logico 
;;; extremo-ramo?(n, r) tem o valor verdadeiro se n e'
;;; um dos extremos do ramo r, e tem o valor falso em caso contrario. 
(define (extremo-ramo? n r)
  (pertence? n (extremos-ramo r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI GRAFO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; REP.INTERNA: Par cujo primeiro elemento e' o conjunto de nos     ;;;;;;;;
;;;;;;;; do grafo, e cujo segundo elemento e' o conjunto de ramos do grafo.  ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; OPERACOES BASICAS

;;; Construtor
;;; 
;;; faz-grafo : conjunto de nos x conjunto de ramos -> grafo 
;;; faz-grafo(cn,cr) tem como valor o grafo cujo conjunto 
;;; de nos e' cn, e cujo conjunto de ramos e' cr.

(define (faz-grafo conj-nos conj-ramos)
  (if (and (conjunto? conj-nos) 
           (conjunto? conj-ramos) 
           (todos-elementos-conjunto-satisfazem? conj-ramos ramo?))
      (cons conj-nos conj-ramos)
      (error "faz-grafo: o 1o argumento tem de ser um conjunto de nós, e o 2o um conjunto de ramos")))


;;; Selectores
;;; 
;;; nos : grafo -> conjunto de nos
;;; nos(g) tem como valor o conjunto de nos do grafo g. 
(define (nos G)
  (car G))

;;; ramos : grafo -> conjunto de ramos
;;; ramos(g) tem como valor o conjunto de ramos do grafo g. 
(define (ramos G)
  (cdr G))

;;; vizinhos : no x grafo -> conjunto de nos
;;; vizinhos(n, g) tem como valor o conjunto de nos do grafo g
;;; ligados ao no n por um ramo.
(define (vizinhos n G)
  (define (vizinhos-aux n lst-ramos)
    (cond ((lista-vazia? lst-ramos) (novo-conj))
          ((pertence? n (extremos-ramo (primeiro lst-ramos)))
           (insere-conj (outro-no-ramo n (primeiro lst-ramos))
                        (vizinhos-aux n (resto lst-ramos))))
          (else (vizinhos-aux n (resto lst-ramos)))))
  
  (vizinhos-aux n (conj->lista (ramos G))))

;;; distancia-entre : no x no x grafo -> inteiro
;;; distancia-entre(n1, n2, g) tem como valor o rotulo do ramo 
;;; de extremos n1 2 n2, se este ramo existir; 
;;; em caso contrario tem valor indefinido.
(define (distancia-entre n1 n2 G)
  
  (define (distancia-entre-aux n1 n2 lst-ramos)
    (cond ((lista-vazia? lst-ramos)
           (error "distancia-entre: não existe um ramo entre os nós dados."))
          ((conjuntos=? (extremos-ramo (primeiro lst-ramos))
                        (faz-conj n1 n2))
           (rotulo-ramo (primeiro lst-ramos)))
          (else (distancia-entre-aux n1 n2 (resto lst-ramos)))))
  
  (distancia-entre-aux n1 n2 (conj->lista (ramos G))))

;;; Reconhecedor
;;; 
;;; grafo? : universal -> logico 
;;; grafo?(arg) tem o valor verdadeiro se arg e' um grafo e tem o valor
;;; falso em caso contrario. 
(define (grafo? u)
  (and (pair? u)
       (conjunto? (car u))
       (conjunto? (cdr u))
       (todos-elementos-conjunto-satisfazem? (cdr u) ramo?)))

;;; Teste
;;; 
;;; grafos=? : grafo x grafo -> logico 
;;; grafos=?(g1, g2) tem o valor verdadeiro se o grafo g1 e'
;;; igual ao grafo g2, e tem o valor falso em caso contrario. 
(define (grafos=? g1 g2)
  (and (conjuntos=? (nos g1) (nos g2))
       (conjuntos=? (ramos g1) (ramos g2))))




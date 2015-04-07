#lang scheme 
(provide (all-defined-out)) 
(require "solucao-parte2.rkt") 
(require (lib "misc.ss" "swindle"))

; Grupo 14: 
; 72913 - Bruno Henriques 
; 73138 - Anto'nio Po'lvora 
; 73412 - Diogo Rato

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI INFO-NO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; REP.INTERNA: Um info-no e' representado por um vector em  ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; que o primeiro elemento e' um no' n ,o segundo elemento   ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; e' a distancia associada a n e o terceiro elemento e' o   ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;:; no' anterior associado igualmente a n                     ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ================================================================================================
;                                       Construtores:
; ================================================================================================
; novo-info-no: no'-> info-no' 
; novo-info-no(n): Devolve o elemento do tipo info-no' cujo no' e' n, cuja distancia e' infinito,
; e cujo no' anterior e' "indefinido" 
; ------------------------------------------------------------------------------------------------

(define (novo-info-no n) 
  (vector n +inf.0 null))

; ================================================================================================
;                                       Selectores: 
; ================================================================================================
; no-info-no: info-no' -> no' 
; no-info-no (i): Devolve o no' de i 
; ------------------------------------------------------------------------------------------------

(define (no-info-no i)  
  (vector-ref i 0))

; ------------------------------------------------------------------------------------------------
; dist-info-no: info-no' -> Inteiro 
; dist-info-no(i): Devolve a distancia de i 
; ------------------------------------------------------------------------------------------------

(define (dist-info-no i) 
  (vector-ref i 1))

; ------------------------------------------------------------------------------------------------
; ant-info-no: info-no' -> no'
; ant-info-no(i): Devolve o no'anterior de i   
; ------------------------------------------------------------------------------------------------

(define (ant-info-no i)
  (vector-ref i 2))

; ================================================================================================
;                                        Modificadores:
; ================================================================================================ 
; modifica-dist!: info-no' x inteiro -> info-no' 
; modifica-dist!(i,d) : Altera a distancia de i para d. Devolve i com distancia alterada 
; ------------------------------------------------------------------------------------------------

(define (modifica-dist! i d)
  (vector-set! i 1 d)  i)



; ------------------------------------------------------------------------------------------------
; modifica-ant!: info-no'x no' -> info-no' 
; modifica-ant!(i,n): Altera o no' anterior de i para n. Devolve i com o no' anterior
; alterado   
; ------------------------------------------------------------------------------------------------

(define (modifica-ant! i n)
  (vector-set! i 2 n)  i)

; ================================================================================================
;                                       Fim do tipo info-no' 
; ================================================================================================



; ================================================================================================ 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAI INFO-NO'S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;; REP.INTERNA: Um info-no's e' representado por um          ;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;; vector, em que cada elemento corresponde a um info-no'    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ================================================================================================
;                                       Construtores: 
; ================================================================================================
; cria-info-inicial: conjunto de no's-> info-no's 
; cria-info-inicial(c): devolve a coleccao de elementos do tipo info-no' obtidos por aplicacao de
; novo-info-no a cada no' do conjunto c 
; ------------------------------------------------------------------------------------------------

(define (cria-info-inicial c)
  (define (cria-info-inicial-aux c)
    (let* ((lista-nos (conj->lista c))
           (comp-lista-nos (comprimento lista-nos))
           (vector-nos (make-vector comp-lista-nos)))
      (dotimes (i comp-lista-nos)
               (vector-set! vector-nos i (novo-info-no (primeiro lista-nos)))
               (set! lista-nos (resto lista-nos)))
      ; o procedimento vai percorrer todo o vector criado inicialmente (vector-nos) e a cada       
      ; elemento do vector vai associar um info-no cujo no um elemento do conjunto de nos c.       
      vector-nos))
  (if (conjunto? c)
      (cria-info-inicial-aux c)
      (error "cria-info-inicial: O argumento tem de ser um conjunto de no's")))

; ================================================================================================
;                                       Selectores: 
; ================================================================================================
; obtem-info-no: info-no's x no' -> info-no' 
; obtem-info-no(i,n): devolve o elemento de i cujo no' e' n. Se o no' nao pertencer ao info-no's i
; entao devolver null 
; ------------------------------------------------------------------------------------------------

(define (obtem-info-no i n)
  (let((info-no-actual null)
       (tamanho (vector-length i))
       (j 0)
       (encontrado? #f))
    (while (and (not encontrado?)(< j tamanho))
           (if (equal? n (no-info-no (vector-ref i j)))
               (begin (set! info-no-actual (vector-ref i j))
                      (set! encontrado? #t))
               (set! j (+ j 1))))
    ; O uso do ciclo while deve-se ao facto de nao ser necessario percorrer todo os elementos do 
    ; info-nos. Assim que encontrado o no n, o ciclo para e devolve o info-no correspondente 
    ; (info-no-actual)
    info-no-actual))



; ------------------------------------------------------------------------------------------------
; no<dist: info-no's x conjunto de no's -> no' 
; no<dist(i,c): devolve o no' do conjunto c com a menor distância associada em i  
; ------------------------------------------------------------------------------------------------

(define (no<dist i c)
  (let ((comp-vector (vector-length i))
        (menor-dist +inf.0)
        (no-menor-dist null))
    ; inicializacao: menor-dist e inicializado a <+infinito>, pois quando for comparado, qualquer 
    ; nu'mero real sera' menor que este.
    (dotimes (j comp-vector)
             (let* ((info-no-actual (vector-ref i j))
                    (no-actual (no-info-no info-no-actual))
                    (dist-actual (dist-info-no info-no-actual)))
               ; Para cada info-no e' memorizado o no e a distancia associada
               (when (and (pertence? no-actual c)
                          (< dist-actual menor-dist))
                 (set! menor-dist dist-actual)
                 (set! no-menor-dist no-actual))))
    ; Quando se verifica que o no actual pertence ao conjunto dos nos a ser avaliado e verifica-se
    ; se a distancia e menor que menor-dist. Caso seja, iremos alterar o valor associado a' menor 
    ; distancia e ao no correspondente     
    no-menor-dist))

; ================================================================================================
;                                        Modificadores: 
; ================================================================================================
; actualiza-dist-ant-no!: info-no's x no' x inteiro x no' -> info-no's 
; actualiza-dist-ant-no!(i,n,d,a) : altera a distancia e o no' anterior associados ao no' n 
; em i, para d e a, respectivamente. Devolve i com a informacao associada ao no' n alterada.
; ------------------------------------------------------------------------------------------------


(define (actualiza-dist-ant-no! i n d a)
  (let ((tamanho (vector-length i)))
    (do ((j 0 (add1 j)))
      ((equal? (no-info-no (vector-ref i j)) n)
       (modifica-dist! (vector-ref i j) d) (modifica-ant! (vector-ref i j) a)))
    ; O procedimento vai percorrer i ate encontrar um info-no' cujo no' seja n, assim que
    ; encontrar, ira' modificar a distancia e o no' anterior desse info-no'    
    i))

; ================================================================================================ 
;                                      Fim do tipo info-no's 
; ================================================================================================


; ================================================================================================ 
; ================================================================================================ 
;                             Fim dos tipos abstractos de informacao 
; ================================================================================================
; ================================================================================================


; ================================================================================================
;                                      Algoritmo de Dijkstra
; ================================================================================================
; Implementacao do algoritmo sem estado interno 

; caminho-mais-curto: Origem X Destino X Grafo -> Caminho completo mais curto entre a origem e o
; destino do grafo 
; caminho-mais-curto (origem, destino, grafo): Devolve um par cujo primeiro elemento e' o caminho
; completo entre a origem e o destino com os ro'tulos dos ramos intercalados entre os no's, e cujo
; segundo elemento e' o somato'rio dos ro'tulos (distancia total) 
; ------------------------------------------------------------------------------------------------

(define (caminho-mais-curto origem destino grafo)
  (define (caminho-mais-curto-aux origem destino grafo)
    (let* ((nos-G (nos grafo))
           (info-nos (actualiza-dist-ant-no! (cria-info-inicial nos-G) origem 0 null))
           ; cria um info-no's inicial (tabela) cujo info-no' referente a' origem tem como
           ; distancia associada zero
           (caminho (nova-lista))
           (n destino))
      (while (pertence? destino nos-G)
             (let ((n (no<dist info-nos nos-G))
                   (lista-viz (nova-lista)))
               ; Atribui-se a n o no' com a menor distancia associada no info-nos dado nos-G
               (set! nos-G (retira-conj n nos-G))
               ; Como se vai analisar o no' n, retira-se este dos nos-G 
               (when (not (equal? n destino))
                 (set! lista-viz (conj->lista (interseccao (vizinhos n grafo) nos-G)))
                 ; criamos uma lista auxiliar com todos os vizinhos do no' n contidos em nos-G 
                 (dolist (vizinho lista-viz) 
                         ; usamos dolist porque queremos percorrer todos os vizinhos 
                         (let ((nova-dist (+ (distancia-entre n vizinho grafo)
                                             (dist-info-no (obtem-info-no info-nos n)))))
                           ; Atribui'mos a nova-dist a soma da distancia entre o vizinho e o no' n
                           ; com o valor existente em info-no de n 
                           (when (< nova-dist 
                                    (dist-info-no (obtem-info-no info-nos vizinho)))
                             (set! info-nos 
                                   (actualiza-dist-ant-no! info-nos vizinho nova-dist n))))))))
      ; verifica-se a nova-dist e' inferior a' registada anteriormente, caso seja 
      ; actualizamos o info-nos com a nova-dist no info-no de n 
      (while (not (equal? n origem))
             (let ((dist-n (dist-info-no (obtem-info-no info-nos n)))
                   (dist-ant (dist-info-no (obtem-info-no info-nos 
                                                          (ant-info-no (obtem-info-no info-nos n))))))                     
               (set! caminho (insere n caminho))
               (set! caminho (insere (- dist-n dist-ant) caminho))
               (set! n (ant-info-no (obtem-info-no info-nos n)))))
      ; Inserimos no caminho o no' n e de seguida inserimos a distancia entre n e o no' anterior, de
      ; seguida atribui'mos o no' anterior de n a' varia'vel n 
      (cons (insere origem caminho) 
            (dist-info-no (obtem-info-no info-nos destino)))))
  ; Devolve ao utilizador um par cujo primeiro elemento e' o caminho (com a origem inserida) e o 
  ; segundo elemento e distancia total 
  (if (and (grafo? grafo) 
           (pertence? origem (nos grafo)) 
           (pertence? destino (nos grafo))) 
      ; O no' de origem e de destino tem que pertencer ao grafo quando este e' va'lido
      (cond ((equal? origem destino)
             (cons (nova-lista) 0))
            ((or (conj-vazio? (vizinhos destino grafo))
                 (conj-vazio? (vizinhos origem grafo)))
             (cons (nova-lista) +inf.0))
            ; caso nao exista um conjunto de ramos que ligue a origem ao destino 
            (else (caminho-mais-curto-aux origem destino grafo)))
      (error "caminho-mais-curto: O 1o e o 2o argumento tem que ser no's que pertencam ao grafo (3o argumento)")))


; ================================================================================================
; ================================================================================================
; Implementacao do algoritmo com estado interno 

; Descricao da varia'vel de estado base-dados: E' representada internamente como um vector com 
; tantos elementos quantos os nos do grafo em que cada elemento e' constitui'do por outro vector
; de 3 posicoes (0, 1 e 2) que contem, respectivamente o no' de origem, o info-nos e o conjunto 
; dos no's analisados. 

; cria-proc-caminhos-mais-curtos: Grafo -> Procedimento com uma varia'vel de estado base-dados 
; associada ao grafo 
; cria-proc-caminhos-mais-curtos (grafo): Devolve um procedimento com estado interno
; ================================================================================================


(define (cria-proc-caminhos-mais-curtos grafo)
  (when (not (grafo? grafo))
    (error "cria-proc-caminhos-mais-curtos: O 1o argumento tem que ser um grafo va'lido"))
  
  (let ((base-dados (make-vector (comprimento (conj->lista (nos grafo))))))
    ; criacao de um vector base-dados com o nu'mero de elementos igual ao numero de no's do 
    ; grafo. O vector ira' ter cada entrada inicializada a 0
    (lambda (origem destino)
      (let ((nos-G (nos grafo))
            (informacoes null)            
            (info-nos null)
            (nos-ja-processados (novo-conj))            
            (comp-base-dados (vector-length base-dados)))
        ; inicializacao de varia'veis auxiliares
        
        (define (caminho-mais-curto-aux origem destino)
          (let ((caminho (nova-lista))
                (n destino)
                (nos-processados (novo-conj)))
            (dolist (i (conj->lista nos-ja-processados))
                    ; Utiliza-se dolist para percorrer todos os elementos da lista
                    (when (pertence? i nos-G)
                      (set! nos-G (retira-conj i nos-G))))
            ; iremos retirar os no's ja' processados dos nos-G para evitar a repeticao do 
            ; processamento dos mesmos no's                        
            (while (pertence? destino nos-G) 
                   (let ((n (no<dist info-nos nos-G))
                         (lista-v (nova-lista)))
                     ; Atribui-se a n o no' com a menor distancia associada no info-nos dado nos-G
                     (set! nos-G (retira-conj n nos-G))
                     ; Como se vai analisar o no' n, retira-se este dos nos-G
                     (set! nos-processados (insere-conj n nos-processados))
                     ; Atribui'mos aos nos-processados o conjunto dos nos n que se vai processando
                     (when (not (equal? n destino)) 
                       (set! lista-v (conj->lista (interseccao (vizinhos n grafo) nos-G)))
                       ;criamos uma lista auxiliar com todos os vizinhos do no' n contidos em nos-G
                       (dolist (vizinho lista-v)
                               ; usamos dolist porque queremos percorrer todos os vizinhos
                               (let ((nova-dist (+ (distancia-entre n vizinho grafo) 
                                                   (dist-info-no (obtem-info-no info-nos n)))))
                                 ; Atribui'mos a nova-dist a soma da distancia entre o vizinho e o
                                 ; no' n com o valor existente em info-no de n
                                 (when (< nova-dist (dist-info-no (obtem-info-no info-nos vizinho)))
                                   (set! info-nos (actualiza-dist-ant-no! info-nos vizinho 
                                                                          nova-dist n)))))))) 
            ; verifica se a nova-dist e inferior a' registada anteriormente, caso seja
            ; actualizamos o info-nos com a nova-dist no info-no de n
            (set! nos-processados (retira-conj destino nos-processados))
            (display nos-processados) ;código acrescentado!!!!!!!!!!!!!
            ; Atribui'mos aos nos-processados o mesmo conjunto sem o no' de destino 
            (when (not (conj-vazio? nos-processados))
              (dolist (i (conj->lista nos-processados))
                      (set! nos-ja-processados (insere-conj i nos-ja-processados)))
              (actualiza-base-dados! origem info-nos nos-ja-processados))
            ; Actualiza-se a base dados somente se forem processados no's
            (while (not (equal? n origem))
                   (let ((dist-n (dist-info-no (obtem-info-no info-nos n)))
                         (dist-ant (dist-info-no 
                                    (obtem-info-no info-nos (ant-info-no (obtem-info-no info-nos n))))))                     
                     (set! caminho (insere n caminho))
                     (set! caminho (insere (- dist-n dist-ant) caminho))
                     (set! n (ant-info-no (obtem-info-no info-nos n)))))
            ; Inserimos no caminho o no' n e de seguida inserimos a distancia entre n e o no'
            ; anterior, de seguida atribui'mos o no' anterior de n a' varia'vel n  
            (cons (insere origem caminho) 
                  (dist-info-no (obtem-info-no info-nos destino)))))
        ; Devolve ao utilizador um par cujo primeiro elemento e' o caminho (com a origem inserida)
        ; e o segundo elemento e distancia total
        
        
        (define (acesso-base-dados! origem)
          ; vai verificar se existe na base de dados uma entrada com a origem. Se existir vamos
          ; aceder a's informacoes ja guardadas, caso contra'rio ira'-se criar uma nova entrada na
          ; base-dados
          
          (let ((satisfaz? #f)
                (i 0))
            (while (not satisfaz?)
                   ; ciclo while porque nao e' necessario percorrer toda a base de dados
                   (cond ((equal? 0 (vector-ref base-dados i))
                          ; caso em que nao existe nenhuma entrada associada ao no' de origem
                          (set! informacoes (vector origem 
                                                    (actualiza-dist-ant-no! (cria-info-inicial nos-G) 
                                                                            origem 0 null)
                                                    (novo-conj)))
                          (set! info-nos (vector-ref informacoes 1))
                          (set! nos-ja-processados (vector-ref informacoes 2))
                          (vector-set! base-dados i informacoes)
                          ; Como nao existe na base-dados iremos inicializar as informações
                          ; correspontes e actualizar a base-dados                          
                          (set! satisfaz? #t))                       
                         ((equal? origem (vector-ref (vector-ref base-dados i) 0))
                          ; caso em que existe uma entrada com a origem na base-dados
                          (set! informacoes (vector-ref base-dados i))
                          (set! info-nos (vector-ref informacoes 1))
                          (set! nos-ja-processados (vector-ref informacoes 2))
                          ; Iremos armazenar toda a informacao associada para ca'lculos posteriores
                          (set! satisfaz? #t))                         
                         (else (set! i (add1 i)))))))             
        
        (define (actualiza-base-dados! origem info-nos nos-ja-processados)
          ; vai actualizar a base dados armazenando toda a informacao relevante para posterior uso
          (do ((i 0 (add1 i)))
            ((equal? origem (vector-ref (vector-ref base-dados i) 0))
             (vector-set! informacoes 1 info-nos)
             (vector-set! informacoes 2 nos-ja-processados)
             (vector-set! base-dados i informacoes))))
        ; Vai percorrer o vector base-dados ate encontrar a entrada pretendida, assim que
        ; encontrar vai actualizar a base-dados
        
        (if (and (pertence? origem (nos grafo))
                 (pertence? destino (nos grafo)))
            ; A origem e o destino tem que pertencer ao grafo quando este e' va'lido
            (cond ((equal? origem destino) 
                   (cons (nova-lista) 0))
                  ; caso onde o no' de origem e' a igual ao do destino
                  ((or (conj-vazio? (vizinhos destino grafo))
                       (conj-vazio? (vizinhos origem grafo)))
                   (cons (nova-lista) +inf.0))
                  ; caso onde ou o destino ou a origem sao inacessi'veis atrave's de outros no's
                  (else (acesso-base-dados! origem)
                        ; iremos verificar os dados que ja' temos armazenados e usa'-los para 
                        ; evitar ca'lculos repetidos
                        (caminho-mais-curto-aux origem destino)))
            (error "caminho-mais-curto: O 1o e o 2o argumento tem que ser no's que pertencam ao grafo"))))))

; ================================================================================================
;                                        Fim do programa
; ================================================================================================

;TESTE
  (define grafo
	(faz-grafo
	   (faz-conj 'no-A 'no-B 'no-C 'no-D 'no-E 'no-F 'no-X)
	   (faz-conj
                 (faz-ramo 'no-A 'no-C 7)
                 (faz-ramo 'no-A 'no-B 9)
                 (faz-ramo 'no-A 'no-F 14)
                 (faz-ramo 'no-C 'no-B 10)
                 (faz-ramo 'no-C 'no-D 15)
                 (faz-ramo 'no-B 'no-D 11)
                 (faz-ramo 'no-B 'no-F 2)
                 (faz-ramo 'no-D 'no-E 6)
                 (faz-ramo 'no-E 'no-F 8))))
(define caminho-mais-curto-est-interno
	(cria-proc-caminhos-mais-curtos grafo))
	(caminho-mais-curto 'no-A 'no-A grafo)
	(caminho-mais-curto 'no-A 'no-F grafo)
	(caminho-mais-curto 'no-A 'no-E grafo)
	(caminho-mais-curto 'no-A 'no-X grafo)
	(caminho-mais-curto 'no-D 'no-C grafo)
	(caminho-mais-curto 'no-A 'no-C grafo)
	(caminho-mais-curto-est-interno 'no-A 'no-A)
	(caminho-mais-curto-est-interno 'no-A 'no-F)
	(caminho-mais-curto-est-interno 'no-A 'no-E)
	(caminho-mais-curto-est-interno 'no-A 'no-X)
	(caminho-mais-curto-est-interno 'no-D 'no-C)
	(caminho-mais-curto-est-interno 'no-A 'no-C)

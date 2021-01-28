;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |guess - 副本|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require "animals.rkt")

;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

;; Some useful constants for examples and tests
(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))
(define test1
  (list
   (list 'sparrow 'small 'flies 'angry)
   (list 'duck 'small 'swims 'flies)
   (list 'duck 'medium 'swims 'flies)
   (list 'crow 'medium 'flies 'angry)
   (list 'duck 'medium 'flies)))

;; (collect-attributes examples) consumes a (listof Example) called
;;   examples and produces a list of attributes as symbols contained
;;   in examples with no duplicates.
;; Examples:
(check-expect (collect-attributes '((sparrow small flies)
                                    (squirrel small angry)))
              (list 'small 'flies 'angry))
(check-expect (collect-attributes '((goose small flies swims angry)
                                    (sparrow small flies)))
              (list 'small 'flies 'swims 'angry))
(check-expect (collect-attributes '((crow medium flies)
                                    (sparrow small flies)
                                    (duck small swims flies)))
              (list 'medium 'flies 'small 'swims))

;; collect-attributes: (listof Example) -> (listof Sym)
(define (collect-attributes examples)
  (local [(define (collect-helper examples accumulator)
            (local [(define (attri-list example example-cons accumulator)
                      (cond
                        [(empty? example) empty]
                        [(symbol=? (first example)
                                   (first example-cons))
                         (attri-list (rest example)
                                     example-cons
                                     accumulator)]
                        [(inlist (first example) accumulator)
                         (attri-list (rest example)
                                     example-cons
                                     accumulator)]
                        [else (cons (first example)
                                    (attri-list (rest example)
                                                example-cons
                                                accumulator))]))] 
              (cond
                [(empty? examples) accumulator]
                [else (collect-helper (rest examples)
                                      (append accumulator
                                              (attri-list (first examples)
                                                          (first examples) 
                                                          accumulator)))])))]
    (collect-helper examples empty)))

;; Tests:
(check-expect (collect-attributes empty) empty)
(check-expect (collect-attributes (list (list 'sparrow 'small 'flies))) 
              (list 'small 'flies))


;; (inlist sym los) is a helper function that consumes a Sym called sym
;;   and a (listof Sym) called los to identify if sym is in los.
;; Examples:
(check-expect (inlist 's (list 'c 'h 'e)) false)
(check-expect (inlist 'a (list 'w 'a 't)) true)

;; inlist: Sym (listof Sym) -> Bool
(define (inlist sym los)
  (cond
    [(empty? los) false]
    [(symbol=? (first los) sym) true] 
    [else (inlist sym (rest los))]))

;; Tests:
(check-expect (inlist 's empty) false)
(check-expect (inlist 's (list 'c)) false) 


;; (split-examples examples symbol) consumes a (listof Example)
;;   called examples and a Symbol called symbol to produce a list
;;   of two (listof Examples), dividing the elements in the original
;;   list based on symbol.
;; Examples:
(check-expect (split-examples seen 'goose) 
              (list
               (list
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry)) 
               (list
                (list 'crow 'medium 'flies 'angry)
                (list 'squirrel 'small 'angry))))
(check-expect (split-examples seen 'small) 
              (list
               (list
                (list 'squirrel 'small 'angry))
               (list
                (list 'crow 'medium 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry))))

;; split-examples: (listof Example) Sym ->
;;   (list (listof Example) (listof Example))
(define (split-examples examples symbol)
  (local [(define (split-helper examples start1 start2)
            (cond
              [(empty? examples) (list start1 start2)]
              [(inlist symbol (first examples))
               (split-helper (rest examples)
                             (cons (first examples) start1)
                             start2)]
              [else
               (split-helper (rest examples)
                             start1
                             (cons (first examples) start2))]))]
    (split-helper examples empty empty)))

;; Tests:
(check-expect (split-examples empty 'bat) (list empty empty))
(check-expect (split-examples seen 'bat)
              (list empty (list
                           (list 'crow 'medium 'flies 'angry)
                           (list 'goose 'large 'swims 'flies 'angry)
                           (list 'goose 'large 'swims 'flies 'angry)
                           (list 'squirrel 'small 'angry))))


;; (histogram examples) consumes a (listof Example) called examples and
;;   produces a list of attribute/count pairs, with each pair indicating
;;   how many times that attribute appears in the examples.
;; Examples:
(check-expect (histogram seen)
              (list
               (list 'small 1) (list 'angry 4) (list 'large 2)
               (list 'swims 2) (list 'flies 3) (list 'medium 1)))
(check-expect (histogram test1)
              (list
               (list 'small 2) (list 'flies 5) (list 'angry 2)
               (list 'swims 2) (list 'medium 3)))

;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local [(define (modeling los)
            (cond
              [(empty? los) empty]
              [else (cons (list (first los) 0)
                          (modeling (rest los)))]))

          (define (addtolist symbol lst)
            (cond
              [(empty? lst) empty]
              [(symbol=? symbol (first (first lst)))
               (cons (list symbol (+ 1 (second (first lst))))
                     (rest lst))]
              [else (cons (first lst)
                          (addtolist symbol (rest lst)))]))

          (define (add-exp-to-list example lst)
            (cond
              [(empty? example) lst]
              [else (add-exp-to-list (rest example)
                                     (addtolist (first example) lst))]))

          (define (hist-help examples accumulator)
            (cond
              [(empty? examples) accumulator]
              [else (hist-help (rest examples)
                               (add-exp-to-list (first examples)
                                                accumulator))]))]
    (hist-help examples (modeling (collect-attributes examples)))))

;; Tests:
(check-expect (histogram empty) empty)
(check-expect (histogram (list (list 'duck 'medium 'flies)))
              (list
               (list 'medium 1) (list 'flies 1)))


;; (augment-histogram histogram attributes total) consumes the result
;;   of function (histogram examples), expressed as (listof (list Sym Nat))
;;   and called histogram, a list of all attributes possible called
;;   attributes, and a Nat of the number of Example in examples called total
;;   to include non-existing kinds of attributes and the number of Example
;;   that do not have a certain attribute in histogram as an AH structure.
;; Examples:
(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect
 (augment-histogram empty (list 'x 'y) 10)
 (list (list 'x 0 10) (list 'y 0 10)))

;; augment-histogram: Histogram (listof Sym) Nat -> AH
;;   Requires:
;;   length of attributes is larger than or equal to the length of histogram
(define (augment-histogram histogram attributes total)
  (local [(define (add-attri histogram attribute)
            (cond
              [(empty? histogram)
               (cons (list attribute 0) empty)]
              [(symbol=? attribute (first (first histogram)))
               histogram]
              [else (cons (first histogram)
                          (add-attri (rest histogram) attribute))]))

          (define (add-attri-list histogram attributes)
            (cond
              [(empty? attributes) histogram]
              [else (add-attri-list (add-attri histogram (first attributes))
                                    (rest attributes))]))

          (define (add-number histogram)
            (cond
              [(empty? histogram) empty]
              [else (cons
                     (list
                      (first (first histogram))
                      (second (first histogram))
                      (- total (second (first histogram))))
                     (add-number (rest histogram)))]))
         
          (define (sorting attributes ah)
            (local [(define (extract attribute ah)
                      (cond
                        [(symbol=? (first (first ah))
                                   attribute)
                         (first ah)]
                        [else (extract attribute (rest ah))]))]
              (cond
                [(empty? attributes) empty]
                [else (cons (extract (first attributes) ah)
                            (sorting (rest attributes) ah))])))]
    (sorting attributes
             (add-number (add-attri-list histogram attributes)))))

;; Tests:
(check-expect (augment-histogram empty empty 10) empty)
(check-expect
 (augment-histogram
  empty
  (list 'a 'b 'c)
  200)
 (list (list 'a 0 200) (list 'b 0 200) (list 'c 0 200)))


;; (entropy positive-counts negative-counts) consumes two elements
;;   from augmented histograms called positive-counts and
;;   negative-counts to produce their entropy.
;; Examples:
(check-within (entropy (list 'large 126 59) (list 'large 146 669)) 0.566 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361)) 0.583 0.001)

;; entropy: (list Sym Nat Nat) (list Sym Nat Nat) -> Num
(define (entropy positive-counts negative-counts)
  (local [(define (P n m)
            (cond
              [(> (+ n m) 0) (/ n (+ n m))]
              [else 0.5]))

          (define (e p)
            (cond
              [(= p 0) 0]
              [else (* (- 0 p) (log p 2))]))]
    (+ (* (P (+ (second positive-counts)
                (second negative-counts))
             (+ (third positive-counts)
                (third negative-counts)))
          (+ (e (P (second positive-counts)
                   (second negative-counts)))
             (e (P (second negative-counts)
                   (second positive-counts)))))
       (* (P (+ (third positive-counts)
                (third negative-counts))
             (+ (second positive-counts)
                (second negative-counts)))
          (+ (e (P (third positive-counts)
                   (third negative-counts)))
             (e (P (third negative-counts)
                   (third positive-counts))))))))

;; Tests:
(check-within (entropy (list 'large 0 59) (list 'large 0 669)) 0.406 0.001)
(check-within (entropy (list 'large 0 0) (list 'large 0 0)) 1.0 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 100 0)) 0.0 0.001)


;; (entropy-attributes positive negative) consumes two augmented
;;   histograms with same attributes in the same order, called
;;   positive and negative, and computes the entropy of each
;;   attribute, producing a list of attribute/entropy pairs.
;; Examples:
(check-within (entropy-attributes
               (list
                (list 'large 126 59) (list 'angry 161 24)
                (list 'small 17 168) (list 'flies 170 15)
                (list 'swims 162 23) (list 'medium 42 143))
               (list
                (list 'large 146 669) (list 'angry 469 346)
                (list 'small 454 361) (list 'flies 615 200)
                (list 'swims 365 450) (list 'medium 215 600)))
              (list
               (list 'large 0.566)
               (list 'angry 0.645)
               (list 'small #i0.583)
               (list 'flies #i0.670)
               (list 'swims #i0.602)
               (list 'medium #i0.690)) 0.001)

;; entropy-attributes: AH AH -> EAL
;;   Requires:
;;   positive and negative have same attributes in the same order.
(define (entropy-attributes positive negative)
  (cond
    [(empty? positive) empty]
    [else (cons (list (first (first positive))
                      (entropy (first positive) (first negative)))
                (entropy-attributes (rest positive) (rest negative)))]))

;; Tests:
(check-expect (entropy-attributes empty empty) empty)
(check-within (entropy-attributes
               (list
                (list 'small 17 168))
               (list
                (list 'small 454 361)))
              (list
               (list 'small 0.583)) 0.001)


;; (best-attribute entropies) consumes a non-empty EAL called entropies
;;   and produces the attribute with minimum entropy.
;; Examples:
(check-expect (best-attribute
               (list
                (list 'large #i0.5663948489858)
                (list 'angry #i0.6447688190492)
                (list 'small #i0.5825593868115)
                (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730)
                (list 'medium #i0.6901071708677))) 'large)
(check-expect (best-attribute
               (list
                (list 'large #i0.5663948489858)
                (list 'angry #i0.3447688190492)
                (list 'small #i0.5825593868115)
                (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730)
                (list 'medium #i0.6901071708677))) 'angry)
(check-expect (best-attribute
               (list
                (list 'large #i0.7663948489858)
                (list 'angry #i0.6447688190492)
                (list 'small #i0.5825593868115)
                (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730)
                (list 'medium #i0.6901071708677))) 'small)

;; best-attribute: EAL -> Sym
;;   Requires:
;;   entropies is non-empty
(define (best-attribute entropies)
  (local [(define (entro-list entropies)
            (cond
              [(empty? (rest entropies))
               (cons (second (first entropies)) empty)]
              [else (cons (second (first entropies))
                          (entro-list (rest entropies)))]))

          (define (min-entro lon)
            (cond
              [(empty? (rest lon)) (first lon)]
              [else (min (first lon) (min-entro (rest lon)))]))] 
    (cond
      [(= (second (first entropies))
          (min-entro (entro-list entropies)))
       (first (first entropies))]
      [else (best-attribute (rest entropies))])))

;; Tests:
(check-expect (best-attribute
               (list
                (list 'medium #i0.6901071708677))) 'medium)
(check-expect (best-attribute
               (list
                (list 'swims #i0.6017998773730)
                (list 'medium #i0.6017998773730))) 'swims)


;; (build-dt examples label) consumes (listof Example) called examples
;;   and a Sym called label to build a decision tree.
;; Examples:
(check-expect (build-dt seen 'goose)
              (list 'large #true #false))
(check-expect (build-dt test1 'duck)
              (list 'angry #false #true))
(check-expect (build-dt (random-animals 1000) 'emu)
              false)

;; build-dt: (listof Example) Sym -> DT
(define (build-dt examples label)
  (local [(define (find-attribute examples label)
            (best-attribute
             (entropy-attributes
              (augment-histogram
               (histogram (first (split-examples examples label)))
               (collect-attributes examples)
               (length (first (split-examples examples label))))
              (augment-histogram
               (histogram (second (split-examples examples label)))
               (collect-attributes examples)
               (length (second (split-examples examples label)))))))

          (define (remove-exp example attri)
            (cond
              [(empty? example) empty]
              [(symbol=? (first example) attri)
               (remove-exp (rest example) attri)]
              [else (cons (first example)
                          (remove-exp (rest example) attri))]))

          (define (remove-exps examples attri)
            (cond
              [(empty? examples) empty]
              [else (cons (remove-exp (first examples) attri)
                          (remove-exps (rest examples) attri))]))

          (define (dt-help examples label)
            (cond
              [(empty? (first (split-examples examples label))) false]
              [(empty? (second (split-examples examples label))) true] 
              [(and (empty? (collect-attributes examples))
                    (> (length (first (split-examples examples label)))
                       (length (second (split-examples examples label))))) 
               true]
              [(and (empty? (collect-attributes examples))
                    (<= (length (first (split-examples examples label)))
                        (length (second (split-examples examples label)))))
               false]
              [else
               (list (find-attribute examples label)
                     (build-dt
                      (remove-exps
                       (first (split-examples
                               examples
                               (find-attribute examples
                                               label)))
                       (find-attribute examples label))
                      label)
                     (build-dt
                      (second (split-examples
                               examples
                               (find-attribute examples label)))
                      label))]))

          (define (simplify dt)
            (cond
              [(boolean? dt) dt]
              [(equal? (second dt) (third dt))
               (simplify (second dt))]
              [else (list (first dt)
                          (simplify (second dt))
                          (simplify (third dt)))]))]
    (simplify (dt-help examples label))))


;; (train-classifier examples label) consumes a (listof Example)
;;   called examples and a Sym called label to produce a function
;;   of type (Example -> Bool).

;; train-classifier: (listof Example) Sym -> (Example -> Bool)
(define (train-classifier examples label)
  (local [(define (classifier-help dtree example)
            (cond
              [(boolean? dtree) dtree]
              [(inlist (first dtree) example)
               (classifier-help (second dtree) example)]
              [else (classifier-help (third dtree) example)]))

          (define (classifier example)
            (classifier-help (build-dt examples label) example)) 
          ] 
    classifier))

;; Tests:
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(check-expect (crow? (list 'angry 'flies 'medium)) true)

;; Some useful constants for examples
(define goose? (train-classifier (random-animals 7000) 'goose)) 
(define squirrel? (train-classifier (random-animals 7000) 'squirrel))
(define crow? (train-classifier (random-animals 7000) 'crow))


;; (performance classifier? examples label) consumes a function of
;;   type (Example -> Bool) called classifier?, a (listof Example)
;;   called examples, and a Sym called label to produce a list
;;   consisting of label, sensitivity, and specificity of the program.

;; performance: (Example -> Bool) (listof Example) Sym -> (list Sym Nat Nat) 
(define (performance classifier? examples label)
  (local [(define (code-count-true examples accumulator)
            (cond
              [(empty? examples) accumulator]
              [(classifier? (rest (first examples)))
               (code-count-true (rest examples) (+ 1 accumulator))]
              [else (code-count-true (rest examples) accumulator)]))

          (define (code-count-false examples accumulator)
            (cond
              [(empty? examples) accumulator]
              [(not (classifier? (rest (first examples))))
               (code-count-false (rest examples) (+ 1 accumulator))]
              [else (code-count-false (rest examples) accumulator)]))

          (define (real-count-true examples accumulator)
            (cond
              [(empty? examples) accumulator]
              [(symbol=? (first (first examples)) label)
               (real-count-true (rest examples) (+ 1 accumulator))]
              [else (real-count-true (rest examples) accumulator)]))

          (define (real-count-false examples accumulator)
            (cond
              [(empty? examples) accumulator]
              [(not (symbol=? (first (first examples)) label))
               (real-count-false (rest examples) (+ 1 accumulator))]
              [else (real-count-false (rest examples) accumulator)]))]
    (list label
          (round (* 100 (/ (code-count-true examples 0)
                           (real-count-true examples 0))))
          (round (* 100 (/ (code-count-false examples 0)
                           (real-count-false examples 0)))))))
; **************** BEGIN INITIALIZATION FOR ACL2s B MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);

#|
Pete Manolios
Fri Jan 27 09:39:00 EST 2012
----------------------------

Made changes for spring 2012.


Pete Manolios
Thu Jan 27 18:53:33 EST 2011
----------------------------

The Beginner level is the next level after Bare Bones level.

|#
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the TRACE* book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
; only load for interactive sessions: 
#+acl2s-startup (include-book "trace-star" :uncertified-okp nil :dir :acl2s-modes :ttags ((:acl2s-interaction)) :load-compiled-file nil);v4.0 change

#+acl2s-startup (assign evalable-printing-abstractions nil)

;arithmetic book
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading arithmetic-5/top book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "arithmetic-5/top" :dir :system)

;basic thms/lemmas about lists
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading std/lists/top book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "std/lists/top" :dir :system)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2's lexicographic-ordering-without-arithmetic book.~%This indicates that either your ACL2 installation is missing the standard books are they are not properly certified.") (value :invisible))
(include-book "ordinals/lexicographic-ordering-without-arithmetic" :dir :system)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "ccg" :uncertified-okp nil :dir :acl2s-modes :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the EVALABLE-LD-PRINTING book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
; only load for interactive sessions: 
#+acl2s-startup (include-book "evalable-ld-printing" :uncertified-okp nil :dir :acl2s-modes :ttags ((:acl2s-interaction)) :load-compiled-file nil);v4.0 change

;; #+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading DataDef+RandomTesting book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
;; (include-book "countereg-gen/top" :uncertified-okp nil :dir :system :load-compiled-file :comp)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "custom" :dir :acl2s-modes :uncertified-okp nil
                                         :load-compiled-file
                                         :comp :ttags :all)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s Beginner mode.") (value :invisible))


;Settings common to all ACL2s modes
(acl2s-common-settings)

; Non-events:
(acl2::set-guard-checking :all)

(defconst *testing-upper-bound* 1000)  

(defun nth-symbol-b-builtin (n)
;  (declare (xargs :guard (natp n)))
;:verify-guards nil)) 
  (intern$ (nth-var-string n) "ACL2S B"))

(defttag t)
(defattach (nth-symbol nth-symbol-b-builtin) :skip-checks t)
(defttag nil)

(defun nth-character-b-builtin (n)
  (declare (xargs :guard (natp n)))
  (nth (mod n *len-alpha-num-chars*) *alpha-num-chars*))

(defattach nth-character nth-character-b-builtin)

(defun nth-acl2-number-b-builtin (n)
  (declare (xargs :guard (natp n)))
  (b* (((mv choice seed)
        (defdata::switch-nat 3 n)))
    (case choice
          (0 (nth-nat seed))
          (1 (nth-integer seed))
          (t (nth-rational seed)))))

(defattach nth-acl2-number nth-acl2-number-b-builtin)

(defun nth-small-pos-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-pos n-small)))

(defun nth-small-integer-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-integer n-small)))

(defun nth-small-nat-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-nat n-small)))

(defun nth-small-neg-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-neg n-small)))

(defun nth-small-positive-ratio-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-positive-ratio n-small)))

(defun nth-small-negative-ratio-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-negative-ratio n-small)))

(defun nth-small-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-rational n-small)))

(defun nth-small-positive-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-positive-rational n-small)))

(defun nth-small-negative-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-negative-rational n-small)))

(defun nth-small-acl2-number-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-acl2-number n-small)))

(defun nth-small-all (n)
  (declare (xargs ;:guard (natp n) ))
            :mode :program
            :verify-guards nil))
  (mv-let (choice seed) 
          (defdata::weighted-switch-nat 
            '(1 ;nil
              1 ;t
              1 ;0
              1 ;nat

              1 ;sym
              1 ;string
              1 ;integer
              1 ;char
              1 ;rational
              1 ;acl2-number
              1 ;atom

              2 ;boolean-list
              5 ;list-aa
              5 ;list-la-la 

              5 ;int-list
              5 ;sym-list
              5 ;stringlist
              5 ;charlist
              5 ;acl2-num-list

              10 ;atom-list
              ) n)
          
          (case choice
            (0 'nil)
            (1 't)
            (2  0)
            (3  (nth-small-nat-testing seed))

            (4  (nth-symbol seed))
            (5  (nth-string seed))
            (6  (nth-small-integer-testing seed))
            (7  (nth-character seed))
            (8  (nth-small-rational-testing seed))
            (9  (nth-small-acl2-number-testing seed))
            (10  (nth-atom seed))

            (11 (nth-boolean-list seed))
            (12 (b* (((list i1 i2) (defdata::split-nat 2 seed))) (list (nth-atom i1) (nth-atom i2)))) ;(nth-list-aa seed))
            (13 (b* (((list i1 i2 i3 i4) (defdata::split-nat 4 seed))) 
                  (list (list (nth-atom i1) (nth-atom i2)) 
                        (list (nth-atom i3) (nth-atom i4))))) ;(list-la-la seed))

            (14 (nth-integer-list seed))
            (15 (nth-symbol-list seed))
            (16 (nth-string-list seed))
            (17 (nth-character-list seed))
            (18 (nth-acl2-number-list seed))

            (19 (nth-atom-list seed))

            (t 'nil)))) ;this case should not come up

(defdata-attach pos :test-enumerator nth-small-pos-testing)
(defdata-attach integer :test-enumerator nth-small-integer-testing)
(defdata-attach nat :test-enumerator nth-small-nat-testing)
(defdata-attach neg :test-enumerator nth-small-neg-testing)
(defdata-attach positive-ratio :test-enumerator nth-small-positive-ratio-testing)
(defdata-attach negative-ratio :test-enumerator nth-small-negative-ratio-testing)
(defdata-attach rational :test-enumerator nth-small-rational-testing)
(defdata-attach positive-rational :test-enumerator nth-small-positive-rational-testing)
(defdata-attach negative-rational :test-enumerator nth-small-negative-rational-testing)
(defdata-attach acl2-number :test-enumerator nth-small-acl2-number-testing)
(defdata-attach all :test-enumerator nth-small-all)

(acl2s-defaults :set num-trials 50)

(defpkg "ACL2S B" ; beginner
  (union-eq '(t nil 
              ;if ; see macro below
              equal

              defun defunc ;for function definitions

              ; + * unary-- unary-/ < ; see definitions below
              numerator denominator
              rationalp integerp

              consp cons ; car cdr

              cond ; macro: explain
              list ; macro: explain

              lambda
              let let* ; macro: explain

              quote

              symbolp symbol-name symbol-package-name
              ;stringp
              ;charp

              check=

              and or iff implies not booleanp 
              ;+ * 
              / posp negp natp <= > >= zp - atom 
              ; true-listp 
              endp 
              ;caar cadr cdar cddr 
              ;caaar caadr cadar caddr cdaar cdadr cddar cdddr
              ;caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
              ;cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
              
              
              defdata sig => oneof listof alistof enum record map
              nat string pos rational integer boolean all neg
              acl2-number true-list char symbol 
              ;; i need them for defdata why?
             
              ;I need the following for sig macro
              x
              
              trace*

              defthm thm defconst in-package
              test?
              acl2s-defaults 
              testing-enabled verbosity-level 
              num-trials num-counterexamples num-witnesses
              cgen-timeout  

              set-defunc-timeout get-defunc-timeout
              set-defunc-termination-strictp get-defunc-termination-strictp
              set-defunc-function-contract-strictp get-defunc-function-contract-strictp
              set-defunc-body-contracts-strictp get-defunc-body-contracts-strictp
              
              )
            '()))

(defthm natp-implies-acl2-numberp
  (implies (natp x)
           (acl2-numberp x))
  :rule-classes ((:rewrite)))

(defthm posp-implies-acl2-numberp
  (implies (posp x)
           (acl2-numberp x))
  :rule-classes ((:rewrite)))

(defthm integerp-implies-acl2-numberp
  (implies (integerp x)
           (acl2-numberp x))
  :rule-classes ((:rewrite)))

(defthm rationalp-implies-acl2-numberp2
  (implies (rationalp x)
           (acl2-numberp x))
  :rule-classes ((:rewrite)))

(defthm natp-implies-rationalp
  (implies (natp x)
           (rationalp x))
  :rule-classes ((:rewrite)))

(defthm posp-implies-rationalp
  (implies (posp x)
           (rationalp x))
  :rule-classes ((:rewrite)))

(defthm integerp-implies-rationalp
  (implies (integerp x)
           (rationalp x))
  :rule-classes ((:rewrite)))


(acl2::in-package "ACL2S B")

(defun acl2s-bb-identity-bool-guard (x)
  (acl2::declare (acl2::xargs :guard (acl2::booleanp x)))
  x)

(acl2::defmacro if (test tb fb)
  `(acl2::if (acl2s-bb-identity-bool-guard ,test) ,tb ,fb))

(acl2::defthm acl2s-bb-identity-bool-guard-backchain
  (acl2::implies (acl2::booleanp x)
                 (equal (acl2s-bb-identity-bool-guard x)
                        x)))

(acl2::defthm acl2s-bb-identity-bool-guard-equal
  (equal (acl2s-bb-identity-bool-guard (equal x y))
         (equal x y)))

;skip testing in defunc events for faster loading
(acl2::table acl2::defunc-defaults-table :skip-tests t :put)

(defunc first (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::car x))

(defunc rest (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::cdr x))

(sig acl2::cdr ((listof :a)) => (listof :a))

(sig rest ((listof :a)) => (listof :a)
     :satisfies (consp acl2::x1))



(defunc second (x)
  :input-contract (and (consp x) (consp (rest x)))
  :output-contract t
  (acl2::cadr x))

(defunc third (x)
  :input-contract (and (consp x) (consp (rest x)) (consp (rest (rest x))))
  :output-contract t
  (acl2::caddr x))

(defunc fourth (x)
  :input-contract (and (consp x) (consp (rest x)) 
                       (consp (rest (rest x)))
                       (consp (rest (rest (rest x)))))
  :output-contract t
  (acl2::cadddr x))

(defunc unary-- (x)
  :input-contract (rationalp x)
  :output-contract t
  (acl2::unary-- x))

(defunc unary-/ (x)
  :input-contract (acl2::and (rationalp x) (acl2::not (equal x 0)))
  :output-contract t
  (acl2::unary-/ x))

(defunc < (x y)
  :input-contract (acl2::and (rationalp x) (rationalp y))
  :output-contract (acl2::booleanp (< x y))
  (acl2::< x y))

(defunc + (x y)
  :input-contract (acl2::and (rationalp x) (rationalp y))
  :output-contract (rationalp (+ x y))
  (acl2::binary-+ x y))

(defunc * (x y)
  :input-contract (acl2::and (rationalp x) (rationalp y))
  :output-contract (rationalp (+ x y))
  (acl2::binary-* x y))

(defun my-preprocess (term wrld)
  (acl2::declare (acl2::ignore wrld))
  (acl2::cond ((acl2::and (consp term)
                          (acl2::or 
                           (equal (acl2::car term) 'acl2s-bb-identity-bool-guard)
                           (equal (acl2::car term) 'acl2s-bb-identity-consp-guard)
                           (equal (acl2::car term) 'acl2s-bb-identity-rationalp-guard)
                           (equal (acl2::car term) 'acl2s-bb-identity-rationalp-not-0-guard)))
               (acl2::cadr term))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::implies))
               (cons 'implies (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::/))
               (cons '/ (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::car))
               (cons 'first (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::cdr))
               (cons 'rest (acl2::cdr term)))
              ((acl2::and (consp term)
                          (consp (acl2::cdr term))
                          (equal (acl2::car term) 'acl2::not)
                          (equal (acl2::caadr term) 'acl2::>))
               (cons '<= (acl2::cdadr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::not))
               (cons 'not (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::*))
               (cons '* (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::+))
               (cons '+ (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::/))
               (cons '/ (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::-))
               (cons '- (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::<))
               (cons '< (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::>))
               (cons '> (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::acl2-number))
               (cons 'acl2-number (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::true-listp))
               (cons 'listp (acl2::cdr term)))
              (t nil)))

; A hack to help proofs go through in this mode.
(acl2::in-theory (acl2::enable rest))

(acl2::table acl2::user-defined-functions-table
             'acl2::untranslate-preprocess
             'my-preprocess)

(defunc len (a) 
  :input-contract t 
  :output-contract (natp (len a))
  (if (atom a)
      0
    (+ 1 (len (rest a)))))

(defthm intp-len 
  (integerp (len x))
  :rule-classes ((:type-prescription) (:rewrite)))

(acl2::defmacro listp (a)
  `(acl2::true-listp ,a))

(defunc append (a b) 
  :input-contract (and (listp a) (listp b))
  :output-contract (and (listp (append a b))
                        (equal (len (append a b)) (+ (len a) (len b))))
  (acl2::append a b))

;(sig append ((listof :a) (listof :a)) => (listof :a))
(sig acl2::append ((listof :a) (listof :a)) => (listof :a))

(defthm append-length
  (equal (len (acl2::append a b))
         (+ (len a) (len b))))

(defunc app (a b) 
  :input-contract (and (listp a) (listp b))
  :output-contract (and (listp (app a b))
                        (equal (len (app a b)) (+ (len a) (len b))))
  (acl2::append a b))

(sig app ((listof :a) (listof :a)) => (listof :a))

(defunc rev (a) 
  :input-contract (listp a) 
  :output-contract (and (listp (rev a))
                        ;(equal (len (rev a)) (len a))
                        )
  (if (endp a)
      nil
    (append (rev (rest a)) (list (first a)))))

(sig rev ((listof :a)) => (listof :a))

(defunc in (a X) 
  :input-contract (listp x)
  :output-contract (booleanp (in a X))
  (if (endp x)
      nil
    (or (equal a (first X))
        (in a (rest X)))))

(defunc remove-dups (a) 
  :input-contract (listp a) 
  :output-contract (listp (remove-dups a))
  (if (endp a)
      nil
    (if (in (first a) (rest a))
        (remove-dups (rest a))
      (cons (first a) (remove-dups (rest a))))))

(sig remove-dups ((listof :a)) => (listof :a))

(defunc nth (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract t
  (if (endp l)
      nil
    (if (zp n)
        (first l)
      (nth (- n 1) (rest l)))))

(sig nth (nat (listof :a)) => :a 
     :satisfies (< acl2::x1 (len acl2::x2))) ;make this package independent later TODO

(defunc nthrest (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract (listp (nthrest n l))
  (if (endp l)
      nil
    (if (zp n)
        l
      (nthrest (- n 1) (rest l)))))

(sig nthrest (nat (listof :a)) => (listof :a))

(defthm natp-acl2-len-tp 
  (natp (acl2::len x))
  :rule-classes ((:type-prescription) (:rewrite)))

(defthm integerp-acl2-len-tp 
  (integerp (acl2::len x))
  :rule-classes ((:type-prescription) (:rewrite)))

#||
(defunc string-len (l)
  :input-contract (stringp l)
  :output-contract (natp (string-len l))
  (acl2::length l))
||#

;; Note you can only attach/overwrite one attribute at a time
(acl2::defdata-attach list :predicate acl2::true-listp :override-ok t)
(acl2::defdata-attach list :enumerator acl2::nth-true-list :override-ok t)


;; Settings specific to this mode(copied from R&I mode)
(acl2::in-package "ACL2")


(set-backchain-limit '(50 100))
(set-rewrite-stack-limit 500)
(acl2s-defaults :set cgen-timeout 20)
(table defunc-defaults-table :skip-tests nil :put)
(table defunc-defaults-table :timeout 50 :put)

(set-irrelevant-formals-ok :warn)
(set-bogus-mutual-recursion-ok :warn)
(set-ignore-ok :warn)

;for beginner users dont be strict in admitting defunc
;(acl2::acl2s-defaults :set acl2::defunc-strict 0)  
(acl2s-defaults :set num-trials 500)

(assign evalable-ld-printingp t)
(assign evalable-printing-abstractions '(list cons))
(assign triple-print-prefix "; ")

;(acl2::set-guard-checking :none)

(acl2::xdoc defunc)

(cw "~@0Beginner mode loaded.~%~@1"
    #+acl2s-startup "${NoMoReSnIp}$~%" #-acl2s-startup ""
    #+acl2s-startup "${SnIpMeHeRe}$~%" #-acl2s-startup "")


(acl2::in-package "ACL2S B")

; ***************** END INITIALIZATION FOR ACL2s B MODE ******************* ;
;$ACL2s-SMode$;Beginner

#|

CS 2800 Homework 11 - Fall 2014

Student names: This homework is done in pairs. Put BOTH names here.

Yueling Qin&&ZeQing Zhang

Technical instructions:

- open this file in ACL2s BEGINNER mode as hw11.lisp

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish some problems, comment
  them out. The same is true for any English text that you may add. This
  file already contains many comments, so you can see what the syntax is.

- when done, save your file and submit it as hw11.lisp.

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tail Recursive Functions and Equivalence Proofs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

1. Consider the following function definitions:

(defunc app (a b)
  :input-contract (and (listp a) (listp b))
  :output-contract (listp (app a b))
  (if (endp a)
    b
    (cons (first a) (app (rest a) b))))

(defunc rev (x)
  :input-contract (listp x)
  :output-contract (listp (rev x))
  (if (endp x)
    ()
    (app (rev (rest x)) (list (first x)))))

|#

(defunc delete (x l)
  :input-contract (listp l)
  :output-contract (listp (delete x l))
  (cond ((endp l)            ())
        ((equal x (first l)) (delete x (rest l)))
        (t                   (cons (first l) (delete x (rest l))))))

(defunc delete-t (x l acc)
  :input-contract (and (listp l) (listp acc))
  :output-contract (listp (delete-t x l acc))
  (cond ((endp l)            acc)
        ((equal x (first l)) (delete-t x (rest l) acc))
        (t                   (delete-t x (rest l) (cons (first l) acc)))))

(defunc delete* (x l)
  :input-contract (listp l)
  :output-contract (listp (delete* x l))
  (rev (delete-t x l ())))


(defthm delete-t-lemma
  (implies (and (listp l) (listp acc))
           (equal (delete-t x l acc)
                  (app (rev (delete x l)) acc))))

(defthm hh
  (implies (and (listp m) (listp n))
            (equal (app (rev m)  (cons x n))
                   (app (rev (cons x m)) n))))

#|

and the following conjecture:

phi: (listp l) => (delete* x l) = (delete x l)

We prove this conjecture in three steps.

(a) Establish a lemma that relates delete-t and delete. Your lemma should
have the following form:

psi: (listp l) /\ (listp acc) => ((delete-t x l acc) = (app (rev (delete x l)) acc)



(b) Assuming psi is valid, prove phi using only equational reasoning.
c1: (listp l)
c2:(listp acc)
c3: (listp l)/\(listp acc) => (delete-t x l acc) = (app (rev (delete x l)) acc)
c4: (delete-t x l acc) = (app (rev (delete x l)) acc) {c1, c2}

c5:(listp (delete x l))
c6: (listp l)/\(listp acc)=>(rev (app acc (delete x l)))= (app (rev acc) (rev (delete x l)))
(the proof in class)

c7:(listp x)=> (rev (rev x)) = x 

LHS:
(delete* x l) 
={def delete*, c1}
= (rev (delete-t x l ()))
={c4, substitution(acc ())}
=(rev (app (rev (delete x l)) ()))
={c6,c1,c2}
=(app (rev (rev delete x l)) (rev ()))
={c7,def app, def rev}
=(app (delete x l) ())
=(delete x l)




RHS:
(delete x l)



the LHS is equal to RHS 


(c) Now prove psi using the IS suggested by delete-t. Note that this IS
gives rise to four proof obligations.

psi: (listp l) /\ (listp acc) => ((delete-t x l acc) = (rev (delete x l)))


1.(not (listp l))/\(not (listp acc))=>psi
2.(listp l)/\(listp acc)/\(endp l)=>psi
3.(listp l)/\(listp acc)/\(not (endp l))/\(equal x (first l))/\psi|(l (rest l))=>psi
4.(listp l)/\(listp acc)/\(not (endp l))/\(not (equal x (first l))/\psi|(l (rest l))=>psi

1.(not (listp l))/\(not (listp acc))/\(listp l)/\(listp acc)=>psi
=nil=>psi
=t

2.(listp l)/\(listp acc)/\(endp l)=>psi
c1:(listp l)
c2:(listp acc)
c3:(endp l)


(delete-t x l acc) 
={def delete, c1,c2,c3}
=acc


(app (rev (delete x l)) acc)
={def delete, c1,c2,c3}
=(app (rev ()) acc)
={def app, def rev}
=acc

3.(listp l)/\(listp acc)/\(not (endp l))/\(equal x (first l))/\psi|(l (rest l))=>psi
c1:(listp l)
c2:(listp acc)
c3:(not (endp l))
c4:(equal x (first l))
c5:(listp (rest l)) /\ (listp l)/\(listp acc) => ((delete-t x (rest l) acc) = (app (rev (delete x (rest l))) acc)
c6:((delete-t x (rest l) acc) = (app (rev (delete x (rest l))) acc)


(delete-t x l acc) 
={def delete, c1,c2,c3,c4}
=(delete-t x (rest l) acc)
={c6}
=(app (rev (delete x (rest l))) acc)


(app (rev (delete x l)) acc)
={def delete, c1,c2,c3,c4}
=(app (rev (delete x (rest l))) acc))


4.(listp l)/\(listp acc)/\(not (endp l))/\(not (equal x (first l))/\psi|(l (rest l))=>psi

c1:(listp l)
c2:(listp acc)
c3:(not (endp l))
c4:(not (equal x (first l)))
c5:(listp (rest l)) /\ (listp l)/\(listp acc) => ((delete-t x (rest l) acc) = (app (rev (delete x (rest l))) acc)
c6:((delete-t x (rest l) acc) = (app (rev (delete x (rest l))) acc)


(defthm hh
  (implies (and (listp m) (listp n))
            (equal (app (rev m)  (cons x n))
                   (app (rev (cons x m)) n))))
c7: (implies (and (listp m) (listp n))
            (equal (app (rev m)  (cons x n))
                   (app (rev (cons x m)) n)))
                   
                   
(delete-t x l acc) 
={def delete-t, c1,c2,c3,c4}
=(delete-t x (rest l) (cons (first l) acc))
={c6,substitution (acc (cons (first l) acc))}
=(app (rev (delete x (rest l))) (cons (first l) acc))
={c7}


(app (rev (delete x l)) acc)
={def delete, c1,c2,c3,c4}
=(app (rev (cons (first l) (delete x (rest l)))) acc)
={c7}

so LHS IS EQUAL TO RHS







|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

2. 

Consider the following function definition:

|#

(defunc exe (a n)
  :input-contract (and (rationalp a) (natp n))
  :output-contract (rationalp (exe a n))
  (if (equal n 0)
    1
    (* a (exe a (- n 1)))))

#|

(a) Prove the following conjecture phi by induction. Use the induction
scheme suggested by the function nind given in the lecture notes.

phi: (rationalp a) /\ (natp n) /\ (natp m) => (exe a n) * (exe a m) = (exe a n+m)



(defunc nind (n)
:input-contract (natp n)
:output-contract t
       (if (equal n 0)
            0
            (nind (- n 1))))
            
            
1.(not (natp n) )=>phi
2.(natp n)/\(equal n 0)/\(natp m)/\(equal m 0) =>phi
3.(natp n)/\(not (equal n 0))/\(equal m 0)/\phi|((n (n-1)))=>phi
4.(natp n)/\(not (equal m 0))/\(euqal n 0)/\phi|(m (m-1))=>phi
5.(natp m)/\(not (equal n 0))/\(not (equal m 0))/\phi|(n (n-1), m (m-1))=>phi


1.(not (natp n))=>phi
(not (natp n))/\(natp n)=>phi
= nil=>phi
=t

2.(natp n)/\(equal n 0)=>phi

(rationalp a) /\ (natp n) /\ (natp m) =>
            (exe a n) * (exe a m) = (exe a n+m)

c1:(rationalp a)
c2:(natp n)
c3:(natp m)
c4:(equal n 0)
c5:(equal m 0)

(exe a n)*(exe a m)
= {c1,c2,c3,c4,c5def exe}
=1*1

(exe a n+m)
={c1,c2,c3,c4,c5}
=(exe a 0)
={def exe}
=1


3.(natp n)/\(not (equal n 0))/\(equal m 0)/\phi|((n (n-1)))=>phi

c1:(natp n)
c2:(not (equal n 0))
c3:(equal m 0)
c4:(exe a (n-1))* (exe a m)= (exe a n+m)

(exe a n)* (exa a m)
={def exe, c1,c2,c3}
=(a* (exe a (n-1))) *(exe a m)
={c3}
=(a*(exe a (n-1)))*1

(exe a n+m)
={c3}
=(exe a n)
={c1,c2,def exe}
=(a* (exe a (n-1)))

4.natp n)/\(not (equal m 0))/\(euqal n 0)/\phi|(m (m-1))=>phi

c1:(natp n)
c2:(not (equal m 0))
c3:(equal n 0)
c4:(exe a (m-1))* (exe a n)= (exe a n+m)


(exe a n)* (exe a m)
=1* (a* (exe a (m-1)))

(exe a n+m)
={def exe ,c3}
=(exe a m)
={def exe, c1,c2}
=(a* (exe a (m-1)))


5.(natp m)/\(not (equal n 0))/\(not (equal m 0))/\phi|(n (n-1), m (m-1))=>phi

c0:(natp m)
c1:(natp n)
c2:(not (equal m 0))
c3:(not (equal n 0))
c4:(exe a (m-1))* (exe a (n-1)) = (exe a (n-1)+(m-1)) {c1,c2,c3,c0}
c5:(not (equal (n+m-1) 0)) {c1,c2,c0,c3}

(exe a n)* (exe a m)
= {c1,c2,c3,c4,def exe}
=(a* (exe a (n-1))) * (a* (exe a (m-1)))
={c4}
=(a* (a* (exe a (n-1)+(m-1))))


(exe a n+m)
={c4}
={c2,c3, n+m is not equal 0}
=(a* (exe a (n+m-1)))
={c2,c3, c5}
=(a* (a* (exe a (m+n-2))))







(b) Define a tail-recursive version exe-t of exe. Your function should have
the signature shown below. Argument acc is an accumulator that collects
intermediate results and, in the end, contains the final value.

|#

(defunc exe-t (a n acc)
  :input-contract (and (rationalp a) (natp n) (rationalp acc))
  :output-contract (rationalp (exe-t a n acc))
  (if (equal n 0)
      acc
      (exe-t a (- n 1) (* a acc))))

#|

(c) Define a function exe* with the same signature as exe such that
(exe* a n) calls exe-t and is intended to compute the same value as (exe a n).

|#

(defunc exe* (a n)
  :input-contract (and (rationalp a) (natp n))
  :output-contract (rationalp (exe* a n))
  (exe-t a n 1))

(defthm hm
    (implies (and (natp n) (rationalp acc) (rationalp a))
              (equal (exe-t a n acc) (* acc (exe a n)))))

#|

(d) Formulate a lemma psi that relates the value computed by
(exe-t a n acc) and the value computed by (exe a n). Your lemma should have
the following form:

psi :(rationalp a)/\(natp n)/\(rationalp acc) => (exe-t a n acc) = (* acc (exe a n)) 

Replace the ... by suitable hypotheses and ACL2 expressions. Function exe*
should not appear in the lemma.



(e) Assuming lemma psi above has been proved, prove the following claim
purely using equational reasoning. If you use psi, make sure to provide a
substitution.

phi:(rationalp a) /\ (natp n) => (exe* a n) = (exe a n)

c1:(rationalp a)
c2:(natp n)
c3:psi

LHS:
(exe* a n)
={def exe*, c1,c2}
=(exe-t a n 1)
=(psi substitution(acc 1), c1,c2,c3,and (rationalp acc)}
=(* 1 (exe a n))
={Arithmetic}
=(exe a n)

RHS:
(exe a n)


LHS= RHS



(f) Finally, prove psi by induction. Use the induction scheme suggested by
function exe-t.

IS: exe-t 
(rationalp a)/\(natp n)/\(rationalp acc) => (exe-t a n acc) = (* acc (exe a n)) 


1.(not (rationalp a))/\(not (natp n))/\(not (rationalp acc))=>psi
2.(rationalp a)/\(natp n)/\(rationalp acc)/\(n=0)=>psi
3.(rationalp a)/\(natp n)/\(rationalp acc)/\(not (n=0))/\psi|(n (-n 1))=>psi



1.(not (rationalp a))/\(not (natp n))/\(not (rationalp acc))/\(rationlp a)/\(nat p)/\(rationalp acc)=>psi
=nil=>psi=t


2.(rationalp a)/\(natp n)/\(rationalp acc)/\(n=0)=>psi
c1:(rationalp a)
c2:(natp n)
c3:(rationalp acc)
c4:(n=0)

(exe-t a n acc)
={def exe-t,c1,c2,c3,c4}
=acc

(* acc (exe a n)) 
={def exe. c1,c2,c3,c4}
=(* acc 1)
=acc



3.(rationalp a)/\(natp n)/\(rationalp acc)/\(not (n=0))/\psi|(n (-n 1))=>psi
c1:(rationalp a)
c2:(natp n)
c3:(rationalp acc)
c4:(not (n=0))
c5:(rationalp a)/\(natp (-n 1))/\(rationalp acc)/\(not (n-1)=0)=>(exe-t a (-n 1) acc) = (* acc (exe a (-n 1)) 
c6:(exe-t a (-n 1) acc) = (* acc (exe a (-n 1)) 

(exe-t a n acc)
={def exe-t,c1,c2,c3,c4}
=(exe-t a (-n 1) (* a acc))
={c6}
=(* acc (* a (exe a (-n 1))))


(* acc (exe a n)) 
={def exe. c1,c2,c3,c4}
=(* acc (*a (exe a (- n 1))))

LHS = RHS
|#
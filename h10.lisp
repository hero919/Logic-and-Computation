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

CS 2800 Homework 10 - Fall 2014

Student names: This homework is done in pairs. Put BOTH names here.


Zeqing Zhang && Yueling Qin




Technical instructions:

- open this file in ACL2s BEGINNER mode as hw10.lisp

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish some problems, comment
  them out. The same is true for any English text that you may add. This
  file already contains many comments, so you can see what the syntax is.

- when done, save your file and submit it as hw10.lisp.

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Induction Proofs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

1. Consider the following function (you proved it terminating in HW06):

(defunc count (i j c)
  :input-contract (and (natp i) (natp j) (natp c))
  :output-contract (natp (count i j c))
  (cond ((and (equal i 0) (equal j 0)) c)
        ((> j 0)                       (count i (- j 1) (+ c 1)))
        (t                             (count (- i 1) 9 (+ c 1)))))

You can assume that this function is admissible (which is true).

(a) Find out what count does, e.g. by evaluating count on some input
values, including the following:

(count 1 6 0) = 16
(count 2 6 0) = 26
(count 1 6 1) = 17
(count 1 6 2) = 18
(count 1 10 0) = 20
(count 1 2 0) = 12

Now conjecture a closed formula (one whose evaluation does not require
recursion) for (count i j c):

phi: (natp i) /\ (natp j) /\ (natp c) => (count i j c) = (+ (* i 10) j c)

(b) Prove conjecture phi using induction. Use the induction scheme suggested by count!

i. ~ ((natp i) /\ (natp j) /\ (natp c)) => (count i j c) = (+ (* i 10) j c)
nil => ~
Since nil implies any thing is true, the statement is always true.

ii. (natp i) /\ (natp j) /\ (natp c) /\ (and (equal i 0) (equal j 0)) =>
(count i j c) = (+ (* i 10) j c)

LHS: (count 0 0 c) 
= { def count }
c

RHS: (+ (* 0 10 ) 0 c)
= {Axiom}
 c
 
LHS = RHS 

It is true.

iii. (natp i) /\ (natp j) /\ (natp c) /\ ~(and (equal i 0) (equal j 0)) /\ (> j 0) /\ phi| (j j-1)(c c+1)
=> (count i j c) = (+ (* i 10) j c)

c1: (natp i)
c2: (natp j)
c3: (natp c)
c4: ~(and (equal i 0) (equal j 0))
c5:(> j 0)
c6:phi| (j j-1)(c c+1)




LHS: (count i j c)
= {def count}
(count i (- j 1) (+ c 1))
{c1,c2,c3,c6}
= (+ (* i 10) j c)


RHS: (+ (* i 10) (- j 1) (+ c 1))
= (+ (* i 10) j c)

LHS = RHS
 
 It is true.

iv. (natp i) /\ (natp j) /\ (natp c) /\ ~(and (equal i 0) (equal j 0)) /\ ~(> j 0) /\ phi|(i (i-1)) (j 9) (c (c+1))
=> (count i j c) = (+ (* i 10) j c)
c1: (natp i)
c2: (natp j)
c3: (natp c)
c4: ~(and (equal i 0) (equal j 0))
c5:~(> j 0)


LHS: (count i j c)
={The definition of count}
(count (- i 1) 9 (+ c 1))
=(+ (* (- i 1) 10) 9 (+ c 1))
=(+ (* i 10) j c)



RHS:(+ (* i 10) j c)

LHS = RHS

It is true.











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

2. Consider:

(defunc in (x l)
  :input-contract (listp l)
  :output-contract (booleanp (in x l))
  (cond ((endp l)            nil)
        ((equal x (first l)) t)
        (t                   (in x (rest l)))))

(defunc subsetp (l1 l2)
  :input-contract (and (listp l1) (listp l2))
  :output-contract (booleanp (subsetp l1 l2))
  (if (endp l1)
    t
    (and (in (first l1) l2)
         (subsetp (rest l1) l2))))

(defunc union (l1 l2)
  :input-contract (and (listp l1) (listp l2))
  :output-contract (listp (union l1 l2))
  (if (endp l1)
    l2
    (cons (first l1) (union (rest l1) l2))))

(defunc intersect (l1 l2)
  :input-contract (and (listp l1) (listp l2))
  :output-contract (listp (intersect l1 l2))
  (cond ((endp l1)          nil)
        ((in (first l1) l2) (cons (first l1) (intersect (rest l1) l2)))
        (t                  (intersect (rest l1) l2))))

(a) Prove the following theorem.

phi_a: (listp l1) /\ (listp l2) => (subsetp l1 (union l1 l2))

i.(not (listp l1)) => phi_a: trivial

ii. (listp l1) /\ (listp l2) /\ (endp l1) => (subsetp l1 (union l1 l2))
c1:(listp l1)
c2:(listp l2) 
c3:(endp l1)
c4:(endp l2)
(subsetp l1 (union l1 l2))
= {The definition of subsetp, c3}
t
It is true.


iii. (listp l1) /\ (listp l2) /\ ~(endp l1) /\ phi_a|(l (rest l)) => (subsetp l1 (union l1 l2))
c1:(listp l1)
c2:(listp l2) 
c3:~(endp l1)
c4:(endp l2)
(subsetp l1 (union l1 l2))
= {The definition of union, c1,c2,c3}
(subsetp l1 l1)
= {The definition of subsetp, c1, c2}
t
It is true.


iii. (listp l1) /\ (listp l2) /\ ~(endp l1) /\ ~(endp l2) => (subsetp l1 (union l1 l2))
= { The definition of union}
(subsetp l1 (cons (first l1) (union (rest l1) l2)))
= { The definition of subsetp}
(and (in (first l1) (cons (first l1) (union (rest l1) l2)))
         (subsetp (rest l1) (cons (first l1) (union (rest l1) l2))))
= {The definition of in}

(and (equal (first l1) (first l)) (subsetp (rest l1) (cons (first l1) (union (rest l1) l2))))
= (and t (subsetp (rest l1) (cons (first l1) (union (rest l1) l2))))
Since it equals to phi_a|(l (rest l))
It is true




(b) Prove the following theorem.

phi_b: (listp l1) /\ (listp l2) /\ (subsetp l1 l2) => (intersect l1 l2) = l1

i. ~((listp l1) /\ (listp l2) /\ (subsetp l1 l2)) => (intersect l1 l2) = l1
={trivial}
true


ii. (listp l1) /\ (listp l2) /\ (subsetp l1 l2) /\ (endp l1) => (intersect l1 l2) = l1
c1:(listp l1)
c2:(listp l2)
c3:(subsetp l1 l2)
c4:(endp l1)

LHS:
(intersect l1 l2)
{the definition of intersect,c4}
= nil

RHS: l1 = nil

LHS = RHS

iii. (listp l1) /\ (listp l2) /\ (subsetp l1 l2) /\ ~(endp l1) /\ phi_b|(l1 (rest l1))
=> (intersect l1 l2) = l1
c1:(listp l1)
c2:(listp l2)
c3:(subsetp l1 l2)
c4:~(endp l1)
c5:phi_b|(l1 (rest l1))

LHS:
(intersect l1 l2) 
= {The definition of intersect},c3}
(cons (first l1) (intersect (rest l1) l2))
= {c5}
(cons (first l1) (rest l1)
= l1


RHS: l1

l1=l1

It is true.



(c) Prove the following theorem.

phi_c: (listp l1) /\ (listp l2) /\ (subsetp l1 l2) => (intersect l1 (union l1 l2)) = l1

i. (~ (listp l1) /\ (listp l2) /\ (subsetp l1 l2)) => (intersect l1 (union l1 l2)) = l1

{trivial}

= t

ii. (listp l1) /\ (listp l2) /\ (subsetp l1 l2) /\ (endp l1) => (intersect l1 (union l1 l2)) = l1

c1:(listp l1)
c2:(listp l2)
c3: (subsetp l1 l2)
c4: (endp l1)

LHS: (intersect l1 (union l1 l2))
= { the definition of intersect, c4}
nil


RHS: l1 
={c4}
nil

LHS = RHS

It is true.

iii. (listp l1) /\ (listp l2) /\ (subsetp l1 l2) /\ ~(endp l1) /\ phi_c|(l1 (rest l1))
=> (intersect l1 (union l1 l2))
c1:(listp l1)
c2:(listp l2)
c3: (subsetp l1 l2)
c4: ~(endp l1)
c5: phi_c|(l1 (rest l1))

(intersect l1 (union l1 l2))
{the definition of union, c3}
= (intersect l1 l2)
{c3}
= t 








|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vagueness of Specifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

3. In this problem you will be using the acl2::test? mechanism. This
function takes a conjecture as input and runs a number of tests to see
whether it can find a counterexample. For example, let us conjecture that,
for positive natural numbers x and y, (x + y) and (x * y) are different.
You can test this conjecture like this:

(acl2::test? (implies (and (posp x) (posp y)) (not (equal (+ x y) (* x y)))))

Test? may report that it succeeded: no counterexamples were found. This
does not mean you have proved the conjecture. It just means test? hasn't
come across a test that falsifies it. In fact, the above conjecture is
wrong. You can try to find more counterexamples by increasing the number of
tests run, like this:

(acl2::acl2s-defaults :set acl2::num-trials 200)

Now try test? again.

In general, make sure to perform conjecture contract checking before
passing your conjecture to acl2::test? . For example,

(acl2::test? (equal (app x y) (app y x)))

will waste time running many useless tests in which x and y are not
lists, and report contract violations. In contrast,

(acl2::test? (implies (and (listp x) (listp y)) (equal (app x y) (app y x))))

will return a counterexample using lists immediately.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Suppose you are asked to write a function that computes the union of two
sets, where a set is represented using a list. The union of two sets
consists of all elements that are contained in at least one of the input
sets. Consider the following definitions.

(defdata set (listof all))

(defunc union1 (s1 s2)
  :input-contract (and (setp s1) (setp s2))
  :output-contract (setp (union1 s1 s2))
  (app s1 s2))

(defunc union2 (s1 s2)
  :input-contract (and (setp s1) (setp s2))
  :output-contract (setp (union2 s1 s2))
  (app (rev s1) (rev s2)))

(defunc union3 (s1 s2)
  :input-contract (and (setp s1) (setp s2))
  :output-contract (setp (union3 s1 s2))
  (if (endp s2)
      s1
    (cons (first s2) (union3 s1 (rest s2)))))

(defunc union4 (s1 s2)
  :input-contract (and (setp s1) (setp s2))
  :output-contract (setp (union4 s1 s2))
  (if (or (endp s1) (endp s2))
      nil
    (cons (first s1) (cons (first s2) (union4 (rest s1) (rest s2))))))

Note that all definitions union[1-4] have the same signature and input and
output contracts.

(a) Prove that these four solutions are pairwise different. That is, for
each pair of functions union[1-4] , find an input (s1 s2) on which the two
functions produce different output. You therefore need to produce 6
examples (there are 6 pairs). Consider using test? as an assistant.

union1 and union2:
(s1 (nil 0)) and (s2 (2 nil t))

union1 and union3:
(s1 (#\b T)) and (s2 (T))

union1 and union4:
(s1 nil nil) and (s2 nil t)

union2 and union3:
(s1 t nil) and (s2 a t)

union2 and union4:
(s1 nil) and (s2 (4))

union3 and union4:
(s1 ((-1) 0)) and (s2 nil)

(b) Function union1 satisfies the specification "union of two sets" given
above: "The union of two sets consists of..." (you don't need to prove
that). What about the others? For union2, union3, union4, if you suspect it
does, say so. If you suspect it doesn't, give a counterexample (how you
find it is up to you).

union2: Yes. It is the same as union1 only reverse the elemensts.

union3: Yes. It is (append s2 s1). The same as union1. 

union4: No. It can fail in (union4 nil '(0))

(c) For each of the functions identified under (b) to _fail_ the union
specification, let's see whether we can say more. Use test? to test the
following conjectures:

(in x (union1 s1 s2)) => (in x (union? s1 s2))
(in x (union? s1 s2)) => (in x (union1 s1 s2))

where union? is one of the functions identified in (b) to fail the
specification (and union1 is the first function, which is known to satisfy
the specification).

What do these conjectures state? Formulate in English.

The first formula said if x in the union1 of s1 and s2, the x also should be
be in the union? of s1 and s2.

The Second formula said if x in the union? of s1 and s2, the x also should be
be in the union1 of s1 and s2.

For your testing, remember to perform conjecture contract checking, and add
proper hypotheses. We are not interested in contract violations.

If ACL2s can't find a counterexample, make it try harder by adjusting the
number of trials.

(acl2::acl2s-defaults :set acl2::num-trials 1000)

(acl2::test? (implies (and (setp s1) (setp s2)) (implies (in x (union1 s1 s2)) (in x (union4 s1 s2)))))
(acl2::test? (implies (and (setp s1) (setp s2)) (implies (in x (union4 s1 s2)) (in x (union1 s1 s2)))))

(d) You have identified in (b) a number of functions to _satisfy_ the union
specification, but according to (a) they are still different. Let's see
whether we can be more precise. Formulate a conjecture about the
relationship of the result returned by union1 (which is "correct") and any
of the other functions deemed to be "correct". Of course, one such
conjecture is that they all implement "union". We are looking for a more
precise conjecture that looks at the representation of the "union" as a
list of elements.

For extra credit, write functions that allow you to test your conjecture
using test?.

(equal (union1 s1 s2) (union? s1 s2))

(acl2::test? (implies (and (setp s1) (setp s2)) (equal (union1 s1 s2) (union? s1 s2))))


|#




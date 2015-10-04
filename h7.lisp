; **************** BEGIN INITIALIZATION FOR ACL2s BB MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);

#|
Pete Manolios
Fri Jan 27 09:39:00 EST 2012
----------------------------

Made changes for spring 2012.

Pete Manolios
Thu Jan 20 08:50:00 EST 2011
----------------------------

The idea of the Bare Bones level is to introduce ACL2 as a
programming language with contracts (a "typed" ACL2) to the
students, using a "minimal" subset of primitive functions.
For example, in the case of the Booleans, all that is built-in
are the constants t and nil and the functions if and equal.

Everything else is built on top of that. 

|#
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the TRACE* book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
; only load for interactive sessions: 
#+acl2s-startup (include-book "trace-star" :uncertified-okp nil :dir :acl2s-modes :ttags ((:acl2s-interaction)) :load-compiled-file nil);v4.0 change

#+acl2s-startup (assign evalable-printing-abstractions nil)

;arithmetic book
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading arithmetic-5/top book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "arithmetic-5/top" :dir :system)

;basic thms/lemmas about lists
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading coi/lists book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "coi/lists/basic" :dir :system)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2's lexicographic-ordering-without-arithmetic book.~%This indicates that either your ACL2 installation is missing the standard books are they are not properly certified.") (value :invisible))
(include-book "ordinals/lexicographic-ordering-without-arithmetic" :dir :system)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "ccg" :uncertified-okp nil :dir :acl2s-modes :ttags
              ((:ccg)) :load-compiled-file nil);v4.0 change

;#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the EVALABLE-LD-PRINTING book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
; only load for interactive sessions: 
;#+acl2s-startup (include-book "evalable-ld-printing" :uncertified-okp nil :dir :acl2s-modes :ttags ((:acl2s-interaction)) :load-compiled-file nil);v4.0 change

;; #+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading DataDef+RandomTesting book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
;; (include-book "countereg-gen/top" :uncertified-okp nil :dir :system :load-compiled-file :comp)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "custom" :dir :acl2s-modes :uncertified-okp nil
                                         :load-compiled-file
                                         :comp :ttags :all)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s Bare Bones mode.") (value :invisible))

;Settings common to all ACL2s modes
(acl2s-common-settings)

; Non-events:
(acl2::set-guard-checking :all)

(defconst *testing-upper-bound* 50)

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

(defun nth-small-complex-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-complex-rational n-small)))

(defun nth-small-all (n)
  (declare (xargs ;:guard (natp n) ))
                  :verify-guards nil))
  (mv-let (choice seed)
          (defdata::weighted-switch-nat 
            '(1  ;nil
              1  ;t
              1 ;0
              1 ;integer
              1  ;bool
              1  ;charlist
              1  ;sym
              1  ;string
              2  ;char
              1  ;acl2-num
              5 ;rational
              5 ;nat
              5 ;pos
              5  ;rational-list
              2  ;sym-list
              20 ;cons-atom
              5  ;nat-list
              10  ;cons-cons-atom
              1  ;stringlist
              10  ;atom-list
              ) n)
        
    (case choice
          (0 'nil)
          (1 't)
          (2 0)
          (3 (nth-small-integer-testing seed))
          (4 (nth (mod seed 2) *boolean-values*))
          (5 (nth-character-list seed))
          (6 (nth-symbol seed))
          (7 (nth-string seed))
          (8 (nth (mod seed (len *character-values*)) *character-values*))
          (9 (nth-small-acl2-number-testing seed))
          (10 (nth-small-rational-testing seed))
          (11 (nth-small-nat-testing seed))
          (12 (nth-small-pos-testing seed))
          (13 (nth-rational-list seed))
          (14 (nth-symbol-list seed))
          (15 (nth-cons-atom seed))
          (16 (nth-nat-list seed))
          (17 (nth-cons-ca-ca seed))
          (18 (nth-string-list seed))
          (19 (nth-atom-list seed))
          (t 'nil)))) ;this case should not come up


(defdata-testing pos :test-enumerator nth-small-pos-testing)
(defdata-testing integer :test-enumerator nth-small-integer-testing)
(defdata-testing nat :test-enumerator nth-small-nat-testing)
(defdata-testing neg :test-enumerator nth-small-neg-testing)
(defdata-testing positive-ratio :test-enumerator nth-small-positive-ratio-testing)
(defdata-testing negative-ratio :test-enumerator nth-small-negative-ratio-testing)
(defdata-testing rational :test-enumerator nth-small-rational-testing)
(defdata-testing positive-rational :test-enumerator nth-small-positive-rational-testing)
(defdata-testing negative-rational :test-enumerator nth-small-negative-rational-testing)
(defdata-testing acl2-number :test-enumerator nth-small-acl2-number-testing)
(defdata-testing complex-rational :test-enumerator nth-small-complex-rational-testing)
(defdata-testing all :test-enumerator nth-small-all)

(acl2s-defaults :set num-trials 30)

(defpkg "ACL2S BB" ; bare bones
  (union-eq '(t nil 
              ;if ; see macro below
              equal

              defun defunc ;for function definitions

              ; + * unary-- unary-/ < ;see definitions below
              numerator denominator
              rationalp integerp
              
              consp cons  

              cond ; macro: explain
              ;list ; harshrc [21st Aug 2012] commented out to allow (defdata list ...) below

              lambda
              let let* ; macro: explain

              quote

              symbolp symbol-name symbol-package-name
              ;stringp
              ;charp

              check=
              
              trace*
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




(acl2::in-package "ACL2S BB")

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

(defunc first (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::car x))

(defunc rest (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::cdr x))

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
                          (equal (acl2::car term) 'acl2::/))
               (cons '/ (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::car))
               (cons 'first (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::cdr))
               (cons 'rest (acl2::cdr term)))
              
              ;; Due to a call to translate in get-free-vars in testing
              ;; code, the following functions/macros that have not been
              ;; defined in bare-bones should not be
              ;; pre-processed. --harshrc Jan 27 2012
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::implies))
              ;;  (cons 'implies (acl2::cdr term)))
              
              ;; ((acl2::and (consp term)
              ;;             (consp (acl2::cdr term))
              ;;             (equal (acl2::car term) 'acl2::not)
              ;;             (equal (acl2::caadr term) 'acl2::>))
              ;;  (cons '<= (acl2::cdadr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::not))
              ;;  (cons 'not (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::*))
              ;;  (cons '* (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::+))
              ;;  (cons '+ (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::/))
              ;;  (cons '/ (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::-))
              ;;  (cons '- (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::<))
              ;;  (cons '< (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::>))
              ;;  (cons '> (acl2::cdr term)))
              ;; ((acl2::and (consp term)
              ;;             (equal (acl2::car term) 'acl2::acl2-number))
              ;;  (cons 'acl2-number (acl2::cdr term)))
              (t nil)))

; A hack to help proofs go through in this mode.
(acl2::in-theory (acl2::enable rest))

;(acl2::verify-guards acl2::nth-true-list)

; harshrc 29 March 2012 -- added nth-list for Pete
;(acl2::defdata |ACL2S BB|::list acl2::true-list)
(defun listp (x) (acl2::declare (acl2::xargs :guard T)) (acl2::true-listp x))
(defun nth-list (n) (acl2::declare (acl2::xargs :guard (acl2::natp n))) (acl2::nth-true-list n))
(acl2::register-custom-type list t nth-list listp)

(COMMON-LISP::DEFMACRO |ACL2S BB|::LIST (COMMON-LISP::&REST ACL2::ARGS)
                       (ACL2::LIST-MACRO ACL2::ARGS))

(acl2::table acl2::user-defined-functions-table
             'acl2::untranslate-preprocess
             'my-preprocess)

;;Settings specific to this mode(copied from R&I mode)
(acl2::in-package "ACL2")
(set-irrelevant-formals-ok :warn)
(set-bogus-mutual-recursion-ok :warn)
(set-ignore-ok :warn)

(set-backchain-limit '(50 100))
(set-rewrite-stack-limit 500)
(acl2::acl2s-defaults :set acl2::subgoal-timeout 60)
(acl2::acl2s-defaults :set acl2::defunc-timeout 200) 


;for beginner users dont be strict in admitting defunc
;(acl2::acl2s-defaults :set acl2::defunc-strict 0)  
(acl2s-defaults :set num-trials 500)

;(assign evalable-ld-printingp t)
;(assign evalable-printing-abstractions '(list cons))
;(assign triple-print-prefix "; ")

(cw "~@0Bare Bones mode loaded.~%~@1"
    #+acl2s-startup "${NoMoReSnIp}$~%" #-acl2s-startup ""
    #+acl2s-startup "${SnIpMeHeRe}$~%" #-acl2s-startup "")

(acl2::in-package "ACL2S BB")


; ***************** END INITIALIZATION FOR ACL2s BB MODE ******************* ;

;$ACL2s-SMode$;Bare Bones
#|

CS 2800 Homework 7 - Fall 2014

Student names: Zeqing Zhang & Yueling Qin

Technical instructions:

- open this file in ACL2s BEGINNER mode as hw07.lisp

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish some problems, comment
  them out. The same is true for any English text that you may add. This
  file already contains many comments, so you can see what the syntax is.

- when done, save your file and submit it as hw07.lisp.

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Conjecture contract checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

The following conjectures are incomplete (irrespective of whether they are
true or false): they are lacking hypotheses that make sure the functions
involved are called only on arguments that satisfy the functions' input
contracts. Make the conjectures meaningful by adding the necessary contract
hypotheses. For example, given a conjecture c, if you think hypotheses h1
and h2 (and nothing else) are necessary to ensure the input contracts of
all functions occurring in c are satisfied, your solution should be

h1 /\ h2 => c

Simplify the conjecture hypotheses as much as possible. In the above
example, suppose h1 => h2 is valid. Then the conjunction h1 /\ h2 is
equivalent to h1 ; thus, simplify your output to

h1 => c

The examples below may contain some functions for which we only know the
signature, and known functions such as + . The input contracts of _all_
functions need to be ensured by the hypotheses.

(a) Given

foo: List x List -> List
bar:  All x List -> List

the conjecture c is

  (foo (bar (+ x y) l) z)   =   (bar (+ y x) (foo l z))

Conjecture with hypotheses:


h1 is
(and (rationalp x) (rationalp y) (listp l) (listp z))

h1 => c

(implies (and (rationalp x) (rationalp y) (listp l) (listp z))
         (equal (foo (bar (+ x y) l) z) (bar (+ y x) (foo l z))))



(b) Consider the following two definitions, the first of which gives rise
to a recognizer natlistp:

|#

(defdata natlist (listof nat))

(defunc evenlistp (l)
  :input-contract t
  :output-contract (booleanp (evenlistp l))
  (if (listp l)
    (integerp (/ (len l) 2))
    nil))

#|

Given

d: List -> All
m: Nat x List -> NatList
f: List -> Nat
s: EvenList -> List

the conjecture c is

(d (m x l)) = (f (s l))

Conjecture with hypotheses:

h1 is
(and (natp x) (Evenlistp l))

h1 => c

(implies (and (natp x) (Evenlistp l))
         (equal (d (m x l)) (f (s l))))

Now define four functions d, m, f, s (as simple as possible) with the above
signatures. The actual outputs of the functions are arbitrary, as long as
they satisfy the output contract.

|#

(defunc d (a)
  :input-contract (listp a)
  :output-contract t
  1)


(defunc m (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract (natlistp (m n l))
  (list 1))

(defunc f (l)
  :input-contract (listp l)
  :output-contract (natp (f l))
  1)

(defunc s (l)
  :input-contract (evenlistp l)
  :output-contract (list p)
  l)



; Suppose we want to write a function that tests our conjecture:

(defunc dummy (x l)
  :input-contract (and (natp x) (evenlistp l))
  :output-contract (booleanp (dummy x l))
  (equal (d (m x l)) (f (s l))))

#|

Complete the function definition. That is, replace the first ... with the
hypotheses you found, and the second ... with an appropriate output
contract. Make sure this definition is accepted by ACL2! Test this function
on at least one input.

|#

(check= (dummy 1 (list 1 2))
    t)
(check= (dummy 2 (list 2 3)) t)

(check= (dummy 2 nil) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Equational reasoning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Instructions:

- perform conjecture contract checking, and add hypotheses if necessary

- run some tests to make an educated guess as to whether the conjecture is
  true or false. In the latter case, find a counterexample to the
  conjecture. In the former case, continues as below.

- use propositional reasoning to rewrite the conjecture into an implication
  with a conjunction of hypotheses in the antecedent and a single
  expression in the conclusion. If that is not possible, you may have to
  split the proof into several parts such that each part can be rewritten
  into such an implication.

- for each implication, extract the context (the hypotheses), and determine
  the derived context: anything that follows immediately from the context
  expressions and may be useful later.

- now perform the proof.

When writing your equational reasoning proofs be sure to justify each step
in the style shown in class, eg.

  (len ())
= { def len }
  0

You can use basic arithmetic facts for free, but in the justification write
"arithmetic", e.g.,

  (first x) + (len (rest x)) + (sum y) + 0
= { Arithmetic }
  (sum y) + (first x) + (len (rest x))

You may use infix notation like x+y+z for arithmetic operators (as done
above), instead of the LISP style prefix notation like (+ x (+ y z)).

You can of course also use previously (in class or in homework) proved
theorems. In this case, cite the theorem in the justification, and give the
substitution that shows how you instantiated the theorem.

Here are the definitions used for the remainder of the questions. (Note:
these may be different from earlier definitions of these functions -- what
counts are the definitions provided here.)

(defunc atom (x)
  :input-contract t
  :output-contract (booleanp (atom x))
  (not (consp a)))

(defunc endp (a)
  :input-contract (listp a)
  :output-contract (booleanp (endp a))
  (not (consp a)))

(defunc listp (l)
  :input-contract t
  :output-contract (booleanp (listp l))
  (or (endp l) (consp l)))

(defunc len (x)
  :input-contract (listp l)
  :output-contract (natp (len x))
  (if (endp x)
      0
    (+ 1 (len (rest x)))))

(defunc twice (l)
  :input-contract (listp l)
  :output-contract (listp (twice l))
  (if (endp l)
    nil
    (cons (first l) (cons (first l) (twice (rest l))))))

(defunc app (a b)
  :input-contract (and (listp a) (listp b))
  :output-contract (and (listp (app a b))
                        (equal (len (app a b))
                               (+ (len a) (len b))))
  (if (endp a)
    b
    (cons (first a) (app (rest a) b))))

(defunc rev (x)
  :input-contract (listp x)
  :output-contract (and (listp (rev x))
                        (equal (len (rev x))
                               (len x)))
  (if (endp x)
    nil
    (app (rev (rest x)) (list (first x)))))

(defunc sum (l)
  :input-contract (natlistp l)
  :output-contract (natp (sum l))
  (if (endp l)
      0
    (+ (first l) (sum (rest l)))))

Recall that for each of the defunc's above we have both a definitional axiom

(ic => (f <args>) = <function body>)

(you can refer to it in justifications as "def. f"), and a contract theorem

(ic => oc).

Definitional axioms and contract theorems of admitted functions are
available for you to use.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For the following conjectures, follow all proof steps outlined above.

(a)

(implies (and (listp x)
              (listp y))
         (equal (len (app x y))
                (len (app y x))))

Hint: carefully observe (and use) the involved functions' contracts.
a: (listp x)
b: (listp y)
c: (len (app x y))
d: (len (app y x))

(a/\b)=>(c=d)

(listp x)/\(listp y) => (equal (len (app x y)) (len (app y x)))

c1: (listp x) 
c2: (listp y)
devided
{c1, c2, def app}
c3: (listp (app x y)) /\ ((len (app x y))= ((len x) + (len y))) 

{c1, c2, def app}
c4: (listp (app y x)) /\ ((len (app y x))= (len y)+ (len x))

LHS:
(len (app x y))
= {c3}
  (len x) + (len y)

  
RHS:
(len (app y x))
={c4}
  (len y) + (len x)

so, we can get LHS = RHS


...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(b)

(implies (and (listp x)
              (listp y))
     (equal (app (rev x) (rev y))
        (rev (app x y))))
a: (listp x)
b: (listp y)
c: (app (rev x) (rev y))
d: (rev (app x y))


(a/\b)=>(c=d)

(listp x) /\(listp y) => (equal (app (rev x) (rev y)) (rev (app x y)))

(b) is false. we can provide a count-example.
(app (rev (list 1 2)) (rev (list 4 5)))=(2 1 5 4)

(rev (app (list 1 2) (list 4 5)))=(5 4 2 1)

the result is not same, so it is false.

...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c)

(implies (and (listp x)
          (listp y))
     (equal (len (app (rev x) (rev y)))
        (len (rev (app x y)))))

a: (listp x)
b: (listp y)
c: (len (app (rev x) (rev y)))
d: (len (rev (app x y)))

(a/\b)=>(c=d)
(listp x)/\(listp y)=>(len (app (rev x) (rev y)))= (len (rev (app x y)))
c1: (listp x) 
c2: (listp y)
devided
{c1,c2, def rev}
c3:(listp (rev x))/\((len (rev x))= (len x))

{c1,c2, def rev}
c4:(listp (rev y))/\((len (rev y))= (len y))

{c3,c4,c1,c2, def app}
c5:(listp (app (rev x) (rev y)))/\((len (app (rev x) (rev y)))
        = (len (rev x))+ (len (rev y)))
        
{c1, c2, def app}
c6: (listp (app x y))/\ ((len (app x y)) = (len x) + (len y))

{c6, def rev}
c7:(listp (rev (app x y))) /\((len (rev (app x y))) = (len (app x y)))


LHS:
(len (app (rev x) (rev y)))
={c5}
 (len (rev x))+ (len (rev y))
={c3,c4}
 (len x)+ (len y)
 
RHS:
(len (rev (app x y)))
={c7}
 (len (app x y))
={c6}
 (len x) + (len y)
 
so, we can get LHS=RHS




...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(d)

(implies (and (listp x)
              (listp y)
              (listp z))
     (implies (equal (twice (app      x     z)) (app (twice      x    ) (twice z)))
          (equal (twice (app (cons x y) z)) (app (twice (cons x y)) (twice y)))))
a: (listp x)
b: (listp y)
c: (listp z)
d: (twice (app x z))
e: (app (twice x) (twice z))
f: (twice (app (cons x y) z))
g: (app (twice (cons x y)) (twice y))


(a/\b/\c)=>((d=e)=>(f=g))
a/\b/\c/\(d=e)=>(f=g)
(listp x)/\(listp y)/\(listp z)/\((twice (app x z))=(app (twice x) (twice z)))
     => ((twice (app (cons x y) z))=(app (twice (cons x y)) (twice y)))
     
c1: (listp x)
c2: (listp y)
c3: (listp z)   
c4: (twice (app x z))=(app (twice x) (twice z))


D it is false, we can give a count-exmple,
x is (list 1) 
y is ()
z is (list 2)

(app (cons (list 1) ()) (list 2))=> (list (list 1) 2)
(twice (list (list 1) 2))=> (list (list 1) (list 1) 2 2)=((1) (1) 2 2)



(app (twice (cons (list 1) ())) (twice ()))=> ((1) (1))

the result is different, so it is false,

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e)

(implies (listp x)
         (and (implies (endp x)
                       (equal (len (twice x))
                              (* 2 (len x))))
              (implies (and (not (endp x))
                            (equal (len (twice (rest x)))
                                   (* 2 (len (rest x)))))
                       (equal (len (twice x))
                              (* 2 (len x))))))

a: (listp x)
b: (endp x)
c: (len (twice x)) = (* 2 (len x))
d: (not (endp x))
e: (len (twice (rest x))) = (* 2 (len (rest x)))
f: (len (twice x))= (* 2 (len x))


a=>((b=> c)/\((d/\e)=>f))




if we make a be A
b=>c be B, 
d/\e be C,
f be D,
so, we have:
A=> (B/\ (C=>D))

we can change to:

(A=> B)/\(A=> (C=>D))
(A=> B)/\(A/\C=>D), we can proof A=>B, AND (A/\C=>D)

(A=>B): (a=> (b=>c)): (a/\b=>c): 
(listp x)/\(endp x)=> (len (twice x))=(* 2 (len x))

(A/\C=>D): a/\(d/\e) => f
(listp x)/\(not (endp x))/\ ((len (twice (rest x))) = (*2 (len (rest x))))
=> (len (twice x))= (* 2 (len x))


so, the statement become to:
((listp x)/\(endp x)=> (len (twice x))=(* 2 (len x))) /\
((listp x)/\(not (endp x))/\ ((len (twice (rest x))) = (*2 (len (rest x))))
=> (len (twice (rest x))) = (* 2 (len (rest x)))=(len (twice x)) = (* (len x)))




First part: (listp x)/\(endp x)=> (len (twice x))=(* 2 (len x))

c1: (listp x) 
c2: (endp x)

LHS:
(len (twice x))
={c2, c1, def twice}
 (len nil)
={def len, c2}
 (len nil)
={def len}
 0
 
 
RHS:
(* 2 (len x))
= {c1, c2, def len}
  (*2 0)
={Arithmetics}
  0
  
so, the result of LHS is same with RHS

second part: (listp x)/\(not (endp x))/\ ((len (twice (rest x))) = (*2 (len (rest x))))
=> (len (twice x))= (* 2 (len x))

c1:(listp x)
c2:(not (endp x))
c3:(len (twice (rest x)))= (*2 (len (rest x)))

LHS:
(len (twice x))
={c1, c2, def twice}
 (len (cons (first x) (cons (first x) (twice (rest x)))))
={def len, c1, c2, axiom if, rest}
 (+1 (len (cons (first x) (twice (rest x)))))
={def len, c1, c2, axiom if, rest}
 (+1 (+ 1 (len (twice (rest x)))))
= {c3}
 (+1 (+ 1 (* 2 (len (rest x)))))
={Arithmetic}
 (+ 2 (* 2 (len (rest x))))
=(* 2 (+ 1 (len (rest x))))
={axiom rest, def len}
=(*2 (len x))

so, we proof the LHS is equal to RHS.

 

 



...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following conjectures are true; prove them following the instructions outlined above.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(f)

(implies (and (natlistp x) (natlistp l))       
     (and (implies (endp x)
               (equal (sum (app x l)) (+ (sum x) (sum l))))
          (implies (and (natp a)
                (equal (sum (app x l)) (+ (sum x) (sum l))))
               (equal (sum (app (cons a x) l)) (+ (sum (cons a x)) (sum l))))))

               
a: (natlistp x)
b: (natlistp l)
c: (endp x) 
d: (sum (app x l)) = (sum x) + (sum l)
e: (natp a)
f: (sum (app (cons a x) l)) = (sum (cons a x)) + (sum l)


(a/\b)=> ((c=>d)/\((e/\d)=> f))
if we make:
a/\b be A,
c=>d be B,
e/\d be C,
f be D,

we can write statement to:
A=> (B/\ (C=>D))

simplify:
(A=> B )/\ (A=> (C=>D))

first part:
A=> B: (a/\b)=> (c=>d): (a/\b)/\c=> d
(natlistp x)/\(natlistp l)/\(endp x) => (sum (app x l)) = (sum x) + (sum l)

c1: (natlistp x)
c2: (natlistp l)
c3: (endp x)

LHS: 
(sum (app x l))
={c1, c2, c3, axiom if}
 (sum l)

RHS:
(sum x) + (sum l)
= {c1, c2, c3, def sum, axiom if}
  0 + (sum l)
={Arithmetic}
 (sum l)

so, the LHS is equal to RHS

second part:
(A=> (C=>D)) : A/\C=>D: a/\b/\e/\d=>f:
(natlistp x)/\ (natlistp l) /\(natp a)/\(sum (app x l)) = (sum x) + (sum l)
=>(sum (app (cons a x) l)) = (sum (cons a x)) + (sum l)

c1: (natlistp x) 
c2: (natlistp l)
c3: (natp a)
c4: (sum (app x l)) = (sum x) + (sum l)


LHS:
(sum (app (cons a x) l))
= {c3, c1, c2, def app,cons, axiom if, rest ,first}
 (sum (cons a (app x l)))
= {def sum,cons, c1, c2, c3, axiom first, rest, if}
 (+ a (sum (app x l)))
= {c4}
 (+ (+ a (sum x)) (sum l))
 
RHS:
(sum (cons a x)) + (sum l)
={c1, c2, c3, def cons, 
 (+ (+ a (sum x)) (sum l))
 
So the result of LHS is equal to RHS.

 








...

|#
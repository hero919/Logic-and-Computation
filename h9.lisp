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
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading coi/lists book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "coi/lists/basic" :dir :system)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2's lexicographic-ordering-without-arithmetic book.~%This indicates that either your ACL2 installation is missing the standard books are they are not properly certified.") (value :invisible))
(include-book "ordinals/lexicographic-ordering-without-arithmetic" :dir :system)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "ccg" :uncertified-okp nil :dir :acl2s-modes :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

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
              2  ;cons-atom ;changed Jan 15th 2013 from 20
              5  ;nat-list
              1  ;cons-cons-atom ;changed Jan 15th 2013 from 10
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
              
              
              defdata nat string pos rational integer boolean all neg
              acl2-number true-list char symbol oneof listof enum record
              ;; i need them for defdata why?
              
              trace*

              defthm thm defconst in-package
              test?
              acl2s-defaults testing-enabled 
              verbosity-level defunc-verbosity-level
              num-trials num-counterexamples num-witnesses
              subgoal-timeout defunc-timeout defunc-strict)
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

(defunc first (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::car x))

(defunc rest (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::cdr x))

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

(defthm append-length
  (equal (len (acl2::append a b))
         (+ (len a) (len b))))

(defunc app (a b) 
  :input-contract (and (listp a) (listp b))
  :output-contract (and (listp (app a b))
                        (equal (len (app a b)) (+ (len a) (len b))))
  (acl2::append a b))

(defunc rev (a) 
  :input-contract (listp a) 
  :output-contract (and (listp (rev a))
                        ;(equal (len (rev a)) (len a))
                        )
  (if (endp a)
      nil
    (append (rev (rest a)) (list (first a)))))

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

(defunc nth (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract t
  (if (endp l)
      nil
    (if (zp n)
        (first l)
      (nth (- n 1) (rest l)))))

(defunc nthrest (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract (listp (nthrest n l))
  (if (endp l)
      nil
    (if (zp n)
        l
      (nthrest (- n 1) (rest l)))))

(defthm natp-acl2-len-tp 
  (natp (acl2::len x))
  :rule-classes ((:type-prescription) (:rewrite)))

(defthm integerp-acl2-len-tp 
  (integerp (acl2::len x))
  :rule-classes ((:type-prescription) (:rewrite)))

#|
(defunc string-len (l)
  :input-contract (stringp l)
  :output-contract (natp (string-len l))
  (acl2::length l))
|#


; harshrc 29 March 2012 -- added nth-list for Pete
(defun nth-list (n)
  (acl2::nth-true-list n))



;;Settings specific to this mode(copied from R&I mode)
(acl2::in-package "ACL2")
(set-backchain-limit '(50 100))
(set-rewrite-stack-limit 500)
(acl2s-defaults :set subgoal-timeout 60)
(acl2s-defaults :set defunc-timeout 200) 

(set-irrelevant-formals-ok :warn)
(set-bogus-mutual-recursion-ok :warn)
(set-ignore-ok :warn)

;for beginner users dont be strict in admitting defunc
;(acl2::acl2s-defaults :set acl2::defunc-strict 0)  
(acl2s-defaults :set num-trials 500)

;(assign evalable-ld-printingp t)
;(assign evalable-printing-abstractions '(list cons))
;(assign triple-print-prefix "; ")

(cw "~@0Beginner mode loaded.~%~@1"
    #+acl2s-startup "${NoMoReSnIp}$~%" #-acl2s-startup ""
    #+acl2s-startup "${SnIpMeHeRe}$~%" #-acl2s-startup "")


(acl2::in-package "ACL2S B")

; ***************** END INITIALIZATION FOR ACL2s B MODE ******************* ;
;$ACL2s-SMode$;Beginner
 
#|

CS 2800 Homework 9 - Fall 2014

Student names: This homework is done in pairs. Put BOTH names here.


Yueling Qin&&Zeqing Zhang

Technical instructions:

- open this file in ACL2s BEGINNER mode as hw09.lisp

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish some problems, comment
  them out. The same is true for any English text that you may add. This
  file already contains many comments, so you can see what the syntax is.

- when done, save your file and submit it as hw09.lisp.

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution.

Instructions for programming problems:

For each function definition, you must provide both contracts and a body.

You must also ALWAYS supply your own tests. This is in addition to the
tests sometimes provided. Make sure you produce sufficiently many new test
cases. This means: cover at least the possible scenarios according to the
data definitions of the involved types. For example, a function taking two
lists should have at least 4 tests: all combinations of each list being
empty and non-empty.

Beyond that, the number of tests should reflect the difficulty of the
function. For very simple ones, the above coverage of the data definition
cases may be sufficient. For complex functions with numerical output, you
want to test whether it produces the correct output on a reasonable
number if inputs.

Use good judgment. For unreasonably few test cases we will deduct points.

We will use ACL2s' check= facility for tests. This function is similar to
the equal function, except that if the evaluations of the two arguments
passed to it are not equal, the function call results in an error message
(rather than returning nil, as in the case of equal). Thus, if any call to
check= results in "not equal", your file will be rejected.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Induction Proofs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

1.

We want to find a closed formula (one whose evaluation does not require
recursion) for the sum of the first n odd numbers. For example, the sum of
the first _zero_ odd numbers is 0. The sum of the first _two_ odd numbers
is 1 + 3 = 4.
1+3+5=9


(a) Define a recursive ACL2s function sum-odd that computes the sum of the
first n odd numbers. Test your function by computing the sum of the first n
odd numbers for n = 1, 2, 3, 4.

(defunc sum-odd (n)
  :input-contract (natp n)
  :output-contract (natp (sum-odd n))
   (cond ((equal n 0)  0)
         ((equal n 1)  1 )
         ((> n 1) (+ (- (* 2 n) 1) (sum-odd (- n 1))))

))

(check= (sum-odd 0) 0)
(check= (sum-odd 1) 1)
(check= (sum-odd 2) 4)
(check= (sum-odd 3) 9)
(check= (sum-odd 4) 16) 

(b) Generalize your test results from (a) and formulate a closed-form
conjecture about the sum of the first n odd numbers. Write your conjecture
as a logical formula, and make sure you include any necessary type
hypotheses in your conjecture. That is, your conjecture should take the
following form:

phi: (natp n) => ((sum-odd (n))=n*n)

(c) Prove your conjecture by induction. To this end, first extract the
induction scheme suggested by sum-odd. Then prove the parts of the
induction scheme using equational reasoning.

(i)~(natp n)=>phi
(ii)(natp n)/\(equal n 0)=>phi
(iii)(natp n)/\~(equal n 0)/\(equal n 1)=> phi
(iiii)(natp n)/\~(equal n 0)/\~(equal n 1)/\phi|(n (- n 1))=>phi


(i)~(natp n)=>phi
~(natp n)=>(natp n) => (sum-odd (n))=n*n
=~(natp n)/\(natp n)=>(sum-odd (n))=n*n
=nil=>phi
= {def implies}
  T
  
  
  
(ii)(natp n)/\(equal n 0)=>phi
c1:(natp n)
c2:(equal n )
LHS:
={def sum-odd, axiom cond, c1, c2}
=0

RHS:n*n
={Arithmitc}
 0
 

(iii)(natp n)/\~(equal n 0)/\(equal n 1)=> phi

c1:(natp n)
c2:~(equal n 0)
c3:(equal n 1)

LHS:(sum-odd (n))
={c1,c2,c3,def sum-odd, axiom cond}
 1
 
RHS:n*n
={Arithmitc,C3}
 1

(iiii)(natp n)/\~(equal n 0)/\~(equal n 1)/\phi|(n (- n 1))=>phi
c1:(natp n)
c2:~(equal n 0)
c3:~(equal n 1)
c4:(sum (- n 1))=(* (- n 1) (- n 1))


LHS:(sum-odd (n))
={c1,c2,c3,c4,def sum-odd}
  (+ (- (* 2 n) 1) (sum-odd (- n 1)))
={c4}
  (+ (- (* 2 n) 1) (/ (* (- (* 2 (- n 1)) 1) (- n 1))  2))
={Arithmtic}
=(* n n)HS



RHS:
(* n n)

SO, the LHS and RHS is same.




...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

2.

Consider:

(defunc remove-dups (a)
  :input-contract (listp a)
  :output-contract (listp (remove-dups a))
  (if (endp a)
    nil
    (if (in (first a) (rest a))
      (remove-dups (rest a))
      (cons (first a) (remove-dups (rest a))))))

Prove the following conjecture by induction, using the induction scheme
suggested by the remove-dups function itself.

Hint: first rewrite this function equivalently using a single cond.

phi: (listp l) => (<= (len (remove-dups l)) (len l))

1. ~(listp l) => (<= (len (remove-dups l)) (len l)) (axiom) 
It is true.

2. (listp a) /\ (endp a) => phi


Since (listp l) => (<= (len (remove-dups l)) (len l))


For (<= (len (remove-dups l)) (len l))
{the definition of remove}
= (<= (len nil) (len nil))
{axiom}
= t

3.
c1:(listp l)
c2:~(endp l)
c3:(listp (rest l)) => (<= (len (remove-dups (rest l))) (len (rest l)))
c4:(consp l) {c1,c2}

Prove:

(listp l) /\ c3 => (<= (len (remove-dups l)) (len l))

(len (remove-dups l))
= (len (remove-dups (cons (first l) (rest l))))
Since it has only two situations.
if (in (first l) (rest l))
(len (remove-dups (cons (first l) (rest l)))) = (len (remove-dups (rest l)))


we know from c3:
(len (remove-dups (rest l))) <= (len (rest l)) 
{definition of len}
(len (remove-dups (rest l))) < (len  l))


if ~(in (first l) (rest l))
(len (remove-dups (cons (first l) (rest l)))) = (len (cons (first l) (remove-dups (rest l))))
Since we know from c3:
(len (remove-dups (rest l))) <= (len (rest l))
(len (cons (first l) (remove-dups (rest l))))
= {The definition of len,c3}
= (+ (len (remove-dups (rest l))) 1) <= (+ 1 (len (rest l))) = (len l)
Therefore it is true.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Induction Schemes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

This set of problems is about Induction Schemes. Remember that *every*
admissible ACL2 function gives rise to an induction scheme. Given an
arbitrary formula phi (which is irrelevant for these problems), determine
the induction scheme for each of the following functions. That is,
instantiate the general induction scheme template from the lecture notes to
a concrete induction scheme for each function. Do not omit "trivial" cases.
Your answer should look like this:

"In order to prove phi, it is sufficient, by the induction principle, to prove:

(i)   ... => phi
(ii)  ... => phi
(iii) ... => phi
..."

Note that, depending on the function given to you, there may be more than
three clauses in the induction scheme.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

3.

(defunc natlistp (l)
  :input-contract (listp l)
  :output-contract (booleanp (natlistp l))
  (cond ((endp l)               t)
    ((not (natp (first l))) nil)
    (t                      (natlistp (rest l)))))

(i) ~(listp l)  => phi
(ii) (listp l)/\(endp l)=> phi
(iii)(listp l)/\~(endp l)/\(not (natp (first l)))=> phi
(iiii)(listp l)/\~(endp l)/\(natp (first l))/\t/\
              phi|(l (rest l))=> phi
  
    
...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

4.

(defunc nth (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract t
  (if (endp l)
    nil
    (if (equal n 0)
      (first l)
      (nth (- n 1) (rest l)))))

Hint: first rewrite this function equivalently using a single cond.

(defunc nth (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract t
  (cond ( (endp l) nil)
        ( (equal n 0) (first l))
        ( else (nth (- n 1) (rest l)))))
 
i. ~(and (natp n) (listp l)) => phi
ii. (and (natp n) (listp l)) /\ (endp l) => phi
iii. (and (natp n) (listp l)) /\ ~(endp l) /\ (equal n 0) => phi
iv. (and (natp n) (listp l)) /\ ~(endp l) /\ ~(equal n 0) /\ phi| (l (rest l))=> phi
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

5.

(defunc in (a x)
 :input-contract (listp x)
 :output-contract (booleanp (in a x))
   cond ((endp x)            nil)
        ((equal a (first x)) t)
        (t                   (in a (rest x))))

(i) ~(listp x)=> phi
(ii)(listp x)/\(endp x)=> phi
(iii)(listp x)/\~(endp x)/\(equal a (first x))=> phi
(iiii)(listp x)/\~(endp x)/\~(equal a (first x))/\
             phi|(x (rest x))=> phi
...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

6.

(defunc exists-symbol (l)
  :input-contract (listp l)
  :output-contract (booleanp (exists-symbol l))
  (and (not (endp l))
       (or (symbolp (first l))
       (exists-symbol (rest l)))))

Hint: first rewrite this function equivalently using a single cond.

(defunc exists-symbol (l)
  :input-contract (listp l)
  :output-contract (booleanp (exists-symbol l))
  (cond ((endp l) nil)
        ((symbolp (first l)) t)
       (else (exists-symbol (rest l)))))
       
       
 i. ~(listp l) => phi
 ii. (listp l)/\(endp l) => phi
 iii. (listp l) /\ ~(endp l) /\ (symbolp (first l)) => phi
 iv. (listp l) /\ ~(endp l) /\ ~ (symbolp (first l)) /\ phi| (l (rest l)) => phi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

7.

(defunc how-many (e l)
  :input-contract (listp l)
  :output-contract (natp (how-many e l))
  (if (endp l)
    0
    (+ (if (equal e (first l)) 1 0)
       (how-many e (rest l)))))
       
       
(defunc how-many (e l)
   (cond ((endp l) 0)
         ((equal e (first l)) (+ 1 (how-many e (rest l))))
         (t (how-many e (rest l)))))

(Be careful with the second (nested) if. The best way to deal with it is
to "hoist" it: pull the inner if outside of the surrounding + function,
which requires duplicating some code.) The result should be code with a
single cond.


(i) ~(listp l)=> phi
(ii) (listp l)/\(endp l)=> phi
(iii)(listp l)/\~(endp l)/\(equal e (first l))/\ phi| ( l (rest l))=> phi
(iiii)(listp l)/\~(endp l)/\~(equal e (first l))/\phi| (l (rest l))=> phi



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

8.

Check this function for admissibility first. If it is not admissible, what
does this mean for the induction scheme?

(defunc run-down-a-list (n l)
  :input-contract (and (integerp n) (listp l))
  :output-contract t
  (if (endp l)
    nil
    (if (equal n 0)
      (first l)
      (run-down-a-list (- n 1) l))))

It is not admissible.
For the induction scheme.
It means it will never stop and if we want to prove some cases, the induction prove 
is the only way to solve the problem.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

9.

Consider the standard admissible definition

(defunc len (l)
  :input-contract (listp l)
  :output-contract (natp (len l))
  (if (endp l)
    0
    (+ 1 (len (rest l)))))

(a) Given some formula phi, write down the induction scheme len gives rise to:
phi: (len x) => (app x nil) = x

(i) ~(listp l)=> phi
(ii) (listp l)/\(endp l)=> phi
(iii) (listp l)/\~(endp l)/\phi|(l (rest l))=>phi


(b) Someone who did not pay attention in class claims that the induction
scheme derived from the len function above can be simplified: in order to
prove any claim phi, instead prove

(i)  (listp x) /\ (endp x) => phi
(ii) (listp x) /\ (not (endp x)) /\ phi|((x (rest x))) => phi

Show that this is a mistake: give an _invalid_ claim phi that can
nevertheless be "proved" using the above wrong induction scheme: give a
counterexample for phi, and prove by propositional reasoning that
nevertheless both (i) and (ii) hold for your choice of phi.

phi : (listp x)
x = 1

substitution sigma such that phi|sigma is equivalent to nil : phi|(x 1) 

 

(i) is equivalent to T:

(listp 1)/\(endp 1)=> (listp 1)
= nil=> nil
=T

(ii) is equivalent to T:


(listp 1)/\(not (endp 1))/\(listp 1)=> (listp 1)
=nil=>phi
=T


(c) Based on your convincing counterexample, the person says he really
meant: in order to prove any claim phi, instead prove

(i)  (not (listp x)) => phi
(ii) (listp x) /\ (not (endp x)) /\ phi|((x (rest x))) => phi

Show that this is not much better: again give an _invalid_ claim phi and a
counterexample for phi, and prove by propositional reasoning that
nevertheless both (i) and (ii) hold for your choice of phi.


phi : (listp x) = nil
x = nil
substitution sigma such that phi|sigma is equivalent to nil : ((listp x)= nil)|(x nil)

(i) is equivalent to T:

(not (listp nil))=>phi
= nil=>phi
=T

  

(ii) is equivalent to T: prove this carefully, including identifying
     context, derived context, etc.
c1:~(listp 1)=> phi
c2: (listp 1) => phi
c3: nil {c1,c2} 

(listp nil)/\((endp nil)=>phi

c1:(listp nil)
c2:(endp nil)

(listp x) /\ (not (endp x)) /\ phi|((x (rest x))) => phi
c1:(listp x)
c2:(not (endp x))
c3:(listp (rest x))=>phi
c4:(listp (rest x)) {c1,c2)
=nil=>phi
=T



|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Induction Schemes of Non-Recursive Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

*Every* admissible ACL2 function gives rise to a valid induction scheme --
even non-recursive ones! Using a few example, let's see what such induction
schemes look like. As above, determine the induction scheme for each of the
following functions (for some arbitrary [and irrelevant] conjecture phi).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

10.

(defunc implies (p q)
  :input-contract (and (booleanp p) (booleanp q))
  :output-contract (booleanp (implies p q))
  (if q
    t
    (not p)))
    
    
i. ~(and (booleanp p) (booleanp q)) => phi
ii. (and (booleanp p) (booleanp q)) /\ q => phi
iii. (and (booleanp p) (booleanp q)) /\ ~q => phi



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

11.

(defunc fmad (a b c)
  :input-contract (and (rationalp a) (rationalp b) (rationalp c))
  :output-contract (rationalp (fmad a b c))
  (+ (* a b) c))
i. ~(and (rationalp a) (rationalp b) (rationalp c)) => phi
ii.(and (rationalp a) (rationalp b) (rationalp c)) 
              /\ (equal (fmad a b c) (+ (* a b) c)) => phi


|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

12.

In this problem you will learn about the relationship between termination
and computational complexity of functions, known in the form of "big-Oh"
notation. You will implement a simple sorting algorithm known as Bubble
sort. The algorithm takes a rationallist and checks whether the list
contains an index i such that the elements x and y at positions i and i+1
satisfy x > y. If so, the algorithm swaps x and y. This check-and-swap
sequence is repeated until the list is sorted.

Give a partial argument why the algorithm terminates, as follows. Suppose
the list is not sorted. How do we know that the check-and-swap sequence
actually modifies the list? (If the list is not sorted and the algorithm
does not modify it, then the algorithm obviously does not terminate, since
the sortedness check will always fail.)

After swap the list we can check whether the two lists are the same. In this
way we can find whether we modify the list. The other way is when we use the 
method again, since it doesn't modified. It will not terminate and will recursive 
forever.

|#

; Recall the definition of a list of rationals, which gives rise to a
; recognizer rationallistp:

(defdata rationallist (listof rational))

; Define a function

; out-of-order : Rationallist -> Nat

; that takes a list l of rationals and returns an index i such that the
; elements x and y of l at positions i and i+1, respectively, satisfy x > y
; (which means they need to be swapped when sorting). If l does not contain
; such elements, return (len l). Note that position counting starts from 0.
; See examples.

; Hint: remember the first and second functions, and be aware of these
; functions' input contracts.

(defunc out-of-order (l)
  :input-contract (rationallistp l)
  :output-contract (natp (out-of-order l))
  (cond ((endp l) 0)
        ((endp (rest l)) 1)
        ((> (first l) (first (rest l))) 0)
        (t (+ 1 (out-of-order (rest l)))))
  )

(check= (out-of-order '(1 2)) 2) ; nothing swappable
(check= (out-of-order '(2 1)) 0)
(check= (out-of-order nil) 0)
(check= (out-of-order '(1)) 1)
(check= (out-of-order '(1 2 3)) 3)

; Define a function

; swap : Nat x Rationallist -> Rationallist

; that takes a natural number i and a list of rationals l such that i+1 <
; (len l). This last condition must be part of your input contract.
; The function returns l, but with elements at positions i and i+1 swapped.

; Hint: again remember the first and second functions. Think about what
; argument this function should recur over.

(defunc swap (i l)
  :input-contract (and (natp i) (rationallistp l) (< (+ i 1) (len l)))
  :output-contract (rationallistp (swap i l))
  (cond ((endp l) l)
        ((endp (rest l)) l)  
        ((equal i 0) (cons (second l) (cons (first l) (rest (rest l))))) 
        (t (cons (first l) (swap (- i 1) (rest l))))))

(check= (swap 0 '(1 2)) '(2 1))
(check= (swap 1 '(1 2 3 4)) '(1 3 2 4))
(check= (swap 0 '(0 0 0 0 1)) '(0 0 0 0 1))

; Now we can write a sort function that looks for swappable elements using
; out-of-order, then swaps them using swap, and repeats this until the list
; is sorted. However, there is a problem: above you have given a partial
; argument why this process will terminate. ACL2s, however, needs to give a
; full termination proof, not just a partial argument, to admit the
; function. This is difficult: if you write the function as suggested
; above, ACL2 will not prove it terminating. We can help it as follows.

; First write a function

; sort-helper : Rationallist x Nat -> Rationallist

; such that (sort-helper l k) simply returns l unchanged if there are no
; swappable elements, and otherwise swaps the swappable elements and looks
; for more swappable elements, but at most k times. This function naturally
; terminates, as the "swap count" k decreases in every recursive call.

; Design some tests where k is too small to sort the list l completely.
; Design some tests where k is large enough to sort the list l. For the
; second category of tests, try to find the minimal k that accomplishes the
; sorting.

(defunc sort-helper (l k)
  :input-contract (and (rationallistp l) (natp k))
  :output-contract (rationallistp (sort-helper l k))
  (cond ((equal k 0) l)
        ((< (+ (out-of-order l) 1) (len l)) (sort-helper (swap (out-of-order l) l) (- k 1)))
        (t l))
  )

(check= (sort-helper (list 3 2 4 1 5) 100) (list 1 2 3 4 5)) ;;Large number
(check= (sort-helper (list  3 6 2 8 1 2) 2) (list 2 3 6 8 1 2))  ;;Small number
(check= (sort-helper (list 1 2 4 3) 0) (list 1 2 4 3))
(check= (sort-helper (list 1 2 9 8) 9) (list 1 2 8 9))
(check= (sort-helper nil 20) nil)
(check= (sort-helper (list 1 2 3 90 89) 0) (list 1 2 3 90 89))
(check= (sort-helper (list 0) 0) (list 0))
(check= (sort-helper (list 4 3 2 1) 4) (list 2 3 1 4))


; Now we want to get rid of the parameter k, which is not user-friendly,
; and define a function

; sort : Rationallist -> Rationallist

; Given a list l, estimate a number -- as a function of (len l) -- that
; gives a safe upper bound on the number of swaps needed to fully sort the
; list. Pass that number to sort-helper for the parameter k. Again, this
; value will depend on (len l).

; Design at least 10 tests. Include all the tests you used for the
; sort-helper function where the parameter k was insufficient to fully sort
; the list.

(defunc sort (l)
  :input-contract (rationallistp l)
  :output-contract (rationallistp (sort l))
  (sort-helper l (* (len l) (len l)))
  
  
  
  )

(check= (sort '(1 2 3 4 5)) '(1 2 3 4 5))
(check= (sort nil) nil)
(check= (sort '(2 3 1 -2 90)) '(-2 1 2 3 90))
(check= (sort '(-1 0 9 6 1 2 3)) '(-1 0 1 2 3 6 9))
(check= (sort '(2/3 3 -1 4/6 0 -2)) '(-2 -1 0 2/3 2/3 3))
(check= (sort '(1)) '(1))
(check= (sort '(9 8 7 1)) '(1 7 8 9))
(check= (sort '(2/3 5/6 0 80 100 2 13 4 -20)) '(-20 0 2/3 5/6 2 4 13 80 100))
(check= (sort '(1 3 7 1 2 2 3 9 0 0)) '(0 0 1 1 2 2 3 3 7 9))
(check= (sort '(0 0)) '(0 0))
(check= (sort '(-1 -10)) '(-10 -1))#|ACL2s-ToDo-Line|#



#|

Closing comments:

Function sort is non-recursive and thus of course terminates. We can in
fact say more: the function will apply at most k swaps to the input list,
so we have an estimate *how long* it will in fact run, depending on the
input length (since k depends on (len l)). We say that our sorting
algorithm, which is known as bubble sort, has quadratic complexity, denoted
O(|l|^2), where |l| is the length of the input list l.
|#
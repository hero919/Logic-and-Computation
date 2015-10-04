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
;;Yueling Qin && Zeqing Zhang


CS 2800 Homework 5 - Fall 2014

Student names: This homework is done in pairs. Put BOTH names here.

Technical instructions:

- open this file in ACL2s BEGINNER mode as hw05.lisp

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish some problems, comment
  them out. The same is true for any English text that you may add. This
  file already contains many comments, so you can see what the syntax is.

- when done, save your file and submit it as hw05.lisp.

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
; Admissible or not?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

This problem set is about the "Definitional Principle".

For each of the definitions below, check whether it is admissible, i.e. it
satisfies Rules 2-6 of the definitional principle. You can assume that Rule
1 is met: the symbol used in the defunc is a new function symbol in each case.

If you claim admissibility,

1. Explain in English why the body contracts hold.
2. Suggest a measure function that can be used to show termination.
   (You do not have to prove the measure function properties in this problem.)
3. Explain in English why the contract theorem holds.

Otherwise, identify the *first* rule in the Definitional Principle that is violated.
("First" is referring to the order the rules where introduced in the lecture.)

(a) If your answer is Rule 2 or 3, explain in English.
(b) If your answer is Rule 4, 5, or 6, provide an input that satisfies the input
    contract, but causes a violation in the body (Rule 4), causes
    non-termination (Rule 5), or the output violates the output contract (Rule 6).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (l)
  :input-contract (listp l)
  :output-contract (listp (f l))
  (if (endp l)
      nil
      (g (rest l))))
it is not admissible, 

It violated the rule 6, we assumed that rule 1 is met, so the function is not 
satisfied the output contract. 
the example is (f (list 1 2)), since l is not empty
therefore we call (g (rest l)). We are not sure taht (g (rest l))
is a list. Therefore it is not admissible.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (x y)
  :input-contract (and (listp x) (integerp y))
  :output-contract (natp (f x y))
  (if (endp x)
      y
    (+ 1 (f (rest x) y))))

This is not admissiable, it break the rule of output contract (rule 6)
we can give a example for it.
if (f (list 1 2) -1)), firstly, we go to recursice call, (+1 (f (list 1) -10))
then, (+1 (+1 (f '() -10))), we get (+1 (+1 -10)), the final answer is negative 
number, so it break the rule of output contract (rule 6).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (n a)
  :input-contract (and (natp n) (listp a))
  :output-contract (natp (f n a))
  (if (endp a)
      0
      (f (rest a))))

It is not admissible. It violate the contract 4.
For the contract, the first time the input contract is ok, but the second time
f should take two input but it only takes one input, therefore it violate the 
input contract.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (x y)
  :input-contract (and (listp x) (listp y))
  :output-contract (natp (f x y))
  (if (endp x)
    -1
    (f (rest x) (cons 1 y))))
    
It is not admissible. It viloted the rule 6. The example is (f (list 1 2) (list 3 4)).
What we return os -1. But -1 is not a natural number.





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (x a)
  :input-contract (and (listp x) (natp a))
  :output-contract (natp (f x a))
  (if (endp x)
    a
    (f (rest x) (+ a 1))))

It is admissible.
1. At the beginning, input contract should be ok. For the 
each recursive call, we know x is a list and a is a natural number
therefore when we call (rest x) we will still get a list and (+ a 1)
we still get a natral number. Which means (f (rest x) (+ a 1)) is still
under the input contrat.

2. For the measure function. It is:

(defunc m ( x a )
  :input-contract (and (listp x) (natp a))
  :output-contract (natp (m x a))
  (len x))
  
  
  Each time we compare (m x a) and (m (rest x) (+ a 1)).
  From the definition of the length, we can know that
  (m x a) > (m (rest x) (+ a 1)) because 
  (len x) > (len (rest x)) 
  (len x)-1 = (len (rest x))
  so, final (len x) > (len x) -1
  
3. We know that the input contract is true each time, each time
a beome (+ a 1) but still be a natural number. Therefore finnally
it is also under the output contract.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (x y)
  :input-contract (and (integerp x) (integerp y))
  :output-contract (integerp (f x y))
  (if (equal x 0)
      0
      (+ (* 2 y) (f (- x 1) y))))

It is not admissible. It violated the rule 5.
The example is (f -5 2), it fits for the input contract. But when we call the 
recursive call, first time -5 - 1, we get -6. We will always get the negative number
but never get 0. Therefore it will never be terminated.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (a n)
  :input-contract (and (listp a) (natp n))
  :output-contract (listp (f a n))
  (if (equal n 0)
      (list n)
      (f (cons (rest a) (first a))
         (- n 1))))

It is not admissible.It violates the rule 4. The example is (f (list 1 2) 3) When we first meet the recursive call,
what we get is (f (cons (rest a) (first a)) (- n 1)) but (cons (rest a) (first a)) 
is not end with nil which means it is not a list. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (a b)
  :input-contract  (and (natp a) (natp b))
  :output-contract (natp (f a b))
  (cond ((and (equal a 0) (equal b 0)) 0)
        ((< a b)                       (f b a))
        (t                             (f a (- b 1)))))

It is not admissible. It break the rule 5.
The example is (f 1 2), 1 < 2, therefore we call (f b a) which is (f 2 1)
then we call (f 2 0), then (f 2 -1) each time a is greater than b. Therefore it 
will never terminated.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (a b)
  :input-contract  (and (natp a) (natp b))
  :output-contract (natp (f a b))
  (cond ((and (equal a 0) (equal b 0)) 0)
        ((< a b)                       (f a (- b 1)))
        (t                             (f b a))))

It is not admissible. It violate the rule 5.
The example is (f 2 4), the first time (< 2 4), we will call (f 2 (- 4 1))
next time (f 2 2) then (f 2 2) (f 2 2)......It will never stop since a b is the 
same number.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (x y)
  :input-contract (listp y)
  :output-contract (natp (f x y))
  (cond ((endp y)  0)
        (t         (f  (len y) (x (rest y))))))

It is not admissible. we assumed we met rule1, tt violate the rule 4. 
The example is (f 1 (list 1 2))
It will go to the second statement (f (len y) (x (rest y))), (len y) is a nat
however (x (rest y)) is (1 (list 2)). It doesn't make any sense what it is. Of course
it could not be a list therefore it violate the input contract.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (x l)
  :input-contract (and (integerp  x) (listp l))
  :output-contract (natp (f x l))
  (cond ((endp l) 0)
        ((> x 0)  (f (len l) (rest l)))
        (t        (- (f x (rest l)) 1))))

It is not admissible. It violate the rule 6. The example is (f -2 (list 1 2 3)),
the return value -1 is negative but the return value should be natural number.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fleshing out the conditions of the Definitional Principle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Consider the following function definition:

(defunc accord (i j c)
  :input-contract (and (natp i) (natp j) (natp c) (< i j))
  :output-contract (natp (accord i j c))
  (cond ((and (equal i 0) (equal j 1)) c)
        ((> i 0) (accord (- i 1) j (+ c 1)))
        (t       (accord (- j 2) (- j 1) (+ c 1)))))

(a) Show that the constraint (< i j) in the input contract of accord holds
for each recursive call of accord. (There are two.)

To solve this problem, you have to substitute the actual arguments used in
the recursive calls of accord for i and j into the expression i < j, and
then show that the resulting expression is valid. Show the math. As always,
you can assume that i < j, i.e. the current inputs satisfy the input contract.

go to the first condition is evaluated, the function terminates and there is no
recursive call.

go to the second condition is evaluated, the recursive call looks like:
(accord (- i 1) j (+ c 1))
So if (< i j) is true, (< (- i 1) j) will still be true.

go to the third condition is evaluated, the recursive call looks like:
(accord (- j 2) (- j 1) (+ c 1))
Regardless of whether or not (< i j) is true, (< (- j 2) (- j 1)) will
always be true.

We assume the first time (< i j) because if not it will break the input contract.
First we think about the first recursive call which is (> i 0) => (accord (- i 1) j (+ c 1))
since we know (< i j) , therefore (- i 1) is still smaller than j. It is ok. 
If i is not greater than 0, which means i is 0 (since i is natural number). We will run 
 (t       (accord (- j 2) (- j 1) (+ c 1))) which (- j 2) < (- j 1). Therefore both recurasive call
 is ok.


(b) Suppose we change the input contract to

    (and (natp i) (natp j) (natp c))

i.e. we drop the constraint i < j.

Provide an input (i j c) that satisfies the new input contract, but one of
the recursive calls in the body of accord causes a contract violation.
Clearly indicate where in the body that contract violation happens.

Hint: this will only work if your input violates (< i j) !

(accord 1 0 0) -> (accord 0 0 1) -> (accord -2 -1 2) 
villated the rule 4: the body contract of input , because the i and j cannot be negative 
number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Consider the following function definition:

(defunc f (x y z)
  :input-contract (and (natp x) (natp y) (listp z))
  :output-contract (natp (f x y z))
  (cond ((equal x 0) (+ y (len z)))
        ((endp z)    (+ x y))
        ((<= y 1)    (+ x (f (- x 1) 0 (rest z))))
        ((< x y)     (f x (- y 2) z))
        (t           (f x y (rest z)))))

(a) Formulate the contract theorem for the above function definition in
propositional logic. You may use ACL2 expressions such as (equal x 0) as
propositional atoms in this formula. (You do not need to prove anything in
this part.)

(and (natp x) (natp y) (listp z))^ (equal x 0) => (natp (+ y (len z)))
(and (natp x) (natp y) (listp z))^ ~(equal x 0) ^(endp z) => (natp (+ x y))
(and (natp x) (natp y) (listp z))^ ~(equal x 0)^ ~(endp z)^ (<= y 1) =>  (natp (+ x (f (- x 1) 0 (rest z))))
(and (natp x) (natp y) (listp z))^ ~(equal x 0)^ ~(endp z)^ (> y 1)^ (< x y) => (natp (f x (- y 2) z))
(and (natp x) (natp y) (listp z))^ ~(equal x 0)^ ~(endp z)^ (> y 1)^ (>= x y) => (natp (f x y (rest z)))

((natp x) /\ (natp y) /\ (listp z)) => (natp (f x y z))

(b) The body contract rule states: "The body contracts of all functions must
  hold, provided the input contract of f holds."

More precisely: for every function call (g arg1 ... argk) occurring in the
body of f, the input contract of g --- applied to the actual arguments arg1
... argk in the call -- holds, provided that (i) the input contract of f
holds, AND (ii) the conditions that lead to that call to g hold.

Formalize this condition in propositional logic, but only for the LAST TWO
clauses of the cond expression: the one guarded by (< x y), and the one
guarded by t. To help you get started, part of this condition for the FIRST
cond clause is provided. Fill in the rest, and do the same for the final
clause. Even if the parts you need to fill in appear trivial, write them
down. (You do not need to prove anything in this part.)

 (natp x) /\ (natp y) /\ 
 (listp z) /\ ~(equal x 0) /\ ~(endp z) /\ ~(<= y 1) /\ (< x y)
  =>  (natp x) /\ (natp (- y 2)) /\ (listp z) /\
  /\ ~(equal x 0) /\ ~(endp z) /\ ~(<= (- y 2) 1) /\ (< x (- y 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Consider the following function definition:

(defunc f (x)
  :input-contract (integerp x)
  :output-contract (and (natp (f x)) (evenp (f x)))
  (cond ((> x 5)   (+ 1 (f (- x 1))))
        ((< x -5)  (- 2 (f (+ x 1))))
        ((evenp x) x)
        (t         (+ 3 (f (* 3 x))))))

You may assume that f is a fresh function symbol, and that evenp returns
true whenever its argument is an even integer.

(a) Do the body contracts hold, under the condition that the input contract
holds? If yes, state so, and give a brief justification. If no, provide an
integer input x such that the call (f x) eventually leads to a body
contract violation.

the body contract will hold, because the input contract, x is an integer
and evenp hold any integer, all conditions return output is integer.

(b) Determine the set TERM of all inputs x such that (f x) terminates. If
this set is infinite, describe it suitably. If it is finite, enumerate its
elements. You do not have to prove termination in this problem.

TERM = {x|-4 -2 0 2 4}

(c) Does the contract theorem hold on the inputs determined in (b)? If yes,
state so, and give a brief justification. If no, provide an integer input x
identified in (b) such that the call (f x) produces a value that violates
the output contract.

No. (f -4) will give us -4 but still not a natural number.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Investigating a non-terminating function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

In this problem you will investigate the termination of a function that
operates on natural numbers _modulo n_ : this means we only consider
natural numbers i in the range 0 <= i < n (note that n is excluded); there
are exactly n such numbers. Arithmetic operations such as addition "wrap
around", as we will see.

We will implicitly introduce the datatype nat-mod-n of such restricted
natural numbers. This is a _parameterized_ datatype: it depends on the
parameter n.

|#

; Define a recognizer nat-mod-n-p for the datatype nat-mod-n. Since the
; type is parameterized by n, so is the recognizer, which hence takes not
; only some a of type All as input, but also n of type Nat:

; nat-mod-n-p : All x Nat -> Boolean

; (nat-mod-n-p a n) returns T iff a is a natural number less than n.

; Carefully think about the contracts; the signature given above says it all.
;
; Define 3 tests.

(defunc nat-mod-n-p (a n)
  :input-contract (natp n)
  :output-contract (booleanp (nat-mod-n-p a n))
  (if (natp a)
    (< a n)
    nil)

)

(check= (nat-mod-n-p 2 4) t)
(check= (nat-mod-n-p 5 4) nil)
(check= (nat-mod-n-p 'a' 4) nil)


; For example, suppose we define the concrete type of numbers 0,1,2,3:

(defdata mod-4 (oneof 0 1 2 3))

; then the following theorem "thm" holds (and ACL2s should be able to prove
; it if you defined nat-mod-n-p correctly):

(thm (equal (mod-4p a) (nat-mod-n-p a 4)))

; What does the theorem state?

; Now define a function

; plus-mod-n : nat-mod-n x nat-mod-n x Nat -> nat-mod-n

; such that (plus-mod-n a b n) returns the sum of a and b modulo n. That is, it
; returns the sum a+b, unless that sum equals or exceeds n, in which case
; the result "wraps around": the result is the remainder of a+b when
; divided by n. See examples below.

; Hints: (i) There is a very simple way of defining this function; you
; don't even need any kind of remainder function. (ii) Use let to store the
; natural number a+b in a local variable.

; Define 3 more tests.
;
; Note the way the input contract is written. Make sure you understand why.

(defunc plus-mod-n (a b n)
  :input-contract (if (natp n) (and (nat-mod-n-p a n) (nat-mod-n-p b n)) nil)
  :output-contract (nat-mod-n-p (plus-mod-n a b n) n)
  (let
    ((s (+ a b)))
    (if (< s n)
      s
      (- s n))))
(check= (plus-mod-n 2 3 10) 5)
(check= (plus-mod-n 2 3  5) 0)
(check= (plus-mod-n 2 3  4) 1)
(check= (plus-mod-n 10 11 12) 9)


; Now define an analogous function

; minus-mod-n : nat-mod-n x nat-mod-n x Nat -> nat-mod-n

; such that (minus-mod-n a b n) returns a-b modulo n. See tests below, and define 3 more.

; Think about what happens when a-b is negative: the output MUST be non-negative.

(defunc minus-mod-n (a b n)
  :input-contract (if (natp n) (and (nat-mod-n-p a n) (nat-mod-n-p b n)) nil)
  :output-contract (nat-mod-n-p (minus-mod-n a b n) n)
  (let
    ((d (- a b)))
    (if (>= d 0)
      d
      (+ d n))))
     

(check= (minus-mod-n 5 3 10) 2)
(check= (minus-mod-n 3 5 10) 8)
(check= (minus-mod-n 3 5  6) 4)
(check= (minus-mod-n 3 17 20) 6)



; We are now switching into program mode, as we are about to define a
; function that ACL2 has difficulties proving terminating (for good reasons):

:program
(acl2::acl2s-defaults :set acl2::testing-enabled nil)

; Write a function

; ring-rendezvous : nat-mod-n x nat-mod-n x Nat -> Boolean

; such that ring-rendezvous(a,b,n) implements the following loop:

; while a and b are not equal:
;   decrement a by 1 modulo n
;   increment b by 1 modulo n
; end of loop

; This can be accomplished as follows. Your function should return t if a
; and b are equal; this simply terminates the loop. Otherwise, the function
; is called recursively, replacing the arguments a and b according to the
; decrement/increment expressions shown above. Note that you have to use
; the plus-mod-n and minus-mod-n functions defined before to properly
; implement decrement and increment modulo n (do not use the built-in
; functions - and +). Parameter n always stays the same.

(defunc ring-rendezvous (a b n)
  :input-contract (if (natp n) (and (nat-mod-n-p a n) (nat-mod-n-p b n)) nil)
  :output-contract (booleanp (ring-rendezvous a b n))
(if (equal a b)
    t
    (ring-rendezvous (minus-mod-n a 1 n) (plus-mod-n b 1 n) n)))

; This function is accepted in program mode. (You may want to convince
; yourself that the function is not accepted without the :program switch.)

; The following switch turns off guard checking, which would create a
; warning in subsequent test cases that makes the tests unreadable:

(acl2::set-guard-checking :none)

; Now try out a few cases at the ACL2 prompt, including:

; (ring-rendezvous 2 3 7) ; return t
; (ring-rendezvous 3 4 7) ; return t
; (ring-rendezvous 2 4 8) ; return t
; (ring-rendezvous 3 4 6) ; stack overflow (function does not terminate)

; Do not remove the comment sign in front of these tests in the .lisp file,
; or your file will not be accepted!

; In the comments _behind_ the tests above, state your findings regarding
; what happens in these function calls.

; Now turn on function call tracing for ring-rendezvous:

(acl2::trace! ring-rendezvous)

; and run the above test cases again at the ACL2 prompt.

; Looking at these test cases, if you find that ring-rendezvous fails to
; terminate in any of them, explain why, by illustrating the call sequence
; in a suitable way.

;1> (|ACL2_*1*_ACL2S B|::RING-RENDEZVOUS 3 4 6)
  ;2> (|ACL2_*1*_ACL2S B|::RING-RENDEZVOUS 2 5 6)
    ;3> (|ACL2_*1*_ACL2S B|::RING-RENDEZVOUS 1 0 6)
      ;4> (|ACL2_*1*_ACL2S B|::RING-RENDEZVOUS 0 1 6)
        ;5> (|ACL2_*1*_ACL2S B|::RING-RENDEZVOUS 5 2 6)
          ;6> (|ACL2_*1*_ACL2S B|::RING-RENDEZVOUS 4 3 6)
            ;7> (|ACL2_*1*_ACL2S B|::RING-RENDEZVOUS 3 4 6)
;;it will loop forever, and cause the stack overflow.

; Looking at the successful (terminating) test cases, come up with
; conjectures for restrictions on the inputs (a b n) such that the function
; terminates.

; (abs (- a b)) cannot be odd when less than even n

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feedback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

;; Please fill out the following *anonymous* feedback form, not as a team,
;; but each team member individually:

;; https://docs.google.com/forms/d/17USXaQp44_e02eAMcmrMToQYkUP9-rKGlapA8EBKFvg/viewform

;; Enter your name here to indicate that you filled out the form:
;; Yueling Qin &&Zeqing Zhang fill out the feedback .

;; Name of questionnaire participant: CS 2800 Fall 2014 Feedback Form 1....

Filling out the feedback form is worth 10% of your homework points.

The form is anonymous, to encourage you to write what you think about this
class. (If you want to stay anonymous, do not reveal your name on the
feedback form.) On the other hand, we must rely on your honesty in stating
whether you filled out the form or not, and in not filling out the form
several times (why would you do that??). We do not want to find
discrepancies in the number of entries on the form, and the number of
people claiming to have filled out the form.

|#
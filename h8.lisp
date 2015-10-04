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

CS 2800 Homework 8 - Fall 2014

Student names: This homework is done in pairs. Put BOTH names here.



Yueling Qin && Zeqing Zhang



Technical instructions:

- open this file in ACL2s BEGINNER mode as hw08.lisp

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish some problems, comment
  them out. The same is true for any English text that you may add. This
  file already contains many comments, so you can see what the syntax is.

- when done, save your file and submit it as hw08.lisp.

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
; Equational reasoning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Instructions for equational reasoning problems:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Here are the definitions used for the remainder of the questions. (Note:
these may be different from earlier definitions of these functions -- what
counts are the definitions provided here.)

(defunc len (x)
  :input-contract (listp x)
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

(defunc in (x l)
  :input-contract (listp l)
  :output-contract (booleanp (in x l))
  (cond ((endp l)            nil)
        ((equal x (first l)) t)
        (t                   (in x (rest l)))))

(defunc sum (l)
  :input-contract (natlistp l)
  :output-contract (natp (sum l))
  (if (endp l)
      0
    (+ (first l) (sum (rest l)))))

(defunc fib (n)
  :input-contract (natp n)
  :output-contract (natp (fib n))
  (if (<= n 1)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defunc evenp (n)
  :input-contract t
  :output-contract (booleanp (evenp n))
  (if (integerp n)
    (integerp (/ n 2))
    nil))

(defdata natlist (listof nat))

(defunc subsetp (l1 l2)
  :input-contract (and (listp l1) (listp l2))
  :output-contract (booleanp (subsetp l1 l2))
  (cond ((endp l1)          t)
        ((in (first l1) l2) (subsetp (rest l1) l2))
        (t                  nil)))

(defunc intersect (l1 l2)
  :input-contract (and (listp l1) (listp l2))
  :output-contract (listp (intersect l1 l2))
  (cond ((endp l1)          nil)
        ((in (first l1) l2) (cons (first l1) (intersect (rest l1) l2)))
        (t                  (intersect (rest l1) l2))))

(defunc ziplists (x y)
  :input-contract (and (listp x) (listp y) (equal (len x) (len y)))
  :output-contract (listp (ziplists x y))
  (if (endp x)
    nil
    (cons (list (first x) (first y))
          (ziplists (rest x) (rest y)))))

Recall that for each of the defunc's above we have both a definitional axiom

(ic => (f <args>) = <function body>)

(you can refer to it in justifications as "def. f"), and a contract theorem

(ic => oc)

(you can refer to it in justifications as "oc of f").

Definitional axioms and contract theorems of admitted functions are
available for you to use.

A "natlist" is a list of natural numbers. You can assume that a natlist is
a list, that reversing a natlist results in a natlist, and that consing a
nat onto a natlist results in a natlist.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

1.

When proving the following conjecture, you can assume the following is a
theorem:

(Thm. 1)

(implies (and (natlistp l1) (natlistp l2))
     (equal (sum (app l1 l2)) (+ (sum l1) (sum l2))))
     
(natlistp l1)/\(natlistp l2)=> (equal (sum (app l1 l2)) (+ (sum l1) (sum l2)))

If you make use of it, you must cite it ("Thm. 1"), and provide a
substitution that shows how you apply it.

Conjecture:

(implies (natlistp x)
     (and (implies (endp x)
               (equal (sum (rev x)) (sum x)))
          (implies (and (natp a)
                (equal (sum (rev x)) (sum x)))
               (equal (sum (rev (cons a x))) (sum (cons a x))))))
              
---------
a:(natlistp x)
b:(endp x)
c:(equal (sum (rev x)) (sum x))
d:(and (natp a)
                (equal (sum (rev x)) (sum x)))
e:(equal (sum (rev (cons a x))) (sum (cons a x)))
a => ((b => c) /\ (d => e))
 

so, we can prove a=> (b=>c) and a=> (d=>e)

first part: a=>(b=>c) 
= a/\b=> c

c1: (natlistp x)




For (b => c), 
c2:(endp x)

(rev x)
={The definition of rev}
  nil
 
For c, left hand side:
(sum nil)
={The definition of sum}
0

right hand side:
(sum nil)
={The definition of nil}
=0


left side = right side

a=>(b => c) = t

second part:

a=> (d=>e)

For a=>(d => e)
=a/\d=>e

c3: (natp a) 
c4:(equal (sum (rev x)) (sum x))
c5:(natlistp x)

LHS:
(equal (sum (rev (cons a x))) (sum (cons a x)))
For left hand side:
  (sum (rev (cons a x))
= {The definition of rev,c5,axiom if}
  (sum (app (rev (rest (cons a x))) (list (first (cons a x)))))
= {The definition of cons,rest,first}
  (sum (app (rev x) (list a)))
= {Thm.1, c1, c3,c5,def natlistp,rev}
  (+ (sum (rev x)) (sum (list a)))
= {The definition of sum,natlistp,if, axiom first,}
(+ (+ (first (list a)) (sum (rest (list a)))) (sum (rev x)))
={axiom first, rest,def sum}
 (+ (+ a (sum nil)) (sum (rev x)))
= {def sum, nil,axiom if}
  (+ (+ a 0) (sum (rev x)))
={Arithmetic}
  (+ a (sum (rev x)))
={def rev,c5,c4}
  (+ a (sum x))


 
For right hand side:
RHS:
(sum (cons a x))
= {The definition of sum, c3, c1}
(+ a (sum x))


so, RHS is euqal to LHS,



...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

2.

Consider the following conjecture:

(and (implies (equal n 1)
          (implies (evenp (fib n))
               (and (not (evenp (fib (- n 1))))
                (not (evenp (fib (+ n 1)))))))
     (implies (and (natp n)
           (>= n 2)
           (implies (evenp (fib n))
                (and (not (evenp (fib (- n 1))))
                 (not (evenp (fib (+ n 1)))))))
          (implies (evenp (fib (+ n 1)))
               (and (not (evenp (fib    n  )))
                (not (evenp (fib (+ n 2))))))))

(Note: this conjecture is used in proving, by induction, that whenever
fib(n) is even, both fib(n-1) and fib(n+1) are odd. That is, the Fibonacci
sequence defined by the fib function does not contain two consecutive even
numbers.)

Prove this conjecture using equational reasoning.

Hint: the proof has four parts!



-------

a:(equal n 1)
b:(evenp (fib n))
c:(and (not (evenp (fib (- n 1))))
                (not (evenp (fib (+ n 1)))))
                
d: (natp n)
e:  (>= n 2)
f:(evenp (fib n))
g:(and (not (evenp (fib (- n 1))))
                 (not (evenp (fib (+ n 1)))))
                
h: (evenp (fib (+ n 1)))
i:(and (not (evenp (fib  n)))
                (not (evenp (fib (+ n 2)))))
(a => (b => c)) /\ ((d /\ e /\ (f => g)) => (h => i))


Using induction to prove the case:


The base case :

(a => (b => c)) this part describe the base case for n=1

(a => (b => c))
= {proposional reasoning}
(a /\ b) => c

(= n 1) /\ (evenp (fib n)) => ~ (evenp (fib (- n 1))) /\ ~ (evenp (fib (+ n 1)))




Prove the base case:

Since if n=1 ,(fib 1)= 1 (The definition of fib), therefore (= n 1) /\ (evenp (fib n)) is always false.
The whole conjecture of base case is always true.



Inductive prove:

((d /\ e /\ (f => g)) 
= {proposional reasoning}
 d /\ e /\ f => g
 
(natp n) /\ (>= n 2) /\ (evenp (fib n)) => ~ (evenp (fib (- n 1))) /\ ~ (evenp (fib (+ n 1)))


Prove the inductive:
c1: (natp n)
c2: (>= n 2) 
c3: (evenp (fib n)) 

For left hand side of equal:
~ (evenp (fib (- n 1)))
Assume it can be false. Therefore (evenp (fib (- n 1))) should be true for some value.

 FOR N=2. SINCE WE HAVE ALREADY PROVE THAT IT SHOULD BE FALSE FROM THE BASE CASE.
 NEXT WE WANT TO PROVE THE CASE (> N 2)  
 (fib (- n 1))
 
 ={The definition of fib, c2}
 (+ (fib (- n 1))
       (fib (- n 2)))
= {Arithmetic, the sum of consequtive natural number should be odd}

Therefore it can't be even for any case. (Contradiction)

Therefore ~ (evenp (fib (- n 1))) is true.


For right hand side of equal:
~ (evenp (fib (+ n 1)))

We also use the same technic 
Assume it can be false. Therefore (evenp (fib (+ n 1))) should be true for some value.

FOR N=2. SINCE WE HAVE ALREADY PROVE THAT IT SHOULD BE FALSE FROM THE BASE CASE.
 NEXT WE WANT TO PROVE THE CASE (> N 2)  
 (fib (+ n 1))
 
 ={The definition of fib, c2}
 (+ (fib (- n 1))
       (fib (- n 2)))
= {Arithmetic, the sum of consequtive natural number should be odd}

Therefore it can't be even for any case. (Contradiction)

Therefore ~ (evenp (fib (- n 1))) is true.


Therefore the inductive case is also true.



From the base case and inductive proof:
we can know that (natp n) => ~ (evenp (fib (- n 1))) /\ ~ (evenp (fib (+ n 1)))


What we want to prove is that:

(h => i)

(evenp (fib (+ n 1))) => (and (not (evenp (fib  n)))
                (not (evenp (fib (+ n 2)))))
(evenp (fib (+ n 1))) => ~ (evenp (fib n)) /\ ~ (evenp (fib (+ n 2)))


Since we know (natp n) => ~ (evenp (fib (- n 1))) /\ ~ (evenp (fib (+ n 1)))

From the arithmetic, we can know that 
(natp (+ n 1)) => ~ (evenp (fib n)) /\ ~ (evenp (fib (+ n 2)))

Since it is always true. Therefore whatever the fib of this number is. The statement is always true.




...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

3.

Prove the following using equational reasoning:

(implies (and (listp x) (listp y) (equal (len x) (len y)))
         (and (implies (endp x)
                       (equal (len (ziplists x y))
                              (len x)))
              (implies (and (consp x)
                            (equal (len (ziplists (rest x) (rest y)))
                                   (len (rest x))))
                       (equal (len (ziplists x y))
                              (len x)))))
                              
                              
 a:(and (listp x) (listp y) (equal (len x) (len y)))
b:(implies (endp x) (equal (len (ziplists x y)) (len x)))
c:(and (consp x) (equal (len (ziplists (rest x) (rest y))) (len (rest x))))
d:(equal (len (ziplists x y)) (len x))

The equation we want to prove:
a => (b /\ (c => d))
From a, we can know 
c1:(listp x)
c2:(listp y)
c3:(equal (len x) (len y))






Prove b:
(implies (endp x) (equal (len (ziplists x y)) (len x)))
seperate b to two parts e and f
e:(endp x)
f:(equal (len (ziplists x y)) (len x))
a=>(e => f)
=a/\e=>f

c1:(listp x)
c2:(listp y)
c3:(equal (len x) (len y))
c4: (endp x)

f
={ c3, c4, the definition of ziplist}

= (equal (len nil) (len nil))

={The definition of len}

(equal 0 0)

={Arithmetic}
t


(for c => d)
for c:
(and (consp x) (equal (len (ziplists (rest x) (rest y))) (len (rest x))))

context for (c => b)
a=>(c=>d)
=a/\c=>d


c5: (consp x)
c6: (equal (len (ziplists (rest x) (rest y))) (len (rest x)))

for d: (equal (len (ziplists x y)) (len x))
CONDITION 1: IF (REST X) IS NOT EMPTY:
= {The definition of ziplist,c5}
(equal (len (cons (list (first x) (first y)) (ziplists (rest x) (rest y))))
       (len x))
= {The definition of len,the definition of rest}

  (equal (+ 1 (len (ziplists (rest x) (rest y)))) (+ 1 (len (rest x))))
  
= {c1,c2,c3,c5,c6,arithmetic}
 since (equal (len (ziplists (rest x) (rest y))) (len (rest x))) when each part +1
 they should get the same answer.
 
 
 CONDITION 2: IF (REST X) IS EMPTY
 = {The definition of ziplist, The definition of len, arithmetic}
 (equal 0 0)
 It is also always true.
 
 
 therefore it is true.
                             
                              

...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

4.

Consider the following conjecture.

(implies
  (and (consp l1)
       (in (first l1) l2)
       (implies (subsetp (rest l1) l2)
                (equal (intersect (rest l1) l2) (rest l1))))
  (implies (subsetp l1 l2)
           (equal (intersect l1 l2) l1)))

Convince yourself that the conjecture satisfies the input contracts of all
functions occurring in it.

(a) Use propositional reasoning to simplify the conjecture into an
implication with a conjunction of hypotheses as antecedent, and an
``atomic'' consequent (i.e., one that contains no Boolean operators inside
it).



a:(consp l1)
b:(in (first l1) l2)
c:(subsetp (rest l1) l2)
d:(equal (intersect (rest l1) l2) (rest l1))
e:(subsetp l1 l2)
f:(equal (intersect l1 l2) l1)

a /\b/\(c=>d)=>(e=>f)

= a/\b/\(c=>d)/\e=>f



(consp l1)/\(in (first l1) l2)/\((subsetp (rest l1) l2)=>(equal (intersect (rest l1) l2) (rest l1)))/\(subsetp l1 l2)
=> (equal (intersect l1 l2) l1)


...

(b) Extract the context from the conjecture.
c1: (consp l1) 
c2: (in (first l1) l2)
c3: (subsetp (rest l1) l2)=>(equal (intersect (rest l1) l2) (rest l1))
c4: (subsetp l1 l2)




...

(c) Derive additional context that may be useful in proving the conjecture.
Remember to justify each such derived context.
c5: (listp l2) {def intersect, subsetp,in}

c6: ~ (endp l1) {c1}

c7:(subsetp (rest l1) l2) {c1}

...

(d) Prove the conjecture using equational reasoning. Be sure to justify
each step.
((intersect l1 l2) = l1)
LHS:(intersect l1 l2)
= {c6,c2,c5, def intersect}
  (cons (first l1) (intersect (rest l1) l2))
= {c3,c7}
  (cons (first l1) (rest l1))
= {def cons,first, rest}
  l1
  
 RHS:l1
 so we prove the LHS is equal to RHS


...

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fibonacci -- Made Efficient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We reduce the amount of testing done by ACL2 while it admits a function:
(acl2::acl2s-defaults :set acl2::subgoal-timeout 1)
;; Now recall the definition of the fib function from above. We also define
;; the type natlist, for later use.

(defunc fib (n)
  :input-contract (natp n)
  :output-contract (natp (fib n))
  (if (<= n 1)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defdata natlist (listof nat))

#|

This is an elegant and very easy to read definition. But it is also very
slow. To see why, we trace the recursive calls to fib when processing
input. This is done as follows:

|#

(acl2::trace! fib)
(acl2::set-guard-checking :none)

#|

(Hint: you can use (acl2::untrace$ fib) to stop tracing the function.)

Now try fib using some _small_ inputs. Start with n=1, n=2, to get a
feeling for the output trace produces. A line of the form
 > (FIB 1)

indicates that fib was called recursively with argument 1, whereas

 < (FIB 1)

indicates that a recursive call to fib with argument 1 was completed.

In the evaluation of (fib  5), how many times is fib called on argument 1 ? 5 times
In the evaluation of (fib 10), how many times is fib called on argument 1 ? 55 times

Hint: you can use the Eclipse editor to count occurrences of certain text
strings, or you can copy the output of trace into your favorite alternative
editor.

Compare the above numbers with the values (fib 5) and (fib 10). What do you
find? 

the numbers with the value (fib 1) is equal to the vaule of (fib n).

You saw how long the trace output of (fib 10) is -- for a fairly small
input of 10. Let's see whether we can make fib more efficient. Our fib-fast
function will be hard for ACL2 to prove terminating, so we write this
function in program mode:

|#

:program

#|

The idea is as follows. First write a function fib-help that, for input n,
computes the _list_ of Fibonacci numbers 0,1,1,2,3,5,8,... in _descending_
order from (fib n) down to (fib 0) = 0. See tests below, and also note the
output contract, which is provided for you. Provide 3 more tests.

To minimize the number of recursive calls required to evaluate (fib-help n),
you MUST use (let ...)  whenever you need the result of a recursive call
several times. Your solution will be considered incorrect if your code contains
several calls to fib-help with the same arguments.

|#

(defunc fib-help (n)
  :input-contract (natp n)
  :output-contract (and (natlistp (fib-help n)) (equal (len (fib-help n)) (+ n 1)))
  (cond ((equal n 0) (cons 0 nil))
        ((equal n 1) (cons 1 (cons 0 nil)))
        (t (let ((l (fib-help (- n 1))))
             (cons (+ (first l) (second l)) l))))

)

(check= (fib-help 0) '(0))
(check= (fib-help 1) '(1 0))
(check= (fib-help 3) '(2 1 1 0))
(check= (fib-help 4) '(3 2 1 1 0))
(check= (fib-help 5) '(5 3 2 1 1 0))

;; Now write a non-recursive function fib-fast, with contracts as for the
;; (slow) fib function, which calls fib-help to compute (fib n).

(defunc fib-fast (n)
  :input-contract (natp n)
  :output-contract (natp (fib-fast n))
  (first (fib-help n)))

;; Now let's see whether fib-fast deserves that name. Turn on tracing for
;; the helper function (fib-fast itself is not recursive):

(acl2::trace! fib-help)

#|

In the evaluation of (fib-fast  5), how many times is fib-help called on argument 1 ? 1 time
In the evaluation of (fib-fast 10), how many times is fib-help called on argument 1 ? 1 time

Compare your results to those obtained with (fib n).

You can also try fib and fib-fast on input 100. Hint: try fib-fast first!

|#
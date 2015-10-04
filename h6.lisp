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


CS 2800 Homework 6 - Fall 2014
Yueling Qin && Zeqing Zhang
Student names: This homework is done in pairs. Put BOTH names here.

Technical instructions:

- open this file in ACL2s BEGINNER mode as hw06.lisp

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish some problems, comment
  them out. The same is true for any English text that you may add. This
  file already contains many comments, so you can see what the syntax is.

- when done, save your file and submit it as hw06.lisp.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Terminates?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

For the following problems, determine whether the given function f
terminates. If it does, provide a measure function m such that

1. m has the same arguments and the same input contract as f.
2. m's output contract is (natp (m ...))
3. m is admissible.

Unless clearly stated otherwise, you also need to prove that

4. on every recursive call of f, given <ic> and under the conditions that
   lead to that call, m applied to the arguments in the call is less than m
   applied to the original inputs.

You should do this proof as shown in class (which is also the way we will
expect you to prove termination in exams):

- write down the propositional logic formalization of the above condition 4
- simplify the formula
- conclude the formula is valid.

Unless clearly stated otherwise, you need to follow these steps for EACH
recursive call separately.

If you determine f does not terminate, provide an input on which the
function runs forever.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc z (l1 l2)
  :input-contract (and (listp l1) (listp l2))
  :output-contract (listp (z l1 l2))
  (cond ((endp l1)             ())
        ((< (len l1) (len l2)) (z (cons 1 l1) (rest l2)))
        (t                     (z (rest l1) (cons 2 l2)))))

The function z doesn't terminate.
If I give a input (z (list 1 2) (list 1 2 3)) -> (z (list 1 1 2) (list 1 2 3)) -> (z (list 1 2) (list 2 1 2 3))
->(z (list 1 1 2) (list 2 1 2 3)) ... It will never stop but imcreasint forever.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc h (a b)
  :input-contract (and (integerp a) (natp b))
  :output-contract (natp (h a b))
  (cond ((> b a)     b)
        ((equal b 0) (h (- a 1) b))
        (t           (h a (+ b 1)))))    
 
The function is terminated.
1. The measure function is :
(defunc m (a b)
  :input-contract (and (integerp a) (natp b))
  :output-contract (natp (m a b))
  (abs (- 2a b)))
      
(1). (and (integerp a) (natp b)) ^ ~(> b a) ^ (equal b 0) => ((m a b) > (m ((- a 1) b))
= (and (integerp a) (natp b)) ^ ~(> b a) ^ (equal b 0) => (abs (- (* 2 a) b)) > (abs (- (* 2 (- a 1)) b))
=(- 2a b) > (- (- 2a b) 2)
Since a is greater than b.
From the formular we can easily see that the right side is true.

(2). (and (integerp a) (natp b)) ^ ~(> b a) ^ ~(equal b 0) 
=> ((m a b) > (m a (+ b 1)))
= ((and (integerp a) (natp b)) ^ ~(> b a) ^ ~(equal b 0)) 
=> (abs (- (* 2 a) b)) > (abs (- (* 2 a) (+ b 1)))
a>=b, so 2a>= b.{def abs,ic}
so 2a>= b =>2a +1>= b+1 => 2a> b+1
(abs (- (* 2 a) b)) > (abs (- (* 2 a) (+ b 1))) = (- 2a b) > (- (- 2a b) 1)
From the formular we can easily see that the right side is true. 

Therefore there is a measure function for this function and it is terminated.
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc g (n)
  :input-contract (natp n)
  :output-contract t
  (cond ((equal n 0) 0)
        ((> n 3)     (+ 1 (g (- n 1))))
        (t           (- 3 (g (* 2 n))))))
It won't be terminated. The counterexample is (g 4).
(g 4) -> (+ 1 (g 3)) -> (+ 1 (- 3 (g 6))) -> (+ 1 ( - 3 (g 5)))
-> (+ 1 (- 3 (g 4)))(HERE (g 4) IS REPEATED WHICH MEANS IT WILL LOOP FOREVER)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc h (l p)
  :input-contract (and (listp l) (consp p))
  :output-contract (booleanp (h l p))
  (cond ((> (len p) (len l)) t)
        ((endp (rest p))     (h (rest l) p))
        (t                   (h l (cons nil p)))))
It won't be terminated. The counterexample is (h (list 1 2 3) (list 1 2)) ->
(h (list 1 2 3) (list nil 1 2)) -> (h (list 1 2 3) (list nil nil 1 2))...
It will loop forever.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (x y)
  :input-contract (and (listp x) (integerp y))
  :output-contract (natp (f x y))
  (cond ((and (endp x) (equal y 0)) 0)
        ((and (endp x) (< y 0))     (+ 1 (f x (+ 1 y))))
        ((endp x)                   (+ 1 (f x (- y 1))))
        (t                          (+ 1 (f (rest x) y)))))

If terminating, prove condition 4 only for the *second* recursive call
(guarded by (endp x) ).
ic/\ ~(and (endp x) (equal y 0))/\(~(and (endp x) (< y 0))) /\ (endp x)

It is terminated.
The meassure functin is 

(defunc m (x y)
   :input-contract (and (listp x) (integerp y))
   :output-contract (natp (m x y))
   (+ (len x) (abs y)))
   

c1:(and (listp x) (integerp y))
c2:~(and (endp x) (equal y 0))
c3:(~(and (endp x) (< y 0)))
c4:(endp x)
ic/\ ~(and (endp x) (equal y 0))/\(~(and (endp x) (< y 0))) /\ (endp x)
=>(m x (- y 1)) < (m x y)


(m x (- y 1))
={def m,ic}
= (+ (len x) (abs (- y 1)))


(m x y)
={def m, ic}
= (+ (len x) (abs y))  


(+ (len x) (abs (- y 1)))< (+ (len x) (abs y))
={Arithmetic}
=(abs (-y 1)) < (abs y))
={c2,c3}=> (y>0), Arithmetic,def abs
=(- y 1) < y or ((-y) +1) < y
we know y-1< y is true, 

another situation that y-1<0, 
so we need to prove 1-y <y
1< 2y=> 1/2< y
we have y>0 &&(integerp y), so y>=1,
so (abs (- y 1))< (abs y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (x a)
  :input-contract (and (integerp  x) (listp a))
  :output-contract (natp (f x a))
  (cond ((< x 0)   (f (len a) (app a a)))
        ((endp a)  0)
        (t         (+ 1 (f x (rest a))))))

The measure function is 
(defunc m (x a)
    :input-contract (and (integerp x) (listp a))
    :output-contract (natp (m x a))
    (if (< x 0)
    (+ (* (len a) 2) 1)
    (len a)))
    
 A.(and (integerp  x) (listp a)) /\ (< x 0) =>(m x a) > (m (len a) (app a a))
 B.(and (integerp x) (listp a)) /\ (~ (< x 0))/\(~ (endp a)) => (m x a) > (m x (rest a))
 
 A For (m x a) > (m (len a) (app a a))
     = {By the definition of measure function,app, (< x 0),def len}
       (+ (* (len a) 2) 1) > (len (app a a))
     ={Arithmetic,def app,ic}
      (+ (* (len a) 2) 1) > (* (len a) 2)
  
 B For (m x a) > (m x (rest a))
      = {By the definition of measure function, (> x 0), (~ (endp a))}
      =(len a) > (len (rest a))
      ={~(endp a), ic, def len, rest}
      (len a) > (- (len a) 1)
     
 

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Terminates!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

The following functions terminate. Provide a measure function m
and prove the conditions outlined above in section "Terminates?".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (x y z)
  :input-contract (and (natp x) (natp y) (listp z))
  :output-contract (natp (f x y z))
  (cond ((equal x 0) (+ y (len z)))
        ((endp z)    (+ x y))
        ((<= y 1)    (+ x (f (- x 1) 0 (rest z))))
        ((< x y)     (f x (- y 2) z))
        (t           (f x y (rest z)))))

Prove condition 4 only for the *first* recursive call (guarded by (<= y 1) ).

The measure functin is:
(defunc m (x y z)
   :input-contract (and (natp x) (natp y) (listp z))
   :output-contract (natp (m x y z))
   (+ x (+ y (len z))))
   
To prove: 
((and (and (and (and (and (natp x) (natp y)) (listp z))  (~ (equal x 0))) (~ (endp z))) (<= y 1))) 
=> (m x y z) > (m (- x 1) 0 (rest z))
(m (- x 1) 0 (rest z)) 
={def m, (~ (endp z)) and ic}
= (- x 1) + 0 + (len (rest z))  


(m x y z)
={def m, ic}
= (x + y + (len z))


first part: (- x 1) < x

second part:
(natp y)and (y <= 1) 
->  0<= y <= 1, so, y>=0, the second argument may decrease

final part: (len (rest z)) < (len z), {def len, (~ (endp z))}

so the sum of thress value must be terminate.
(- x 1) + 0 + (len (rest z))  < (x + y + (len z))

.....



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc h (a b c)
  :input-contract (and (natp a) (integerp b) (listp c))
  :output-contract (natp (h a b c))
  (cond ((equal a 0) (len c))
        ((endp c)    (+ a b))
        ((<= b 1)    (+ a (h (- a 1) (+ b 1) (rest c))))
        ((< a b)     (h a (- b 2) c))
        (t           (h a b (rest c)))))

Prove condition 4 only for the *first* recursive call (guarded by (<= b 1) ).

The measure function is 
(defunc m (a b c)
  :input-contract (and (natp a) (integerp b) (listp c))
  :output-contract (natp (m a b c))
  (+ a (+ (abs b) (len c))))
  
 Prove: (and (natp a) (integerp b) (listp c)) /\(? (equal a 0))  /\(~ (endp c)) /\ (<= b 1) 
 =>  (m a b c) > (m (- a 1) (+ b 1) (len (rest c)))
 
      = {the definition of the measure function}
       (+ a (+ (abs b) (len c)))) >  (+ (- a 1) (+ (abs (+ b 1)) (len (rest c))))
      = {Arithmetic}
        (+ a (+ (abs b) (len c)))) >  (+ (- a 1) (+ (abs (+ b 1)) (len (rest c))))
      if b is negative number
     (+ (+ a b) c) > (- (+ (+ a b) c) 3)
     if b is positive number or 0.
     (+ (+ a b) c) > (- (+ (+ a b) c) 1)
     
     That is true. Therefore the function is terminated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defunc f (a b)
  :input-contract  (and (natp a) (natp b))
  :output-contract (natp (f a b))
  (cond
    ((and (equal a 0) (equal b 0))  0)
    ((< a b)                        (f b a))
    (t                              (f b (- a 1)))))

The measure function is 
(defunc m (a b)
  :input-contract  (and (natp a) (natp b))
  :output-contract (natp (m a b))
  (if (< a b)
      b
      a))
      
      
      

  
Prove: (and (natp a) (natp b)) /\(~(and (equal a 0) (equal b 0)))/\ (< a b) 
=> (m a b) > (m b a)
   = {definition of m,ic}
   b > a
   
  
 (and (natp a) (natp b)) /\ (~(and (equal a 0) (equal b 0)))/\~ (< a b) 
 => (m a b) > (m b (- a 1))
 = {definition of m,ic,?(< a b)}
 
 so, we have ~(< a b), it men a>=b, 
 if (m b (- a 1)), b< (- a 1)
 (m b (- a 1)) = (- a 1)
 we can prove : a > (- a 1)
 
 
 if b>= (- a 1)
 (m b (- a 1)) = b
 a > b based on condition a>=b and b> = (- a 1), so a not equal b,
     
 It is true. The function is terminated.
   
 
   
   
   
   

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Termination arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(a) Albert is considering the following function:

    (defunc wicked (a b c)
      :input-contract (and (integerp a) (posp b) (integerp c))
      :output-contract (integerp (wicked a b c))
      (cond ((>= a b) a)
            ((>= b c) b)
            ((< c 0)  (wicked (+ a b) b c))
            (t        (wicked a b (+ a b)))))

He claims that this function will be admitted by ACL2s since its body is a
legal expression satisfying all body contracts, and the output contract is
trivially satisfied as well. But he forgot something! Show that the
function does not (always) terminate, by giving an input in the form of
concrete values a,b,c that satisfies the input contract but causes an
infinite execution.

(2 3 4) -> (2 3 5) -> (2 3 5) -> (2 3 5) -> ... It will always be (2 3 5)
never terminated.

(b) Albert admits there were several typos in his definition. His new
proposal is (read carefully!):

(defunc wicked (a b c)
  :input-contract (and (posp a) (posp b) (integerp c))
  :output-contract (integerp (wicked a b c))
  (cond ((>= a b) a)
        ((< c 0)  (wicked (+ a b) b c))
        (t        (wicked a b (- a b)))))

Prove that this function terminates.

Hint: First run function wicked on a few well-chosen test cases (exercising
all clauses of the cond) and see what happens. How long can chains of
recursive calls to this function actually be?

The measure function should be :
(defunc m (a b c)
  :input-contract (and (posp a) (posp b) (integerp c))
  :output-contract (natp (m a b c))
  (if (>= a b)
      a
      (if (< c 0)
      (+ (+ a b) 1)
      (+ (+ a b) 2)))

)

Prove the correctness:
(and (posp a) (posp b) (integerp c))/\~(>= a b)/\(< c 0) 
=> (m a b c) > (m (+ a b) b c)

(m a b c) 
={The definition of m,~(>= a b)/\(< c 0)}
(+ (+ a b) 1)

(m (+ a b) b c)
={The definition of m, (+ a b)>= b}
(+ a b)

(+ (+ a b) 1) > (+ a b)

(and (posp a) (posp b) (integerp c))/\~(>= a b)/\~(< c 0)
=> (m a b c) > (m a b (- a b))
(m a b c)
={{The definition of m,~(>= a b)/\~(< c 0)}
(+ (+ a b) 2)

(m a b (- a b))
={The definition of m,(- a b)< 0}
(+ (+ a b) 1)

(+ (+ a b) 2) > (+ (+ a b) 1)

These things are true. Therefore it is terminated.

Be sure that m would be admitted by ACL2s. Then prove that, on every
recursive call of wicked, the value of m decreases, under the conditions of
that recursive call.



|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Consider the following function definition:

(defunc count (i j c)
  :input-contract (and (natp i) (natp j) (natp c))
  :output-contract (natp (count i j c))
  (cond ((and (equal i 0) (equal j 0)) c)
        ((> j 0) (count i (- j 1) (+ c 1)))
        (t       (count (- i 1) 9 (+ c 1)))))

(a) Consider the call (count 2 3 0). Below are the first three argument
tuples that appear in recursive calls to count. Show the next three:

(2 3 0)  (2 2 1)  (2 1 2) -> (2 0 3) -> (1 9 4) -> (1 8 5)->...

What does (count 2 3 0) evaluate to? 21

(b) Using the insights generated in (a), show that function count
terminates, by defining a measure function, and proving condition 4 as
outlined above.

(defunc m (i j c)
  :input-contract (and (natp i) (natp j) (natp c))
  :output-contract (natp (m i j c))
  (+ (* i 10) j))
  
  )


Proof that the value of m decreases on recursive calls.

(and (natp i) (natp j) (natp c))/\ ~(and (equal i 0) (equal j 0))/\(> j 0) 
=> (m i j c) > (m i (- j 1) (+ c 1))
 (m i j c)
 = {The definition of m,ic}
  (+ (* i 10) j)
  
 
  
  (m i (- j 1) (+ c 1))
  ={The definition of m,ic}
  (+ (* i 10) (- j 1)
 
 (m i j c) > (m i (- j 1) (+ c 1))
 
(and (natp i) (natp j) (natp c))/\ ~(and (equal i 0) (equal j 0))/\~(> j 0) 
=> (m i j c) > (m (- i 1) 9 (+ c 1))
 (m i j c)
 = {The definition of m,~(> j 0)}
  (+ (* i 10) j)

  (m (- i 1) 9 (+ c 1))
  = {The definition of m, ~(> j 0)}
  (+ (*  (- i 1) 10) 9)
  = {Arthemetic}
  
  Since 10i > 10i - 1
  Therefore the statement is true. 
  The finction is terminated.
  


|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Given a (not necessarily admissible) ACL2s function f:X->Y and an element x
of X, let args_f(x) denote the set of elements of the ACL2s universe that f
is called upon during the evaluation of f(x) (including x itself). For
example, if f is len and x is (1 2), then args_f(x) = {(1 2),(2),()}.

(a) Consider

(defunc f (x)
  :input-contract (natp x)
  :output-contract (natp (f x))
  (cond ((<= x 1) 0)
        ((evenp x) (f (/ x 2)))
        (t         (f (- x 1)))))

Function evenp returns T whenever its integer argument is even.

Does this function terminate?

terminate.

(b) For the same function as in (a), determine the set args_f(5). How many
elements does this set have?
 
{(5), (4), (2), (1)} 


(c) Consider

(defunc f (x)
  :input-contract (natp x)
  :output-contract (natp (f x))
  (if (<= x 1)
    0
    (f (+ x 1))))

Does this function terminate?

not terminate, f(2)-> f(3) -> f(4) -> f(5)....
It will be always inreaseing by 1 forever.

(d) For the same function as in (c), determine the set args_f(5). How many
elements does this set have?

{5, 6, 7, 8 , 9 , 10, 11, 12,......}
There are infinite elements in (c).

(e) Give an example of a function f and an input x such that

1. x satisfies f's input contract
2. f does not terminate on input x
3. args_f(x) is *finite*.

Clearly define your (inadmissible) function f, specify input x, explain why
f does not terminate on input x, and write down all elements of args_f(x).

(defunc f (x)
  :input-contract (natp x)
  :output-contract (natp (f x))
  (if (= x 5)
  (f x)
  (f (+ x 1))))
  
  If the argument is 5. Then it will alwasys recursive in (f 5) and the argument is only one element
  5. { (5) }

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A TERMINATION TESTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

The subject of this exercise is to study the problem of designing a
function that automatically checks whether another (recursively defined)
function terminates.

But wait -- we already know this problem is undecidable! That means there
is no ACL2s function that always determines correctly whether some given
other function f terminates.

We will therefore only solve a simple version of this problem: our function
will determine whether a given function f terminates in a predetermined
number of "steps". We will see shortly what "steps" means.

We begin with an example.

1. Define a function f that takes a positive natural number and is defined
mathematically as follows:

         1        if n is 1
       /
f(n) = - f(n/2)   if n is even
       \
         f(3*n+1) if n is greater than 1 and odd.

Here is how to read that notation: first observe that the three cases on
the right are mutually exclusive, i.e. every positive natural number fits
into exactly one case. Given n, determine which case it fits in. The
expression associated with that case determines f(n).

Define this function in ACL2s, in :program mode. Recall that (posp n)
recognizes positive natural numbers.

|#

:program

(defunc f (n)
  :input-contract (posp n)
  :output-contract (equal (f n) 1)
  (cond ((equal n 1) 1)
        ((posp (/ n 2)) (f (/ n 2)))
        (t    (f (+ (* 3 n) 1))))

)

; What does this function return, if anything?

; 1

; Write at least 3 check= tests that (should) confirm your conjecture.


(check= (f  8) 1)
(check= (f  5) 1)
(check= (f 40) 1)
(check= (f 4) 1)


#|

You can think of this function as generating a sequence of positive natural
numbers, namely the numbers that f is called on recursively. For example:

f(8) = f(4) = f(2) = f(1) = 1

To get a feel for f, write down the call sequences for the following
initial arguments, until the recursion ends:

f(10) = f(5) = f(16) = f(8) = f(4) = f(2) = f(1) = 1.
f( 7) = f(22) =f(11) = f(34)= f(17) = f(52) =f(26) = f(13)= f(40) = f(20)
=f(10)=f(5) = f(16) = f(8) = f(4) = f(2) = f(1) = 1.

Hint: try out (acl2::trace! f)

The reason we have defined this function in :program mode is that ACL2s
cannot prove its termination. In fact, nobody knows whether this function
always terminates! You can read the whole story on Wikipedia. Search for
Collatz Conjecture.

Think about why it is (apparently) difficult to define a measure function
for f. Try it! (No response is required in your homework file.)

2. Modify f into a function f-c that takes not only n but also two other
arguments, count and limit, which are natural numbers such that count <=
limit. The idea is that count counts the number of recursive calls we have
to make to evaluate (f n). If that number exceeds limit, we abort the
entire computation and return the symbol ! (exclamation mark). Otherwise we
proceed as in f. The argument limit is a constant -- it is not changed in
recursive calls. Think about what values to pass to count in recursive
calls.

f-c : Pos x Nat x Nat -> Pos union {!}

The input contract MUST enforce the arithmetic relationship between count
and limit mentioned above. The output contract MUST state that f-c returns
a positive natural number OR the symbol ! .

|#

(defunc f-c (n count limit)
  :input-contract (and (posp n) (natp limit) (natp count))
  :output-contract (or (posp (f-c n count limit)) (equal (f-c n count limit) '!))
    (cond ((equal limit count) '!)
          ((equal n 1) 1)
          ((posp (/ n 2)) (f-c (/ n 2) (+ 1 count) limit))
          (t   (f-c (+ (* 3 n) 1) (+ 1 count) limit)))
  

)

(check= (f-c 8 0 2) '!)
(check= (f-c 8 0 3) '!)
(check= (f-c 8 0 4)  1)

; Write at least 3 more tests.
(check= (f-c 20 4 4) '!)
(check= (f-c 3 4 4) '!)
(check= (f-c 1 1 0) 1)
#|

3. Define a function f-terminates that takes two arguments: a positive
natural n and a natural number limit, and checks whether (f n) returns
after at most limit recursive calls. f-terminates returns a Boolean.
Obviously, in the body of f-terminates use f-c instead of f .

f-terminates : Pos x Nat -> Bool

|#

(defunc f-terminates (n limit)
  :input-contract (and (posp n) (natp limit))
  :output-contract (booleanp (f-terminates n limit))
  (equal (f-c n 0 limit) 1))

(check= (f-terminates 8 2) nil)
(check= (f-terminates 8 3) nil)
(check= (f-terminates 8 4) t)

; Write at least 3 more check= tests
(check= (f-terminates 6 15) t)
(check= (f-terminates 6 18) t)
(check= (f-terminates 6 14) t)

#|

4. Find the number *limit* of recursive calls that it takes for f to
terminate on input

1267650600228229401496703205376

That is, the following two tests must pass with your number *limit*,
which you should enter in place of the ... below:

|#

(defconst *limit* 101)

(check= (f-terminates 1267650600228229401496703205376 (- *limit* 1)) nil)
(check= (f-terminates 1267650600228229401496703205376    *limit*   ) t  )

#|

Hint: to determine *limit*, consider the trace mechanism mentioned above,
or binary search for the optimal parameters to f-c.

What is the mathematical relationship between the (large) input to f shown
above, and the number *limit* you found? Answer and explain in English.

*limit* is ...

5. Remember that f may not terminate on all inputs: nobody has been able to
prove that it does. Now observe that function f-terminates itself always
does terminate, even if f does not! The reason is that, instead of f, we
are using f-c within f-terminates, and f-c only recurs a given number of
times.

We can therefore use ...-terminates to test termination of functions even if
we _know_ they do not terminate. Consider:

|#

(acl2::acl2s-defaults :set acl2::testing-enabled nil) ; this turns off contract testing

(defunc g (n)
  :input-contract (integerp n)
  :output-contract (integerp (g n))
  (if (equal n 0)
    0
    (+ 1 (g (- n 1)))))



#|

This function behaves like the identity on natural number inputs, and
diverges on all others. Try (g 10), (g 0), (g -1). What does ACL2s return on
the final example?
(g 10) -> 10
(g 0)=0
(g -1) ->(+1 (g -2)) ->(+ 2 (g -3)).....

Now modify g into a function g-c that relates to g like f-c does to f. That
is, g-c counts the number of recursive calls we have to make to evaluate (g
n). If that number exceeds a given limit, we abort the entire computation and
return the symbol ! (exclamation mark).

g-c : Int x Nat x Nat -> Int union {!}

This function is a bit more tricky than f, as it is not tail-recursive!
'let' is your friend.

|#

(defunc g-c (n count limit)
  :input-contract (and (integerp n) (natp count) (natp limit))
  :output-contract (or (natp (g-c n count limit)) (equal '! (g-c n count limit)))
  (cond ((equal count limit) '!)
        ((equal n 0) count)
        (t  (g-c (- n 1) (+ 1 count) limit)))

)

(check= (g-c 0 1 1) '!)
(check= (g-c 0 1 2) 1)
(check= (g-c 1 1 2) '!)

#|

Now define a similar wrapper function g-terminates that tests termination
of g within a pre-specified number of recursive calls. In the body of
g-terminates use g-c instead of g .

g-terminates : Int x Nat -> Bool

|#

(defunc g-terminates (n limit)
  :input-contract (and (integerp n) (natp limit))
  :output-contract (booleanp (g-terminates n limit))
  (natp (g-c n 0 limit)))

(check= (g-terminates 10 10)  nil)
(check= (g-terminates 10 11)  t)
(check= (g-terminates -1 10)  nil) ; g does not terminate at all on this input
(check= (g-terminates -1 100) nil) ; g does not terminate at all on this input

; Write 2 more check= tests
(check= (g-terminates 8 9) t)
(check= (g-terminates 9 0) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Some final comments (no response required): In this exercise, for every
function f we wanted to termination-test, we had to write a new function
f-terminates. Wouldn't it be better to have a function terminatesp that
takes (the code of) another function as input and performs the steps above?
so we don't have to write another function ?-terminates every time, which
anyway differs from the others mostly in the name of the function that is
being tested?

Yes, that would be better. But there are a number of difficulties. An
obvious one is that the different functions we want to test may take
different numbers of arguments, and may return different types of values.
So how many arguments would terminatesp have? and what would be their
types? Not obvious.

Supporting such "generic" terminatesp functions requires quite heavy
programming language support, such as higher-order functions: functions
that take other functions as input and can run them on arbitrary arguments.
As we have often mentioned, ACL2s does not support higher-order functions,
since they make proving theorems much much harder.

Note that a true termination prover would have to be a higher-order
procedure, since it takes another procedure/function as input.

|#

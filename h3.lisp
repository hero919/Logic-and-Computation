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
;;NAME: YUELING QIN && ZEQING ZHANG





CS 2800 Homework 3 - Fall 2014

Student names: This homework is done in pairs. Put BOTH names here.

For this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw03.lisp

- make sure you are in Bare Bones mode. This is essential! Note that
  you can only change the mode when the session is not running,
  so that the correct mode before starting the session.

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish some problems, comment
  them out. The same is true for any English text that you may add. This
  file already contains many comments, so you can see what the syntax is.

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

- when done, save your file and submit it as hw03.lisp.

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A. Basic Boolean Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recall the following elementary Boolean functions:

(defunc booleanp (x)
  :input-contract t
  :output-contract (booleanp (booleanp x))
  (if (equal x t)
      t
    (equal x nil)))

; Note how we use the very function we are defining -- booleanp -- in the
; output contract of the definition

; Recall the definitions of 'and' and 'or' :
(defunc and (a b)
  :input-contract (if (booleanp a) (booleanp b) nil)
  :output-contract (booleanp (and a b))
  (if a b nil))

; Note how we DO NOT use the very function we are defining -- and -- in the
; input contract of the definition. What would happen if we changed the
; input contract to (and (booleanp a) (booleanp b)) ? Try it but do not
; submit the result -- it should fail.

(defunc or (a b)
  :input-contract (and (booleanp a) (booleanp b))
  :output-contract (booleanp (or a b))
  (if a t b))

(check= (or t nil)  t)
(check= (or t t) t)
(check= (or nil nil) nil)
(check= (or nil t) t)



; What happens if you evaluate (or 1 nil)? Try it. Here is what you should see:

#|

ACL2 Error in ACL2::TOP-LEVEL:  The guard for the function call (OR A B),
which is (AND (BOOLEANP A) (BOOLEANP B)), is violated by the arguments
in the call (OR 1 NIL).
See :DOC set-guard-checking for information about suppressing this
check with (set-guard-checking :none), as recommended for new users.
To debug see :DOC print-gv, see :DOC trace, and see :DOC wet.

|#

; When ACL2s reports a "guard" violation, as it did when you tried
; evaluating (or 1 nil), we violated the contract of a function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; oor: Boolean x Boolean x Boolean -> Boolean

; (oor a b c) implements the Boolean "a or b or c".

; Use the or function.

(defunc oor (a b c)
  :input-contract (and (and (booleanp a) (booleanp b)) (booleanp c))
  :output-contract (booleanp (oor a b c))
  (or (or a b) c))

(check= (oor t   nil t  ) t)
(check= (oor nil nil nil) nil)
(check= (oor nil t nil) t)
(check= (oor t t t) t)
(check= (oor nil t t ) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Provide an example of an expression containing an if that has three
; arguments as required for if but whose evaluation leads to a contract
; violation.
#|
(if (and 0 9) t nil)
|#

; The solution must be given inside a comment, to prevent ACL2 from
; complaining about your expression.

; ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; not: Boolean -> Boolean

; (not a) implements the Boolean "not a".

(defunc not (a)
  :input-contract (booleanp a)
  :output-contract (booleanp (not a))
  (if a nil t))

(check= (not t) nil)
(check= (not nil) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; follows-from: Boolean x Boolean -> Boolean

; (follows-from a b) implements the Boolean "b implies a".

; Use the implies function.

(defunc follows-from (a b)
  :input-contract (and (booleanp a) (booleanp b))
  :output-contract (booleanp (follows-from a b))
  (if b a t))

(check= (follows-from t   t)   t)
(check= (follows-from nil nil) t)
(check= (follows-from nil   t)  nil)
(check= (follows-from t nil) t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; iff: Boolean x Boolean -> Boolean

; (iff a b) implements the Boolean "a equivalent to b".

(defunc iff (a b)
  :input-contract (and (booleanp a) (booleanp b))
  :output-contract (booleanp (iff a b))
  (if a b (not b)))

(check= (iff t   t)   t)
(check= (iff nil nil) t)
(check= (iff t nil) nil)
(check= (iff nil t) nil)

#|
; Provide an example of an expression containing 'equal' whose evaluation
; does not lead to a contract violation, but when we replace the equal with
; 'iff', evaluation does lead to a contract violation.

(equal 0 nil)
|#





; Can you provide an example of an expression containing 'iff' whose
; evaluation does not lead to a contract violation, but when we replace the
; iff with 'equal', evaluation leads to a contract violation? If so,
; provide an example. If not, explain.


#|


Not possible. the guards (contract) of iff are more restrictive than those of equal,
so anytime one can call iff, one can also call equal.

|#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; minor : Boolean x Boolean x Boolean -> Boolean

; (minor a b c) is t if the minority of a,b,c is t, i.e. at most one of the
; three. Otherwise minor is nil. See test cases (and remember to provide your own).

(defunc minor (a b c)
  :input-contract (and (and (booleanp a) (booleanp b)) (booleanp c))
  :output-contract (booleanp (minor a b c))
  (or (or (and (not a) (not b))
          (and (not a) (not c)))
    (and (not b) (not c))))



(check= (minor t   nil t)   nil)
(check= (minor nil t   nil) t)
(check= (minor t t   nil) nil)
(check= (minor t t   t) nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; exactly-one: Boolean x Boolean -> Boolean

; (exactly-one a b) is t if exactly one of a and b is t, and nil otherwise.

; Hint: first build a truth table for this function, and then simply and express
; the result using Boolean connectives you have already defined (so that ACL2
; can accept your function).

(defunc exactly-one (a b)
  :input-contract (and (booleanp a) (booleanp b))
  :output-contract (booleanp (exactly-one a b))
  (if a
    (equal nil b)
    b))

(check= (exactly-one t   t) nil)
(check= (exactly-one t nil) t)
(check= (exactly-one nil nil) nil)
(check= (exactly-one nil t) t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; B. Basic Numerical Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; <= : Rational x Rational -> Boolean

; (<= a b) is t if a <= b, otherwise it is nil.

(defunc <= (a b)
  :input-contract (and (rationalp a) (rationalp b))
  :output-contract (booleanp (<= a b))
  (or (< a b) (equal a b)))

(check= (<= 1 2) t)
(check= (<= 4 2) nil)
(check= (<= -14 -9) t)
(check= (<= -2/1 8) t)
(check= (<= 0 0) t)
(check= (<= 0 1) t)
(check= (<= 1 -2) nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; <=<= : Rational x Rational x Rational -> Boolean

; (<= a b c) is t if a <= b <= c, otherwise it is nil.

(defunc <=<= (a b c)
  :input-contract (and (and (rationalp a) (rationalp b)) (rationalp c))
  :output-contract (booleanp (<=<= a b c))
  (and (<= a b) (<= b c)))


(check= (<=<= 1/4 1/3 1/2) t)
(check= (<=<= 1 2 3) t)
(check= (<=<= -1 0 -2) nil)
(check= (<=<= -3 9 1) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; abs: Rational -> Rational

; (abs a) returns the absolute value of the rational number a. (Consult a
; math book or Wikipedia if you don't know what the absolute value of a
; number is.)

(defunc abs (a)
  :input-contract (rationalp a)
  :output-contract (rationalp (abs a))
  (if (< a 0)
    (unary-- a)
    a))

(check= (abs 3) 3)
(check= (abs 0) 0)
(check= (abs -2) 2)
(check= (abs -10) 10)
(check= (abs -1/2) 1/2)
(check= (abs 3/5) 3/5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; natp: All -> Boolean

; (natp a) is t if a is a natural number, otherwise it is nil.

(defunc natp (a)
  :input-contract t
  :output-contract (booleanp (natp a))
  (if (integerp a)
    (< -1 a)
    nil))



(check= (natp 12) t)
(check= (natp 3/2) nil)
(check= (natp -23/2) nil)
(check= (natp -1) nil)
(check= (natp 0) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define))))))))
; - : Rational x Rational -> Rational

; (- a b) returns a - b

(defunc - (a b)
  :input-contract (and (rationalp a) (rationalp b))
  :output-contract (rationalp (- a b))
  
  (+ a (unary-- b)))
  
  
  
  
  (check= (- 2 4) -2)
  (check= (- 5 5) 0)
  (check= (- 6 3) 3)
  (check= (- 1/2 0) 1/2)
  (check= (- 0 1) -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; /: Rational x Rational-{0} -> Rational

; (/ a b) returns a/b; note that b has to be non-0.
; Think about the input contract.

(defunc / (a b)
  :input-contract(and (and (rationalp a) (rationalp b)) (not (equal b 0)))
  :output-contract (rationalp (/ a b))
  (* a (unary-/ b))
  )




(check= (/ 23 46) 1/2)
(check= (/ 2 8) 1/4)
(check= (/ 6 2) 3)
(check= (/ 0 1) 0)
(check= (/ 4 2) 2)
(check= (/ -4 3) -4/3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; divides: Nat x Nat-{0} -> Boolean)))))))))

; (divides x y) is t iff x is divisible by y.

(defunc divides (x y)
  :input-contract (and (and (natp x) (natp y)) (not (equal y 0)))
  :output-contract (booleanp (divides x y))
  (integerp (/ x y))
  
  
  )
(check= (divides 12 4) t)
(check= (divides 14 7) t)
(check= (divides 0 7) t)
(check= (divides 13 3) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following functions need recursion

; The following directive turns off some static checks that ACL2 performs.
; We turn them off since some of these checks are non-trivial and may
; require advanced features of ACL2 that we have not studied.

:program

; Define
; rem: Nat x Nat-{0} -> Nat

; (rem x y) returns the remainder of the integral division of x by y.

(defunc rem (x y)
  :input-contract (and (and (natp x) (natp y)) (not (equal y 0)))
  :output-contract (natp (rem x y))
  (cond ((equal x 0) 0)
        ((if (< (- x y) 0)
               x
               (rem (- x y) y)))

))

(check= (rem 2 4) 2)
(check= (rem 4 2) 0)
(check= (rem 4 3) 1)
(check= (rem 4 4) 0)
(check= (rem 0 1) 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; nat/: Nat x Nat-{0} -> Nat

; (nat/ x y) returns the result of integer division of x by y.
; That is, it returns the integral part (floor) of x/y,
; which is a natural number. See the examples below.

; Hint: this is a *non-recursive* function. Use rem from above.

(defunc nat/ (x y)
  :input-contract (and (and (natp x) (natp y)) (not (equal y 0)))
  :output-contract (natp (nat/ x y))
  (cond ((if (or (< x y) (equal x 0))
           0
          (/ (- x (rem x y)) y)))))
        
  
  
(check= (nat/ 10 2) 5)
(check= (nat/ 11 2) 5)
(check= (nat/ 0 5) 0)
(check= (nat/ 4 3) 1)
 
 
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; add-digits: Nat -> Nat

; (add-digits x) returns the sum of the decimal digits in x,
; which is a natural number.

; Hint: Write a recursive definition that sums up the digits in x from the
; least significant to the most significant. The function rem will be
; helpful.

(defunc add-digits (x)
  :input-contract (natp x)
  :output-contract (natp (add-digits x))
  (if (< (/ x 10) 1)
    x
  (+ (rem  x 10) 
     (add-digits (nat/ x 10))))
  

)

(check= (add-digits 000) 0)
(check= (add-digits 123) 6)
(check= (add-digits 901) 10)
(check= (add-digits 001) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; C. Basic List Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recall the meaning of these important built-in functions: cons consp listp first rest

; Function endp is how we test to see if a list is empty (often done in recursive function definitions):

(defunc endp (l)
  :input-contract (listp l)
  :output-contract (booleanp (endp l))
  (equal l ()))

(check= (endp (cons 1 ())) nil)

; (endp 3) is an error!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; len: List -> Nat

; (len l) returns the number of (top-level) elements in l.

(defunc len (l)
  :input-contract (listp l)
  :output-contract (natp (len l))
  (if (endp l)
    0
    (+ 1 (len (rest l))))
  
)
(check= (len '((1 2))) 1)
(check= (len '() ) 0)
(check= (len (list (list 1 2) 2 (list 12 24))) 3)
(check= (len (list (list 1 2) (list 3 4) (list 5 6))) 3)
(check= (len (list (list 1 2) 2)) 2)

; In the test cases below and the following, we use the 'quote' mechanism
; to write lists even more succintly then using (list ...).
; We can think of the notation
; '(1 2 3)
; as an abbreviation for
; (list 1 2 3)
; It therefore evaluates to the list containing 1, 2, and 3.

(check= (len ())       0)
(check= (len '(1))     1)
(check= (len '((1 2))) 1) ; make sure you understand this output!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; has-threep: List -> Boolean

; (has-threep a) returns true if the list a has length at least 3, nil otherwise.

(defunc has-three (a)
  :input-contract (listp a)
  :output-contract (booleanp (has-three a))
  (if (consp a)
    (if (consp (rest a))
      (consp (rest (rest a)))
      nil)
    nil)
  )


(check= (has-three ())       nil)
(check= (has-three '(1 1 1)) t)
(check= (has-three '(1 2)) nil)
(check= (has-three '(1)) nil)
(check= (has-three '( 1 2 3 4 5 1 2)) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; n-th: Nat x List -> All

; (n-th n l) returns the nth element of list l, COUNTING FROM 0.
; If l does not have an nth element, nil is returned.

(defunc n-th (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract t
  (cond ((endp l) nil)
  
  
  
  ((if (equal n 0)
    (first l)
    (n-th (- n 1) (rest l))))
    
    
   

))

(check= (n-th 0  '(1 2)) 1)
(check= (n-th 10 '(1 2)) nil)
(check= (n-th 1 '()) nil)
(check= (n-th 3 '(3 9 0 1 2)) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; elem: All x List -> Boolean

; (elem a X) returns t iff a is an element of list X.

(defunc elem (a X)
  :input-contract (listp X)
  :output-contract (booleanp (elem a X))
  (if (endp X)
    nil
    (or (equal (first X) a)
        (elem a (rest X))))

)

(check= (elem 2 '(1 2 3 4)) t)
(check= (elem 2 '(1 3 3 4)) nil)
(check= (elem 4 '()) nil)
(check= (elem '(1 2 3) (list (list 1 2 3) 2)) t)
(check= (elem 'x (list 'x 'y 'z)) t)
(check= (elem '() '(2 3 5)) nil)
(check= (elem '() '()) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; has-dups: List -> Boolean

; (has-dups l) returns t if l contains any element twice (or more); nil otherwise.

(defunc has-dups (l)
  :input-contract (listp l)
  :output-contract (booleanp (has-dups l))
  (if (endp l)
    nil
    (or (elem (first l) (rest l))
        (has-dups (rest l))))

)
  
  

(check= (has-dups '(1))   nil)
(check= (has-dups '(1 1)) t)
(check= (has-dups '()) nil)
(check= (has-dups '(3 3 2 3)) t)
(check= (has-dups '(1 2 3 4 5)) nil)


; Define two recognizer functions
; rationallistp : All -> Bool
; natlistp      : All -> Bool
; which recognize a list of rationals and a list of naturals, respectively.
; These will be recursive functions; use rationalp and natp, respectively.
; Note that the empty list () is both a rationallist and a natlist.

(defunc rationallistp (l)
  :input-contract t
  :output-contract (booleanp (rationallistp l))
  (if (listp l)
   (if (endp l) 
          t
         (and (rationalp (first l))
             (rationallistp (rest l))))
   nil)



  )

(check= (rationallistp '()) t)
(check= (rationallistp '(1 t)) nil)
(check= (rationallistp '(1/2 -2/7 0 2 1)) t)
(check= (rationallistp 'x) nil)
(check= (rationallistp 2) nil)

(defunc natlistp (l)
  :input-contract t
  :output-contract (booleanp (natlistp l))
  (if (listp l)
  (if (endp l)
    t
   (and (natp (first l))
        (natlistp (rest l))))
  nil

)

)

(check= (natlistp '()) t)
(check= (natlistp '(1 1/2)) nil)
(check= (natlistp 2) nil)
(check= (natlistp '(n)) nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; sorted : Rationallist -> Boolean

; (sorted l) returns t if l is a sorted (smallest to largest) list of
; rationals, nil otherwise.

(defunc sorted (l)
  :input-contract (rationallistp l)
  :output-contract (booleanp (sorted l))
  (if (endp l)
    t
       (if (endp (rest l))
           t
        (and (<= (first l) (first (rest l)))
         (sorted (rest l)))
       

)))

(check= (sorted '(-1 -1/2 2 5)) t)
(check= (sorted '(-1 -1/2 2 1)) nil)
(check= (sorted '(0 0 0 0)) t)
(check= (sorted '()) t)
(check= (sorted '(9)) t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; scale-natlist : Nat x Natlist -> Natlist

; (scale-natlist s l ) takes a list of natural numbers as input and
; scales it by the factor of s, i.e. each element is multiplied by s.

(defunc scale-natlist (s l)
  :input-contract (and (natp s) (natlistp l))
  :output-contract (natlistp (scale-natlist s l))
  (if (endp l)
    nil
    (cons (* s (first l)) (scale-natlist s (rest l)))))
  
  


(check= (scale-natlist 3 '( 1 2 3)) '( 3 6 9))
(check= (scale-natlist 3 nil) nil)
(check= (scale-natlist 0 '(0)) '(0))
(check= (scale-natlist 9 '()) '())
(check= (scale-natlist 10 '(10 12)) '(100 120))#|ACL2s-ToDo-Line|#



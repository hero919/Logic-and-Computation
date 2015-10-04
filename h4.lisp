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
homework4
YUELING QIN & ZEQING ZHANG

CS 2800 Homework 4 - Fall 2014

Student names: This homework is done in pairs. Put BOTH names here.

For this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw04.lisp

- make sure you are in BEGINNERS mode. As far as our class is concerned,
  this mode differs from Bare Bones mode (which we have used so far) in
  that most functions you "expect" are built-in, including all common
  recognizers, Booleans, arithmetic functions, and list processing
  functions, greatly faciliating programming.

  It also differs in the way it treats the Booleans: functions like 'and'
  and 'or' can now take any number of arguments, including zero or one:

  [[(and nil t t)]] = nil  , [[(or t)]] = t

  They can even take non-Boolean arguments. However, when called on Boolean
  argumetns, 'and' and 'or' behave as in propositional logic.

  BEGINNERS is essential for this homework! Note that you can only change
  the mode when the session is not running, so that the correct mode before
  starting the session.

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

- when done, save your file and submit it as hw04.lisp.

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Encryption using XOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

The Boolean xor function can be used to implement a simple
encryption/decryption scheme. In this homework you are asked to implement
that scheme and put an ASCII-like encoder/decoder on top of that, so that
you can encrypt actual English sentences, not just sequences of bits.

|#

;; Data definitions for lists of booleans, symbols, letters:

(defdata booleanlist (listof boolean))
(defdata symbollist  (listof symbol))
(defdata letterlist  (enum '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

#|

The definition of the letterlist enumeration type automatically gives rise
to a list representation of the elements of the type, via the constant
*letterlist-values*

For example:

|#

(check= *letterlist-values* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

;;; Auxiliary functions ;;;

;; Define the following simple functions in ACL2. To avoid confusion,
;; contracts are provided.

;; Prefix of l of length n. If l doesn't have n elements, return l
(defunc prefix (l n)
  :input-contract (and (listp l) (natp n))
  :output-contract (listp (prefix l n))
  (cond ((or (equal n 0) (endp l)) nil)
        (t (cons (first l) (prefix (rest l) (- n 1))))))

(check= (prefix '(1 2 3) 2) '(1 2))
(check= (prefix '(1 2 3) 4) '(1 2 3))
(check= (prefix '() 1) nil)
(check= (prefix '(1 2 3) 0) nil)
(check= (prefix '(1) 5) '(1))
(check= (prefix '(8 2 12 0) 3) '(8 2 12))

; position of x in l (counting from 0) if x occurs in l, (len l) otherwise
(defunc pos (x l)
  :input-contract (listp l)
  :output-contract (natp (pos x l))
  (cond ((endp l) 0)
        ((equal (first l) x) 0)
        (t (+ 1 (pos x (rest l))))))

(check= (pos 'c *letterlist-values*) 2)
(check= (pos 'a *letterlist-values*) 0)
(check= (pos 0 '(0 1 2 3)) 0)
(check= (pos 0 '(1 23)) 2)
(check= (pos '@ *letterlist-values*) (len *letterlist-values*))

; n raised to power e. We define 0^0 = 1
(defunc power (n e)
  :input-contract (and (natp n) (natp e))
  :output-contract (natp (power n e))
  (if (> e 0)
    (* n (power n (- e 1)))
    1))

(check= (power 3 1) 3)
(check= (power 3 3) 27)
(check= (power 5 0) 1)
(check= (power 0 0) 1)

; The following directive turns off some static checks that ACL2 performs.
; We turn them off for the rest of this homework since some of these checks
; are non-trivial and may require advanced features of ACL2 that we have
; not studied.

:program

;; We will implement our encoding scheme in three steps: (i) functions to
;; encrypt "sentences" into boolean lists, (ii) functions to decrypt boolean
;; lists into "sentences", and (iii) the actual encryption scheme, using
;; xor.

;;; Conversion from sentences to boolean lists ;;;

; Function that converts symbols to a simplified ASCII code. Your function
; takes an arbitrary symbol as input and returns 0..25 if the symbol is a
; letter, 26 otherwise. See tests below.

; symbol2dec : Symbol -> Nat

; Hint: this is a one-liner. Use the pos function defined above
; and *letterlist-values*.

(defunc symbol2dec (s)
  :input-contract (symbolp s)
  :output-contract (natp (symbol2dec s))
  (pos s *letterlist-values* ))

(check= (symbol2dec 'z) 25) ; the quote expression 'z returns the symbol z: [['z]] = z
(check= (symbol2dec '@) 26) ; ditto
(check= (symbol2dec 'a) 0)
(check= (symbol2dec 'b) 1)

; Function that converts a natural number n to a boolean list, as follows.
; If n >= 26, return nil. Otherwise, return the binary representation of n,
; where we interpret nil as 0 and t as 1. Leading zeros must be included as
; leading nil's in the result! See tests below.

; dec2booleanlist : Nat -> Booleanlist

; Since 2^5 = 32 > 26, the result is going to be a boolean list of length
; exactly 5. What is the condition for the left-most (highest-order) bit to
; be 1?

; In the stub below, some of the code is given to you to get you started.
; Make sure you understand the existing code. Then fill in the rest.

; The form 'let*' is used to define local variables. If you don't know this
; form, use any Lisp book or online documentation to find out about it, e.g.

; http://www.lispworks.com/documentation/HyperSpec/Body/s_let_l.htm

(defunc dec2booleanlist (n)
  :input-contract (natp n)
  :output-contract (booleanlistp (dec2booleanlist n))
  (if (< n 26)
    (let*  ((d4 (>= n 16))
           (n  (if d4 (- n 16) n))
           (d5 (>= n 8))
           (n (if d5 (- n 8) n))
           (d6 (>= n 4))
           (n (if d6 (- n 4) n))
           (d7 (>= n 2))
           (n (if d7 (- n 2) n))
           (d8 (>= n 1))
           (n (if d8 (- n 1) n)))
          (list d4 d5 d6 d7 d8))
    nil))
    

(check= (dec2booleanlist  8) '(nil t nil nil nil)) ; 8 = 01000. Do not skip leading nil's (zeros): the output MUST be a list of length 5.
(check= (dec2booleanlist 32) nil)
(check= (dec2booleanlist 17) '(t nil nil nil t))
(check= (dec2booleanlist 25) '(t t nil nil t))

; Function that converts a symbol s into a boolean list. First convert s
; into a decimal number using a function defined previously, then convert
; that into boolean list using a function defined previously. No recursion.

; symbol2booleanlist : Symbol -> Booleanlist

(defunc symbol2booleanlist (s)
  :input-contract (symbolp s)
  :output-contract (booleanlistp (symbol2booleanlist s))
  (dec2booleanlist (pos s *letterlist-values*)))

(check= (symbol2booleanlist 'f) '(nil nil t nil t))
(check= (symbol2booleanlist 'z) '(t t nil nil t))
(check= (symbol2booleanlist 'a) '(nil nil nil nil nil))
(check= (symbol2booleanlist 'b) '(nil nil nil nil t))

; Function that converts a symbol list sl into a boolean list. See the
; tests below: the result must be the _concatenation_ (use append) of all
; boolean lists obtained by converting the symbols in sl into boolean
; lists.

; symbollist2booleanlist : Symbollist -> Booleanlist

(defunc symbollist2booleanlist (sl)
  :input-contract (symbollistp sl)
  :output-contract (booleanlistp (symbollist2booleanlist sl))
  (cond ((endp sl) nil)
       ;
        (t (append (symbol2booleanlist (first sl)) 
                   (symbollist2booleanlist (rest sl))))))
        



; Notice that you must put spaces between symbols, but the number
; of spaces are irrelevant.
(check= (symbollist2booleanlist '(i   a m    s a m))'(NIL T NIL NIL NIL   
          NIL NIL NIL NIL NIL 
          NIL T T NIL NIL     
          T NIL NIL T NIL     
          NIL NIL NIL NIL NIL 
          NIL T T NIL NIL))  

(check= (symbollist2booleanlist '())
        '())
(check= (symbollist2booleanlist '(a))
        '(nil nil nil nil nil))
(check= (symbollist2booleanlist '(a b c d e f))
        '(nil nil nil nil nil
              nil nil nil nil t
              nil nil nil t nil
              nil nil nil t t
              nil nil t nil nil
              nil nil t nil t))

(check= (symbollist2booleanlist '(c  l  a  i  r    e))
        '(nil nil nil t nil 
              nil t nil t t
              nil nil nil nil nil 
              nil t nil nil nil 
              t nil nil nil t
              nil nil t nil nil))

(check= (symbollist2booleanlist '(b   o b)) '(nil nil nil nil t
                                                  nil t t t nil 
                                                  nil nil nil nil t))

(check= (symbollist2booleanlist '()) nil)




;;; Conversion from booleanlist to symbollist ;;;

; Function that takes a boolean list (any length), interprets nil as 0 and
; t as 1 and the list as a number in binary representation. Output is the
; decimal value of the number. Hint: use power!

; booleanlist2dec : Booleanlist -> Nat

(defunc booleanlist2dec (l)
  :input-contract (booleanlistp l)
  :output-contract (natp (booleanlist2dec l))
  (cond ((endp l) 0)
        ((first l) (+ (power 2 (- (len l) 1)) (booleanlist2dec (rest l))))
        (t (booleanlist2dec (rest l)))))

(check= (booleanlist2dec '(t t nil t)) 13)
(check= (booleanlist2dec '(t t nil t t nil t nil t)) 437)
(check= (booleanlist2dec '(t t t)) 7)
(check= (booleanlist2dec '()) 0)

; Function that takes a decimal number dec. If dec >= 26, return the
; symbol - (dash). Otherwise, return the dec'th letter of the alphabet (counting
; starts from 0).

; Hint: use *letterlist-values* and nth.

; dec2symbol : Nat -> Symbol

(defunc dec2symbol (dec)
  :input-contract (natp dec)
  :output-contract (symbolp (dec2symbol dec))
  (if (>= dec 26)
    '- 
    (nth dec *letterlist-values*)))


(check= (dec2symbol  0) 'a)
(check= (dec2symbol 10) 'k)
(check= (dec2symbol 1) 'b)
(check= (dec2symbol 25) 'z)
(check= (dec2symbol 27) '-)




; Function that converts a boolean list to a symbol. First convert the
; boolean list into a decimal number, then convert that into a symbol. All
; using previously defined functions.

; booleanlist2symbol : Booleanlist -> Symbol

(defunc booleanlist2symbol (l)
  :input-contract (booleanlistp l)
  :output-contract (symbolp (booleanlist2symbol l))
  (dec2symbol (booleanlist2dec l)))

(check= (booleanlist2symbol '(nil t nil t nil)) 'k) ; 01010 (bin) = 10 (dec), 10th letter = k
(check= (booleanlist2symbol '(t t nil nil t)) 'z)
(check= (booleanlist2symbol '(nil t t t t )) 'p)
(check= (booleanlist2symbol '(t nil nil t t)) 't)

(check= *letterlist-values* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

; Function that converts a boolean list into a symbol list. This function
; takes the first 5 booleans in the lists, interprets them as a binary
; number and converts them into a symbol, using previously defined
; functions. Then the function takes the next 5 booleans in the list, and
; so on. The result must be the list of all symbols obtained this
; way.

; Hint: use nthrest: (nthrest n l) applies rest to l n times, e.g.,
; (nthrest 0 '(1 2 3)) = (1 2 3) and (nthrest 2 '(1 2 3)) = (3)

; booleanlist2symbollist : Booleanlist -> Symbollist

(defunc booleanlist2symbollist (l)
  :input-contract (booleanlistp l)
  :output-contract (symbollistp (booleanlist2symbollist l))
  (cond ((endp l) '())
        (t (cons (booleanlist2symbol (prefix l 5)) (booleanlist2symbollist (nthrest 5 l))))))
        



(check= (booleanlist2symbollist '(nil nil nil nil nil nil nil nil nil t nil nil nil t nil)) '(a b c))
(check= (booleanlist2symbollist '(nil nil nil nil t
                                      nil nil nil nil nil 
                                      nil nil nil  t t)) '(b a d))
(check= (booleanlist2symbollist '()) '())
(check= (booleanlist2symbollist '(nil t t nil t
                                      nil t nil nil nil 
                                      nil t nil t t)) '(n i l))


;;; Now the Encoding and Decoding ;;;

; Remember the xor function:
(defunc xor (a b)
  :input-contract (and (booleanp a) (booleanp b))
  :output-contract (booleanp (xor a b))
  (not (iff a b)))

;; bitwise xor. This function takes two boolean lists and xor's
;; corresponding elements. If one of the lists has fewer elements than the
;; other, assume nil for the missing elements; see tests below

; bitwise-xor : Booleanlist x Booleanlist -> Booleanlist

(defunc bitwise-xor (l1 l2)
  :input-contract (and (booleanlistp l1) (booleanlistp l2))
  :output-contract (booleanlistp (bitwise-xor l1 l2))
  (cond ((and (endp l1) (endp l2))
         nil)
        ((endp l1)
         (cons (xor nil (first l2)) (bitwise-xor l1 (rest l2))))
        ((endp l2)
         (cons (xor (first l1) nil) (bitwise-xor (rest l1) l2)))
        (t
         (cons (xor (first l1) (first l2)) (bitwise-xor (rest l1) (rest l2))))))
  



(check= (bitwise-xor '(nil t t) '(t nil nil)) '(t t t))
(check= (bitwise-xor '(nil t t) '(t nil))     '(t t t)) ; same as previous, by convention
(check= (bitwise-xor '() '()) nil)
(check= (bitwise-xor '() '(nil nil nil)) '(nil nil nil))
(check= (bitwise-xor '() '(t nil t)) '(t nil t))
(check= (bitwise-xor '(t nil nil) '()) '(t nil nil))


; The idea of our encryption scheme is as follows. Show that the formula

; x <> k <> k

; can be simplified to x. What does this result mean? When you "xor" a
; given value x against a "key" k twice, you get x back. So the same key
; can be used to encrypt x AND to later decrypt the scrambled message to
; recover x.

; Our encryption scheme is thus based on bitwise xor-ing the input message
; (after translating it into a bitstring) against a key that only the
; intended communicators know. To decrypt, we apply the key again.

; We first need to define such a key.

; Replace the boolean list below with your own secret key if you like. Make
; sure it is at least a few dozen booleans long: the shorter, the weaker
; the encryption mechanism, since keys shorter than the message will be
; filled up with nil (which is easy to guess and exploit by an attacker).

:logic ; ignore but do not remove this line

(defdata key (enum
              '(  t nil t t nil 
                    t nil nil nil t 
                    t nil nil nil t 
                    nil t t nil t 
                    nil t nil t t
                    nil t nil t t
                    nil t nil nil t
                    nil t t nil t 
                    t t nil nil nil 
                    t t nil t nil 
                    t nil t t nil 
                    nil t nil t)))

:program ; ignore but do not remove this line

; Note that key is an explicit enumeration of boolean values. You can
; get access to those values in the form of a list using the constant
; expression

;  *key-values*

; for instance:

(check= (listp *key-values*) T)

; Function to encrypt a message. A "message" is a list of symbols. Convert
; that list into a boolean list, then encrypt the result against the secret
; key, i.e. apply the bit-wise xor to the result and your secret key:

; encrypt : Symbollist -> Booleanlist

(defunc encrypt (message)
  :input-contract (symbollistp message)
  :output-contract (booleanlistp (encrypt message))
  (bitwise-xor (symbollist2booleanlist message) *key-values*)
  )

(check= (encrypt '(o k))
        '(t t nil nil nil 
            t t nil t t
            t nil nil nil t 
                    nil t t nil t 
                    nil t nil t t
                    nil t nil t t
                    nil t nil nil t
                    nil t t nil t 
                    t t nil nil nil 
                    t t nil t nil 
                    t nil t t nil 
                    nil t nil t))
(check= (encrypt '(h e l l o))
        '(t nil nil nil t 
            t nil t nil t 
            t t nil t nil 
            nil nil t t nil 
            nil nil t nil t
            nil t nil t t
                    nil t nil nil t
                    nil t t nil t 
                    t t nil nil nil 
                    t t nil t nil 
                    t nil t t nil 
                    nil t nil t))
              
              
            

(check= (encrypt '()) '(t nil t t nil 
                    t nil nil nil t 
                    t nil nil nil t 
                    nil t t nil t 
                    nil t nil t t
                    nil t nil t t
                    nil t nil nil t
                    nil t t nil t 
                    t t nil nil nil 
                    t t nil t nil 
                    t nil t t nil 
                    nil t nil t))

(check= (encrypt '(c l a i r e i s s p y o h h))
        '(t nil t nil nil 
            t t nil t nil 
            t nil nil nil t
            nil nil t nil t
            t t nil t nil
            nil t t t t
            nil nil nil nil t
            t t t t t
            nil t nil t nil
            t nil t nil t
            nil t t t nil
            nil nil t nil nil
            nil nil  t t t
            nil nil  t t t))



; The result of this function depends on your choice of key. Show the
; output of this function on a few test cases.

; Function that decrypts an encrypted message. An "encrypted message" is a
; list of booleans. xor that list bitwise against the secret key, and convert
; the result into a list of symbols.

; decrypt : Booleanlist -> Symbollist

(defunc decrypt (cryptic-message)
  :input-contract (booleanlistp cryptic-message)
  :output-contract (symbollistp (decrypt cryptic-message))
  (booleanlist2symbollist (bitwise-xor cryptic-message *key-values*)))

 (check= (decrypt '(t nil t nil nil 
            t t nil t nil 
            t nil nil nil t
            nil nil t nil t
            t t nil t nil
            nil t t t t
            nil nil nil nil t
            t t t t t
            nil t nil t nil
            t nil t nil t
            nil t t t nil
            nil nil t nil nil
            nil nil  t t t
            nil nil  t t t)) '(c l a i r e i s s p y o h h))
 
(check= (decrypt '(t nil nil nil t t nil t nil t t t nil t
                     nil nil nil t t nil nil nil t nil t t t
                     nil nil nil nil t t t nil nil t nil nil
                     t nil t nil nil t t t t t nil t nil t
                     t t nil nil t nil nil nil nil nil nil t))
        '(h e l l o t h e r e b o b))
        
 

; The result of this function depends on your choice of key. Show the
; output of this function on a few test cases.

; Now test whether encryption followed by decryption reproduces the
; original message. The following test will pass with the key defined
; above.

(check=
 (let ((m '(m e e t     y o u     u n d e r     t h e     b r i d g e)))
   (equal (decrypt (encrypt m))
          m))
 t)


; Does decrypting the encrypted message always reproduce the original? Or
; can you think of cases (without contract violation) where the
; encryption and decryption are not inverses of each other? Create such an
; example in the test below (note: we check= against nil, not against t):

(check=
 (let ((m '(h)))
   (equal (decrypt (encrypt m))
          m))
 nil)


#|
no, 
if we have a example: 'h e l l o' when we encrypt it. We will first encrypt 'hello',
then after that, the program still running and what it does is to copy the rest of 
the secret key which we don't use. Because of this, when we decrypt the sentence we will
get 'hello' first. Then it will translate the rest of the sentence. Therefore they are not the
same. But if the length of the boolean list is greater than the key. We will get the exact answer 
which we want to translate. 


|#
; Hint: here is how you can get more output for a failing check= : in
; addition to the info that it failed, it prints the two (non-equal) values.
; The example below will fail and is thus placed in comments.


#|
(acl2::without-evisc
 (check= 1 t))

|#
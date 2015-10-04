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
Name :Zeqing Zhang && Yueling Qin

In this homework we use the following ascii character combinations to
represent the Boolean operators:

  NOT     ~

  AND     /\
  OR      \/

  IMPLIES =>

  EQUIV   =
  XOR     <>

The binding powers of these functions are listed from highest to lowest
in the above table. Within one group (no blank line), the binding powers
are equal.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A. Truth tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Construct the truth table for the following Boolean formula. Use
*alphabetical* order for the variables in the formula. Include result
columns for all relevant subformulas -- no shortcuts. For example, if the
formula contains the propositional variable x in negated form, then you
need a column for the subformula ~x.

Then decide whether the formula is satisfiable, unsatisfiable, valid, or
falsifiable (several of these predicates will hold).

Carefully think about binding powers -- if needed, add parentheses to make
it clear (to you) how this formula is to be parsed.

1.  (p => q) /\ (~r => ~q) = p => r

For this formula, your table should have 10 columns (including those for p,q,r).
p . q . r . ~q . ~r . (p=> q) . (~r => ~q) . (p => r) . ((p =>q) /\ (~r => ~q)) .result
---------------------------------------------------------------------------------------
t   t   t   f    f      t           t           t                t                 t
t   f   t   t    f      f           t           t                t                 f
t   t   f   f    t      t           f           f                f                 f
t   f   f   t    t      f           t           f                f                 t
f   t   t   f    f      t           t           t                t                 t
f   f   t   t    f      t           t           t                t                 t
f   t   f   f    t      t           f           t                f                 t
f   f   f   t    t      t           t           t                t                 t

... 

2. Someone suggests to introduce a new Boolean connective, *, such that
   a * b abbreviates the expression ~(~a /\ b).

(a) Formally define this connective, using a truth table.

a  . b .  ~a .(~a /\b) . ~(~a /\b)=a * b
------------------------------------------
T    T     F      F          T        
T    F     F      F          T        
F    T     T      T          F        
F    F     T      F          T        


(b) Do we really need this new connective, or can you express a * b using a
single connective that we already know?

~(~a/\b)
=~(~a)\/ ~b
=a\/ ~b
=a=> b
so, we can get the single connective with two object 

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; B. Boolean formula simplification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Simplify the following formulas, i.e. find a simpler formula that is
logically equivalent to the given formula. "Simpler" means: with as few
logical connectives as possible. For example, the formula

(p \/ (p => q))

is a tautology. The simplest equivalent formula is therefore T.

In general, there may be several solutions that can be viewed as
"equally simple", e.g., a \/ b and b \/ a are both "simplest".

Find the answer by applying a suitable selection of simplification and
rewrite rules, as discussed in class and listed in the Lecture Notes. You
need some creativity here. It is useful to think one step ahead when
applying a rule: sometimes it may be required to build an intermediate
formula that is more complex than the one you started from, in order to
then be able to simplify the result drastically.

If all else fails, use this strategy: rewrite the formula using only the
"elementary" connectives ~   \/   and /\   , and simplify that as much as
possible, using basic identities such as p /\ ~p = F. At the end see if you
can write the result using fewer connectives, e.g. by combining ~p \/ q to
p => q.

Write your simplification/rewrite steps in the following format:

  (~p \/ ~q) /\ T

=
  (~p \/ ~q)

= { de Morgan }

  ~(p /\ q)

As shown above, give the name of the rule you are using if possible; this
may help us give you partial credit if you make a mistake.

3.  (a \/ ~b) => (c /\ ~a) \/ ~b
= ~(a \/ ~b) \/ ((c /\ ~a) \/ ~b)
= (~a /\ b) \/ (c /\ ~a) \/ ~b
={distributivity rule}
= (~a /\ (b \/ c)) \/ ~b
={distributivity rule}
= (~a \/ ~b) /\ ((b \/ c) \/ ~b)
={DeMorgan's laws} and {associativity rule} 
= ~(a /\ b) /\ (b \/ c \/ ~b)
={equalities rule}
= ~(a /\ b) /\ T
={equalities rule}
= ~(a /\ b)

...

4.  ( p /\ ( ~p \/ q)) \/ ((p \/ ~q) /\ q)
??P /\ (~p \/ q)) \/ ((p \/ ~q) /\ q)
={distributivity rule}
=((p /\ ~p) \/ (p /\ q)) \/ ((p /\q) \/(~q /\q)
={equalities rule} for and 
=(p/\q) \/ (p /\ q)
={equalities rule} 
=(p /\ q)



5.  ~(~h /\ i) => (i => (~h /\ j))
=(h /\ ~i) =>(i => (~h /\ j))
=~(h \/ ~i) \/ (~i \/ (~h /\ j))
=(~h /\ i) \/ ~i \/(~h /\ j)
=((~h \/ ~i) /\ (i \/ ~i )) \/ (~h /\ j)
={distributivity rule}
=((~h \/ ~i) /\ T) \/ (~h /\ j)
={equality rule}
=(~h \/ ~i) \/ (~h /\ j)
={equality rule}
=~h \/ ~i \/ (~h /\ j)
={Associativity rule}
=~i \/ ~h
={Absorption rule}

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; C. Boolean formula classification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Boolean functions can be valid, satisfiable, unsatisfiable, or falsifiable.
These properties can overlap. For example, valid functions are also
satisfiable.

Determine the status of the following Boolean formulas, regarding these
properties. You must state ALL of the four properties a formula fulfills.

Give evidence for your decisions, as follows:

- simplify valid formulas to T, or show the truth table,
- simplify unsatisfiable formulas to F, or show truth table,
- give a suitable satisfying or falsifying assignment for satisfiable and
  for falsifiable formulas.

6.  a \/ ~b => ((a => b) = (a = b))
a . b . ~b . (a \/ ~b) . (a => b) . (a = b) . ((a => b) = (a = b)) . result 
T   T   F      T            T          T                 T               T
T   F   T      T            F          F                 T               T
F   T   F      F            T          F                 F               T
F   F   T      T            T          T                 T               T
From the truth table, we can see the statement should be (Valid )
...

7.  ((e /\ ~(g => (e \/ h))) \/ (~e /\ (h => e))) => (e \/ h)
e . g . h . ~e . e \/h . (g => (e \/ h)). ~(g => (e \/ h)). e /\ ~(g => (e \/ h)). (h => e) . (~e /\ (h => e)) .((e /\ ~(g => (e \/ h))) \/ (~e /\ (h => e))) . result 
T   T   T   F       T         T                  F                F                   T            F                       F                                      T
T   F   T   F       T         T                  F                F                   T            F                       F                                      T
T   T   F   F       T         T                  F                F                   T            F                       F                                      T
T   F   F   F       T         T                  F                F                   T            F                       F                                      T
F   T   T   T       T         T                  F                F                   F            F                       F                                      T
F   F   T   T       T         T                  F                F                   F            F                       F                                      T
F   T   F   T       F         F                  T                T                   T            T                       T                                      F
F   F   F   T       F         F                  T                T                   T            T                       T                                      F
From the truth table, we can see the statement should be (Satisfiable and Falsifiable)

8.  (x = ~z) /\ (~x <> z)
x . z . ~x . ~z . ( x = ~z) . (~x <> z). result
T   T   F    F       F            T        F
T   F   F    T       T            F        F 
F   T   T    F       T            F        F
F   F   T    T       F            T        F 
From the truth table, we can see the statement should be (Unsatisfiable)
...

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; D. Expressiveness of Boolean connectives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

For the following problems, show that you can express the Boolean
connective on the left of the = using ONLY the Boolean connective(s)
mentioned in { } on the right, plus the propositional constants T and F and
parentheses if needed. For example, if your task was:

x \/ y  =  { express using ~ , /\ , T , F , () }

your answer would be:

x \/ y  =  ~(~x /\ ~y)

the justification being de Morgan's law. You do not have to give a
justification for these problems.

9.  ~x    =    { express using => , T , F , () }
 ~x= ~x\/F = x=>F
...

10.  ~x    =    { express using <> , T , F , () }
~x = (T/\~x) \/ (F/\x) = T<>X
...

11.  x /\ y  =  { express using NAND , T , F , () , where NAND ("not and")
                  is the binary connective defined by NAND(x,y) = ~(x /\ y) }
=(NAND (NAND (X,Y), T)


|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; E. Problem solving using propositional logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
You are given three labeled boxes: two are empty, one contains money;
but you don't know which one. The labels on boxes 1 and 2 read, "This box
is empty"; the label on box 3 reads "The money is in box 2". You also know
that exactly one label tells the truth. The question is: where is the
money?

Formalize the puzzle as a satisfiability problem, and solve it, as follows.

(a) Define propositional variables to represent relevant atomic facts
(whose truth we currently do not know).

Hint: relevant facts may include whether a particular label tells the
truth, and whether a particular box contains the money.
way1:
A : The label in the frist box is correct. 

B : The label in the second box is correct. 

C : The label in the third box is correct. 

and B= ~C
============================================
way2:

A': box 1has money
B': box 2has money
C': box 3has money

label 1: ~A'
label 2: ~B'
label 3: B'


we assume the situation there is only one box has money, therefore, we can get the logic here:
(A' /\ ~B' /\ ~C') \/ (~A' /\ B' /\ ~C') \/ (~A' /\ ~B' /\ C'), But that is not enough, we should have 
another precondition, the situation that only one label tell true, mean other two false.
(~A' /\ B' /\ ~C') \/ (A' /\ ~B' /\ ~B') \/ (A' /\ B' /\ B') 
=============================================





(b) Using these variables, formulate the knowledge given in the problem as
propositional formulas. Be careful with the statement that exactly one
label tells the truth: it means that one tells the truth (we don't know
which one) and the other two "lie".
WAY1:
Due to the logic , we can express the sentence as:

(A <> B) /\ (B <> C)

which (A <> B) means box 1 or box 2 has money inside them.
(B <> C) means the sentance "B doen't have money" and "B has money " , 
only one is true. Finally "AND" them means the logic should be both right 
if we want the whole sentance correct.

WAY2:
we can get this equation by combined all logic 

((A' /\ ~B' /\ ~C') \/ (~A' /\ ~B' /\ C')) \/ (~A' /\ B' /\ ~C') /\ ((~A' /\ B' /\ ~C') \/ (A' /\ ~B' /\ ~C') \/ (A' /\ B' /\ B'))

.========================================================================

(c) Show that the formula you have found in (b) is satisfiable. State where
the money is, and which label tells the truth, according to the satisfying
assignment. You may use simplifications, or truth tables.
WAY1:
(A <> B) /\ (B <> C)
A . B . C . (A <> B) .  (B <> C) . (A <> B) /\ (B <> C)
T   T   F      F            T           F
T   F   T      T            T           T
F   T   F      T            T           T
F   F   T      F            T           F

We can see from the table that there exits true, therefore the formula is satisfiable and fisitiable 

WAY2:
((A' /\ ~B' /\ ~C') \/ (~A' /\ ~B' /\ C')) \/ (~A' /\ B' /\ ~C') /\ ((~A' /\ B' /\ ~C') \/ (A' /\ ~B' /\ ~C') \/ (A' /\ B' /\ B'))
A'  B' C'  .result.

F   F   F    F  
F   F   T    F  
F   T   F    F  
F   T   T    F  
T   F   F    T  
T   F   T    F  
T   T   F    F  
T   T   T    F  

==========================================================================
(d) Someone objects: "What if there are several solutions? Then we still
don't know where the money is!" You have to admit that the person is right.
Think about how we could figure out, using propositional logic, whether the
solution we found is unique. Then apply your idea to this problem. What do
you find?
 Yes, in the problem, we don't assume that there is only one box have money. 
 If the money is both in box 2 and box 3. The logic is still right. But we obey 
 the precondition. Therefore we also have to think about the there is only one 
 box have money.
 So, from this mistake, we know WAY1 have some problems: we didn't consider the precondition, 
 therefore, combined this logic here, we konw we must combined all situations, otherwise we don't where is money,
 after thinking about these, we get only one solution.

|#
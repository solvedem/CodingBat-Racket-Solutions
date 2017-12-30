#lang racket

; ----------------------------------------------------

;Warmup-1 > sleep_in 
;The parameter weekday is True if it is a weekday, and the parameter vacation is True if we are on vacation.
;We sleep in if it is not a weekday or we're on vacation. Return True if we sleep in.
;sleep_in(False, False) → True
;sleep_in(True, False) → False
;sleep_in(False, True) → True

(define (sleep_in weekday vacation)
    (or (equal? weekday "F")(equal? vacation "T")))

;(sleep_in "F" "F") → #t
;(sleep_in "T" "F") → #f
;(sleep_in "F" "T") → #t

; ----------------------------------------------------

;Warmup-1 > monkey_trouble 
;We have two monkeys, a and b, and the parameters a_smile and b_smile indicate if each
;is smiling. We are in trouble if they are both smiling or if neither of them is smiling. Return True if we are in trouble.

;monkey_trouble(True, True) → True
;monkey_trouble(False, False) → True
;monkey_trouble(True, False) → False

(define (monkey_trouble a_smile b_smile)
  (or (and (equal? a_smile "T")(equal? b_smile "T"))(and (equal? a_smile "F")(equal? b_smile "F"))))

;(monkey_trouble "T" "T") → #t
;(monkey_trouble "F" "F") → #t
;(monkey_trouble "T" "F") → #f

; ----------------------------------------------------

;Warmup-1 > sum_double 
;Given two int values, return their sum. Unless the two values are the same, then return double their sum.

;sum_double(1, 2) → 3
;sum_double(3, 2) → 5
;sum_double(2, 2) → 8

(define (sum_double x y)
  (if (equal? x y)
      (* 2 (+ x y))
      (+ x y)))

;(sum_double 1 2) → 3
;(sum_double 3 2) → 5
;(sum_double 2 2) → 8

; ----------------------------------------------------

;Warmup-1 > parrot_trouble 
;prev  |  next  |  chance
;We have a loud talking parrot. The "hour" parameter is the current hour time in the range 0..23.
;We are in trouble if the parrot is talking and the hour is before 7 or after 20. Return True if we are in trouble.

;parrot_trouble(True, 6) → True
;parrot_trouble(True, 7) → False
;parrot_trouble(False, 6) → False

(define (parrot_trouble talking hour)
  (and (equal? talking "T")(or (< hour 7)(> hour 20))))

;(parrot_trouble "T" 6) → #t
;(parrot_trouble "T" 7) → #f
;(parrot_trouble "F" 6) → #f

; ----------------------------------------------------

;Warmup-1 > makes10 
;Given 2 ints, a and b, return True if one if them is 10 or if their sum is 10.

;makes10(9, 10) → True
;makes10(9, 9) → False
;makes10(1, 9) → True

(define (makes10 a b)
  (or (or (and (equal? a 10)(not(equal? b 10)))(and (equal? b 10)(not(equal? a 10))))(equal?(+ a b) 10)))

;(makes10 9 10) → #t
;(makes10 9 9) → #f
;(makes10 1 9) → #t

; ----------------------------------------------------

;Warmup-1 > near_hundred 
;Given an int n, return True if it is
;within 10 of 100 or 200. Note: abs(num) computes the
;absolute value of a number.

;near_hundred(93) → True
;near_hundred(90) → True
;near_hundred(89) → False

(define (near_hundred n)
  (or (<= (abs (- 100 n)) 10)(<= (abs (- 200 n)) 10)))

;(near_hundred 93) → #t
;(near_hundred 90) → #t
;(near_hundred 89) → #f

; ----------------------------------------------------

;Warmup-1 > pos_neg 
;Given 2 int values, return True if one is negative
;and one is positive. Except if the
;parameter "negative" is True, then return True only if both are negative.

;pos_neg(1, -1, False) → True
;pos_neg(-1, 1, False) → True
;pos_neg(-4, -5, True) → True

(define (pos_neg a b negative)
  (if (equal? negative "T")
      (and (< a 0)(< b 0))
      (or (and (< a 0)(>= b 0))(and (< b 0)(>= a 0)))))

;(pos_neg 1 -1 "F") → #t
;(pos_neg -1 1 "F") → #t
;(pos_neg -4 -5 "T") → #t

; ----------------------------------------------------

;Warmup-1 > not_string 
;Given a string, return a new string where "not "
;has been added to the front. However, if the string
;already begins with "not", return the string unchanged.


;not_string('candy') → 'not candy'
;not_string('x') → 'not x'
;not_string('not bad') → 'not bad'

(define (not_string s)
  (if(or (<= (string-length s) 1)(not(equal? (substring s 0 3) "not")))
     (string-append "not " s)
     s))

;(not_string "candy") → "not candy"
;(not_string "x") → "not x"
;(not_string "not bad") → "not bad"

; ----------------------------------------------------

;Warmup-1 > missing_char 
;Given a non-empty string and an int n, return a new string where the char at index n has been removed.
;The value of n will be a valid index of a char in the original string (i.e. n will be in the range 0..len(str)-1 inclusive).

;missing_char('kitten', 1) → 'ktten'
;missing_char('kitten', 0) → 'itten'
;missing_char('kitten', 4) → 'kittn'

(define (missing_char str n)
  (string-append (substring str 0 n)(substring str (+ n 1) (string-length str))))

;(missing_char "kitten" 1) → "ktten"
;(missing_char "kitten" 0) → "itten"
;(missing_char "kitten" 4) → "kittn"

; ----------------------------------------------------

;Warmup-1 > front_back 
;Given a string, return a new string where the first and last chars have been exchanged.

;front_back('code') → 'eodc'
;front_back('a') → 'a'
;front_back('ab') → 'ba'

(define (front_back str)
  (if (<= (string-length str) 1)
      str
      (string-append (substring str (- (string-length str) 1)(string-length str))
                      (substring str 1 (- (string-length str) 1))(substring str 0 1))))

;(front_back "code") → "eodc"
;(front_back "a") → "a"
;(front_back "ab") → "ba"

; ----------------------------------------------------

;Warmup-1 > front3 
;Given a string, we'll say that the front is the first 3 chars of the string.
;If the string length is less than 3, the front is whatever is there.
;Return a new string which is 3 copies of the front.

;front3('Java') → 'JavJavJav'
;front3('Chocolate') → 'ChoChoCho'
;front3('abc') → 'abcabcabc'

(define (front3 str)
  (if (> (string-length str) 3)
      (string-append (substring str 0 3)(substring str 0 3)(substring str 0 3))
      (string-append str str str)))

;(front3 "Java") ; → "JavJavJav"
;(front3 "Chocolat") ; → "ChoChoCho"
;(front3 "abc") ; → "abcabcabc"

; ----------------------------------------------------


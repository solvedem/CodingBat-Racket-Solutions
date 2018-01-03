#lang racket

;--------------------------------------------------------------
;Warmup-2 > string_times 
;Given a string and a non-negative int n,
;return a larger string that is n copies of the original string.

;string_times('Hi', 2) → 'HiHi'
;string_times('Hi', 3) → 'HiHiHi'
;string_times('Hi', 1) → 'Hi'

(define (string_times str n output)
  (if (= n 0)
      output
      (string_times str (- n 1) (string-append output str))))

;(string_times "Hi" 2 "") ; → "HiHi"
;(string_times "Hi" 3 "") ; → "HiHiHi"
;(string_times "Hi" 1 "") ; → "Hi"

;--------------------------------------------------------------

;Warmup-2 > front_times 
;Given a string and a non-negative int n,
;we'll say that the front of the string is the first 3
;chars, or whatever is there
;if the string is less than length 3. Return n copies of the front;

;front_times('Chocolate', 2) → 'ChoCho'
;front_times('Chocolate', 3) → 'ChoChoCho'
;front_times('Abc', 3) → 'AbcAbcAbc'

(define (front_times str n output)
  (if (= n 0)
      output
      (if (>= (string-length str)  3)
          (front_times str (- n 1) (string-append output (substring str 0 3)))
          (front_times str (- n 1) (string-append output (substring str 0 (string-length str)))))))


;(front_times "Chocolate" 2 "") ; → "ChoCho"
;(front_times "Chocolate" 3 "") ; → "ChoChoCho"
;(front_times "Abc" 3 "") ; → "AbcAbcAbc"

;--------------------------------------------------------------

;Warmup-2 > string_bits 
;Given a string, return a new string made of every other char starting with the first, so "Hello" yields "Hlo".


;string_bits('Hello') → 'Hlo'
;string_bits('Hi') → 'H'
;string_bits('Heeololeo') → 'Hello'

(define (string_bits str n output)
  (if (< n 0)
      output
      (if (= (modulo n 2) 0)
          (string_bits str (- n 1) (string-append (substring str n (+ n 1)) output))
          (string_bits str (- n 1) output))))

;(string_bits "Hello" (- (string-length "Hello") 1 ) "")
;(string_bits "Hi" (- (string-length "Hi") 1 ) "")
;(string_bits "Heeololeo" (- (string-length "Heeololeo") 1 ) "")


;--------------------------------------------------------------

;Warmup-2 > string_splosion
;Given a non-empty string like "Code" return a string like "CCoCodCode".

;string_splosion('Code') → 'CCoCodCode'
;string_splosion('abc') → 'aababc'
;string_splosion('ab') → 'aab'

(define (string_splosion n str output)
  (if (>= n (string-length str))
      output
      (string_splosion (+ n 1) str (string-append output (substring str 0 (+ n 1))))))

;(string_splosion 0 "Code" "")
;(string_splosion 0 "abc" "")
;(string_splosion 0 "ab" "")

;--------------------------------------------------------------

;Warmup-2 > last2 
;Given a string, return the count of the number
;of times that a substring length 2 appears in the
;string and also as the last 2 chars of the string,
;so "hixxxhi" yields 1 (we won't count the end substring).

;last2('hixxhi') → 1
;last2('xaxxaxaxx') → 1
;last2('axxxaaxx') → 2


(define (sub str)
  (if (< (string-length str) 2)
      0
      (substring str (- (string-length str) 2) (string-length str) )))

(define (counter sub str n count)
  (if (>= n (- (string-length str) 2))
      count
      (if (equal? (substring str n (+ n 2)) sub)
          (counter sub str (+ n 1) (+ count 1))
          (counter sub str (+ n 1) count))))

(define (last2 str)
  (counter (sub str) str 0 0))

;(last2 "hixxhi") ; → 1
;(last2 "xaxxaxaxx") ; → 1
;(last2 "axxxaaxx") ; → 2

;--------------------------------------------------------------

;Warmup-2 > array_count9 
;Given an array of ints, return the number of 9's in the array.

;array_count9([1, 2, 9]) → 1
;array_count9([1, 9, 9]) → 2
;array_count9([1, 9, 9, 3, 9]) → 3

(define (counter_ li n count)
  (if (<= n 0)
      count
      (if (= (first li) 9)
          (counter_ (rest li) (- n 1) (+ count 1))
          (counter_ (rest li) (- n 1) count))))

(define (array_count9 li)
  (counter_ li (length li) 0))

;(array_count9 (list 1 2 9)) ; → 1
;(array_count9 (list 1 9 9)) ; → 2
;(array_count9 (list 1 9 9 3 9)) ; → 3

;--------------------------------------------------------------

;Warmup-2 > array_front9 
;Given an array of ints, return True if one of
;the first 4 elements in the array is a 9. The
;array length may be less than 4.

;array_front9([1, 2, 9, 3, 4]) → True
;array_front9([1, 2, 3, 4, 9]) → False
;array_front9([1, 2, 3, 4, 5]) → False

(define (counter3 li n count)
  (if (<= n 1)
      (> count 0)
      (if (= (first li) 9)
          (counter3 (rest li) (- n 1) (+ count 1))
          (counter3 (rest li) (- n 1) count))))

(define (array_front9 li)
  (counter3 li (length li) 0))

;(array_front9 (list 1 2 9 3 4)) ; → #t
;(array_front9 (list 1 2 3 4 9)) ; → #f
;(array_front9 (list 1 2 3 4 5)) ; → #f


;--------------------------------------------------------------

;Warmup-2 > array123 
;Given an array of ints, return True
;if the sequence of numbers 1, 2, 3
;appears in the array somewhere.

;array123([1, 1, 2, 3, 1]) → True
;array123([1, 1, 2, 4, 1]) → False
;array123([1, 1, 2, 1, 2, 3]) → True


(define (array_ ls a b c)
  (if (or (<= (length ls) 0) (and (and (= a 1) (= b 2)) (= c 3)))
      (and (and (= a 1) (= b 2)) (= c 3))
      (if (empty? (rest (rest ls)))
          #f
          (array_ (rest ls) (first ls) (first (rest ls)) (first (rest (rest ls)))))))

(define (array123 ls)
  (array_ ls -1 -1 0))


;(array123 (list 1 1 2 3 1)) ; → #t
;(array123 (list 1 1 2 4 1)) ; → #f
;(array123 (list 1 1 2 1  2 3)) ; → #t

;--------------------------------------------------------------

;Warmup-2 > string_match 
;Given 2 strings, a and b, return the number of the
;positions where they contain the same length 2 substring.
;So "xxcaazz" and "xxbaaz" yields 3, since the "xx", "aa",
;and "az" substrings appear in the same place in both strings.

;string_match('xxcaazz', 'xxbaaz') → 3
;string_match('abc', 'abc') → 2
;string_match('abc', 'axc') → 0


(define (string_match n str1 str2 count)
  (if (or (> (+ n 2)(string-length str1))(> (+ n 2)(string-length str2)))
      count
      (if (equal? (substring str1 n (+ n 2))(substring str2 n (+ n 2)))
          (string_match (+ n 1) str1 str2 (+ count 1))
          (string_match (+ n 1) str1 str2 count))))

;(string_match 0 "xxcaazz" "xxbaaz" 0) ; → 3
;(string_match 0 "abc" "abc" 0) ; → 2
;(string_match 0 "abc" "axc" 0) ; → 0
      
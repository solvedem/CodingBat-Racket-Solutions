#lang racket

;-------------------------------------------------

;String-1 > hello_name 
;Given a string name, e.g. "Bob", return a
;greeting of the form "Hello Bob!".

;hello_name('Bob') → 'Hello Bob!'
;hello_name('Alice') → 'Hello Alice!'
;hello_name('X') → 'Hello X!'

(define (hello_name name)
  (string-append "Hello " name "!"))

;(hello_name "Bob")
;(hello_name "Alice")
;(hello_name "X")

;-------------------------------------------------

;String-1 > make_abba 
;Given two strings, a and b, return the
;result of putting them
;together in the order abba, e.g. "Hi"
;and "Bye" returns "HiByeByeHi".

;make_abba('Hi', 'Bye') → 'HiByeByeHi'
;make_abba('Yo', 'Alice') → 'YoAliceAliceYo'
;make_abba('What', 'Up') → 'WhatUpUpWhat'

(define (make_abba a b)
  (string-append  a b b a))

;(make_abba "Hi" "Bye")
;(make_abba "Yo" "Alice")
;(make_abba "What" "Up")

;-------------------------------------------------

;String-1 > make_tags 
;The web is built with HTML strings like
;"<i>Yay</i>" which draws Yay as italic text.
;In this example, the "i" tag makes <i> and </i>
;which surround the word "Yay". Given tag and word
;strings, create the HTML string with tags around the
;word, e.g. "<i>Yay</i>".

;make_tags('i', 'Yay') → '<i>Yay</i>'
;make_tags('i', 'Hello') → '<i>Hello</i>'
;make_tags('cite', 'Yay') → '<cite>Yay</cite>'

(define (make_tags tag str)
  (string-append "<" tag ">" str "</" tag ">"))

;(make_tags "i" "Yay")
;(make_tags "i" "Hello")
;(make_tags "cite" "Yay")


;-------------------------------------------------

;String-1 > make_out_word
;Given an "out" string length 4, such as
;"<<>>", and a word, return a new string where
;the word is in the middle of the out string,
;e.g. "<<word>>".

;make_out_word('<<>>', 'Yay') → '<<Yay>>'
;make_out_word('<<>>', 'WooHoo') → '<<WooHoo>>'
;make_out_word('[[]]', 'word') → '[[word]]'

;-------------------------------------------------

;String-1 > extra_end 
;Given a string, return a new string made of
;3 copies of the last 2 chars of the original
;string. The string length will be at least 2.

;extra_end('Hello') → 'lololo'
;extra_end('ab') → 'ababab'
;extra_end('Hi') → 'HiHiHi'

(define (extra_end str)
  (define sub (substring str (- (string-length str) 2)(string-length str)))
  (string-append sub sub sub))

;(extra_end "Hello")
;(extra_end "ab")
;(extra_end "Hi")

;-------------------------------------------------

;String-1 > first_two 
;Given a string, return the string made of
;its first two chars, so the String "Hello"
;yields "He". If the string is shorter than
;length 2, return whatever there is, so "X"
;yields "X", and the empty string "" yields the
;empty string "".

;first_two('Hello') → 'He'
;first_two('abcdefg') → 'ab'
;first_two('ab') → 'ab'

(define (first_two str)
  (if (< (string-length str) 2)
      str
      (substring str 0 2)))

;(first_two "Hello")
;(first_two "abcdefg")
;(first_two "ab")

;-------------------------------------------------

;String-1 > first_half 
;Given a string of even length, return
;the first half. So the string "WooHoo" yields "Woo".

;first_half('WooHoo') → 'Woo'
;first_half('HelloThere') → 'Hello'
;first_half('abcdef') → 'abc'

(define (first_half str)
  (substring str 0 (/ (string-length str) 2)))

;(first_half "WooHoo")
;(first_half "HelloThere")
;(first_half "abcdef")


;-------------------------------------------------

;String-1 > without_end 
;Given a string, return a version without
;the first and last char, so "Hello" yields
;"ell". The string length will be at least 2.

;without_end('Hello') → 'ell'
;without_end('java') → 'av'
;without_end('coding') → 'odin'

(define (without_end str)
  (substring str 1 (- (string-length str) 1)))

;(without_end "Hello")
;(without_end "java")
;(without_end "coding")
;-------------------------------------------------

;String-1 > combo_string 
;Given 2 strings, a and b, return a string
;of the form short+long+short, with the
;shorter string on the outside and the longer
;string on the inside. The strings will not
;be the same length, but they may be empty (length 0).

;combo_string('Hello', 'hi') → 'hiHellohi'
;combo_string('hi', 'Hello') → 'hiHellohi'
;combo_string('aaa', 'b') → 'baaab'

(define (combo_string s1 s2)
  (if (> (string-length s1)(string-length s2))
      (string-append s2 s1 s2)
      (string-append s1 s2 s1)))

;(combo_string "Hello" "hi")
;(combo_string "hi" "Hello")
;(combo_string "aaa" "b")

;-------------------------------------------------


;String-1 > non_start 
;Given 2 strings, return their concatenation,
;except omit the first char of each. The strings
;will be at least length 1.

;non_start('Hello', 'There') → 'ellohere'
;non_start('java', 'code') → 'avaode'
;non_start('shotl', 'java') → 'hotlava'

(define (non_start s1 s2)
  (string-append (substring s1 1 (string-length s1))
                 (substring s2 1 (string-length s2))))

;(non_start "Hello" "There")
;(non_start "java" "code")
;(non_start "shotl" "java")

;-------------------------------------------------

;String-1 > left2 
;Given a string, return a "rotated left 2" version
;where the first 2 chars are moved to the end. The
;string length will be at least 2.

;left2('Hello') → 'lloHe'
;left2('java') → 'vaja'
;left2('Hi') → 'Hi'


(define (left2 str)
  (string-append (substring str 2 (string-length str))
                 (substring str 0 2)))

;(left2 "Hello")
;(left2 "java")
;(left2 "Hi")
                 
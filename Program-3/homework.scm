; Function to count how many values in range are divisible by third parameter
(define (count-div low high div)
  (let* ((start (min low high)) ; Start with smaller number
         (end (max low high))   ; End with larger number

         (first-divisible
          (if (= (remainder start div) 0) ; If start%div == 0
              start                       ; Then begin with start
              (+ start (- div (remainder start div)))))) ; Else increment

    (if (> first-divisible end) ; If first divisible > end range
        0                       ; Return 0
        (+ 1 (quotient (- end first-divisible) div))))) ; Count how many divs fit in range

; Rewriting above to accept predicate function
(define (count-predicate low high pred)
  (let* ((start (min low high))    
         (end (max low high)))

    ; Helper function to loop through range
    (define (helper current count)
      (cond
        ((> current end) count) ; If you pass end, return count
        ((pred current) (helper (+ current 1) (+ count 1))) ; If pred is true, increment count
        (else (helper (+ current 1) count))))

      (helper start 0)))

(write (count-div 1 12 4))
(newline)

(write (count-predicate 1 12 (lambda (a) (= (modulo a 3) 0))))
(newline)
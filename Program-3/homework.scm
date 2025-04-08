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


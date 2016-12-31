# scheme-actors

A toy implementation of a Scheme-like language that implements a vague notion of actors with concurrent message passing and futures.

The interpreter is based on code from this wikibook: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

It's been simplified and modified fairly heavily. No complicated error handling, dotted lists, or mutable variables.

## Example

    ; Fibonacci function.
    (define fib (lambda (n)
      (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))

    ; Factorial function.
    (define fact (lambda (n)
      (if (= n 0) 1 (* n (fact (- n 1))))))

    ; Actor that computes a Fibonacci number and prints when done.
    (define fib@ (alpha (n)
      (begin0 (fib n)
              (display "(fib ")
              (display n)
              (displayln ") is available."))))

    ; Actor that computes n! and prints when done.
    (define fact@ (alpha (n)
      (begin0 (fact n)
              (display "(fact ")
              (display n)
              (displayln ") is available."))))

    ; Compute the 25th Fibonacci number and 9!. Then
    ; evaluate the futures and subtract them.
    (let ((fib-future (fib@ 25))
          (fact-future (fact@ 9)))
      (displayln (- (fact-future) (fib-future))))

The following output is displayed:

    (fact 9) is available.
    (fib 25) is available.
    287855

Although the `(fib@ 25)` message pass happens first, `(fact@ 9)` is the first to complete due to the inefficiency of `fib`. On the last line, both `future`s are evaluated, and the calling site blocks until both are available before computing and printing the difference.

; 1.2.a
; full-trace-wrapper breaks tail recursion because when proc calls itself, full-trace-wrapper needs to save the current stack frame (to save val). This allows tail-call optimization where the computer doesn't need to create a new stack frame on each recursive function call.
; Since this is just an optimization, it is not essential and merely provides a performance improvement. I do not believe it is possible to make a full trace wrapper that is tail recursive-compatible, since it would need to do something with the returned value of proc before returning. However, it would be easy to make a wrapper that only prints out the entries to proc and is tail recursive-compatible, by simply printing the entry info and then returning (apply proc args) immediately.


; 1.2.b
(define (first-layer-trace-wrapper procname proc args)
  (if (not (eq-get proc 'first-layer-trace-wrapper-is-first))
    (begin 
      (eq-put! proc 'first-layer-trace-wrapper-is-first #t)
      (newline)
      (display ";Entering ") (write procname)
      (display " with ") (write args)
      (let ((val (apply proc args)))
        (newline)
        (display ";Leaving ") (write procname)
        (display " Value=") (write val)
        (eq-put! proc 'first-layer-trace-wrapper-is-first #f)
        val))
  (apply proc args)))

;Testing
(display "\nfact1\n")
(display (fact 10))

(advise-n-ary fact first-layer-trace-wrapper)
(display "\n\nfact2\n")
(fact 10)

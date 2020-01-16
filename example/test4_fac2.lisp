(decl
  
  (define fac (fun (n)
    (if (== n 0)
      1
      (* n (fac (- n 1))))))
  
  (define main (log (show (fac 6))))
)

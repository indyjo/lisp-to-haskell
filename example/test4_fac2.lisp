(decl
  
  (define fac (fun (n)
    (if (== n 0)
      1
      (* n (fac (- n 1))))))
  
  (define main (do
    (v (compute	(fac 6)))
    (_ (log	(show v)))))

)

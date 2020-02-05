(do
	(n	(alloc 0))
	(_	(while
		(do
			(x	(get n))
			(_	(compute (< x 3))))
		(do
			(_	(log "Your name: "))
			(name	getLine)
			(_	(log (<> "Hello, " name)))
			(_	(update n (fun (x) (+ x 1)))))))
)

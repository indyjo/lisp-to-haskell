(do
	(r (alloc 1))
	(n (alloc 6))
	(_ (while
		(do
			(v (get n))
			(_ (compute (> v 1))))
		(do
			(v (get n))
			(_ (update r (fun (x) (* v x))))
			(_ (update n (fun (x) (+ x (-1)))))
		)
	))
	(f (get r))
	(_ (log (show f)))
)


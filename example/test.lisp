(do
	(n (alloc 0))
	(cond_action (compute (do
		(nv (get n))
		(_ (compute (< nv 3))))))
	(_ (while cond_action (do
		(_ (putStrLn "Your name: "))
		(name getLine)
		(_ (putStrLn (++ "Hello, " name)))
		(_ (update n (+ 1))))))
)

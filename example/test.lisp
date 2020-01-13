(do
	(n (alloc 0))
	(cond_action (compute (do
		(nv (get n))
		(_ (compute (< nv 3))))))
	(_ (while cond_action (do
		(_ (log "Your name: "))
		(name getLine)
		(_ (log (<> "Hello, " name)))
		(_ (update n (+ 1))))))
)

(decl
	(record Employee
		(name	String)
		(salary	Int)
	)
	(define e (Employee "Jonas" 1000000))
	(define main
		(do
			(_	(log "Employee name:"))
			(_	(log (name e)))
			(_	(log "Employee salary:"))
			(_	(log (show (salary e))))
		)
	)
)

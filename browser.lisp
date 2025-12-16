(fun a 
	(latch 
		(rv -1 1)
		(st 100)))

(step-gen
	(index 
		(a) (rv 0 1))
	(st 1))
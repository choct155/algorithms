MODULE UF_class
	IMPLICIT NONE

		TYPE UF 	
			!Declare variables
			INTEGER :: n,p,q,p_q
		END TYPE UF

	CONTAINS
		FUNCTION add (p,q) result (p_q)
			p_q=p+q
		END FUNCTION add
END MODULE UF_class

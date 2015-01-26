PROGRAM test
	USE vars

	IMPLICIT NONE

	ALLOCATE(vec(5))

	PRINT *, 'Hello Motherfucker!'

	CALL test_call

	PRINT *, 'Might as well sneak an array op in'

	!For each spot in the vector...
	DO i=1,SIZE(vec)
		!...print the number...
		PRINT *,i
		!...and store its square in the vector...
		vec(i)=i**2.
	END DO

	!Print the vector
	PRINT *,vec

END PROGRAM test
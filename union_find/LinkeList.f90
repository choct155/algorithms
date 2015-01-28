!This implementaton of linked lists in Fortran is an adaptation:
!"An Introduction to Fortran Pointer Techniques", A. Stock (2009)

MODULE LinkeList
	IMPLICIT NONE

	!Construct derived type to hold:
		!(1) the value of the number in the collection; and,
		!(2) a pointer to the address of the next collection member
	TYPE tElem



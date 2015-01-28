!This implementaton of linked lists in Fortran is an adaptation:
!"An Introduction to Fortran Pointer Techniques", A. Stock (2009)

MODULE LinkeList
	IMPLICIT NONE

	!Construct derived type to hold:
	TYPE tElem
		!(1) the value of the number in the collection; and,
		INTEGER :: val
		!(2) a pointer to the address of the next collection member
		TYPE(tElem), Pointer :: nextElem
	END TYPE tElem

CONTAINS
	
	!Define routine to add an element to the linked list
	SUBROUTINE addElem(firstElem,aElem,newVal)
		!Declare pointers: first position and floating position
		TYPE(tElem),POINTER :: firstElem, aElem
		!Declare new value
		INTEGER :: newVal

		!Start at the beginning of the collection
		aElem=>firstElem
		!By testing the association of the current pointer...
		DO WHILE(ASSOCIATED(aElem))
			!...move through to the last element
			aElem=>aElem%nextElem
		END DO

		!Allocate memory for new pointer
		ALLOCATE(aElem%nextElem)

		!Write value of new pointer to memory
		aElem%nextElem%val=newVal

		!Move to the new pointer
		aElem=>aElem%nextElem

		!Close out the list
		NULLIFY(aElem%nextElem)
	END SUBROUTINE addElem

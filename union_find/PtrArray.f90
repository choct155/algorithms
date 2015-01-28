MODULE PtrArray
	IMPLICIT NONE

	!Construct derived type to enable pointer arrays with our derived pointer
	TYPE tElemPtr
		TYPE(tElem), POINTER :: Elem
	END TYPE tElemPtr


	!Declare pointer array
	TYPE(tElemPtr),DIMENSION(:),ALLOCATABLE :: Elems

END MODULE PtrArray
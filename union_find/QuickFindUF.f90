!This class provides an object that can:
!	1. Locate elements in a collection;
!	2. Test their connection;
!	3. Create unions between previously unconnected elements.

!The construction is a Fortran adaptation of the Java implementation
!provided by Robert Sedgewick of Princeton (Algorithms 1, Coursera)

MODULE QuickFindUF
	IMPLICIT NONE

	!Construct a quick find object that holds:
	TYPE QF_obj
		INTEGER						:: val	!(1) the value collection
		INTEGER						:: id	!(2) the corresponding IDs
		TYPE(QF_obj),POINTER		:: next !(3) a pointer to the next item in the collection
	END TYPE QF_obj

	!Construct an object to hold a collection of quick find objects
	TYPE QuickFindList
		TYPE(QF_obj),POINTER	:: first !A pointer to the first list position
	END TYPE QuickFindList

CONTAINS

	!*********************************
	!***Define explicit constructor***
	!*********************************

	FUNCTION new_QF() RESULT(new_qf_coll)
		TYPE(QuickFindList)		:: new_qf_coll	!Declare new quick find collection
		
		!Allocate memory for the first position
		ALLOCATE(new_qf_coll%first)

		!Start with only the pointer to first position (aka - empty list)
		NULLIFY(new_qf_coll%first%next)
	END FUNCTION new_QF

	!**************************************************************
	!***Define function to populate collection with given values***
	!**************************************************************

	SUBROUTINE fill_QF(coll,n,vals)
		TYPE(QuickFindList),INTENT(INOUT)	:: coll 		!Declare collection to be populated
		INTEGER,DIMENSION(n),INTENT(IN)		:: vals			!Declare array of values in collection
		INTEGER,INTENT(IN)					:: n 			!Declare number of elements in collection
		INTEGER								:: i 			!Declare iterator
		TYPE(QF_obj),POINTER 			:: current		!Declare current position pointer

		!Initialize pointer position
		current=>coll%first

		!Populate the ID array
		DO i=1,n		!Roll through each member of the collection...
			ALLOCATE(current%next)		!Allocate space for next QF object
			current%val=vals(i)			!Assign given value to new QF object
			current%id=i 				!Assign sequential ID to new QF object
			current=>current%next		!Move the current pointer forward
		END DO

		!Once the values have been assigned, close out the collection
		NULLIFY(current%next)
	END SUBROUTINE fill_QF

	!********************************************************
	!***Define function to print contents of QF collection***
	!********************************************************

	SUBROUTINE print_QF_coll(coll)
		TYPE(QuickFindList),INTENT(IN)	:: coll 	!Declare input collection
		TYPE(QF_obj),POINTER 			:: current	!Declare current position pointer

		!Initialize pointer position to first item in collection
		current=>coll%first%next

		!Print header
		PRINT *, 'ID 	Value'

		!Print IDs and values
		DO 		!Roll through the collection
			IF(.NOT. ASSOCIATED(current%next)) EXIT 	!...if we hit the end, stop...
			PRINT *, current%id,' 	',current%val 		!...otherwise print the current QF object'
			current=>current%next						!...and proceed to the next item
		END DO

	END SUBROUTINE print_QF_coll

	!*******************************************************
	!***Define function to return the ID of a given value***
	!*******************************************************

	FUNCTION val_id(coll,inval) RESULT(vid)
		TYPE(QuickFindList),INTENT(IN)		:: coll 		!Declare input QF collection
		INTEGER,INTENT(IN) 					:: inval		!Declare value to be located
		INTEGER 							:: vid 			!Declare ID to be returned for given value
		TYPE(QF_obj),POINTER  				:: current 		!Declare pointer to current item
		LOGICAL 							:: found 		!Declare logical marker signaling identification

		!Point current to collection start
		current=>coll%first%next

		!I suspect we haven't found shit yet
		found=.FALSE.

		!Identify the ID associated with the given value
		PRINT *,'***Searching for ID of',inval,'***'
		DO 		!Roll through each item...
			IF(.NOT. ASSOCIATED(current%next)) EXIT 	!...if at the end of the list, stop...
			IF(current%val==inval) THEN 				!...otherwise, if the value has been located...
				vid=current%id 							!...assign the associated ID to the output vehicle...
				found=.TRUE. 							!...and commemorate the occasion with our logical...
			ELSE 
				current=>current%next 					!...otherwise, advance to the next item
			ENDIF
		END DO

		!If we didn't locate the value, tell me about it
		IF(found.EQV..FALSE.) THEN 
			PRINT *, "Man!!! We ain't found shit! ... with respect to",inval
		ELSE
			PRINT *, "The ID of",inval,"is",vid
		ENDIF

	END FUNCTION val_id


	!************************************************************
	!***Define function to identify connections between values***
	!************************************************************

	FUNCTION QF_connected(coll,p,q) RESULT(connected)
		TYPE(QuickFindList),INTENT(IN)		:: coll 		!Declare input QF collection
		INTEGER,INTENT(IN)					:: p 			!Declare first test value
		INTEGER,INTENT(IN)					:: q  			!Declare second test value
		LOGICAL								:: connected 	!Declare test outcome logical
! 		TYPE(QuickFindUF),POINTER 			:: current_p	!Declare pointer to current item in 'p' search
! 		TYPE(QuickFindUF),POINTER 			:: current_q	!Declare pointer to current item in 'q' search
		INTEGER 							:: pid 			!Declare ID container for first value
		INTEGER 							:: qid 			!Declare ID container for second value

		!Initialize ID variables to zero (Fortran is unity-indexed)
		pid=0
		qid=0

		!Probably didn't find any connection yet
		connected=.FALSE.

		!Leverage val_id function to capture IDs of p and q
		pid=val_id(coll,p)
		qid=val_id(coll,q)

		!If at least one value is not in the collection, let me know
		IF(pid==0.OR.qid==0) PRINT *, 'At least one of the values is not in the collection'

		!Test for a connection
		IF(pid==qid.AND.pid>0) connected=.TRUE.

	END FUNCTION QF_connected

END MODULE QuickFindUF
PROGRAM union_find
	USE QuickFindUF
	
	IMPLICIT NONE

	!Declarations
	INTEGER,DIMENSION(5)	:: val_set=(/ 4,8,1,2,5 /)  !Declare values to be manipulated
	TYPE(QuickFindList)		:: qf_coll					!Declare collection structure

	!Initialize new QF collection
	PRINT *,'***Initializing new Quick Find collection***'
	qf_coll=new_QF()

	!Populate new QF collection
	PRINT *,'***Populating new Quick Find collection***'
	CALL fill_QF(qf_coll,SIZE(val_set),val_set)

	!Print contents of collection
	CALL print_QF_coll(qf_coll)

END PROGRAM union_find
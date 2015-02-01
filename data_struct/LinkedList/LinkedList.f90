!This implementation of linked lists in Fortran is an adaptation:
!"An Introduction to Fortran Pointer Techniques", A. Stock (2009)
!"Object Oriented Programming via Fortran 90/95", Ed Akin (2003)

MODULE LinkedList
    IMPLICIT NONE
    
    !Construct derived type to hold:
    TYPE tElem
        INTEGER                 :: val          !(1) the value of the number in the list; and,
        TYPE(tElem),POINTER     :: nextElem     !(2) a pointer to the address of the next list member.
    END TYPE tElem
    
    !Construct a derived type to hold the tElem items defined above
    TYPE tElemList
        TYPE(tElem),POINTER     :: firstElem    !A pointer to the first list position (list is never empty)
    END TYPE tElemList
    
    CONTAINS
    
    !**************************************
    !***Define explicit list constructor***
    !**************************************
    FUNCTION new_tElemList() RESULT(new_list)
        TYPE(tElemList)         :: new_list     !Declare linked list output
        
        !Allocate memory for first position
        ALLOCATE(new_list%firstElem)
        
        !Start with an empty list
        NULLIFY(new_list%firstElem%nextElem)
    END FUNCTION new_tElemList
    
    !*********************************************************    
    !***Define routine to add an element to the linked list***
    !*********************************************************
    SUBROUTINE addElem(list,newVal)
        TYPE(tElemList),INTENT(INOUT)   :: list         !Declare linked list to be modified
        INTEGER                         :: newVal       !Declare value to be added to the list
        TYPE(tElem),POINTER             :: previous     !Declare previous position pointer
        TYPE(tElem),POINTER             :: current      !Declare current position pointer
        
        !Initialize positions of previous (first) and current (second) pointers in list
        previous=>list%firstElem
        current=>previous%nextElem
        
        !Roll through the list...
        DO
            !...if we get to the end of the list, we will append...
            IF(.NOT. ASSOCIATED(current)) EXIT
            !...if the value to be inserted is smaller than the current value, we will insert it before current...
            IF(newVal<current%val) EXIT
            !...otherwise, just keep searching for the right spot
            previous=>previous%nextElem
            current=>current%nextElem
        END DO
        
        !Allocate space before the current position
        ALLOCATE(previous%nextElem)
        
        !Assign the new value
        previous%nextElem%val=newVal
        
        !Point the new item to current
        previous%nextElem%nextElem=>current
    END SUBROUTINE addElem
    
    !**************************************************************
    !***Define routine to remove an element from the linked list***
    !**************************************************************    
    SUBROUTINE rmElem(list,dropVal)
        TYPE(tElemList),INTENT(INOUT)   :: list         !Declare linked list to be modified
        INTEGER                         :: dropVal      !Declare value to be dropped from the list
        LOGICAL                         :: found        !Declare logical to mark identification of the value to be dropped
        TYPE(tElem),POINTER             :: previous     !Declare previous position pointer
        TYPE(tElem),POINTER             :: current      !Declare current position pointer
        
        
        !Initialize positions of previous (first) and current (second) pointers in list
        previous=>list%firstElem
        current=>previous%nextElem
        
        !Initialize value identification logical
        found=.FALSE.
        
        !Roll through the list...
        DO
            !...if the value in question has already been found or we reach the end of the list, stop progressing...
            IF(found.OR.(.NOT. ASSOCIATED(current))) THEN
                PRINT *, 'Value has been marked found, or does not exist in this list.'
                PRINT *, 'Aborting rmElem call'
            ENDIF
            !...if we find the value in question...
            IF(dropVal==current%val) THEN
                !...commemorate the event...
                found=.TRUE.
                PRINT *, 'Eureka!',dropVal,'exists at',LOC(current)
                !...and stop looking...
                EXIT
            !...otherwise, keep looking
            ELSE
                previous=>previous%nextElem
                current=>current%nextElem
            ENDIF
        END DO
        
        !If we have found the value and stopped on it...
        IF(found) THEN
            !...bypass (aka - delete) the element...
            previous%nextElem=>current%nextElem
            PRINT *, 'Deleting',LOC(current)
            !...and free the space used by the dropped element
            DEALLOCATE(current)
        ENDIF
        
    END SUBROUTINE rmElem
        
    !********************************************    
    !***Define convenience function: Is empty?***
    !********************************************
    FUNCTION list_empty(list) RESULT(TorF)
        TYPE(tElemList),INTENT(IN)  :: list     !Declare input list
        LOGICAL                     :: TorF     !Declare outcome of emptiness test
        
        !Test is for association of the first position pointer
        TorF=.NOT. ASSOCIATED(list%firstElem%nextElem)
    END FUNCTION list_empty
    
    !****************************************
    !***Define convenience function: Print***
    !****************************************
    SUBROUTINE print_list(list)
        TYPE(tElemList),INTENT(IN)  :: list     !Declare input list
        TYPE(tElem),POINTER         :: current  !Declare pointer to link values to list position
        INTEGER                     :: i        !Counter
        
        !Initialize pointer position at the beginning of the list
        current=>list%firstElem%nextElem
        
        !Initialize counter
        i=0
        
        !Print header
        PRINT *, 'Position      Value'
        
        !Roll through the list...
        DO
            !...if we reach the end, stop...
            IF(.NOT. ASSOCIATED(current%nextElem)) EXIT
            !...otherwise iterate...
            i=i+1
            !...print the current (position,value)...
            PRINT *, i,'        ',current%val
            !...and proceed to the next element
            current=>current%nextElem
        END DO
    END SUBROUTINE print_list
    
    !************************************
    !***Define set membership function***
    !************************************
    FUNCTION in_LinkedList(list,valTest) RESULT(found)
        TYPE(tElemList),INTENT(IN)  :: list     !Declare input list
        TYPE(tElem),POINTER         :: current  !Declare pointer to link values to list position
        INTEGER                     :: valTest  !Declare value being tested for membership
        LOGICAL                     :: found    !Declare logical indicating discovery of value
        
        !Initialize pointer position at the beginning of the list
        current=>list%firstElem%nextElem
        
        !Initialize discovery logical 
        found=.FALSE.
        
        !Roll through the list....
        DO
            !...if we reach the end, stop...
            IF(.NOT. ASSOCIATED(current%nextElem)) EXIT
            !...otherwise, if the current value is the one we are looking for...
            IF(current%val==valTest) THEN
                !...mark the discovery with the logical...
                found=.TRUE.
                !...and stop looking...
                EXIT
            ELSE
                !...otherwise move to the next value
                current=>current%nextElem
            ENDIF
        END DO
    END FUNCTION in_LinkedList
    
    !*********************************************************************
    !***Define convenience function to combine the elements of two sets***
    !*********************************************************************
    FUNCTION LinkedList_combine(list1,list2) RESULT(list1_2)
        TYPE(tElemList),INTENT(IN)      :: list1        !Declare first input list
        TYPE(tElemList),INTENT(IN)      :: list2        !Declare second input list
        TYPE(tElemList)                 :: list1_2      !Declare composite output list
        TYPE(tElem),POINTER             :: current      !Declare position holder for first list
        
        !Initialize output list
        list1_2=list2
        
        !Initialize pointer position at beginning of first list
        current=>list1%firstElem%nextElem
        
        !Roll through each element of the first list...
        DO
            !...if we reach the end, stop...
            IF(.NOT. ASSOCIATED(current%nextElem)) EXIT
            !...otherwise, add the value from the current element in list 1 to list 2
            CALL addElem(list1_2,current%val)
            !...and advance the current pointer
            current=>current%nextElem
        END DO
        
    END FUNCTION LinkedList_combine

    
    !***Define function to return unique elements of a linked list (a linked set)***
    
    !***Define function to return exclusive disjunction between two linked sets***
        
END MODULE LinkedList
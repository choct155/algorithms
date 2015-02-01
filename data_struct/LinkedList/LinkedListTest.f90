!INCLUDE '/home/choct155/Projects/algorithms/data_struct/LinkedList/LinkedList.f90'                                                                                
PROGRAM LinkedListTest
    USE LinkedList
                              
    IMPLICIT NONE
    
    !Declare something to work with
    INTEGER,DIMENSION(5) :: arr1=(/ 1,2,3,4,5 /)
    INTEGER,DIMENSION(5) :: arr2
    INTEGER :: scalar=2
    INTEGER :: i
    
    !Declare new linked list
    TYPE(tElemList) :: ll
    TYPE(tElemList) :: ll2
    TYPE(tElemList) :: ll1_2
    
    !Initialize second array
    arr2=(/ (arr1(i)+5,i=1,5) /)
    
    !Initialize new linked lists
    PRINT *, '***Initializing new linked lists***'
    PRINT *, NEW_LINE('A')
    ll=new_tElemList()
    ll2=new_tElemList()
    
    !Check list status
    PRINT *, 'Is the list empty?',list_empty(ll)
    
    !Add the elements of the first array to ll
    PRINT *, NEW_LINE('A')
    PRINT *, '***Adding elements of the first array to the linked list***'
    PRINT *, NEW_LINE('A')
    DO i=1,SIZE(arr1)
        CALL addElem(ll,arr1(i))
    END DO
    
    !Check list status
    PRINT *, 'Is the list empty?'
    IF(list_empty(ll)) THEN
        PRINT *, 'It appears to still be empty...'
    ELSE
        PRINT *, 'Nope, there is stuff in there.  See for yourself...'
        CALL print_list(ll)
    ENDIF
    
    !Remove a single value
    PRINT *, NEW_LINE('A')
    PRINT *, '***Removing a single value***'
    PRINT *, NEW_LINE('A')
    CALL rmElem(ll,3)
    CALL print_list(ll)
    
    !Test for the existence of a value in the list
    PRINT *, NEW_LINE('A')
    PRINT *, '***Testing set membership***'
    PRINT *, NEW_LINE('A')
    PRINT *, 'Is 2 currently in the linked list?',in_LinkedList(ll,2)
    
    !Combine two lists
    PRINT *, NEW_LINE('A')
    PRINT *, '***Adding elements of the second array to the second linked list***'
    PRINT *, NEW_LINE('A')
    DO i=1,SIZE(arr2)
        CALL addElem(ll2,arr2(i))
    END DO
    PRINT *, 'What does the combination of linked lists look like?'
    ll1_2=LinkedList_combine(ll,ll2)
    CALL print_list(ll1_2)
    
END PROGRAM LinkedListTest
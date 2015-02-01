# Makefile for the LinkedList test collection

# Define function to compile and link component scripts
ll_test: LinkedListTest.o LinkedList.o
	gfortran -o ll_test LinkedListTest.o LinkedList.o

# Define functions for each script that compile without linking
LinkedList.mod: LinkedList.o LinkedList.f90
	gfortran -c LinkedList.f90

LinkedList.o: LinkedList.f90
	gfortran -c LinkedList.f90

LinkedListTest.o: LinkedListTest.f90
	gfortran -c LinkedListTest.f90

# Define housekeeping function
clean:
	rm LinkedListTest.o LinkedList.o ll_test 
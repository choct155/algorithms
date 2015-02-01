# Makefile for the union_find collection

# Define function to compile and link component scripts
u_find: union_find.o QuickFindUF.o
	gfortran -o u_find union_find.o QuickFindUF.o

# Define functions for each script that compile without linking
QuickFindUF.mod: QuickFindUF.o QuickFindUF.f90
	gfortran -c QuickFindUF.f90

QuickFindUF.o: QuickFindUF.f90
	gfortran -c QuickFindUF.f90

union_find.o: union_find.f90
	gfortran -c union_find.f90

# Define housekeeping function
clean:
	rm union_find.o QuickFindUF.o u_find 
# Makefile for the union_find collection

# Define function to compile and link component scripts
u_find: union_find.o UF_class.o
	gfortran -o u_find union_find.o UF_class.o

# Define functions for each script that compile without linking
UF_class.mod: UF_class.o UF_class.f90
	gfortran -c UF_class.f90

UF_class.o: UF_class.f90
	gfortran -c UF_class.f90

union_find.o: union_find.f90
	gfortran -c union_find.f90

# Define housekeeping function
clean:
	rm union_find.o UF_class.o u_find 
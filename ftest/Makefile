# Makefile for the fortran test (ftest) collection

# Define function to compile and link component scripts
hello: test.o test_call.o vars.o
	gfortran -o hello test_call.o test.o vars.o

# Define functions for each script that compile without linking
vars.mod: vars.o vars.f90
	gfortran -c vars.f90

vars.o: vars.f90
	gfortran -c vars.f90

test.o: test.f90
	gfortran -c test.f90

test_call.o: test_call.f90
	gfortran -c test_call.f90

# Define housekeeping function
clean:
	rm test.o test_call.o hello vars.0
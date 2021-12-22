performance.f90 contains a module with function to perform matrix multiplication and 
a main proram to test it. It exploits the debugging module for warnings, error handling, pre and post conditions and checkpoints.
To compile:
gfortran debugging.f90 performance.f90 -o performance.out
The performance.out executable it's already available
To execute:
./performance.out


matrix.f90 is a program for testing the functions and subroutines contained in
the 'matrix' module. It exploits the debugging module for warnings, error handling, pre and post conditions and checkpoints.
To compile
gfortran debugging.f90 matrix.f90 -o matrix.out
The matrix.out executable it's already available
To execute:
./matrix.out


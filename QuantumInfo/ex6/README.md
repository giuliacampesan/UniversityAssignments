to compile:
gfortran -I/opt/homebrew/Cellar/fftw/3.3.10/include -L/opt/homebrew/Cellar/fftw/3.3.10/lib split_operator.f90 debugging.f90 matrix.f90 main.f90  -lfftw3 -lm -llapack -lblas -ffree-line-length-0 -ofast -o dmat.out
to execute:
./main.out N d separable step
e.g.
./main.out 2 2 .FALSE. 1
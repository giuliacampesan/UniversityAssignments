This is a program to compute the time evolution of the ground state of the harmonic oscillator
to compile: 
gfortran -I/opt/homebrew/Cellar/fftw/3.3.10/include -L/opt/homebrew/Cellar/fftw/3.3.10/lib split_operator.f90 debugging.f90 harmonic_oscillator.f90  -lfftw3 -lm -llapack -lblas -ffree-line-length-0 -o time_evolution.out


to execute: 
./time_evolution.out Nx L1 L2 Nt t0 tf tT mass omega

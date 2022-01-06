!program to test used subroutines
program performance
    use matrixmultiplication
    implicit none

    integer rows1; integer cols1;
    integer rows2; integer cols2;
    real*8, dimension (:,:), allocatable :: matrix1
    real*8, dimension (:,:), allocatable :: matrix2
    real*8, dimension (:,:), allocatable :: myresults, myresultsT, routine
    integer iterations;
    integer :: mdim
    !integer n
    real*8 start, finish
    integer jj 
    real*8, dimension(:), allocatable ::  times
    integer narg
    character(100) :: ch_mdim
    real*8 :: precision
   
    !logical value to be used in the check routine
    debug=.TRUE.
    stop_exec= .TRUE.
    precision = 10.0d-14    !value up to which two numbers are considered to be equal
    iterations = 1

    

    open(1, file = 'matmulF_prv.dat', status = 'unknown') 
    open(2, file = 'matmulrow_prv.dat', status = 'unknown') 
    open(3, file = 'matmulcol_prv.dat', status = 'unknown') 

    narg= COMMAND_ARGUMENT_COUNT()
    CALL GET_COMMAND_ARGUMENT(narg,ch_mdim) 
    READ(ch_mdim,*)mdim

        !n = mdim
        rows1= mdim; cols1 = mdim !n
        rows2 = mdim; cols2 = mdim

        !matrices allocation
        allocate(matrix1(rows1, cols1))
        allocate(matrix2(rows2, cols2))
        allocate(times(iterations))

        !matrices random initialization
        call random_number(matrix1)
        call random_number(matrix2)

        !matrices multiplication
        myresults = mymatmul_row(matrix1, matrix2, iterations, 2)
        myresultsT = mymatmul_col(matrix1, matrix2, iterations, 3)

        !checking if the two functions return the same matrix-up to set precision
        call matrix_eq(myresults, myresultsT, precision)
        
        !fortran built-in subroutine
        do jj = 1, iterations
            start=0; finish=0
            call cpu_time(start)
            routine=matmul(matrix1, matrix2)
            call cpu_time(finish)
            times(jj) = finish-start
        end do

        print *, average(times)
        !print *, 'fortran routine', rows1, average(times), stdev(times)
        !write(1,*) rows1, average(times), stdev(times)

        !matrices deallocation
        deallocate(matrix1); deallocate(matrix2)
        deallocate(times)

    
    close(1); close(2); close(3)
    
    
    
end program performance


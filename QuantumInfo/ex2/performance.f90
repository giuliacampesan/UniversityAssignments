

    

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
    integer, dimension(:), allocatable :: mdim
    integer n
    real*8 start, finish
    integer jj, ii 
    real*8, dimension(:), allocatable ::  times
    real*8 :: precision
   
    !logical value to be used in the check routine
    debug=.TRUE.
    stop_exec= .TRUE.

    iterations = 1
    mdim= [100, 300, 600, 1200, 1300, 1500, 1700]

    !value up to which two numbers are considered to be equal
    precision = 10.0d-14

    open(1, file = 'matmulF_prv.dat', status = 'unknown') 
    open(2, file = 'matmulstd_prv.dat', status = 'unknown') 
    open(3, file = 'matmulT_prv.dat', status = 'unknown') 

    

    do ii = 1, size(mdim)
        n = mdim(ii)
        rows1= n; cols1 = n !n
        rows2 = n; cols2 = n

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

        print *, 'fortran routine', rows1, average(times), stdev(times)
        write(1,*) rows1, average(times), stdev(times)

        !matrices deallocation
        deallocate(matrix1); deallocate(matrix2)
        deallocate(times)

    end do
    close(1); close(2); close(3)
    
    
    
end program performance


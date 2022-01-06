!this is a module that includes two functions: 
!mymatmul_row does it looping row by row on the first matrix
!mymatmul_col does it looping col by col on the first matrix
!it exploits the 'debugging' module for errors, warning, pre/post-conditions and checkpoints
module matrixmultiplication
   
    use debugging 

    implicit none

    contains

       function mymatmul_row(mat1, mat2, iterations, fwrite) result(mat)
            !dimension of the two input matrices
            integer*4 nrows1, ncols1, nrows2, ncols2
            !input matrices
            real*8, dimension(:,:) :: mat1, mat2
            !output matrix
            real*8, dimension(size(mat1, 1), size(mat2, 2)) :: mat
            !loop variables
            integer*4 row, col, ii
            !cputime storage variables
            real*8 start, finish
            real*8, dimension(iterations) :: times
            integer iitt,  iterations
            !writing to file
            integer fwrite
            !control if debug mode is active
            !logical debug
           
           
            nrows1 = size(mat1, 1);     ncols1 = size(mat1, 2);
            nrows2 = size(mat2, 1);     ncols2 = size(mat2, 2);
            !preconditions on matrices dimensions
            call check( condition=(ncols1.ne.nrows2), &
                        msg_type = 'ERROR', &
                        msg='incompatible shapes, ncols1 different from nrows2')
           
            !matrix multiplication
            do iitt=1, iterations
                start=0; finish=0
                mat=0
                call cpu_time(start)
                do row = 1, nrows1
                    do ii = 1, ncols1
                        do col=1, ncols2
                            mat(row, col) = mat(row, col) + mat1(row, ii)  * mat2(ii, col)
                        enddo
                    enddo
                enddo
                !performance evaluation
                call cpu_time(finish)
                times(iitt) = finish - start
            enddo
            !post-conditions on matrix dimensions
            call check( condition=(size(mat, 1).NE.nrows1).OR.(size(mat, 2).NE.ncols2), &
                        msg_type = 'ERROR', &
                        msg='wrong output matrix shape')

            print *, average(times)           
            !print *, 'matrix mul by rows', nrows1, average(times), stdev(times)
            !write(fwrite,*) nrows1, average(times), stdev(times)

        end function mymatmul_row

        

        function mymatmul_col(mat1, mat2, iterations, fwrite) result(mat)
            
            !dimension of the two input matrices
            integer*4 nrows1, ncols1, nrows2, ncols2
            !input matrices
            real*8, dimension(:,:) :: mat1, mat2
            !output matrix
            real*8, dimension(size(mat1, 1), size(mat2, 2)) :: mat
            !loop variables
            integer*4 row, col, ii
            !cputime storage variables
            real*8 start, finish
            real*8, dimension(iterations) :: times
            integer iitt,  iterations
            !writing to file
            integer fwrite
            !control if debug mode is active
            
           
            nrows1 = size(mat1, 1);     ncols1 = size(mat1, 2);
            nrows2 = size(mat2, 1);     ncols2 = size(mat2, 2);
            
            !preconditions on matrices dimensions
            call check( condition=(ncols1.ne.nrows2), &
                            msg_type = 'ERROR', &
                            msg='incompatible shapes, ncols1 different from nrows2')
            !matrix multiplication
            do iitt =1, iterations
                start=0; finish=0
                mat =0
                call cpu_time(start)
                
                do col=1, ncols2
                    do ii = 1, ncols1
                        do row = 1, nrows1
                            mat(row, col) = mat(row, col) + mat1(row, ii)  * mat2(ii, col)
                        enddo
                    enddo
                enddo
                !performance evaluation
                call cpu_time(finish)
                times(iitt) = finish - start
            enddo
            !post-conditions on matrix dimension
            call check( condition=(size(mat, 1).NE.nrows1).OR.(size(mat, 2).NE.ncols2), &
                        msg_type = 'ERROR', &
                        msg='wrong output matrix shape')

            !print *, 'matrix mul by columns', nrows1, average(times), stdev(times)
            !write(fwrite,*) nrows1, average(times), stdev(times)
            print *, average(times)

        end function mymatmul_col

        !computing average of a vector elements
        function average(x) result(avg)

        real*8, dimension(:) :: x
        real*8 avg

        avg = sum(x)/ (max(1, size(x)))

        end function average


        !computing standard deviation of a vector elements
        function stdev(x) result(std)

        real*8, dimension(:) :: x
        real*8 std

        call check( condition=(size(x)<2), &
                    msg_type = 'ERROR', &
                    msg='division by zero')

        std = sqrt((sum(x**2)-sum(x)**2/size(x))/(size(x)-1)) 

        end function


end module

    


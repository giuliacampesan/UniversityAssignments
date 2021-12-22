module matrixmultiplication
    implicit none
    contains
       function mymatmul_row(mat1, mat2, iterations, fwrite) result(mat)
            
            integer*4 nrows1, ncols1, nrows2, ncols2
            real*8, dimension(:,:) :: mat1, mat2
        
            real*8, dimension(size(mat1, 1), size(mat2, 2)) :: mat
            integer*4 row, col, ii
            real*8 start, finish
            real*8, dimension(iterations) :: times
            integer iitt
            integer iterations

            integer fwrite
           
           
            nrows1 = size(mat1, 1);     ncols1 = size(mat1, 2);
            nrows2 = size(mat2, 1);     ncols2 = size(mat2, 2);
          
            if (ncols1.ne.nrows2) then
                print *, 'incompatible shapes'
            end if

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
                call cpu_time(finish)
                times(iitt) = finish - start
            enddo

            print *, 'matrix mul by rows', nrows1, average(times), stdev(times)
            write(fwrite,*) nrows1, average(times), stdev(times)

        end function mymatmul_row

        

        function mymatmul_col(mat1, mat2, iterations, fwrite) result(mat)
            
            integer*4 nrows1, ncols1, nrows2, ncols2
            real*8, dimension(:,:) :: mat1, mat2
            real*8, dimension(size(mat1, 1), size(mat2, 2)) :: mat

            integer*4 row, col, ii
            real*8 :: start, finish
            real*8, dimension(iterations) :: times
            integer iitt
            integer iterations

            integer fwrite
           
            nrows1 = size(mat1, 1);     ncols1 = size(mat1, 2);
            nrows2 = size(mat2, 1);     ncols2 = size(mat2, 2);
          
            if (ncols1.ne.nrows2) then
                print *, 'incompatible shapes'
            end if

            do iitt =1, iterations
                start=0; finish=0
                mat =0
                call cpu_time(start)
                
                !do col=1, ncols2
                    !do ii = 1, ncols1
                        !do row = 1, nrows1
                do row = 1, nrows1
                    do ii = 1, ncols1
                        do col=1, ncols2
                            mat(col, row) = mat(col, row) + mat2(ii, row)  * mat1(col, ii)
                        enddo
                    enddo
                enddo
                call cpu_time(finish)
                times(iitt) = finish - start
            enddo

            print *, 'matrix mul by columns', nrows1, average(times), stdev(times)
            write(fwrite,*) nrows1, average(times), stdev(times)

        end function mymatmul_col


        function average(x) result(avg)

        real*8, dimension(:) :: x
        real*8 avg

        avg = sum(x)/ (max(1, size(x)))

        end function average



        function stdev(x) result(std)

        real*8, dimension(:) :: x
        real*8 std

        std = sqrt((sum(x**2)-sum(x)**2/size(x))/(size(x)-1)) 

        end function


end module

    


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

    open(1, file = 'matmulF_new2-o0.dat', status = 'unknown') 
    open(2, file = 'matmulstd_new2-o0.dat', status = 'unknown') 
    open(3, file = 'matmulT_new2-o0.dat', status = 'unknown') 

    iterations = 1
    mdim= [100, 300, 500, 700, 900, 1100, 1300, 1500, 1700, 1900]

    do ii = 1, size(mdim)

        n = mdim(ii)
        rows1= n; cols1 = n
        rows2 = n; cols2 = n

        allocate(matrix1(rows1, cols1))
        allocate(matrix2(rows2, cols2))
        allocate(times(iterations))

        call random_number(matrix1)
        call random_number(matrix2)

        myresults = mymatmul_row(matrix1, matrix2, iterations, 2)
        myresultsT = mymatmul_col(matrix1, matrix2, iterations, 3)
        
        do jj = 1, iterations
            start=0; finish=0
            call cpu_time(start)
            routine=matmul(matrix1, matrix2)
            call cpu_time(finish)
            times(jj) = finish-start
        end do

        print *, 'fortran routine', rows1, average(times), stdev(times)
        write(1,*) rows1, average(times), stdev(times)

        deallocate(matrix1); deallocate(matrix2)
        deallocate(times)

    end do
    close(1); close(2); close(3)
    
    
end program performance


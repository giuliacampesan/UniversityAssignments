module matrices

    use debugging
    implicit none

    type :: dcmatrix
        double complex, dimension(:,:), allocatable :: mel
        integer :: nrows, ncols
        double complex ::  trace
        double complex :: determinant
    end type dcmatrix

    !personal note
    !how to define interface for subroutine
    !interface INIT 
        !procedure initialize
    !end interface

    interface operator(.RINIT.)
        module procedure random_initialize
    end interface

    interface operator(.MINIT.)
            module procedure mel_initialize
    end interface

    interface operator(.ADJ.)
            module procedure adjoint
    end interface

    interface operator(.TRC.)
                module procedure trace
    end interface

    contains


    function random_initialize(dim) result(M)
        integer, dimension(2), intent(in) :: dim
        type(dcmatrix) :: M
        integer ii, jj


        M%nrows = dim(1)
        M%ncols = dim(2)
        allocate(M%mel(M%nrows, M%ncols))

        do ii=1, dim(1)
            do jj=1, dim(2)
                M%mel(ii, jj) = COMPLEX(rand(), rand())
            enddo
        enddo

        
    end function

    function mel_initialize(matrix_el) result(M)
        double complex, dimension(:,:), intent(in) :: matrix_el
        type(dcmatrix) :: M


        M%nrows = size(matrix_el, 1)
        M%ncols = size(matrix_el, 2)
        allocate(M%mel(M%nrows, M%ncols))

        M%mel = matrix_el
        M%trace = .TRC.M%mel

        
    end function


    function trace(matrix) result(trc)
        double complex, dimension(:,:), intent(in) :: matrix
        double complex trc
        integer ii

        call check( condition = size(matrix, 1).NE.size(matrix, 2), &
                    msg_type = 'ERROR', &
                    msg='cannot calculate trace for rectangular matrix')

        ii = size(matrix, 1)

        trc=0
        do ii=1, size(matrix, 1)
            trc = trc + matrix(ii, ii)
        enddo
        
    end function

    function adjoint(matrix) result(adj)

        double complex, dimension(:,:), intent(in)  :: matrix
        double complex, dimension(:,:), allocatable:: adj
        
        if (.NOT.allocated(adj)) then
            allocate(adj(size(matrix, 2), size(matrix, 1)))
        end if
        adj = CONJG(TRANSPOSE(matrix))

    end function 


    subroutine printing(matrix, fwrite)

        double complex, dimension(:,:) :: matrix
        integer, optional :: fwrite
        integer ii, nrows

        nrows= size(matrix, 1)

        if(present(fwrite)) then
            
            do ii=1,nrows ! formatted output: https://bit.ly/3ChxboO
                    write(fwrite, fmt="(*('('sf9.6xspf9.6'i)':x))") matrix(ii,:)
                    write(*, fmt="(*('('sf9.6xspf9.6'i)':x))") matrix(ii,:)
            end do
        else  
            do ii=1,nrows ! formatted output: https://bit.ly/3ChxboO
                write(*, fmt="(*('('sf9.6xspf9.6'i)':x))") matrix(ii,:)
        end do
        endif

        

    end subroutine


    subroutine printing_vctr(vctr, fwrite)

        double complex, dimension(:) :: vctr
        integer, optional :: fwrite
        integer ii, nrows

        nrows= size(vctr, 1)

        if(present(fwrite)) then
            
            do ii=1,nrows ! formatted output: https://bit.ly/3ChxboO
                    write(fwrite, fmt="(*('('sf9.6xspf9.6'i)':x))") vctr(ii)
                    write(*, fmt="(*('('sf9.6xspf9.6'i)':x))") vctr(ii)
            end do
        else  
            do ii=1,nrows ! formatted output: https://bit.ly/3ChxboO
                write(*, fmt="(*('('sf9.6xspf9.6'i)':x))") vctr(ii)
        end do
        endif

        

    end subroutine

end module


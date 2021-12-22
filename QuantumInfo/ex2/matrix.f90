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
        integer :: fwrite
        integer ii, nrows

        nrows= size(matrix, 1)

        
           
        do ii=1,nrows ! formatted output: https://bit.ly/3ChxboO
                write(fwrite, fmt="(*('('sf9.6xspf9.6'i)':x))") matrix(ii,:)
                write(*, fmt="(*('('sf9.6xspf9.6'i)':x))") matrix(ii,:)
        end do

        

    end subroutine

end module

program main
    use matrices
    implicit none
    type(dcmatrix) :: M, M_adj
    integer, dimension(2) :: dim
    double complex :: trc, trc_adj
    double complex, dimension(:,:), allocatable :: adj 
    real(8) precision
    

    debug=.TRUE.
    stop_exec= .TRUE.

    open(1, file = 'matrix.dat', status = 'unknown') 


    print *,' Enter the matrix dimensions' 
    read *, dim(1) , dim(2)

    print *, 'Enter precision: 10DEXP format'
    read *, precision

    M = .RINIT.dim
    trc = .TRC.M%mel
    adj = .ADJ.M%mel
    trc_adj = .TRC.adj
    M_adj = mel_initialize(adj)


    print *, 'Matrix'
    call printing(M%mel, 1)
    print *, 'Matrix trace'
    write(*, fmt="(*(sf9.6xspf9.6'i':x))") trc
    print *, 'Adjoint'
    call  printing(adj, 1)
    print *, 'Adjoint trace'
    write(*, fmt="(*(sf9.6xspf9.6'i':x))") M_adj%trace
    

    call check( condition = .NOT.complex_are_eq(trc_adj, M_adj%trace, precision), &
                msg_type='WARNING', &
                msg = 'adjoint matrix trace is different from the conjugate of trace') 
    


    close(1)


end program main
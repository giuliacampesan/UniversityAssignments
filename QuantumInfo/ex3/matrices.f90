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

    interface operator(.RHINIT.)
                module procedure random_hermitian_initialize
    end interface

    interface operator(.RDINIT.)
                    module procedure random_diagonal_initialize
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

        M%trace = .TRC.M%mel

        
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


    function random_uniform(a,b) result(x)
        implicit none
        real,intent(in) :: a,b
        real :: x
        real :: u
        u = rand()
        x = (b-a)*u + a
    end function random_uniform

    function random_stdnormal() result(x)
        real(8) :: x
        real(8), parameter :: pi = 4.d0*atan(1.d0)
        real(8) :: u1,u2

        call RANDOM_NUMBER(u1)
        call RANDOM_NUMBER(u2)
        x = sqrt(-2*log(1-u1))*cos(2*pi*(1-u2))
     end function random_stdnormal

    function random_hermitian_initialize(dim) result(M)
        integer, intent(in) :: dim
        type(dcmatrix) :: M
        integer ii, jj



        M%nrows = dim
        M%ncols = dim
        allocate(M%mel(M%nrows, M%ncols))

        do ii=1, M%nrows
            !M%mel(ii, ii) = random_uniform(-1.,1.)
            M%mel(ii, ii) = random_stdnormal()
            do jj=ii+1, M%ncols
                !M%mel(ii, jj) = COMPLEX(random_uniform(-1.,1.), random_uniform(-1.,1.))
                M%mel(ii, jj) = COMPLEX(random_stdnormal(), random_stdnormal())

                M%mel(jj, ii) = CONJG(M%mel(ii, jj))
            enddo
        enddo

        M%trace = .TRC.M%mel

    end function


    function random_diagonal_initialize(dim) result(M)
        integer, intent(in) :: dim
        type(dcmatrix) :: M
        integer ii, jj



        M%nrows = dim
        M%ncols = dim
        allocate(M%mel(M%nrows, M%ncols))
        do ii=1, M%nrows
            !M%mel(ii, ii) = random_uniform(-1.,1.)
            M%mel(ii, ii) = random_stdnormal()
            do jj=ii+1, M%ncols
                !M%mel(ii, jj) = COMPLEX(random_uniform(-1.,1.), random_uniform(-1.,1.))
                M%mel(ii, jj) = 0

                M%mel(jj, ii) = 0
            enddo
        enddo

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

    subroutine diagonalize(matrix, precision, eigv_file, spacing_file) !result(w)
        double complex, dimension(:,:), intent(in)  :: matrix
        integer nrows, ncols
        character :: jobz, uplo
        integer :: N, lda, lwork, info
        complex(8) :: dummy(1)
        complex(8),  allocatable :: work(:)
        real(8), allocatable ::  eigv(:), rwork(:), norm_spacing(:)
        integer(4), parameter :: nb = 64
        real(8) precision 
        integer eigv_file, spacing_file


        nrows=size(matrix, 1); ncols=size(matrix, 2)
        

        call check (condition=(nrows.NE.ncols), &
                    msg_type='ERROR', &
                    msg='can not diagonalize rectangular matrix')

        jobz = 'N'
        uplo = 'U'
        N = nrows
        lda = max(1, N)
        

        allocate(eigv(N))
        allocate(rwork(max(1, 3*N-2)))
        allocate(norm_spacing(N-1))
        


        lwork=-1
        !   zheev(jobz, uplo, N,  A, lda,   w, work, lwork, rowrk, info)
        call zheev('N', 'U', N, matrix, N, eigv, dummy, lwork, rwork, info)
        if (info == 0) then
           lwork = max((nb+1)*N, nint(real(dummy(1))))
        endif
       
        allocate (work(lwork))
        call zheev('N', 'U', N, matrix, N, eigv, work, lwork, rwork, info)


        call check(condition=(info.GT.0), &
                    msg_type='ERROR', &
                    msg ='the algorithm failed to converge')

        call check(condition=(info.LT.0), &
                    msg_type='ERROR', &
                    msg ='a parameter had an illegal value')

        

        call check ( condition=.NOT.(real_are_eq(sum(eigv), real(.TRC.matrix), precision)), &
                    msg_type='WARNING', &
                    msg = 'trace is not preserved' )
 
        !print *, 'eigenvalues sum', sum(eigv)
        !print *, 'trace', real(.TRC.matrix)
        

        norm_spacing = normalized_spacing(eigv)
        !print *, eigv
        !print *, norm_spacing

        call save_array(norm_spacing, spacing_file)
        call save_array(eigv, eigv_file)
        


        deallocate(eigv)
        deallocate(rwork)
        deallocate (work)
        deallocate(norm_spacing)

    end subroutine diagonalize


    function normalized_spacing(eigv) result(norm_spacing)

        real(8) :: eigv(:), spacing(size(eigv)-1), norm_spacing(size(eigv)-1), mean_spacing

    
        spacing= eigv(2:size(eigv))-eigv(1:(size(eigv)-1))
        mean_spacing = sum(spacing)/size(spacing)
        !print *, mean_spacing
        norm_spacing = spacing / mean_spacing


    end function normalized_spacing

    subroutine save_array(vector, fwrite)
        integer fwrite
        real(8) :: vector(:)
        integer ii 

        do ii=1, size(vector)
            write(fwrite, *) vector(ii)
        enddo

    end subroutine save_array

       
    




    




end module

program main
    use matrices
    implicit none
    type(dcmatrix) :: MH, MD, M_adj
    integer, dimension(2) :: dim
    double complex :: trc, trc_adj
    double complex, dimension(:,:), allocatable :: adj 
    real(8) precision
    integer ii
    
  

    debug=.TRUE.
    stop_exec= .TRUE.

    open(1, file = 'outfile/spacing_diag_50_2nd_2000.dat', status = 'unknown') 
    open(2, file = 'outfile/eigenvalues_hermitian_50_2nd_2000.dat', status = 'unknown') 
    open(3, file = 'outfile/spacing_hermitian_50_2nd_2000.dat', status = 'unknown') 
    open(4, file = 'outfile/eigenvalues_diagonal_50_2nd_2000.dat', status = 'unknown') 
    !open(5, file = 'outfile/spacing_diag_50.dat', status = 'unknown') 



    print *,' Enter the matrix dimensions' 
    read *, dim(1) , dim(2)

    print *, 'Enter precision: 10DEXP format'
    read *, precision

    
    

    do ii=1,50

        MH = .RHINIT.dim(1)
        MD = .RDINIT.dim(1)


        trc = .TRC.MH%mel

        adj = .ADJ.MH%mel
        trc_adj = .TRC.adj
        M_adj = mel_initialize(adj)
    
    
        !print *, 'Matrix'
        !call printing(M%mel, 1)
        !print *, 'Matrix trace'
        !write(*, fmt="(*(sf12.12xspf1.1'i':x))") trc
        !print *, 'Adjoint'
        !call  printing(adj, 1)
        !print *, 'Adjoint trace'
        !write(*, fmt="(*(sf12.12xspf1.1'i':x))") M_adj%trace
        
    
        call check( condition = .NOT.complex_are_eq(trc_adj, M_adj%trace, precision), &
                    msg_type='WARNING', &
                    msg = 'adjoint matrix trace is different from the conjugate of trace')


        call diagonalize(MH%mel, precision, 2, 3)
        call diagonalize(MD%mel, precision, 4, 1)

        deallocate(MH%mel)
        deallocate(MD%mel)


        print *, ii
    enddo

    close(1); close(2); close(3); close(4)


end program main
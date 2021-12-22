module split_operator
    use harmonic_oscillator
    contains

    function H_V(N, interval, omega, mass, tt, T) result(V)
        integer(8) :: N, ii
        real(8) :: interval(2), omega, mass, tt, T, V(N+1), dx, x

        dx = (interval(2)-interval(1)) / N

        do ii=1, N+1
            x = interval(1) + (ii-1) * dx
            V(ii) = 1d0/2 * mass * omega ** 2 * (x - tt/T)**2
            !print *, V(ii)
        enddo

    end function H_V

    function H_V_time_independent(N, interval, omega, mass, tt, T) result(V)
        integer(8) :: N, ii
        real(8) :: interval(2), omega, mass, tt, T, V(N+1), dx, x

        dx = (interval(2)-interval(1)) / N

        do ii=1, N+1
            x = interval(1) + (ii-1) * dx
            V(ii) = 1d0/2 * mass * omega ** 2 * (x )**2
        enddo

    end function H_V_time_independent

    function H_T(N, interval, mass) result(T)
        integer(8) :: N, ii 
        real(8) :: interval(2), mass, dx, dp, p, T(N+1)
       
        real(8)    :: L
        real(8)    :: M

        real(8)    :: PI
        real(8)    :: K

        PI = ACOS(-1.0d0)
        L = interval(2)-interval(1)
        K  = 2*PI / L
        dx = L / N
        dp = K / N

        do ii=1,INT(N/2)
            T(ii) = (1.0d0/(2.0d0*mass)) * (2.0d0*PI*(ii)/L)**2
        end do

        do ii=INT(N/2)+1,N+1
            T(ii) = (1.0d0/(2.0d0*mass)) * (2.0d0*PI*(ii-N-1)/L)**2
        end do


        
    end function H_T


    function FFT(psi) result(fft_psi)

        double complex :: psi(:), fft_psi(size(psi, 1))

        integer ::  N, FFTW_FORWARD, FFTW_MEASURE
        integer(8) :: plan

        parameter (FFTW_FORWARD=-1)
        parameter (FFTW_MEASURE=1)

         N = size(psi, 1)

        call dfftw_plan_dft_1d(plan,N,psi,fft_psi,FFTW_FORWARD,FFTW_MEASURE)
        call dfftw_execute_dft(plan, psi, fft_psi)

        fft_psi = fft_psi/sqrt(1d0 * N)

        call dfftw_destroy_plan(plan)


    end function FFT

    function IFFT(fft_psi) result(psi)

        double complex  :: fft_psi(:), psi(size(fft_psi, 1))

        integer N

        integer FFTW_BACKWARD
        parameter (FFTW_BACKWARD=1)

        integer FFTW_MEASURE
        parameter (FFTW_MEASURE=1)

        integer(8) plan

        N = size(fft_psi, 1)

        call dfftw_plan_dft_1d(plan, N, fft_psi, psi, FFTW_BACKWARD, FFTW_MEASURE)

        call dfftw_execute_dft(plan, fft_psi, psi)

        psi = psi / sqrt( 1d0 * N)

        call dfftw_destroy_plan(plan)

    end function IFFT

    function time_evolution(psi, interval, omega, mass, time, TT, dt) result(psi_evolved)
        
        double complex :: psi(:), psi_evolved(size(psi, 1))
        real(8) ::  interval(2), omega, mass, time, TT, dt, V(size(psi, 1)), T(size(psi, 1))
        integer(8) :: N, ii
        
        N = size(psi, 1)-1 
        
        !V = H_V(N, interval, omega, mass, time, TT)
        V = H_V_time_independent(N, interval, omega, mass, time, TT)
        T = H_T(N, interval, mass)

        do ii=1, N+1
            psi_evolved(ii)  =  EXP ( COMPLEX (0.0d0, - 1d0 / 2 * V(ii) * dt) )  * psi(ii)
        enddo
        psi_evolved = FFT(psi_evolved)

        do ii =1, N+1
            psi_evolved(ii) =  EXP ( COMPLEX (0.0d0, - 1d0 / 2 * T(ii) * dt) )  * psi_evolved(ii)
        enddo 

        psi_evolved = IFFT(psi_evolved)

        do ii=1, N+1
            psi_evolved(ii)  =  EXP ( COMPLEX (0.0d0, - 1d0 / 2 * V(ii) * dt) )  * psi_evolved(ii)
        enddo
        
    end function time_evolution

    function l2_norm(psi) result(norm)
        double complex :: psi(:)
        real(8) norm
        integer ii

        norm =0
        do ii=1, size(psi)
            norm = norm + real(conjg(psi(ii))*psi(ii))
        enddo

        norm = sqrt(norm)
    end function l2_norm





    subroutine print_wvfc_expv_norm(xs, ys, zs, filename, unit)
        ! input arguments
        real(8),    dimension(:)   :: xs
        real(8),    dimension(:)   :: ys
        complex(8), dimension(:,:) :: zs
        character(*) :: filename
        integer(4) :: unit

        real(8)    :: expv, flct
        integer(4) :: ii, jj

        ! write results on file to use for plotting
        open(unit, file=filename, status='replace')
        do ii=1,size(ys,1)
            ! compute expectation value
            expv = 0.0d0
            do jj=1, size(xs,1)
                expv = expv + xs(jj) * REAL(zs(jj,ii)*CONJG(zs(jj,ii))) * (xs(2)-xs(1))
            end do
            ! compute fluctuation
            flct = 0.0d0
            do jj=1,size(xs,1)
                flct = flct + (REAL(zs(jj,ii)*CONJG(zs(jj,ii))) * (xs(2)-xs(1))) * (xs(jj) - expv)**2
            end do
            flct = SQRT(flct)
            ! write expectation value and expectation value +/- sigma on file
            write(unit,'(g0ag0ag0ag0ag0ag0ag0ag0)') ys(ii), char(9), expv, char(9), expv-      flct, char(9), expv+      flct, char(9), &
                                                                                    expv-2.0d0*flct, char(9), expv+2.0d0*flct, char(9), &
                                                                                    expv-3.0d0*flct, char(9), expv+3.0d0*flct
        end do
        close(unit)
    end subroutine print_wvfc_expv_norm


    function pure_state(d, N, sep) result(psi)

        integer :: d, N, dim
        logical :: sep 
        double complex, allocatable :: psi(:)
        real(8), allocatable :: re(:), im(:)

        if (sep.EQV..TRUE.) then
            
            dim = d*N

        else 
            
            dim = d**N

        endif

        allocate(psi(dim))
        allocate(re(dim))
        allocate(im(dim))

        call random_number(re)
        call random_number(im)

        re =  2*re-1
        im = 2*im-1


        psi = (re* complex(1.0d0, 0.0d0) + im * complex(0.0d0, 1.0d0)) / COMPLEX( SQRT ( SUM(re**2  + im**2) ), 0.0d0 )
       

    end function

    function density_matrix(psi) result(rho)

        double complex :: psi(:)
        double complex, allocatable :: ket(:,:), bra(:,:), rho(:, :)
        integer N

        N = size(psi, 1)

        allocate(ket(N, 1)); allocate(bra(1, N)); allocate(rho(N, N))

        bra(1, :)= conjg(psi)
        ket(:, 1) = psi 

        rho = MATMUL(ket, bra)

    end function

function versor(N, ii) result(vect)
    integer :: N, ii
    real(8) :: vect(N)

    vect = 0
    vect(ii) = 1
end function

function ID_matrix (N) result(ID)
    integer :: N, ii 
    real(8) :: ID(N,N)



    ID = 0
    do ii=1, N 
        ID(ii, ii) =1
    enddo

end function

subroutine kronecker_product(mat_A, mat_B, mat) 
    real(8) ::  mat_A(:,:), mat_B(:,:), mat(:,:)
    integer ii, jj, M1, N1, M2, N2

    M1 = size(mat_A, 1)
    N1 = size(mat_A, 2)
    M2 = size(mat_B, 1)
    N2 = size(mat_B, 2)

    do ii=1, M1
        do jj=1, N1
            mat( 1 + (ii-1) * M2 : 1+(ii)* M2 , 1 + (jj-1) * N2 : 1+(jj)* N2 ) = mat_A(ii, jj) * mat_B
            
        enddo
    enddo
 end subroutine

 subroutine kronecker_product_cmplx(mat_A, mat_B, mat) 
    double complex ::  mat_A(:,:), mat_B(:,:), mat(:,:)
    integer ii, jj, M1, N1, M2, N2

    M1 = size(mat_A, 1)
    N1 = size(mat_A, 2)
    M2 = size(mat_B, 1)
    N2 = size(mat_B, 2)

    do ii=1, M1
        do jj=1, N1
            mat( 1 + (ii-1) * M2 : 1+(ii)* M2 , 1 + (jj-1) * N2 : 1+(jj)* N2 ) = mat_A(ii, jj) * mat_B
            
        enddo
    enddo
 end subroutine


function reduced_density_matrix(rho, system, dim_A, dim_B) result(reduced_rho)

    double complex :: rho(:, :)
    double complex, allocatable :: reduced_rho(:,:)
    character(1) :: system
    integer ii,  jj, dim_A, dim_B
    real(8), allocatable :: id(:,:), ket(:,:), bra(:,:), bra_id(:,:), ket_id(:,:)

    if (system=='A') then
        allocate( reduced_rho(dim_B, dim_B) )
        allocate(ket(dim_A, 1))
        allocate(bra(1, dim_A))
        
        reduced_rho = 0 
        id = ID_matrix(dim_B)
        
        
        do ii=1, dim_A 
            ket(:, 1) = versor(dim_A, ii)
            bra(1, :) = versor(dim_A, ii)

            allocate(bra_id(size(bra, 1)*size(id, 1), size(bra, 2)*size(id, 2)))
            allocate(ket_id(size(ket, 1)*size(id, 1), size(ket, 2)*size(id, 2)))

            call kronecker_product(bra, id, bra_id)
            call kronecker_product(ket, id, ket_id)
            
            reduced_rho = reduced_rho + MATMUL (MATMUL(bra_id, rho), ket_id)

            deallocate(bra_id) ; deallocate(ket_id)
            
        enddo

        elseif (system=='B') then
            allocate( reduced_rho(dim_A, dim_A) )
            allocate(ket(dim_B, 1))
            allocate(bra(1, dim_B))
            
    
            reduced_rho = 0 
            id = ID_matrix(dim_A)
            
            
            do ii=1, dim_B
                ket(:, 1) = versor(dim_B, ii)
                bra(1, :) = versor(dim_B, ii)
    
                allocate(bra_id(size(bra, 1)*size(id, 1), size(bra, 2)*size(id, 2)))
                allocate(ket_id(size(ket, 1)*size(id, 1), size(ket, 2)*size(id, 2)))
    
                !print *, shape(bra_id)
                !print *, shape(ket_id)
    
                call kronecker_product( id, bra, bra_id)
                
                call kronecker_product(id, ket,  ket_id)
                
                reduced_rho = reduced_rho + MATMUL (MATMUL(bra_id, rho), ket_id)
    
                deallocate(bra_id) ; deallocate(ket_id)
                
            enddo
           
           

    endif


end function 



function dmat_init_rand_state(N, D, isSep) result(state)
    ! input arguments
    integer(4) :: N
    integer(4) :: D
    logical    :: isSep

    integer(4) :: dim
    complex(8), dimension(:), allocatable :: state
    real(8),    dimension(:), allocatable :: re, im
    real(8)    :: norm

    if (isSep) then
        dim = D*N
    else
        dim = D**N
    end if

    allocate(state(dim))
    allocate(re(dim))
    allocate(im(dim))

    ! fill real and imaginary part vectors
    call RANDOM_NUMBER(re)
    call RANDOM_NUMBER(im)
    re = re*2.0d0 - 1.0d0
    im = im*2.0d0 - 1.0d0

    ! fill state vector with coefficients and normalize
    norm  = SUM(re**2 + im**2)
    state = (re*COMPLEX(1.0d0,0.0d0) + im*COMPLEX(0.0d0,1.0d0)) / COMPLEX(SQRT(norm),0.0d0)

    ! do ii=1,dim
    !     call RANDOM_NUMBER(re)
    !     call RANDOM_NUMBER(im)
    !     state(ii) = COMPLEX(re*2.0d0-1.0d0, im*2.0d0-1.0d0)
    ! end do
end function dmat_init_rand_state

end module
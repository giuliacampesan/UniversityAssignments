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
        
        V = H_V_time_independent(N, interval, omega, mass, time, TT)
        !V = H_V(N, interval, omega, mass, time, TT)
        T = H_T(N, interval, mass)

        do ii=1, N+1
            psi_evolved(ii)  =  ZEXP ( COMPLEX (0.0d0, - 1d0 / 2 * V(ii) * dt) )  * psi(ii)
        enddo

        psi_evolved = FFT(psi_evolved)

        do ii =1, N+1
            psi_evolved(ii) =  ZEXP ( COMPLEX (0.0d0, - 1d0 * T(ii) * dt) )  * psi_evolved(ii)
        enddo 

        psi_evolved = IFFT(psi_evolved)

        do ii=1, N+1
            psi_evolved(ii)  =  ZEXP ( COMPLEX (0.0d0, - 1d0 / 2 * V(ii) * dt) )  * psi_evolved(ii)
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
        real(8),    dimension(:)   :: xs
        real(8),    dimension(:)   :: ys
        complex(8), dimension(:,:) :: zs
        character(*) :: filename
        integer(4) :: unit

        real(8)    :: expv, flct
        integer(4) :: ii, jj

        open(unit, file=filename, status='replace')
        do ii=1,size(ys,1)
            expv = 0.0d0
            do jj=1, size(xs,1)
                expv = expv + xs(jj) * REAL(zs(jj,ii)*CONJG(zs(jj,ii))) * (xs(2)-xs(1))
            end do
            flct = 0.0d0
            do jj=1,size(xs,1)
                flct = flct + (REAL(zs(jj,ii)*CONJG(zs(jj,ii))) * (xs(2)-xs(1))) * (xs(jj) - expv)**2
            end do
            flct = SQRT(flct)
            write(unit,'(g0ag0ag0ag0ag0ag0ag0ag0)') ys(ii), char(9), expv, char(9), expv-      flct, char(9), expv+      flct, char(9), &
                                                                                    expv-2.0d0*flct, char(9), expv+2.0d0*flct, char(9), &
                                                                                    expv-3.0d0*flct, char(9), expv+3.0d0*flct
        end do
        close(unit)
    end subroutine print_wvfc_expv_norm

end module
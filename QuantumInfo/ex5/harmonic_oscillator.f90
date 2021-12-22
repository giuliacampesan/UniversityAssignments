module harmonic_oscillator
    use debugging 
    implicit none
    contains
    function hamiltonian(N, interval, omega) result(h)

        real(8), allocatable :: h(:,:)
        real(8) :: omega
        integer N
        real(8) :: interval(2)
        real(8) :: dx, x
        integer :: ii


        dx = (interval(2)-interval(1)) / N
        

        allocate( h( N+1, N+1 ) )

        h = 0 
        
        do ii=1, N+1
            x = interval(1) + (ii-1) * dx
            h(ii, ii) = 1d0 / (1 * dx**2 ) + 1d0 / 2 * omega**2 * x**2
        enddo

        do ii=1, N 
            h(ii, ii+1) = - 1d0 / (2 * dx**2 )
            h(ii+1, ii) = h(ii, ii+1)
        enddo
        
    end function

    function ham_init(N, interval, mass,omega, t0, tT) result(ham)
        
        integer(4) :: N     
        real(8) :: interval(2)
        real(8)    :: mass    
        real(8)    :: omega   
        real(8)    :: t0   
        real(8)    :: tT     

        real(8), dimension(N+1,N+1) :: ham
        real(8) :: DX, L1, L2
        integer(4) :: ii, jj

        dx = (interval(2)-interval(1)) / N
        L1 = interval(1)
        do ii=1,N+1
            do jj=1,N+1
                if (ABS(ii-jj)==1) then
                    ham(ii,jj) = - (1.0d0/(2.0d0*mass)) * (1.0d0/(DX**2))
                else if (ii==jj) then
                    ham(ii,jj) =   (1.0d0/(2.0d0*mass)) * (2.0d0/(DX**2)) + (0.5d0*mass*omega**2) * (L1 + DX*(ii-1) - t0/tT)**2
                else
                    ham(ii,jj) =    0.0d0
                end if
            end do
        end do
    end function ham_init

    

    subroutine diagonalize_hamiltonian(h, eigv)
        real(8) :: h(:,:), eigv(:)
        integer N, LDZ, INFO
        real(8),  allocatable ::  UD(:), WORK(:)  
        integer ii
        

        N = size(h, 1)
        LDZ = MAX(1, N)
        allocate(UD(N-1)); allocate(WORK(MAX(1, 2*N-2)))
        
        do ii=1, N 
            eigv(ii) = h(ii, ii)
        enddo 

        do ii=1, N-1
            UD(ii) = h(ii, ii+1)
        enddo


        call DSTEV( 'V', N, eigv, UD, h, LDZ, WORK, INFO )


        call check( condition= (INFO.NE.0), &
                    msg_type = 'ERROR', &
                    msg = 'something went wrong in dstev')

        deallocate(UD); deallocate(WORK)

    end subroutine

    



    function relative_error(observed, theoretical) result(error)
        real(8) :: observed, theoretical, error 

        error = (observed-theoretical) / theoretical

    end function



    end module harmonic_oscillator


    program main
        use harmonic_oscillator
        use split_operator
        integer(4) :: Nx
        integer narg
        character(100) :: ch_Nx, ch_L1, ch_L2, ch_Nt, ch_t0, ch_tf, ch_tT, ch_mass, ch_omega, filename
        real(8) :: interval(2)
        real(8) :: omega, mass
        real(8), allocatable :: H(:,:), eigv(:)
        integer(4)::  ii, jj
        real(8) :: dx, L2, L1
        real(8) :: t0, tT, dt, tf
        integer :: Nt
        real(8), allocatable :: times(:), position(:)
        double complex, allocatable  :: psi(:)
        double complex, allocatable :: psi_evolved(:)
        integer :: unit 
        double complex, allocatable :: psi_all(:,:)
        real(8) :: norm
        
        



        narg= COMMAND_ARGUMENT_COUNT()
        

        if (narg.EQ.0) then 
            print *, 'setting parameters to default values: Nx=2000, L1=-10, L2=10, Nt=2000, t0=0, tf=7, T=1, m=1, w=1'
            print * ,'if you want to insert your own parameters re-run with command-line arguments: Nx L1 L2 Nt t0 tf T mass omega'

            Nx = 2000
            write (ch_Nx,*) Nx

            L1 = -10
            write (ch_L1,*) L1

            L2 = 10
            write (ch_L2,*) L2
            

            Nt = 2000
            write (ch_Nt,*) Nt

            t0=0
            write (ch_t0,*) t0
            
            tT=1
            write (ch_tT,*) tT 
            
            tf = 10
            write (ch_tf,*) tf
           
            mass = 1
            write (ch_mass,*) mass

            omega = 1
            write (ch_omega,*) omega

            

        else 
            if (narg.LT.9) then 
                print *, 'invalid number of arguments'
            
            else 

                CALL GET_COMMAND_ARGUMENT(narg-(narg-1), ch_Nx) 
                READ(ch_Nx,*) Nx

                CALL GET_COMMAND_ARGUMENT(narg-(narg-2),ch_L1) 
                READ(ch_L1,*) L1

                CALL GET_COMMAND_ARGUMENT(narg-(narg-3),ch_L2) 
                READ(ch_L2,*) L2
                
                CALL GET_COMMAND_ARGUMENT(narg-(narg-4),ch_Nt) 
                READ(ch_Nt,*) Nt

                CALL GET_COMMAND_ARGUMENT(narg-(narg-5),ch_t0) 
                READ(ch_t0,*) t0

                CALL GET_COMMAND_ARGUMENT(narg-(narg-6),ch_tf) 
                READ(ch_tf,*) tf 

                CALL GET_COMMAND_ARGUMENT(narg-(narg-7),ch_tT) 
                READ(ch_tT,*) tT 

                CALL GET_COMMAND_ARGUMENT(narg-(narg-8),ch_mass) 
                READ(ch_mass,*) mass

                CALL GET_COMMAND_ARGUMENT(narg-(narg-9),ch_omega) 
                READ(ch_omega,*) omega

            endif

        endif


        dx =  (L2-L1) / Nx
        interval(1) = L1
        interval(2) = L2

        

        allocate(times(Nt+1))
        allocate(position(Nx+1))
        dt = (tf- t0) / Nt


        do ii=1, Nt+1
            times(ii) = t0 + (ii-1) * dt
        enddo

        do ii=1, Nx+1
            position(ii) = L1 + (ii-1) * dx 
        enddo
        

        !H = hamiltonian(Nx, interval, omega)
        H = ham_init(Nx, interval, mass,omega, t0, tT)
        allocate(eigv(Nx+1))
        allocate(psi(Nx+1))
        allocate(psi_evolved(Nx+1))
        allocate(psi_all(Nx+1, Nt+1))
        
        
        call diagonalize_hamiltonian(H, eigv)

        do ii=1, size(H,1)
            psi(ii) = complex(H(ii,1), 0.0d0)
        enddo

        !normalize eigenfunction
        do ii=1, size(psi, 1)
            psi(ii) = psi(ii) * SQRT( Nx / (L2 - L1) )
        enddo
        

        filename = '../outfile/norm_'//TRIM(ch_Nx)//'_'//TRIM(ch_L1)//'_'//TRIM(ch_L2)//'_'//TRIM(ch_Nt)//'_'//TRIM(ch_t0)//'_'//TRIM(ch_tf)//'_'//TRIM(ch_tT)//'_'//TRIM(ch_mass)//'_'//TRIM(ch_omega)//'.dat'
        unit=11
        open(unit, file=filename, status='unknown')

        do ii=1, Nt+1


            
           

            psi_evolved = time_evolution(psi, interval, omega, mass, times(ii), tT, dt)
            

            psi_all(:, ii) = psi

            !norm = l2_norm(psi_evolved)
            !write(11, *) norm**2 * dx

            psi = psi_evolved !/ (norm * dx)

            
            
        enddo


        filename = '../outfile/x_'//TRIM(ch_Nx)//'_'//TRIM(ch_L1)//'_'//TRIM(ch_L2)//'_'//TRIM(ch_Nt)//'_'//TRIM(ch_t0)//'_'//TRIM(ch_tf)//'_'//TRIM(ch_tT)//'_'//TRIM(ch_mass)//'_'//TRIM(ch_omega)//'.dat'
        unit=12
        open(unit, file=filename, status='unknown')
        do ii=1, Nx+1
            write(12, *) position(ii)
        enddo

        filename = '../outfile/y_'//TRIM(ch_Nx)//'_'//TRIM(ch_L1)//'_'//TRIM(ch_L2)//'_'//TRIM(ch_Nt)//'_'//TRIM(ch_t0)//'_'//TRIM(ch_tf)//'_'//TRIM(ch_tT)//'_'//TRIM(ch_mass)//'_'//TRIM(ch_omega)//'.dat'
        unit=13
        open(unit, file=filename, status='unknown')
        do ii=1, Nt+1
            write(13, *) times(ii)
        enddo

        filename = '../outfile/z_'//TRIM(ch_Nx)//'_'//TRIM(ch_L1)//'_'//TRIM(ch_L2)//'_'//TRIM(ch_Nt)//'_'//TRIM(ch_t0)//'_'//TRIM(ch_tf)//'_'//TRIM(ch_tT)//'_'//TRIM(ch_mass)//'_'//TRIM(ch_omega)//'.dat'
        unit = 14
        open(unit, file=filename, status='unknown')
        do ii = 1, size(psi_all,2)
            write(14,*) REAL(psi_all(:,ii)*CONJG(psi_all(:,ii)))
        end do
        
        filename = '../outfile/expv_'//TRIM(ch_Nx)//'_'//TRIM(ch_L1)//'_'//TRIM(ch_L2)//'_'//TRIM(ch_Nt)//'_'//TRIM(ch_t0)//'_'//TRIM(ch_tf)//'_'//TRIM(ch_tT)//'_'//TRIM(ch_mass)//'_'//TRIM(ch_omega)//'.dat'
        unit=15
        call print_wvfc_expv_norm(position, times, psi_all, filename, unit)

        close(12); close(13); close(14); close(15)
        
        print *, "["//achar(27)//"[1;32m"//'JOB DONE'//achar(27)//"[0m]"


end program main









       




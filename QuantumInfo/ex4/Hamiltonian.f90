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
        print *, 'H is allocated', allocated(h)

        h = 0 
        
        do ii=1, N+1
            x = interval(1) + (ii-1) * dx
            !print *, 'position', x
            h(ii, ii) = 1d0 / (1 * dx**2 ) + 1d0 / 2 * omega**2 * x**2
        enddo

        do ii=1, N 
            h(ii, ii+1) = - 1d0 / (2 * dx**2 )
            h(ii+1, ii) = h(ii, ii+1)
        enddo
        
    end function

    

    subroutine diagonalize_hamiltonian(h, values_file, vectrs_file, errors_file)
        real(8) :: h(:,:)
        integer N, LDZ, INFO
        real(8),  allocatable :: eigvalues(:), UD(:), WORK(:), eigvectors(:,:)
        integer values_file, vectrs_file, errors_file
        integer ii
        real(8), dimension(size(eigvalues)):: rel_error
        

        N = size(h, 1)
        LDZ = MAX(1, N)
        allocate(eigvalues(N)); allocate(UD(N-1)); allocate(WORK(MAX(1, 2*N-2))); allocate(eigvectors(LDZ, N))
        
        do ii=1, N 
            eigvalues(ii) = h(ii, ii)
        enddo 

        do ii=1, N-1
            UD(ii) = h(ii, ii+1)
        enddo

        !print *, 'fin qua tutto ok'

        call DSTEV( 'V', N, eigvalues, UD, eigvectors, LDZ, WORK, INFO )

        !print *, 'la dstev è ok'

        call check( condition= (INFO.NE.0), &
                    msg_type = 'ERROR', &
                    msg = 'something went wrong in dstev')

        do ii=1, N
            rel_error(ii)= relative_error(eigvalues(ii),1d0 / 2 + (ii-1))
            write(values_file, *) eigvalues(ii)
            write(vectrs_file, *) eigvectors(:, ii)
            write(errors_file, *) rel_error(ii)

        enddo

        
        print *, 'eigenvalues', eigvalues(1), eigvalues(2), eigvalues(3)
        

        deallocate(eigvalues); deallocate(UD); deallocate(WORK); deallocate(eigvectors)
        !print *, 'hai deallocato'

    end subroutine

    function relative_error(observed, theoretical) result(error)
        real(8) :: observed, theoretical, error 

        error = (observed-theoretical) / theoretical

    end function



    end module harmonic_oscillator

    program main
        use harmonic_oscillator
        integer(4) :: N 
        integer narg
        character(100) :: ch_dx, ch_L1, ch_L2, filename
        real(8) :: interval(2)
        real(8) :: omega
        real(8), allocatable :: H(:,:)
        integer(4)::  ii
        real(8) :: dx, L2, L1



        narg= COMMAND_ARGUMENT_COUNT()

        CALL GET_COMMAND_ARGUMENT(narg,ch_dx) 
        READ(ch_dx,*)dx 

        CALL GET_COMMAND_ARGUMENT(narg-1,ch_L2) 
        READ(ch_L2,*)L2

        CALL GET_COMMAND_ARGUMENT(narg-2,ch_L1) 
        READ(ch_L1,*)L1

        N = nint( (L2-L1) / dx )
        print *, N
        interval(1) = L1
        interval(2) = L2
        omega = 1
        
        open(1, file = 'outfile/eigvalues_spacing_'//TRIM(ch_dx)//'_lbound'//TRIM(ch_L1)//'.dat', status = 'unknown') 
        open(2, file = 'outfile/eigenvectors_spacing'//TRIM(ch_dx)//'_lbound'//TRIM(ch_L1)//'.dat', status = 'unknown')
        open(3, file = 'outfile/errors_spacing'//TRIM(ch_dx)//'_lbound'//TRIM(ch_L1)//'.dat', status = 'unknown')

       

        H = hamiltonian(N, interval, omega)
        
        !do ii=1, size(H, 1)
         !   print *, H(ii, :)
        !enddo 
        call diagonalize_hamiltonian(H, 1, 2, 3)

        close(1); close(2); close(3)

end program main




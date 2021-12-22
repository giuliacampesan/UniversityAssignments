program main
    use harmonic_oscillator
    use split_operator
    use matrices

    integer :: N, d, ii, unit, narg, step
    logical :: separable
    double complex,  allocatable :: state(:), rho_redB(:,:), rho_redA(:,:), rho(:,:)
    real(8) :: t_i, t_f
    character(100) :: filename, N_ch, d_ch, is_separable, separable_ch, step_ch
    
    narg= COMMAND_ARGUMENT_COUNT()

    CALL GET_COMMAND_ARGUMENT(narg-(narg-1), N_ch) 
    READ(N_ch,*) N

    CALL GET_COMMAND_ARGUMENT(narg-(narg-2), d_ch) 
    READ(d_ch,*) d

    CALL GET_COMMAND_ARGUMENT(narg-(narg-3), separable_ch) 
    READ(separable_ch,*) separable

    CALL GET_COMMAND_ARGUMENT(narg-(narg-4), step_ch) 
    READ(step_ch,*) step



    if (separable) then
        is_separable='separable'
    else 
        is_separable = 'not_separable'
    endif


    unit=11
    filename = '../outfile/time_d_'//TRIM(d_ch)//'_N_'//TRIM(N_ch)//'_'//is_separable//'.dat'
    open(unit, file=filename, action='write', position='append')
    open(unit, file=filename, status='unknown')

    do ii=1, N, step

        call CPU_TIME(t_i)

        state = pure_state(d, ii,  separable)
        
        call CPU_TIME(t_f)


        write(unit, *) ii,  t_f-ti 

    enddo

    
    
    close(unit)

    state = pure_state(d, N,  separable)
    print *, 'state coefficients'
    call printing_vctr(state)

    rho = density_matrix(state)

    print *, 'density matrix'
    call printing(rho)

    rho_redA = reduced_density_matrix(rho, 'B', d, d)
    rho_redB = reduced_density_matrix(rho, 'A', d, d)

    print *, 'reduced density matrix A-tracing out B'
    call printing(rho_redA)

    print *, 'reduced density matrix B-tracing out A'
    call printing(rho_redB)

    


end program main
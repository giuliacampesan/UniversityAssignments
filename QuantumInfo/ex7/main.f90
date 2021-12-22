program main

    use ising
    use matrices
    
    integer :: N,  narg
    real(8) :: lambda
    double complex, allocatable :: H(:,:)
    real(8), allocatable :: eigv(:)
    character(100) :: N_ch, lambda_ch, k_ch


    narg= COMMAND_ARGUMENT_COUNT()

    CALL GET_COMMAND_ARGUMENT(narg-(narg-1), N_ch) 
    READ(N_ch,*) N

    CALL GET_COMMAND_ARGUMENT(narg-(narg-2), lambda_ch) 
    READ(lambda_ch,*) lambda

    CALL GET_COMMAND_ARGUMENT(narg-(narg-3), k_ch) 
    READ(k_ch,*) k
    

    H = ising_ham(N, lambda)

    allocate( eigv (size(H, 1) ) )

    call diagonalize_hamiltonian(H, eigv)

    print *, eigv(:k)

end program
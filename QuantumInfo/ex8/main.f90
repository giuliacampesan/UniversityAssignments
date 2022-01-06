program main

    use ising
    use matrices
    
    integer :: N,  narg, N_iterations, ii
    real(8) :: lambda
    double complex, allocatable :: H(:,:), H_L(:,:), H_R(:,:), eigvctrs(:,:)
    real(8), allocatable :: eigvls(:)
    character(100) :: N_ch, lambda_ch, N_iterations_ch


    narg= COMMAND_ARGUMENT_COUNT()

    CALL GET_COMMAND_ARGUMENT(narg-(narg-1), N_ch) 
    READ(N_ch,*) N

    CALL GET_COMMAND_ARGUMENT(narg-(narg-2), lambda_ch) 
    READ(lambda_ch,*) lambda

    CALL GET_COMMAND_ARGUMENT(narg-(narg-3), N_iterations_ch) 
    READ(N_iterations_ch,*) N_iterations
    

    H = ising_ham(N, lambda)
    H_L = kronecker_product_cmplx(ising_identity(N-1), s_x())
    H_R = kronecker_product_cmplx(s_x(), ising_identity(N-1))

    do ii=1, N_iterations
        

        call RSRG_iteration(H, H_L, H_R, N)
        H = H *0.5d0
        H_L = H_L  * sqrt(0.5d0)
        H_R = H_R * sqrt(0.5d0)

    enddo

    allocate(eigvctrs(2**N, 2**N)); allocate(eigvls(2**N))

    call ising_diagonalize_hamiltonian(H, eigvctrs, eigvls)
    print *, eigvls(1) / (N )


end program
module ising 

    use debugging
    use matrices
    implicit none
    contains

    function ID_ising (N) result(ID)
        integer :: N, ii 
        double complex :: ID(2**N, 2**N)
    
    
    
        ID = complex(0.0d0, 0.0d0)
        do ii=1, size(ID, 1)
            ID(ii, ii) = complex(1.0d0, 0.0d0)
        enddo
        
    end function


    function s_x() result(s)
        double complex :: s(2,2)
        
        s = cmplx(0.0d0, 0.0d0)

        s(1,2) = COMPLEX( 1.0d0, 0.0d0)
        s(2,1) = COMPLEX( 1.0d0, 0.0d0)

    end function 


  function s_z() result(s)

      double complex :: s(2,2)
      
      s = cmplx(0.0d0, 0.0d0)

      s(1,1) = COMPLEX( 1.0d0, 0.0d0)
      s(2,2) = COMPLEX( -1.0d0, 0.0d0)

  end function

        function kronecker_product_cmplx(mat_A, mat_B)  result(mat)
            double complex ::  mat_A(:,:), mat_B(:,:)
            double complex, allocatable :: mat(:,:)
            integer ii, jj, M1, N1, M2, N2
        
            M1 = size(mat_A, 1)
            N1 = size(mat_A, 2)
            M2 = size(mat_B, 1)
            N2 = size(mat_B, 2)

            allocate( mat( M1*M2, N1*N2 ) )
        
            do ii=1, M1
                do jj=1, N1
                    mat( 1 + (ii-1) * M2 : 1+(ii)* M2 , 1 + (jj-1) * N2 : 1+(jj)* N2 ) = mat_A(ii, jj) * mat_B
                    
                enddo
            enddo

        end function


         function ising_ham (N, lambda) result(H)

            integer :: N, ii
            real(8) :: lambda
            double complex :: H(2**N, 2**N)

            do ii=1, N
                H = H + kronecker_product_cmplx(    &
                        kronecker_product_cmplx(    & 
                        ID_ising( ii-1 ), s_z() ),  &
                        ID_ising( N-ii ) )
            enddo

            H = lambda * H

            do ii=1, N 

                H = H +     kronecker_product_cmplx(                        &
                            kronecker_product_cmplx( ID_ising( ii-1 ),      &
                            kronecker_product_cmplx( s_x(), s_x() ) )  ,    &
                            ID_ising( N -ii -1 ) ) 
            enddo


         end function

         subroutine ising_diagonalize_hamiltonian(h, eigvctrs, eigvls)
            double complex :: h(:,:), eigvctrs(:,:)
            real(8) :: eigvls(:)
            character :: jobz, uplo
            integer N, LDA, LWORK, INFO 
            complex(8) :: dummy(1)
            complex(8),  allocatable ::  WORK(:)  
            complex(8), allocatable :: rwork(:)
            integer(4), parameter :: nb = 64
    
            N = size(h, 1)
            jobz = 'N'
            uplo = 'U'
            LDA = MAX(1, N)

            allocate (rwork(max(1, 3*N-2)))

            eigvctrs = h
            
            lwork=-1
            call zheev('V', 'U', N, eigvctrs, N, eigvls, dummy, lwork, rwork, info)
            if (info == 0) then
               lwork = max((nb+1)*N, nint(real(dummy(1))))
            endif
        
            allocate (work(lwork))
            call zheev('V', 'U', N, eigvctrs, N, eigvls, work, lwork, rwork, info)

    
    
    
            call check(condition=(info.GT.0), &
                msg_type='ERROR', &
                msg ='the algorithm failed to converge')

            call check(condition=(info.LT.0), &
                msg_type='ERROR', &
                msg ='a parameter had an illegal value')
            deallocate(WORK)
    
        end subroutine


        subroutine RSRG_iteration(H_N, H_L, H_R, N)

            double complex :: H_N(:,:), H_L(:,:), H_R(:,:)
            integer        :: N 
            double complex :: H_2N(2**(2*N), 2**(2*N))
            double complex :: PJ(2**(2*N), 2**N)
            double complex :: PJ_ADJ(2**N, 2**(2*N))
            double complex :: H_L_int(2**(2*N), 2**(2*N)), H_R_int(2**(2*N), 2**(2*N))
            double complex :: eigvctrs(2**(2*N), 2**(2*N))
            real(8)        :: eigvls(2**(2*N))
            

            H_2N =  kronecker_product_cmplx( H_N, ising_identity(N) ) +&
                    kronecker_product_cmplx( ising_identity(N), H_N ) +&
                    kronecker_product_cmplx( H_L, H_R )


            call ising_diagonalize_hamiltonian(H_2N, eigvctrs, eigvls)

            PJ = eigvctrs(:, :2**N)
            PJ_ADJ = ADJOINT( PJ )

            H_N = MATMUL ( MATMUL( PJ_ADJ, H_2N ), PJ)

            H_L_int = kronecker_product_cmplx( ising_identity(N), H_L )
            H_R_int = kronecker_product_cmplx( H_R , ising_identity(N) )

            H_L = MATMUL( MATMUL( PJ_ADJ , H_L_int), PJ)
            H_R = MATMUL( MATMUL( PJ_ADJ , H_R_int), PJ)

        end subroutine




        ! function to create an identity matrix for N particles
        function ising_identity(N) result(id)
            ! input arguments
            integer(4) :: N

            ! output
            complex(8), dimension(2**N,2**N) :: id

            integer(4) :: ii

            id = COMPLEX(0.0d0, 0.0d0)
            do ii=1,size(id,1)
                id(ii,ii) = COMPLEX(1.0d0,0.0d0)
            end do
        end function


       

end module 
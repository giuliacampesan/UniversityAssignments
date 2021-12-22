module ising 

    use debugging
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

         subroutine diagonalize_hamiltonian(h, eigv)
            double complex :: h(:,:)
            real(8) ::  eigv(:)
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
            
            lwork=-1
            call zheev('N', 'U', N, h, N, eigv, dummy, lwork, rwork, info)
            if (info == 0) then
               lwork = max((nb+1)*N, nint(real(dummy(1))))
            endif
        
            allocate (work(lwork))
            call zheev('N', 'U', N, h, N, eigv, work, lwork, rwork, info)

    
    
    
            call check(condition=(info.GT.0), &
                msg_type='ERROR', &
                msg ='the algorithm failed to converge')

            call check(condition=(info.LT.0), &
                msg_type='ERROR', &
                msg ='a parameter had an illegal value')
            deallocate(WORK)
    
        end subroutine


        function ising_tensor_prod(mat1, mat2) result(res)
            ! input arguments
            complex(8), dimension(:,:) :: mat1
            complex(8), dimension(:,:) :: mat2

            ! output
            complex(8), dimension(:,:), allocatable :: res

            integer(4) :: N1, N2, M1, M2
            integer(4) :: ii, jj, lli, llj, uui, uuj

            N1 = size(mat1,1)
            N2 = size(mat2,1)
            M1 = size(mat1,2)
            M2 = size(mat2,2)

            allocate(res(N1*N2,M1*M2))

            ! loop to execute tensor product between mat1 and mat2
            do ii=1,N1
                do jj=1,M1
                    lli = (ii-1)*N2 + 1
                    llj = (jj-1)*M2 + 1
                    uui =  ii   *N2
                    uuj =  jj   *M2
                    res(lli:uui,llj:uuj) = mat1(ii,jj)*mat2
                end do
            end do
        end function ising_tensor_prod


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
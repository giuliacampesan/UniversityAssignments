
!this is a module that includes procedures for debugging
!'check' is a general subroutine that, whether the 'debug' flag is true,
!when a certain condition is satisfied, it prints either a 'WARNING' or an 'ERROR' message
!if an error is encountered, and the 'stop_exec' flag is on,
!it stops the execution of the program 
!its input variables are:
!condition: logical value that controls when to raise an error or warning
!msg_type (optional): standards one are 'ERROR' or 'WARNING'
!msg (optional): message to be printed on the standard output
!input (optional): any input value to be printed on the standard output
module debugging
    implicit none

    logical :: debug        !debug flags
    logical :: stop_exec    !stop execution flags

    contains


    subroutine check(condition, msg_type, msg, input)

    implicit none

    logical condition
    character(*) :: msg_type
    character(*), optional ::  msg
    class(*), optional :: input


    if (condition) then 
        if(debug.EQV..TRUE.) then
            if (msg_type=='ERROR') then
                if (present(msg)) then
                    print *, "["//achar(27)//"[1;31m"//msg_type//achar(27)//"[0m]"//msg
                else 
                    print "["//achar(27)//"[1;31m"//msg_type//achar(27)//"[0m]"
                endif

            else if (msg_type=='WARNING') then
                if (present(msg)) then
                    print *, "["//achar(27)//"[1;33m"//msg_type//achar(27)//"[0m]"//msg
                else 
                    print "["//achar(27)//"[1;33m"//msg_type//achar(27)//"[0m]"
                endif

            else
                print *, msg
            endif

            if (present(input)) then
                select type(input)
                    type is (logical)
                        print *, input, "logical variable"
                    type is (integer(2))
                        print *, input, "integer(2) variable"
                    type is (integer(4))
                        print *, input, "integer(2) variable"
                    type is (real(4))
                        print *, input, "real*4 variable"
                    type is (real(8))
                        print *, input, "real*8 variable"
                    type is (complex)
                        print *, input, "complex variable"  
                    type is (character(*))             
                        print *, input, "array of character" 
                end select
            endif
        endif

        if (stop_exec.AND.(msg_type.EQ.'ERROR')) then
            print *, 'stopping execution'
            stop
        endif

    endif

end subroutine check

!checks if two real numbers are equal up to a certain precision
function real_are_eq(el1, el2, precision) result(equal)

    real*8 :: el1, el2 
    real(8) precision
    logical :: equal


    if (abs(el1-el2).LT.precision) then
        equal = .TRUE.
    else 
        equal = .FALSE.
    endif

end function

!checks if two complex numbers are equal up to a certain precision
function complex_are_eq(el1, el2, precision) result(equal)

    double complex :: el1, el2 
    real(8) precision
    logical :: equal


    if (abs(el1-el2).LT.precision) then
        equal = .TRUE.
    else 
        equal = .FALSE.
    endif
    equal =.TRUE.

end function

!checks if the elements of two matrices are equal up to a certain precision
subroutine matrix_eq(mat1, mat2, precision) 

    real*8, dimension(:,:) :: mat1, mat2
    real*8 :: precision
    logical :: equal
    integer nrows1, nrows2, ncols1, ncols2
    integer ii, jj

    nrows1 = size(mat1, 1); ncols1 = size(mat2, 1)
    nrows2 = size(mat1, 2); ncols2 = size(mat2, 2)

   
    call check( condition=((nrows1.NE.nrows2).OR.(ncols1.NE.ncols2)), &
                msg_type = 'ERROR', &
                msg='incompatible shapes')

    do ii=1, nrows1
        do jj=1, nrows2
            equal = real_are_eq(mat1(ii, jj), mat2(ii, jj), precision)
            call check( condition= (equal.EQV..FALSE.), &
                        msg_type = 'WARNING', &
                        msg='elements are different above precision')
        enddo
    enddo
end subroutine



end module



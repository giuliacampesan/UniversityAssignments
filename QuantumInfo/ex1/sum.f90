program sum

    implicit none
    
    integer*2 small2byte
    integer* 2 big2byte
    integer*2 sum2byte
    integer*4 small4byte
    integer*4 big4byte
    integer*4 sum4byte
    real*4 asingle; real*4 bsingle; real*4 sumSingle
    real*8 adouble; real*8 bdouble; real*8 sumDouble

    small2byte=1 ; big2byte=2000000 
    sum2byte=small2byte+big2byte
    print *, '2*10^6+1 with integer*2 precision:    ', sum2byte

    small4byte=1 ; big4byte=2000000
    sum4byte=small4byte+big4byte
    print *, '2*10^6+1 with integer*4 precision: ', sum4byte

    asingle = 4.0*atan(1.0)*1.0E+32
    bsingle = sqrt(2.0)*1.0E+21
    sumSingle=asingle+bsingle

    adouble = 4.D0*datan(1.D0)*1.0E+32
    bdouble = dsqrt(2.D0)*1.0E+21
    sumDouble=adouble+bdouble

    print *, 'pi*10^32+sqrt(2)*10^21 in single precision: ', sumSingle
    print *, 'pi*10^32+sqrt(2)*10^21 in double precision: ', sumDouble

end program sum



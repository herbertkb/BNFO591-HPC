program gamma

implicit none

integer, parameter :: TERMS = 100000
real, parameter :: LOWER = -4.5, UPPER = 4.5
character (len=*), parameter :: OUT_FILE = "my_gamma.out"

double precision :: gamma_prod, gamma_int

real, parameter :: PI = 4.0 * atan(1.0)
double precision :: x

print *, "Writing gamma product function for values from ", LOWER, &
         " to ", UPPER, " into file ", OUT_FILE

open(unit=11, file=OUT_FILE, status='replace')
    x = LOWER
    do while(x <= UPPER)
!        write (11,*) x, gamma_prod(x, TERMS)
        x = x + 0.01
    end do
close(11)

print *, "Finished."


print *, gamma_int(dble(0.5), dble(1e-3), 100)

stop

end program

!!-----------------------------------------------------------------------------

function gamma_prod (x, terms)
    double precision :: gamma_prod, &   !! returns a double precision value
                        x,          &   !! for a given x
                        prod            !! by continually looping a product
    integer          :: terms           !! so many terms
    
    gamma_prod = 0
    prod = 1
    
    do n=1,terms
        prod = prod * (1 + 1/real(n))**x / (1 + (x / real(n)))
    end do
    
    gamma_prod = (1/x) * prod
    
    return
end function gamma_prod

!!-----------------------------------------------------------------------------

function gamma_int (x, delta_x, terms)
    double precision :: gamma_int, &
                        x,         &
                        delta_x,   & 
                        r_sum        
    integer :: n, t, terms
    
!! int(a,b) f(x)dx =~ dx/3[f(x_0) + 4f(x_1) + 2f(x_2) ... f(x_n)]
!! gamma(x) = int_0^inf e^-t * t^(x-1) dt

    r_sum = f(x, 1)
!    print *, r_sum
    
    do t=2,terms-1
        if(mod(t, 2) == 1) then
            r_sum = r_sum + 4*f(x, t)
        else
            r_sum = r_sum + 2*f(x, t)
        end if
        print *, r_sum
    end do
    
    r_sum = r_sum + f(x, terms)

    gamma_int = (delta_x / 3) * r_sum
!    print *, gamma_int

    
    return
    
    contains
    
    double precision function f(x, t) result(g)
        double precision :: x
        integer :: t
        g =  exp(-1 * dble(t)) * dble(t)**(x-1)
!        print *, g
        return
    end function f
    
end function gamma_int
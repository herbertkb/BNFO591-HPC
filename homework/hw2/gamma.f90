program gamma

implicit none

integer, parameter :: TERMS = 100000
real, parameter :: LOWER = -4.5, UPPER = 4.5
character (len=*), parameter :: OUT_PROD = "my_gamma.out", &
                                OUT_SIMPSON = "my_gammaplusahalf.out"
double precision, parameter :: PI = 4.0 * atan(1.0)
double precision :: gamma_prod, gamma_simpson, gamma_half_error
double precision :: x, simpson

integer :: i

print *, "Writing gamma product function for values from ", LOWER, &
         " to ", UPPER, " into file ", OUT_PROD

open(unit=11, file=OUT_PROD, status='replace')
    x = LOWER
    do while(x <= UPPER)
        write (11,*) x, gamma_prod(x, TERMS)
        x = x + 0.01
    end do
close(11)

print *, "Finished."

print *, "Writing Gamma(n + 1/2) for n = {0 .. 10} to ", OUT_SIMPSON
open(unit=12, file=OUT_SIMPSON, status='replace')
    do i=0,10
        simpson = gamma_simpson(dble(i+0.5), dble(100), dble(0.001))
        write (12,*) i+0.5, simpson, gamma_half_error(int(i), simpson)
    end do
close(12)
print *, "Finished."

stop

end program

!!-------------------------------------------------------------------

function gamma_prod (x, terms)
    implicit none
    double precision :: gamma_prod, &   !! returns a double precision value
                        x,          &   !! for a given x
                        prod            !! by continually looping a product
    integer          :: terms,      &   !! so many terms
                        n               !! tracked by a counter
    gamma_prod = 0
    prod = 1
    
    do n=1,terms
        prod = prod * (1 + 1/real(n))**x / (1 + (x / real(n)))
    end do
    
    gamma_prod = (1/x) * prod
    
    return
end function gamma_prod

!!-------------------------------------------------------------------

double precision function gamma_simpson (x, t, h) result(integral)
    implicit none
    double precision :: x, t, h
    double precision, external :: G
    
    integer :: intervals, n
    
    intervals = t / h   
    
    integral = G(x, dble(0)) + G(x, t)
    
    do n=1,intervals-1
    
        if(mod(n, 2) == 1) then
            integral = integral + 4 * G(x, n*h)
        else
            integral = integral + 2 * G(x, n*h)
        end if
        
    end do
    
    integral = integral * h / 3
    
    return
    
end function gamma_simpson

!!-------------------------------------------------------------------

double precision function G(x, t) result(y)
    double precision :: x, t
    
    y = exp(-t) * t**(x - dble(1))

    return
end function G

!!-------------------------------------------------------------------

!! Gamma(n+1/2) = (1*3*5...(2n-1))/2**n * sqrt(PI)
double precision function gamma_half_error(n, measure) result(error)
    double precision :: actual, measure
    double precision, parameter :: PI = 4.0*atan(1.0)
    integer :: n, i
    
    actual = sqrt(PI) / 2**n
    do i=1,(2*n-1), 2
        actual = actual * i
    end do
    
    error = abs(actual - measure)

    return
end function gamma_half_error

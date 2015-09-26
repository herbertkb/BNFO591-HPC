!! Fortran HW2. Program 1 - Calculating first m prime numbers
!! Keith Herbert
!! This program uses trial division to find the first m prime numbers.

program mprimes

implicit none

integer, parameter :: m = 100   !! how many primes to find
integer, dimension(m):: PRIMES  !! sequence of primes
integer :: prime_index, primes_found
integer :: n, i

primes_found = 0
prime_index = 1
n = 2

do while (primes_found < m)
    !! iterate up to sqrt(n)+1
    do i=2, int(sqrt(real(n)))
        !! test if i is a divisor of n. if it is, jump out to increment n
        if (mod(n,i) == 0) then
            goto 100
        endif
    end do
    
    !! add i to PRIMES b/c no divsors were found
    PRIMES(prime_index) = n
    prime_index  = prime_index + 1
    primes_found = primes_found + 1
    
    100 n = n+1

end do

!! Print the first 10 primes found
print*, PRIMES(1:10)

end program mprimes

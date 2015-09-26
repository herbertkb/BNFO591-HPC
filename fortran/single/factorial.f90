program factorial

implicit none

!! declarations
integer::n              !! factorial(n) to be calculated
integer::int_product    !! result from integer loop
integer::i              !! index variable for calculation loops
real::real_product      !! result from real loop

!! prompt for n
print*,"find factorial of?: "
read (*,*) n

!! integer calculation 
int_product = 1
do i=1,n
    int_product = int_product * i
end do


!! real calculation 
real_product = 1.0
do i=1,n
    real_product = real_product * i
end do


!! open file and write out results of above calcuations
open(unit=1, file="factorial.out", status='replace')
    write (1,*) n
    write (1,*) int_product
    write (1,*) real_product
close (1)

end program factorial


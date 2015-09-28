program nfibonacci
implicit none

character (len=*), parameter :: out_filename="my_fibonacci.out"
integer, parameter :: m = 100
double precision, dimension(0:m) :: FIBONACCI
integer :: i


!! calculate the sequence
FIBONACCI(0) = 1
FIBONACCI(1) = 1
do i=2,m
    FIBONACCI(i) = FIBONACCI(i - 1) + FIBONACCI(i - 2)
end do

!! write the sequence to file
open(unit=11, file=out_filename, status='replace')
    do i=0,m
        write(11,*) i, FIBONACCI(i)
    end do
close(11)


!! Print first 10
print *, INT(FIBONACCI(0:9))

end program nfibonacci
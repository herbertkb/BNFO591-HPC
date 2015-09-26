program fibonacci

implicit none

integer::nfib,i     !! nfib = how many elements to find
                    !! i is index variable for calculation loop

!! seqfib = array of 64-bit integers to hold the fibonacci sequence
integer*8, dimension(:), allocatable :: seqfib

!! save output filename as constant to change easier
character (len=*), parameter :: out_filename="myfib.dat"


!! prompt for nfib and scale the array to hold the sequence
print *,"How many elements in fibonnaci sequence?"
read *, nfib
allocate(seqfib(nfib))


!! calculate the sequence
seqfib(1) = 1
seqfib(2) = 1

do i=3,nfib
    seqfib(i) = seqfib(i - 1) + seqfib(i - 2)
end do

!! write the sequence to file
! print*,seqfib
open(unit=1, file=out_filename, status='replace')
    write(1,*) seqfib
close(1)

end program fibonacci

program nfactorial

implicit none

integer :: n, i
integer*8 :: nfact_1, nfact_2    !! use 64bit intgers to store larger n! 
integer*8, dimension(:), allocatable :: memio 


nfact_1 = 1                     !! nfact_1 will hold the loop based caclulation
nfact_2 = 1                     !! nfact_2 is for the recusrive calculation

print *, "Give me a nonnegative n: "
read *, n

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

do i = 1, n
    nfact_1 = nfact_1 * i
end do

print *, "Iterative nfactorial: ", nfact_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

allocate(memio(n))
memio(1) = 1

do i = 2, n
    !! print *, i, memio(i-1) 
    memio(i) = i * memio(i-1)
end do

nfact_2 = memio(n)

print *, "Recursive nfactorial: ", nfact_2

end program nfactorial



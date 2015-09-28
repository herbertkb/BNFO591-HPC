!! Fortran HW2. Program 1 - Calculating n!
!! Keith Herbert
!! This program uses looping and recursion by memiozation to 
!! calculate factorials from 1 to some m

program nfactorial
    implicit none
    
    double precision :: iterative_fact, recursive_fact
    integer, parameter :: m = 100
    integer :: i
    double precision :: FACTORIALS(2, m)
    character (len=*), parameter :: out_file="my_factorials.out"

    !! Find 1..m! by iteration and recursion 
    do i=1,m
        FACTORIALS(1, i) = iterative_fact(i)
        FACTORIALS(2, i) = recursive_fact(i)
    end do

    !! Write to file 
    open(unit=11, file=out_file, status='replace')
        do i=1,m
            write (11,*), i, FACTORIALS(1, i), FACTORIALS(2,i)
        end do
    close(11)
    
    !! Print first ten from the iterative and recursive sequences
    print*, "Iterative: ", INT(FACTORIALS(1, 1:10)) 
    print*, "Recursive: ", INT(FACTORIALS(2, 1:10))
    
    stop
end program nfactorial

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function iterative_fact(n) 
    implicit  none
    double precision :: iterative_fact
    integer :: n, i
    
    iterative_fact = 1
    
    do i = 1, n
        iterative_fact = iterative_fact * i
    end do
    
    return
end function iterative_fact

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function recursive_fact(n)
    implicit none
    double precision :: recursive_fact
    
    integer, intent(in) :: n
    integer :: i
    double precision, dimension(:), allocatable :: memio 
    
    allocate(memio(n))
    memio(1) = 1

    do i = 2, n
        memio(i) = i * memio(i-1)
    end do
    
    recursive_fact = memio(n)
    
    return
end function recursive_fact
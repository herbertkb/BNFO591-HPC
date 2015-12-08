!! powreset.f90
!! Keith Herbert
!! BNFO591 Final Project, part 2
!!
!! Reads a file of indexed ICD10 codes as
!! 1    ABCD
!! 2    EDFG
!! ...
!! 99   ZYXW
!! 
!! and outputs a file containing the power set of possible code combinations as
!!      9 1 4 -999 2 3
!! where the first number is the index of the subset,
!! other numbers to the left of -999 are in a subset,
!! and those to the right are not
!!
!! The subsets of a set can be modeled as a binary expansion with each
!! digit representing if a given element of the set is in a particular
!! subset. Given S = {a, b, c}, we can model subsets as 
!! 000 = {}
!! 001 = {a}
!! 010 = {b}
!! 011 = {a, b}
!! 100 = {c}
!! 101 = {a, c}
!! 110 = {b, c}
!! 111 = {a, b, c}
!!
!! This program works by taking the index of the current subset and converting
!! it to its binary string as a logical array. The logical array is parsed
!! to generate the output line.
!!
program powerset
    implicit none

    interface
        integer function CountLinesInFile(filename) result (lineCount)
            character(len=*), intent(in) :: filename
        end function
        
        subroutine ConvertIndexToSubsetArray(indx, subset)
            integer(kind=8)        :: indx
            logical, intent(inout) :: subset(:)
        end subroutine
      
    end interface
    

    character(len=*), parameter :: IN_FILE       = "Alphabetical_ICD.dat" 
    character(len=*), parameter :: OUT_FILE      = "no_opt_timing.dat"
    integer(kind=1), parameter  :: OUT_UNIT      = 100
    logical,parameter           :: WRITE_TO_FILE = .TRUE.
    integer, parameter          :: N             = 10
   
    integer :: code_size                !! how many codes provided
    logical, allocatable :: subset(:)   !! indicates a given subset of codes
    integer :: i                        !! index variables
    integer(kind=8) :: subset_index     !! the current subset 
    integer(kind=8) :: temp             !! a copy of subset_index destroyed
                                        !!   during conversion to binary string
    integer(kind=1) :: digit            !! the binary digit shaved off on each
                                        !!   pass of the conversion process.
    
    integer(kind=8) :: time_start, time_end, rate !! timing vales
    integer(kind=8) :: times(N)                  !! timing array
       
    allocate(subset(CountLinesInFile(IN_FILE)))
    
    open(unit=OUT_UNIT, file=OUT_FILE)
    
    do i = 0, N
        call system_clock(time_start, rate)
        
        do subset_index=1,10**i
            call ConvertIndexToSubsetArray(subset_index, subset)
        enddo
        
        call system_clock(time_end)
        
        times(i) = time_end - time_start
        
        print *, i, times(i)
        write(OUT_UNIT, *) i, times(i)
    end do
    
    close(OUT_UNIT)

end program

subroutine ConvertIndexToSubsetArray(indx, subset)
    implicit none
    integer(kind=8),intent(in) :: indx        
    integer(kind=8)            :: temp
    logical, intent(inout)     :: subset(:)
    integer :: i
    integer(kind=8) :: digit
    
    temp = indx    !! can't modify dummy var x, so i need to destroy temp
    
    do i=1,size(subset)
        !! Stop when nothing left to convert.
        if (temp < 0) then 
            exit
        endif
    
        !! Get the rightmost digit. 
        !! This indicates membership for the ith element
        digit = iand(temp, 1)
        
        !! Set that elements membership in the subset
        if(digit == 0) then
            subset(i) = .FALSE.
        else
            subset(i) = .TRUE.
        end if
        
        !! right shift the integer to shave off the processed digit
        temp = ishft(temp, -1)
    end do
end subroutine

!! Returns an integer equal to the number of non-blank lines in FILENAME
integer function CountLinesInFile(filename) result (lineCount)
    implicit none
    character(len=*), intent(in) :: filename
    integer :: iostatus
    character(len=1) :: a

    lineCount = 0
    open(unit=11, file=filename, status='old')
        do
            read(11, *, iostat=iostatus) a  
            if (iostatus .EQ. 0) then
                if (a .NE. "") then
                    lineCount = lineCount + 1
                endif
            else
                exit
            end if
       end do
    close(unit=11)
    
    return
    
end function CountLinesInFile
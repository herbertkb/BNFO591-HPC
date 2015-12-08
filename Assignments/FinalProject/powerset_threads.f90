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
!     use omp_lib
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
    character(len=*), parameter :: OUT_FILE      = "thread_timing.dat"
    integer(kind=1), parameter  :: OUT_UNIT      = 100
    logical,parameter           :: WRITE_TO_FILE = .TRUE.
  
    logical, allocatable :: subset(:)   !! indicates a given subset of codes
    integer :: i,j                      !! index variables
    integer(kind=8) :: subset_index     !! the current subset
    
    integer(kind=8) :: time_start, time_end, time_elapsed !! timing vales
    
    integer(kind=8), dimension(3) :: calc_sets = &
                                        (/ 10D0**3D0, 10D0**6D0, 2D0**20D0 /)
    integer, dimension(4) :: threads = (/ 2, 4, 8, 16 /)
    integer :: num_threads
    
   
    allocate(subset(CountLinesInFile(IN_FILE)))
    
    open(unit=OUT_UNIT, file=OUT_FILE)
    
    do i=1,size(calc_sets)
        do j=1, size(threads) 

        num_threads = threads(j)
        call omp_set_num_threads(num_threads)
        call system_clock(time_start)
!$omp parallel       
                      
!$omp do            
            do subset_index=1,calc_sets(i)
                call ConvertIndexToSubsetArray(subset_index, subset)
            enddo
!$omp end do            
            
!$omp end parallel
        call system_clock(time_end)
        time_elapsed = time_end - time_start

        print *, calc_sets(i), threads(j), time_elapsed
        write (OUT_UNIT, *) calc_sets(i), threads(j), time_elapsed
   
        end do  !! end threads
    end do  !! end calc_sets
    
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
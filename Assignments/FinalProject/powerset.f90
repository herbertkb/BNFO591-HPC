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
        
        subroutine WriteSubset(u, ind, subset)
            integer(kind=1), intent(in) :: u   !! logical unit for output
            integer(kind=8), intent(in) :: ind !! index of subset to start line
            logical, intent(inout) :: subset(:)!! subset to be printed
        end subroutine
        
        
    end interface
    

    character(len=*), parameter :: IN_FILE  = "Alphabetical_ICD.dat" 
    character(len=*), parameter :: OUT_FILE = "all_possible_subsets.dat"
    integer(kind=1), parameter  :: OUT_UNIT = 100
    logical,parameter           :: WRITE_TO_FILE = .TRUE.
    integer, parameter          :: HOW_FAR = 100
   
    integer :: code_size                !! how many codes provided
    logical, allocatable :: subset(:)   !! indicates a given subset of codes
    integer :: i,j                      !! index variables
    integer(kind=8) :: subset_index     !! the current subset 
    integer(kind=8) :: temp             !! a copy of subset_index destroyed
                                        !!   during conversion to binary string
    integer(kind=1) :: digit            !! the binary digit shaved off on each
                                        !!   pass of the conversion process.
     
    
    print *, "Reading ", IN_FILE, " and getting code indices"
    code_size = CountLinesInFile(IN_FILE)
    print *, code_size, " codes in ", IN_FILE
   
    
    print *, "Calculating ICD10 code subsets and writing to ", OUT_FILE
    open(unit=OUT_UNIT, file=OUT_FILE)
    allocate(subset(code_size))
    
    do subset_index=1,HOW_FAR
        !! First we need need to convert the current index to a binary string
        !! stored in a logical array.
        call ConvertIndexToSubsetArray(subset_index, subset)

        !! Then print the subset represented by the array
        if(WRITE_TO_FILE) then
            call WriteSubset(OUT_UNIT, subset_index, subset)
        end if
    enddo
    close(OUT_UNIT)
    
    print *, "Finished."

end program

subroutine WriteSubset(u, ind, subset)
    implicit none
    integer(kind=1), intent(in) :: u     !! logical unit for output
    integer(kind=8), intent(in) :: ind   !! index of subset to start line
    logical, intent(inout) :: subset(:)  !! subset to be printed
    integer :: j                         !! index variable 

    !! print the index at start of line
    write(u, '(I15)', ADVANCE="NO") ind
    
    !! print the element IDs in the subset 
    do j=1, size(subset)
        if(subset(j)) then
           write(u, '(I2)', ADVANCE="NO") j
        endif
    end do
    
    !! this separates the ins from the outs
    write(u, '(I5)', ADVANCE = "NO") -999
    
    !! print the element IDs NOT in the subset
    do j=1, size(subset)
        if(.NOT. subset(j)) then
           write(u, '(I4)', ADVANCE="NO") j
        endif
    end do
    
    !! new line
    write(u, '(A)') ""
    
end subroutine

subroutine ConvertIndexToSubsetArray(indx, subset)
    implicit none
    integer(kind=8),intent(in)  :: indx        
    integer(kind=8)             :: temp
    logical, intent(inout)      :: subset(:)
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
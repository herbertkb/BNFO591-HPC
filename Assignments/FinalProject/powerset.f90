!! orderbyalpha.f90
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


program powerset
    implicit none

    interface
        integer function CountLinesInFile(filename) result (lineCount)
            character(len=*), intent(in) :: filename
        end function
    end interface
    

    character(len=*), parameter :: IN_FILE  = "Alphabetical_ICD.dat" 
    character(len=*), parameter :: OUT_FILE = "all_possible_subsets.dat"
    integer(kind=1), parameter  :: OUT_UNIT = 100 
    
    integer, parameter :: HOW_FAR = 100     !! how many subsets to output
    integer, allocatable :: code_indices(:)
    integer :: code_size
    logical, allocatable :: subset(:)
    integer :: i,j                          !! index variable
    integer(kind=8) :: temp 
    integer(kind=1) :: digit
     
    
    print *, "Reading ", IN_FILE, " and getting code indices"
    allocate( code_indices( CountLinesInFile(IN_FILE) ) )
    code_size = size(code_indices)
    do i=1,code_size
        code_indices(i) = i
    enddo
    
    print *, code_size, " codes in ", IN_FILE
    
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
    
    print *, "Calculating ICD10 code subsets and writing to ", OUT_FILE
    open(unit=OUT_UNIT, file=OUT_FILE)
    allocate(subset(code_size))
    
    do i=1,HOW_FAR
        !! First we need need to convert the current index to a binary string
        !! stored in a logical array.
        temp = i
        do j=1,code_size
            
            if (temp < 0) then 
                exit
            endif
        
            digit = iand(temp, 1)
                    
            if(digit == 0) then
                subset(j) = .FALSE.
            else
                subset(j) = .TRUE.
            end if
           
            temp = ishft(temp, -1)
        end do

        !! Then print the subset represented by the array
        write(OUT_UNIT, '(I20)', ADVANCE="NO") i 
        do j=1, code_size
            if(subset(j)) then
               write(OUT_UNIT, '(I2)', ADVANCE="NO") j
            endif
        end do
        write(OUT_UNIT, '(I5)', ADVANCE = "NO") -999
        do j=1, code_size
            if(.NOT. subset(j)) then
               write(OUT_UNIT, '(I4)', ADVANCE="NO") j
            endif
        end do
        write(OUT_UNIT, '(A)') ""
    enddo
    close(OUT_UNIT)
    
    print *, "Finished."

end program



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
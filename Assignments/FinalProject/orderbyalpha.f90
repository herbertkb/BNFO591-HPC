!! orderbyalpha.f90
!! Keith Herbert
!! BNFO591 Final Project, part 1
!! 
!! Collects, sorts, and prints by index an array of 4 letter ICD10 Codees
!! from a file as:
!! 
!! ABCD
!! ZYXW
!!  ...
!! EDFG
!! 
!! into
!! 1    ABCD
!! 2    EDFG
!! ...
!! 99   ZYXW

program orderbyalpha
    implicit none
    interface
        integer function CountLinesInFile(filename) result (lineCount)
            character(len=*), intent(in) :: filename
        end function 
        subroutine SelectionSort(array)
            character(len=4), intent(inout) :: array(:)
        end subroutine
    end interface


    character(len=*), parameter :: IN_FILE  = "ICD10_Codes_Unordered.txt" 
    character(len=*), parameter :: OUT_FILE = "Alphabetical_ICD.dat"      
    integer, parameter :: IN_LU = 100
    integer, parameter :: OUT_LU = 101


    integer, parameter :: CODE_LENGTH = 4
    character(len=CODE_LENGTH) :: code
    character(len=CODE_LENGTH), allocatable :: codes(:)

    integer :: i, code_count
    integer :: iostatus

!! Find out how many codes we need to store and sort
    allocate(codes(CountLinesInFile(IN_FILE)))
    
!! Parse the file into an array of codes
    open(unit=IN_LU, file=IN_FILE, status='old')
!         print *, "reading ", IN_FILE    
        do i=1,size(codes)
            read(IN_LU, *, iostat=iostatus) code  
            if (iostatus .EQ. 0) then
                print *, "reading ", code
                codes(i) = code
            else
                exit
            end if
        end do
    close(IN_LU)
   
   
!! Sort the collection of codes
    call SelectionSort(codes)
    

!! Write the ordered list of codes to STDOUT and OUT_FILE
    open(unit=OUT_LU, file=OUT_FILE)
        do i=1,size(codes)
            print *, i, codes(i)
            write (OUT_LU, *) i, codes(i) 
        enddo
    close(OUT_LU)
    
    

endprogram

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


!! Adapted from RosettaCode.org, C implementation of Selection Sort
subroutine SelectionSort(array)
    implicit none
    character(len=4), intent(inout) :: array(:)
    integer :: n
    integer :: i, j
    integer :: smallest

    character(len=4) :: temp
    
    n = size(array, 1)
    
    !! for each element in the array
    do i=1,n
    
        !! find the ith smallest
        smallest = i
        do j=i, n
            if (array(j) < array(smallest)) then
                 smallest = j
            end if
        end do
        
        !! and swap array(i) with the ith smallest
        temp = array(i)
        array(i) = array(smallest)
        array(smallest) = temp
        
    end do 
    
end subroutine
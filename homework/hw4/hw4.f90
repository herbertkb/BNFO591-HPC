!! Keith Herbert 
!! FORTRAN Homework Assignment 4
!! Various Sequence Alignment Functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program hw4
    implicit none
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    interface
!         function SequenceAlphabet(seq)
!             character(len=*), intent(in) :: seq
!         end function
        
        subroutine SelectionSort(array)
            character(len=1), intent(inout) :: array(:)
        end subroutine
        
        integer function StringIndexInArray(array, string)
            character(len=1), intent(in) :: array(:)
            character(len=1), intent(in) :: string
        end function
!         character(*) function ReadFastaFile(filename)
!             character(len=*), intent(in) :: filename
!         end function
        subroutine GetUniqueValues(seq, list)
            character(len=1), intent(in) :: seq(:)
            character(len=1), allocatable, intent(inout) :: list(:)
        end subroutine
        
    end interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    


    character(len=*),parameter  ::                              &
            seq_file   = "TitinFastaFormat.txt",                &
            probe_file = "WPh_1Probe.txt",                      &
            letter_freq_file =  "unique_letter_fequencies.dat"  
                              

    character(:), allocatable :: full_seq, probe 
    character(len=2**20) :: temp_seq
    character(len=1024)  :: curr_line
    integer :: iostatus
    
    !! Variables for finding the unique letter frequencies 
    integer, dimension(26) :: letter_freqs = 0
    character :: letter
    integer :: letter_ascii, position
    integer :: i
    
   
!! Read in the sequence file  
    temp_seq = ""
    open(unit=11, file=seq_file, status='old')
        do
            read(11, *, iostat=iostatus), curr_line
            if (iostatus .NE. 0 )        exit      ! end of file, stop reading
            
            if (curr_line(1:1) .NE. '>') then      ! skip headers
                temp_seq = trim(temp_seq) // trim(curr_line) 
            end if
        end do
    close(unit=11)
   
    full_seq = temp_seq(:index(temp_seq, " "))
    
!! read in the probe
    temp_seq = ""
    open(unit=11, file=probe_file, status='old')
        do
            read(11, *, iostat=iostatus), curr_line
            if (iostatus .NE. 0 )        exit      ! end of file, stop reading
            
            if (curr_line(1:1) .NE. '>') then      ! skip headers
                temp_seq = trim(temp_seq) // trim(curr_line) 
            end if
    
        end do
    close(unit=11)
     
    probe = temp_seq(:index(temp_seq, " "))

    
!! Find the frequency for each letter in the sequence    
    do i=1,len(full_seq)
        !! get the letter at the current index
        letter = full_seq(i:i)
        
        !! its ASCII representation
        letter_ascii = iachar(letter)
        
        !! its relative position between A and Z -> (1 and 26)
        position = letter_ascii - 64
        
        !! update the frequency for that position
        letter_freqs(position) = letter_freqs(position) + 1
    end do

!! Display them
    open(unit=11, file=letter_freq_file)
    do i=1,26
        if(letter_freqs(i) > 0) then
            letter = char(i+64)
            print *, letter, letter_freqs(i)
            write(11, *) letter, letter_freqs(i) 
        endif
    enddo
    close(11)


    
    
    
    
    
    
    
end program hw4

! subroutine GetUniqueValues(seq, list)
!     implicit none
!     character(len=1), intent(in) :: seq(:)
!     character(len=1), intent(inout) :: list(:)
!     character(len=1), allocatable :: tempList(:)
!     integer :: m, n
!     integer :: i, j, k
!     logical :: isUnique = .FALSE.
!     
!     m = size(seq)
!     
!  
!     k = 0
!     do i=1, m
!         if (.NOT. ANY( list == seq(i) ) ) then
!             k = k+1
!             list(k) = seq(i)
!         end if
!     end do
!     
!     print *, list(:k)
!          
!     !! resize list to remove empty cells.
!     allocate(tempList(k))
!     tempList = list(:k)
!     print *, size(tempList)
!     
!     deallocate(list)
!     allocate(list(k))
!     
!     list = tempList
!     print *, size(list)
!     
! end subroutine
! 






! character*(*) function ReadFastaFile(filename)
!     implicit none
!     character, intent(in) :: filename
!     character(:), allocatable :: seq_trimmed
!     character(len=2**20) :: seq
!     character(len=1024)  :: curr_line
!     integer :: iostatus
!     
!     open(unit=11, file=filename, status='old')
!     do
!         read(11, *, iostat=iostatus), curr_line
!         if (iostatus .NE. 0 )        exit      ! end of file, stop reading
!         
!         if (curr_line(1:1) .NE. '>') then      ! skip headers
!             seq = trim(seq) // trim(curr_line) 
!         end if
!     end do
!     close(unit=11)
!     
!     seq_trimmed = seq(:index(seq, " "))
!         
! end function



! function SequenceAlphabet(seq)
!     character, allocatable :: SequenceAlphabet(:)
!     character(len=*), intent(in) :: seq
!     character, dimension(2**7)   :: temp_alphabet
!     integer :: i, alphabet_size
!     
!     do i=1, len(temp_alphabet)
!         if (index(seq, char(i)) > 0 ) then
!             temp_alphabet(i) = .true.
!         end if
!     end do
!     
!     do i=1, len(temp_alphabet)
!         if (temp_alphabet) then
!             alphabet_size = alphabet_size + 1
!         end if
!     end do
!     
!     allocate( SequenceAlphabet(alphabet_size) )
! end function


!! Adapted from RosettaCode.org, C implementation of Selection Sort
subroutine SelectionSort(array)
    implicit none
    character(len=1), intent(inout) :: array(:)
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



integer function StringIndexInArray(array, string)
    character(len=4), intent(in) :: array(:)
    character(len=4), intent(in) :: string
    integer :: i
    
    do i=1,size(array)
        if(string == array(i)) then
            StringIndex = i
            exit
        end if
    end do
    
    return
    
end function
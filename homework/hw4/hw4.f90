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
                              
                              
    !! Variables to read sequence files
    character(:), allocatable :: full_seq, probe 
    character(len=2**20) :: temp_seq
    character(len=1024)  :: curr_line
    integer :: iostatus
    
    integer :: i, j, k  !! never enough index variables
    
    !! Variables for finding the unique letter frequencies 
    integer, dimension(26) :: letter_freqs = 0
    character :: letter
    integer :: letter_ascii, position
    
    
    !! Variables to build pairwise matrix
!     integer :: unique_count
!     character, allocatable :: unique_letters(:)
!     integer, allocatable :: P(:,:)
    integer :: n
    character :: curr, next
    integer   :: c_indx, n_indx
    integer, dimension(26,26) :: PAM = 0
    
   
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    open(unit=11, file=letter_freq_file)
    do i=1,26
        if(letter_freqs(i) > 0) then
            letter = char(i+64)
!             print *, letter, letter_freqs(i)
            write(11, *) letter, letter_freqs(i) 
        endif
    enddo
    close(11)
    
!! Pairwise Adjacency Matrix
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do n=1,len(full_seq)-1
        curr = full_seq(n:n)
        next = full_seq(n+1:n+1)
               
        c_indx = iachar(curr)-64
        n_indx = iachar(next)-64
        
        PAM(c_indx, n_indx) = PAM(c_indx, n_indx) + 1
    enddo
    
    write(*, '(A5)', ADVANCE='NO') " "
    do i=1,26
        if (letter_freqs(i) == 0) cycle
        write(*, '(A5)', ADVANCE='NO') char(i+64)
    end do
    write(*,*) ""
    
    
    do i=1,26
        if (letter_freqs(i) == 0) cycle
        write(*, '(A5)', ADVANCE='NO') char(i+64)
        do j=1,26
            if (letter_freqs(j) == 0) cycle
            write(*, '(I5)', ADVANCE="NO") PAM(i,j)
        enddo
        write(*,*) ""
    enddo

    
end program hw4
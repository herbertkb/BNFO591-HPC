!! Keith Herbert 
!! FORTRAN Homework Assignment 4
!! Various Sequence Alignment Functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program hw4
    implicit none

    !! Hard Values
    character(len=*),parameter  ::                              &
            seq_file   = "TitinFastaFormat.txt",                &
            probe_file = "WPh_1Probe.txt",                      &
            letter_freq_file =  "unique_letter_fequencies.dat"  
    
    real, parameter :: match_threshold = 0.12                
                              
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
    
    
    !! Variables for Exact Linear Search
    integer :: exact_match_count
    character(:), allocatable :: substring
    
    
    !! Variables for Partial Alignments
    integer :: partial_alignment_count, matching_count
    real :: match_score
    
    !! Timing Variables
    real :: start_time, end_time
    
    
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
            print *, letter, letter_freqs(i)
            write(11, *) letter, letter_freqs(i) 
        endif
    enddo
    close(11)
    
!! Pairwise Adjacency Matrix
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Iterate through the sequence and update the score for each pair
    do n=1,len(full_seq)-1
        curr = full_seq(n:n)
        next = full_seq(n+1:n+1)
               
        c_indx = iachar(curr)-64
        n_indx = iachar(next)-64
        
        PAM(c_indx, n_indx) = PAM(c_indx, n_indx) + 1
    enddo
    
    !! Display the matrix
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
    
    
!! Exact Linear Search of Probe Against Full Sequence
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    exact_match_count = 0
    do n=1,len(full_seq)-len(probe)
        substring = full_seq(n:n+len(probe))
        if( substring .EQ. probe  ) then
            exact_match_count = exact_match_count + 1
        endif
    end do

    write(*,*) "Exact Alignments of probe in sequence: ", exact_match_count
    
!! Partial Alignment of Probe Against Full Sequence
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
    partial_alignment_count = 0
!     write(*,*) 1, 1, probe
    
    call cpu_time(start_time) 
    do n=1,len(full_seq)-len(probe)
        substring = full_seq(n:n+len(probe)-2)
        matching_count = 0
        do i=1,len(probe)
            if( probe(i:i) .EQ. substring(i:i) ) then
                matching_count = matching_count + 1 
            endif
        enddo
        
        
        match_score = real(matching_count) / real(len(probe)) 
!         print *, matching_count, match_score, len(probe)
        
        if( match_score >= match_threshold ) then
            partial_alignment_count = partial_alignment_count + 1
!             write(*,*) n, n+len(probe), substring
        endif
    end do
    call cpu_time(end_time) 
    
    write(*,*) "Time to search for partial alignments at", match_threshold, &
            "% threshold:", (end_time - start_time)
    write(*,*) "Total Partial Alignments: ", partial_alignment_count
    
end program hw4
program grade_me_1
character (len=1)::grade
real:: test_score

! test_score = random_number * 100
call random_seed()
call random_number(test_score)

test_score = test_score*100

print*,'Score:',test_score

if(test_score.lt.60) then 
    grade='F' 
elseif (test_score.lt.70) then 
    grade='D' 
elseif(test_score.lt.80) then 
    grade='C' 
elseif(test_score.lt.90) then 
    grade='B' 
else 
    grade = 'A'
endif

print*,'Grade',grade

end program grade_me_1

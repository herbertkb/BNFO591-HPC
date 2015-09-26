program grade_me_2

character(len=4)::grade
integer::test_score

test_score = 88

select case (test_score)
    case (:59)
        grade='fail'
    case (60:)
        grade='pass'
end select

print *, 'score:',test_score
print *, 'grade: ', grade


end program grade_me_2

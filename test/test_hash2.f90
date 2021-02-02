program test_hash
 use modhash, only: seed_hash, hash, hash2
 use iso_fortran_env, only: int32, real32
 implicit none
 integer(int32) :: pc, pb

 pc = seed_hash
 pb = 0
 call hash2([1], pc, pb)
 call check(pc == hash([1], seed_hash), 'seed int32_array 1')

 pc = seed_hash
 pb = 0
 call hash2([1,2], pc, pb)
 call check(pc == hash([1, 2], seed_hash), 'seed int32_array 2')

 pc = seed_hash
 pb = 0
 call hash2([1, 2, 3], pc, pb)
 call check(pc == hash([1, 2, 3], seed_hash), 'seed int32_array 3')

 pc = seed_hash
 pb = 0
 call hash2([1, 2, 3, 4], pc, pb)
 call check(pc == hash([1, 2, 3, 4], seed_hash), 'seed int32_array 4')

 pc = seed_hash
 pb = 0
 call hash2([1, 2, 3, 4, 5], pc, pb)
 call check(pc == hash([1, 2, 3, 4 , 5], seed_hash), 'seed int32_array 5')


 pc = seed_hash
 pb = seed_hash
 call hash2([1], pc, pb)
 call check(pc == -2020856101 .and. pb == 1987275883, 'pc pb int32 1')

 call hash2([1,2], pc, pb)
 call check(pc == -845732042 .and. pb == 483289004, 'pc pb int32 2')

 call hash2([1, 2, 3], pc, pb)
 call check(pc == -787512166 .and. pb == 1839643712, 'pc pb int32 3')

 call hash2([1, 2, 3, 4], pc, pb)
 call check(pc == -2036325979 .and. pb == 1242371621, 'pc pb int32 4')

 call hash2([1, 2, 3, 4, 5], pc, pb)
 call check(pc == 1615783110 .and. pb == 1420343745, 'pc pb int32 5')


 pc = seed_hash
 pb = 0
 call hash2([1.], pc, pb)
 call check(pc == hash([1.], seed_hash), 'seed real32_array 1')

 pc = seed_hash
 pb = 0
 call hash2([1.,2.], pc, pb)
 call check(pc == hash([1., 2.], seed_hash), 'seed real32_array 2')

 pc = seed_hash
 pb = 0
 call hash2([1., 2., 3.], pc, pb)
 call check(pc == hash([1., 2., 3.], seed_hash), 'seed real32_array 3')

 pc = seed_hash
 pb = 0
 call hash2([1., 2., 3., 4.], pc, pb)
 call check(pc == hash([1., 2., 3., 4.], seed_hash), 'seed real32_array 4')

 pc = seed_hash
 pb = 0
 call hash2([1., 2., 3., 4., 5.], pc, pb)
 call check(pc == hash([1., 2., 3., 4., 5.], seed_hash), 'seed real32_array 5')


 pc = seed_hash
 pb = seed_hash
 call hash2([1.], pc, pb)
 call check(pc == 775533625.and. pb == -1968582623, 'pc pb real32 1')

 call hash2([1.,2.], pc, pb)
 call check(pc == -1947114094 .and. pb == -467174068, 'pc pb real32 2')

 call hash2([1., 2., 3.], pc, pb)
 call check(pc == 1942420507 .and. pb == -123740994, 'pc pb real32 3')

 call hash2([1., 2., 3., 4.], pc, pb)
 call check(pc == -412244660 .and. pb == -554047323, 'pc pb real32 4')

 call hash2([1., 2., 3., 4., 5.], pc, pb)
 call check(pc == -556407929 .and. pb == -1674357654 , 'pc pb real32 5')


 pc = seed_hash
 pb = 0
 call hash2('a', pc, pb)
 call check(pc == hash('a', seed_hash), 'seed char_array 1')

 pc = seed_hash
 pb = 0
 call hash2('ab', pc, pb)
 call check(pc == hash('ab', seed_hash), 'seed char_array 2')

 pc = seed_hash
 pb = 0
 call hash2('abc', pc, pb)
 call check(pc == hash('abc', seed_hash), 'seed char_array 3')

 pc = seed_hash
 pb = 0
 call hash2('abcd', pc, pb)
 call check(pc == hash('abcd', seed_hash), 'seed char_array 4')

 pc = seed_hash
 pb = 0
 call hash2('abcde', pc, pb)
 call check(pc == hash('abcde', seed_hash), 'seed char_array 5')


 pc = seed_hash
 pb = 0
 call hash2(12345, pc, pb)
 call check(pc == hash(12345, seed_hash), 'seed int32_scal 1')


 pc = seed_hash
 pb = 0
 call hash2(12345., pc, pb)
 call check(pc == hash(12345., seed_hash), 'seed real32_scal 1')

contains

 subroutine check(lcheck, a)
  logical, intent(in) :: lcheck
  character(*), intent(in), optional :: a

  if (.not. lcheck) then
   if (present(a)) print'(2a)', 'ERROR: ', a
   error stop
  end if
 end subroutine

end program

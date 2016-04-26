program main
      real, dimension(5) :: x
external incb
! THIS IS THE RIGHT WAY
!      interface
!         subroutine incb(a)
 !          real, dimension(:) :: a
  !       end subroutine incb
   !   end interface
      x = 0.
      call incb(x)
      print *, x
end program main

! Maze Generator in Fortran.
! Joe Wingbermuehle
! 2010-09-28

program maze

   implicit none
   integer :: width, height
   parameter (width = 39)
   parameter (height = 23)
   integer :: mz(width, height)

   mz(:,:) = 1
   call generate_maze(width, height, mz)
   call show_maze(width, height, mz)

end program maze

! Display the maze.
subroutine show_maze(width, height, mz)

   implicit none
   integer, intent(in) :: width, height
   integer, intent(in) :: mz(width, height)
   integer :: x, y

   do y = 1, height
      do x = 1, width
         if (mz(x, y) .eq. 0) then
            write (*,'(AA)',advance='no') '  '
         else
            write (*,'(AA)',advance='no') '[]'
         end if
      end do
      print *, ''
   end do

end subroutine show_maze

! Generate the maze.
subroutine generate_maze(width, height, mz)

   implicit none
   integer, intent(in)     :: width, height
   integer, intent(inout)  :: mz(width, height)
   integer                 :: x, y
   integer                 :: seed(8)

   call itime(seed)
   call random_seed(put = seed)
   mz(2, 2) = 0
   do y = 2, height, 2
      do x = 2, width, 2
         call carve_maze(width, height, mz, x, y)
      end do
   end do
   mz(2, 1) = 0
   mz(width - 1, height) = 0

end subroutine generate_maze

! Carve the maze at the specified coordinates.
subroutine carve_maze(width, height, mz, x, y)

   implicit none
   integer, intent(in)     :: width, height
   integer, intent(inout)  :: mz(width, height)
   integer, intent(in)     :: x, y
   real     :: rand
   integer  :: dir, cnt
   integer  :: dx, dy, localx, localy
   integer  :: x1, y1, x2, y2

   call random_number(rand)
   cnt = 0
   dir = rand * 4
   localx = x
   localy = y
   do
      dx = 0
      dy = 0
      select case (dir)
         case (0)
            dx = 1
         case (1)
            dy = 1
         case (2)
            dx = -1
         case default
            dy = -1
      end select
      x1 = localx + dx
      y1 = localy + dy
      x2 = x1 + dx
      y2 = y1 + dy
      if (     x2 .gt. 1 .and. x2 .lt. width    &
         .and. y2 .gt. 1 .and. y2 .lt. height   &
         .and. mz(x1, y1) .eq. 1 .and. mz(x2, y2) .eq. 1) then
         mz(x1, y1) = 0
         mz(x2, y2) = 0
         localx = x2
         localy = y2
         call random_number(rand)
         dir = rand * 4
         cnt = 0
      else
         cnt = cnt + 1
         if (cnt .gt. 3) then
            exit
         end if
         dir = mod(dir + 1, 4)
      end if
   end do

end subroutine carve_maze


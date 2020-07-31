module point_module
implicit none
  
  ! Type for vertexes
  type, public :: vertex
      real, public :: X, Y
  end type vertex
  
  ! Type for triangle
  type, public :: triangle
      type(vertex), allocatable, private :: point(:)
      contains
      final :: TriangleDestructor
      end type triangle
  
      ! Binary operators
  interface operator (.zoom.)
      module procedure TriangleZoom
  end interface
  
  interface operator (.rotate.)
      module procedure TriangleRotate
  end interface
    
  private :: TriangleZoom
  private :: TriangleRotate
    contains
    
  
!===== Reading array from a file =========
  function TriangleConstructor() result(s) 
    type(triangle) :: s
    character(16)  :: filename
    integer n

    print*, ' Enter the filename...' 
    read*, filename
      
    open(1,file=filename) 
    read(1,*) n
    allocate(s.point(n))     
    read(1,*) s.point(1:n)
    close(1)
    print*, 'Successfully read the file'
  end function TriangleConstructor
   
  ! getCenter of triangle ( or other polygone )
  function get_center(s) result (center)
    type(triangle) s
    type(vertex) :: center
    integer      :: i,n
    
    n = size(s.point)
    center.X = 0
    center.Y = 0
     
    do i=1,n
        center.X = center.X + s.point(i).X/n
        center.Y = center.Y + s.point(i).Y/n
    end do
  
    print*, 'Center:'
    write(*,'(f8.2,f8.2)') center.X, center.Y
  end function get_center
  
!============ Print array from file to console ( input data) =======
  subroutine PrintTriangle(s)
  implicit none
    type(triangle)   :: s
    integer          :: i, n
    
    n = size(s.point)
    write(*,'(a7, a7)') 'X', 'Y'
     
    do i=1,n
        write(*,'(f8.2,f8.2)') (s.point(i))
    end do
  end subroutine PrintTriangle
 
  
  ! Subroutine for saving solution to a new file
  subroutine SaveToFile(s)
  implicit none
    type(triangle), intent(in) :: s
    integer                    :: i, n
    character(16)              :: filename

    n = size(s.point)
    
    print*, ' Enter the filename in which you would like to save new triangle'
    read(*,*) filename
    open(1,file=filename, action='write') 
    write(1,'(i8)') n
    
    do i=1,n
       write(1,'(f8.2)') (s.point(i))
    end do
    
    close(1)
    print*, 'File successfully created.'
  end subroutine SaveToFile
    
!============ Zoom ================== 
  function TriangleZoom(s, k) result(t_new) 
    type(triangle), intent(in) :: s
    type(triangle)             :: t_new
    real, intent(in)           :: k
    integer                    :: i, n
    type(vertex)               :: center
    
    n = size(s.point)
    allocate(t_new.point(n))
    center = get_center(s)
    
    do i=1,n
        t_new.point(i).X = center.X+(s.point(i).X-center.X)*k
        t_new.point(i).Y = center.Y+(s.point(i).Y-center.Y)*k
    end do
    
  end function TriangleZoom
  
  !============ Rotate  ==================
  function TriangleRotate(s, angle) result(t_new) 
    type(triangle), intent(in) :: s
    type(triangle)             :: t_new
    real, intent(in)           :: angle
    type(vertex)               :: center
    real                       :: r, pi
    integer                    :: i, n
    
    n = size(s.point)
    pi = 4.D0*DATAN(1.D0)
    r = (pi*angle)/180
   
    allocate(t_new.point(n))
    center = get_center(s)
    
    do i=1,n
        t_new.point(i).X = (s.point(i).X - center.X)*cos(r) - &
            & (s.point(i).Y - center.Y)*sin(r) + center.X
        t_new.point(i).Y = (s.point(i).X - center.X)*sin(r) + &
            & (s.point(i).Y - center.Y)*cos(r) + center.Y
    end do
  end function TriangleRotate
  
  ! Destructor
  subroutine TriangleDestructor(s)
    type(triangle) :: s
    
    if (allocated(s.point)) then
       deallocate(s.point)
    end if
  end subroutine TriangleDestructor
  
end module point_module
    
!=====================================================
program main
  use point_module
  implicit none
  real            :: k, angle
  type(triangle)  :: t, new_t
  type(vertex)    :: center
  character(10)   :: answer
  integer operation
  
  t = TriangleConstructor()
  call PrintTriangle(t) 
        
  do while (.true.)      
      print*, 'Select operation: 1 - zoom, 2 - rotation'
      read*, operation
  
      select case(operation)
          case(1)
          print*, ' Enter koefficient of zoom'
          read*, k
          
          if (k<=0) then
              print*, 'Wrong koefficient'
              cycle
          end if

          new_t = t.zoom.k 
      
          case(2)
          print*, 'Enter angle in degrees'
          read*, angle
          
          new_t = t.rotate.angle
      end select
      
      print*, '===================================================='
      print*, 'Triangle after operation'
      call PrintTriangle(new_t)
      center = get_center(new_t)

      print*, 'Would you like to save new triangle? Type <y> &
          & if yes or other button if not'
      read*, answer
      
      if ((answer=='y') .or. (answer=='Y')) then
          call SaveToFile(new_t)
          pause
      else
          pause
      end if
  end do
end program main

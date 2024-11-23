module cli_mo

  implicit none

  private
  public :: cli_ty

  character(255) :: arg
  integer        :: status, i_arg

  ! Dummy arguments to suppress -W-unused-functions message
  ! Or, use -Wno-unused-functions
  character(255) cdummy
  integer idummy
  real rdummy
  logical ldummy

  type cli_ty
    character(255) :: pdir, home
    character(19)  :: datetime
    character(255) :: title
    character(255) :: exe
    character(255) :: version
    character(255) :: author
    character(255) :: copyright
    character(255) :: usage(255)
    integer        :: n_usage
  contains
    procedure get_args
    procedure print_help
    procedure print_version
  end type

  interface get_value

    module subroutine get_character ( str, i_arg )
      character(*), intent(inout) :: str
      integer,      intent(inout) :: i_arg
    end subroutine

    module subroutine get_integer ( ival, i_arg )
      integer, intent(inout) :: ival
      integer, intent(inout) :: i_arg
    end subroutine

    module subroutine get_real ( rval, i_arg )
      real,    intent(inout) :: rval
      integer, intent(inout) :: i_arg
    end subroutine

    module subroutine get_logical ( ok, i_arg )
      logical, intent(inout) :: ok
      integer, intent(inout) :: i_arg
    end subroutine

  end interface

contains

  subroutine get_args ( this, dir, date_fr, date_to ) 

    class(cli_ty), intent(inout)           :: this
    character(*),  intent(inout), optional :: dir, date_fr, date_to
    integer nargs

    call get_environment_variable ( 'HOME', this%home )
    call get_environment_variable ( 'PWD',   this%pdir )

    if ( .not. present( dir ) ) then
      dir = this%pdir
    end if

    this%datetime = get_datetime ()

    nargs = command_argument_count()

    if ( nargs == 0 ) call this%print_help

    i_arg = 1

    do while ( i_arg <= nargs )

      call get_command_argument ( i_arg, arg, status = status )

      select case ( adjustl(arg) )

        case ( '--dir' )
          call get_value ( dir, i_arg )

        case ( '--date_fr' )
          call get_value ( date_fr, i_arg )

        case ( '--date_to' )
          call get_value ( date_to, i_arg )

        case ( '--cdummy' ) ! To suppress -W-unused-fuctions 
          call get_value ( cdummy, i_arg )

        case ( '--idummy' ) ! To suppress -W-unused-fuctions 
          call get_value ( idummy, i_arg )

        case ( '--rdummy' ) ! To suppress -W-unused-fuctions
          call get_value ( rdummy, i_arg )

        case ( '--ldummy' ) ! To suppress -W-unused-fuctions
          call get_value ( ldummy, i_arg )

        case ( '-v', '--version' )
          call this%print_version ( this%datetime )

        case ( '-h', '--help' )
          call this%print_help

        case default
          print '(a, a, /)', 'Unrecognized command-line option: ', arg
          call this%print_help

      end select

      if ( status /= 0 ) print *, 'Error', status, 'on argument', i_arg

      i_arg = i_arg + 1

    end do

    print *, 'Program name: '//trim(this%exe)
    print *, 'Number of command arguments: ', command_argument_count() / 2
    print *, 'Home directory: '//trim(this%home)
    print *, 'Present working directory: '//trim(this%pdir)

  end subroutine get_args

  module subroutine get_character ( str, i_arg )

    character(*), intent(inout) :: str
    integer     , intent(inout) :: i_arg

    i_arg = i_arg + 1
    call get_command_argument ( i_arg, arg, status = status )
    if ( status /= 0 ) print *, 'Error', status, 'on argument', i_arg
    str = trim(arg)
    print '(a, i2, a)', 'Arg', i_arg / 2, ': '//trim(str)

  end subroutine

  module subroutine get_integer ( ival, i_arg )

    integer, intent(inout) :: ival
    integer, intent(inout) :: i_arg

    i_arg = i_arg + 1
    call get_command_argument ( i_arg, arg, status = status )
    if ( status /= 0 ) print *, '*** Error', status, 'on argument', i_arg
    read (arg, *) ival
    print '(a, i2, a, i0)', 'Arg', i_arg / 2, ': ', ival

  end subroutine

  module subroutine get_real ( rval, i_arg )

    real,    intent(inout) :: rval
    integer, intent(inout) :: i_arg

    i_arg = i_arg + 1
    call get_command_argument ( i_arg, arg, status = status )
    if ( status /= 0 ) print *, '*** Error', status, 'on argument', i_arg
    read (arg, *) rval
    print '(a, i2, a, g0)', 'Arg', i_arg / 2, ': ', rval

  end subroutine

  module subroutine get_logical ( ok, i_arg )

    logical, intent(inout) :: ok
    integer, intent(inout) :: i_arg

    i_arg = i_arg + 1
    call get_command_argument ( i_arg, arg, status = status )
    if ( status /= 0 ) print *, '*** Error', status, 'on argument', i_arg
    read ( arg, * ) ok
    print '(a, i2, a, l)', 'Arg', i_arg / 2, ': ', ok

  end subroutine

  function get_datetime () result ( datetime )

    character(19) :: datetime
    character(8)  :: date
    character(10) :: time
    character(5)  :: zone

    call date_and_time ( DATE = date, TIME = time, ZONE = zone )

    write ( datetime, '(a4, "-", a2, "-", a2, " ", a2, ":", a2)' ) &
      date(1:4), date(5:6), date(7:8), time(1:2), time(3:4)

  end function

  subroutine print_help ( this )

    class(cli_ty), intent(in) :: this
    integer j

    print '(a)', ''
    print '(a)', repeat('=', 80)
    print '(a)', trim(this%title)
    print '(a)', repeat('-', 80)

    do j = 1, this%n_usage
      print *, trim(this%usage(j))
    end do

    print '(a)', repeat('=', 80)

    stop

  end subroutine

  subroutine print_version ( this, datetime )

    class(cli_ty), intent(in) :: this
    character(*),  intent(in) :: datetime

    print '(a)', repeat('=', 80)
    print '(a)', 'Executable file: '//trim(this%exe)
    print '(a)', 'Version '//trim(this%version)
    print '(a)', 'Created by '// trim(this%author)
    print '(a)', trim(this%copyright)
    print '(a)', repeat('=', 80)
    print '(a)', trim(datetime)

    stop

  end subroutine

end module

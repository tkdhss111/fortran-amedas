#include "macro.fi"

module make_weather_db_mo

  use file_mo
  use dt_mo
  use spline_mo
  use logger_mo

  implicit none

  private
  public :: is_eq
  public :: logger, paste
  public :: dt_ty, strptime, seq_dt
  public :: site_ty, read_sites, extract_data
  public :: amedas_ty, download_amedas, impute_amedas, print_amedas, write_csv_amedas 
  public :: make_mean_weather
  public :: extract_1h_sharp
  public :: init_amedas_site, read_amedas_site, write_amedas_site

  type site_ty
    integer         :: IndexNbr  = iNA ! International weather station number
    integer         :: prec_no   = iNA
    character(15)   :: name      = 'NA'
    character(15)   :: kanji     = 'NA'
    integer         :: no        = iNA
    integer         :: groupCode = iNA
    real            :: lat       = NA
    real            :: lon       = NA
    integer         :: has_rj    = iNA
    logical         :: skip      = .false.
    character(1000) :: url       = 'NA'
  end type

  type, extends(site_ty) :: amedas_ty
    type(dt_ty)    :: HS ! Hour-start time stamp
    type(dt_ty)    :: HE ! Hour-end   time stamp
    real           :: hpa     = NA ! Air pressure
    real           :: hpa0    = NA ! Sea level pressure
    real           :: pr      = NA ! Precipitation
    real           :: tp      = NA ! Temperature
    real           :: hm      = NA ! Relative humidity N.B. Do not use integer
    real           :: hm1h    = NA ! 1-hour mean relative humidity N.B. Do not use integer
    real           :: ws      = NA ! Wind speed
    real           :: ws1h    = NA ! 1-hour mean wind speed
    character(3*3) :: wd      = '' ! Wind direction
    real           :: ws_max  = NA ! Maximun wind speed
    character(3*3) :: wd_max  = '' ! Wind direction at ws_max
    real           :: rm      = NA ! Sunshine duration in minutes N.B. Do not use integer
    real           :: rm1h    = NA ! 1-hour mean sunshine duration in minutes N.B. Do not use integer
    character(255) :: missing = '' ! List of typs of missing data 
    real           :: tp1h    = NA ! 1-hour mean temparature
    real           :: tp5h    = NA ! 5-hour mean temparature
    real           :: tp3d    = NA ! 3-day mean temparature
    real           :: tp7d    = NA ! 7-day mean temparature
  end type

  type(logger_ty) :: logger

  character(255) :: errmsg

contains

  subroutine init_amedas_site ( file )

    character(*), intent(in)     :: file
    type(dt_ty)                  :: t_fr, t_to
    type(dt_ty), allocatable     :: ts(:)
    type(amedas_ty), allocatable :: amedas_site(:)
    integer u, n

    __LOG__( 'S: init_amedas_site: '//trim(file) )

    !block
    !logical exist
    !inquire( file = file, exist = exist )
    !if ( exist ) then
    !  __LOG__( 'AMeDas file exists ... skipped binary file initialization.' )
    !  return
    !end if
    !end block

    t_fr = strptime ( '2008-06-25', '%Y-%m-%d' )
    t_to = strptime ( '2030-12-31', '%Y-%m-%d' )
    ts = seq_dt ( t_fr, t_to, '10 mins' )
    n = size(ts)

    allocate ( amedas_site(n) )

    amedas_site%HE = ts

    open ( newunit = u, file = file, form = 'unformatted', iostat = iostat, iomsg = iomsg )

    if ( iostat /= 0 ) then
      __ERROR__( iomsg )
      stop 1
    end if

    write ( u ) n
    write ( u ) amedas_site
    close ( u )

    __LOG__( 'E: init_amedas_site: '//trim(file) )

  end subroutine

  function read_amedas_site ( file ) result ( amedas_site )

    character(*), intent(in)     :: file
    type(amedas_ty), allocatable :: amedas_site(:)
    integer u, n

    __LOG__( 'S: read_amedas_site: '//trim(file) )

    open ( newunit = u, file = file, form = 'unformatted', iostat = iostat, iomsg = iomsg )

    if ( iostat /= 0 ) then
      __ERROR__( iomsg )
      stop 1
    end if

    read ( u ) n
    allocate ( amedas_site(n) )
    read ( u ) amedas_site
    close ( u )

    __LOG__( 'E: read_amedas_site: '//trim(file) )

  end function 

  subroutine write_amedas_site ( amedas_site, file )

    type(amedas_ty), intent(in) :: amedas_site(:)
    character(*),    intent(in) :: file
    integer u, n

    __LOG__( 'S: write_amedas_site: '//trim(file) )

    n = size(amedas_site)

    open ( newunit = u, file = file, form = 'unformatted', iostat = iostat, iomsg = iomsg )

    if ( iostat /= 0 ) then
      __ERROR__( iomsg )
      stop 1
    end if

    write ( u ) n
    write ( u ) amedas_site
    close ( u )

    __LOG__( 'E: write_amedas_site: '//trim(file) )

  end subroutine

  function read_sites ( csvfile ) result ( sites )

    character(*), intent(in)   :: csvfile
    type(site_ty), allocatable :: sites(:)
    type(tab_ty)               :: tab
    integer,       allocatable :: IndexNbrs(:)
    integer,       allocatable :: prec_nos(:)
    character(19), allocatable :: names(:)

    __LOG__( 'S: read_sites: '//trim(csvfile) )

    call tab%read_csvfile ( csvfile )

    allocate ( IndexNbrs(tab%nr) )
    allocate (  prec_nos(tab%nr) )
    allocate (     names(tab%nr) )

    call tab%get_col ( 'IndexNbr',  IndexNbrs )
    call tab%get_col ( 'prec_no',   prec_nos  )
    call tab%get_col ( 'name',      names     )

    allocate ( sites(tab%nr) )

    sites%IndexNbr = IndexNbrs
    sites%prec_no  = prec_nos
    sites%name     = names

    __LOG__( 'E: read_sites: '//trim(csvfile) )

  end function

  subroutine make_mean_weather ( amedas )

    ! N.B. Hour end (HE) values

    type(amedas_ty), intent(inout) :: amedas(:)
    integer n10mins

    __LOG__( 'S: make_mean_weather' )

    n10mins = 6 * 1      ! 1hour
    amedas(n10mins:)%ws1h = moving_ave ( amedas%ws, k = n10mins, step = 1 ) ! ws is 10min-mean
    amedas(n10mins:)%rm1h = moving_ave ( amedas%rm, k = n10mins, step = 1 ) * 6 ! rm is sunshine duration in minutes 0--10
    amedas(n10mins:)%hm1h = moving_ave ( amedas%hm, k = n10mins, step = 1 ) ! hm is spot value
    amedas(n10mins:)%tp1h = moving_ave ( amedas%tp, k = n10mins, step = 1 ) ! tp is spot value
    n10mins = 6 * 5      ! 5hours
    amedas(n10mins:)%tp5h = moving_ave ( amedas%tp, k = n10mins, step = 1 ) ! tp is spot value
    n10mins = 6 * 24 * 3 ! 3days
    amedas(n10mins:)%tp3d = moving_ave ( amedas%tp, k = n10mins, step = 1 ) ! tp is spot value
    n10mins = 6 * 24 * 7 ! 7days
    amedas(n10mins:)%tp7d = moving_ave ( amedas%tp, k = n10mins, step = 1 ) ! tp is spot value

    __LOG__( 'E: make_mean_weather' )

  end subroutine

  subroutine print_amedas ( amedas, date_fr, date_to )

    type(amedas_ty),        intent(in) :: amedas(:)
    character(*), optional, intent(in) :: date_fr, date_to
    character(19) :: date_fr_, date_to_
    integer i

    __LOG__( 'S: print_amedas' )

    if ( present( date_fr ) ) then
      date_fr_ = date_fr
    else
      date_fr_ = '2000-01-01'
    end if

    if ( present( date_to ) ) then
      date_to_ = date_to
    else
      date_to_ = '2099-01-01'
    end if

    __INFO__( '*** All spot weather are hour-end (HE) timestamp ***' )

    do i = 1, size(amedas)
      if ( amedas(i)%HS%date < date_fr_ .or. date_to_ < amedas(i)%HS%date ) then
        cycle
      end if
      print '(a19, "HS  "$)', amedas(i)%HS%datetime ! hour start time stamp
      print '(a19, "HE  "$)', amedas(i)%HE%datetime ! hour   end time stamp
      print '(a, f7.2$)', 'hpa:',     amedas(i)%hpa
      print '(a, f7.2$)', ' hpa0:',   amedas(i)%hpa0
      print '(a, f7.2$)', ' pr:',     amedas(i)%pr
      print '(a, f7.2$)', ' hm:',     amedas(i)%hm
      print '(a, f7.2$)', ' hm1h:',   amedas(i)%hm1h
      print '(a, f7.2$)', ' ws:',     amedas(i)%ws
      print '(a, f7.1$)', ' ws1h:',   amedas(i)%ws1h
      print '(a, f7.2$)', ' ws_max:', amedas(i)%ws_max
      !print '(a,    a$)', ' wd:',     trim(amedas(i)%wd)
      !print '(a,    a$)', ' wd_max:', trim(amedas(i)%wd_max)
      print '(a, f7.2$)', ' rm:',     amedas(i)%rm
      print '(a, f7.2$)', ' rm1h:',   amedas(i)%rm1h
      print '(a, f7.1$)', ' tp:',     amedas(i)%tp
      print '(a, f7.1$)', ' tp1h:',   amedas(i)%tp1h
      print '(a, f7.1$)', ' tp5h:',   amedas(i)%tp5h
      print '(a, f7.1$)', ' tp3d:',   amedas(i)%tp3d
      print '(a, f7.1$)', ' tp7d:',   amedas(i)%tp7d
      print '(a, a20  )', 'missing:', amedas(i)%missing
    end do

    __LOG__( 'E: print_amedas' )

  end subroutine

  subroutine write_csv_amedas ( amedas, file )

    type(amedas_ty), intent(in) :: amedas(:)
    character(*),    intent(in) :: file
    integer i, u

    __LOG__( 'S: write_csv_amedas: '//trim(file) )

    call logger%open ( __FILE__, __LINE__, newunit = u, file = file, status = 'replace' )

    write( u, '(a)' ) 'HS,HE,hpa,hpa0,pr,hm,ws,ws_max,rm,tp,wd,wd_max,missing'

    do i = 1, size(amedas)
      write( u, '( 2(a19, ","), 8(f7.2, ","), 2(a, ","), a20)' ) &
      amedas(i)%HS%datetime,  & ! hour start time stamp
      amedas(i)%HE%datetime,  & ! hour   end time stamp
      amedas(i)%hpa,          &
      amedas(i)%hpa0,         &
      amedas(i)%pr,           &
      amedas(i)%hm,           &
      amedas(i)%ws,           &
      amedas(i)%ws_max,       &
      amedas(i)%rm,           &
      amedas(i)%tp,           &
      trim(amedas(i)%wd),     &
      trim(amedas(i)%wd_max), &
      trim(adjustl(amedas(i)%missing))
    end do

    close( u )

    __LOG__( 'E: write_csv_amedas: '//trim(file) )

  end subroutine

  function extract_data ( HE, file ) result ( amedas )

    type(dt_ty),  intent(in)     :: HE
    character(*), intent(in)     :: file
    type(amedas_ty), allocatable :: amedas(:)
    character(2000), allocatable :: lines(:)
    character(80),   allocatable :: vals(:, :)
    character(19)                :: datetime 
    type(dt_ty)                  :: HE_fllw
    integer, parameter :: ncols   = 11 
    integer, parameter :: nlines  = 6*24
    integer u, i, j, k, p, dp, i_fr, i_to

    !__LOG__( 'S: extract_data' )

    call logger%open ( __FILE__, __LINE__, newunit = u, file = file, status = 'old' )

    allocate ( amedas(nlines) )
    allocate ( lines(0:nlines) )
    allocate ( vals(nlines, ncols) )

    vals  = ''
    lines = ''

    ! Skip heaer lines
    do i = 1, nlines
      read ( u, '(a)' ) lines(0)
      if ( index( lines(0), '最大瞬間' ) > 0 ) then
        exit
      end if
    end do

    k = 1
    do i = 1, nlines
      read( u, '(a)' ) lines(i)
      p = 1
      do j = 1, ncols
        dp = index(lines(i)(p:), '</td>')
        if ( dp == 0 ) then
          __ERROR__( paste( 'No </td> mark found on the line in HTML:', &
                            'i:', i, ', j:', j, ', p:', p, ', line:', trim(lines(i)(p:)) ) )
          stop
        end if
        i_to = p + dp - 2
        i_fr = index(lines(i)(:i_to), '>', back = .true. ) + 1
        if ( i_fr <= i_to ) then
          vals(k, j) = lines(i)(i_fr:i_to)
        else
          vals(k, j) = 'NA'
        end if
        p = p + dp
      end do
      k = k + 1
    end do

    close( u )

    do i = 1, nlines
      if ( vals(i, 1) == '24:00' ) then
        ! (date) 24:00:00 -> (date + 1 day) 00:00:00
        HE_fllw = HE
        amedas(i)%HE = HE_fllw%plus ( days = 1 )
      else
        write ( datetime, '(a10, " ", a5, ":00")' ) HE%date, trim(vals(i, 1))
        amedas(i)%HE = strptime ( datetime )
      end if
      amedas(i)%HS = amedas(i)%HE%minus ( hrs = 1 )
      amedas(i)%hpa    = read_dble ( vals(i, 2), is_na_zero = .false. ) 
      amedas(i)%hpa0   = read_dble ( vals(i, 3), is_na_zero = .false. ) 
      amedas(i)%pr     = read_dble ( vals(i, 4), is_na_zero = .true.  ) 
      amedas(i)%tp     = read_dble ( vals(i, 5), is_na_zero = .false. ) 
      amedas(i)%hm     = read_dble ( vals(i, 6), is_na_zero = .false. ) 
      amedas(i)%ws     = read_dble ( vals(i, 7), is_na_zero = .false. ) 
      amedas(i)%wd     = trim(vals(i, 8))
      amedas(i)%ws_max = read_dble ( vals(i, 9), is_na_zero = .false. )
      amedas(i)%wd_max = trim(vals(i, 10))
      amedas(i)%rm     = read_dble ( vals(i, 11), is_na_zero = .true. ) 
      amedas(i)%missing = '' 
    end do

    !__LOG__( 'E: extract_data' )
    
  end function

  function read_dble ( txt, is_na_zero ) result ( x )

    character(*), intent(in) :: txt
    logical,      intent(in) :: is_na_zero
    real :: x

    if ( is_numeric(txt) ) then
      ! N.B. intel compiler reads '--' as zero
      read ( txt, * ) x
    else
      if ( is_na_zero ) then
        x = 0.0 ! for precipitation and solar radiation
      else
        x = NA
      end if
    end if

  end function

  subroutine download_amedas ( outfile, dir, dur, prec_no, IndexNbr, year, month, day, istat )

    use ifport

    character(*),   intent(in)  :: dir, dur
    integer,        intent(in)  :: prec_no, IndexNbr, year, month, day
    character(255), intent(out) :: outfile 
    integer,        intent(out) :: istat
    character(1000)             :: url
    character(255) :: outdir
    logical exist
    integer byte
    integer, parameter :: kb = 1024

    !__LOG__( 'S: download_amedas' )

! Example mito: latest is yesterday
! https://www.data.jma.go.jp/obd/stats/etrn/view/10min_s1.php?prec_no=40&block_no=47629&year=2023&month=1&day=17&view=
    istat = 0
    url = 'https://www.data.jma.go.jp/obd/stats/etrn/view/'//trim(dur)//'_s1.php'
    write( url, '(a,   i2)' ) trim(url)//'?prec_no=',  prec_no
    write( url, '(a,   i5)' ) trim(url)//'&block_no=', IndexNbr
    write( url, '(a,   i4)' ) trim(url)//'&year=',     year
    write( url, '(a, i0.2)' ) trim(url)//'&month=',    month
    write( url, '(a, i0.2)' ) trim(url)//'&day=',      day
    write( outfile, &
      '("IndexNbr=", i5, "/year=", i4, "/month=", i0, "/day=", i0, ".html")' ) &
        IndexNbr, year, month, day

    outdir = trim(dir)//'/html/'//dirname(outfile)
    inquire( directory = outdir, exist = exist )

    if ( .not. exist ) then
      __EXEC__( 'mkdir -p '//trim(outdir) )
    end if

    inquire( file = trim(dir)//'/html/'//trim(outfile), exist = exist )

    if ( .not. exist ) then
      __EXEC__( 'mkdir -p '//trim(dir)//'/html' )
      __LOG__( 'curl "'//trim(url)//&
                  '" -o "'//trim(dir)//'/html/'//trim(outfile)//'"' )
      __EXEC__( 'curl "'//trim(url)//&
                  '" -o "'//trim(dir)//'/html/'//trim(outfile)//'"' )
      call sleep( 1 + int(3 * rand()) )
    else
      !__LOG__( trim(outfile)//' ... file exists (skipped downloading)' )
    end if

    inquire( file = trim(dir)//'/html/'//trim(outfile), size = byte )

    if ( byte < 50 * kb ) then
      __ERROR__( 'File size is too small check and remmove the file.' )
      print *, 'file: ', trim(dir)//'/html/'//trim(outfile)
      __EXEC__( 'cat "'//trim(dir)//'/html/'//trim(outfile)//'"' )
      __EXEC__( 'rm "'//trim(dir)//'/html/'//trim(outfile)//'"' )
      istat = 1
      return
    end if

    !__LOG__( 'E: download_amedas' )

  end subroutine

  subroutine impute_amedas ( amedas )

    type(amedas_ty), intent(inout) :: amedas(:)
    integer i

    __LOG__( 'S: impute_amedas' )

    where ( amedas%wd == '×' )
      amedas%wd = 'NA'
    end where

    where ( amedas%wd_max == '×' )
      amedas%wd_max = 'NA'
    end where

    do i = 1, size(amedas)
      if ( is_eq ( amedas(i)%hpa, NA ) ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'hpa;'
      end if
      if ( is_eq ( amedas(i)%hpa0, NA ) ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'hpa0;'
      end if
      if ( is_eq ( amedas(i)%pr, NA ) ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'pr;'
      end if
      if ( is_eq ( amedas(i)%tp, NA ) ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'tp;'
      end if
      if ( is_eq ( amedas(i)%hm, NA ) ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'hm;'
      end if
      if ( is_eq ( amedas(i)%ws, NA ) ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'ws;'
      end if
      if ( is_eq ( amedas(i)%ws_max, NA ) ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'ws_max;'
      end if
      if ( is_eq ( amedas(i)%rm, NA ) ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'rm;'
      end if
      if ( amedas(i)%wd == 'NA' ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'wd;'
      end if
      if ( amedas(i)%wd_max == 'NA' ) then
        amedas(i)%missing = trim(amedas(i)%missing)//'wd_max;'
      end if
    end do

    ! N.B. AMeDas data is 10-min value
    ! NA's in 3 hours (3 * 6 * 10mins) are to be interploted.
    ! Longer NA streak must be processed accordingly (e.g. to be replaced by Tokyo weather)

    call interpolate_linear ( amedas%hpa,    max_nnas = 18 )
    call interpolate_linear ( amedas%hpa0,   max_nnas = 18 )
    call interpolate_linear ( amedas%pr,     max_nnas = 18 )
    call interpolate_linear ( amedas%tp,     max_nnas = 18 )
    call interpolate_linear ( amedas%hm,     max_nnas = 18 )
    call interpolate_linear ( amedas%ws,     max_nnas = 18 )
    call interpolate_linear ( amedas%ws_max, max_nnas = 18 )
    call interpolate_linear ( amedas%rm,     max_nnas = 18 )
    call taketa_sandwich    ( amedas%wd )
    call taketa_sandwich    ( amedas%wd_max )

    __LOG__( 'E: impute_amedas' )

  end subroutine impute_amedas

  function extract_1h_sharp ( amedas ) result ( amedas1h )

    type(amedas_ty), intent(inout) :: amedas(:)
    type(amedas_ty), allocatable   :: amedas1h(:)
    integer,         allocatable   :: ii(:), ii_1h(:)
    integer i

    ii = [( i, i = 1, size(amedas) )] ! Use indexing to avoid segmentation error
    ii_1h = pack( ii, amedas%HS%datetime(15:16) == '00' .and. .not. is_eq ( amedas%tp7d, NA ) )
    amedas1h = amedas(ii_1h)

  end function

end module

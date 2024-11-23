! Note. Today's data are not available!
! Today's weather information at Tokyo (anti-webscraping format)
! URL: https://www.jma.go.jp/bosai/amedas/#area_type=offices&area_code=130000&amdno=44132&format=table1h&elems=53614
#include "macro.fi"

program main

  use cli_mo
  use make_weather_db_mo
  use dt_mo

  implicit none

  type(dt_ty)              :: HE     ! Now
  type(dt_ty)              :: HE_fr  ! Start date
  type(dt_ty)              :: HE_to  ! End date
  type(dt_ty), allocatable :: HEs(:) ! Datetimes
  integer                  :: nHEs   ! Number of datetimes

  ! Command arguments
  type(cli_ty)   :: cli     ! Command line interface
  character(255) :: dir     ! Working directory
  character(10)  :: date_fr ! Start date
  character(10)  :: date_to ! End date
  integer        :: i = 1

  ! URL
  character(255) :: outfile
  type(amedas_ty), allocatable :: amedas(:), amedas_site(:), amedas_site_1h(:)
  type(site_ty),   allocatable :: sites(:)
  character(255)               :: file_site, file_site_1h
  character(255)               :: csvfile_site, csvfile_site_1h
  integer i_d, i_s, nsites, istat

  cli%title     = 'Server Program for AMeDas till Yesterday'
  cli%exe       = 'fortran-amedas'
  cli%author    = 'Hisashi Takeda, Ph.D.'
  cli%copyright = '2020 Copyright(C) All Rights Reserved.'
  cli%version   = '1.0'
  cli%usage(i)  = '==================================================';i=i+1
  cli%usage(i)  = 'Usage: '//trim(cli%exe)//' [OPTIONS]'              ;i=i+1
  cli%usage(i)  = ''                                                  ;i=i+1
  cli%usage(i)  = 'N.B. ALL MEAN WEATHER ARE HOUR-END TIMESTAMP'      ;i=i+1
  cli%usage(i)  = ''                                                  ;i=i+1
  cli%usage(i)  = ''                                                  ;i=i+1
  cli%usage(i)  = 'Example: '//trim(cli%exe)//' --dir ./'             ;i=i+1
  cli%usage(i)  = '                             --date_fr 2010-06-01' ;i=i+1
  cli%usage(i)  = '                             --date_to 2020-12-31' ;i=i+1
  cli%usage(i)  = ''                                                  ;i=i+1
  cli%usage(i)  = 'Program options:'                                  ;i=i+1
  cli%usage(i)  = '  --dir     followed by path of working directory' ;i=i+1
  cli%usage(i)  = '  --date_fr followed by start date as yyyy-mm-dd'  ;i=i+1
  cli%usage(i)  = '  --date_to followed by   end date as yyyy-mm-dd'  ;i=i+1
  cli%usage(i)  = ''                                                  ;i=i+1
  cli%usage(i)  = '  -v, --version print version information and exit';i=i+1
  cli%usage(i)  = '  -h, --help    print usage information and exit'  ;i=i+1
  cli%usage(i)  = '==================================================';i=i+1
  cli%n_usage   = i-1

  ! Default values
  HE = HE%now()
  HE = HE%minus ( days = 1 )

 ! N.B. 10-min data are stored from 2008-06-25 and clean data starts from 2008-07-02; 7 days later.
  date_fr = '2008-06-25' ! Epoch date
  date_to = HE%date      ! Yesterday is the latest

  call cli%get_args ( dir, date_fr, date_to )

  call logger%init ( file  = trim(dir)//'/amedas.log', &
                     app   = trim(cli%exe), &
                     email = 'dsbiztiu@gmail.com' )

  if ( this_image() == 1 ) then
    print *, repeat('=', 80)
    print *, '      dir: ', trim(dir)
    print *, ' date_fr: ', date_fr
    print *, ' date_to: ', date_to
    print *, repeat('=', 80)
  end if

  HE_fr = strptime ( date_fr, '%Y-%m-%d' )
  HE_to = strptime ( date_to, '%Y-%m-%d' )
  HEs   = seq_dt ( HE_fr, HE_to, '1 day' )
  nHEs  = size(HEs)

  sites = read_sites ( csvfile = trim(dir)//'/kansho_tepco.csv' )

  nsites = size(sites)

  !loop_site_i_s : do i_s = 1, nsites

  i_s = this_image()

    __LOG__( repeat( '=', 50 ) )

    file_site       = trim(dir)//'/amedas_'//trim(sites(i_s)%name)//'.bin'
    file_site_1h    = trim(dir)//'/amedas_'//trim(sites(i_s)%name)//'_1h.bin'
    csvfile_site    = trim(dir)//'/csv/amedas_'//trim(sites(i_s)%name)//'.csv'
    csvfile_site_1h = trim(dir)//'/csv/amedas_'//trim(sites(i_s)%name)//'_1h.csv'

    call init_amedas_site ( file = trim(dir)//'/amedas_'//trim(sites(i_s)%name)//'.bin' )

    amedas_site = read_amedas_site ( file = file_site )

    do i_d = 1, nHEs

      call download_amedas ( outfile,   &
        dir      = dir,                 &
        dur      = '10min',             &
        prec_no  = sites(i_s)%prec_no,  &
        IndexNbr = sites(i_s)%IndexNbr, &
        year     = HEs(i_d)%yr,         &
        month    = HEs(i_d)%mo,         &
        day      = HEs(i_d)%dy,         &
        istat    = istat )

      if ( istat /= 0 ) cycle

      amedas = extract_data ( HEs(i_d), file = trim(dir)//'/html/'//trim(outfile) )

      if ( is_eq ( amedas(1)%tp, NA ) ) then
        __EXEC__( 'rm "'//trim(dir)//'/html/'//trim(outfile)//'"' )
        __LOG__( 'Downloaded HTML file has been removed since temperature is NA.' )
        __LOG__( 'Probably, data are not uploaded in JMA HP.'//& 
                 'Pleser reconsider the time to fetch HTML file in service.timer.' )
      end if

      amedas_site(6*24*(i_d-1) + 1 : 6*24*(i_d-1) + size(amedas)) = amedas 

    end do

    call impute_amedas ( amedas_site )

    !call make_mean_weather ( amedas_site )

    amedas_site_1h = extract_1h_sharp ( amedas_site )

    call write_amedas_site ( amedas_site,    file = file_site    )
    call write_amedas_site ( amedas_site_1h, file = file_site_1h )

    amedas_site    = pack( amedas_site,    amedas_site%HE%date    < HE.date )
    amedas_site_1h = pack( amedas_site_1h, amedas_site_1h%HE%date < HE.date )

    call write_csv_amedas ( amedas_site,    file = csvfile_site    )
    call write_csv_amedas ( amedas_site_1h, file = csvfile_site_1h )

    call print_amedas ( amedas_site, date_fr = HE%date, date_to = HE%date )

  !end do loop_site_i_s

end program

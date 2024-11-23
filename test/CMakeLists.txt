set ( EXE unit_test )
set ( ARGS
  --dir "/mnt/DATA/amedas"
  #--date_fr "2010-06-01"
  #--date_to "2022-12-31"
  )
set ( SRCS 
  ~/1_projects/utils/file/src/file_mo.f90
  ~/1_projects/utils/00_template_ifx/src/spline_mo.f90
  ~/2_tools/fortran-logger/src/logger_mo.f90
  ~/2_tools/fortran-datetime/src/dt_mo.f90
  ../app/cli_mo.f90
  ../app/main.f90
  ../src/make_weather_db_mo.f90
  )
set ( MKLROOT /opt/intel/oneapi/mkl/latest )
set ( LIBS
  ${MKLROOT}/lib/libmkl_blas95_ilp64.a 
  ${MKLROOT}/lib/libmkl_lapack95_ilp64.a -Wl,--start-group 
  ${MKLROOT}/lib/libmkl_intel_ilp64.a
  ${MKLROOT}/lib/libmkl_sequential.a
  ${MKLROOT}/lib/libmkl_core.a -Wl,--end-group -lpthread -lm -ldl
)
set ( CMAKE_Fortran_FLAGS 
  "-I${MKLROOT}/include/mkl/intel64/ilp64 -i4 -r8 -no-wrap-margin -I${MKLROOT}/include -qmkl -lsqlite3" )

if ( CMAKE_BUILD_TYPE MATCHES "Debug" )
  set ( CMAKE_Fortran_FLAGS_DEBUG
    "-coarray=shared -coarray-num-images=9 -g -O0 -warn all -check bounds -traceback -Ddebug ${CMAKE_Fortran_FLAGS}" )
endif()

add_executable ( ${EXE} ${SRCS} )
target_link_libraries ( ${EXE} ${LIBS} )
target_include_directories ( ${EXE} PUBLIC "../include" )
add_test ( NAME ${EXE} COMMAND ${EXE} ${ARGS} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

cmake_minimum_required ( VERSION 3.21 )
set ( CMAKE_Fortran_COMPILER ifx )
set ( CMAKE_C_COMPILER icx )
project ( hello LANGUAGES Fortran VERSION 1.0.0 )
enable_language ( Fortran C )
enable_testing ()

if (UNIX)
  set ( MYHOME /home/hss )
endif (UNIX)
if (WIN32)
  set ( MYHOME C:/Users/hss/0_tkd/1_hss )
endif (WIN32)

add_subdirectory ( app )
add_subdirectory ( test )

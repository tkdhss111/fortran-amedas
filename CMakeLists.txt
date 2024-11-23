cmake_minimum_required ( VERSION 3.21 )
set ( CMAKE_Fortran_COMPILER ifx )
set ( CMAKE_C_COMPILER icx )
enable_language ( Fortran C )
enable_testing ()
project ( hello LANGUAGES Fortran VERSION 1.0.0 )

add_subdirectory ( app )
add_subdirectory ( test )

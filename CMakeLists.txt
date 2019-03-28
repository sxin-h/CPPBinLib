cmake_minimum_required(VERSION 3.1...3.14)

if(${CMAKE_VERSION} VERSION_LESS 3.13)
	cmake_policy(VERSION ${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION})
else()
	cmake_policy(VERSION 3.14)
endif()

project(CPPBinLib VERSION 1.0
	              DESCRIPTION "A C++ wrapper for reading ANSYS result files."
	              LANGUAGES Fortran CXX)


set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Create some cache variables for the various ANSYS
# libraries and files
set(ANSYS_BINLIB "C:/Program Files/ANSYS Inc/v192/ansys/custom/misc/win64/binlib.lib" 
	CACHE FILEPATH "Path the the ANSYS BinLib library file")
set(ANSYS_BINLIB_DLL "C:/Program Files/ANSYS Inc/v192/ansys/custom/misc/win64/binlib.dll"
	CACHE FILEPATH "Path to the ANSYS BinLib library dll")
set(ANSYS_RESRD_FORTRAN "C:/Program Files/ANSYS Inc/v192/ansys/customize/user/ResRd.F"
	CACHE FILEPATH "Path to the ANSYS ResRd.F fortran file")
set(ANSYS_INCLUDE_PATH "C:/Program Files/ANSYS Inc/v192/ansys/customize/include"
	CACHE PATH "Path to the ANSYS customization include directory")


option(PACKAGE_TESTS "Build the tests" ON)
if(PACKAGE_TESTS)
    enable_testing()
    add_subdirectory(test)
endif()

# Packages
find_package(Eigen3 REQUIRED)
if(NOT EIGEN3_FOUND)
	message(FATAL_ERROR "Could not find the eigen libraries...")
endif()

find_package(Boost REQUIRED)

add_subdirectory(src)

#===============================================================================
# C to Fortran Shim Library
#===============================================================================

set(CBINLIB_SOURCES ${ANSYS_RESRD_FORTRAN} CBinLibShim.f90)

include_directories(${ANSYS_INCLUDE_PATH})

add_definitions(-D__EFL -DNOSTDCALL -DARGTRAIL)
add_definitions(-DPCWINNT_SYS -DPCWIN64_SYS)
add_definitions(-DPCWINX64_SYS -DCADOE_ANSYS -DFORTRAN)

add_library(cbinlib STATIC ${CBINLIB_SOURCES})
target_link_libraries(cbinlib ${ANSYS_BINLIB})
set_target_properties(cbinlib PROPERTIES AUTOMOC FALSE)



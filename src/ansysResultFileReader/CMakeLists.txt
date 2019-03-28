#===============================================================================
#  Bring in the necessary include directories
#===============================================================================
include_directories(${Boost_INCLUDE_DIR})
include_directories(${EIGEN3_INCLUDE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/include/padt/cppbinlib)

add_definitions(-D_SCL_SECURE_NO_WARNINGS -DBOOST_ALL_DYN_LINK)

set(ANSYS_RST_SOURCES
    AnsysRSTReader.cpp
    )

set(ANSYS_RST_HEADERS
    ${CMAKE_SOURCE_DIR}/include/padt/cppbinlib/AnsysRSTReader.h
    ${CMAKE_SOURCE_DIR}/include/padt/cppbinlib/CBinLib.h
    )


#===============================================================================
# Group the header files in the IDE
#===============================================================================
source_group("ANSYS RST Reader Header Files" FILES ${ANSYS_RST_HEADERS})
source_group("ANSYS RST Reader Source Files" FILES ${ANSYS_RST_SOURCES})

#===============================================================================
# Add the library to the build
#===============================================================================
add_library(rstreader STATIC ${ANSYS_RST_SOURCES} 
                             ${ANSYS_RST_HEADERS})

target_link_libraries(rstreader cbinlib)
set_target_properties(rstreader PROPERTIES LINKER_LANGUAGE CXX)
set_target_properties(rstreader PROPERTIES AUTOMOC FALSE)


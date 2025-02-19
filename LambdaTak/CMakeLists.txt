cmake_minimum_required(VERSION 3.10)
project(LambdaTak VERSION 1.0)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED True)

# Add the main executable
add_executable(lambdaTak src/main.c src/board.c src/tps.c src/moves.c)

# Manually find CUnit on macOS (Homebrew)
if(APPLE)
    set(CUNIT_INCLUDE_DIR /opt/homebrew/include)
    set(CUNIT_LIBRARY /opt/homebrew/lib/libcunit.dylib)
else()
    find_library(CUNIT_LIBRARY NAMES cunit libcunit PATHS /usr/lib /usr/local/lib)
    find_path(CUNIT_INCLUDE_DIR NAMES CUnit/CUnit.h PATHS /usr/include /usr/local/include)
endif()

if(CUNIT_LIBRARY AND CUNIT_INCLUDE_DIR)
    message(STATUS "Found CUnit: ${CUNIT_LIBRARY}, ${CUNIT_INCLUDE_DIR}")
    add_executable(testSuite tests/testMain.c tests/testBoard.c src/board.c src/tps.c tests/testTPS.c tests/testMove.c src/moves.c)
    target_include_directories(testSuite PRIVATE ${CUNIT_INCLUDE_DIR})
    target_link_libraries(testSuite PRIVATE ${CUNIT_LIBRARY})
    enable_testing()
    add_test(NAME BoardTests COMMAND testSuite)
else()
    message(FATAL_ERROR "CUnit not found. Install CUnit and set CUNIT_LIBRARY and CUNIT_INCLUDE_DIR manually.")
endif()


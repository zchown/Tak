cmake_minimum_required(VERSION 3.10)
project(LambdaTak VERSION 1.0)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED True)

# Set compiler optimization flags for different build types
set(CMAKE_C_FLAGS_DEBUG "-g -O0 -fopenmp -Wall -Wextra -Wpedantic -Werror -Wno-unused-parameter -Wno-unused-variable -Wno-unused-function -Wno-unused-but-set-variable -Wno-unused-value -Wno-unused-result -Wno-unused-label -Wno-unused-local-typedefs -Wno-unused-const-variable -Wno-unused-macros")
set(CMAKE_C_FLAGS_RELEASE "-O3 -DNDEBUG -march=native -mtune=native -funroll-loops -fprefetch-loop-arrays -fomit-frame-pointer -finline-functions -finline-limit=1000 -flto -fwhole-program") 

# If no build type is specified, default to Debug
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build: Debug or Release" FORCE)
endif()

# Add the main executable
add_executable(lambdaTak src/main.c src/board.c src/tps.c src/moves.c src/perft.c src/ptn.c)

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
    add_executable(testSuite tests/testMain.c tests/testBoard.c src/board.c src/tps.c tests/testTPS.c tests/testMove.c src/moves.c src/perft.c src/ptn.c tests/testGeneral.c)
    target_include_directories(testSuite PRIVATE ${CUNIT_INCLUDE_DIR})
    target_link_libraries(testSuite PRIVATE ${CUNIT_LIBRARY})
    enable_testing()
    add_test(NAME BoardTests COMMAND testSuite)
else()
    message(FATAL_ERROR "CUnit not found. Install CUnit and set CUNIT_LIBRARY and CUNIT_INCLUDE_DIR manually.")
endif()


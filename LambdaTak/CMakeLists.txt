cmake_minimum_required(VERSION 3.10)
project(LambdaTak VERSION 1.0)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED True)

# Set compiler optimization flags for different build types
set(CMAKE_C_FLAGS_DEBUG "-g -O0")
set(CMAKE_C_FLAGS_RELEASE "-O3 -DNDEBUG -march=native -mtune=native -funroll-loops -fprefetch-loop-arrays -fomit-frame-pointer -finline-functions -finline-limit=1000 -flto -fwhole-program") 

# If no build type is specified, default to Debug
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build: Debug or Release" FORCE)
endif()

# Find WebSocket and JSON libraries
find_library(WEBSOCKETS_LIBRARY NAMES websockets libwebsockets)
find_path(WEBSOCKETS_INCLUDE_DIR NAMES libwebsockets.h)
find_library(JANSSON_LIBRARY NAMES jansson)
find_path(JANSSON_INCLUDE_DIR NAMES jansson.h)

# Handle macOS Homebrew installations
if(APPLE)
    list(APPEND WEBSOCKETS_INCLUDE_DIR /opt/homebrew/include)
    list(APPEND WEBSOCKETS_LIBRARY /opt/homebrew/lib/libwebsockets.dylib)
    list(APPEND JANSSON_INCLUDE_DIR /opt/homebrew/include)
    list(APPEND JANSSON_LIBRARY /opt/homebrew/lib/libjansson.dylib)
endif()

if(WEBSOCKETS_LIBRARY AND WEBSOCKETS_INCLUDE_DIR AND JANSSON_LIBRARY AND JANSSON_INCLUDE_DIR)
    message(STATUS "Found libwebsockets: ${WEBSOCKETS_LIBRARY}")
    message(STATUS "Found jansson: ${JANSSON_LIBRARY}")
else()
    message(FATAL_ERROR "libwebsockets or jansson not found. Install using:\n  sudo apt-get install libwebsockets-dev libjansson-dev\nOr on macOS: brew install libwebsockets jansson")
endif()

# Add main executable
add_executable(lambdaTak 
    src/main.c 
    lib/board.c 
    lib/tps.c 
    lib/moves.c 
    lib/perft.c 
    lib/ptn.c 
    ai/eval.c 
    ai/searches.c
    src/aiPlayer.c
    lib/zobrist.c
    ai/monteCarlo.c
    ai/qlearner.c
    ai/utils.c
    ai/trainer.c
)

target_include_directories(lambdaTak PRIVATE
    ${WEBSOCKETS_INCLUDE_DIR}
    ${JANSSON_INCLUDE_DIR}
)
target_link_libraries(lambdaTak
    ${WEBSOCKETS_LIBRARY}
    ${JANSSON_LIBRARY}
)

# training executable
add_executable(trainTak
    ai/runTraining.c
    lib/board.c 
    lib/tps.c 
    lib/moves.c 
    lib/perft.c 
    lib/ptn.c 
    ai/eval.c 
    ai/searches.c
    lib/zobrist.c
    ai/qlearner.c
    ai/utils.c
    ai/trainer.c
    ai/monteCarlo.c
)

# Test suite configuration
# Manually find CUnit on macOS (Homebrew)
if(APPLE)
    set(CUNIT_INCLUDE_DIR /opt/homebrew/include)
    set(CUNIT_LIBRARY /opt/homebrew/lib/libcunit.dylib)
else()
    find_library(CUNIT_LIBRARY NAMES cunit libcunit)
    find_path(CUNIT_INCLUDE_DIR NAMES CUnit/CUnit.h)
endif()

if(CUNIT_LIBRARY AND CUNIT_INCLUDE_DIR)
    message(STATUS "Found CUnit: ${CUNIT_LIBRARY}, ${CUNIT_INCLUDE_DIR}")
    add_executable(testSuite 
        tests/testMain.c 
        tests/testBoard.c 
        lib/board.c 
        lib/tps.c 
        tests/testTPS.c 
        tests/testMove.c 
        lib/moves.c 
        lib/perft.c 
        lib/ptn.c 
        tests/testGeneral.c
        lib/zobrist.c)
    target_include_directories(testSuite PRIVATE ${CUNIT_INCLUDE_DIR})
    target_link_libraries(testSuite PRIVATE ${CUNIT_LIBRARY})
    enable_testing()
    add_test(NAME BoardTests COMMAND testSuite)
else()
    message(FATAL_ERROR "CUnit not found. Install CUnit and set CUNIT_LIBRARY and CUNIT_INCLUDE_DIR manually.")
endif()

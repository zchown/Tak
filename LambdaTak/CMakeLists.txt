cmake_minimum_required(VERSION 3.10)
project(LambdaTak VERSION 1.0)

cmake_policy(SET CMP0074 NEW)
set(OpenMP_ROOT "/opt/homebrew/opt/libomp")

# Find OpenMP before other dependencies
find_package(OpenMP REQUIRED)

if(APPLE)
    # Apple Silicon optimizations
    set(CMAKE_C_FLAGS_RELEASE "-O3 -mcpu=apple-m1 -mtune=native -ffast-math -flto")
    set(CMAKE_C_FLAGS_DEBUG "-O3 -g -mcpu=apple-m1 -mtune=native -ffast-math -flto -fno-omit-frame-pointer -fno-optimize-sibling-calls -fsanitize=undefined -fsanitize=address")

    # Accelerate Framework
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -framework Accelerate")

    # Explicit OpenMP paths
    set(OPENMP_INCLUDE_DIR "${OpenMP_ROOT}/include")
    set(OPENMP_LIB_DIR "${OpenMP_ROOT}/lib")
    include_directories(${OPENMP_INCLUDE_DIR})
    link_directories(${OPENMP_LIB_DIR})
endif()

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

# WebSocket/JSON Configuration
find_library(WEBSOCKETS_LIBRARY NAMES websockets
    PATHS /opt/homebrew/lib
    REQUIRED)
find_path(WEBSOCKETS_INCLUDE_DIR NAMES libwebsockets.h
    PATHS /opt/homebrew/include)

find_library(JANSSON_LIBRARY NAMES jansson
    PATHS /opt/homebrew/lib
    REQUIRED)
find_path(JANSSON_INCLUDE_DIR NAMES jansson.h
    PATHS /opt/homebrew/include)

# Main executable
add_executable(lambdaTak 
    src/main.c 
    lib/board.c 
    lib/tps.c 
    lib/moves.c 
    lib/perft.c 
    lib/ptn.c 
    ai/eval.c 
    ai/transposition.c
    ai/searches.c
    src/aiPlayer.c
    ai/pythonTrainer.c
    lib/zobrist.c
    ai/monteCarloGraphSearch.c
    ai/neuralNetworks.c
    ai/neuralNetTrainer.c
    ai/accelerateNeuralNet.c
)

target_include_directories(lambdaTak PRIVATE
    ${WEBSOCKETS_INCLUDE_DIR}
    ${JANSSON_INCLUDE_DIR}
)

target_link_libraries(lambdaTak PRIVATE
    ${WEBSOCKETS_LIBRARY}
    ${JANSSON_LIBRARY}
    OpenMP::OpenMP_C
    omp
)


# training executable
add_executable(trainTak
    ai/runTraining.c
    ai/pythonTrainer.c
    lib/board.c 
    lib/tps.c 
    lib/moves.c 
    lib/perft.c 
    lib/ptn.c 
    ai/eval.c 
    ai/transposition.c
    ai/searches.c
    lib/zobrist.c
    ai/monteCarloGraphSearch.c
    ai/neuralNetworks.c
    ai/neuralNetTrainer.c
    ai/accelerateNeuralNet.c
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

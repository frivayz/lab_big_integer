cmake_minimum_required(VERSION 3.14)
project(BigIntegerLab LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# ──────────────────────────────────────────────
#  Google Test (fetched automatically)
# ──────────────────────────────────────────────
include(FetchContent)
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG        v1.14.0
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

# ──────────────────────────────────────────────
#  BigInteger library
# ──────────────────────────────────────────────
add_library(big_integer STATIC big_integer.cpp)
target_include_directories(big_integer PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Strict flags apply ONLY to the bigint code, not to GoogleTest
target_compile_options(big_integer PRIVATE -Wall -Wextra -Wpedantic -Werror)
target_compile_options(big_integer PRIVATE -fsanitize=address,undefined -fno-sanitize-recover=all)
target_link_options(big_integer PUBLIC -fsanitize=address,undefined)

# ──────────────────────────────────────────────
#  Tests
# ──────────────────────────────────────────────
enable_testing()

add_executable(big_integer_tests tests.cpp)
target_link_libraries(big_integer_tests PRIVATE big_integer GTest::gtest_main)

include(GoogleTest)
gtest_discover_tests(big_integer_tests)

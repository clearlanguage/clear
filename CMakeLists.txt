cmake_minimum_required(VERSION 3.20.0)
include(FetchContent)

project(clear)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

set(CLEAR_INCLUDE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/Source")

file(GLOB_RECURSE MY_SOURCES CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/Source/*.cpp")
add_executable(clear "${MY_SOURCES}"
        Source/Compilation/CompilationManager.cpp)

find_package(LLVM REQUIRED CONFIG)
find_package(Python3 REQUIRED)

add_custom_command(TARGET clear PRE_BUILD 
                   COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/Scripts/Build.py
                   VERBATIM
)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

llvm_map_components_to_libnames(llvm_libs
    support
    core
    irreader
    executionengine
    mcjit
    target
    analysis
    codegen
    x86codegen     
    x86asmparser
    AArch64CodeGen 
    AArch64AsmParser
    Linker
    Passes
)

FetchContent_Declare(
  fast_float
  GIT_REPOSITORY https://github.com/fastfloat/fast_float.git
  GIT_TAG tags/v6.1.6
  GIT_SHALLOW TRUE)

FetchContent_Declare(
  tomlplusplus
  GIT_REPOSITORY https://github.com/marzer/tomlplusplus.git
  GIT_TAG v3.4.0
)

FetchContent_MakeAvailable(fast_float tomlplusplus)

include_directories(${LLVM_INCLUDE_DIRS} ${CLEAR_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
target_link_libraries(clear PUBLIC ${llvm_libs} fast_float tomlplusplus::tomlplusplus)

link_directories(${LLVM_LIBRARY_DIRS})

add_compile_definitions("$<$<NOT:$<CONFIG:Debug>>:NDEBUG>")

cmake_minimum_required(VERSION 3.20.0)
# Enable Hot Reload for MSVC compilers if supported.
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

project(alkhat)

set(CMAKE_CXX_STANDARD 20)

set(LLVM_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ThirdParty/llvm-project/llvm/cmake/modules")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

message(STATUS "Using include dirs in: ${LLVM_INCLUDE_DIR}")

include_directories(${LLVM_INCLUDE_DIR})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

file(GLOB_RECURSE MY_SOURCES CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/Source/*.cpp")
add_executable(alkhat "${MY_SOURCES}")

llvm_map_components_to_libnames(
        llvm_libs
        ${LLVM_TARGETS_TO_BUILD}
        orcjit
        support
        core
        irreader
        codegen
        mc
        mcparser
        option)

target_link_libraries(alkhat ${llvm_libs})


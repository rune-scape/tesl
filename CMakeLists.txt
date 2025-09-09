cmake_minimum_required(VERSION 3.16)

set(TESL_PICO OFF CACHE BOOL "target pico-sdk")

if(TESL_PICO)
    include(pico_sdk_import.cmake)
endif()

if (NOT TESL_PICO)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fdiagnostics-color")
endif()

set(CMAKE_CXX_FLAGS_RELEASE_INIT "-ffile-prefix-map=${CMAKE_SOURCE_DIR}/=")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti -fno-exceptions")
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 11)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(tesl C CXX)

if(TESL_PICO)
    if(PICO_SDK_VERSION_STRING VERSION_LESS "2.1.0")
        message(FATAL_ERROR "Raspberry Pi Pico SDK version 2.1.0 (or later) required. Your version is ${PICO_SDK_VERSION_STRING}")
    endif()

    if(NOT DEFINED PICO_STDIO_USB_CONNECT_WAIT_TIMEOUT_MS)
        set(PICO_STDIO_USB_CONNECT_WAIT_TIMEOUT_MS 3000)
    endif()
    pico_sdk_init()
endif()

if(MSVC)
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif()

add_subdirectory(fmt)
target_compile_definitions(fmt PUBLIC FMT_USE_LOCALE=0 FMT_OPTIMIZE_SIZE=2 FMT_BUILTIN_TYPES=0 FMT_USE_EXCEPTIONS=0 FMT_REDUCE_INT_INSTANTIATIONS=1 FMT_USE_WRITE_CONSOLE FMT_USE_STD_STRING=1 FMT_USE_FILE_PRINT_BUFFER=0)

add_subdirectory(intrusive_shared_ptr)

add_subdirectory(src)

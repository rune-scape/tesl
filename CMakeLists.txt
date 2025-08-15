cmake_minimum_required(VERSION 3.16)

set(TESL_PICO OFF CACHE BOOL "target pico-sdk")

if (TESL_PICO)
    include(pico_sdk_import.cmake)
endif ()

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_C_STANDARD 11)

project(tesl C CXX)

if (TESL_PICO)
    if (PICO_SDK_VERSION_STRING VERSION_LESS "2.1.0")
        message(FATAL_ERROR "Raspberry Pi Pico SDK version 2.1.0 (or later) required. Your version is ${PICO_SDK_VERSION_STRING}")
    endif()

    if (NOT DEFINED PICO_STDIO_USB_CONNECT_WAIT_TIMEOUT_MS)
        set(PICO_STDIO_USB_CONNECT_WAIT_TIMEOUT_MS 3000)
    endif()
    pico_sdk_init()
endif ()

if (MSVC)
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif ()

add_executable(tesl
    main.cpp
    tesl_common.hpp tesl_common.cpp
    tesl_math.hpp tesl_math.cpp
    tesl_str.hpp tesl_str.cpp
    tesl_fn.hpp tesl_fn.cpp
    tesl_array.hpp
    tesl_dynamic_array.hpp tesl_dynamic_array.cpp
    tesl_map.hpp
    tesl_dynamic_map.hpp tesl_dynamic_map.cpp
    tesl_var.hpp tesl_var.cpp
    tesl_ref.hpp
    tesl_type.hpp tesl_type.cpp
    tesl_hash.hpp tesl_hash.cpp
    #tesl_symbol.hpp tesl_symbol.cpp
    tesl_tokenizer.hpp tesl_tokenizer.cpp
    tesl_parser.hpp tesl_parser.cpp
    #tesl_compiler.hpp tesl_compiler.cpp
)

add_subdirectory(fmt)
target_compile_definitions(fmt PUBLIC FMT_USE_LOCALE=0 FMT_OPTIMIZE_SIZE=2 FMT_BUILTIN_TYPES=0 FMT_USE_EXCEPTIONS=0 FMT_REDUCE_INT_INSTANTIATIONS=1 FMT_USE_WRITE_CONSOLE FMT_USE_STD_STRING=1 FMT_USE_FILE_PRINT_BUFFER=0)
target_compile_options(fmt PRIVATE -fno-rtti -fno-exceptions)
target_link_options(fmt PRIVATE -fno-rtti -fno-exceptions)
target_link_libraries(tesl PRIVATE fmt)

add_subdirectory(intrusive_shared_ptr)
target_link_libraries(tesl PRIVATE isptr)

#target_compile_options(tesl PUBLIC -fsanitize=address -m32)
#target_link_options(tesl PUBLIC -fsanitize=address -m32)
#target_compile_options(tesl PUBLIC -fsanitize=address -fno-rtti -fno-exceptions)
#target_link_options(tesl PUBLIC -fsanitize=address -fno-rtti -fno-exceptions)
target_compile_options(tesl PRIVATE -fno-rtti -fno-exceptions -Werror=switch)
target_link_options(tesl PRIVATE -fno-rtti -fno-exceptions -Werror=switch)
# maybe -ffast-math -static -static-libgcc -static-libstdc++ -nodefaultlibs idk

if (TESL_PICO)
    target_link_libraries(tesl PUBLIC
        pico_stdlib
        #pico_printf
        #pico_platform_panic
        #pico_runtime
        #pico_runtime_init
        #tinyusb_board
        #tinyusb_device
        #pico_stdio
        #pico_runtime
        #pico_printf
        #pico_platform
        #pico_unique_id
        #tinyusb_device
        #tinyusb_board
    )
    #pico_enable_stdio_usb(tesl 0)
    #pico_enable_stdio_uart(tesl 1)

    # create map/bin/hex/uf2 file etc.
    pico_add_extra_outputs(tesl)
endif ()

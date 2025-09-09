
add_executable(tesl
    main.cpp
    tcolor.hpp tcolor.cpp
    tesl_common.hpp tesl_common.cpp
    tesl_bootstrap.hpp tesl_bootstrap.cpp
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
    tesl_signature.hpp tesl_signature.cpp
    tesl_operator.hpp tesl_operator.cpp
    tesl_member.hpp tesl_member.cpp
    tesl_symbol_table.hpp
    tesl_tokenizer.hpp tesl_tokenizer.cpp
    tesl_parser.hpp tesl_parser.cpp
)

target_link_libraries(tesl PRIVATE fmt)
target_link_libraries(tesl PRIVATE isptr)
target_link_options(tesl PRIVATE -Werror=switch)

if(WIN32)
    target_link_options(tesl PRIVATE -static-libgcc -static-libstdc++)
endif()

if(TESL_PICO)
    target_sources(pico_stdlib PRIVATE tesl_syscall_stub_getentropy.c)
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
else()
    target_compile_options(tesl PRIVATE -flto)
    target_link_options(tesl PRIVATE -flto)
endif()

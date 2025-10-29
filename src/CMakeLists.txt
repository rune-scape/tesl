
add_executable(tesl
    main.cpp
    tesl_fwd.hpp
    tcolor.hpp tcolor.cpp
    tesl_fmt.hpp tesl_fmt.cpp
    tesl_common.hpp tesl_common.cpp
    tesl_coroutine.hpp
    tesl_builtins.hpp tesl_builtins.cpp
    tesl_bind.hpp tesl_bind.cpp
    tesl_math.hpp tesl_math.cpp
    tesl_str.hpp tesl_str.cpp
    tesl_fn.hpp tesl_fn.cpp
    tesl_array.hpp
    tesl_dynamic_array.hpp tesl_dynamic_array.cpp
    tesl_map.hpp
    tesl_dynamic_map.hpp tesl_dynamic_map.cpp
    tesl_var.hpp tesl_var.cpp
    tesl_ref.hpp
    tesl_hash.hpp tesl_hash.cpp
    tesl_env.hpp tesl_env.cpp
    tesl_name.hpp tesl_name.cpp
    tesl_signature.hpp tesl_signature.cpp
    tesl_type.hpp tesl_type.cpp
    tesl_member_function.hpp tesl_member_function.cpp
    tesl_member_variable.hpp tesl_member_variable.cpp
    tesl_symbol_table.hpp
    tesl_parse.hpp
    tesl_tokenizer.hpp tesl_tokenizer.cpp
    tesl_grammar.hpp tesl_grammar.cpp
    tesl_parser.hpp tesl_parser.cpp
    #tesl_compiler.hpp tesl_compiler.cpp
)

target_link_libraries(tesl PRIVATE fmt)
target_link_libraries(tesl PRIVATE isptr)
target_link_libraries(tesl PRIVATE mio)
target_compile_options(tesl PRIVATE -Werror=switch -Werror=dangling-reference)
target_compile_definitions(tesl PUBLIC ISPTR_SUPPORT_STD_FORMAT=0)

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

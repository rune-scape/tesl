cmake_minimum_required(VERSION 3.16)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_C_STANDARD 11)

set(TARGET_PY32 OFF CACHE BOOL "")

project(tesl C CXX)

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
    tesl_fmt.hpp tesl_fmt.cpp
    tesl_hash.hpp tesl_hash.cpp
    tesl_tokenizer.hpp tesl_tokenizer.cpp
    tesl_symbol.hpp tesl_symbol.cpp
    #tesl_compiler.hpp tesl_compiler.cpp
    #tesl_parser.hpp tesl_parser.cpp
)

#add_subdirectory(fmt)
#target_compile_definitions(tesl PRIVATE FMT_USE_LOCALE=0 FMT_OPTIMIZE_SIZE=2 FMT_BUILTIN_TYPES=0 FMT_USE_EXCEPTIONS=0)
#target_compile_options(fmt PUBLIC -fno-rtti -fno-exceptions -nodefaultlibs -lc)
#target_link_options(fmt PUBLIC -fno-rtti -fno-exceptions -nodefaultlibs -lc)
#target_link_libraries(tesl PRIVATE fmt)

add_library(rpmalloc STATIC rpmalloc/rpmalloc/rpmalloc.c)
target_compile_definitions(rpmalloc PRIVATE ENABLE_OVERRIDE=1)
target_include_directories(rpmalloc PUBLIC rpmalloc/rpmalloc)
target_link_libraries(tesl PRIVATE rpmalloc)

add_subdirectory(intrusive_shared_ptr)
target_link_libraries(tesl PRIVATE isptr)

#add_executable(tesl main.cpp tesl.hpp tesl.cpp printf.h printf.c)
#target_compile_options(tesl PUBLIC -fsanitize=address -m32)
#target_link_options(tesl PUBLIC -fsanitize=address -m32)
#target_compile_options(tesl PUBLIC -fsanitize=address -fno-rtti -fno-exceptions)
#target_link_options(tesl PUBLIC -fsanitize=address -fno-rtti -fno-exceptions)
target_compile_definitions(tesl PRIVATE FMT_USE_LOCALE=0 FMT_OPTIMIZE_SIZE=2 FMT_BUILTIN_TYPES=0 FMT_USE_EXCEPTIONS=0 FMT_USE_STD_STRING=0)
target_compile_options(tesl PUBLIC -fno-rtti -fno-exceptions -nodefaultlibs -lc -lgcc -flto)
target_link_options(tesl PUBLIC -fno-rtti -fno-exceptions -nodefaultlibs -lc -lgcc -flto)
# maybe -ffast-math idk

cmake_minimum_required(VERSION 3.16)

set(TARGET_PY32 OFF CACHE BOOL "")

if (TARGET_PY32)
    set(CMAKE_SYSTEM_NAME Generic)
    set(CMAKE_SYSTEM_VERSION 1)

    if (GCC_PATH)
        if (WIN32)  # add .exe for windows
            set(CMAKE_C_COMPILER ${GCC_PATH}/arm-none-eabi-gcc.exe)
            set(CMAKE_CXX_COMPILER ${GCC_PATH}/arm-none-eabi-g++.exe)
            set(CMAKE_ASM_COMPILER ${GCC_PATH}/arm-none-eabi-gcc.exe)
            set(CMAKE_AR ${GCC_PATH}/arm-none-eabi-ar.exe)
            set(CMAKE_OBJCOPY ${GCC_PATH}/arm-none-eabi-objcopy.exe)
            set(CMAKE_OBJDUMP ${GCC_PATH}/arm-none-eabi-objdump.exe)
            set(SIZE ${GCC_PATH}/arm-none-eabi-size.exe)
        else ()
            set(CMAKE_C_COMPILER ${GCC_PATH}/arm-none-eabi-gcc)
            set(CMAKE_CXX_COMPILER ${GCC_PATH}/arm-none-eabi-g++)
            set(CMAKE_ASM_COMPILER ${GCC_PATH}/arm-none-eabi-gcc)
            set(CMAKE_AR ${GCC_PATH}/arm-none-eabi-ar)
            set(CMAKE_OBJCOPY ${GCC_PATH}/arm-none-eabi-objcopy)
            set(CMAKE_OBJDUMP ${GCC_PATH}/arm-none-eabi-objdump)
            set(SIZE ${GCC_PATH}/arm-none-eabi-size)
        endif ()
    else ()
        set(CMAKE_C_COMPILER arm-none-eabi-gcc)
        set(CMAKE_CXX_COMPILER arm-none-eabi-g++)
        set(CMAKE_ASM_COMPILER arm-none-eabi-gcc)
        set(CMAKE_AR arm-none-eabi-ar)
        set(CMAKE_OBJCOPY arm-none-eabi-objcopy)
        set(CMAKE_OBJDUMP arm-none-eabi-objdump)
        set(SIZE arm-none-eabi-size)
    endif ()
    set(CMAKE_TRY_COMPILE_TARGET_TYPE "STATIC_LIBRARY")
    
    set(CMAKE_CXX_STANDARD 17)
    set(CMAKE_C_STANDARD 11)

    project(tesl C CXX ASM)

    # override options
    set(ENABLE_HAL_DRIVER OFF CACHE BOOL "Enable HAL driver")
    set(ENABLE_LL_DRIVER OFF CACHE BOOL "Enable LL driver")
    set(ENABLE_SYSTEM_XXX_C ON CACHE BOOL "Enable system_XXX.c")
    set(ENABLE_STARTUP_FILE ON CACHE BOOL "Enable startup file")
    set(ENABLE_LD_SCRIPT ON CACHE BOOL "Enable linker script")
    set(ENABLE_DEBUG_DEFINE ON CACHE BOOL "Enable debug define")
    set(MCU_MODEL "PY32F030x8" CACHE STRING "MCU model, like PY32F030x8")
    message("MCU_MODEL: ${MCU_MODEL}")

    add_subdirectory(Drivers)

    add_executable(${PROJECT_NAME}.elf)

    file(GLOB SOURCES ${CMAKE_CURRENT_LIST_DIR}/*.c)
    target_sources(${PROJECT_NAME}.elf
        PRIVATE
        ${SOURCES}
    )
    target_include_directories(${PROJECT_NAME}.elf
        PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}
    )



    target_link_libraries(${PROJECT_NAME}.elf PRIVATE drivers)

    target_compile_options(${PROJECT_NAME}.elf
        PRIVATE
        -mcpu=cortex-m0plus
        -mthumb
        -mthumb-interwork
        -ffunction-sections
        -fdata-sections
        -fno-common
        -fmessage-length=0
        $<$<COMPILE_LANGUAGE:ASM>:-xassembler-with-cpp>
    )

    target_compile_options(${PROJECT_NAME}.elf PRIVATE 
        $<$<CONFIG:Release>:-Ofast>
        $<$<CONFIG:RelWithDebInfo>:-Ofast -g>
        $<$<CONFIG:MinSizeRel>:-Os>
        $<$<CONFIG:Debug>:-Og -g>
    )

    target_link_options(${PROJECT_NAME}.elf
        PRIVATE
        -mcpu=cortex-m0plus 
        -mthumb 
        -mthumb-interwork
        -Wl,-gc-sections
        -Wl,--print-memory-usage
        -Wl,-Map=${PROJECT_BINARY_DIR}/${PROJECT_NAME}.map
        -specs=nano.specs
        -specs=nosys.specs
        -static
        -lc
        -lm
    )

    target_sources(${PROJECT_NAME}.elf
        PRIVATE
        main.cpp tesl.hpp tesl.cpp
    )
else ()
    set(CMAKE_CXX_STANDARD 20)
    set(CMAKE_C_STANDARD 11)

    project(tesl C CXX)

    if (MSVC)
        set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
    endif ()

    add_executable(tesl main.cpp tesl.hpp tesl.cpp)
    #add_executable(tesl main.cpp tesl.hpp tesl.cpp printf.h printf.c)
    #target_compile_options(tesl PUBLIC -fsanitize=address -m32)
    #target_link_options(tesl PUBLIC -fsanitize=address -m32)
    target_compile_options(tesl PUBLIC -fsanitize=address)
    target_link_options(tesl PUBLIC -fsanitize=address)
endif ()

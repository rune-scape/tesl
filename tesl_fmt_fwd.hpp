#pragma once

// fmt from https://github.com/fmtlib

/* - part of fmt/base.h - */
// Formatting library for C++ - the base API for char/UTF-8
//
// Copyright (c) 2012 - present, Victor Zverovich
// All rights reserved.
//
// For the license information refer to format.h.

#if defined(FMT_IMPORT_STD) && !defined(FMT_MODULE)
#  define FMT_MODULE
#endif

#ifndef FMT_MODULE
#  include <limits.h>  // CHAR_BIT
#  include <stdio.h>   // FILE
#  include <string.h>  // memcmp

#  include <type_traits>  // std::enable_if
#endif

// The fmt library version in the form major * 10000 + minor * 100 + patch.
#define FMT_VERSION 110201

// Detect compiler versions.
#if defined(__clang__) && !defined(__ibmxl__)
#  define FMT_CLANG_VERSION (__clang_major__ * 100 + __clang_minor__)
#else
#  define FMT_CLANG_VERSION 0
#endif
#if defined(__GNUC__) && !defined(__clang__) && !defined(__INTEL_COMPILER)
#  define FMT_GCC_VERSION (__GNUC__ * 100 + __GNUC_MINOR__)
#else
#  define FMT_GCC_VERSION 0
#endif
#if defined(__ICL)
#  define FMT_ICC_VERSION __ICL
#elif defined(__INTEL_COMPILER)
#  define FMT_ICC_VERSION __INTEL_COMPILER
#else
#  define FMT_ICC_VERSION 0
#endif
#if defined(_MSC_VER)
#  define FMT_MSC_VERSION _MSC_VER
#else
#  define FMT_MSC_VERSION 0
#endif

// Detect standard library versions.
#ifdef _GLIBCXX_RELEASE
#  define FMT_GLIBCXX_RELEASE _GLIBCXX_RELEASE
#else
#  define FMT_GLIBCXX_RELEASE 0
#endif
#ifdef _LIBCPP_VERSION
#  define FMT_LIBCPP_VERSION _LIBCPP_VERSION
#else
#  define FMT_LIBCPP_VERSION 0
#endif

#ifdef _MSVC_LANG
#  define FMT_CPLUSPLUS _MSVC_LANG
#else
#  define FMT_CPLUSPLUS __cplusplus
#endif

// Detect __has_*.
#ifdef __has_feature
#  define FMT_HAS_FEATURE(x) __has_feature(x)
#else
#  define FMT_HAS_FEATURE(x) 0
#endif
#ifdef __has_include
#  define FMT_HAS_INCLUDE(x) __has_include(x)
#else
#  define FMT_HAS_INCLUDE(x) 0
#endif
#ifdef __has_builtin
#  define FMT_HAS_BUILTIN(x) __has_builtin(x)
#else
#  define FMT_HAS_BUILTIN(x) 0
#endif
#ifdef __has_cpp_attribute
#  define FMT_HAS_CPP_ATTRIBUTE(x) __has_cpp_attribute(x)
#else
#  define FMT_HAS_CPP_ATTRIBUTE(x) 0
#endif

#define FMT_HAS_CPP14_ATTRIBUTE(attribute) \
  (FMT_CPLUSPLUS >= 201402L && FMT_HAS_CPP_ATTRIBUTE(attribute))

#define FMT_HAS_CPP17_ATTRIBUTE(attribute) \
  (FMT_CPLUSPLUS >= 201703L && FMT_HAS_CPP_ATTRIBUTE(attribute))

// Detect C++14 relaxed constexpr.
#ifdef FMT_USE_CONSTEXPR
// Use the provided definition.
#elif FMT_GCC_VERSION >= 702 && FMT_CPLUSPLUS >= 201402L
// GCC only allows constexpr member functions in non-literal types since 7.2:
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=66297.
#  define FMT_USE_CONSTEXPR 1
#elif FMT_ICC_VERSION
#  define FMT_USE_CONSTEXPR 0  // https://github.com/fmtlib/fmt/issues/1628
#elif FMT_HAS_FEATURE(cxx_relaxed_constexpr) || FMT_MSC_VERSION >= 1912
#  define FMT_USE_CONSTEXPR 1
#else
#  define FMT_USE_CONSTEXPR 0
#endif
#if FMT_USE_CONSTEXPR
#  define FMT_CONSTEXPR constexpr
#else
#  define FMT_CONSTEXPR
#endif

// Detect consteval, C++20 constexpr extensions and std::is_constant_evaluated.
#if !defined(__cpp_lib_is_constant_evaluated)
#  define FMT_USE_CONSTEVAL 0
#elif FMT_CPLUSPLUS < 201709L
#  define FMT_USE_CONSTEVAL 0
#elif FMT_GLIBCXX_RELEASE && FMT_GLIBCXX_RELEASE < 10
#  define FMT_USE_CONSTEVAL 0
#elif FMT_LIBCPP_VERSION && FMT_LIBCPP_VERSION < 10000
#  define FMT_USE_CONSTEVAL 0
#elif defined(__apple_build_version__) && __apple_build_version__ < 14000029L
#  define FMT_USE_CONSTEVAL 0  // consteval is broken in Apple clang < 14.
#elif FMT_MSC_VERSION && FMT_MSC_VERSION < 1929
#  define FMT_USE_CONSTEVAL 0  // consteval is broken in MSVC VS2019 < 16.10.
#elif defined(__cpp_consteval)
#  define FMT_USE_CONSTEVAL 1
#elif FMT_GCC_VERSION >= 1002 || FMT_CLANG_VERSION >= 1101
#  define FMT_USE_CONSTEVAL 1
#else
#  define FMT_USE_CONSTEVAL 0
#endif
#if FMT_USE_CONSTEVAL
#  define FMT_CONSTEVAL consteval
#  define FMT_CONSTEXPR20 constexpr
#else
#  define FMT_CONSTEVAL
#  define FMT_CONSTEXPR20
#endif

// Check if exceptions are disabled.
#ifdef FMT_USE_EXCEPTIONS
// Use the provided definition.
#elif defined(__GNUC__) && !defined(__EXCEPTIONS)
#  define FMT_USE_EXCEPTIONS 0
#elif defined(__clang__) && !defined(__cpp_exceptions)
#  define FMT_USE_EXCEPTIONS 0
#elif FMT_MSC_VERSION && !_HAS_EXCEPTIONS
#  define FMT_USE_EXCEPTIONS 0
#else
#  define FMT_USE_EXCEPTIONS 1
#endif
#if FMT_USE_EXCEPTIONS
#  define FMT_TRY try
#  define FMT_CATCH(x) catch (x)
#else
#  define FMT_TRY if (true)
#  define FMT_CATCH(x) if (false)
#endif

#ifndef FMT_USE_STD_STRING
#  define FMT_USE_STD_STRING 1
#endif

#ifdef FMT_NO_UNIQUE_ADDRESS
// Use the provided definition.
#elif FMT_CPLUSPLUS < 202002L
// Not supported.
#elif FMT_HAS_CPP_ATTRIBUTE(no_unique_address)
#  define FMT_NO_UNIQUE_ADDRESS [[no_unique_address]]
// VS2019 v16.10 and later except clang-cl (https://reviews.llvm.org/D110485).
#elif FMT_MSC_VERSION >= 1929 && !FMT_CLANG_VERSION
#  define FMT_NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
#endif
#ifndef FMT_NO_UNIQUE_ADDRESS
#  define FMT_NO_UNIQUE_ADDRESS
#endif

#if FMT_HAS_CPP17_ATTRIBUTE(fallthrough)
#  define FMT_FALLTHROUGH [[fallthrough]]
#elif defined(__clang__)
#  define FMT_FALLTHROUGH [[clang::fallthrough]]
#elif FMT_GCC_VERSION >= 700 && \
    (!defined(__EDG_VERSION__) || __EDG_VERSION__ >= 520)
#  define FMT_FALLTHROUGH [[gnu::fallthrough]]
#else
#  define FMT_FALLTHROUGH
#endif

// Disable [[noreturn]] on MSVC/NVCC because of bogus unreachable code warnings.
#if FMT_HAS_CPP_ATTRIBUTE(noreturn) && !FMT_MSC_VERSION && !defined(__NVCC__)
#  define FMT_NORETURN [[noreturn]]
#else
#  define FMT_NORETURN
#endif

#ifdef FMT_NODISCARD
// Use the provided definition.
#elif FMT_HAS_CPP17_ATTRIBUTE(nodiscard)
#  define FMT_NODISCARD [[nodiscard]]
#else
#  define FMT_NODISCARD
#endif

#ifdef FMT_DEPRECATED
// Use the provided definition.
#elif FMT_HAS_CPP14_ATTRIBUTE(deprecated)
#  define FMT_DEPRECATED [[deprecated]]
#else
#  define FMT_DEPRECATED /* deprecated */
#endif

#if FMT_GCC_VERSION || FMT_CLANG_VERSION
#  define FMT_VISIBILITY(value) __attribute__((visibility(value)))
#else
#  define FMT_VISIBILITY(value)
#endif

// Detect pragmas.
#define FMT_PRAGMA_IMPL(x) _Pragma(#x)
#if FMT_GCC_VERSION >= 504 && !defined(__NVCOMPILER)
// Workaround a _Pragma bug https://gcc.gnu.org/bugzilla/show_bug.cgi?id=59884
// and an nvhpc warning: https://github.com/fmtlib/fmt/pull/2582.
#  define FMT_PRAGMA_GCC(x) FMT_PRAGMA_IMPL(GCC x)
#else
#  define FMT_PRAGMA_GCC(x)
#endif
#if FMT_CLANG_VERSION
#  define FMT_PRAGMA_CLANG(x) FMT_PRAGMA_IMPL(clang x)
#else
#  define FMT_PRAGMA_CLANG(x)
#endif
#if FMT_MSC_VERSION
#  define FMT_MSC_WARNING(...) __pragma(warning(__VA_ARGS__))
#else
#  define FMT_MSC_WARNING(...)
#endif

// Enable minimal optimizations for more compact code in debug mode.
FMT_PRAGMA_GCC(push_options)
#if !defined(__OPTIMIZE__) && !defined(__CUDACC__) && !defined(FMT_MODULE)
FMT_PRAGMA_GCC(optimize("Og"))
#  define FMT_GCC_OPTIMIZED
#endif
FMT_PRAGMA_CLANG(diagnostic push)

#ifdef FMT_ALWAYS_INLINE
// Use the provided definition.
#elif FMT_GCC_VERSION || FMT_CLANG_VERSION
#  define FMT_ALWAYS_INLINE inline __attribute__((always_inline))
#else
#  define FMT_ALWAYS_INLINE inline
#endif
// A version of FMT_ALWAYS_INLINE to prevent code bloat in debug mode.
#if defined(NDEBUG) || defined(FMT_GCC_OPTIMIZED)
#  define FMT_INLINE FMT_ALWAYS_INLINE
#else
#  define FMT_INLINE inline
#endif

#ifndef FMT_BEGIN_NAMESPACE
#  define FMT_BEGIN_NAMESPACE \
    namespace fmt {           \
    inline namespace v11_tesl {
#  define FMT_END_NAMESPACE \
    }                       \
    }
#endif

#ifndef FMT_EXPORT
#  define FMT_EXPORT
#  define FMT_BEGIN_EXPORT
#  define FMT_END_EXPORT
#endif

#ifdef _WIN32
#  define FMT_WIN32 1
#else
#  define FMT_WIN32 0
#endif

#if !defined(FMT_HEADER_ONLY) && FMT_WIN32
#  if defined(FMT_LIB_EXPORT)
#    define FMT_API __declspec(dllexport)
#  elif defined(FMT_SHARED)
#    define FMT_API __declspec(dllimport)
#  endif
#elif defined(FMT_LIB_EXPORT) || defined(FMT_SHARED)
#  define FMT_API FMT_VISIBILITY("default")
#endif
#ifndef FMT_API
#  define FMT_API
#endif

#ifndef FMT_OPTIMIZE_SIZE
#  define FMT_OPTIMIZE_SIZE 0
#endif

// FMT_BUILTIN_TYPE=0 may result in smaller library size at the cost of higher
// per-call binary size by passing built-in types through the extension API.
#ifndef FMT_BUILTIN_TYPES
#  define FMT_BUILTIN_TYPES 1
#endif

#define FMT_APPLY_VARIADIC(expr) \
  using unused = int[];          \
  (void)unused { 0, (expr, 0)... }

FMT_BEGIN_NAMESPACE

// A formatter for objects of type T.
template <typename T, typename Char = char, typename Enable = void>
struct formatter {
  // A deleted default constructor indicates a disabled formatter.
  formatter() = delete;
};

FMT_END_NAMESPACE

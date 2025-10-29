#pragma once

#include <climits>
#include <cmath>
#include <cstddef>
#include <cstring>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <new>
#include <source_location>
#include <string_view>
#include <type_traits>
#include <fmt/fwd.h>
#include "tesl_fwd.hpp"
#include "tcolor.hpp"

//#define TESL_USE_CHAR32
#define TESL_USE_64_BIT_NUMBERS

#define TESL_DEBUG_TOKENIZER
#define TESL_DEBUG_GRAMMAR
#define TESL_DEBUG_PARSER
#define TESL_DEBUG_VALUE
#define TESL_DEBUG_COMPILE
//#define TESL_DEBUG_EVAL

// This is used to clearly mark flexible-sized arrays that appear at the end of
// some dynamically-allocated structs, known as the "struct hack".
#define FLEXIBLE_ARRAY 0

#define TESL_ALWAYS_INLINE inline __attribute__((always_inline))

#define TESL_CONSTEVAL(v) []() { constexpr auto ret = v; return ret; }()

#define TESL_EMPTY_STATEMENT do {} while (false)

#define TESL_ERROR_PRINT_MSG_BASE(prefix, ...) \
  ::tesl::_log_print_prefix_impl(stderr, ::tcolor::fg::red, prefix); \
  ::tesl::_log_print_msg_impl(stderr, ::tcolor::fg::red, __VA_ARGS__); \
  ::tesl::_log_print_source_location_impl(stderr, ::std::source_location::current());

#define TESL_WARN_PRINT_MSG_BASE(prefix, ...) \
  ::tesl::_log_print_prefix_impl(stderr, ::tcolor::fg::yellow, prefix); \
  ::tesl::_log_print_msg_impl(stderr, ::tcolor::fg::yellow, __VA_ARGS__); \
  ::tesl::_log_print_source_location_impl(stderr, ::std::source_location::current());

#define TESL_INFO_PRINT_MSG_BASE(prefix, ...) \
  ::tesl::_log_print_prefix_impl(stderr, ::tcolor::fg::white, prefix); \
  ::tesl::_log_print_msg_impl(stderr, ::tcolor::fg::white, __VA_ARGS__); \

#define TESL_FAIL_MSG_BASE(prefix, action, ...) \
  do { \
    TESL_ERROR_PRINT_MSG_BASE(prefix, __VA_ARGS__); \
    action; \
  } while (false)

#define TESL_FAIL_COND_MSG_BASE(prefix, cond, action, ...) \
  if (cond) [[unlikely]] { \
    TESL_FAIL_MSG_BASE(prefix, action, __VA_ARGS__); \
  } else do {} while (false)

#define TESL_FAIL_MSG(action, ...) \
  TESL_FAIL_MSG_BASE("error", action, __VA_ARGS__)

#define TESL_FAIL_COND_MSG(cond, action, ...) \
  TESL_FAIL_COND_MSG_BASE("error", cond, action, __VA_ARGS__)

#define TESL_FAIL_COND(cond, action) \
  TESL_FAIL_COND_MSG(cond, action, "'" #cond "' is true")

#ifndef NDEBUG
#define TESL_ASSERT_MSG(cond, ...) \
  TESL_FAIL_COND_MSG_BASE("assertion failed", !(cond), abort(), __VA_ARGS__)
#define TESL_ASSERT(cond) \
  TESL_FAIL_COND_MSG_BASE("assertion failed", !(cond), std::abort(), "'" #cond "' is false")
#define TESL_UNREACHABLE \
  do { \
    TESL_FAIL_MSG(abort(), "unreachable code"); \
    __builtin_unreachable(); \
  } while (false)
#else
#define TESL_ASSERT_MSG(cond, ...) TESL_EMPTY_STATEMENT
#define TESL_ASSERT(cond) TESL_EMPTY_STATEMENT
#define TESL_UNREACHABLE __builtin_unreachable()
#endif

namespace tesl {
  struct Null {};
  constexpr Null null_value{};

  using Bool = bool;

#ifdef TESL_USE_64_BIT_NUMBERS
  using FloatT = double;
  using IntT = std::int64_t;
  using UIntT = std::uint64_t;
#else
  using FloatT = float;
  using IntT = std::int32_t;
  using UIntT = std::uint32_t;
#endif

  using SizeT = std::size_t;

  struct WidePtr {
    void * ptr;
    SizeT size;
  };

  template<typename T>
  struct ArrayView {
    T * _ptr = nullptr;
    std::size_t _size = 0;

    TESL_ALWAYS_INLINE constexpr const T * data() const { return _ptr; }
    TESL_ALWAYS_INLINE constexpr const T * begin() const { return _ptr; }
    TESL_ALWAYS_INLINE constexpr const T * end() const { return _ptr + _size; }
    TESL_ALWAYS_INLINE constexpr IntT size() const { return _size; }

    TESL_ALWAYS_INLINE constexpr T & operator[](IntT i) { return _ptr[i]; }
    TESL_ALWAYS_INLINE constexpr const T & operator[](IntT i) const { return _ptr[i]; }

    TESL_ALWAYS_INLINE constexpr ArrayView &operator=(const ArrayView &) = default;
    TESL_ALWAYS_INLINE constexpr ArrayView &operator=(ArrayView &&) = default;
    TESL_ALWAYS_INLINE constexpr ArrayView(const ArrayView &) = default;
    TESL_ALWAYS_INLINE constexpr ArrayView(ArrayView &&) = default;

    template<size_t Size>
    TESL_ALWAYS_INLINE constexpr ArrayView(T (& data)[Size]) : _ptr(data), _size(Size) {}
    TESL_ALWAYS_INLINE constexpr ArrayView(T * data, IntT size) : _ptr(data), _size(size) {}
    TESL_ALWAYS_INLINE constexpr ArrayView() {}
  };

  template<typename T> inline constexpr const T * empty_str;
  template<> inline constexpr const char * empty_str<char> = "";
  template<> inline constexpr const wchar_t * empty_str<wchar_t> = L"";
  template<> inline constexpr const char16_t * empty_str<char16_t> = u"";
  template<> inline constexpr const char32_t * empty_str<char32_t> = U"";

  template<typename T> inline constexpr T null_terminator;
  template<> inline constexpr char null_terminator<char> = '\0';
  template<> inline constexpr wchar_t null_terminator<wchar_t> = L'\0';
  template<> inline constexpr char16_t null_terminator<char16_t> = u'\0';
  template<> inline constexpr char32_t null_terminator<char32_t> = U'\0';

#ifdef TESL_USE_CHAR32
  using CommonCharT = char32_t;
  #define TESL_STR(str) U ## str
#else
  using CommonCharT = char;
  #define TESL_STR(str) str
#endif

  template<typename CharT>
  using BasicStrViewT = std::basic_string_view<CharT>;

  using CharStrView = BasicStrViewT<char>;
  using WCharStrView = BasicStrViewT<wchar_t>;
  using Char16StrView = BasicStrViewT<char16_t>;
  using Char32StrView = BasicStrViewT<char32_t>;
  using StrView = BasicStrViewT<CommonCharT>;

#define TESL_STRVIEW(str) StrView(TESL_STR(str))

  template<typename T>
  constexpr IntT string_length(const T * str) {
    return BasicStrViewT<T>{str}.length();
  }

  constexpr IntT variant_storage_size = sizeof(void *);

  typedef void (* FnPtrBare)(void * this_, void * args, void * ret);

  inline void empty_fn(void * this_, void * args, void * ret) { }

  #define MOV(...) static_cast<std::remove_reference_t<decltype(__VA_ARGS__)> &&>(__VA_ARGS__)
  #define FWD(...) static_cast<decltype(__VA_ARGS__) &&>(__VA_ARGS__)

  template<typename T>
  inline void swap(T & a, T & b) {using std::swap;
    T c(MOV(a));
    a = MOV(b);
    b = MOV(c);
  }

  template<typename T>
  T min(T a, T b) {
    if (a > b) {
      return b;
    } else {
      return a;
    }
  }

  template<typename T>
  T max(T a, T b) {
    if (a < b) {
      return b;
    } else {
      return a;
    }
  }

  inline UIntT get_float_bits(FloatT f) {
    static_assert(sizeof(FloatT) == sizeof(UIntT));
    union {
      FloatT f;
      UIntT i;
    } u;

    return u.i;
  }

  template<auto ... Vs>
  struct value_pack {};

  template<typename ... Ts>
  struct type_pack {};

  template<typename T>
  inline void memswap(T & a, T & b) {
    alignas(T) char tmp[sizeof(T)];
    memcpy(reinterpret_cast<void *>(tmp), reinterpret_cast<void *>(&a), sizeof(T));
    memcpy(reinterpret_cast<void *>(&a), reinterpret_cast<void *>(&b), sizeof(T));
    memcpy(reinterpret_cast<void *>(&b), reinterpret_cast<void *>(tmp), sizeof(T));
  }

  void print(std::FILE * f, StrView str);

  inline void print(CharStrView str) { print(stdout, str); }

  inline void println(std::FILE * f, CharStrView str) {
    print(f, str);
    print(f, "\n");
  }

  inline void println(CharStrView str) { println(stdout, str); }

  void _log_print_prefix_impl(FILE * f, tcolor::fg col, const char * prefix);
  void _log_print_msg_impl(FILE * f, tcolor::fg col, const char * msg);
  void _log_print_source_location_impl(FILE * f, const std::source_location & loc);

  template<typename T, typename = void, typename ... Args>
  struct _log_print_msg_include_error_disabler {
    using type = T;
  };

  template<typename ... T>
  void _log_print_msg_impl(FILE * f, tcolor::fg col, typename _log_print_msg_include_error_disabler<const char *, void, T...>::type msg, T && ...) {
    static_assert(false, "you may want to include 'tesl_fmt.hpp' to use formatted error messages");
  }

  void print_error_sourcev(FILE * file, int line_num, const char * line_start, const char * error_start, const char * error_point, const char * error_end);

  inline void print_error_source(int line_num, const char * line_start, const char * error_start, const char * error_point, const char * error_end) {
    print_error_sourcev(stderr, line_num, line_start, error_start, error_point, error_end);
  }

  template<typename T> void bind_type_info(TypeInfo & type);

  template<> void bind_type_info<Null>(TypeInfo & type);
  template<> void bind_type_info<Bool>(TypeInfo & type);
  template<> void bind_type_info<IntT>(TypeInfo & type);
  template<> void bind_type_info<FloatT>(TypeInfo & type);
  template<> void bind_type_info<Type>(TypeInfo & type);
}

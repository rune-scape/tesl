#pragma once

#include <cassert>
#include <climits>
#include <cmath>
#include <cstddef>
#include <cstring>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <new>
#include <string_view>
#include <type_traits>

//#define TESL_USE_CHAR32
#define TESL_USE_64_BIT_NUMBERS

#define TESL_DEBUG_PARSER
#define TESL_DEBUG_VALUE
#define TESL_DEBUG_COMPILE
//#define TESL_DEBUG_EVAL

//#define TESL_DISABLE_PRINTF

#ifdef TESL_DISABLE_PRINTF
#define tesl_printf(...)
#else
#include <cstdio>
#define tesl_printf(...) printf(__VA_ARGS__)
#endif

#ifndef TESL_DEBUG_COMPILE
#define TE_FAIL_COND(cond, action)\
    if (cond) {\
        tesl_printf("error: \'" #cond "\' is true (at %s:%d in %s)\n", __FILE__, __LINE__, __func__);\
        action;\
    } else ((void)0)
#define TE_FAIL_COND_MSG(cond, action, ...)\
    if (cond) {\
        tesl_printf(__VA_ARGS__);\
        action;\
    } else ((void)0)
#else
#define TE_FAIL_COND(cond, action)\
    if (cond) {\
        action;\
    } else ((void)0)
#define TE_FAIL_COND_MSG(cond, action, ...)\
    if (cond) {\
        action;\
    } else ((void)0)
#endif

// This is used to clearly mark flexible-sized arrays that appear at the end of
// some dynamically-allocated structs, known as the "struct hack".
#define FLEXIBLE_ARRAY 0

#define TESL_ALWAYS_INLINE inline __attribute__((always_inline))

#define TESL_UNREACHABLE abort();

namespace tesl {
  struct Null {};
  constexpr Null null_value{};

  using Bool = bool;

#ifdef TESL_USE_64_BIT_NUMBERS
  using FloatT = double;
  using IntT = int64_t;
  using UIntT = uint64_t;
#else
  using FloatT = float;
  using IntT = int32_t;
  using UIntT = uint32_t;
#endif

  template<typename T>
  struct ArrayView {
    T * _ptr = nullptr;
    T * _end = _ptr;

    TESL_ALWAYS_INLINE constexpr const T * data() const { return _ptr; }
    TESL_ALWAYS_INLINE constexpr const T * begin() const { return _ptr; }
    TESL_ALWAYS_INLINE constexpr const T * end() const { return _end; }
    TESL_ALWAYS_INLINE constexpr IntT size() const { return _end - _ptr; }

    TESL_ALWAYS_INLINE constexpr T & operator[](IntT i) { return _ptr[i]; }
    TESL_ALWAYS_INLINE constexpr const T & operator[](IntT i) const { return _ptr[i]; }

    TESL_ALWAYS_INLINE constexpr ArrayView &operator=(const ArrayView &) = default;
    TESL_ALWAYS_INLINE constexpr ArrayView &operator=(ArrayView &&) = default;
    TESL_ALWAYS_INLINE constexpr ArrayView(const ArrayView &) = default;
    TESL_ALWAYS_INLINE constexpr ArrayView(ArrayView &&) = default;

    TESL_ALWAYS_INLINE constexpr ArrayView(T * data, IntT size) : _ptr(data), _end(_ptr + size) {}
    TESL_ALWAYS_INLINE constexpr ArrayView() {}
  };

  template<typename T>
  inline IntT string_length(const T * str) {
    IntT result = 0;
    for (; *str; ++str, ++result);
    return result;
  }

  template<>
  inline IntT string_length<char>(const char * str) {
    return strlen(str);
  }

  template<typename T>
  constexpr IntT constexpr_strlen(const T * str) {
    return *str ? 1 + constexpr_strlen(str + 1) : 0;
  }

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
  using StrViewBaseT = std::basic_string_view<CharT>;

  using CharStrView = StrViewBaseT<char>;
  using WCharStrView = StrViewBaseT<wchar_t>;
  using Char16StrView = StrViewBaseT<char16_t>;
  using Char32StrView = StrViewBaseT<char32_t>;
  using StrView = StrViewBaseT<CommonCharT>;

#define ConstStrViewT(str) StrView{str, constexpr_strlen(str)}
#define TESL_STRVIEW(str) ConstStrViewT(TESL_STR(str))

  constexpr IntT variant_storage_size = sizeof(void *);

  typedef void (* FnPtr)(void * context, void * args, void * ret);

  struct FnObjBase {
    FnPtr ptr = nullptr;
    void * context = nullptr;

    bool is_valid() const {
      return ptr != nullptr;
    }

    void operator()(void * args, void * ret) const {
      ptr(context, args, ret);
    }
  };

  inline void empty_fn(void * context, void * args, void * ret) { }

  #define MOV(...) static_cast<std::remove_reference_t<decltype(__VA_ARGS__)> &&>(__VA_ARGS__)
  #define FWD(...) static_cast<decltype(__VA_ARGS__) &&>(__VA_ARGS__)

  template<typename T>
  inline void swap(T & a, T & b) {
    T c(MOV(a));
    a = MOV(b);
    b = MOV(c);
  }

  template<typename T>
  TESL_ALWAYS_INLINE T min(T a, T b) {
    if (a > b) {
      return b;
    } else {
      return a;
    }
  }

  template<typename T>
  TESL_ALWAYS_INLINE T max(T a, T b) {
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

    // Normalize +/- 0.0 and NaN values so they hash the same.
    if (f == 0.0f) {
      u.f = 0.0f;
    } else if (std::isnan(f)) {
      u.f = NAN;
    } else {
      u.f = f;
    }

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

  void print_error(int line_num, const char * line_start, const char * error_start, const char * error_point, const char * error_end);

  /*template<typename ... Ts>
  inline void printv(Ts && ... vs) {
    (print(vs), ...);
    tesl_printf("\n");
  }*/
} // namespace tesl

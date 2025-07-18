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
#include <type_traits>

//#define TESL_USE_CHAR32
#define TESL_USE_64_BIT_NUMBERS

#define TESL_DEBUG_TOKENIZER
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

namespace tesl {
  struct NullT {};
  constexpr NullT null_value{};

  struct BoolT {
    bool value = false;
    BoolT() = default;
    explicit BoolT(bool v) : value(v) {}
    explicit operator bool() { return value; }
  };

#ifdef TESL_USE_64_BIT_NUMBERS
  using FloatT = double;
  using IntT = int64_t;
  using UIntT = uint64_t;
#else
  using FloatT = float;
  using IntT = int32_t;
  using UIntT = uint32_t;
#endif

  using HashT = uint64_t;

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

  template<typename T>
  constexpr const auto * empty_str = "";
  template<>
  constexpr const auto * empty_str<char> = "";
  template<>
  constexpr const auto * empty_str<wchar_t> = L"";
  template<>
  constexpr const auto * empty_str<char16_t> = u"";
  template<>
  constexpr const auto * empty_str<char32_t> = U"";

#ifdef TESL_USE_CHAR32
  using CommonCharT = char32_t;
  #define TESL_STR(str) U ## str
#else
  using CommonCharT = char;
  #define TESL_STR(str) str
#endif


  template<typename T>
  struct StrViewBaseT : public ArrayView<const T> {
    using base = ArrayView<const T>;
    using base::base;

    TESL_ALWAYS_INLINE constexpr IntT length() const {
      return base::size();
    }

    TESL_ALWAYS_INLINE constexpr bool operator==(const StrViewBaseT &other) const {
      auto len = length();
      if (len == other.length()) {
        for (IntT i = 0; i < len; ++i) {
          if ((*this)[i] != other[i]) {
            return false;
          }
        }
        return true;
      }

      return false;
    }

    TESL_ALWAYS_INLINE constexpr const T * ptr() const { return base::data(); }

    TESL_ALWAYS_INLINE StrViewBaseT(const T * str) : base::_ptr(str), base::_end(str + string_length(str)) {}
  };

  using CharStrViewT = StrViewBaseT<char>;
  using WCharStrViewT = StrViewBaseT<wchar_t>;
  using Char16StrViewT = StrViewBaseT<char16_t>;
  using Char32StrViewT = StrViewBaseT<char32_t>;
  using StrViewT = StrViewBaseT<CommonCharT>;

#define ConstStrViewT(str) StrViewT{str, constexpr_strlen(str)}
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

  #define MOV(...) static_cast<std::remove_reference_t<decltype(__VA_ARGS__)>&&>(__VA_ARGS__)
  #define FWD(...) static_cast<decltype(__VA_ARGS__)&&>(__VA_ARGS__)

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

  HashT hash(BoolT b);
  HashT hash(IntT i);
  HashT hash(FloatT f);
  HashT hash(char c);
  HashT hash(wchar_t c);
  HashT hash(char16_t c);
  HashT hash(char32_t c);
  HashT hash(const char * str);
  HashT hash(const wchar_t * str);
  HashT hash(const char16_t * str);
  HashT hash(const char32_t * str);
  HashT hash(const CharStrViewT & str);
  HashT hash(const WCharStrViewT & str);
  HashT hash(const Char16StrViewT & str);
  HashT hash(const Char32StrViewT & str);

  void print(BoolT b);
  void print(IntT i);
  void print(FloatT f);
  void print(char c);
  void print(wchar_t c);
  void print(char16_t c);
  void print(char32_t c);
  void print(const char * str);
  void print(const wchar_t * str);
  void print(const char16_t * str);
  void print(const char32_t * str);
  void print(const CharStrViewT & str);
  void print(const WCharStrViewT & str);
  void print(const Char16StrViewT & str);
  void print(const Char32StrViewT & str);
  void print_error(IntT line_num, const char * line_start, const char * error_start, const char * error_point, const char * error_end);

  template<typename ... Ts>
  inline void printv(Ts && ... vs) {
    (print(vs), ...);
    tesl_printf("\n");
  }
} // namespace tesl

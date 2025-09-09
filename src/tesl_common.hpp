#pragma once

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
#include "tesl_fwd.hpp"
#include "tcolor.hpp"

//#define TESL_USE_CHAR32
#define TESL_USE_64_BIT_NUMBERS

#define TESL_DEBUG_TOKENIZER
#define TESL_DEBUG_PARSER
#define TESL_DEBUG_VALUE
#define TESL_DEBUG_COMPILE
//#define TESL_DEBUG_EVAL

// This is used to clearly mark flexible-sized arrays that appear at the end of
// some dynamically-allocated structs, known as the "struct hack".
#define FLEXIBLE_ARRAY 0

#define TESL_ALWAYS_INLINE inline __attribute__((always_inline))

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

  struct FnContext {
    Env * env;
    void * this_;
    Variant * user_data;
  };

  typedef void (* FnPtrBare)(FnContext * context, void * args, void * ret);

  inline void empty_fn(FnContext * context, void * args, void * ret) { }

  #define MOV(...) static_cast<std::remove_reference_t<decltype(__VA_ARGS__)> &&>(__VA_ARGS__)
  #define FWD(...) static_cast<decltype(__VA_ARGS__) &&>(__VA_ARGS__)

  struct GlobalSymbolIndex {
    using IndexT = uint32_t;
    static constexpr IndexT max_index = std::numeric_limits<IndexT>::max();
    IndexT index = max_index;

    constexpr bool is_valid() const { return index != max_index; }
  };

  struct LocalSymbolIndex {
    using IndexT = uint16_t;
    static constexpr IndexT max_index = std::numeric_limits<IndexT>::max();
    IndexT index = max_index;

    constexpr bool is_valid() const { return index != max_index; }
  };

  template<typename T>
  inline void swap(T & a, T & b) {
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
  using remove_cvref_t = std::remove_cv_t< std::remove_reference_t<T> >;

  template<typename T>
  inline void memswap(T & a, T & b) {
    alignas(T) char tmp[sizeof(T)];
    memcpy(reinterpret_cast<void *>(tmp), reinterpret_cast<void *>(&a), sizeof(T));
    memcpy(reinterpret_cast<void *>(&a), reinterpret_cast<void *>(&b), sizeof(T));
    memcpy(reinterpret_cast<void *>(&b), reinterpret_cast<void *>(tmp), sizeof(T));
  }

  void print_error_sourcev(FILE * file, int line_num, const char * line_start, const char * error_start, const char * error_point, const char * error_end);

  inline void print_error_source(int line_num, const char * line_start, const char * error_start, const char * error_point, const char * error_end) {
    print_error_sourcev(stderr, line_num, line_start, error_start, error_point, error_end);
  }

  using NameRef = Ref<Name>;
  using SignatureRef = Ref<Signature>;
  using TypeRef = Ref<const TypeInfo>;

  template<typename T> TypeRef make_type_info();
  template<typename T> TypeRef get_builtin_type_info_of();

  template<> TypeRef make_type_info<Null>();
  template<> TypeRef make_type_info<Bool>();
  template<> TypeRef make_type_info<IntT>();
  template<> TypeRef make_type_info<FloatT>();
  template<> TypeRef make_type_info<TypeRef>();
  template<> TypeRef get_builtin_type_info_of<Null>();
  template<> TypeRef get_builtin_type_info_of<Bool>();
  template<> TypeRef get_builtin_type_info_of<IntT>();
  template<> TypeRef get_builtin_type_info_of<FloatT>();
  template<> TypeRef get_builtin_type_info_of<TypeRef>();
} // namespace tesl

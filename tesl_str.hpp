#pragma once

#include "tesl_common.hpp"
#include "tesl_fmt_fwd.hpp"
#include "tesl_hash.hpp"
#include "tesl_type.hpp"
#include "tesl_array.hpp"

namespace tesl {
  // some members are just to get it to work with fmt
  template<typename T>
  struct BasicStrT {
    using CharT = T;
    using value_type = T;
    using iterator = T *;
    using const_iterator = const T *;

    Array<T, 4 / sizeof(T)> _buffer;

    constexpr IntT max_size() const {
      return std::numeric_limits<IntT>::max();
    }

    constexpr bool operator==(const BasicStrT & other) const {
      return _buffer == other._buffer;
    }

    TESL_ALWAYS_INLINE constexpr const T * c_str() const { return _buffer.data(); }
    TESL_ALWAYS_INLINE constexpr const T * data() const { return _buffer.data(); }
    TESL_ALWAYS_INLINE constexpr T * data() { return _buffer.data(); }
    TESL_ALWAYS_INLINE constexpr const T * begin() const { return _buffer.begin(); }
    TESL_ALWAYS_INLINE constexpr T * begin() { return _buffer.begin(); }
    TESL_ALWAYS_INLINE constexpr const T * end() const { return _buffer.end() - 1; }
    TESL_ALWAYS_INLINE constexpr T * end() { return _buffer.end() - 1; }
    TESL_ALWAYS_INLINE constexpr const T & front() const { return *begin(); }
    TESL_ALWAYS_INLINE constexpr T & front() { return *begin(); }
    TESL_ALWAYS_INLINE constexpr const T & back() const { return *(end() - 1); }
    TESL_ALWAYS_INLINE constexpr T & back() { return *(end() - 1); }
    TESL_ALWAYS_INLINE constexpr IntT size() const { return _buffer.size() - 1; }
    TESL_ALWAYS_INLINE constexpr IntT length() const { return size(); }

    TESL_ALWAYS_INLINE constexpr const T & operator[](IntT i) const { return _buffer[i]; }
    TESL_ALWAYS_INLINE constexpr T & operator[](IntT i) { return _buffer[i]; }

    TESL_ALWAYS_INLINE constexpr operator StrViewBaseT<T>() const { return {data(), static_cast<size_t>(size())}; }

    TESL_ALWAYS_INLINE constexpr BasicStrT & operator+=(T c) {
      _buffer.insert(_buffer.size() - 1, c);
      return *this;
    }
    TESL_ALWAYS_INLINE constexpr BasicStrT & operator+=(BasicStrT other) {
      _buffer.remove_at(_buffer.size() - 1);
      _buffer += other._buffer;
      return *this;
    }
    TESL_ALWAYS_INLINE constexpr BasicStrT operator+(BasicStrT other) const {
      BasicStrT result{*this};
      result += other;
      return result;
    }

    TESL_ALWAYS_INLINE constexpr void resize(IntT new_size) {
      _buffer.resize(new_size + 1);
      _buffer[new_size] = null_terminator<T>;
    }

    TESL_ALWAYS_INLINE constexpr BasicStrT & operator=(const BasicStrT & other) { _buffer = other._buffer; return *this; }
    TESL_ALWAYS_INLINE constexpr BasicStrT & operator=(BasicStrT && other) { _buffer = MOV(other._buffer); return *this; }
    TESL_ALWAYS_INLINE constexpr BasicStrT(const BasicStrT & other) { this->operator=(other); }
    TESL_ALWAYS_INLINE constexpr BasicStrT(BasicStrT && other) { this->operator=(MOV(other)); }

    TESL_ALWAYS_INLINE constexpr BasicStrT(const StrViewBaseT<T> & str) : BasicStrT(str.data(), str.length()) {}

    TESL_ALWAYS_INLINE constexpr BasicStrT(const T * str, IntT len) {
      _buffer.reserve(len + 1);
      _buffer += ArrayView{str, len};
      _buffer.push_back('\0');
    }
    TESL_ALWAYS_INLINE constexpr BasicStrT(const T * str, size_t len) : BasicStrT(str, static_cast<IntT>(len)) {}

    template<typename T2>
    TESL_ALWAYS_INLINE BasicStrT(const T2 * str) : BasicStrT(str, string_length(str)) {}
    TESL_ALWAYS_INLINE constexpr BasicStrT() {
      _buffer.push_back('\0');
    }
  };

  extern template struct BasicStrT<char>;
  extern template struct BasicStrT<wchar_t>;
  extern template struct BasicStrT<char16_t>;
  extern template struct BasicStrT<char32_t>;

  using CharStr = BasicStrT<char>;
  using WCharStr = BasicStrT<wchar_t>;
  using Char16Str = BasicStrT<char16_t>;
  using Char32Str = BasicStrT<char32_t>;
  using Str = BasicStrT<CommonCharT>;

  template<typename T>
  TESL_ALWAYS_INLINE HashT hash(BasicStrT<T> str) {
    return hash(static_cast<StrViewBaseT<T>>(str));
  }

  template<> inline const TypeInfo * type_info_of<Str> = &typeInfoStr;
}

template<typename CharT>
class fmt::formatter<tesl::CharStr, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::CharStr & str, Context & ctx) const {
    return format_to(ctx.out(), "{}", tesl::CharStrView{str.data(), static_cast<size_t>(str.size())});
  }
};

template<typename CharT>
class fmt::formatter<tesl::WCharStr, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::WCharStr & str, Context & ctx) const {
    return format_to(ctx.out(), "{}", tesl::WCharStrView{str.data(), static_cast<size_t>(str.size())});
  }
};

template<typename CharT>
class fmt::formatter<tesl::Char16Str, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Char16Str & str, Context & ctx) const {
    return format_to(ctx.out(), "{}", tesl::Char16StrView{str.data(), static_cast<size_t>(str.size())});
  }
};

template<typename CharT>
class fmt::formatter<tesl::Char32Str, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Char32Str & str, Context & ctx) const {
    return format_to(ctx.out(), "{}", tesl::Char32StrView{str.data(), static_cast<size_t>(str.size())});
  }
};

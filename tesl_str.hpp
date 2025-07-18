#pragma once

#include "tesl_common.hpp"
#include "tesl_type.hpp"
#include "tesl_vector.hpp"

namespace tesl {
  template<typename T>
  struct StrBaseT {
    using CharT = T;

    Vector<T, 4 / sizeof(T)> _buffer;
  
    constexpr bool operator==(const StrBaseT & other) const {
      return _buffer == other._buffer;
    }
  
    TESL_ALWAYS_INLINE constexpr const T * ptr() const {
      if (_buffer.is_empty()) {
        if constexpr (std::is_same_v<T, wchar_t>) {
          return L"";
        } else if constexpr (std::is_same_v<T, char16_t>) {
          return u"";
        } else if constexpr (std::is_same_v<T, char32_t>) {
          return U"";
        } else {
          static_assert(std::is_same_v<T, char>, "unknown char type!");
          return "";
        }
      }

      return _buffer.data();
    }
    TESL_ALWAYS_INLINE constexpr const T * begin() const { return _buffer.begin(); }
    TESL_ALWAYS_INLINE constexpr const T * end() const { return _buffer.end() - 1; }
    TESL_ALWAYS_INLINE constexpr IntT length() const { return _buffer.size() - 1; }

    TESL_ALWAYS_INLINE constexpr T operator[](IntT i) { return _buffer[i]; }

    TESL_ALWAYS_INLINE constexpr operator StrViewBaseT<T>() { return {ptr(), length()}; }

    TESL_ALWAYS_INLINE constexpr StrBaseT & operator+=(T c) {
      _buffer.insert(_buffer.size() - 1, c);
      return *this;
    }
    TESL_ALWAYS_INLINE constexpr StrBaseT & operator+=(StrBaseT other) {
      _buffer.remove_at(_buffer.size() - 1);
      _buffer += other._buffer;
      return *this;
    }
    TESL_ALWAYS_INLINE constexpr StrBaseT & operator+(StrBaseT other) {
      StrBaseT result{*this};
      result += other;
      return result;
    }

    TESL_ALWAYS_INLINE constexpr StrBaseT & operator=(const StrBaseT & other) { _buffer = other._buffer; return *this; }
    TESL_ALWAYS_INLINE constexpr StrBaseT & operator=(StrBaseT && other) { _buffer = MOV(other._buffer); return *this; }
    TESL_ALWAYS_INLINE constexpr StrBaseT(const StrBaseT & other) { this->operator=(other); }
    TESL_ALWAYS_INLINE constexpr StrBaseT(StrBaseT && other) { this->operator=(MOV(other)); }

    TESL_ALWAYS_INLINE constexpr StrBaseT(const StrViewBaseT<T> & str) : StrBaseT(str.ptr(), str.length()) {}

    TESL_ALWAYS_INLINE constexpr StrBaseT(const T * str, IntT len) {
      _buffer.reserve(len + 1);
      _buffer += ArrayView{str, len};
      _buffer.push_back('\0');
    }

    template<typename T2>
    TESL_ALWAYS_INLINE StrBaseT(const T2 * str) : StrBaseT(str, string_length(str)) {}
    TESL_ALWAYS_INLINE constexpr StrBaseT() {
      _buffer.push_back('\0');
    }
  };

  using CharStrT = StrBaseT<char>;
  using WCharStrT = StrBaseT<wchar_t>;
  using Char16StrT = StrBaseT<char16_t>;
  using Char32StrT = StrBaseT<char32_t>;
  using StrT = StrBaseT<CommonCharT>;

  template<typename T>
  TESL_ALWAYS_INLINE HashT hash(StrBaseT<T> str) {
    return hash(static_cast<StrViewBaseT<T>>(str));
  }

  TESL_ALWAYS_INLINE HashT hash(StrBaseT<char> str) {
    return hash(static_cast<StrViewBaseT<char>>(str));
  }

  template<> inline const TypeInfo * type_info_of<StrT> = &typeInfoStr;

  void print(const CharStrT & str);
  void print(const WCharStrT & str);
  void print(const Char16StrT & str);
  void print(const Char32StrT & str);

} 
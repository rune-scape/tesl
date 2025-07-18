#pragma once

#include "tesl_common.hpp"

namespace tesl {
  namespace detail {
    HashT hash_mix(uint64_t a, HashT b);
    HashT hash_buffer(const void * buffer, IntT len);

    TESL_ALWAYS_INLINE HashT hash_one(uint64_t v) {
      return hash_mix(v, UINT64_C(0x2d358dccaa6c78a5));
    }
  }

  TESL_ALWAYS_INLINE HashT hash(BoolT b) {
    return detail::hash_one(b.value);
  }

  TESL_ALWAYS_INLINE HashT hash(IntT i) {
    return detail::hash_one(i);
  }

  TESL_ALWAYS_INLINE HashT hash(FloatT f) {
    return detail::hash_one(get_float_bits(f));
  }

  TESL_ALWAYS_INLINE HashT hash(char c) {
    return detail::hash_one(c);
  }

  TESL_ALWAYS_INLINE HashT hash(wchar_t c) {
    return detail::hash_one(c);
  }

  TESL_ALWAYS_INLINE HashT hash(char16_t c) {
    return detail::hash_one(c);
  }

  TESL_ALWAYS_INLINE HashT hash(char32_t c) {
    return detail::hash_one(c);
  }

  TESL_ALWAYS_INLINE HashT hash(const char * str) {
    return detail::hash_buffer(str, string_length(str));
  }

  TESL_ALWAYS_INLINE HashT hash(const wchar_t * str) {
    return detail::hash_buffer(str, string_length(str) * sizeof(wchar_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const char16_t * str) {
    return detail::hash_buffer(str, string_length(str) * sizeof(char16_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const char32_t * str) {
    return detail::hash_buffer(str, string_length(str) * sizeof(char32_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const CharStrViewT & str) {
    return detail::hash_buffer(str.ptr(), str.length());
  }

  TESL_ALWAYS_INLINE HashT hash(const WCharStrViewT & str) {
    return detail::hash_buffer(str.ptr(), str.length() * sizeof(wchar_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const Char16StrViewT & str) {
    return detail::hash_buffer(str.ptr(), str.length() * sizeof(char16_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const Char32StrViewT & str) {
    return detail::hash_buffer(str.ptr(), str.length() * sizeof(char32_t));
  }
}
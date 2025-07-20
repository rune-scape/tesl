#pragma once

#include "tesl_common.hpp"

namespace tesl {
  using HashT = uint64_t;

  namespace detail {
    HashT hash_mix(uint64_t a, HashT b);
    HashT hash_buffer(const void * buffer, IntT len);

    TESL_ALWAYS_INLINE HashT hash_one(uint64_t v) {
      return hash_mix(v, UINT64_C(0x4d5a2da51de1aa47));
    }
  }

  TESL_ALWAYS_INLINE HashT hash(Bool b) {
    return detail::hash_one(b);
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

  TESL_ALWAYS_INLINE HashT hash(const CharStrView & str) {
    return detail::hash_buffer(str.data(), str.size());
  }

  TESL_ALWAYS_INLINE HashT hash(const WCharStrView & str) {
    return detail::hash_buffer(str.data(), str.size() * sizeof(wchar_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const Char16StrView & str) {
    return detail::hash_buffer(str.data(), str.size() * sizeof(char16_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const Char32StrView & str) {
    return detail::hash_buffer(str.data(), str.size() * sizeof(char32_t));
  }
}
#pragma once

#include "tesl_common.hpp"

namespace tesl {
  using HashT = std::uint64_t;
  constexpr std::uint64_t default_seed = UINT64_C(0x9E3779B97F4A7C15);

  namespace detail {
    std::uint64_t hash_mix(std::uint64_t a, std::uint64_t b);
    HashT hash_buffer(const void * buffer, IntT len);

    TESL_ALWAYS_INLINE std::uint64_t hash_one(std::uint64_t v) {
      return hash_mix(v, default_seed);
    }

    template<typename T>
    TESL_ALWAYS_INLINE HashT hash_pointer(const T * p) {
      static_assert(sizeof(p) <= sizeof(std::uint64_t));
      return hash_one(reinterpret_cast<std::uintptr_t>(p));
    }
  }

  TESL_ALWAYS_INLINE HashT hash(Bool b) {
    return detail::hash_one(static_cast<std::uint64_t>(b));
  }

  TESL_ALWAYS_INLINE HashT hash(IntT i) {
    return detail::hash_one(static_cast<std::uint64_t>(i));
  }

  TESL_ALWAYS_INLINE HashT hash(FloatT f) {
    // Normalize +/- 0.0 and NaN values so they hash the same.
    if (f == 0.0f) {
      f = 0.0f;
    } else if (std::isnan(f)) {
      f = NAN;
    }

    return detail::hash_one(get_float_bits(f));
  }

  TESL_ALWAYS_INLINE HashT hash(char c) {
    return detail::hash_one(static_cast<std::uint64_t>(c));
  }

  TESL_ALWAYS_INLINE HashT hash(wchar_t c) {
    return detail::hash_one(static_cast<std::uint64_t>(c));
  }

  TESL_ALWAYS_INLINE HashT hash(char16_t c) {
    return detail::hash_one(static_cast<std::uint64_t>(c));
  }

  TESL_ALWAYS_INLINE HashT hash(char32_t c) {
    return detail::hash_one(static_cast<std::uint64_t>(c));
  }

  TESL_ALWAYS_INLINE HashT hash(const char * str) {
    return detail::hash_buffer(str, string_length(str));
  }

  TESL_ALWAYS_INLINE HashT hash(const wchar_t * str) {
    return detail::hash_buffer(str, string_length(str) * sizeof(wchar_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const char16_t * str) {
    return detail::hash_buffer(reinterpret_cast<const char *>(str), string_length(str) * sizeof(char16_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const char32_t * str) {
    return detail::hash_buffer(reinterpret_cast<const char *>(str), string_length(str) * sizeof(char32_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const CharStrView & str) {
    return detail::hash_buffer(str.data(), str.size());
  }

  TESL_ALWAYS_INLINE HashT hash(const WCharStrView & str) {
    return detail::hash_buffer(reinterpret_cast<const char *>(str.data()), str.size() * sizeof(wchar_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const Char16StrView & str) {
    return detail::hash_buffer(reinterpret_cast<const char *>(str.data()), str.size() * sizeof(char16_t));
  }

  TESL_ALWAYS_INLINE HashT hash(const Char32StrView & str) {
    return detail::hash_buffer(reinterpret_cast<const char *>(str.data()), str.size() * sizeof(char32_t));
  }
}
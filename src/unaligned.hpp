#ifndef TEXT_EDITOR_EXAMPLE_SRC_UNALIGNED_HPP_INCLUDED
#define TEXT_EDITOR_EXAMPLE_SRC_UNALIGNED_HPP_INCLUDED
#pragma once

#include <cstddef>

#pragma pack(push, 1)
template<typename T>
struct unaligned {
  template<typename U> unaligned(const unaligned<U> & v) noexcept: value(v.value) {} // NOLINT(google-explicit-constructor)
  template<typename U> unaligned(unaligned<U> && v) noexcept: value(static_cast<U &&>(v.value)) {} // NOLINT(google-explicit-constructor)
  unaligned(const T & v) noexcept: value(v) {} // NOLINT(google-explicit-constructor)
  template<typename U> unaligned & operator=(const unaligned<U> & v) & { value = v.value; return *this; }
  template<typename U> unaligned & operator=(unaligned<U> && v) & { value = static_cast<U &&>(v.value); return *this; }
  unaligned & operator=(const T & v) & { value = v; return *this; }
  template<typename U> operator U() const { return value; } // NOLINT(google-explicit-constructor)
  T value;
};
#pragma pack(pop)

static_assert(alignof(unaligned<std::max_align_t>) == 1);

#endif //TEXT_EDITOR_EXAMPLE_SRC_UNALIGNED_HPP_INCLUDED

#pragma once

#include "tesl_common.hpp"
#include "tesl_hash.hpp"
#include "tesl_array.hpp"
#include <fmt/fwd.h>
#include <string>

namespace tesl {
  template<typename T, typename Traits = std::char_traits<T>, typename Alloc = std::allocator<T>>
  using BasicStrT = std::basic_string<T, Traits, Alloc>;

  using CharStr = BasicStrT<char>;
  using WCharStr = BasicStrT<wchar_t>;
  using Char16Str = BasicStrT<char16_t>;
  using Char32Str = BasicStrT<char32_t>;
  using Str = BasicStrT<CommonCharT>;

  template<typename T>
  TESL_ALWAYS_INLINE HashT hash(BasicStrT<T> str) {
    return hash(static_cast<BasicStrViewT<T>>(str));
  }

  template<> TypeRef make_type_info<Str>();
  template<> TypeRef get_builtin_type_info_of<Str>();
}

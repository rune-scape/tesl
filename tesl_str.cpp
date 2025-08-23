#include "tesl_str.hpp"

namespace tesl {
  /*template struct BasicStrT<char>;
  template struct BasicStrT<wchar_t>;
  template struct BasicStrT<char16_t>;
  template struct BasicStrT<char32_t>;*/
  
  TESL_DEFINE_BUILTIN_TYPE_INFO_GETTER(Str, Str)
}
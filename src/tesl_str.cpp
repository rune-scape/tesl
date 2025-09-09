#include "tesl_str.hpp"

#include "tesl_bootstrap.hpp"
#include "tesl_type.hpp"

namespace tesl {
  template<>
  TypeRef make_type_info<Str>() {
    TypeRef ref = TESL_NEW_BUILTIN_TYPE_INFO(Str, "Str");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Str>() {
    static TypeRef ret = make_type_info<Str>();
    return ret;
  }
}
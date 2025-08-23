#include "tesl_type.hpp"

#include "tesl_fn.hpp"
#include "tesl_math.hpp"
#include "tesl_str.hpp"
#include "tesl_dynamic_array.hpp"
#include "tesl_dynamic_map.hpp"

namespace tesl {
  template<>
  TypeRef get_type_info_of<Null>() {
    static TypeRef ref = new_ref<TypeInfo>(
      TESL_STRVIEW("Null"),
      0,
      1,
      FnObjBase{empty_fn, nullptr},
      FnObjBase{empty_fn, nullptr},
      FnObjBase{empty_fn, nullptr},
      FnObjBase{empty_fn, nullptr}
    );
    return ref;
  }

  TESL_DEFINE_BUILTIN_TYPE_INFO_GETTER(Bool, Bool)
  TESL_DEFINE_BUILTIN_TYPE_INFO_GETTER(IntT, Int)
  TESL_DEFINE_BUILTIN_TYPE_INFO_GETTER(FloatT, Float)
  TESL_DEFINE_BUILTIN_TYPE_INFO_GETTER(TypeRef, Type)

  TypeRef get_builtin_type_info(detail::BuiltinType i) {
    switch (i) {
      case detail::builtin_Null: return get_type_info_of<Null>();
      case detail::builtin_Bool: return get_type_info_of<Bool>();
      case detail::builtin_Int: return get_type_info_of<IntT>();
      case detail::builtin_Float: return get_type_info_of<FloatT>();
      case detail::builtin_Type: return get_type_info_of<TypeRef>();
      case detail::builtin_Vec2: return get_type_info_of<Vec2>();
      case detail::builtin_Vec3: return get_type_info_of<Vec3>();
      case detail::builtin_Vec4: return get_type_info_of<Vec4>();
      case detail::builtin_Mat2: return get_type_info_of<Mat2>();
      case detail::builtin_Mat3: return get_type_info_of<Mat3>();
      case detail::builtin_Mat4: return get_type_info_of<Mat4>();
      case detail::builtin_Str: return get_type_info_of<Str>();
      case detail::builtin_Fn: return get_type_info_of<FnObj>();
      case detail::builtin_Array: return get_type_info_of<DynamicArray>();
      case detail::builtin_Map: return get_type_info_of<DynamicMap>();
      case detail::builtin_max: break;
    }

    TESL_UNREACHABLE;
  }
}
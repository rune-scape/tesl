#include "tesl_type.hpp"

#include "tesl_fn.hpp"
#include "tesl_math.hpp"
#include "tesl_str.hpp"

namespace tesl {
  namespace detail {
    template<typename T>
    void default_init(void * context, void * args, void * ret) {
      new (ret) T();
    }

    template<typename T>
    void default_copy(void * context, void * args, void * ret) {
      new (ret) T(*reinterpret_cast<const T *>(args));
    }

    template<typename T>
    void default_move(void * context, void * args, void * ret) {
      new (ret) T(MOV(*reinterpret_cast<const T *>(args)));
    }

    template<typename T>
    void default_deinit(void * context, void * args, void * ret) {
      reinterpret_cast<T *>(args)->~T();
    }

    template<typename T>
    constexpr TypeInfo make_static_type_info(StrViewT name) {
      return {
        name,
        sizeof(T),
        alignof(T),
        {default_init<T>, nullptr},
        {default_copy<T>, nullptr},
        {default_move<T>, nullptr},
        {default_deinit<T>, nullptr}
      };
    }
  }


  TypeInfo typeInfoNull{TESL_STRVIEW("Null"), 0, 1, {empty_fn, nullptr}, {empty_fn, nullptr}, {empty_fn, nullptr}, {empty_fn, nullptr}};
  TypeInfo typeInfoBool = detail::make_static_type_info<BoolT>(TESL_STRVIEW("Bool"));
  TypeInfo typeInfoInt = detail::make_static_type_info<IntT>(TESL_STRVIEW("Int"));
  TypeInfo typeInfoFloat = detail::make_static_type_info<FloatT>(TESL_STRVIEW("Float"));
  TypeInfo typeInfoVec2 = detail::make_static_type_info<Vec2>(TESL_STRVIEW("Vec2"));
  TypeInfo typeInfoVec3 = detail::make_static_type_info<Vec3>(TESL_STRVIEW("Vec3"));
  TypeInfo typeInfoVec4 = detail::make_static_type_info<Vec4>(TESL_STRVIEW("Vec4"));
  TypeInfo typeInfoMat2 = detail::make_static_type_info<Mat2>(TESL_STRVIEW("Mat2"));
  TypeInfo typeInfoMat3 = detail::make_static_type_info<Mat3>(TESL_STRVIEW("Mat3"));
  TypeInfo typeInfoMat4 = detail::make_static_type_info<Mat4>(TESL_STRVIEW("Mat4"));
  TypeInfo typeInfoStr = detail::make_static_type_info<StrT>(TESL_STRVIEW("Str"));
  TypeInfo typeInfoFn = detail::make_static_type_info<FnObj>(TESL_STRVIEW("Fn"));
}
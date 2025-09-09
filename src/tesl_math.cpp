#include "tesl_math.hpp"

#include "tesl_bootstrap.hpp"
#include "tesl_type.hpp"

namespace tesl {
  template<>
  TypeRef make_type_info<Vec2>() {
    TypeRef ref = TESL_NEW_BUILTIN_TYPE_INFO(Vec2, "Vec2");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Vec2>() {
    static TypeRef ret = make_type_info<Vec2>();
    return ret;
  }

  template<>
  TypeRef make_type_info<Vec3>() {
    TypeRef ref = TESL_NEW_BUILTIN_TYPE_INFO(Vec3, "Vec3");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Vec3>() {
    static TypeRef ret = make_type_info<Vec3>();
    return ret;
  }

  template<>
  TypeRef make_type_info<Vec4>() {
    TypeRef ref = TESL_NEW_BUILTIN_TYPE_INFO(Vec4, "Vec4");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Vec4>() {
    static TypeRef ret = make_type_info<Vec4>();
    return ret;
  }

  template<>
  TypeRef make_type_info<Mat2>() {
    TypeRef ref = TESL_NEW_BUILTIN_TYPE_INFO(Mat2, "Mat2");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Mat2>() {
    static TypeRef ret = make_type_info<Mat2>();
    return ret;
  }

  template<>
  TypeRef make_type_info<Mat3>() {
    TypeRef ref = TESL_NEW_BUILTIN_TYPE_INFO(Mat3, "Mat3");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Mat3>() {
    static TypeRef ret = make_type_info<Mat3>();
    return ret;
  }

  template<>
  TypeRef make_type_info<Mat4>() {
    TypeRef ref = TESL_NEW_BUILTIN_TYPE_INFO(Mat4, "Mat4");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Mat4>() {
    static TypeRef ret = make_type_info<Mat4>();
    return ret;
  }
}  
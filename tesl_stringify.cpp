#include "tesl_stringify.hpp"

#include "tesl_var.hpp"

namespace tesl {
  StrViewT stringify_null() {
    return TESL_STRVIEW("null");
  }

  StrViewT stringify_bool(BoolT v) {
    if (v) {
      return TESL_STRVIEW("true");
    } else {
      return TESL_STRVIEW("false");
    }
  }

  StrT stringify_int(IntT v, IntT base) {
    static detail::LrIntFormatter<IntT, StrT::CharT, 10> base_10_formatter;
    static constexpr IntT buffer_size = 66;

    assert(base >= 2 && base <= 36);

    StrT::CharT buffer[buffer_size];
    IntT write_length = 0;
    switch (base) {
      case 10: {
        write_length = base_10_formatter.print_to_buffer(v, {buffer, buffer_size});
      } break;
      default: {
        write_length = detail::print_to_buffer<IntT, StrT::CharT>(v, base, {buffer, buffer_size});
      } break;
    }

    return {buffer, write_length};
  }

  StrT stringify_float(FloatT v, FloatFormatSettings format_settings) {
    IntT buffer_size = static_cast<IntT>(std::log10(v)) + 3 + format_settings.precision;
    char buffer[buffer_size];
    IntT write_length = 0;
    snprintf(buffer, buffer_size, "%.*g", format_settings.precision, v);
    return StrT{buffer, write_length};
  }

  StrT stringify_vec2(Vec2 v, FloatFormatSettings format_settings) {
    StrT result = "Vec2(";
    result += stringify_float(v.x);
    result += ", ";
    result += stringify_float(v.y);
    result += ")";
    return result;
  }

  StrT stringify_vec3(Vec3 v, FloatFormatSettings format_settings) {
    StrT result = "Vec3(";
    result += stringify_float(v.x);
    result += ", ";
    result += stringify_float(v.y);
    result += ", ";
    result += stringify_float(v.z);
    result += ")";
    return result;
  }

  StrT stringify_vec4(Vec4 v, FloatFormatSettings format_settings) {
    StrT result = "Vec4(";
    result += stringify_float(v.x);
    result += ", ";
    result += stringify_float(v.y);
    result += ", ";
    result += stringify_float(v.z);
    result += ", ";
    result += stringify_float(v.w);
    result += ")";
    return result;
  }

  StrT stringify_mat2(Mat2 v, FloatFormatSettings format_settings) {
    StrT result = "Mat2(";
    result += stringify_vec2(v[0]);
    result += ", ";
    result += stringify_vec2(v[1]);
    result += ")";
    return result;
  }

  StrT stringify_mat3(Mat3 v, FloatFormatSettings format_settings) {
    StrT result = "Mat3(";
    result += stringify_vec3(v[0]);
    result += ", ";
    result += stringify_vec3(v[1]);
    result += ", ";
    result += stringify_vec3(v[2]);
    result += ")";
    return result;
  }

  StrT stringify_mat4(Mat4 v, FloatFormatSettings format_settings) {
    StrT result = "Mat4(";
    result += stringify_vec4(v[0]);
    result += ", ";
    result += stringify_vec4(v[1]);
    result += ", ";
    result += stringify_vec4(v[2]);
    result += ", ";
    result += stringify_vec4(v[3]);
    result += ")";
    return result;
  }



  //todo: fixme: pleaseee: need to generalize stringifying and printing to reduce code
  void print(const Vec2 & vec) {
    printv("Vec2(", vec[0], ", ", vec[1], ")");
  }
  void print(const Vec3 & vec) {
    printv("Vec3(", vec[0], ", ", vec[1], ", ", vec[2], ")");
  }
  void print(const Vec4 & vec) {
    printv("Vec4(", vec[0], ", ", vec[1], ", ", vec[2], ", ", vec[3], ")");
  }

  void print(const Mat2 & mat) {
    printv("Mat2("
      "(", mat[0][0], ", ", mat[0][1], ")" ", "
      "(", mat[1][0], ", ", mat[1][1], ")"
    ")");
  }
  void print(const Mat3 & mat) {
    printv("Mat3("
      "(", mat[0][0], ", ", mat[0][1], ", ", mat[0][2], ")" ", "
      "(", mat[1][0], ", ", mat[1][1], ", ", mat[1][2], ")" ", "
      "(", mat[2][0], ", ", mat[2][1], ", ", mat[2][2], ")"
    ")");
  }
  void print(const Mat4 & mat) {
    printv("Mat4("
      "(", mat[0][0], ", ", mat[0][1], ", ", mat[0][2], ", ", mat[0][3], ")" ", "
      "(", mat[1][0], ", ", mat[1][1], ", ", mat[1][2], ", ", mat[1][3], ")" ", "
      "(", mat[2][0], ", ", mat[2][1], ", ", mat[2][2], ", ", mat[2][3], ")" ", "
      "(", mat[3][0], ", ", mat[3][1], ", ", mat[3][2], ", ", mat[3][3], ")"
    ")");
  }

  void print(const CharStrT & str) { print(str.ptr()); }
  void print(const WCharStrT & str) { print(str.ptr()); }
  void print(const Char16StrT & str) { print(str.ptr()); }
  void print(const Char32StrT & str) { print(str.ptr()); }

  // todo: fixxx
  ///void print(const Variant & v) { print(v.stringify()); }
}

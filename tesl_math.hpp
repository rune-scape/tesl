#pragma once

#include "tesl_common.hpp"
#include "tesl_fmt_fwd.hpp"
#include "tesl_hash.hpp"
#include "tesl_type.hpp"

namespace tesl {
  namespace constants {
    constexpr FloatT pi = 3.141592653589793238462643383279502884197169399375105820974944;
    constexpr FloatT half_pi = 3.141592653589793238462643383279502884197169399375105820974944 / 2.0;
    constexpr FloatT tau = 3.141592653589793238462643383279502884197169399375105820974944 * 2.0;
    constexpr FloatT root_pi = 1.7724538509055160272981674833411451827975;
    constexpr FloatT root_half_pi = 1.253314137315500251207882642405522626503;
    constexpr FloatT root_tau = 2.506628274631000502415765284811045253007;
    constexpr FloatT e = 2.7182818284590452353602874713526624977572470936999595749669676;
    constexpr FloatT log2_e = 1.44269504088896340736;
    constexpr FloatT log10_e = 0.434294481903251827651;
    constexpr FloatT ln_2 = 0.693147180559945309417232121458176568075500134360255254;
    constexpr FloatT ln_10 = 2.30258509299404568402;
    constexpr FloatT root_2 = 1.414213562373095048801688724209698078569671875376948073;
    constexpr FloatT inv_root_2 = 0.707106781186547524401;
    constexpr FloatT euler = 0.577215664901532860606512090082402431042159335939923598805;
  }

  struct Vec2 {
    union {
      FloatT arr[2] = {0.0};
      FloatT elements[2];
      struct { FloatT x, y; };
      struct { FloatT r, g; };
    };

    FloatT & operator[](IntT i) { return arr[i]; }
    const FloatT & operator[](IntT i) const { return arr[i]; }

    constexpr Vec2() = default;
    constexpr Vec2(FloatT v0, FloatT v1) : arr{v0, v1} {}
  };
  
  struct Vec3 {
    union {
      FloatT arr[3] = {0.0};
      FloatT elements[3];
      struct { FloatT x, y, z; };
      struct { FloatT r, g, b; };
    };

    FloatT & operator[](IntT i) { return arr[i]; }
    const FloatT & operator[](IntT i) const { return arr[i]; }

    constexpr Vec3() = default;
    constexpr Vec3(FloatT v0, FloatT v1, FloatT v2) : arr{v0, v1, v2} {}
  };
  
  struct Vec4 {
    union {
      FloatT arr[4] = {0.0};
      FloatT elements[4];
      struct { FloatT x, y, z, w; };
      struct { FloatT r, g, b, a; };
    };

    FloatT & operator[](IntT i) { return arr[i]; }
    const FloatT & operator[](IntT i) const { return arr[i]; }

    constexpr Vec4() = default;
    constexpr Vec4(FloatT v0, FloatT v1, FloatT v2, FloatT v3) : arr{v0, v1, v2, v3} {}
  };
  
  struct Mat2 {
    union {
      Vec2 arr[2]{
        {1.0, 0.0},
        {0.0, 1.0}
      };
      FloatT elements[2 * 2];
    };

    Vec2 & operator[](IntT i) { return arr[i]; }
    const Vec2 & operator[](IntT i) const { return arr[i]; }

    constexpr Mat2() = default;
  };
  
  struct Mat3 {
    union {
      Vec3 arr[3]{
        {1.0, 0.0, 0.0},
        {0.0, 1.0, 0.0},
        {0.0, 0.0, 1.0}
      };
      FloatT elements[3 * 3];
    };

    Vec3 & operator[](IntT i) { return arr[i]; }
    const Vec3 & operator[](IntT i) const { return arr[i]; }

    constexpr Mat3() = default;
  };
  
  struct Mat4 {
    union {
      Vec4 arr[4]{
        {1.0, 0.0, 0.0, 0.0},
        {0.0, 1.0, 0.0, 0.0},
        {0.0, 0.0, 1.0, 0.0},
        {0.0, 0.0, 0.0, 1.0}
      };
      FloatT elements[4 * 4];
    };

    Vec4 & operator[](IntT i) { return arr[i]; }
    const Vec4 & operator[](IntT i) const { return arr[i]; }

    constexpr Mat4() = default;
  };

  TESL_ALWAYS_INLINE HashT hash(Vec2 v) {
    HashT h = detail::hash_one(get_float_bits(v.elements[0]));
    for (IntT i = 1; i < 2; ++i) {
      h = detail::hash_mix(get_float_bits(v.elements[i]), h);
    }
    return h;
  }

  TESL_ALWAYS_INLINE HashT hash(Vec3 v) {
    HashT h = detail::hash_one(get_float_bits(v.elements[0]));
    for (IntT i = 1; i < 3; ++i) {
      h = detail::hash_mix(get_float_bits(v.elements[i]), h);
    }
    return h;
  }

  TESL_ALWAYS_INLINE HashT hash(Vec4 v) {
    HashT h = detail::hash_one(get_float_bits(v.elements[0]));
    for (IntT i = 1; i < 4; ++i) {
      h = detail::hash_mix(get_float_bits(v.elements[i]), h);
    }
    return h;
  }

  TESL_ALWAYS_INLINE HashT hash(Mat2 v) {
    HashT h = detail::hash_one(get_float_bits(v.elements[0]));
    for (IntT i = 1; i < 4; ++i) {
      h = detail::hash_mix(get_float_bits(v.elements[i]), h);
    }
    return h;
  }

  TESL_ALWAYS_INLINE HashT hash(Mat3 v) {
    HashT h = detail::hash_one(get_float_bits(v.elements[0]));
    for (IntT i = 1; i < 9; ++i) {
      h = detail::hash_mix(get_float_bits(v.elements[i]), h);
    }
    return h;
  }

  TESL_ALWAYS_INLINE HashT hash(Mat4 v) {
    HashT h = detail::hash_one(get_float_bits(v.elements[0]));
    for (IntT i = 1; i < 16; ++i) {
      h = detail::hash_mix(get_float_bits(v.elements[i]), h);
    }
    return h;
  }

  template<> inline const TypeInfo * type_info_of<Vec2> = &typeInfoVec2;
  template<> inline const TypeInfo * type_info_of<Vec3> = &typeInfoVec3;
  template<> inline const TypeInfo * type_info_of<Vec4> = &typeInfoVec4;
  template<> inline const TypeInfo * type_info_of<Mat2> = &typeInfoMat2;
  template<> inline const TypeInfo * type_info_of<Mat3> = &typeInfoMat3;
  template<> inline const TypeInfo * type_info_of<Mat4> = &typeInfoMat4;
}

template<typename CharT>
class fmt::formatter<tesl::Vec2, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Vec2 & v, Context & ctx) const {
    return format_to(ctx.out(), "Vec2({}, {})", v.x, v.y);
  }
};

template<typename CharT>
class fmt::formatter<tesl::Vec3, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Vec3 & v, Context & ctx) const {
    return format_to(ctx.out(), "Vec3({}, {}, {})", v.x, v.y, v.z);
  }
};

template<typename CharT>
class fmt::formatter<tesl::Vec4, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Vec4 & v, Context & ctx) const {
    return format_to(ctx.out(), "Vec4({}, {}, {}, {})", v.x, v.y, v.z, v.w);
  }
};

template<typename CharT>
class fmt::formatter<tesl::Mat2, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Mat2 & m, Context & ctx) const {
    return format_to(ctx.out(), "Mat2({}, {})", m[0], m[1]);
  }
};

template<typename CharT>
class fmt::formatter<tesl::Mat3, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Mat3 & m, Context & ctx) const {
    return format_to(ctx.out(), "Mat3({}, {}, {})", m[0], m[1], m[2]);
  }
};

template<typename CharT>
class fmt::formatter<tesl::Mat4, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Mat4 & m, Context & ctx) const {
    return format_to(ctx.out(), "Mat4({}, {}, {}, {})", m[0], m[1], m[2], m[3]);
  }
};

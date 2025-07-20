#pragma once

#include "tesl_common.hpp"
#include "tesl_fmt_fwd.hpp"

namespace tesl {
  struct TypeInfo {
    StrView name;
    IntT size;
    IntT align;

    // initializes the data at the return address (no args)
    FnObjBase init;

    // copies the argument into the return address
    FnObjBase copy;

    // moves the argument into the return address
    FnObjBase move;

    // deinitializes the argument (no return)
    FnObjBase deinit;

    void * allocate() const {
      return operator new(size);
    }

    void * new_() const {
      void * ret = allocate();
      init(nullptr, ret);
      return ret;
    }

    void delete_(void * ptr) const {
      deinit(ptr, nullptr);
      operator delete(ptr);
    }
  };

  extern TypeInfo typeInfoNull;
  extern TypeInfo typeInfoBool;
  extern TypeInfo typeInfoInt;
  extern TypeInfo typeInfoFloat;
  extern TypeInfo typeInfoVec2;
  extern TypeInfo typeInfoVec3;
  extern TypeInfo typeInfoVec4;
  extern TypeInfo typeInfoMat2;
  extern TypeInfo typeInfoMat3;
  extern TypeInfo typeInfoMat4;
  extern TypeInfo typeInfoStr;
  extern TypeInfo typeInfoFn;

  template<typename T> inline const TypeInfo * type_info_of;
  template<> inline const TypeInfo * type_info_of<Null> = &typeInfoNull;
  template<> inline const TypeInfo * type_info_of<Bool> = &typeInfoBool;
  template<> inline const TypeInfo * type_info_of<IntT> = &typeInfoInt;
  template<> inline const TypeInfo * type_info_of<FloatT> = &typeInfoFloat;

  inline auto format_as(const tesl::TypeInfo * t) { return *t; }
}

template<typename CharT>
class fmt::formatter<tesl::TypeInfo, CharT> {
public:
  template<typename Context>
  constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context>
  constexpr auto format(const tesl::TypeInfo & type, Context & ctx) const {
    return format_to(ctx.out(), "{}", type.name);
  }
};

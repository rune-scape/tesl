#pragma once

#include "tesl_common.hpp"

namespace tesl {
  struct TypeInfo {
    StrViewT name;
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
  template<> inline const TypeInfo * type_info_of<NullT> = &typeInfoNull;
  template<> inline const TypeInfo * type_info_of<BoolT> = &typeInfoBool;
  template<> inline const TypeInfo * type_info_of<IntT> = &typeInfoInt;
  template<> inline const TypeInfo * type_info_of<FloatT> = &typeInfoFloat;
}

#pragma once

#include "tesl_common.hpp"
#include "tesl_ref.hpp"
#include <fmt/fwd.h>

namespace tesl {
  // list of builtin types to help bootstrap type system
  namespace detail {
    enum BuiltinType {
      builtin_Null = 0,
      builtin_Bool,
      builtin_Int,
      builtin_Float,
      builtin_Vec2,
      builtin_Vec3,
      builtin_Vec4,
      builtin_Mat2,
      builtin_Mat3,
      builtin_Mat4,
      builtin_Str,
      builtin_Fn,
      builtin_Type,
      builtin_Array,
      builtin_Map,
      builtin_max
    };
  }

  struct TypeInfo : RefCounted<TypeInfo> {
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
      return operator new(size, std::align_val_t{static_cast<size_t>(align)});
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

    TESL_ALWAYS_INLINE bool operator==(const TypeInfo & other) const { return this == &other; }

    TypeInfo() = delete;
    TypeInfo(const TypeInfo &) = delete;
    TypeInfo(TypeInfo &&) = delete;
    TypeInfo & operator=(const TypeInfo &) = delete;
    TypeInfo & operator=(TypeInfo &&) = delete;

    constexpr TypeInfo(
        StrView pName,
        IntT pSize,
        IntT pAlign,
        FnObjBase pInit,
        FnObjBase pCopy,
        FnObjBase pMove,
        FnObjBase pDeinit)
      : name(pName),
        size(pSize),
        align(pAlign),
        init(pInit),
        copy(pCopy),
        move(pMove),
        deinit(pDeinit) {}
  };

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
    Ref<TypeInfo> new_default_type_info(StrView name) {
      return new_ref<TypeInfo>(
        name,
        sizeof(T),
        alignof(T),
        FnObjBase{default_init<T>, nullptr},
        FnObjBase{default_copy<T>, nullptr},
        FnObjBase{default_move<T>, nullptr},
        FnObjBase{default_deinit<T>, nullptr}
      );
    }
  }

  TypeRef get_builtin_type_info(detail::BuiltinType i);

  inline const tesl::TypeInfo & format_as(TypeRef t) { return *t; }
  inline const tesl::TypeInfo & format_as(const TypeInfo * t) { return *t; }

#define TESL_DEFINE_BUILTIN_TYPE_INFO_GETTER(type, name) \
  template<> \
  TypeRef get_type_info_of<type>() { \
    static TypeRef ref = detail::new_default_type_info<type>(TESL_STRVIEW(#name)); \
    return ref; \
  }
  
  TESL_DECLARE_BUILTIN_TYPE_INFO_GETTER(Null, Null)
  TESL_DECLARE_BUILTIN_TYPE_INFO_GETTER(Bool, Bool)
  TESL_DECLARE_BUILTIN_TYPE_INFO_GETTER(IntT, Int)
  TESL_DECLARE_BUILTIN_TYPE_INFO_GETTER(FloatT, Float)
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

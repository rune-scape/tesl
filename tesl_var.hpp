#pragma once

#include "tesl_common.hpp"

#include "tesl_type.hpp"

namespace tesl {
  struct Variant {
    const TypeInfo * type = type_info_of<NullT>;
    char _storage[variant_storage_size] = {0};
    static_assert(variant_storage_size >= sizeof(void *), "variant storage must be big enough to store a pointer!");

    TESL_ALWAYS_INLINE constexpr void * _get_ptr_to_storage() const { return (void *)_storage; }
    TESL_ALWAYS_INLINE void *& _get_storage_as_ptr() { return *((void **)_storage); }
    TESL_ALWAYS_INLINE void * _get_storage_as_ptr() const { return *((void **)_storage); }

    TESL_ALWAYS_INLINE void * _get_ptr_to_data() const {
      if (type->size <= variant_storage_size) {
        return _get_ptr_to_storage();
      } else {
        return _get_storage_as_ptr();
      }
    }

    template<typename T>
    TESL_ALWAYS_INLINE T & get() {
      return *reinterpret_cast<T *>(_get_ptr_to_data());
    }

    template<typename T>
    TESL_ALWAYS_INLINE const T & get() const {
      return *reinterpret_cast<const T *>(_get_ptr_to_data());
    }

    TESL_ALWAYS_INLINE Variant & operator=(const Variant & other) {
      // todo: maybe delay destructor
      if (type->size == other.type->size) {
        void * data_ptr = _get_ptr_to_data();
        type->deinit(data_ptr, nullptr);
        type = other.type;
      } else {
        this->~Variant();
        type = other.type;
        if (other.type->size > variant_storage_size) {
          _get_storage_as_ptr() = other.type->allocate();
        }
      }

      if (type->size <= variant_storage_size) {
        type->copy(other._get_ptr_to_storage(), _get_ptr_to_storage());
      } else {
        type->copy(other._get_storage_as_ptr(), _get_storage_as_ptr());
      }
      return *this;
    }

    TESL_ALWAYS_INLINE Variant & operator=(Variant && other) {
      // todo: maybe delay destructor
      if (type->size == other.type->size) {
        void * data_ptr = _get_ptr_to_data();
        type->deinit(data_ptr, nullptr);
        type = other.type;
      } else {
        this->~Variant();
        type = other.type;
        if (other.type->size > variant_storage_size) {
          _get_storage_as_ptr() = other.type->allocate();
        }
      }

      if (type->size <= variant_storage_size) {
        type->move(other._get_ptr_to_storage(), _get_ptr_to_storage());
      } else {
        type->move(other._get_storage_as_ptr(), _get_storage_as_ptr());
      }
      return *this;
    }

    TESL_ALWAYS_INLINE Variant(const Variant & other) { this->operator=(other); }
    TESL_ALWAYS_INLINE Variant(Variant && other) { this->operator=(MOV(other)); }
    TESL_ALWAYS_INLINE constexpr Variant() = default;

    template<typename T>
    constexpr Variant(T v) : type(type_info_of<T>) {
      if (type->size <= variant_storage_size) {
        type->init(nullptr, _get_ptr_to_storage());
        type->move((void *)&v, _get_ptr_to_storage());
      } else {
        type->move((void *)&v, _get_storage_as_ptr() = type->new_());
      }
    }

    ~Variant() {
      if (type->size <= variant_storage_size) {
        type->deinit(_get_ptr_to_storage(), nullptr);
      } else {
        type->delete_(_get_storage_as_ptr());
      }
      type = type_info_of<NullT>;
    }
  };

  /*namespace detail {
    template<Type T>
    struct type_of_impl { using type = typename type_of_impl<Type(T | TYPE_FLAG_CONSTANT)>::type *; };
    template<>
    struct type_of_impl<TYPE_NULL_VAL> { using type = void; };
    template<>
    struct type_of_impl<TYPE_FLOAT_VAL> { using type = FloatT; };
    template<>
    struct type_of_impl<TYPE_VEC2_VAL> { using type = Vec2; };
    template<>
    struct type_of_impl<TYPE_VEC3_VAL> { using type = Vec3; };
    template<>
    struct type_of_impl<TYPE_VEC4_VAL> { using type = Vec4; };
    template<>
    struct type_of_impl<TYPE_INT_VAL> { using type = IntT; };
    template<>
    struct type_of_impl<TYPE_MAT2_VAL> { using type = Mat2; };
    template<>
    struct type_of_impl<TYPE_MAT3_VAL> { using type = Mat3; };
    template<>
    struct type_of_impl<TYPE_MAT4_VAL> { using type = Mat4; };
    template<>
    struct type_of_impl<TYPE_STR_VAL> { using type = StrT; };

    template<>
    struct type_of_impl<TYPE_FUNCTION> { using type = FnObj; };
  } // namespace detail

  template<Type T> using type_of = typename detail::type_of_impl<T>::type;

  template<Type Ty> constexpr bool is_ref(Type type) { return !(type & TYPE_FLAG_CONSTANT); }
  template<Type Ty> constexpr bool is_mat(Type type) { return type == TYPE_MAT2_VAL || type == TYPE_MAT3_VAL || type == TYPE_MAT4_VAL; }
  template<Type Ty> constexpr bool is_vec(Type type) { return type == TYPE_VEC2_VAL || type == TYPE_VEC3_VAL || type == TYPE_VEC4_VAL; }

  template<typename T>
  T value_get(const ValueUnionT & val);
  template<>
  TESL_ALWAYS_INLINE void value_get<void>(const ValueUnionT & val) {}

  template<Type Ty>
  TESL_ALWAYS_INLINE type_of<Type(Ty | TYPE_FLAG_CONSTANT)> value_deref(const ValueUnionT & val);

  template<typename T>
  TESL_ALWAYS_INLINE constexpr void value_set(ValueUnionT & tev, const T & v);

  template<typename T>
  TESL_ALWAYS_INLINE constexpr TypedValueUnionT make_value(const T & v);

#define MAKE_VALUE_IMPL(ty, member_name, ref_ty, ref_member_name)\
  MAKE_VALUE_CONST_IMPL(ty, member_name)\
  MAKE_VALUE_REF_IMPL(ref_ty, ref_member_name)
#define MAKE_VALUE_CONST_IMPL(ty, member_name)\
  template<> TESL_ALWAYS_INLINE constexpr type_of<ty> value_get<type_of<ty>>(const ValueUnionT &val) { return val.member_name; }\
  template<> TESL_ALWAYS_INLINE constexpr type_of<ty> value_deref<ty>(const ValueUnionT &val) { return val.member_name; }\
  template<> TESL_ALWAYS_INLINE constexpr void value_set<type_of<ty>>(ValueUnionT &tev, const type_of<ty> & v) { tev.member_name = v; }\
  template<> TESL_ALWAYS_INLINE constexpr TypedValueUnionT make_value<type_of<ty>>(const type_of<ty> & v) { return {ValueUnionT{v}, ty}; }
#define MAKE_VALUE_REF_IMPL(ref_ty, ref_member_name)\
  template<> TESL_ALWAYS_INLINE constexpr type_of<ref_ty> value_get<type_of<ref_ty>>(const ValueUnionT &val) { return val.ref_member_name; }\
  template<> TESL_ALWAYS_INLINE constexpr type_of<Type(ref_ty | TYPE_FLAG_CONSTANT)> value_deref<ref_ty>(const ValueUnionT &val) { return *val.ref_member_name; }\
  template<> TESL_ALWAYS_INLINE constexpr void value_set<type_of<ref_ty>>(ValueUnionT &tev, const type_of<ref_ty> & v) { tev.ref_member_name = v; }\
  template<> TESL_ALWAYS_INLINE constexpr TypedValueUnionT make_value<type_of<ref_ty>>(const type_of<ref_ty> & v) { return {ValueUnionT{v}, ref_ty}; }

  MAKE_VALUE_IMPL(TYPE_INT_VAL, int_, TYPE_INT_REF, int_ref)
  MAKE_VALUE_IMPL(TYPE_FLOAT_VAL, float_, TYPE_FLOAT_REF, float_ref)
  MAKE_VALUE_IMPL(TYPE_VEC2_VAL, vec2, TYPE_VEC2_REF, vec2_ref)
  MAKE_VALUE_IMPL(TYPE_VEC3_VAL, vec3, TYPE_VEC3_REF, vec3_ref)
  MAKE_VALUE_IMPL(TYPE_VEC4_VAL, vec4, TYPE_VEC4_REF, vec4_ref)
  MAKE_VALUE_IMPL(TYPE_MAT2_VAL, mat2, TYPE_MAT2_REF, mat2_ref)
  MAKE_VALUE_IMPL(TYPE_MAT3_VAL, mat3, TYPE_MAT3_REF, mat3_ref)
  MAKE_VALUE_IMPL(TYPE_MAT4_VAL, mat4, TYPE_MAT4_REF, mat4_ref)
  MAKE_VALUE_IMPL(TYPE_STR_VAL, str, TYPE_STR_REF, str_ref)


#undef MAKE_VALUE_IMPL*/

  void print(const Variant & value);

  // many definitions, for robustness
  TESL_ALWAYS_INLINE void print(const TypeInfo & type) { print(type.name); }
  TESL_ALWAYS_INLINE void print(TypeInfo & type) { print(type.name); }
  TESL_ALWAYS_INLINE void print(const TypeInfo * type) { print(type->name); }
  TESL_ALWAYS_INLINE void print(TypeInfo * type) { print(type->name); }
}

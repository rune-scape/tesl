#pragma once

#include "tesl_common.hpp"

#include "tesl_ref.hpp"
#include "tesl_type.hpp"

namespace tesl {
  struct VarRef {
    void * data;
    TypeRef type;
  };

  struct Variant {
    union Storage {
      void * ptr = nullptr;
      char data[variant_storage_size];
    };

    Storage _storage;
    TypeRef _type = get_builtin_type_info_of<Null>();

    TESL_ALWAYS_INLINE void * _get_ptr_to_storage() { return reinterpret_cast<void *>(&_storage); }
    TESL_ALWAYS_INLINE const void * _get_ptr_to_storage() const { return reinterpret_cast<const void *>(&_storage); }
    TESL_ALWAYS_INLINE void * & _get_storage_as_ptr() { return _storage.ptr; }
    TESL_ALWAYS_INLINE const void * const & _get_storage_as_ptr() const { return _storage.ptr; }

    TESL_ALWAYS_INLINE void * _get_ptr_to_data() {
      if (_type->size <= variant_storage_size) {
        return _get_ptr_to_storage();
      } else {
        return _get_storage_as_ptr();
      }
    }

    TESL_ALWAYS_INLINE const void * _get_ptr_to_data() const {
      if (_type->size <= variant_storage_size) {
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
    
    TESL_ALWAYS_INLINE TypeRef get_type() const {
      return _type;
    }

    void clear();
    operator VarRef();
    operator VarRef() const;

    Variant & operator=(const VarRef & other);
    Variant & operator=(const Variant & other);
    Variant & operator=(Variant && other);

    Variant(const VarRef & other);
    Variant(const Variant & other);
    Variant(Variant && other);
    TESL_ALWAYS_INLINE Variant() = default;

    template<typename T, typename = std::enable_if_t<!std::is_same_v<remove_cvref_t<T>, Variant> > >
    Variant(T && v) : _type(get_builtin_type_info_of<remove_cvref_t<T> >()) {
      void * other_ptr = const_cast<void *>(reinterpret_cast<const void *>(&v));
      if (_type->size <= variant_storage_size) {
        void * self_ptr = _get_ptr_to_storage();
        _type->init(self_ptr);
        if constexpr (std::is_rvalue_reference_v<T>) {
          _type->move(other_ptr, self_ptr);
        } else {
          _type->copy(other_ptr, self_ptr);
        }
      } else {
        void * & self_ptr = _get_storage_as_ptr();
        self_ptr = _type->new_();
        if constexpr (std::is_rvalue_reference_v<T>) {
          _type->move(other_ptr, self_ptr);
        } else {
          _type->copy(other_ptr, self_ptr);
        }
      }
    }

    ~Variant();
  };

  template<> TypeRef make_type_info<Variant>();
  template<> TypeRef get_builtin_type_info_of<Variant>();
}

template<typename CharT>
class fmt::formatter<tesl::Variant, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Variant & v, Context & ctx) const {
    // todo: fix when dynamic methods are implemented, after 'stringify' method is made
    return format_to(ctx.out(), "Variant({})", v.get_type());
  }
};

#pragma once

#include "tesl_common.hpp"

#include "tesl_fn.hpp"
#include "tesl_result.hpp"
#include "tesl_type.hpp"

namespace tesl {
  struct Variant {
    union Storage {
      void * ptr = nullptr;
      std::aligned_storage_t<variant_storage_size> data;
    };

    mutable Storage _storage;
    Type _type = get_type_of<Null>();

    TESL_ALWAYS_INLINE void * _get_ptr_to_storage() { return reinterpret_cast<void *>(&_storage.data); }
    TESL_ALWAYS_INLINE const void * _get_ptr_to_storage() const { return reinterpret_cast<const void *>(&_storage.data); }
    TESL_ALWAYS_INLINE void * & _get_storage_as_ptr() { return _storage.ptr; }
    TESL_ALWAYS_INLINE const void * const & _get_storage_as_ptr() const { return _storage.ptr; }

    TESL_ALWAYS_INLINE void * _get_ptr_to_data() {
      if (_type.size() <= variant_storage_size) {
        return _get_ptr_to_storage();
      } else {
        return _get_storage_as_ptr();
      }
    }

    TESL_ALWAYS_INLINE const void * _get_ptr_to_data() const {
      if (_type.size() <= variant_storage_size) {
        return _get_ptr_to_storage();
      } else {
        return _get_storage_as_ptr();
      }
    }

    template<typename T>
    TESL_ALWAYS_INLINE T & _get_unchecked() {
      return *reinterpret_cast<T *>(_get_ptr_to_data());
    }

    template<typename T>
    TESL_ALWAYS_INLINE const T & _get_unchecked() const {
      return *reinterpret_cast<const T *>(_get_ptr_to_data());
    }

    TESL_ALWAYS_INLINE Type get_type() const {
      return _type;
    }

    void clear();

    Result<Null, CallError> call_method(Signature signature, void * args, void * ret) const;
    void format(FmtParseContext & parse_ctx, FmtFormatContext & ctx) const;
    operator VarRef();
    operator VarRef() const;

    Variant & operator=(const VarRef & other);
    Variant & operator=(const Variant & other);
    Variant & operator=(Variant && other);

    Variant(const VarRef & other);
    Variant(const Variant & other);
    Variant(Variant && other);
    TESL_ALWAYS_INLINE Variant() {}

    template<typename T, typename = std::enable_if_t<!std::is_same_v<std::remove_cvref_t<T>, Variant> && !std::is_same_v<std::remove_cvref_t<T>, VarRef> > >
    Variant(T && v) : _type(get_type_of<std::remove_cvref_t<T> >()) {
      constexpr bool should_move = !std::is_reference_v<T> || std::is_rvalue_reference_v<T>;
      void * other_ptr = const_cast<void *>(reinterpret_cast<const void *>(&v));
      if (_type.size() <= variant_storage_size) {
        void * self_ptr = _get_ptr_to_storage();
        if constexpr (std::is_rvalue_reference_v<T>) {
          _type.move_init(self_ptr, other_ptr);
        } else {
          _type.copy_init(self_ptr, other_ptr);
        }
      } else {
        void * & self_ptr = _get_storage_as_ptr();
        self_ptr = _type.allocate().ptr;
        if constexpr (std::is_rvalue_reference_v<T>) {
          _type.move_init(self_ptr, other_ptr);
        } else {
          _type.copy_init(self_ptr, other_ptr);
        }
      }
    }

    ~Variant();
  };

  template<> void bind_type_info<Variant>(TypeInfo & type);
}

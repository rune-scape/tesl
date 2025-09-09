#include "tesl_var.hpp"

#include "tesl_bootstrap.hpp"

namespace tesl {
  /*Str Variant::stringify() const {
    //todo: dynamic call to stringify ,,, when i implement it all the stuff for it..;
  }*/

  void Variant::clear() {
    if (_type->size <= variant_storage_size) {
      _type->deinit(_get_ptr_to_storage());
    } else {
      _type->delete_(_get_storage_as_ptr());
    }
    _type = get_builtin_type_info_of<Null>();
  }

  Variant::operator VarRef() {
    return {_get_ptr_to_data(), _type};
  }

  Variant::operator VarRef() const {
    return {const_cast<void *>(_get_ptr_to_data()), _type};
  }

// todo: maybe delay destructor
#define TESL_VARIANT_COPY_ASSIGN_IMPL(copy_fn, other_type, other_ptr_to_storage, other_storage_as_ptr) \
  if (_type->size <= variant_storage_size) { \
    if (other_type->size <= variant_storage_size) { \
      _type->deinit(_get_ptr_to_storage()); \
    } else { \
      _type->deinit(_get_ptr_to_storage()); \
      _get_storage_as_ptr() = other_type->allocate(); \
    } \
  } else { \
    if (other_type->size <= variant_storage_size) { \
      _type->delete_(_get_storage_as_ptr()); \
    } else { \
      _type->deinit(_get_storage_as_ptr()); \
    } \
  } \
  _type = other_type; \
  if (_type->size <= variant_storage_size) { \
    _type->copy_fn(other_ptr_to_storage, _get_ptr_to_storage()); \
  } else { \
    _type->copy_fn(other_storage_as_ptr, _get_storage_as_ptr()); \
  }

  Variant & Variant::operator=(const VarRef & other) {
    TESL_VARIANT_COPY_ASSIGN_IMPL(copy, other.type, other.data, other.data)
    return *this;
  }

  Variant & Variant::operator=(const Variant & other) {
    TESL_VARIANT_COPY_ASSIGN_IMPL(copy, other._type, const_cast<void *>(other._get_ptr_to_storage()), const_cast<void *>(other._get_storage_as_ptr()))
    return *this;
  }

  Variant & Variant::operator=(Variant && other) {
    TESL_VARIANT_COPY_ASSIGN_IMPL(move, other._type, other._get_ptr_to_storage(), other._get_storage_as_ptr())
    return *this;
  }

#undef TESL_VARIANT_COPY_ASSIGN_IMPL

#define TESL_VARIANT_COPY_CONSTRUCT_IMPL(copy_fn, other_ptr_to_storage, other_storage_as_ptr) \
  if (_type->size <= variant_storage_size) { \
    void * self_ptr = _get_ptr_to_storage(); \
    _type->init(self_ptr); \
    _type->copy(other_ptr_to_storage, self_ptr); \
  } else { \
    void * & self_ptr = _get_storage_as_ptr(); \
    self_ptr = _type->new_(); \
    _type->copy(other_storage_as_ptr, self_ptr); \
  }

  Variant::Variant(const VarRef & other) : _type(other.type) {
    TESL_VARIANT_COPY_CONSTRUCT_IMPL(copy, other.data, other.data)
  }

  Variant::Variant(const Variant & other) : _type(other._type) {
    TESL_VARIANT_COPY_CONSTRUCT_IMPL(copy, const_cast<void *>(other._get_ptr_to_storage()), const_cast<void *>(other._get_storage_as_ptr()))
  }

  Variant::Variant(Variant && other) : _type(other._type) {
    TESL_VARIANT_COPY_CONSTRUCT_IMPL(move, other._get_ptr_to_storage(), other._get_storage_as_ptr())
  }

#undef TESL_VARIANT_COPY_CONSTRUCT_IMPL

  Variant::~Variant() {
    clear();
  }

  template<>
  TypeRef make_type_info<Variant>() {
    TypeRef ref = TESL_NEW_BUILTIN_TYPE_INFO(Variant, "Variant");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Variant>() {
    static TypeRef ret = make_type_info<Variant>();
    return ret;
  }
}

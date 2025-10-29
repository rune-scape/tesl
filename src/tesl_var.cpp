#include "tesl_var.hpp"

#include "tesl_builtins.hpp"
#include "tesl_var_ref.hpp"

namespace tesl {
  Result<Null, CallError> Variant::call_method(Signature signature, void * args, void * ret) const {
    return _type.call_method(signature, const_cast<void *>(_get_ptr_to_data()), args, ret);
  }

  void Variant::format(FmtParseContext & parse_ctx, FmtFormatContext & ctx) const {
    FmtFormatter formatter{&parse_ctx, &ctx};
    auto result = call_method(TESL_BUILTIN_SIGNATURE("format(fmt:Formatter)"), &formatter, nullptr);
    if (result.is_err()) {
      ctx.advance_to(format_to(ctx.out(), "<{}@{:p}>", _type, _get_ptr_to_data()));
    }
  }

  void Variant::clear() {
    Type old_type = _type;
    _type = get_type_of<Null>();
    auto type_size = old_type.size();
    if (type_size <= variant_storage_size) {
      old_type.deinit(_get_ptr_to_storage());
    } else {
      old_type.delete_({_get_storage_as_ptr(), type_size});
    }
  }

  Variant::operator VarRef() {
    return {_get_ptr_to_data(), _type};
  }

  Variant::operator VarRef() const {
    return {const_cast<void *>(_get_ptr_to_data()), _type};
  }

// todo: maybe delay destructor
#define TESL_VARIANT_COPY_ASSIGN_IMPL(copy_fn, type, other_type, other_ptr_to_storage, other_storage_as_ptr) \
  auto type_size = type.size(); \
  auto other_type_size = other_type.size(); \
  if (type_size <= variant_storage_size) { \
    if (other_type_size <= variant_storage_size) { \
      type.deinit(_get_ptr_to_storage()); \
    } else { \
      type.deinit(_get_ptr_to_storage()); \
      _get_storage_as_ptr() = other_type.allocate().ptr; \
    } \
  } else { \
    if (other_type_size <= variant_storage_size) { \
      type.delete_({_get_storage_as_ptr(), type_size}); \
    } else { \
      type.deinit(_get_storage_as_ptr()); \
    } \
  } \
  type = other_type; \
  if (other_type_size <= variant_storage_size) { \
    type.copy_fn(_get_ptr_to_storage(), other_ptr_to_storage); \
  } else { \
    type.copy_fn(_get_storage_as_ptr(), other_storage_as_ptr); \
  }

  Variant & Variant::operator=(const VarRef & other) {
    TESL_VARIANT_COPY_ASSIGN_IMPL(copy_init, _type, other.type, other.data, other.data)
    return *this;
  }

  Variant & Variant::operator=(Variant && other) {
    TESL_VARIANT_COPY_ASSIGN_IMPL(move_init, _type, other._type, other._get_ptr_to_storage(), other._get_storage_as_ptr())
    return *this;
  }

  Variant & Variant::operator=(const Variant & other) {
    return this->operator=(other.operator tesl::VarRef());
  }

#undef TESL_VARIANT_COPY_ASSIGN_IMPL

#define TESL_VARIANT_COPY_CONSTRUCT_IMPL(copy_fn, type, other_ptr_to_storage, other_storage_as_ptr) \
  if (type.size() <= variant_storage_size) { \
    void * self_ptr = _get_ptr_to_storage(); \
    type.copy_fn(self_ptr, other_ptr_to_storage); \
  } else { \
    void * & self_ptr = _get_storage_as_ptr(); \
    self_ptr = type.allocate().ptr; \
    type.copy_fn(self_ptr, other_storage_as_ptr); \
  }

  Variant::Variant(const VarRef & other) : _type(other.type) {
    TESL_VARIANT_COPY_CONSTRUCT_IMPL(copy_init, _type, other.data, other.data)
  }

  Variant::Variant(Variant && other) : _type(other._type) {
    TESL_VARIANT_COPY_CONSTRUCT_IMPL(move_init, _type, other._get_ptr_to_storage(), other._get_storage_as_ptr())
  }

  Variant::Variant(const Variant & other) : Variant(other.operator tesl::VarRef()) {}

#undef TESL_VARIANT_COPY_CONSTRUCT_IMPL

  Variant::~Variant() {
    clear();
  }

  template<>
  void bind_type_info<Variant>(TypeInfo & type) {
    // todo: finish
  }
}

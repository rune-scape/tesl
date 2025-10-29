#include "tesl_type.hpp"

#include "tesl_builtins.hpp"
#include "tesl_env.hpp"
#include "tesl_fn.hpp"
#include "tesl_member_function.hpp"
#include "tesl_member_variable.hpp"
#include "tesl_symbol.hpp"

namespace tesl {
  Result<Null, CallError> TypeInfo::coerce(Type type, void * this_, void * ret) const {
    return type.call_method(TESL_BUILTIN_SIGNATURE("->"), this_, nullptr, ret);
  }

  bool TypeInfo::has_method(Signature signature) const {
    return get_member_function(signature).has_value();
  }

  void TypeInfo::format(FmtParseContext & parse_ctx, FmtFormatContext & ctx) const {
    ctx.advance_to(format_to(ctx.out(), "{}", tesl::Name{global_name_symbol}));
  }

  Result<Null, CallError> TypeInfo::call_method(GlobalTypeSymbol type_symbol, Signature signature, void * this_, void * args, void * ret) const {
    auto fn = get_member_function(signature);
    if (!fn.has_value()) {
      return CallError::could_not_find(signature, {type_symbol, false});
    }
    
    return fn.unwrap().operator()(this_, args, ret);
  }

  Optional<const MemberVariable &> TypeInfo::get_member_variable(Name name) const {
    return _member_variables.get(name.symbol);
  }

  Optional<const MemberFunction &> TypeInfo::get_member_function(Signature signature) const {
    return _member_functions.get(signature.symbol);
  }

#define TESL_BUILTIN_TYPE_DEF(type, local_name, global_name) \
  template<> \
  Type get_type_of<type>() { \
    return Type::make_value(builtin::get_type_symbol_of<type>()); \
  }
#include "tesl_builtin_types.inc"

  TypeInfo::TypeInfo(TypeInfoData p_type_info_data) : TypeInfoData(MOV(p_type_info_data)), _hash(hash(Name{p_type_info_data.global_name_symbol})) {}
  TypeInfo::TypeInfo() = default;
  TypeInfo::~TypeInfo() = default;

  TypeInfo & Type::get_data() const {
    return Env::current->get_type_data(symbol).unwrap();
  }

  bool Type::is_valid() const {
    return Env::current->get_type_data(symbol).has_value();
  }

  void Type::format(FmtParseContext & parse_ctx, FmtFormatContext & ctx) const {
    if (!is_valid()) {
      ctx.advance_to(format_to(ctx.out(), "<invalid-type>"));
    }

    //if (type.is_ref) {
    //  return format_to(ctx.out(), "& {}", type.global_name());
    //} else {
      ctx.advance_to(format_to(ctx.out(), "{}", global_name()));
    //}
  }

  Result<Null, CallError> Type::call_method(Signature signature, void * this_, void * args, void * ret) const {
    return get_data().call_method(symbol, signature, this_, args, ret);
  }

  Optional<const MemberVariable &> Type::get_member_variable(Name name) const {
    return get_data().get_member_variable(name);
  }

  Optional<const MemberFunction &> Type::get_member_function(Signature signature) const {
    return get_data().get_member_function(signature);
  }

  Name Type::local_name() const {
    return get_data().local_name_symbol;
  }

  Name Type::global_name() const {
    return get_data().global_name_symbol;
  }

  SizeT Type::size() const {
    return get_data().size;
  }

  SizeT Type::align() const {
    return get_data().align;
  }

  WidePtr Type::allocate() const {
    return get_data().allocate();
  }

  void Type::deallocate(WidePtr ptr) const {
    get_data().deallocate(ptr);
  }

  void Type::init(void * ptr) const {
    get_data().init(ptr);
  }

  void Type::deinit(void * ptr) const {
    get_data().init(ptr);
  }

  void Type::copy_init(void * ptr, void * from) const {
    get_data().copy_init(ptr, from);
  }

  void Type::move_init(void * ptr, void * from) const {
    get_data().move_init(ptr, from);
  }

  WidePtr Type::new_() const {
    return get_data().new_();
  }

  void Type::delete_(WidePtr ptr) const {
    get_data().delete_(ptr);
  }

  Type::Type(GlobalTypeSymbol s, bool p_is_ref) : symbol(s), _hash(Env::current->get_type_data(s).unwrap()._hash) {}
  Type::Type() = default;
}
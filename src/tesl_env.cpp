#include "tesl_env.hpp"

#include "tesl_builtins.hpp"
#include "tesl_name.hpp"
#include "tesl_member_function.hpp"
#include "tesl_member_variable.hpp"
#include "tesl_signature.hpp"
#include "tesl_symbol.hpp"
#include "tesl_type.hpp"

namespace tesl {
  namespace detail {
    EnvRef populate_builtin_env(const EnvRef & env) {
      // set data directly
      auto & name_array = env->names._data;
      name_array.reserve(builtin::name_count);
      TESL_ASSERT(name_array.size() == 0);
      for (GlobalSymbolIndexT i = 0; i < builtin::name_count; ++i) {
        name_array.emplace_back(builtin::name_strings[i]);
      }

      auto & name_map = env->name_map;
      for (GlobalSymbolIndexT i = 0; i < builtin::name_count; ++i) {
        name_map.insert_unique(Str{builtin::name_strings[i]}, GlobalNameSymbol{i});
      }

      // set data directly
      auto & type_array = env->types._data;
      type_array.reserve(builtin::type_count);
      TESL_ASSERT(type_array.size() == 0);
      for (GlobalSymbolIndexT i = 0; i < builtin::type_count; ++i) {
        type_array.emplace_back(builtin::type_info_data[i]);
      }

      auto & type_map = env->type_map;
      for (GlobalSymbolIndexT i = 0; i < builtin::type_count; ++i) {
        type_map.insert_unique(Str{builtin::global_type_name_strings[i]}, GlobalTypeSymbol{i});
      }

      // set data directly
      auto & signature_array = env->signatures._data;
      signature_array.reserve(builtin::signature_count);
      TESL_ASSERT(signature_array.size() == 0);
      for (GlobalSymbolIndexT i = 0; i < builtin::signature_count; ++i) {
        signature_array.emplace_back(SignatureData::parse(builtin::signature_strings[i]).unwrap());
      }

      auto & signature_map = env->signature_map;
      for (GlobalSymbolIndexT i = 0; i < builtin::signature_count; ++i) {
        signature_map.insert_unique(Str{builtin::signature_strings[i]}, GlobalSignatureSymbol{i});
      }

      #define TESL_BUILTIN_TYPE_DEF(type, local_name, global_name) bind_type_info<type>(type_array[builtin::find_type_symbol(global_name).index]);
      #include "tesl_builtin_types.inc"

      return env;
    }
  }

  GlobalNameSymbol Env::add_name(NameData name) {
    GlobalNameSymbol symbol{names.size()};
    name_map.insert({name.str, symbol});
    names.push_back(name);
    return symbol;
  }

  GlobalSignatureSymbol Env::add_signature(SignatureData sig) {
    GlobalSignatureSymbol symbol{signatures.size()};
    signature_map.insert({sig.str, symbol});
    signatures.push_back(sig);
    return symbol;
  }

  GlobalTypeSymbol Env::add_type(TypeInfo type) {
    GlobalTypeSymbol symbol{types.size()};
    type_map.insert({names.get(type.global_name_symbol).unwrap().str, symbol});
    types.push_back(type);
    return symbol;
  }

  Optional<NameData &> Env::get_name_data(GlobalNameSymbol sym) {
    return names.get(sym);
  }

  Optional<SignatureData &> Env::get_signature_data(GlobalSignatureSymbol sym) {
    return signatures.get(sym);
  }

  Optional<TypeInfo &> Env::get_type_data(GlobalTypeSymbol sym) {
    return types.get(sym);
  }

  Env::Env() = default;
  Env::~Env() = default;

  EnvRef Env::_get_shallow_builtin_env() {
    static EnvRef env = new_ref<Env>();
    return env;
  }

  EnvRef Env::current = _get_shallow_builtin_env();
  const EnvRef Env::builtin = detail::populate_builtin_env(_get_shallow_builtin_env());

  /*Signature & SignatureRef::operator*() const { return env->signatures.get_unchecked(symbol); }
  Signature * SignatureRef::operator->() const { return &env->signatures.get_unchecked(symbol); }
    
  TypeInfo & TypeRef::operator*() const { return env->types.get_unchecked(symbol); }
  TypeInfo * TypeRef::operator->() const { return &env->types.get_unchecked(symbol); }
  
  NameRef TypeRef::get_local_name() const { return {(*this)->local_name_symbol, env}; }
  NameRef TypeRef::get_global_name() const { return {(*this)->global_name_symbol, env}; }*/
}
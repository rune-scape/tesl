#pragma once

#include "tesl_common.hpp"
#include "tesl_builtins.hpp"
#include "tesl_env.hpp"
#include "tesl_member_function.hpp"

namespace tesl {
  void bind_member_variable(TypeInfo & type_symbol, Name name_symbol, MemberVariable var);
  void bind_member_function(TypeInfo & type_symbol, Signature signature, MemberFunction fn);
}
/*
#define TESL_BIND_BUITIN_BARE_MEMBER_VARIABLE(type, var, name, sig) \
  ::tesl::bind_member_variable(type, ::tesl::FunctionBare{fn, ::tesl::get_builtin_signature(sig)})
*/
#define TESL_BIND_BUITIN_BARE_MEMBER_FUNCTION(type, fn, sig, ...) \
  ::tesl::bind_member_function(type, TESL_BUILTIN_SIGNATURE(sig), {fn, {__VA_ARGS__}})

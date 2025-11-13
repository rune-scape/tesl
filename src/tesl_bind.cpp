#include "tesl_bind.hpp"

#include "tesl_fmt.hpp"
#include "tesl_member_variable.hpp"
#include "tesl_member_function.hpp"
#include "tesl_symbol.hpp"

namespace tesl {
  void bind_member_variable(TypeInfo & type, Name name, MemberVariable var) {
    TESL_FAIL_COND_MSG(type._member_variables.is_full(), return, "too many member variables for type '{}'", type);
    TESL_FAIL_COND_MSG(type._member_variables.has(name.symbol), return, "variable '{}' already defined in type '{}'", name, type);
    type._member_variables.insert(name.symbol, MOV(var));
  }

  void bind_member_function(TypeInfo & type, Signature signature, MemberFunction fn) {
    TESL_FAIL_COND_MSG(type._member_functions.is_full(), return, "too many member functions for type '{}'", type);
    TESL_FAIL_COND_MSG(type.get_member_function(signature).has_value(), return, "function '{}' already defined in type '{}'", signature, type);
    type._member_functions.insert(signature.symbol, MOV(fn));
  }
} 

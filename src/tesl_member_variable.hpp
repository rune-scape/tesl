#pragma once

#include "tesl_common.hpp"

#include "tesl_member_function.hpp"
#include "tesl_symbol.hpp"

namespace tesl {
  struct MemberVariable {
    Type type;
    MemberFunction getter;
    MemberFunction setter;

    MemberVariable(Type t, MemberFunction get, MemberFunction set);
    ~MemberVariable();
  };
}

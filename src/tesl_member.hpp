#pragma once

#include "tesl_common.hpp"
#include "tesl_fn.hpp"
#include "tesl_operator.hpp"
#include "tesl_ref.hpp"
#include "tesl_type.hpp"

namespace tesl {
  struct MemberVariable {
    TypeRef type = get_builtin_type_info_of<Null>();
    Operator getter;
    Operator setter;
  };

  struct Member {
    enum Kind {
      NONE,
      VARIABLE,
      FUNCTION,
      FUNCTION_POOL
    };
    Kind kind = VARIABLE;
    union {
      IntT _unused = 0;
      MemberVariable variable;
      Operator function;
      LocalSymbolTable<Operator> function_pool;
    };

    Member & operator=(const Member & other);
    Member & operator=(Member && other);

    Member(const Member & other);
    Member(Member && other);

    ~Member();
  };
}
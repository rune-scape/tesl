#pragma once

#include "tesl_common.hpp"
#include "tesl_array.hpp"
#include "tesl_fn.hpp"
#include "tesl_result.hpp"

namespace tesl {
  using ReturnTypes = Array<Type>;

  struct MemberFunction {
    enum Kind {
      INVALID,
      BARE,
    };
    Kind kind = INVALID;
    union {
      int _unused;
      FnPtrBare _bare;
    };
    ReturnTypes return_types;

    Result<Null, CallError> operator()(void * this_, void * args, void * ret) const;

    MemberFunction & operator=(const MemberFunction & other);
    MemberFunction & operator=(MemberFunction && other);

    MemberFunction(const MemberFunction & other);
    MemberFunction(MemberFunction && other);

    MemberFunction(FnPtrBare fn, ReturnTypes rts);
    ~MemberFunction();
  };
}

#include "tesl_type.hpp"

#pragma once

#include "tesl_common.hpp"
#include "tesl_fn.hpp"
#include "tesl_signature.hpp"
#include "tesl_type.hpp"

namespace tesl {
  struct OperatorBare {
    FnPtrBare bare;
    SignatureRef signature;
  };

  struct OperatorNative {
    FnPtr native;
    SignatureRef signature;
  };

  struct Operator {
    enum Kind {
      NONE,
      BARE,
      NATIVE
    };

    Kind kind = NONE;
    SignatureRef signature;
    union {
      IntT _unused = 0;
      FnPtrBare bare;
      FnPtr native;
    };

    static Operator make_bare(OperatorBare fn);
    static Operator make_native(OperatorNative fn);

    void operator()(FnContext * context, void * args, void * ret) const;

    Operator & operator=(const Operator & other);
    Operator & operator=(Operator && other);

    Operator(const Operator & other);
    Operator(Operator && other);

    Operator() = default;
    ~Operator();
  };
}

#pragma once

#include "tesl_common.hpp"
#include "tesl_type.hpp"

namespace tesl {
  struct VarRef {
    void * data;
    Type type;
  };

}
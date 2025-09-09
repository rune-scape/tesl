#pragma once

#include "tesl_common.hpp"
#include "tesl_ref.hpp"
#include "tesl_str.hpp"

namespace tesl {
  struct Name : RefCounted<Name> {
    Str str;
    Name(StrView pStr) : str(pStr) {}
  };
}
#pragma once

#include "tesl_common.hpp"
#include "tesl_str.hpp"
#include "tesl_map.hpp"

namespace tesl {
  struct Module : RefCounted<Module> {
    Map<Str, IntT> type_map;
  };
}
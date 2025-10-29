#pragma once

#include "tesl_common.hpp"
#include "tesl_array.hpp"

namespace tesl {
  struct CallFrame {
    Array<uint8_t> stack;
    const uint8_t * instruction_ptr;
  };

  struct Fiber {
    Array<CallFrame> frames;
  };

  void run_bytecode(FnContext * context, void * args, void * ret);
}

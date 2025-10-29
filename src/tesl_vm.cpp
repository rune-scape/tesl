#include "tesl_vm.hpp"
#include "tesl_fn.hpp"

namespace tesl {
  enum Opcode {

#define TESL_OPCODE_DEF(name) OPCODE_##name,
#include "tesl_opcodes.inc"

  };

  void run_bytecode(void * this_, void * args, void * ret) {
    Fiber * fiber = reinterpret_cast<Fiber *>(this_);
    CallFrame * frame = nullptr;
    uint8_t * ip = nullptr;

#define LOAD_FRAME \
  do { \
    frame = &fiber->frames.back(); \
    ip = frame->instruction_ptr; \
  } while (false)

#ifdef TESL_COMPUTED_GOTO
  static void * dispatch_table[] = {

#define TESL_OPCODE_DEF(name) &&opcode_label_##name,
#include "tesl_opcodes.inc"

  };

#define RUN_LOOP DISPATCH()
#define OPCODE_CASE(name) opcode_label_##name

#define DISPATCH() goto *dispatch_table[opcode = static_cast<Opcode>(*ip++)];

#else

#define RUN_LOOP loop: switch (opcode = static_cast<Opcode>(*ip++))
#define OPCODE_CASE(name) case OPCODE_##name

#define DISPATCH() goto loop

#endif

    // todo: finish

    Opcode opcode;
    RUN_LOOP {
      OPCODE_CASE(CALL_FUNCTION): {
        // todo: finish
        DISPATCH();
      }
    }
  }
}

// opcode definitions for use with macros (yuck)
// for ease of development

#ifndef TESL_OPCODE_DEF
#error "TESL_OPCODE_DEF must be defined!"
#endif

TESL_OPCODE_DEF(CALL_FUNCTION)
TESL_OPCODE_DEF(CALL_MEMBER_FUNCTION)
TESL_OPCODE_DEF(POP)
TESL_OPCODE_DEF(JUMP)
TESL_OPCODE_DEF(JUMP_IF_ZERO)
TESL_OPCODE_DEF(RETURN)

#undef TESL_OPCODE_DEF

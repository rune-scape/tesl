// symbol name definitions for bootstrapping
// TESL_SYMBOL_NAME_DEF(name) will ... define a symbol of such name

#ifndef TESL_SYMBOL_NAME_DEF
#error "TESL_SYMBOL_NAME_DEF must be defined!"
#endif

TESL_SYMBOL_NAME_DEF("Null")
TESL_SYMBOL_NAME_DEF("Bool")
TESL_SYMBOL_NAME_DEF("Int")
TESL_SYMBOL_NAME_DEF("Float")
TESL_SYMBOL_NAME_DEF("Type")
TESL_SYMBOL_NAME_DEF("stringify")

#undef TESL_SYMBOL_NAME_DEF

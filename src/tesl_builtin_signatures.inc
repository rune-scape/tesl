// symbol name definitions for bootstrapping
// TESL_BUILTIN_SIGNATURE_DEF(ret, kind, ...) will define a symbol of such name

#ifndef TESL_BUILTIN_SIGNATURE_DEF
#error "TESL_BUILTIN_SIGNATURE_DEF must be defined!"
#endif

TESL_BUILTIN_SIGNATURE_DEF("->")
TESL_BUILTIN_SIGNATURE_DEF("format(fmt:Formatter)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_literal(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_identifier(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_grouping(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_member_access(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_call(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_subscript(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_prefix(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_arithmetic(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_comparison(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_bool_arithmetic(grammar:SyntaxStream)")
TESL_BUILTIN_SIGNATURE_DEF("resolve_assignment(grammar:SyntaxStream)")

#undef TESL_BUILTIN_SIGNATURE_DEF

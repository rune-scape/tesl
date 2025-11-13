// symbol name definitions for bootstrapping
// TESL_BUILTIN_TYPE_DEF(ret, kind, ...) will define a symbol of such name

#ifndef TESL_BUILTIN_TYPE_DEF
#error "TESL_BUILTIN_TYPE_DEF must be defined!"
#endif

TESL_BUILTIN_TYPE_DEF(::tesl::Null, "Null", "Null")
TESL_BUILTIN_TYPE_DEF(::tesl::Bool, "Bool", "Bool")
TESL_BUILTIN_TYPE_DEF(::tesl::IntT, "Int", "Int")
TESL_BUILTIN_TYPE_DEF(::tesl::FloatT, "Float", "Float")
TESL_BUILTIN_TYPE_DEF(::tesl::Vec2, "Vec2", "Vec2")
TESL_BUILTIN_TYPE_DEF(::tesl::Vec3, "Vec3", "Vec3")
TESL_BUILTIN_TYPE_DEF(::tesl::Vec4, "Vec4", "Vec4")
TESL_BUILTIN_TYPE_DEF(::tesl::Mat2, "Mat2", "Mat2")
TESL_BUILTIN_TYPE_DEF(::tesl::Mat3, "Mat3", "Mat3")
TESL_BUILTIN_TYPE_DEF(::tesl::Mat4, "Mat4", "Mat4")
TESL_BUILTIN_TYPE_DEF(::tesl::Str, "Str", "Str")
TESL_BUILTIN_TYPE_DEF(::tesl::Type, "Type", "Type")
TESL_BUILTIN_TYPE_DEF(::tesl::FmtFormatter, "Formatter", "fmt:Formatter")
TESL_BUILTIN_TYPE_DEF(::tesl::grammar::SyntaxNode, "SyntaxNode", "grammar:SyntaxNode")
TESL_BUILTIN_TYPE_DEF(::tesl::grammar::SyntaxStream, "SyntaxStream", "grammar:SyntaxStream")
TESL_BUILTIN_TYPE_DEF(::tesl::grammar::SyntaxResolver, "SyntaxResolver", "grammar:SyntaxResolver")

#undef TESL_BUILTIN_TYPE_DEF

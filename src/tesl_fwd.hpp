
namespace tesl {
  // tesl_compiler.hpp
  struct Compiler;

  // tesl_dynamic_array.hpp
  struct DynamicArray;

  // tesl_dynamic_map.hpp
  struct DynamicMap;

  // tesl_env.hpp
  struct Env;

  // tesl_fn.hpp
  struct FnObj;

  // tesl_math.hpp
  struct Vec2;
  struct Vec3;
  struct Vec4;
  struct Mat2;
  struct Mat3;
  struct Mat4;

  // tesl_module.hpp
  struct Module;

  // tesl_parser.hpp
  struct Parser;

  // tesl_ref.hpp
  template<typename T> struct Ref;
  template<typename T> struct RefCounted;

  // tesl_type.hpp
  struct TypeInfo;

  // tesl_symbol.hpp
  struct Symbol;
  template<typename T> struct SymbolTable;

  // tesl_var.hpp
  struct VarRef;
  struct Variant;
}

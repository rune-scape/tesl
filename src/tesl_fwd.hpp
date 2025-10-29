#include "fmt/format.h"
#include <memory>
#include <fmt/fwd.h>

namespace tesl {
  // tesl_ref.hpp
  template<typename T> struct Ref;
  template<typename T> struct RefCounted;

  // tesl_fmt.hpp
  using FmtBuffer = fmt::memory_buffer;
  using FmtParseContext = fmt::format_parse_context;
  using FmtFormatContext = fmt::format_context;
  struct FmtFormatter;

  // tesl_dynamic_array.hpp
  struct DynamicArray;

  // tesl_dynamic_map.hpp
  struct DynamicMap;

  // tesl_fn.hpp
  struct CallError;
  struct FnPtr;

  // tesl_env.hpp
  struct Env;
  using EnvRef = Ref<Env>;

  // tesl_module.hpp
  struct Module;
  using ModuleRef = Ref<Module>;

  // tesl_array.hpp
  template<typename T, typename Alloc = std::allocator<T> >
  struct Array;

  // tesl_map.hpp
  namespace detail {
    struct MapHasher;
    struct MapCompareEqual;
  }
  template<typename KeyT, typename ValueT, typename HasherT = detail::MapHasher, typename EqualsT = detail::MapCompareEqual>
  struct Map;

  // tesl_math.hpp
  struct Vec2;
  struct Vec3;
  struct Vec4;
  struct Mat2;
  struct Mat3;
  struct Mat4;

  // tesl_member_function.hpp
  struct MemberFunction;

  // tesl_member_variable.hpp
  struct MemberVariable;

  // tesl_tokenizer.hpp
  struct Tokenizer;

  // tesl_grammar.hpp
  namespace grammar {
    struct SyntaxNode;
    struct SyntaxStream;
  }

  // tesl_parser.hpp
  struct Parser;

  // tesl_compiler.hpp
  struct Compiler;

  // tesl_name.hpp
  struct NameData;
  struct Name;

  // tesl_signature.hpp
  struct SignatureData;
  struct Signature;

  // tesl_type.hpp
  struct TypeInfo;
  struct Type;

  // tesl_symbol_table.hpp
  template<typename SymbolT, typename ValueT> struct FlatSymbolTable;
  template<typename GlobalSymbolT, typename LocalSymbolT, typename ValueT> struct LayeredSymbolTable;

  // tesl_var_wrapper.hpp
  struct VarWrapper;

  // tesl_var.hpp
  struct VarRef;
  struct Variant;

  namespace coroutine {
    template<typename T>
    struct Generator;
  }
}

#include "tesl_syntax_resolver.hpp"

#include "tesl_bind.hpp"
#include "tesl_grammar.hpp"
#include "tesl_type.hpp"

namespace tesl {
  namespace grammar {
    grammar::SyntaxStream SyntaxResolver::resolve_literal(grammar::SyntaxStream syntax) {
      println("resolve_literal");
      for (auto & node : syntax) {
        println("  {}", node);
        co_yield MOV(node);
      }
    }

    grammar::SyntaxStream SyntaxResolver::resolve_identifier(grammar::SyntaxStream syntax) {
      println("resolve_identifier");
      for (auto & node : syntax) {
        println("  {}", node);
        co_yield MOV(node);
      }
    }

    grammar::SyntaxStream SyntaxResolver::resolve_grouping(grammar::SyntaxStream syntax) {
      println("resolve_grouping");
      for (auto & node : syntax) {
        println("  {}", node);
      }
      co_yield grammar::SyntaxNode::make_expresion({get_type_of<Null>()});
    }

    grammar::SyntaxStream SyntaxResolver::resolve_member_access(grammar::SyntaxStream syntax) {
      println("resolve_member_access");
      for (auto & node : syntax) {
        println("  {}", node);
      }
      co_yield grammar::SyntaxNode::make_expresion({get_type_of<Null>()});
    }

    grammar::SyntaxStream SyntaxResolver::resolve_call(grammar::SyntaxStream syntax) {
      println("resolve_call");
      for (auto & node : syntax) {
        println("  {}", node);
      }
      co_yield grammar::SyntaxNode::make_expresion({get_type_of<Null>()});
    }

    grammar::SyntaxStream SyntaxResolver::resolve_subscript(grammar::SyntaxStream syntax) {
      println("resolve_subscript");
      for (auto & node : syntax) {
        println("  {}", node);
      }
      co_yield grammar::SyntaxNode::make_expresion({get_type_of<Null>()});

    }

    grammar::SyntaxStream SyntaxResolver::resolve_prefix(grammar::SyntaxStream syntax) {
      println("resolve_prefix");
      for (auto & node : syntax) {
        println("  {}", node);
      }
      co_yield grammar::SyntaxNode::make_expresion({get_type_of<Null>()});

    }

    grammar::SyntaxStream SyntaxResolver::resolve_arithmetic(grammar::SyntaxStream syntax) {
      println("resolve_arithmetic");
      for (auto & node : syntax) {
        println("  {}", node);
      }
      co_yield grammar::SyntaxNode::make_expresion({get_type_of<Null>()});
    }

    grammar::SyntaxStream SyntaxResolver::resolve_comparison(grammar::SyntaxStream syntax) {
      println("resolve_comparison");
      for (auto & node : syntax) {
        println("  {}", node);
      }
      co_yield grammar::SyntaxNode::make_expresion({get_type_of<Null>()});
    }

    grammar::SyntaxStream SyntaxResolver::resolve_bool_arithmetic(grammar::SyntaxStream syntax) {
      println("resolve_bool_arithmetic");
      for (auto & node : syntax) {
        println("  {}", node);
      }
      co_yield grammar::SyntaxNode::make_expresion({get_type_of<Null>()});
    }

    grammar::SyntaxStream SyntaxResolver::resolve_assignment(grammar::SyntaxStream syntax) {
      println("resolve_assignment");
      for (auto & node : syntax) {
        println("  {}", node);
      }
      co_yield grammar::SyntaxNode::make_expresion({get_type_of<Null>()});
    }
  }

#define BIND_RESOLVE_FN(type, fn_name) \
  bind_member_function( \
    type, \
    TESL_BUILTIN_SIGNATURE(#fn_name "(grammar:SyntaxStream)"), \
    { \
      [](void * this_, void * args, void * ret) { \
        *reinterpret_cast<grammar::SyntaxStream *>(ret) = reinterpret_cast<grammar::SyntaxResolver *>(this_)->fn_name(MOV(*reinterpret_cast<grammar::SyntaxStream *>(args))); \
      }, \
      { \
        get_type_of<grammar::SyntaxStream>() \
      } \
    } \
  )

  template<> void bind_type_info<grammar::SyntaxResolver>(TypeInfo & type) {
    BIND_RESOLVE_FN(type, resolve_literal);
    BIND_RESOLVE_FN(type, resolve_identifier);
    BIND_RESOLVE_FN(type, resolve_grouping);
    BIND_RESOLVE_FN(type, resolve_member_access);
    BIND_RESOLVE_FN(type, resolve_call);
    BIND_RESOLVE_FN(type, resolve_subscript);
    BIND_RESOLVE_FN(type, resolve_prefix);
    BIND_RESOLVE_FN(type, resolve_arithmetic);
    BIND_RESOLVE_FN(type, resolve_comparison);
    BIND_RESOLVE_FN(type, resolve_bool_arithmetic);
    BIND_RESOLVE_FN(type, resolve_assignment);
  }

#undef BIND_RESOLVE_FN

}

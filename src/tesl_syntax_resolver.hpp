#pragma once

#include "tesl_common.hpp"

namespace tesl {
  namespace grammar {
    struct SyntaxResolver {
      grammar::SyntaxStream resolve_literal(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_identifier(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_grouping(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_member_access(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_call(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_subscript(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_prefix(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_arithmetic(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_comparison(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_bool_arithmetic(grammar::SyntaxStream);
      grammar::SyntaxStream resolve_assignment(grammar::SyntaxStream);
    };
  }

  template<> void bind_type_info<grammar::SyntaxResolver>(TypeInfo & type);
}
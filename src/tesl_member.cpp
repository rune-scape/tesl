#include "tesl_member.hpp"

namespace tesl {
  Member & Member::operator=(const Member & other) {
    Kind old_kind = kind;
    kind = NONE;
    switch (old_kind) {
      case NONE:
        break;
      case VARIABLE:
        variable.~MemberVariable();
        break;
      case FUNCTION:
        function.~Operator();
        break;
      case FUNCTION_POOL:
        function_pool.~LocalSymbolTable<Operator>();
        break;
    }
    switch (other.kind) {
      case NONE:
        break;
      case VARIABLE:
        new (&variable) MemberVariable(other.variable);
        break;
      case FUNCTION:
        new (&function) Operator(other.function);
        break;
      case FUNCTION_POOL:
        new (&function_pool) LocalSymbolTable<Operator>(other.function_pool);
        break;
    }
    kind = other.kind;
    return *this;
  }

  Member & Member::operator=(Member && other) {
    memswap(*this, other);
    return *this;
  }

  Member::Member(const Member & other) : kind(other.kind) {
    switch (other.kind) {
      case NONE:
        break;
      case VARIABLE:
        new (&variable) MemberVariable(other.variable);
        break;
      case FUNCTION:
        new (&function) Operator(other.function);
        break;
      case FUNCTION_POOL:
        new (&function_pool) LocalSymbolTable<Operator>(other.function_pool);
        break;
    }
  }

  Member::Member(Member && other) {
    memswap(*this, other);
  }

  Member::~Member() {
    Kind old_kind = kind;
    kind.~Kind();
    switch (old_kind) {
      case NONE:
        break;
      case VARIABLE:
        variable.~MemberVariable();
        break;
      case FUNCTION:
        function.~Operator();
        break;
      case FUNCTION_POOL:
        function_pool.~LocalSymbolTable<Operator>();
        break;
    }
  }
}
#pragma once

#include "tesl_common.hpp"
#include "tesl_array.hpp"
#include "tesl_ref.hpp"
#include "tesl_type.hpp"

namespace tesl {
  struct Signature : RefCounted<Signature> {
    enum Kind {
      CALL = 0,
      SUBSCRIPT,
      UNARY_PLUS,
      UNARY_MINUS,
      LOGICAL_NOT,
      BITWISE_NOT,
      MULTIPLY,
      DIVIDE,
      REMAINDER,
      ADD,
      SUBTRACT,
      BITWISE_LEFT_SHIFT,
      BITWISE_RIGHT_SHIFT,
      BITWISE_AND,
      BITWISE_XOR,
      BITWISE_OR,
      COMPARE_LESS_THAN,
      COMPARE_GRATER_THAN,
      COMPARE_LESS_THAN_OR_EQUAL_TO,
      COMPARE_GRATER_THAN_OR_EQUAL_TO,
      COMPARE_EQUAL,
      COMPARE_NOT_EQUAL,
      LOGICAL_AND,
      LOGICAL_OR,
      ADD_ASSIGN,
      SUBTRACT_ASSIGN,
      MULTIPLY_ASSIGN,
      DIVIDE_ASSIGN,
      REMAINDER_ASSIGN,
    };
    Kind kind = CALL;
    Array<TypeRef> parameter_types;
    Array<TypeRef> return_types;

    bool operator==(const Signature & other) {
      return kind == other.kind && parameter_types == other.parameter_types && return_types == other.return_types;
    }

    bool operator!=(const Signature & other) {
      return !this->operator==(other);
    }
  };

  SignatureRef parse_signature(const Env * env, const char * str);
}
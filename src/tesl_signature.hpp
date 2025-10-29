#pragma once

#include "tesl_common.hpp"
#include "tesl_array.hpp"
#include "tesl_hash.hpp"
#include "tesl_result.hpp"
#include "tesl_str.hpp"
#include "tesl_symbol.hpp"

namespace tesl {
  struct SignatureData {
    enum Kind {
      COERCE,
      CALL,
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

    Kind kind;
    GlobalNameSymbol name_symbol; // empty unless this is a named call signature
    Array<GlobalTypeSymbol> parameter_types;
    Str str;
    HashT hash;

    bool operator==(const SignatureData & other);

    TESL_ALWAYS_INLINE bool operator!=(const SignatureData & other) {
      return !this->operator==(other);
    }

    static Result<SignatureData, Str> parse(StrView str);

    SignatureData & operator=(const SignatureData &) = default;
    SignatureData & operator=(SignatureData &&) = default;

    TESL_ALWAYS_INLINE friend HashT hash(const SignatureData & v) { return v.hash; }

    SignatureData(const SignatureData &) = default;
    SignatureData(SignatureData &&) = default;

    SignatureData(Kind p_kind, GlobalNameSymbol p_name_symbol, Array<GlobalTypeSymbol> p_parameter_types, CharStrView p_str);
    ~SignatureData();
  };

  struct Signature {
    GlobalSignatureSymbol symbol;
    HashT _hash;

    SignatureData & get_data() const;
    bool is_valid() const;
    StrView str() const { return get_data().str; }

    TESL_ALWAYS_INLINE bool operator==(Signature other) const { return symbol == other.symbol; }
    TESL_ALWAYS_INLINE bool operator!=(Signature other) const { return symbol != other.symbol; }

    TESL_ALWAYS_INLINE friend HashT hash(const Signature & v) { return v._hash; }

    TESL_ALWAYS_INLINE friend const SignatureData & format_as(const Signature & v) { return v.get_data(); }

    Signature & operator=(const Signature & other) = default;
    Signature & operator=(Signature && other) = default;

    Signature(const Signature & other) = default;
    Signature(Signature && other) = default;

    Signature(GlobalSignatureSymbol s);
  };
}

template<>
class fmt::formatter<tesl::SignatureData, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::SignatureData & v, Context & ctx) const {
    return format_to(ctx.out(), "{}", v.str);
  }
};

template<>
struct fmt::formatter<tesl::Signature, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Signature & signature, Context & ctx) const {
    if (!signature.is_valid()) {
      return format_to(ctx.out(), "<invalid-signature>");
    }

    return format_to(ctx.out(), "{}", signature.get_data());
  }
};

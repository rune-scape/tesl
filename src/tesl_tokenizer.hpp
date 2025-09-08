#pragma once

#include "tesl_common.hpp"
#include "tesl_var.hpp"
#include <fmt/fwd.h>

namespace tesl {
  struct Token {
    enum Kind : char {
      NONE,
      END,
      IDENTIFIER,
      LITERAL,

#define TESL_TOKEN_SYMBOL_DEF(str, name) name,
#define TESL_TOKEN_KEYWORD_DEF(str, name) name,
#define TESL_TOKEN_LITERAL_KEYWORD_DEF(str, value)
#define TESL_TOKEN_DECIMAL_POINT
#define TESL_TOKEN_GROUP(...)
#define TESL_TOKEN_GROUP_END
#include "tesl_tokens.inc"

    };

    Kind kind = NONE;
    CharStrView span;
    Variant literal;

    bool operator==(Kind k) { return kind == k; }
    bool operator!=(Kind k) { return kind != k; }

    bool operator==(Token t) = delete;
    bool operator!=(Token t) = delete;

    Token() = default;
    
    Token & operator=(const Token &) = default;
    Token & operator=(Token &&) = default;

    Token(Kind k, CharStrView s, Variant l = {}) : kind(k), span(s), literal(l) {}
    Token(const Token &) = default;
    Token(Token &&) = default;
    ~Token() = default;
  };

  inline bool operator==(Token::Kind k, const Token & tok) { return k == tok.kind; }
  inline bool operator!=(Token::Kind k, const Token & tok) { return k != tok.kind; }

  struct Tokenizer {
    const char * input = nullptr;
    const char * input_it = nullptr;
    const char * line_start = nullptr;
    IntT line_num = 1;
    bool has_error = false;

    char32_t _parse_octal_char();
    char32_t _parse_hex_char(IntT len);
    Token _tokenize_string_literal();
    Token _tokenize_number_literal();
    Token _tokenize_identifier();
    Token next_token();
    void reset();

    Tokenizer(const char * p_input) : input(p_input), input_it(p_input), line_start(p_input) {}
  };
}

template<typename CharT>
class fmt::formatter<tesl::Token, CharT> {
public:
  template<typename Context>
  constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context>
  auto format(const tesl::Token & token, Context & ctx) const {
    using namespace tesl;
    switch (token.kind) {
      case Token::NONE:
      case Token::END:
        return format_to(ctx.out(), "{}", token.kind);
      case Token::IDENTIFIER:
        return format_to(ctx.out(), "identifier '{}'", token.span);
      case Token::LITERAL:
        return format_to(ctx.out(), "literal {}", token.literal.get_type());

#define TESL_TOKEN_SYMBOL_DEF(str, name) \
  case Token::name: return format_to(ctx.out(), "{:?}", token.kind);
#define TESL_TOKEN_KEYWORD_DEF(str, name) \
  case Token::name: return format_to(ctx.out(), "{:?}", token.kind);
#define TESL_TOKEN_LITERAL_KEYWORD_DEF(str, value)
#define TESL_TOKEN_DECIMAL_POINT
#define TESL_TOKEN_GROUP(...)
#define TESL_TOKEN_GROUP_END
#include "tesl_tokens.inc"

    }

    return format_to(ctx.out(), "{:?}", token.kind);
  }
};

template<typename CharT>
class fmt::formatter<tesl::Token::Kind, CharT> {
  bool is_quoted = false;
public:
  template<typename Context>
  constexpr auto parse(Context & ctx) {
    if (ctx.begin() != ctx.end() && (*ctx.begin() == '?')) {
      is_quoted = true;
      return ctx.begin() + 1;
    }
    return ctx.begin();
  }
  template<typename Context>
  constexpr auto format_quoted_impl(const char * str, Context & ctx) const {
    if (is_quoted) {
      return format_to(ctx.out(), "'{}'", str);
    } else {
      return format_to(ctx.out(), "{}", str);
    }
  }

  template<typename Context>
  constexpr auto format(const tesl::Token::Kind & kind, Context & ctx) const {
    using namespace tesl;

    switch (kind) {
      case Token::NONE:
        return format_to(ctx.out(), "{}", "<none>");
      case Token::END:
        return format_to(ctx.out(), "{}", "<end>");
      case Token::IDENTIFIER:
        return format_to(ctx.out(), "{}", "<identifier>");
      case Token::LITERAL:
        return format_to(ctx.out(), "{}", "<literal>");

#define TESL_TOKEN_SYMBOL_DEF(str, name) \
  case Token::name: return format_quoted_impl(str, ctx);
#define TESL_TOKEN_KEYWORD_DEF(str, name) \
  case Token::name: return format_quoted_impl(str, ctx);
#define TESL_TOKEN_LITERAL_KEYWORD_DEF(str, value)
#define TESL_TOKEN_DECIMAL_POINT
#define TESL_TOKEN_GROUP(...)
#define TESL_TOKEN_GROUP_END
#include "tesl_tokens.inc"

    }

    return format_to(ctx.out(), "<unknown-token:{:#04x}>", static_cast<int>(kind));
  }
};

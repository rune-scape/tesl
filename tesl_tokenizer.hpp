#pragma once

#include "tesl_common.hpp"
#include "tesl_fmt_fwd.hpp"
#include "tesl_var.hpp"

namespace tesl {
  struct Token {
    enum Kind : char {
      NONE,
      END,
      IDENTIFIER,
      LITERAL,

#define TOKEN(str, name) name,
#define TOKEN_LITERAL(str, value)
#define TOKEN_NUMBER
#define GROUP_START(...)
#define GROUP_END
#include "tesl_tokens.inl"
#undef GROUP_END
#undef GROUP_START
#undef TOKEN_NUMBER
#undef TOKEN_LITERAL
#undef TOKEN

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
  static constexpr IntT TokenSize = sizeof(Token);

  inline bool operator==(Token::Kind k, const Token & tok) { return k == tok.kind; }
  inline bool operator!=(Token::Kind k, const Token & tok) { return k != tok.kind; }

  struct Tokenizer {
    const char * input = nullptr;
    const char * current = nullptr;
    const char * line_start = nullptr;
    IntT line_num = 1;
    bool has_error = false;

    char32_t _parse_octal_char();
    char32_t _parse_hex_char(IntT len);
    Token _tokenize_string_literal();
    bool _try_tokenize_number_literal(Token & tok);
    bool _try_tokenize_identifier_literal(Token & tok);
    Token next_token();

    Tokenizer(const char * p_input) : input(p_input), current(p_input), line_start(p_input) {}
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

#define TOKEN(str, name) case Token::name: return format_to(ctx.out(), "'{}'", token.kind);
#define TOKEN_LITERAL(str, value)
#define TOKEN_NUMBER
#define GROUP_START(...)
#define GROUP_END
#include "tesl_tokens.inl"
#undef GROUP_END
#undef GROUP_START
#undef TOKEN_NUMBER
#undef TOKEN_LITERAL
#undef TOKEN

    }

    return format_to(ctx.out(), "{}", token.kind);
  }
};

template<typename CharT>
class fmt::formatter<tesl::Token::Kind, CharT> {
public:
  template<typename Context>
  constexpr auto parse(Context & ctx) const { return ctx.begin(); }
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

#define TOKEN(str, name) case Token::name: return format_to(ctx.out(), "{}", str);
#define TOKEN_LITERAL(str, value)
#define TOKEN_NUMBER
#define GROUP_START(...)
#define GROUP_END
#include "tesl_tokens.inl"
#undef GROUP_END
#undef GROUP_START
#undef TOKEN_NUMBER
#undef TOKEN_LITERAL
#undef TOKEN

    }

    return format_to(ctx.out(), "<unknown-token:{:#04x}>", static_cast<int>(kind));
  }
};

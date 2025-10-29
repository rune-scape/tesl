#pragma once

#include "tesl_common.hpp"
#include "tesl_fmt.hpp"
#include "tesl_parse.hpp"
#include "tesl_var.hpp"

namespace tesl {
  struct Token {
    enum Kind : char {

#define TESL_TOKEN_META_DEF(str, name) name,
#define TESL_TOKEN_SYMBOL_DEF(str, name) name,
#define TESL_TOKEN_KEYWORD_DEF(str, name) name,
#define TESL_TOKEN_LITERAL_KEYWORD_DEF(str, value)
#define TESL_TOKEN_GROUP(...)
#define TESL_TOKEN_GROUP_END
#include "tesl_tokens.inc"

    };

    Kind kind = NONE;
    CharStrView span;
    Variant literal;

    TESL_ALWAYS_INLINE bool operator==(Kind k) { return kind == k; }
    TESL_ALWAYS_INLINE bool operator!=(Kind k) { return kind != k; }
    TESL_ALWAYS_INLINE friend bool operator==(Token::Kind k, const Token & tok) { return k == tok.kind; }
    TESL_ALWAYS_INLINE friend bool operator!=(Token::Kind k, const Token & tok) { return k != tok.kind; }

    bool operator==(Token t) = delete;
    bool operator!=(Token t) = delete;

    Token() = default;
    
    Token & operator=(const Token &) = default;
    Token & operator=(Token &&) = default;

    Token(const Token &) = default;
    Token(Token &&) = default;

    template<typename T>
    Token(Kind k, CharStrView s, T && l) : kind(k), span(s), literal(FWD(l)) {}
    Token(Kind k, CharStrView s) : kind(k), span(s) {}
    ~Token() = default;
  };

  const char * get_token_name(Token::Kind t);
  bool is_token_meta(Token::Kind t);

  struct Tokenizer {
    CharStrView input;
    CharStrView::const_iterator input_it = nullptr;
    CharStrView::const_iterator line_start = nullptr;
    IntT line_num = 1;
    bool has_error = false;
    bool strict_parsing = false;
    bool _has_current_token = false;

    Token _current;

    Token _tokenize_string_literal(char quote_char);
    Token _tokenize_number_literal(const char * num_span_begin, parse::ParsedNumber & parsed_number);
    Token _tokenize_identifier();
    Token _next_token();

    Token & current();
    Token & next();
    void reset();

    Tokenizer(CharStrView pInput) : input(pInput), input_it(pInput.begin()), line_start(pInput.begin()) {}
  };
}

template<>
class fmt::formatter<tesl::Token, tesl::CommonCharT> {
public:
  template<typename Context>
  constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context>
  constexpr auto format(const tesl::Token & token, Context & ctx) const {
    using namespace tesl;

    switch (token.kind) {
      case Token::IDENTIFIER:
        return format_to(ctx.out(), "identifier '{}'", token.span);
      case Token::LITERAL:
        return format_to(ctx.out(), "literal {}", token.literal.get_type());
      default:
        break;
    }

    return format_to(ctx.out(), "{:?}", token.kind);
  }
};

template<>
class fmt::formatter<tesl::Token::Kind, tesl::CommonCharT> {
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
  constexpr auto format(const tesl::Token::Kind & kind, Context & ctx) const {
    const char * name = get_token_name(kind);
    if (name == nullptr) {
      return format_to(ctx.out(), "<unknown-token:{:#04x}>", static_cast<int>(kind));
    }

    if (tesl::is_token_meta(kind)) {
      return format_to(ctx.out(), "<{}>", name);
    } else {
      return format_to(ctx.out(), "'{}'", name);
    }
  }
};

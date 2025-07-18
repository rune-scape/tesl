#pragma once

#include "tesl_common.hpp"
#include "tesl_var.hpp"

namespace tesl {
  struct Token {
    enum Kind : char {
      NONE,
      END,
      IDENTIFIER,
      LITERAL,
      COMMA,
      DOT,
      SEMICOLON,
      COLON,
      QUESTION_MARK,
      OPEN_PAREN,
      CLOSE_PAREN,
      OPEN_SQUARE_BRACKET,
      CLOSE_SQUARE_BRACKET,
      OPEN_CURLY_BRACKET,
      CLOSE_CURLY_BRACKET,
      IF,
      ELSE,
      SWITCH,
      CASE,
      FOR,
      WHILE,
      DO,
      BREAK,
      CONTINUE,
      RETURN,
      AND,
      AND_AND,
      CARET,
      CARET_CARET,
      PIPE,
      PIPE_PIPE,
      EQUAL,
      EQUAL_EQUAL,
      BANG,
      BANG_EQUAL,
      LESS,
      LESS_LESS,
      LESS_EQUAL,
      GREATER,
      GREATER_GREATER,
      GREATER_EQUAL,
      PLUS,
      PLUS_PLUS,
      PLUS_EQUAL,
      MINUS,
      MINUS_MINUS,
      MINUS_EQUAL,
      STAR,
      STAR_EQUAL,
      SLASH,
      SLASH_EQUAL,
      PERCENT,
      PERCENT_EQUAL,
    } kind = NONE;
    CharStrViewT name;
    Variant literal;

    bool operator==(Kind k) { return kind == k; }
    bool operator!=(Kind k) { return kind != k; }

    bool operator==(Token t) = delete;
    bool operator!=(Token t) = delete;

    Token() = default;
    
    Token & operator=(const Token &) = default;
    Token & operator=(Token &&) = default;
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
    Token _tokenize_number_literal();
    Token _tokenize_string_literal();
    Token next_token();

    Tokenizer(const char * p_input) : input(p_input), current(p_input), line_start(p_input) {}

    template<typename ... Ts>
    void error(Ts && ... vs) {
      has_error = true;
      (print(vs), ...);
      print("\n");
    }
  };

  void print(Token);
  void print(Token::Kind);
}

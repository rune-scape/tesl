#include "tesl_tokenizer.hpp"

#include "tesl_str.hpp"
#include "tesl_vector.hpp"
#include "tesl_fmt.hpp"

namespace tesl {
  bool is_digit(char c) {
	  return static_cast<UIntT>(c - '0') < 10;
  }

  bool is_octal_digit(char c) {
	  return static_cast<UIntT>(c - '0') < 8;
  }

  bool is_hex_digit(char c) {
	  return is_digit(c) || static_cast<UIntT>((c | 32) - 'a') < 6;
  }

  bool is_alpha(char c) {
	  return static_cast<UIntT>((c | 32) - 'a') < 26;
  }

  bool is_valid_id_starter(char c) {
    return is_alpha(c) || c == '_';
  }

  bool is_valid_id_meat(char c) {
    return is_digit(c) || is_alpha(c) || c == '_';
  }

  bool validate_id(CharStrView & str) {
    const char * s = str.data();
    if (is_valid_id_starter(*s)) {
      s++;
      while (is_valid_id_meat(*s)) s++;
      str = {str.begin(), static_cast<size_t>(s - str.begin())};
      return true;
    }
    return false;
  }

#define tokenizer_error(...) \
  do { \
    has_error = true; \
    fmt::println(__VA_ARGS__); \
  } while(false)

  Token Tokenizer::_tokenize_number_literal() {
    Token tok;

    char * parsed_int_end = nullptr;
    // todo: maybe custom parser for better errors
    IntT parsed_int = strtol(current, &parsed_int_end, 0);
    char * parsed_float_end = nullptr;
    FloatT parsed_float = strtof(current, &parsed_float_end);
    if (parsed_float_end > parsed_int_end) {
      // strtof parsed more, use that
      tok.kind = Token::LITERAL;
      tok.span = {current, static_cast<size_t>(parsed_float_end - current)};
      tok.literal = parsed_float;
    } else if (parsed_int_end > current) {
      // strtol parsed something...
      tok.kind = Token::LITERAL;
      tok.span = {current, static_cast<size_t>(parsed_int_end - current)};
      tok.literal = parsed_int;
    }
    
    return tok;
  }

  char32_t Tokenizer::_parse_octal_char() {
    char32_t result = 0;
    for (IntT i = 0; i < 3; ++i) {
      char c = *current;
      if (is_octal_digit(c)) {
        break;
      }
      result <<= 3;
      result |= c - '0';
      current++;
    }
    return result;
  }

  char32_t Tokenizer::_parse_hex_char(IntT len) {
    const char * start = current;
    char32_t result = 0;
    for (IntT i = 0; i < len; ++i) {
      char c = *current;
      IntT n = 0;
      switch (c) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
          n = c - '0';
          break;
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
          n = 10 + c - 'a';
          break;
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
          n = 10 + c - 'A';
          break;
        default:
          tokenizer_error("error: invalid hex number!");
          print_error(line_num, line_start, start, current, start + len);
          return U'\0';
      }
      result <<= 4;
      result |= n;
      current++;
    }
    return result;
  }

  Token Tokenizer::_tokenize_string_literal() {
    static constexpr const char * hex_chars = "0123456789abcdef";

    Token tok;
    tok.span = {current, 0};

    Str result;
    result = Str();
    bool end_of_str = false;
    while (!end_of_str) {
      current++;
      char c = *current;
      switch (c) {
        case '"':
          current++;
          end_of_str = true;
          break;
        case '\r':
        case '\n':
        case '\0':
          tokenizer_error("error: unexpected end of file!");
          print_error(line_num, line_start, tok.span.data(), current - 1, current);
          end_of_str = true;
          break;
        case '\\':
          current++;
          c = *current;
          switch (c) {
            case '\'': result += '\''; break;
            case '\"': result += '\"'; break;
            case '\?': result += '\?'; break;
            case '\\': result += '\\'; break;
            case 'a': result += '\a'; break;
            case 'b': result += '\b'; break;
            case 'e': result += '\33'; break;
            case 'f': result += '\f'; break;
            case 'n': result += '\n'; break;
            case 'r': result += '\r'; break;
            case 't': result += '\t'; break;
            case 'v': result += '\v'; break;

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
              result += _parse_octal_char();
              break;
            case 'x':
              current++;
              result += _parse_hex_char(2);
              break;
            case 'u':
              current++;
              result += _parse_hex_char(4);
              break;
            case 'U':
              current++;
              result += _parse_hex_char(8);
              break;
            default:
              tokenizer_error("error: unknown escape sequence!");
              print_error(line_num, line_start, tok.span.data(), current - 1, current);
              end_of_str = true;
              break;
          }
          break;
        default:
          result += c;
          break;
      }
    }

    tok.span = {tok.span.begin(), static_cast<size_t>(current - tok.span.begin())};
    tok.kind = Token::LITERAL;
    tok.literal = MOV(result);
    return tok;
  }

#define extend_span(sv) ((sv) = CharStrView{(sv).begin(), (sv).size() + 1})

  Token Tokenizer::next_token() {
    Token token;

    bool new_line = current == input;
    token.span = {current, 0};
    do {
      current = token.span.end();
      token = {};
      token.span = {current, 0};



      /*switch (token.span[0]) {
        case '\0':
          token.kind = Token::END;
          break;

#define MATCH_TOKEN(str) strncmp(token.span.data() + 1, str + 1, sizeof(str) - 2)
#define TOKEN(str, name) \
  if (MATCH_TOKEN(str)) { \
    token.kind = Token::name; \
    token.span = {}; \
    break; \
  }
#define TOKEN_LITERAL(str, value) \
  if (MATCH_TOKEN(str)) { \
    token.kind = Token::LITERAL; \
    token.literal = value; \
    token.span = {}; \
    break; \
  }
#define GROUP_START(c) case c: {
#define GROUP_END } break;
#include "tesl_tokens.inl"
#undef GROUP_END
#undef GROUP_START
#undef TOKEN_LITERAL
#undef TOKEN
#undef MATCH_TOKEN

      }*/



      if (token.span[0] == '\0') {
        token.kind = Token::END;
        continue;
      }

      // number literal
      if (is_digit(token.span[0]) || (token.span[0] == '.' && is_digit(token.span[1]))) {
        token = _tokenize_number_literal();
        continue;
      }

      // string literal
      if (token.span[0] == '"') {
        token = _tokenize_string_literal();
        continue;
      }

      // identifier
      if (validate_id(token.span)) {
        const IntT id_len = token.span.size();

#define MATCHES_TOKEN_(str) (id_len == (sizeof(str) - 1) && strncmp(str, token.span.data(), id_len) == 0)
        switch (token.span[0]) {
          case 'b': {
            if (MATCHES_TOKEN_("break")) {
              token.kind = Token::BREAK;
            }
          } break;
          case 'c': {
            if (MATCHES_TOKEN_("case")) {
              token.kind = Token::CASE;
            } else if (MATCHES_TOKEN_("continue")) {
              token.kind = Token::CONTINUE;
            }
          } break;
          case 'd': {
            if (MATCHES_TOKEN_("do")) {
              token.kind = Token::DO;
            }
          } break;
          case 'e': {
            if (MATCHES_TOKEN_("else")) {
              token.kind = Token::ELSE;
            }
          } break;
          case 'f': {
            if (MATCHES_TOKEN_("false")) {
              token.kind = Token::LITERAL;
              token.literal = false;
            } else if (MATCHES_TOKEN_("for")) {
              token.kind = Token::FOR;
            }
          } break;
          case 'i': {
            if (MATCHES_TOKEN_("if")) {
              token.kind = Token::IF;
            }
          } break;
          case 'r': {
            if (MATCHES_TOKEN_("return")) {
              token.kind = Token::RETURN;
            }
          } break;
          case 's': {
            if (MATCHES_TOKEN_("switch")) {
              token.kind = Token::SWITCH;
            }
          } break;
          case 't': {
            if (MATCHES_TOKEN_("true")) {
              token.kind = Token::LITERAL;
              token.literal = true;
            }
          } break;
          case 'w': {
            if (MATCHES_TOKEN_("while")) {
              token.kind = Token::WHILE;
            }
          } break;
        }
#undef MATCHES_TOKEN_

        if (token.kind == Token::NONE) {
          token.kind = Token::IDENTIFIER;
        }
  
        break;
      }

      // special
      extend_span(token.span);
      switch (token.span[0]) {
        case '&': {
          if (*token.span.end() == '&') {
            extend_span(token.span);
            token.kind = Token::AND_AND;
          } else {
            token.kind = Token::AND;
          }
        } break;
        case '^': {
          token.kind = Token::CARET;
        } break;
        case '|': {
          if (*token.span.end() == '|') {
            extend_span(token.span);
            token.kind = Token::PIPE_PIPE;
          } else {
            token.kind = Token::PIPE;
          }
        } break;
        case '=': {
          if (*token.span.end() == '=') {
            extend_span(token.span);
            token.kind = Token::EQUAL_EQUAL;
          } else {
            token.kind = Token::EQUAL;
          }
        } break;
        case '!': {
          if (*token.span.end() == '=') {
            extend_span(token.span);
            token.kind = Token::BANG_EQUAL;
          } else {
            token.kind = Token::BANG;
          }
        } break;
        case '<': {
          if (*token.span.end() == '<') {
            extend_span(token.span);
            token.kind = Token::LESS_LESS;
          } else if (*token.span.end() == '=') {
            extend_span(token.span);
            token.kind = Token::LESS_EQUAL;
          } else {
            token.kind = Token::LESS;
          }
        } break;
        case '>': {
          if (*token.span.end() == '>') {
            extend_span(token.span);
            token.kind = Token::GREATER_GREATER;
          } else if (*token.span.end() == '=') {
            extend_span(token.span);
            token.kind = Token::GREATER_EQUAL;
          } else {
            token.kind = Token::GREATER;
          }
        } break;
        case '+': {
          if (*token.span.end() == '+') {
            extend_span(token.span);
            token.kind = Token::PLUS_PLUS;
          } else if (*token.span.end() == '=') {
            extend_span(token.span);
            token.kind = Token::PLUS_EQUAL;
          } else {
            token.kind = Token::PLUS;
          }
        } break;
        case '-': {
          if (*token.span.end() == '-') {
            extend_span(token.span);
            token.kind = Token::MINUS_MINUS;
          } else if (*token.span.end() == '=') {
            extend_span(token.span);
            token.kind = Token::MINUS_EQUAL;
          } else {
            token.kind = Token::MINUS;
          }
        } break;
        case '*': {
          if (*token.span.end() == '=') {
            extend_span(token.span);
            token.kind = Token::STAR_EQUAL;
          } else {
            token.kind = Token::STAR;
          }
        } break;
        case '/': {
          if (*token.span.end() == '=') {
            extend_span(token.span);
            token.kind = Token::SLASH_EQUAL;
          } else {
            token.kind = Token::SLASH;
          }
        } break;
        case '%': {
          if (*token.span.end() == '=') {
            extend_span(token.span);
            token.kind = Token::PERCENT_EQUAL;
          } else {
            token.kind = Token::PERCENT;
          }
        } break;
        case '(':
          token.kind = Token::OPEN_PAREN;
          break;
        case ')':
          token.kind = Token::CLOSE_PAREN;
          break;
        case '[':
          token.kind = Token::OPEN_SQUARE_BRACKET;
          break;
        case ']':
          token.kind = Token::CLOSE_SQUARE_BRACKET;
          break;
        case '{':
          token.kind = Token::OPEN_CURLY_BRACKET;
          break;
        case '}':
          token.kind = Token::CLOSE_CURLY_BRACKET;
          break;
        case ',':
          token.kind = Token::COMMA;
          break;
        case '.':
          token.kind = Token::DOT;
          break;
        case ';':
          token.kind = Token::SEMICOLON;
          break;
        case ':':
          token.kind = Token::COLON;
          break;
        case '?':
          token.kind = Token::QUESTION_MARK;
          break;
        case '\r':
          if (token.span.end()[0] == '\n') extend_span(token.span);
          // fallthrough
        case '\n':
          new_line = true;
          line_num++;
          line_start = token.span.end();
          break;
        case ' ':
        case '\t':
          break;
        default: {
          tokenizer_error("error: unrecognised token: {}", token);
          print_error(line_num, line_start, token.span.data(), token.span.data(), token.span.end());
        } break;
      }
    } while (token.kind == Token::NONE);

    current = token.span.end();
    if (token.kind != Token::END && token.span.size() == 0) {
      current++;
      tokenizer_error("internal error: zero-length token!");
    }

    return token;
  }

} // namespace tesl

#include "tesl_tokenizer.hpp"

#include "tesl_str.hpp"
#include "tesl_vector.hpp"

namespace tesl {
  bool is_digit(char c) {
    return c >= '0' && c <= '9';
  }

  bool is_alpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
  }

  bool validate_id(const char * str, const char *& end) {
    if (is_alpha(*str) || *str == '_') {
      str++;
      while (is_alpha(*str) || is_digit(*str) || *str == '_') str++;
      end = str;
      return true;
    }
    return false;
  }

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
      tok.name = {current, parsed_float_end - current};
      tok.literal = parsed_float;
    } else if (parsed_int_end > current) {
      // strtol parsed something...
      tok.kind = Token::LITERAL;
      tok.name = {current, parsed_int_end - current};
      tok.literal = parsed_int;
    }
    
    return tok;
  }

  char32_t Tokenizer::_parse_octal_char() {
    char32_t result = 0;
    for (IntT i = 0; i < 3; ++i) {
      char c = *current;
      if (c < '0' || c > '7') {
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
          error("error: invalid hex number!");
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
    tok.name._ptr = current;

    StrT result;
    result = StrT();
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
          error("error: unexpected end of file!");
          print_error(line_num, line_start, tok.name.ptr(), current - 1, current);
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
              error("error: unknown escape sequence!");
              print_error(line_num, line_start, tok.name.ptr(), current - 1, current);
              end_of_str = true;
              break;
          }
          break;
        default:
          result += c;
          break;
      }
    }

    tok.name._end = current;
    tok.kind = Token::LITERAL;
    tok.literal = MOV(result);
    return tok;
  }

  Token Tokenizer::next_token() {
    Token token;
    token.name._ptr = current;
    token.name._end = current;

    bool new_line = token.name.end() == input;
    do {
      {
        const char * tok_start = token.name.end();
        token = {};
        token.name._ptr = token.name._end = tok_start;
        current = token.name._ptr;
      }

      if (token.name[0] == '\0') {
        token.kind = Token::END;
        continue;
      }

      // number literal
      if (is_digit(token.name[0]) || (token.name[0] == '.' && is_digit(token.name[1]))) {
        token = _tokenize_number_literal();
        continue;
      }

      // string literal
      if (token.name[0] == '"') {
        token = _tokenize_string_literal();
        continue;
      }

      if (validate_id(token.name._ptr, token.name._end)) {
        const IntT id_len = token.name.length();

#define MATCHES_TOKEN_(str) (id_len == (sizeof(str) - 1) && strncmp(str, token.name.ptr(), id_len) == 0)
        switch (token.name[0]) {
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

      /* Look for an operator or special character. */
      switch (*token.name._end++) {
        case '&': {
          if (*token.name._end == '&') {
            token.name._end++;
            token.kind = Token::AND_AND;
          } else {
            token.kind = Token::AND;
          }
        } break;
        case '^': {
          if (*token.name._end == '^') {
            token.name._end++;
            token.kind = Token::CARET_CARET;
          } else {
            token.kind = Token::CARET;
          }
        } break;
        case '|': {
          if (*token.name._end == '|') {
            token.name._end++;
            token.kind = Token::PIPE_PIPE;
          } else {
            token.kind = Token::PIPE;
          }
        } break;
        case '=': {
          if (*token.name._end == '=') {
            token.name._end++;
            token.kind = Token::EQUAL_EQUAL;
          } else {
            token.kind = Token::EQUAL;
          }
        } break;
        case '!': {
          if (*token.name._end == '=') {
            token.name._end++;
            token.kind = Token::BANG_EQUAL;
          } else {
            token.kind = Token::BANG;
          }
        } break;
        case '<': {
          if (*token.name._end == '<') {
            token.name._end++;
            token.kind = Token::LESS_LESS;
          } else if (*token.name._end == '=') {
            token.name._end++;
            token.kind = Token::LESS_EQUAL;
          } else {
            token.kind = Token::LESS;
          }
        } break;
        case '>': {
          if (*token.name._end == '>') {
            token.name._end++;
            token.kind = Token::GREATER_GREATER;
          } else if (*token.name._end == '=') {
            token.name._end++;
            token.kind = Token::GREATER_EQUAL;
          } else {
            token.kind = Token::GREATER;
          }
        } break;
        case '+': {
          if (*token.name._end == '+') {
            token.name._end++;
            token.kind = Token::PLUS_PLUS;
          } else if (*token.name._end == '=') {
            token.name._end++;
            token.kind = Token::PLUS_EQUAL;
          } else {
            token.kind = Token::PLUS;
          }
        } break;
        case '-': {
          if (*token.name._end == '-') {
            token.name._end++;
            token.kind = Token::MINUS_MINUS;
          } else if (*token.name._end == '=') {
            token.name._end++;
            token.kind = Token::MINUS_EQUAL;
          } else {
            token.kind = Token::MINUS;
          }
        } break;
        case '*': {
          if (*token.name._end == '=') {
            token.name._end++;
            token.kind = Token::STAR_EQUAL;
          } else {
            token.kind = Token::STAR;
          }
        } break;
        case '/': {
          if (*token.name._end == '=') {
            token.name._end++;
            token.kind = Token::SLASH_EQUAL;
          } else {
            token.kind = Token::SLASH;
          }
        } break;
        case '%': {
          if (*token.name._end == '=') {
            token.name._end++;
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
          if (token.name._end[0] == '\n') token.name._end++;
          // fallthrough
        case '\n':
          new_line = true;
          line_num++;
          line_start = token.name.end();
          break;
        case ' ':
        case '\t':
          break;
        default: {
          error("error: unrecognised token: ", token);
          print_error(line_num, line_start, token.name.ptr(), token.name.ptr(), token.name.end());
        } break;
      }
    } while (token.kind == Token::NONE);
#ifdef TESL_DEBUG_TOKENIZER
    tesl_printf("token: ");
    print(token);
    tesl_printf("\n");
#endif
  
    current = token.name.end();
    if (token.kind != Token::END && token.name.length() == 0) {
      current++;
      error("internal error: zero-length token!");
    }

    return token;
  }

  void print(Token::Kind tok) {
    static constexpr const char * hex_chars = "0123456789abcdef";

    switch (tok) {
      case Token::NONE: tesl_printf("<none>"); return;
      case Token::END: tesl_printf("<end>"); return;
      case Token::IDENTIFIER: tesl_printf("<identifier>"); return;
      case Token::LITERAL: tesl_printf("<literal>"); return;
      case Token::COMMA: tesl_printf(","); return;
      case Token::DOT: tesl_printf("."); return;
      case Token::SEMICOLON: tesl_printf(";"); return;
      case Token::COLON: tesl_printf(":"); return;
      case Token::QUESTION_MARK: tesl_printf("?"); return;
      case Token::OPEN_PAREN: tesl_printf("("); return;
      case Token::CLOSE_PAREN: tesl_printf("); return;"); return;
      case Token::OPEN_SQUARE_BRACKET: tesl_printf("["); return;
      case Token::CLOSE_SQUARE_BRACKET: tesl_printf("]"); return;
      case Token::OPEN_CURLY_BRACKET: tesl_printf("{"); return;
      case Token::CLOSE_CURLY_BRACKET: tesl_printf("}"); return;
      case Token::IF: tesl_printf("if"); return;
      case Token::ELSE: tesl_printf("else"); return;
      case Token::SWITCH: tesl_printf("switch"); return;
      case Token::CASE: tesl_printf("case"); return;
      case Token::FOR: tesl_printf("for"); return;
      case Token::WHILE: tesl_printf("while"); return;
      case Token::DO: tesl_printf("do"); return;
      case Token::BREAK: tesl_printf("break"); return;
      case Token::CONTINUE: tesl_printf("continue"); return;
      case Token::RETURN: tesl_printf("return"); return;
      case Token::AND: tesl_printf("&"); return;
      case Token::AND_AND: tesl_printf("&&"); return;
      case Token::CARET: tesl_printf("^"); return;
      case Token::CARET_CARET: tesl_printf("^^"); return;
      case Token::PIPE: tesl_printf("|"); return;
      case Token::PIPE_PIPE: tesl_printf("||"); return;
      case Token::EQUAL: tesl_printf("="); return;
      case Token::EQUAL_EQUAL: tesl_printf("=="); return;
      case Token::BANG: tesl_printf("!"); return;
      case Token::BANG_EQUAL: tesl_printf("!="); return;
      case Token::LESS: tesl_printf("<"); return;
      case Token::LESS_LESS: tesl_printf("<<"); return;
      case Token::LESS_EQUAL: tesl_printf("<="); return;
      case Token::GREATER: tesl_printf(">"); return;
      case Token::GREATER_GREATER: tesl_printf(">>"); return;
      case Token::GREATER_EQUAL: tesl_printf(">="); return;
      case Token::PLUS: tesl_printf("+"); return;
      case Token::PLUS_PLUS: tesl_printf("++"); return;
      case Token::PLUS_EQUAL: tesl_printf("+="); return;
      case Token::MINUS: tesl_printf("-"); return;
      case Token::MINUS_MINUS: tesl_printf("--"); return;
      case Token::MINUS_EQUAL: tesl_printf("-="); return;
      case Token::STAR: tesl_printf("*"); return;
      case Token::STAR_EQUAL: tesl_printf("*="); return;
      case Token::SLASH: tesl_printf("/"); return;
      case Token::SLASH_EQUAL: tesl_printf("/="); return;
      case Token::PERCENT: tesl_printf("%"); return;
      case Token::PERCENT_EQUAL: tesl_printf("%="); return;
    }

    tesl_printf("<unknown_token:0x");
    char tmp[] = {hex_chars[(tok >> 4) & 0xf], hex_chars[(tok) & 0xf], '\0'};
    tesl_printf(tmp);
    tesl_printf(">");
  }

  void print(Token tok) {
    switch (tok.kind) {
      case Token::NONE:
      case Token::END:
        print(tok.kind);
        return;
      case Token::IDENTIFIER:
        print("identifier '");
        print(tok.name);
        print("'");
        return;
      case Token::LITERAL:
        print("literal ");
        print(*tok.literal.type);
        return;
      case Token::COMMA:
      case Token::DOT:
      case Token::SEMICOLON:
      case Token::COLON:
      case Token::QUESTION_MARK:
      case Token::OPEN_PAREN:
      case Token::CLOSE_PAREN:
      case Token::OPEN_SQUARE_BRACKET:
      case Token::CLOSE_SQUARE_BRACKET:
      case Token::OPEN_CURLY_BRACKET:
      case Token::CLOSE_CURLY_BRACKET:
      case Token::IF:
      case Token::ELSE:
      case Token::SWITCH:
      case Token::CASE:
      case Token::FOR:
      case Token::WHILE:
      case Token::DO:
      case Token::BREAK:
      case Token::CONTINUE:
      case Token::RETURN:
      case Token::AND:
      case Token::AND_AND:
      case Token::CARET:
      case Token::CARET_CARET:
      case Token::PIPE:
      case Token::PIPE_PIPE:
      case Token::EQUAL:
      case Token::EQUAL_EQUAL:
      case Token::BANG:
      case Token::BANG_EQUAL:
      case Token::LESS:
      case Token::LESS_LESS:
      case Token::LESS_EQUAL:
      case Token::GREATER:
      case Token::GREATER_GREATER:
      case Token::GREATER_EQUAL:
      case Token::PLUS:
      case Token::PLUS_PLUS:
      case Token::PLUS_EQUAL:
      case Token::MINUS:
      case Token::MINUS_MINUS:
      case Token::MINUS_EQUAL:
      case Token::STAR:
      case Token::STAR_EQUAL:
      case Token::SLASH:
      case Token::SLASH_EQUAL:
      case Token::PERCENT:
      case Token::PERCENT_EQUAL:
        print("'");
        print(tok.kind);
        print("'");
        return;
    }
  }
} // namespace tesl

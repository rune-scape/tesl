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

#define tokenizer_error(...) \
  do { \
    has_error = true; \
    fmt::println(stderr, __VA_ARGS__); \
  } while(false)

  bool Tokenizer::_try_tokenize_number_literal(Token & tok) {
    if (is_digit(current[0]) || (current[0] == '.' && is_digit(current[1]))) {
      char * parsed_int_end = nullptr;
      // todo: maybe custom parser for better errors
      IntT parsed_int = strtol(current, &parsed_int_end, 0);
      char * parsed_float_end = nullptr;
      FloatT parsed_float = strtof(current, &parsed_float_end);
      if (parsed_float_end > parsed_int_end) {
        // strtof parsed more, use that

        tok = {
          Token::LITERAL,
          {current, static_cast<CharStrView::size_type>(parsed_float_end - current)},
          parsed_float
        };
        return true;
      } else if (parsed_int_end > current) {
        // strtol parsed something...
        tok = {
          Token::LITERAL,
          {current, static_cast<CharStrView::size_type>(parsed_int_end - current)},
          parsed_int
        };
        return true;
      }
    }

    return false;
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

    tok = {
      Token::LITERAL,
      {tok.span.begin(), static_cast<CharStrView::size_type>(current - tok.span.begin())},
      MOV(result)
    };
    return tok;
  }

  bool Tokenizer::_try_tokenize_identifier_literal(Token & tok) {
    if (is_valid_id_starter(tok.span[0])) {
      CharStrView::size_type len = 1;
      while (is_valid_id_meat(tok.span[len])) {
        len++;
      }
      tok = {
        Token::IDENTIFIER,
        {tok.span.begin(), len}
      };
      return true;
    }
    return false;
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

      switch (current[0]) {
        case '\0':
          // end of input token
          token.kind = Token::END;
          break;
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
          // number literal token
          _try_tokenize_number_literal(token);
          break;
        case '"':
          // string literal token
          token = _tokenize_string_literal();
          break;
        case '\r':
          if (token.span.end()[1] == '\n') {
            extend_span(token.span);
          }
          // fallthrough
        case '\n':
          extend_span(token.span);
          new_line = true;
          line_num++;
          line_start = token.span.end();
          break;
        case ' ':
        case '\t':
          extend_span(token.span);
          break;
        default: {
          tokenizer_error("error: unrecognised token: {}", token);
          print_error(line_num, line_start, token.span.data(), token.span.data(), token.span.data() + 1);
        } break;

#define MATCH_TOKEN(str) \
  strncmp(current + 1, str + 1, sizeof(str)-1 - 1) == 0
#define TOKEN(str, name) \
  if (MATCH_TOKEN(str)) { \
    token.kind = Token::name; \
    token.span = {token.span.begin(), sizeof(str)-1}; \
    break; \
  }
#define TOKEN_LITERAL(str, value) \
  if (MATCH_TOKEN(str)) { \
    token.kind = Token::LITERAL; \
    token.literal = value; \
    token.span = {token.span.begin(), sizeof(str)-1}; \
    break; \
  }
#define TOKEN_NUMBER \
  if (_try_tokenize_number_literal(token)) { \
    break; \
  }
#define GROUP_START(c) \
  case c: {
#define GROUP_END \
  } break;
#include "tesl_tokens.inl"
#undef GROUP_END
#undef GROUP_START
#undef TOKEN_NUMBER
#undef TOKEN_LITERAL
#undef TOKEN
#undef MATCH_TOKEN

      }

      if (token.kind != Token::NONE) {
        break;
      }

      // identifier token
      if (_try_tokenize_identifier_literal(token)) {
        break;
      }
#ifdef TESL_DEBUG_TOKENIZER
      if (token.kind == Token::NONE && token.span.size() == 0) {
        tokenizer_error("internal error: failed to consume any input! (char value: {:#04})", static_cast<int>(token.span[0]));
        extend_span(token.span);
      }
#endif
    } while (true);

    current = token.span.end();
#ifdef TESL_DEBUG_TOKENIZER
    if (token.kind != Token::END && token.span.size() == 0) {
      tokenizer_error("internal error: zero-length token! (char value: {:#04})", static_cast<int>(token.span[0]));
      current++;
    }
#endif

    return token;
  }

} // namespace tesl

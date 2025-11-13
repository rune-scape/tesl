#include "tesl_tokenizer.hpp"

#include "tesl_common.hpp"
#include "tesl_coroutine.hpp"
#include "tesl_env.hpp"
#include "tesl_grammar.hpp"
#include "tesl_str.hpp"
#include "tesl_parse.hpp"
#include <fmt/format.h>

namespace tesl {
  const char * get_token_name(Token::Kind t) {
    switch (t) {

#define TESL_TOKEN_META_DEF(str, name) \
  case Token::name: return str;
#define TESL_TOKEN_SYMBOL_DEF(str, name) \
  case Token::name: return str;
#define TESL_TOKEN_KEYWORD_DEF(str, name) \
  case Token::name: return str;
#define TESL_TOKEN_LITERAL_KEYWORD_DEF(str, value)
#define TESL_TOKEN_GROUP(...)
#define TESL_TOKEN_GROUP_END
#include "tesl_tokens.inc"

    }

    TESL_UNREACHABLE;
  }

  bool is_token_meta(Token::Kind t) {
    switch (t) {

#define TESL_TOKEN_META_DEF(str, name) \
  case Token::name: return true;
#define TESL_TOKEN_SYMBOL_DEF(str, name) \
  case Token::name: return false;
#define TESL_TOKEN_KEYWORD_DEF(str, name) \
  case Token::name: return false;
#define TESL_TOKEN_LITERAL_KEYWORD_DEF(str, value)
#define TESL_TOKEN_GROUP(...)
#define TESL_TOKEN_GROUP_END
#include "tesl_tokens.inc"

    }

    return false;
  }

#define tokenizer_error(...) \
  do { \
    has_error = true; \
    TESL_ERROR_PRINT_MSG_BASE("tokenizer error", __VA_ARGS__); \
  } while(false)

#define internal_tokenizer_error(...) \
  do { \
    has_error = true; \
    TESL_ERROR_PRINT_MSG_BASE("internal tokenizer error", __VA_ARGS__); \
  } while(false)

  Token Tokenizer::_tokenize_number_literal(const char * num_span_begin, parse::ParsedNumber & parsed_number) {
    using namespace parse;

    auto end = input.end();
    auto num_span_end = input_it;

    input_it = find_valid_id_end(input_it, end);
    if (num_span_end != input_it) {
      tokenizer_error("unexpected text after number");
      print_error_source(line_num, line_start, num_span_end, num_span_end, input_it);
      return {
        Token::NONE,
        {num_span_begin, input_it}
      };
    }

    if (!parsed_number.has_integer_digits && !parsed_number.has_fractional_digits) {
      internal_tokenizer_error("expected integer or fractional digits");
      print_error_source(line_num, line_start, num_span_begin, num_span_begin, num_span_end);
      return {
        Token::NONE,
        {num_span_begin, input_it}
      };
    }

    switch (parsed_number.error) {
      case ParseNumberError::no_error:
        break;
      case ParseNumberError::no_input:
        internal_tokenizer_error("expected number literal input!");
        print_error_source(line_num, line_start, num_span_end, num_span_end, num_span_end + 1);
        return {
          Token::NONE,
          {num_span_begin, input_it}
        };
      case ParseNumberError::missing_exponent_digits:
        tokenizer_error("missing exponent");
        print_error_source(line_num, line_start, num_span_end, num_span_end, input_it);
        return {
          Token::NONE,
          {num_span_begin, input_it}
        };
    }

    if (parsed_number.has_decimal_point || parsed_number.has_exponent) {
      // todo: warn about number being too large and other stuff
      return {Token::LITERAL, {num_span_begin, num_span_end}, parsed_number.get_float()};
    } else {
      if (parsed_number.is_int_too_big()) {
        tokenizer_error("integer too big (max={})", std::numeric_limits<IntT>::max());
        print_error_source(line_num, line_start, num_span_begin, num_span_begin, num_span_end);
        return {
          Token::NONE,
          {num_span_begin, input_it}
        };
      }
      return {Token::LITERAL, {num_span_begin, num_span_end}, parsed_number.get_int()};
    }
  }

  Token Tokenizer::_tokenize_string_literal(char quote_char) {
    using namespace parse;

    const char * begin = input_it;
    const char * end = input.end();

#ifdef TESL_DEBUG_TOKENIZER
    if (input_it == input.end()) {
      internal_tokenizer_error("expected string literal starter, got end of input");
      return {
        Token::END,
        {input_it, input_it}
      };
    }
    if (*input_it != quote_char) {
      input_it++;
      internal_tokenizer_error("invalid string literal starter");
      print_error_source(line_num, line_start, begin, begin, input_it);
      return {
        Token::NONE,
        {begin, input_it}
      };
    }
#endif

    input_it++;

    // todo: try to partially recover from error to avoid snowballing errors
    Str result;
    while (input_it < end) {
      char c = *input_it;
      switch (c) {
        case '"':
        case '\'':
          input_it++;
          if (c == quote_char) {
            return {Token::LITERAL, {begin, input_it}, MOV(result)};
          }
          result += c;
          break;
        case '\r':
        case '\n':
          tokenizer_error("unexpected end of line");
          print_error_source(line_num, line_start, begin, input_it, input_it + 1);
          return {Token::LITERAL, {begin, input_it}, MOV(result)};
        case '\0':
          tokenizer_error("unexpected null");
          print_error_source(line_num, line_start, begin, input_it, input_it + 1);
          return {Token::LITERAL, {begin, input_it}, MOV(result)};
        case '\\': {
          const char * escape_seq_begin = input_it;
          input_it++;
          auto char_escape_result = parse_character_escape<char>(input_it, end);
          switch (char_escape_result.error) {
            case ParseCharacterEscapeError::no_error:
              break;
            case ParseCharacterEscapeError::no_input:
              break;
            case ParseCharacterEscapeError::out_of_range:
              tokenizer_error("character escape out of range");
              print_error_source(line_num, line_start, escape_seq_begin, escape_seq_begin, input_it);
              break;
            case ParseCharacterEscapeError::incomplete_escape:
              tokenizer_error("incomplete uncode escape");
              print_error_source(line_num, line_start, escape_seq_begin, escape_seq_begin, input_it);
              break;
            case ParseCharacterEscapeError::unknown_escape:
              tokenizer_error("unknown escape sequence");
              print_error_source(line_num, line_start, escape_seq_begin, escape_seq_begin, input_it);
              break;
          }
          result += char_escape_result.c;
        } break;
        default:
          result += c;
          input_it++;
          break;
      }
    }

    tokenizer_error("unexpected end of input");
    print_error_source(line_num, line_start, input_it - 1, input_it - 1, input_it);
    return {Token::LITERAL, {begin, input_it}, MOV(result)};
  }

  Token Tokenizer::_tokenize_identifier() {
    using namespace parse;

#ifdef TESL_DEBUG_TOKENIZER
    if (input_it >= input.end() || !is_valid_id_starter(*input_it)) {
      internal_tokenizer_error("invalid identifier starter");
      print_error_source(line_num, line_start, input_it, input_it, min(input_it + 1, input.end()));
      return {};
    }
#endif
    auto id_begin = input_it;
    input_it = find_valid_id_end(input_it + 1, input.end());
    return {
      Token::IDENTIFIER,
      CharStrView{id_begin, input_it}
    };
  }

  static bool match_symbol(CharStrView input, CharStrView match_str) {
    using namespace parse;

    using traits = std::char_traits<char>;
    auto len = min(input.length(), match_str.length());
    return 0 == traits::compare(input.data(), match_str.data(), len);
  }

  static bool match_keyword(CharStrView input, CharStrView match_str) {
    using namespace parse;

    using traits = std::char_traits<char>;
    auto len = min(input.length(), match_str.length());
    return 0 == traits::compare(input.data(), match_str.data(), len) && input.length() < len && !is_valid_id_meat(input.data()[len]);
  }

  Token Tokenizer::_next_token() {
    using namespace parse;

    const char * const end = input.end();

    //bool new_line = input_it == input;
    while (input_it < end) {
      const char * parsed_number_begin = input_it;
      ParsedNumber parsed_number;
      const char * next_it = input_it + 1;
      switch (*input_it) {
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
          parse_number_integer_part(parsed_number, input_it, end);
          if (*input_it == '.') {
            next_it = input_it + 1;
            goto decimal_point;
          }
          parse_number_exponent_part(parsed_number, input_it, end);
          return _tokenize_number_literal(parsed_number_begin, parsed_number);
        case '"':
          // string literal token
          return _tokenize_string_literal('"');
        case '\'':
          // alternate string literal token
          return _tokenize_string_literal('\'');
        case '#': {
          // comment
          const char * comment_begin = input_it;
          input_it++;
          input_it = find_line_end(input_it, input.end());
          line_num++;
          //new_line = true;
          line_start = input_it;
          if (strict_parsing) {
            tokenizer_error("strict parsing does not allow comments");
            print_error_source(line_num, line_start, comment_begin, comment_begin, input_it);
            return {};
          }
        } continue;
        case '\r':
          if (input_it < end && *input_it == '\n') {
            input_it++;
          }
          [[fallthrough]];
        case '\n': {
          input_it++;
          line_num++;
          //new_line = true;
          line_start = input_it;
          if (strict_parsing) {
            tokenizer_error("strict parsing does not allow newlines");
            print_error_source(line_num, line_start, input_it - 1, input_it - 1, input_it);
            return {};
          }
        } continue;
        case ' ':
        case '\t':
          input_it++;
          if (strict_parsing) {
            tokenizer_error("strict parsing does not allow spaces or tabs");
            print_error_source(line_num, line_start, input_it - 1, input_it - 1, input_it);
            return {};
          }
          continue;
        default:
          break;

#define TESL_TOKEN_META_DEF(str, name)
#define TESL_TOKEN_SYMBOL_DEF(str, name) \
  if (match_symbol({next_it, static_cast<SizeT>(end - next_it)}, {&str[1], sizeof(str)-1 - 1})) { \
    Token result{Token::name, {input_it, sizeof(str)-1}}; \
    input_it = result.span.end(); \
    return result; \
  }
#define TESL_TOKEN_KEYWORD_DEF(str, name) \
  if (match_keyword({next_it, static_cast<SizeT>(end - next_it)}, {&str[1], sizeof(str)-1 - 1})) { \
    Token result{Token::name, {input_it, sizeof(str)-1}}; \
    input_it = result.span.end(); \
    return result; \
  }
#define TESL_TOKEN_LITERAL_KEYWORD_DEF(str, value) \
  if (match_keyword({next_it, static_cast<SizeT>(end - next_it)}, {&str[1], sizeof(str)-1 - 1})) { \
    Token result{Token::LITERAL, {input_it, sizeof(str)-1}, value}; \
    input_it = result.span.end(); \
    return result; \
  }
#define TESL_TOKEN_DECIMAL_POINT \
  decimal_point: { \
    const char * tmp_input_it = input_it; \
    ParsedNumber parsed_float{parsed_number}; \
    parse_number_fractional_part(parsed_float, tmp_input_it, end); \
    if (parsed_float.has_integer_digits || parsed_float.has_fractional_digits) { \
      parse_number_exponent_part(parsed_float, tmp_input_it, end); \
      if (parsed_float.has_fractional_digits || parsed_float.has_exponent) { \
        input_it = tmp_input_it; \
        return _tokenize_number_literal(parsed_number_begin, parsed_float); \
      } else { \
        return _tokenize_number_literal(parsed_number_begin, parsed_number); \
      } \
    } \
  }
#define TESL_TOKEN_FORWARD_SLASH \
  if (next_it != end && *next_it == '/') { \
    input_it = find_line_end(input_it, input.end()); \
    continue; \
  }
#define TESL_TOKEN_GROUP(c) \
  case c: {
#define TESL_TOKEN_GROUP_END \
  } break;
#include "tesl_tokens.inc"

      }

      // identifier token
      if (is_valid_id_starter(*input_it)) {
        return _tokenize_identifier();
      }

      tokenizer_error("unrecognised character: {:?}", *input_it);
      print_error_source(line_num, line_start, input_it, input_it, min(input_it + 1, end));
      input_it++;
    }

    return {Token::END, {input_it, 0}};
  }

  Token & Tokenizer::next() {
    const char * const begin = input_it;
    _current = MOV(_next_token());

#ifdef TESL_DEBUG_TOKENIZER
    if (input_it != _current.span.end()) {
      internal_tokenizer_error("input iter desynced from token end!");
      print_error_source(line_num, line_start, begin, begin, min(begin + 1, input.end()));
      _current.span = {begin, 1};
    }
    if (input_it < begin) {
      internal_tokenizer_error("input iter moved backward!");
      print_error_source(line_num, line_start, begin, begin, min(begin + 1, input.end()));
      _current.span = {begin, 1};
    }
    if (_current.span.begin() < begin) {
      internal_tokenizer_error("token span.begin() moved backwards!");
      print_error_source(line_num, line_start, begin, begin, min(begin + 1, input.end()));
      _current.span = {begin, 1};
    }
    if (_current.kind != Token::END && _current.span.length() == 0) {
      internal_tokenizer_error("zero-length token!");
      print_error_source(line_num, line_start, begin, begin, min(begin + 1, input.end()));
      _current.span = {begin, 1};
    }
#endif

    _has_current_token = true;
    return _current;
  }

  Token & Tokenizer::current() {
    if (!_has_current_token) {
      return next();
    }
    return _current;
  }

  void Tokenizer::reset() {
    input_it = input.begin();
    line_start = input.begin();
    line_num = 1;
    has_error = false;
    _has_current_token = false;
  }

  Tokenizer::Tokenizer(CharStrView p_input) : input(p_input), input_it(p_input.begin()), line_start(p_input.begin()) {}

  Tokenizer::~Tokenizer() = default;

  grammar::SyntaxStream make_token_generator(Tokenizer tokenizer) {
    using namespace grammar;

    while (true) {
      Token t = tokenizer._next_token();
      switch (t.kind) {
        case Token::END:
          co_return;
        case Token::IDENTIFIER:
          co_yield SyntaxNode::make_identifier(Env::current->add_name(t.span));
        case Token::LITERAL:
          co_yield SyntaxNode::make_literal(t.literal);
        default:
          co_yield SyntaxNode::make_token(t.kind);
      }
    }
  }
}

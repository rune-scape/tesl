#pragma once

#include "tesl_common.hpp"
#include "tesl_math.hpp"
#include <cmath>
#include <limits>
#include <string>

namespace tesl::parse {
  template<typename CharT>
  constexpr bool is_digit(CharT c) {
    return static_cast<UIntT>(c - '0') < 10u;
  }

  template<typename CharT>
  constexpr bool is_octal_digit(CharT c) {
    return static_cast<UIntT>(c - '0') < 8u;
  }

  template<typename CharT>
  constexpr bool is_hex_digit(CharT c) {
    return is_digit(c) || static_cast<UIntT>((c | 32u) - 'a') < 6u;
  }

  template<typename CharT>
  constexpr bool is_alpha(CharT c) {
    return static_cast<UIntT>((c | 32u) - 'a') < 26u;
  }

  template<typename CharT>
  constexpr bool is_valid_id_starter(CharT c) {
    return is_alpha(c) || c == '_';
  }

  template<typename CharT>
  constexpr bool is_valid_id_meat(CharT c) {
    return is_digit(c) || is_valid_id_starter(c);
  }

  template<typename IterT, typename SentinelT>
  IterT find_valid_id_end(IterT it, SentinelT end) {
    while (it != end && is_valid_id_meat(*it)) {
      it++;
    }
    return it;
  }

  /*template<typename CharT>
  constexpr bool remove_prefix(BasicStrViewT<CharT> & str, BasicStrViewT<CharT> prefix) {
    if (str.length() >= prefix.length()) {
        return 0 == BasicStrViewT<CharT>::traits_type::compare(str.data(), prefix.data(), prefix.length());
    }

    return false;
  }*/

  template<typename IterT, typename SentinelT>
  IterT find_line_end(IterT it, SentinelT end) {
    for (; it != end; ++it) {
      switch (*it) {
        case '\r':
          return it;
        case '\n':
          return it;
      }
    }
    return it;
  }

  template<typename IterT, typename SentinelT>
  IterT find_next_line(IterT it, SentinelT end) {
    for (; it != end; ++it) {
      switch (*it) {
        case '\r':
          it++;
          if (it != end && *it == '\n') {
            it++;
          }
          return it;
        case '\n':
          it++;
          return it;
      }
    }
    return it;
  }

  // negative return value means it didnt parse
  // -2 means it was one of [a-zA-Z0-9_] (means it wasn't identifier meat)
  template<typename CharT>
  constexpr uint8_t char_to_number(CharT c) {
    std::uint8_t char_map[] = {
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      0  , 1  , 2  , 3  , 4  , 5  , 6  , 7  ,
      8  , 9  , 255, 255, 255, 255, 255, 255,
      255, 10 , 11 , 12 , 13 , 14 , 15 , 16 ,
      17 , 18 , 19 , 20 , 21 , 22 , 23 , 24 ,
      25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 ,
      33 , 34 , 35 , 255, 255, 255, 255, 255,
      255, 10 , 11 , 12 , 13 , 14 , 15 , 16 ,
      17 , 18 , 19 , 20 , 21 , 22 , 23 , 24 ,
      25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 ,
      33 , 34 , 35 , 255, 255, 255, 255, 255
    };
    
    auto i = static_cast<UIntT>(c);
    if (i < std::size(char_map)) {
      return char_map[i];
    }
    return 255;
  }

  template<typename T, IntT Base>
  struct representable_digits_for_base {};

  template<typename T>
  struct representable_digits_for_base<T, 2> { static constexpr IntT value = std::numeric_limits<T>::digits; };

  template<typename T>
  struct representable_digits_for_base<T, 8> { static constexpr IntT value = std::numeric_limits<T>::digits / 3; };

  template<typename T>
  struct representable_digits_for_base<T, 16> { static constexpr IntT value = std::numeric_limits<T>::digits / 4; };

  template<typename T>
  struct representable_digits_for_base<T, 10> { static constexpr IntT value = std::numeric_limits<T>::digits10; };

  template<typename T, IntT Base>
  constexpr IntT representable_digits_for_base_v = representable_digits_for_base<T, Base>::value;
  //constexpr IntT representable_digits_for_base_v = static_cast<IntT>(std::numeric_limits<T>::digits * std::log(2.0) / std::log(static_cast<double>(Base)));

  // if it would not overflow, computes `*a = *a + b` and returns true
  // otherwise returns false
  template <typename T>
  bool add_avoid_overflow(T * a_ptr, T b) {
    T a = *a_ptr;
    if (b >= 0) {
      if (a <= (std::numeric_limits<T>::max() - b)) {
        *a_ptr = a + b;
        return true;
      }
    } else {
      if (a >= (std::numeric_limits<T>::min() - b)) {
        *a_ptr = a + b;
        return true;
      }
    }
    return false;
  }

  // if it would not overflow, computes `*a = *a - b` and returns true
  // otherwise returns false
  template <typename T>
  bool sub_avoid_overflow(T * a_ptr, T b) {
    T a = *a_ptr;
    if (b >= 0) {
      if (a >= (std::numeric_limits<T>::min() + b)) {
        *a_ptr = a - b;
        return true;
      }
    } else {
      if (a <= (std::numeric_limits<T>::max() + b)) {
        *a_ptr = a - b;
        return true;
      }
    }
    return false;
  }

  template <typename T>
  struct ParsedInteger {
    // number of leading zeros skipped
    IntT leading_zeros_parsed = 0;
    // number of significant digits parsed into `number`
    IntT significant_digits_parsed = 0;
    // digits parsed that could not fit into `number`
    IntT extra_digits_parsed = 0;
    // y'know
    T number = 0;
    bool would_overflow = false;
    // only 1 extra digit is stored (if and only if `would_overflow` is true), juust in case.
    //std::uint8_t extra_digit = 0;
  };

  // meant to be called after leading zeros have been skipped
  template<typename T, typename IterT, typename SentinelT>
  void parse_integer_impl(ParsedInteger<T> & result, IterT & it, SentinelT end, IntT base, IntT max_digits) {
    // restrict parsing end to what can be representable
    auto digits_to_parse = max_digits - result.significant_digits_parsed;

    for (; it != end && result.significant_digits_parsed < max_digits; ++it, ++result.significant_digits_parsed) {
      std::uint8_t num = char_to_number(*it);
      if (num >= base) {
        return;
      }
      result.number = static_cast<T>(result.number * base + num);
    }

    if (it == end) {
      return;
    }
    
    // now we check one extra
    std::uint8_t num = char_to_number(*it);
    if (num >= base) {
      return;
    }
    
    // see if it fits
    if (add_avoid_overflow<T>(&result.number, num)) {
      it++;
      result.significant_digits_parsed++;
      if (it == end) {
        return;
      }

      num = char_to_number(*it);
      if (num >= base) {
        return;
      }
    }

    //result.extra_digit = num;
    result.would_overflow = true;
    return;
  }

  template<typename IterT, typename SentinelT>
  void parse_integer_sign(IterT & it, SentinelT end, IntT & sign) {
    if (it == end) {
      sign = 1;
      return;
    }
  
    auto c = *it;
    if (c == '-') {
      it++;
      sign = -1;
    } else if (c == '+') {
      it++;
      sign = 1;
    }
  }

  // skip leading zeros and return number of digits parsed
  template<typename IterT, typename SentinelT>
  IntT parse_integer_leading_zeros(IterT & it, SentinelT end) {
    IntT digits_parsed = 0;
    for (; it != end && *it == '0'; ++it, ++digits_parsed);
    return digits_parsed;
  }

  // skip to end of number and return number of digits parsed
  template<typename IterT, typename SentinelT>
  IntT parse_integer_find_end(IterT & it, SentinelT end, UIntT base) {
    IntT digits_parsed = 0;
    for (; it != end; ++it, ++digits_parsed) {
      std::uint8_t num = char_to_number(*it);
      if (num >= base) {
        break;
      }
    }
    return digits_parsed;
  }

  // does not parse plus or minus
  template<typename T, UIntT Base, typename IterT, typename SentinelT>
  ParsedInteger<T> parse_integer(IterT & it, SentinelT end) {
    ParsedInteger<T> result;

    result.leading_zeros_parsed = parse_integer_leading_zeros(it, end);
    parse_integer_impl(result, it, end, Base, representable_digits_for_base_v<T, Base>);
    result.extra_digits_parsed = parse_integer_find_end(it, end, Base);
    return result;
  }

  enum class ParseCharacterEscapeError {
    no_error,
    no_input,
    out_of_range,
    incomplete_escape,
    unknown_escape,
  };

  struct ParsedCharacterEscape {
    char32_t c = 0;
    ParseCharacterEscapeError error = ParseCharacterEscapeError::no_error;
  };

  template<typename CharT, typename IterT, typename SentinelT>
  ParsedCharacterEscape parse_character_escape(IterT & it, SentinelT end) {
    ParsedCharacterEscape result;

    if (it == end) {
      result.error = ParseCharacterEscapeError::no_input;
      return result;
    }

    auto c = *it;
    switch (c) {
      case '\'': result.c = '\''; break;
      case '\"': result.c = '\"'; break;
      case '\?': result.c = '\?'; break;
      case '\\': result.c = '\\'; break;
      case 'a': result.c = '\a'; break;
      case 'b': result.c = '\b'; break;
      case 'e': result.c = '\33'; break;
      case 'f': result.c = '\f'; break;
      case 'n': result.c = '\n'; break;
      case 'r': result.c = '\r'; break;
      case 't': result.c = '\t'; break;
      case 'v': result.c = '\v'; break;

      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7': {
        // todo: add unicode conversions
        ParsedInteger<CharT> num_result;
        parse_integer_impl(num_result, it, end, 8, 3);
        if (num_result.would_overflow) {
          result.error = ParseCharacterEscapeError::out_of_range;
        }
        result.c = num_result.number;
      } break;
      case 'x': {
        // todo: add unicode conversions
        it++;
        ParsedInteger<CharT> num_result;
        parse_integer_impl(num_result, it, end, 16, representable_digits_for_base_v<CharT, 16>);
        if (num_result.significant_digits_parsed == 0) {
          result.error = ParseCharacterEscapeError::incomplete_escape;
        }
        if (num_result.would_overflow) {
          result.error = ParseCharacterEscapeError::out_of_range;
        }
        result.c = num_result.number;
      } break;
      case 'u': {
        // todo: add unicode conversions
        it++;
        ParsedInteger<CharT> num_result;
        parse_integer_impl(num_result, it, end, 16, 4);
        if (num_result.significant_digits_parsed != 4) {
          result.error = ParseCharacterEscapeError::incomplete_escape;
        }
        if (num_result.would_overflow) {
          result.error = ParseCharacterEscapeError::out_of_range;
        }
        result.c = num_result.number;
      } break;
      case 'U': {
        // todo: add unicode conversions
        it++;
        ParsedInteger<CharT> num_result;
        parse_integer_impl(num_result, it, end, 16, 8);
        if (num_result.significant_digits_parsed != 8) {
          result.error = ParseCharacterEscapeError::incomplete_escape;
        }
        if (num_result.would_overflow) {
          result.error = ParseCharacterEscapeError::out_of_range;
        }
        result.c = num_result.number;
      } break;
      default:
        it++;
        result.error = ParseCharacterEscapeError::unknown_escape;
        break;
    }

    return result;
  }

  enum class ParseNumberError {
    no_error,
    no_input,
    missing_exponent_digits,
  };

  struct ParsedNumber {
    static_assert(sizeof(IntT) >= sizeof(FloatT));
    // number = significand * (base ^ exponent)
    ParsedInteger<IntT> significand;
    // explicit exponent
    ParsedInteger<IntT> exponent;
    // implicit exponent from significand
    IntT implicit_exponent = 0;
    // how many digits of base <base> can be fully represented in an IntT
    IntT representable_digits = representable_digits_for_base_v<IntT, 10>;
    ParseNumberError error = ParseNumberError::no_error;
    std::uint8_t base = 10;
    bool has_decimal_point = false;
    bool has_integer_digits = false;
    bool has_fractional_digits = false;
    bool has_exponent = false;

    bool is_int_too_big() {
      return significand.would_overflow;
    }

    // this function is only meaningful if there were no fractional digits and no exponent
    IntT get_int() {
      return significand.number;
    }

    FloatT get_float() {
      // todo: this is probly more complicated
      // `exponent.number` has a much greater range than the exponent of any float type we would use
      // so we shouldn't have to check if it would overflow, it will hopefully resolve to INF anyway
      IntT real_exponent = exponent.number;
      // if it would overflow it should just calculate to inf anyway, no need to worry. i think
      add_avoid_overflow(&real_exponent, implicit_exponent);

      return static_cast<FloatT>(significand.number) * std::pow<FloatT>(static_cast<FloatT>(base), static_cast<FloatT>(real_exponent));
    }
  };

  template<typename IterT, typename SentinelT>
  void parse_number_integer_part(ParsedNumber & parsed_number, IterT & it, SentinelT end) {
    if (it == end) {
      return;
    }

    // parse base marker
    IterT integer_begin = it;
    if (*it == '0') {
      it++;
      if (it != end) {
        switch (*it) {
          case 'X':
          case 'x':
            it++;
            integer_begin = it;
            parsed_number.base = 16;
            parsed_number.representable_digits = representable_digits_for_base_v<IntT, 16>;
            break;
          case 'B':
          case 'b':
            it++;
            integer_begin = it;
            parsed_number.base = 2;
            parsed_number.representable_digits = representable_digits_for_base_v<IntT, 2>;
            break;
        }
      }
    }

    parse_integer_leading_zeros(it, end);
    parse_integer_impl(parsed_number.significand, it, end, parsed_number.base, parsed_number.representable_digits);
    parsed_number.implicit_exponent = parse_integer_find_end(it, end, parsed_number.base);
    parsed_number.has_integer_digits = integer_begin != it;
  }

  template<typename IterT, typename SentinelT>
  void parse_number_fractional_part(ParsedNumber & parsed_number, IterT & it, SentinelT end) {
    if (it == end) {
      return;
    }

    if (*it == '.') {
      it++;
      parsed_number.has_decimal_point = true;
      IterT fractional_begin = it;
      IntT integer_digits_parsed = parsed_number.significand.significant_digits_parsed;
      if (integer_digits_parsed == 0) {
        // haven't hit first significant digit yet, skip more leading zeros
        parsed_number.implicit_exponent -= parse_integer_leading_zeros(it, end);
      }
      parse_integer_impl(parsed_number.significand, it, end, parsed_number.base, parsed_number.representable_digits);
      parse_integer_find_end(it, end, parsed_number.base);
      parsed_number.has_fractional_digits = fractional_begin != it;
      parsed_number.implicit_exponent -= parsed_number.significand.significant_digits_parsed - integer_digits_parsed;
    }
  }

  template<typename IterT, typename SentinelT>
  void parse_number_exponent_part(ParsedNumber & parsed_number, IterT & it, SentinelT end) {
    using CharT = std::remove_cv_t<std::remove_reference_t<decltype(*std::declval<IterT>())>>;

    CharT exponent_char = parsed_number.base == 10 ? 'e' : 'p';
    if (it == end) {
      return;
    }

    // ascii case-insensitive comparison
    if ((static_cast<UIntT>(*it) | 32u) == exponent_char) {
      it++;
      parsed_number.has_exponent = true;

      IntT exponent_multiplier = 1;
      parse_integer_sign(it, end, exponent_multiplier);
      parse_integer_leading_zeros(it, end);
      parse_integer_impl(parsed_number.exponent, it, end, parsed_number.base, parsed_number.representable_digits);
      parsed_number.exponent.number *= exponent_multiplier;

      if (parsed_number.exponent.significant_digits_parsed == 0) {
        parsed_number.error = ParseNumberError::missing_exponent_digits;
        return;
      }
    }
  }

  template<typename IterT, typename SentinelT>
  ParsedNumber parse_number(IterT & it, SentinelT end) {
    ParsedNumber parsed_number;
    if (it == end) {
      parsed_number.error = ParseNumberError::no_input;
      return parsed_number;
    }

    parse_number_integer_part(parsed_number, it, end);
    parse_number_fractional_part(parsed_number, it, end);
    parse_number_exponent_part(parsed_number, it, end);
    return parsed_number;
  }

  TESL_ALWAYS_INLINE ParsedNumber parse_number(CharStrView str) {
    auto begin = str.begin();
    auto end = str.end();
    return parse_number(begin, end);
  }

  TESL_ALWAYS_INLINE ParsedNumber parse_number(WCharStrView str) {
    auto begin = str.begin();
    auto end = str.end();
    return parse_number(begin, end);
  }

  TESL_ALWAYS_INLINE ParsedNumber parse_number(Char16StrView str) {
    auto begin = str.begin();
    auto end = str.end();
    return parse_number(begin, end);
  }

  TESL_ALWAYS_INLINE ParsedNumber parse_number(Char32StrView str) {
    auto begin = str.begin();
    auto end = str.end();
    return parse_number(begin, end);
  }
}

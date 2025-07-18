#pragma once

#include "tesl_common.hpp"
#include "tesl_math.hpp"
#include "tesl_str.hpp"

namespace tesl {
  namespace detail {
    template<typename CharT>
    constexpr CharT number_digits[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};

    template<typename NumberT, typename CharT>
    IntT print_to_buffer(NumberT num, IntT base, ArrayView<CharT> buffer_out) {
      IntT buffer_size = std::numeric_limits<NumberT>::digits / std::log2(base) + 1;
      CharT buffer[buffer_size];
      CharT * out = buffer + buffer_size - 1;
      
      if (num == 0) {
        buffer_out[0] = number_digits<CharT>[0];
        return 1;
      }

      bool negative = false;
      if (num < 0) {
        negative = true;
        num = -num;
      }

      do {
        IntT digit = num % base;
        num /= base;
        *--out = number_digits<CharT>[digit];
      } while (num != 0);

      if (negative) {
        *--out = '-';
      }

      IntT write_length = std::min(buffer - out, buffer_out.size());
      for (IntT i = 0; i < write_length; ++i) {
        buffer_out[i] = out[i];
      }

      return write_length;
    }

    // idea from https://towardsdatascience.com/34-faster-integer-to-string-conversion-algorithm-c72453d25352/
    // assumes no larger than 64 bit integers
    template<typename NumberT, typename CharT, IntT Base, IntT ClumpSize = 2>
    struct LrIntFormatter {
      static_assert(Base >= 2 && Base <= 36, "invalid base");

      // gives exact number of powers of the square of the base that fit into the number type.
      // tested with signed and unsigned int32 and int64
      static constexpr IntT _powers_size = static_cast<NumberT>(std::ceil(std::numeric_limits<NumberT>::digits / std::log2(Base) - 1)) + 1;
      static constexpr IntT _digit_clumps_size = std::pow(Base, ClumpSize);
      static_assert(_digit_clumps_size < std::numeric_limits<NumberT>::max() && _digit_clumps_size < 8192, "combination of clumps size and base is too big");

      CharT _digit_clumps[_digit_clumps_size * ClumpSize];
      NumberT _powers[_powers_size];

      // does not print null terminator
      // returns the number of characters written
      IntT print_to_buffer(NumberT num, ArrayView<CharT> buffer_out) const {
        constexpr IntT buffer_size = std::numeric_limits<NumberT>::digits / std::log2(Base) + 1;
        CharT buffer[buffer_size];
        CharT * out = buffer;

        if (num == 0) {
          buffer_out[0] = number_digits<CharT>[0];
          return 1;
        }

        if (num < 0) {
          *out++ = '-';
          num = -num;
        }

        IntT initial_digits;
        IntT max_power_index = get_max_power_index(num, initial_digits);
        NumberT power_value = _powers[max_power_index];
        NumberT print_value = num / power_value;
        const CharT * digit_clump = &_digit_clumps[print_value * ClumpSize];
        for (IntT i = ClumpSize - initial_digits; i < ClumpSize; ++i) {
          *out++ = digit_clump[i];
        }
        num -= print_value * power_value;

        for (IntT i = max_power_index - ClumpSize; i >= 0; i -= ClumpSize) {
          NumberT power_value = _powers[i];
          NumberT print_value = num / power_value;
          const CharT * digit_clump = &_digit_clumps[print_value * ClumpSize];
          for (IntT i = 0; i < ClumpSize; ++i) {
            *out++ = digit_clump[i];
          }
          num -= print_value * power_value;
        }

        IntT write_length = std::min(out - buffer, buffer_out.size());
        for (IntT i = 0; i < write_length; ++i) {
          buffer_out[i] = buffer[i];
        }

        return write_length;
      }

      // doesnt work for num = 0
      IntT get_max_power_index(NumberT num, IntT & r_digits) const {
        r_digits = 1;
        for (IntT i = ClumpSize;; i += ClumpSize) {
          if (i >= _powers_size || num < _powers[i]) {
            i -= ClumpSize;
            IntT ret = i;
            for (; i < _powers_size && num >= _powers[i]; ++i) {
              r_digits++;
            }
            return ret;
          }
        }
        assert(false);
      }

      void populate_powers() {
        _powers[0] = 1;
        for (IntT i = 1; i < _powers_size; ++i) {
          _powers[i] = _powers[i - 1] * Base;
        }
      }

      void populate_digit_clumps() {
        // least significant digit is at 0
        IntT digit_values[ClumpSize] = {0};
        for (IntT i = 0; i < _digit_clumps_size; ++i) {
          for (IntT d = 0; d < ClumpSize; ++d) {
            if (digit_values[d] >= Base) {
              // reset and carry the one
              digit_values[d] = 0;
              IntT next_d = d + 1;
              assert(next_d < ClumpSize);
              digit_values[next_d]++;
            }

            _digit_clumps[i * ClumpSize + (ClumpSize - 1 - d)] = number_digits<CharT>[digit_values[d]];
          }
          digit_values[0]++;
        }
      }

      LrIntFormatter() {
        populate_powers();
        populate_digit_clumps();
      }
    };
  }

  struct FloatFormatSettings {
    IntT precision = 8;
  };

  StrViewT stringify_null();
  StrViewT stringify_bool(BoolT v);
  StrT stringify_int(IntT v, IntT base = 10);
  StrT stringify_float(FloatT v, FloatFormatSettings format_settings = {});
  // todo: generalize stringify and print
  StrT stringify_vec2(Vec2 v, FloatFormatSettings format_settings = {});
  StrT stringify_vec3(Vec3 v, FloatFormatSettings format_settings = {});
  StrT stringify_vec4(Vec4 v, FloatFormatSettings format_settings = {});
  StrT stringify_mat2(Mat2 v, FloatFormatSettings format_settings = {});
  StrT stringify_mat3(Mat3 v, FloatFormatSettings format_settings = {});
  StrT stringify_mat4(Mat4 v, FloatFormatSettings format_settings = {});

}

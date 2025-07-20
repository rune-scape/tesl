#include "tesl_common.hpp"
#include "unicode.hpp"

namespace tesl {
  /*void print(const char16_t * str) {
    using namespace unicode;
    char buffer[256];

    while (*str) {
      char * output_ptr = buffer;
      char * output_end = buffer + sizeof(buffer) - 1;
      codec_error e = codec_error::ok;
      for (; *str && !(e & codec_error::insufficient_space); ++str) {
        char32_t cp = U'\0';
        e |= utf16::decode_next(cp, str);
        e |= utf8::encode(cp, output_ptr, output_end);
      }
      *output_ptr = '\0';
    }

    print(buffer);
  }

  void print(const char32_t * str) {
    using namespace unicode;
    char buffer[256];

    while (*str) {
      char * output_ptr = buffer;
      char * output_end = buffer + sizeof(buffer) - 1;
      codec_error e = codec_error::ok;
      for (; *str && !(e & codec_error::insufficient_space); ++str) {
        e |= utf8::encode(*str, output_ptr, output_end);
      }
      *output_ptr = '\0';
    }

    print(buffer);
  }

  void print(const Char16StrView & str) {
    using namespace unicode;
    char buffer[256];

    const char16_t * input_ptr = str.data();
    const char16_t * input_end = str.end();
    while (input_ptr != input_end) {
      char * output_ptr = buffer;
      char * output_end = buffer + sizeof(buffer) - 1;
      codec_error e = codec_error::ok;
      for (; input_ptr != input_end && !(e & codec_error::insufficient_space); ++input_ptr) {
        char32_t cp = U'\0';
        e |= utf16::decode_next(cp, input_ptr, input_end);
        e |= utf8::encode(cp, output_ptr, output_end);
      }
      *output_ptr = '\0';
    }

    print(buffer);
  }

  void print(const Char32StrView & str) {
    using namespace unicode;
    char buffer[256];

    const char32_t * input_ptr = str.data();
    const char32_t * input_end = str.end();
    while (input_ptr != input_end) {
      char * output_ptr = buffer;
      char * output_end = buffer + sizeof(buffer) - 1;
      codec_error e = codec_error::ok;
      for (; input_ptr != input_end && !(e & codec_error::insufficient_space); ++input_ptr) {
        e |= utf8::encode(*input_ptr, output_ptr, output_end);
      }
      *output_ptr = '\0';
    }

    print(buffer);
  }*/

  void print_error(int line_num, const char * line_start, const char * start, const char * point, const char * const end) {
#ifndef TESL_DISABLE_PRINTF
    const char error_point_highlight_str[] = "^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>";
    const int error_point_highlight_str_size = sizeof(error_point_highlight_str) - 1;
    const char * error_highlight_str = error_point_highlight_str + 1;
    const int error_highlight_str_size = error_point_highlight_str_size - 1;
    const int error_highlight_start_str_size = error_highlight_str_size - 1;
    const char * line_end = line_start;
    do {
      while (*line_end != '\0' && *line_end != '\r' && *line_end != '\n') line_end++;
      const int line_length = line_end - line_start;
      tesl_printf("%4d | %.*s\n", line_num, line_length, line_start);
      if (start <= line_end && end > line_start) {
        const char * highlight_start = max(start, line_start);
        const char * highlight_end = min(line_end, end);
        int highlight_skip_length = highlight_start - line_start;
        tesl_printf("     | %*s", highlight_skip_length, "");
        if (line_start <= point && point <= line_end) {
          int highlight_begin_length = point - highlight_start;
          const int highlight_end_length = max<int>(1, highlight_end - point);
          for (; highlight_begin_length > 0; highlight_begin_length -= error_highlight_start_str_size)
            tesl_printf("%.*s", min(highlight_begin_length, error_highlight_start_str_size), error_highlight_str);
          tesl_printf("%.*s\n", min(highlight_end_length, error_point_highlight_str_size), error_point_highlight_str);
        } else {
          const int highlight_length = highlight_end - highlight_start;
          tesl_printf("%.*s\n", min(highlight_length, error_highlight_str_size), error_highlight_str);
        }
      }
      if (*line_end == '\0') {
        break;
      } else if (line_end[0] == '\r' && line_end[1] == '\n') {
        line_start = line_end + 2;
      } else {
        line_start = line_end + 1;
      }
      line_end = line_start;
      line_num++;
    } while (line_end < end);
#endif
  }
} // namespace tesl

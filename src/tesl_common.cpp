#include "tesl_common.hpp"

#include "tesl_bind.hpp"
#include "tesl_fmt.hpp"
#include "tesl_type.hpp"
#include <fmt/format.h>
#include <memory>

namespace tesl {
  namespace impl {
    void format_null(void * this_, void * pArgs, void * pRet) {
      struct Args {
        FmtFormatter formatter;
      };
      Args & args = *reinterpret_cast<Args *>(pArgs);
      format_to(args.formatter.out(), "null");
    }

    void format_bool(void * this_, void * pArgs, void * pRet) {
      struct Args {
        FmtFormatter formatter;
      };
      Bool & v = *reinterpret_cast<Bool *>(this_);
      Args & args = *reinterpret_cast<Args *>(pArgs);
      format_to(args.formatter.out(), "{}", v ? "true" : "false");
    }

    void format_int(void * this_, void * pArgs, void * pRet) {
      struct Args {
        FmtFormatter formatter;
      };
      IntT & v = *reinterpret_cast<IntT *>(this_);
      Args & args = *reinterpret_cast<Args *>(pArgs);
      fmt::format_int formatted_int{v};
      format_to(args.formatter.out(), "{}", v);
    }

    void format_float(void * this_, void * pArgs, void * pRet) {
      struct Args {
        FmtFormatter formatter;
      };
      FloatT & v = *reinterpret_cast<FloatT *>(this_);
      Args & args = *reinterpret_cast<Args *>(pArgs);
      format_to(args.formatter.out(), "{}", v);
    }

    void format_type(void * this_, void * pArgs, void * pRet) {
      struct Args {
        FmtFormatter formatter;
      };
      Type & v = *reinterpret_cast<Type *>(this_);
      Args & args = *reinterpret_cast<Args *>(pArgs);
      format_to(args.formatter.out(), "{}", v);
    }

    void convert_int_to_float(void * this_, void * pArgs, void * pRet) {
      IntT & v = *reinterpret_cast<IntT *>(this_);
      FloatT & ret = *reinterpret_cast<FloatT *>(pRet);
      std::construct_at(&ret, v);
    }
  }

  template<>
  void bind_type_info<Null>(TypeInfo & type) {
    TESL_BIND_BUITIN_BARE_MEMBER_FUNCTION(type, impl::format_null, "format(fmt:Formatter)");
    // todo: finish
  }

  template<>
  void bind_type_info<Bool>(TypeInfo & type) {
    TESL_BIND_BUITIN_BARE_MEMBER_FUNCTION(type, impl::format_bool, "format(fmt:Formatter)");
    // todo: finish
  }

  template<>
  void bind_type_info<IntT>(TypeInfo & type) {
    TESL_BIND_BUITIN_BARE_MEMBER_FUNCTION(type, impl::format_int, "format(fmt:Formatter)");
    TESL_BIND_BUITIN_BARE_MEMBER_FUNCTION(type, impl::convert_int_to_float, "->");
    // todo: finish
  }

  template<>
  void bind_type_info<FloatT>(TypeInfo & type) {
    TESL_BIND_BUITIN_BARE_MEMBER_FUNCTION(type, impl::format_float, "format(fmt:Formatter)");
    // todo: finish
  }

  template<>
  void bind_type_info<Type>(TypeInfo & type) {
    TESL_BIND_BUITIN_BARE_MEMBER_FUNCTION(type, impl::format_type, "format(fmt:Formatter)");
    // todo: finish
  }

  void print(std::FILE * f, StrView str) {
    fmt::detail::print(f, str);
  }

  void _log_print_prefix_impl(FILE * f, tcolor::fg col, const char * prefix) {
    tcolor::setColor(f, col);
    tcolor::setStyle(f, tcolor::style::bold);
    std::fputs(prefix, f);
    std::fputs(": ", f);
    tcolor::setStyle(f, tcolor::style::reset);
  }

  void _log_print_msg_impl(FILE * f, tcolor::fg col, const char * msg) {
    tcolor::setColor(f, col);
    std::fputs(msg, f);
    tcolor::setColor(f, tcolor::fg::reset);
  }

  void _log_print_source_location_impl(FILE * f, const std::source_location & loc) {
    tcolor::setColor(f, tcolor::fg::gray);
    fmt::print(f, " (at {}:{}:{} in {})\n", loc.file_name(), loc.line(), loc.column(), loc.function_name());
    tcolor::setColor(f, tcolor::fg::reset);
  }

  void print_error_sourcev(FILE * file, int line_num, const char * line_start, const char * start, const char * point, const char * const end) {
    // todo: modernize
    TESL_ASSERT(start <= point && point <= end);
    const char * line_end = line_start;
    do {
      while (*line_end != '\0' && *line_end != '\r' && *line_end != '\n') line_end++;
      const CharStrView::size_type line_length = line_end - line_start;
      fmt::println(file, "{: 4} | {}", line_num, CharStrView{line_start, line_length});
      if (start <= line_end && end > line_start) {
        const char * highlight_start = max(start, line_start);
        const char * highlight_end = min(line_end, end);
        int highlight_skip_length = highlight_start - line_start;
        fmt::print(file, "     | {: <{}}", "", highlight_skip_length);
        if (line_start <= point && point <= line_end) {
          int highlight_begin_length = point - highlight_start;
          const int highlight_end_length = max<int>(1, highlight_end - point);
          fmt::print(file, "{:~<{}}", "", highlight_begin_length);
          fmt::println(file, "^{:~<{}}", "", highlight_end_length - 1);
        } else {
          const int highlight_length = highlight_end - highlight_start;
          fmt::println(file, "{:~<{}}", "", highlight_length);
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
  }
} // namespace tesl

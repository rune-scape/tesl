#pragma once

#include "tesl_common.hpp"
#include <fmt/format.h>
//#include <fmt/color.h>

namespace tesl {
  struct FmtFormatter {
    fmt::parse_context<CommonCharT> * parse_ctx = nullptr;
    fmt::buffered_context<CommonCharT> * format_ctx = nullptr;

    auto out() { return format_ctx->out(); }
    auto out() const { return format_ctx->out(); }
  };

  using fmt::vformat_to;
  using fmt::format_to;
  using fmt::vformat;
  using fmt::format;
  using fmt::vprint;
  using fmt::print;
  using fmt::vprintln;
  using fmt::println;

  /*template <typename OutputIt, typename ... T, FMT_ENABLE_IF(fmt::detail::is_output_iterator<std::remove_cvref_t<OutputIt>, char>::value)>
  inline auto format_to(OutputIt && out, fmt::format_string<T...> fmt, T && ... args) -> std::remove_cvref_t<OutputIt> {
    return vformat_to(out, fmt.str, fmt::vargs<T...>{{args...}});
  }*/

  template<typename T, typename ... Args>
  struct _log_print_msg_include_error_disabler<T, void, Args...> {};

  template <typename ... T>
  inline void _log_print_msg_impl(std::FILE * f, tcolor::fg col, fmt::format_string<T...> fstr, T && ... args) {
    tcolor::setColor(f, col);
    print(f, fstr, FWD(args)...);
    tcolor::setColor(f, tcolor::fg::reset);
  }

  template<>
  void bind_type_info<FmtFormatter>(TypeInfo & type);
}

template<>
class fmt::formatter<tesl::Null, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(tesl::Null, Context & ctx) const {
    return format_to(ctx.out(), "null");
  }
};

#pragma once

#include "tesl_common.hpp"
#include <fmt/format.h>

#define TESL_LOG_PRINT_IMPL_(col, prefix, ...) \
  tcolor::setColor(stderr, tcolor::fg::col); \
  tcolor::setStyle(stderr, tcolor::style::bold); \
  std::fputs(prefix ": ", stderr); \
  tcolor::setStyle(stderr, tcolor::style::reset); \
  tcolor::setColor(stderr, tcolor::fg::col); \
  fmt::print(stderr, __VA_ARGS__); \
  tcolor::setColor(stderr, tcolor::fg::reset);

#define TESL_LOG_SOURCE_LOCATION_IMPL_() \
  tcolor::setColor(stderr, tcolor::fg::gray); \
  fmt::print(stderr, " (at {}:{} in {})\n", __FILE__, __LINE__, __func__); \
  tcolor::setColor(stderr, tcolor::fg::reset);

#define TESL_ERROR_PRINT_BASE(prefix, ...) \
  TESL_LOG_PRINT_IMPL_(red, prefix, __VA_ARGS__) \
  TESL_LOG_SOURCE_LOCATION_IMPL_()

#define TESL_WARN_PRINT_BASE(prefix, ...) \
  TESL_LOG_PRINT_IMPL_(yellow, prefix, __VA_ARGS__) \
  TESL_LOG_SOURCE_LOCATION_IMPL_()

#define TESL_INFO_PRINT_BASE(prefix, ...) \
  TESL_LOG_PRINT_IMPL_(white, prefix, __VA_ARGS__)

#define TESL_FAIL_BASE(prefix, action, ...) \
  do { \
    TESL_ERROR_PRINT_BASE(prefix, __VA_ARGS__); \
    action; \
  } while (false)

#define TESL_FAIL_COND_BASE(prefix, cond, action, ...) \
  if (cond) [[unlikely]] { \
    TESL_FAIL_BASE(prefix, action, __VA_ARGS__); \
  } else do {} while (false)

#define TESL_FAIL_MSG(action, ...) \
  TESL_FAIL_BASE("error", action, __VA_ARGS__)

#define TESL_FAIL_COND_MSG(cond, action, ...) \
  TESL_FAIL_COND_BASE("error", cond, action, __VA_ARGS__)

#define TESL_FAIL_COND(cond, action) \
  TESL_FAIL_COND_MSG(cond, action, "'" #cond "' is true")

#ifndef NDEBUG
#define TESL_ASSERT_MSG(cond, ...) \
  TESL_FAIL_COND_BASE("assertion failed", !(cond), abort(), __VA_ARGS__)
#define TESL_ASSERT(cond) \
  TESL_FAIL_COND_BASE("assertion failed", !(cond), abort(), "'" #cond "' is false")
#else
#define TESL_ASSERT_MSG(cond, msg) do {} while (false)
#define TESL_ASSERT(cond) do {} while (false)
#endif

#ifndef NDEBUG
#define TESL_UNREACHABLE \
  do { \
    TESL_FAIL_COND_BASE("unreachable code", true, abort(), "at {}:{} in {}", __FILE__, __LINE__, __func__); \
    __builtin_unreachable(); \
  } while (false)
#else
#define TESL_UNREACHABLE __builtin_unreachable()
#endif

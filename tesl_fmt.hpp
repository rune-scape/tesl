#pragma once

#include "tesl_common.hpp"
#include "tesl_str.hpp"

// fmt from https://github.com/fmtlib

/* - part of fmt/base.h - */
// Formatting library for C++ - the base API for char/UTF-8
//
// Copyright (c) 2012 - present, Victor Zverovich
// All rights reserved.
//
// For the license information refer to format.h.

FMT_BEGIN_NAMESPACE

// Implementations of enable_if_t and other metafunctions for older systems.
template <bool B, typename T = void>
using enable_if_t = typename std::enable_if<B, T>::type;
template <bool B, typename T, typename F>
using conditional_t = typename std::conditional<B, T, F>::type;
template <bool B> using bool_constant = std::integral_constant<bool, B>;
template <typename T>
using remove_reference_t = typename std::remove_reference<T>::type;
template <typename T>
using remove_const_t = typename std::remove_const<T>::type;
template <typename T>
using remove_cvref_t = typename std::remove_cv<remove_reference_t<T>>::type;
template <typename T>
using make_unsigned_t = typename std::make_unsigned<T>::type;
template <typename T>
using underlying_t = typename std::underlying_type<T>::type;
template <typename T> using decay_t = typename std::decay<T>::type;
using nullptr_t = decltype(nullptr);

#if (FMT_GCC_VERSION && FMT_GCC_VERSION < 500) || FMT_MSC_VERSION
// A workaround for gcc 4.9 & MSVC v141 to make void_t work in a SFINAE context.
template <typename...> struct void_t_impl {
  using type = void;
};
template <typename... T> using void_t = typename void_t_impl<T...>::type;
#else
template <typename...> using void_t = void;
#endif

struct monostate {
  constexpr monostate() {}
};

// An enable_if helper to be used in template parameters which results in much
// shorter symbols: https://godbolt.org/z/sWw4vP. Extra parentheses are needed
// to workaround a bug in MSVC 2019 (see #1140 and #1186).
#ifdef FMT_DOC
#  define FMT_ENABLE_IF(...)
#else
#  define FMT_ENABLE_IF(...) fmt::enable_if_t<(__VA_ARGS__), int> = 0
#endif

template <typename T> constexpr auto min_of(T a, T b) -> T {
  return a < b ? a : b;
}
template <typename T> constexpr auto max_of(T a, T b) -> T {
  return a > b ? a : b;
}

namespace detail {
// Suppresses "unused variable" warnings with the method described in
// https://herbsutter.com/2009/10/18/mailbag-shutting-up-compiler-warnings/.
// (void)var does not work on many Intel compilers.
template <typename... T> FMT_CONSTEXPR void ignore_unused(const T&...) {}

constexpr auto is_constant_evaluated(bool default_value = false) noexcept
    -> bool {
// Workaround for incompatibility between clang 14 and libstdc++ consteval-based
// std::is_constant_evaluated: https://github.com/fmtlib/fmt/issues/3247.
#if FMT_CPLUSPLUS >= 202002L && FMT_GLIBCXX_RELEASE >= 12 && \
    (FMT_CLANG_VERSION >= 1400 && FMT_CLANG_VERSION < 1500)
  ignore_unused(default_value);
  return __builtin_is_constant_evaluated();
#elif defined(__cpp_lib_is_constant_evaluated)
  ignore_unused(default_value);
  return std::is_constant_evaluated();
#else
  return default_value;
#endif
}

// Suppresses "conditional expression is constant" warnings.
template <typename T> FMT_ALWAYS_INLINE constexpr auto const_check(T val) -> T {
  return val;
}

FMT_NORETURN FMT_API void assert_fail(const char* file, int line,
                                      const char* message, int error_code = 0);

#if defined(FMT_ASSERT)
// Use the provided definition.
#elif defined(NDEBUG)
// FMT_ASSERT is not empty to avoid -Wempty-body.
#  define FMT_ASSERT(condition, message) \
    fmt::detail::ignore_unused((condition), (message))
#else
#  define FMT_ASSERT(condition, message)                                    \
    ((condition) /* void() fails with -Winvalid-constexpr on clang 4.0.1 */ \
         ? (void)0                                                          \
         : fmt::detail::assert_fail(__FILE__, __LINE__, (message)))
#endif

#ifdef FMT_USE_INT128
// Use the provided definition.
#elif defined(__SIZEOF_INT128__) && !defined(__NVCC__) && \
    !(FMT_CLANG_VERSION && FMT_MSC_VERSION)
#  define FMT_USE_INT128 1
using int128_opt = __int128_t;  // An optional native 128-bit integer.
using uint128_opt = __uint128_t;
inline auto map(int128_opt x) -> int128_opt { return x; }
inline auto map(uint128_opt x) -> uint128_opt { return x; }
#else
#  define FMT_USE_INT128 0
#endif
#if !FMT_USE_INT128
enum class int128_opt {};
enum class uint128_opt {};
// Reduce template instantiations.
inline auto map(int128_opt) -> monostate { return {}; }
inline auto map(uint128_opt) -> monostate { return {}; }
#endif

#ifndef FMT_USE_BITINT
#  define FMT_USE_BITINT (FMT_CLANG_VERSION >= 1500)
#endif

#if FMT_USE_BITINT
FMT_PRAGMA_CLANG(diagnostic ignored "-Wbit-int-extension")
template <int N> using bitint = _BitInt(N);
template <int N> using ubitint = unsigned _BitInt(N);
#else
template <int N> struct bitint {};
template <int N> struct ubitint {};
#endif  // FMT_USE_BITINT

// Casts a nonnegative integer to unsigned.
template <typename Int>
FMT_CONSTEXPR auto to_unsigned(Int value) -> make_unsigned_t<Int> {
  FMT_ASSERT(std::is_unsigned<Int>::value || value >= 0, "negative value");
  return static_cast<make_unsigned_t<Int>>(value);
}

template <typename Char>
using unsigned_char = conditional_t<sizeof(Char) == 1, unsigned char, unsigned>;

// A heuristic to detect std::string and std::[experimental::]string_view.
// It is mainly used to avoid dependency on <[experimental/]string_view>.
template <typename T, typename Enable = void>
struct is_std_string_like : std::false_type {};
template <typename T>
struct is_std_string_like<T, void_t<decltype(std::declval<T>().find_first_of(
                                 typename T::value_type(), 0))>>
    : std::is_convertible<decltype(std::declval<T>().data()),
                          const typename T::value_type*> {};

// Check if the literal encoding is UTF-8.
enum { is_utf8_enabled = "\u00A7"[1] == '\xA7' };
enum { use_utf8 = !FMT_WIN32 || is_utf8_enabled };

#ifndef FMT_UNICODE
#  define FMT_UNICODE 1
#endif

static_assert(!FMT_UNICODE || use_utf8,
              "Unicode support requires compiling with /utf-8");

template <typename T> constexpr const char* narrow(const T*) { return nullptr; }
constexpr FMT_ALWAYS_INLINE const char* narrow(const char* s) { return s; }

template <typename Char>
FMT_CONSTEXPR auto compare(const Char* s1, const Char* s2, size_t n) -> int {
  if (!is_constant_evaluated() && sizeof(Char) == 1) return memcmp(s1, s2, n);
  for (; n != 0; ++s1, ++s2, --n) {
    if (*s1 < *s2) return -1;
    if (*s1 > *s2) return 1;
  }
  return 0;
}

namespace adl {
using namespace std;

template <typename Container>
auto invoke_back_inserter()
    -> decltype(back_inserter(std::declval<Container&>()));
}  // namespace adl

template <typename It, typename Enable = std::true_type>
struct is_back_insert_iterator : std::false_type {};

template <typename It>
struct is_back_insert_iterator<
    It, bool_constant<std::is_same<
            decltype(adl::invoke_back_inserter<typename It::container_type>()),
            It>::value>> : std::true_type {};

// Extracts a reference to the container from *insert_iterator.
template <typename OutputIt>
inline FMT_CONSTEXPR20 auto get_container(OutputIt it) ->
    typename OutputIt::container_type& {
  struct accessor : OutputIt {
    FMT_CONSTEXPR20 accessor(OutputIt base) : OutputIt(base) {}
    using OutputIt::container;
  };
  return *accessor(it).container;
}
}  // namespace detail

// Parsing-related public API and forward declarations.
FMT_BEGIN_EXPORT

/**
 * An implementation of `std::basic_string_view` for pre-C++17. It provides a
 * subset of the API. `fmt::basic_string_view` is used for format strings even
 * if `std::basic_string_view` is available to prevent issues when a library is
 * compiled with a different `-std` option than the client code (which is not
 * recommended).
 */
template <typename Char> class basic_string_view {
 private:
  const Char* data_;
  size_t size_;

 public:
  using value_type = Char;
  using iterator = const Char*;

  constexpr basic_string_view() noexcept : data_(nullptr), size_(0) {}

  /// Constructs a string view object from a C string and a size.
  constexpr basic_string_view(const Char* s, size_t count) noexcept
      : data_(s), size_(count) {}

  constexpr basic_string_view(nullptr_t) = delete;

  /// Constructs a string view object from a C string.
#if FMT_GCC_VERSION
  FMT_ALWAYS_INLINE
#endif
  FMT_CONSTEXPR20 basic_string_view(const Char* s) : data_(s) {
#if FMT_HAS_BUILTIN(__builtin_strlen) || FMT_GCC_VERSION || FMT_CLANG_VERSION
    if (std::is_same<Char, char>::value && !detail::is_constant_evaluated()) {
      size_ = __builtin_strlen(detail::narrow(s));  // strlen is not constexpr.
      return;
    }
#endif
    size_t len = 0;
    while (*s++) ++len;
    size_ = len;
  }

  /// Constructs a string view from a `fmt::basic_string` or a
  /// `std::basic_string_view` object.
  template <typename S,
            FMT_ENABLE_IF(detail::is_std_string_like<S>::value&& std::is_same<
                          typename S::value_type, Char>::value)>
  FMT_CONSTEXPR basic_string_view(const S& s) noexcept
      : data_(s.data()), size_(s.size()) {}

  /// Returns a pointer to the string data.
  constexpr auto data() const noexcept -> const Char* { return data_; }

  /// Returns the string size.
  constexpr auto size() const noexcept -> size_t { return size_; }

  constexpr auto begin() const noexcept -> iterator { return data_; }
  constexpr auto end() const noexcept -> iterator { return data_ + size_; }

  constexpr auto operator[](size_t pos) const noexcept -> const Char& {
    return data_[pos];
  }

  FMT_CONSTEXPR void remove_prefix(size_t n) noexcept {
    data_ += n;
    size_ -= n;
  }

  FMT_CONSTEXPR auto starts_with(basic_string_view<Char> sv) const noexcept
      -> bool {
    return size_ >= sv.size_ && detail::compare(data_, sv.data_, sv.size_) == 0;
  }
  FMT_CONSTEXPR auto starts_with(Char c) const noexcept -> bool {
    return size_ >= 1 && *data_ == c;
  }
  FMT_CONSTEXPR auto starts_with(const Char* s) const -> bool {
    return starts_with(basic_string_view<Char>(s));
  }

  FMT_CONSTEXPR auto compare(basic_string_view other) const -> int {
    int result =
        detail::compare(data_, other.data_, min_of(size_, other.size_));
    if (result != 0) return result;
    return size_ == other.size_ ? 0 : (size_ < other.size_ ? -1 : 1);
  }

  FMT_CONSTEXPR friend auto operator==(basic_string_view lhs,
                                       basic_string_view rhs) -> bool {
    return lhs.compare(rhs) == 0;
  }
  friend auto operator!=(basic_string_view lhs, basic_string_view rhs) -> bool {
    return lhs.compare(rhs) != 0;
  }
  friend auto operator<(basic_string_view lhs, basic_string_view rhs) -> bool {
    return lhs.compare(rhs) < 0;
  }
  friend auto operator<=(basic_string_view lhs, basic_string_view rhs) -> bool {
    return lhs.compare(rhs) <= 0;
  }
  friend auto operator>(basic_string_view lhs, basic_string_view rhs) -> bool {
    return lhs.compare(rhs) > 0;
  }
  friend auto operator>=(basic_string_view lhs, basic_string_view rhs) -> bool {
    return lhs.compare(rhs) >= 0;
  }
};

using string_view = basic_string_view<char>;

// DEPRECATED! Will be merged with is_char and moved to detail.
template <typename T> struct is_xchar : std::false_type {};
template <> struct is_xchar<wchar_t> : std::true_type {};
template <> struct is_xchar<char16_t> : std::true_type {};
template <> struct is_xchar<char32_t> : std::true_type {};
#ifdef __cpp_char8_t
template <> struct is_xchar<char8_t> : std::true_type {};
#endif

// Specifies if `T` is a character (code unit) type.
template <typename T> struct is_char : is_xchar<T> {};
template <> struct is_char<char> : std::true_type {};

template <typename T> class basic_appender;
using appender = basic_appender<char>;

// Checks whether T is a container with contiguous storage.
template <typename T> struct is_contiguous : std::false_type {};

class context;
template <typename OutputIt, typename Char> class generic_context;
template <typename Char> class parse_context;

// Longer aliases for C++20 compatibility.
template <typename Char> using basic_format_parse_context = parse_context<Char>;
using format_parse_context = parse_context<char>;
template <typename OutputIt, typename Char>
using basic_format_context =
    conditional_t<std::is_same<OutputIt, appender>::value, context,
                  generic_context<OutputIt, Char>>;
using format_context = context;

template <typename Char>
using buffered_context =
    conditional_t<std::is_same<Char, char>::value, context,
                  generic_context<basic_appender<Char>, Char>>;

template <typename Context> class basic_format_arg;
template <typename Context> class basic_format_args;

// A separate type would result in shorter symbols but break ABI compatibility
// between clang and gcc on ARM (#1919).
using format_args = basic_format_args<context>;

/// Reports a format error at compile time or, via a `format_error` exception,
/// at runtime.
// This function is intentionally not constexpr to give a compile-time error.
FMT_NORETURN FMT_API void report_error(const char* message);

enum class presentation_type : unsigned char {
  // Common specifiers:
  none = 0,
  debug = 1,   // '?'
  string = 2,  // 's' (string, bool)

  // Integral, bool and character specifiers:
  dec = 3,  // 'd'
  hex,      // 'x' or 'X'
  oct,      // 'o'
  bin,      // 'b' or 'B'
  chr,      // 'c'

  // String and pointer specifiers:
  pointer = 3,  // 'p'

  // Floating-point specifiers:
  exp = 1,  // 'e' or 'E' (1 since there is no FP debug presentation)
  fixed,    // 'f' or 'F'
  general,  // 'g' or 'G'
  hexfloat  // 'a' or 'A'
};

enum class align { none, left, right, center, numeric };
enum class sign { none, minus, plus, space };
enum class arg_id_kind { none, index, name };

// Basic format specifiers for built-in and string types.
class basic_specs {
 private:
  // Data is arranged as follows:
  //
  //  0                   1                   2                   3
  //  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
  // +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  // |type |align| w | p | s |u|#|L|  f  |          unused           |
  // +-----+-----+---+---+---+-+-+-+-----+---------------------------+
  //
  //   w - dynamic width info
  //   p - dynamic precision info
  //   s - sign
  //   u - uppercase (e.g. 'X' for 'x')
  //   # - alternate form ('#')
  //   L - localized
  //   f - fill size
  //
  // Bitfields are not used because of compiler bugs such as gcc bug 61414.
  enum : unsigned {
    type_mask = 0x00007,
    align_mask = 0x00038,
    width_mask = 0x000C0,
    precision_mask = 0x00300,
    sign_mask = 0x00C00,
    uppercase_mask = 0x01000,
    alternate_mask = 0x02000,
    localized_mask = 0x04000,
    fill_size_mask = 0x38000,

    align_shift = 3,
    width_shift = 6,
    precision_shift = 8,
    sign_shift = 10,
    fill_size_shift = 15,

    max_fill_size = 4
  };

  unsigned data_ = 1 << fill_size_shift;
  static_assert(sizeof(basic_specs::data_) * CHAR_BIT >= 18, "");

  // Character (code unit) type is erased to prevent template bloat.
  char fill_data_[max_fill_size] = {' '};

  FMT_CONSTEXPR void set_fill_size(size_t size) {
    data_ = (data_ & ~fill_size_mask) |
            (static_cast<unsigned>(size) << fill_size_shift);
  }

 public:
  constexpr auto type() const -> presentation_type {
    return static_cast<presentation_type>(data_ & type_mask);
  }
  FMT_CONSTEXPR void set_type(presentation_type t) {
    data_ = (data_ & ~type_mask) | static_cast<unsigned>(t);
  }

  constexpr auto align() const -> align {
    return static_cast<fmt::align>((data_ & align_mask) >> align_shift);
  }
  FMT_CONSTEXPR void set_align(fmt::align a) {
    data_ = (data_ & ~align_mask) | (static_cast<unsigned>(a) << align_shift);
  }

  constexpr auto dynamic_width() const -> arg_id_kind {
    return static_cast<arg_id_kind>((data_ & width_mask) >> width_shift);
  }
  FMT_CONSTEXPR void set_dynamic_width(arg_id_kind w) {
    data_ = (data_ & ~width_mask) | (static_cast<unsigned>(w) << width_shift);
  }

  FMT_CONSTEXPR auto dynamic_precision() const -> arg_id_kind {
    return static_cast<arg_id_kind>((data_ & precision_mask) >>
                                    precision_shift);
  }
  FMT_CONSTEXPR void set_dynamic_precision(arg_id_kind p) {
    data_ = (data_ & ~precision_mask) |
            (static_cast<unsigned>(p) << precision_shift);
  }

  constexpr bool dynamic() const {
    return (data_ & (width_mask | precision_mask)) != 0;
  }

  constexpr auto sign() const -> sign {
    return static_cast<fmt::sign>((data_ & sign_mask) >> sign_shift);
  }
  FMT_CONSTEXPR void set_sign(fmt::sign s) {
    data_ = (data_ & ~sign_mask) | (static_cast<unsigned>(s) << sign_shift);
  }

  constexpr auto upper() const -> bool { return (data_ & uppercase_mask) != 0; }
  FMT_CONSTEXPR void set_upper() { data_ |= uppercase_mask; }

  constexpr auto alt() const -> bool { return (data_ & alternate_mask) != 0; }
  FMT_CONSTEXPR void set_alt() { data_ |= alternate_mask; }
  FMT_CONSTEXPR void clear_alt() { data_ &= ~alternate_mask; }

  constexpr auto localized() const -> bool {
    return (data_ & localized_mask) != 0;
  }
  FMT_CONSTEXPR void set_localized() { data_ |= localized_mask; }

  constexpr auto fill_size() const -> size_t {
    return (data_ & fill_size_mask) >> fill_size_shift;
  }

  template <typename Char, FMT_ENABLE_IF(std::is_same<Char, char>::value)>
  constexpr auto fill() const -> const Char* {
    return fill_data_;
  }
  template <typename Char, FMT_ENABLE_IF(!std::is_same<Char, char>::value)>
  constexpr auto fill() const -> const Char* {
    return nullptr;
  }

  template <typename Char> constexpr auto fill_unit() const -> Char {
    using uchar = unsigned char;
    return static_cast<Char>(static_cast<uchar>(fill_data_[0]) |
                             (static_cast<uchar>(fill_data_[1]) << 8) |
                             (static_cast<uchar>(fill_data_[2]) << 16));
  }

  FMT_CONSTEXPR void set_fill(char c) {
    fill_data_[0] = c;
    set_fill_size(1);
  }

  template <typename Char>
  FMT_CONSTEXPR void set_fill(basic_string_view<Char> s) {
    auto size = s.size();
    set_fill_size(size);
    if (size == 1) {
      unsigned uchar = static_cast<detail::unsigned_char<Char>>(s[0]);
      fill_data_[0] = static_cast<char>(uchar);
      fill_data_[1] = static_cast<char>(uchar >> 8);
      fill_data_[2] = static_cast<char>(uchar >> 16);
      return;
    }
    FMT_ASSERT(size <= max_fill_size, "invalid fill");
    for (size_t i = 0; i < size; ++i)
      fill_data_[i & 3] = static_cast<char>(s[i]);
  }

  FMT_CONSTEXPR void copy_fill_from(const basic_specs& specs) {
    set_fill_size(specs.fill_size());
    for (size_t i = 0; i < max_fill_size; ++i)
      fill_data_[i] = specs.fill_data_[i];
  }
};

// Format specifiers for built-in and string types.
struct format_specs : basic_specs {
  int width;
  int precision;

  constexpr format_specs() : width(0), precision(-1) {}
};

/**
 * Parsing context consisting of a format string range being parsed and an
 * argument counter for automatic indexing.
 */
template <typename Char = char> class parse_context {
 private:
  basic_string_view<Char> fmt_;
  int next_arg_id_;

  enum { use_constexpr_cast = !FMT_GCC_VERSION || FMT_GCC_VERSION >= 1200 };

  FMT_CONSTEXPR void do_check_arg_id(int arg_id);

 public:
  using char_type = Char;
  using iterator = const Char*;

  constexpr explicit parse_context(basic_string_view<Char> fmt,
                                   int next_arg_id = 0)
      : fmt_(fmt), next_arg_id_(next_arg_id) {}

  /// Returns an iterator to the beginning of the format string range being
  /// parsed.
  constexpr auto begin() const noexcept -> iterator { return fmt_.begin(); }

  /// Returns an iterator past the end of the format string range being parsed.
  constexpr auto end() const noexcept -> iterator { return fmt_.end(); }

  /// Advances the begin iterator to `it`.
  FMT_CONSTEXPR void advance_to(iterator it) {
    fmt_.remove_prefix(detail::to_unsigned(it - begin()));
  }

  /// Reports an error if using the manual argument indexing; otherwise returns
  /// the next argument index and switches to the automatic indexing.
  FMT_CONSTEXPR auto next_arg_id() -> int {
    if (next_arg_id_ < 0) {
      report_error("cannot switch from manual to automatic argument indexing");
      return 0;
    }
    int id = next_arg_id_++;
    do_check_arg_id(id);
    return id;
  }

  /// Reports an error if using the automatic argument indexing; otherwise
  /// switches to the manual indexing.
  FMT_CONSTEXPR void check_arg_id(int id) {
    if (next_arg_id_ > 0) {
      report_error("cannot switch from automatic to manual argument indexing");
      return;
    }
    next_arg_id_ = -1;
    do_check_arg_id(id);
  }
  FMT_CONSTEXPR void check_arg_id(basic_string_view<Char>) {
    next_arg_id_ = -1;
  }
  FMT_CONSTEXPR void check_dynamic_spec(int arg_id);
};

FMT_END_EXPORT

namespace detail {

// Constructs fmt::basic_string_view<Char> from types implicitly convertible
// to it, deducing Char. Explicitly convertible types such as the ones returned
// from FMT_STRING are intentionally excluded.
template <typename Char, FMT_ENABLE_IF(is_char<Char>::value)>
constexpr auto to_string_view(const Char* s) -> basic_string_view<Char> {
  return s;
}
template <typename T, FMT_ENABLE_IF(is_std_string_like<T>::value)>
constexpr auto to_string_view(const T& s)
    -> basic_string_view<typename T::value_type> {
  return s;
}
template <typename Char>
constexpr auto to_string_view(basic_string_view<Char> s)
    -> basic_string_view<Char> {
  return s;
}

template <typename T, typename Enable = void>
struct has_to_string_view : std::false_type {};
// detail:: is intentional since to_string_view is not an extension point.
template <typename T>
struct has_to_string_view<
    T, void_t<decltype(detail::to_string_view(std::declval<T>()))>>
    : std::true_type {};

/// String's character (code unit) type. detail:: is intentional to prevent ADL.
template <typename S,
          typename V = decltype(detail::to_string_view(std::declval<S>()))>
using char_t = typename V::value_type;

enum class type {
  none_type,
  // Integer types should go first,
  int_type,
  uint_type,
  long_long_type,
  ulong_long_type,
  int128_type,
  uint128_type,
  bool_type,
  char_type,
  last_integer_type = char_type,
  // followed by floating-point types.
  float_type,
  double_type,
  long_double_type,
  last_numeric_type = long_double_type,
  cstring_type,
  string_type,
  pointer_type,
  custom_type
};

// Maps core type T to the corresponding type enum constant.
template <typename T, typename Char>
struct type_constant : std::integral_constant<type, type::custom_type> {};

#define FMT_TYPE_CONSTANT(Type, constant) \
  template <typename Char>                \
  struct type_constant<Type, Char>        \
      : std::integral_constant<type, type::constant> {}

FMT_TYPE_CONSTANT(int, int_type);
FMT_TYPE_CONSTANT(unsigned, uint_type);
FMT_TYPE_CONSTANT(long long, long_long_type);
FMT_TYPE_CONSTANT(unsigned long long, ulong_long_type);
FMT_TYPE_CONSTANT(int128_opt, int128_type);
FMT_TYPE_CONSTANT(uint128_opt, uint128_type);
FMT_TYPE_CONSTANT(bool, bool_type);
FMT_TYPE_CONSTANT(Char, char_type);
FMT_TYPE_CONSTANT(float, float_type);
FMT_TYPE_CONSTANT(double, double_type);
FMT_TYPE_CONSTANT(long double, long_double_type);
FMT_TYPE_CONSTANT(const Char*, cstring_type);
FMT_TYPE_CONSTANT(basic_string_view<Char>, string_type);
FMT_TYPE_CONSTANT(const void*, pointer_type);

constexpr auto is_integral_type(type t) -> bool {
  return t > type::none_type && t <= type::last_integer_type;
}
constexpr auto is_arithmetic_type(type t) -> bool {
  return t > type::none_type && t <= type::last_numeric_type;
}

constexpr auto set(type rhs) -> int { return 1 << static_cast<int>(rhs); }
constexpr auto in(type t, int set) -> bool {
  return ((set >> static_cast<int>(t)) & 1) != 0;
}

// Bitsets of types.
enum {
  sint_set =
      set(type::int_type) | set(type::long_long_type) | set(type::int128_type),
  uint_set = set(type::uint_type) | set(type::ulong_long_type) |
             set(type::uint128_type),
  bool_set = set(type::bool_type),
  char_set = set(type::char_type),
  float_set = set(type::float_type) | set(type::double_type) |
              set(type::long_double_type),
  string_set = set(type::string_type),
  cstring_set = set(type::cstring_type),
  pointer_set = set(type::pointer_type)
};

struct view {};

template <typename T, typename Enable = std::true_type>
struct is_view : std::false_type {};
template <typename T>
struct is_view<T, bool_constant<sizeof(T) != 0>> : std::is_base_of<view, T> {};

template <typename Char, typename T> struct named_arg;
template <typename T> struct is_named_arg : std::false_type {};
template <typename T> struct is_static_named_arg : std::false_type {};

template <typename Char, typename T>
struct is_named_arg<named_arg<Char, T>> : std::true_type {};

template <typename Char, typename T> struct named_arg : view {
  const Char* name;
  const T& value;

  named_arg(const Char* n, const T& v) : name(n), value(v) {}
  static_assert(!is_named_arg<T>::value, "nested named arguments");
};

template <bool B = false> constexpr auto count() -> int { return B ? 1 : 0; }
template <bool B1, bool B2, bool... Tail> constexpr auto count() -> int {
  return (B1 ? 1 : 0) + count<B2, Tail...>();
}

template <typename... Args> constexpr auto count_named_args() -> int {
  return count<is_named_arg<Args>::value...>();
}
template <typename... Args> constexpr auto count_static_named_args() -> int {
  return count<is_static_named_arg<Args>::value...>();
}

template <typename Char> struct named_arg_info {
  const Char* name;
  int id;
};

// named_args is non-const to suppress a bogus -Wmaybe-uninitialized in gcc 13.
template <typename Char>
FMT_CONSTEXPR void check_for_duplicate(named_arg_info<Char>* named_args,
                                       int named_arg_index,
                                       basic_string_view<Char> arg_name) {
  for (int i = 0; i < named_arg_index; ++i) {
    if (named_args[i].name == arg_name) report_error("duplicate named arg");
  }
}

template <typename Char, typename T, FMT_ENABLE_IF(!is_named_arg<T>::value)>
void init_named_arg(named_arg_info<Char>*, int& arg_index, int&, const T&) {
  ++arg_index;
}
template <typename Char, typename T, FMT_ENABLE_IF(is_named_arg<T>::value)>
void init_named_arg(named_arg_info<Char>* named_args, int& arg_index,
                    int& named_arg_index, const T& arg) {
  check_for_duplicate<Char>(named_args, named_arg_index, arg.name);
  named_args[named_arg_index++] = {arg.name, arg_index++};
}

template <typename T, typename Char,
          FMT_ENABLE_IF(!is_static_named_arg<T>::value)>
FMT_CONSTEXPR void init_static_named_arg(named_arg_info<Char>*, int& arg_index,
                                         int&) {
  ++arg_index;
}
template <typename T, typename Char,
          FMT_ENABLE_IF(is_static_named_arg<T>::value)>
FMT_CONSTEXPR void init_static_named_arg(named_arg_info<Char>* named_args,
                                         int& arg_index, int& named_arg_index) {
  check_for_duplicate<Char>(named_args, named_arg_index, T::name);
  named_args[named_arg_index++] = {T::name, arg_index++};
}

// To minimize the number of types we need to deal with, long is translated
// either to int or to long long depending on its size.
enum { long_short = sizeof(long) == sizeof(int) && FMT_BUILTIN_TYPES };
using long_type = conditional_t<long_short, int, long long>;
using ulong_type = conditional_t<long_short, unsigned, unsigned long long>;

template <typename T>
using format_as_result =
    remove_cvref_t<decltype(format_as(std::declval<const T&>()))>;
template <typename T>
using format_as_member_result =
    remove_cvref_t<decltype(formatter<T>::format_as(std::declval<const T&>()))>;

template <typename T, typename Enable = std::true_type>
struct use_format_as : std::false_type {};
// format_as member is only used to avoid injection into the std namespace.
template <typename T, typename Enable = std::true_type>
struct use_format_as_member : std::false_type {};

// Only map owning types because mapping views can be unsafe.
template <typename T>
struct use_format_as<
    T, bool_constant<std::is_arithmetic<format_as_result<T>>::value>>
    : std::true_type {};
template <typename T>
struct use_format_as_member<
    T, bool_constant<std::is_arithmetic<format_as_member_result<T>>::value>>
    : std::true_type {};

template <typename T, typename U = remove_const_t<T>>
using use_formatter =
    bool_constant<(std::is_class<T>::value || std::is_enum<T>::value ||
                   std::is_union<T>::value || std::is_array<T>::value) &&
                  !has_to_string_view<T>::value && !is_named_arg<T>::value &&
                  !use_format_as<T>::value && !use_format_as_member<U>::value>;

template <typename Char, typename T, typename U = remove_const_t<T>>
auto has_formatter_impl(T* p, buffered_context<Char>* ctx = nullptr)
    -> decltype(formatter<U, Char>().format(*p, *ctx), std::true_type());
template <typename Char> auto has_formatter_impl(...) -> std::false_type;

// T can be const-qualified to check if it is const-formattable.
template <typename T, typename Char> constexpr auto has_formatter() -> bool {
  return decltype(has_formatter_impl<Char>(static_cast<T*>(nullptr)))::value;
}

// Maps formatting argument types to natively supported types or user-defined
// types with formatters. Returns void on errors to be SFINAE-friendly.
template <typename Char> struct type_mapper {
  static auto map(signed char) -> int;
  static auto map(unsigned char) -> unsigned;
  static auto map(short) -> int;
  static auto map(unsigned short) -> unsigned;
  static auto map(int) -> int;
  static auto map(unsigned) -> unsigned;
  static auto map(long) -> long_type;
  static auto map(unsigned long) -> ulong_type;
  static auto map(long long) -> long long;
  static auto map(unsigned long long) -> unsigned long long;
  static auto map(int128_opt) -> int128_opt;
  static auto map(uint128_opt) -> uint128_opt;
  static auto map(bool) -> bool;

  template <int N>
  static auto map(bitint<N>) -> conditional_t<N <= 64, long long, void>;
  template <int N>
  static auto map(ubitint<N>)
      -> conditional_t<N <= 64, unsigned long long, void>;

  template <typename T, FMT_ENABLE_IF(is_char<T>::value)>
  static auto map(T) -> conditional_t<
      std::is_same<T, char>::value || std::is_same<T, Char>::value, Char, void>;

  static auto map(float) -> float;
  static auto map(double) -> double;
  static auto map(long double) -> long double;

  static auto map(Char*) -> const Char*;
  static auto map(const Char*) -> const Char*;
  template <typename T, typename C = char_t<T>,
            FMT_ENABLE_IF(!std::is_pointer<T>::value)>
  static auto map(const T&) -> conditional_t<std::is_same<C, Char>::value,
                                             basic_string_view<C>, void>;

  static auto map(void*) -> const void*;
  static auto map(const void*) -> const void*;
  static auto map(volatile void*) -> const void*;
  static auto map(const volatile void*) -> const void*;
  static auto map(nullptr_t) -> const void*;
  template <typename T, FMT_ENABLE_IF(std::is_pointer<T>::value ||
                                      std::is_member_pointer<T>::value)>
  static auto map(const T&) -> void;

  template <typename T, FMT_ENABLE_IF(use_format_as<T>::value)>
  static auto map(const T& x) -> decltype(map(format_as(x)));
  template <typename T, FMT_ENABLE_IF(use_format_as_member<T>::value)>
  static auto map(const T& x) -> decltype(map(formatter<T>::format_as(x)));

  template <typename T, FMT_ENABLE_IF(use_formatter<T>::value)>
  static auto map(T&) -> conditional_t<has_formatter<T, Char>(), T&, void>;

  template <typename T, FMT_ENABLE_IF(is_named_arg<T>::value)>
  static auto map(const T& named_arg) -> decltype(map(named_arg.value));
};

// detail:: is used to workaround a bug in MSVC 2017.
template <typename T, typename Char>
using mapped_t = decltype(detail::type_mapper<Char>::map(std::declval<T&>()));

// A type constant after applying type_mapper.
template <typename T, typename Char = char>
using mapped_type_constant = type_constant<mapped_t<T, Char>, Char>;

template <typename T, typename Context,
          type TYPE =
              mapped_type_constant<T, typename Context::char_type>::value>
using stored_type_constant = std::integral_constant<
    type, Context::builtin_types || TYPE == type::int_type ? TYPE
                                                           : type::custom_type>;
// A parse context with extra data used only in compile-time checks.
template <typename Char>
class compile_parse_context : public parse_context<Char> {
 private:
  int num_args_;
  const type* types_;
  using base = parse_context<Char>;

 public:
  FMT_CONSTEXPR explicit compile_parse_context(basic_string_view<Char> fmt,
                                               int num_args, const type* types,
                                               int next_arg_id = 0)
      : base(fmt, next_arg_id), num_args_(num_args), types_(types) {}

  constexpr auto num_args() const -> int { return num_args_; }
  constexpr auto arg_type(int id) const -> type { return types_[id]; }

  FMT_CONSTEXPR auto next_arg_id() -> int {
    int id = base::next_arg_id();
    if (id >= num_args_) report_error("argument not found");
    return id;
  }

  FMT_CONSTEXPR void check_arg_id(int id) {
    base::check_arg_id(id);
    if (id >= num_args_) report_error("argument not found");
  }
  using base::check_arg_id;

  FMT_CONSTEXPR void check_dynamic_spec(int arg_id) {
    ignore_unused(arg_id);
    if (arg_id < num_args_ && types_ && !is_integral_type(types_[arg_id]))
      report_error("width/precision is not integer");
  }
};

// An argument reference.
template <typename Char> union arg_ref {
  FMT_CONSTEXPR arg_ref(int idx = 0) : index(idx) {}
  FMT_CONSTEXPR arg_ref(basic_string_view<Char> n) : name(n) {}

  int index;
  basic_string_view<Char> name;
};

// Format specifiers with width and precision resolved at formatting rather
// than parsing time to allow reusing the same parsed specifiers with
// different sets of arguments (precompilation of format strings).
template <typename Char = char> struct dynamic_format_specs : format_specs {
  arg_ref<Char> width_ref;
  arg_ref<Char> precision_ref;
};

// Converts a character to ASCII. Returns '\0' on conversion failure.
template <typename Char, FMT_ENABLE_IF(std::is_integral<Char>::value)>
constexpr auto to_ascii(Char c) -> char {
  return c <= 0xff ? static_cast<char>(c) : '\0';
}

// Returns the number of code units in a code point or 1 on error.
template <typename Char>
FMT_CONSTEXPR auto code_point_length(const Char* begin) -> int {
  if (const_check(sizeof(Char) != 1)) return 1;
  auto c = static_cast<unsigned char>(*begin);
  return static_cast<int>((0x3a55000000000000ull >> (2 * (c >> 3))) & 3) + 1;
}

// Parses the range [begin, end) as an unsigned integer. This function assumes
// that the range is non-empty and the first character is a digit.
template <typename Char>
FMT_CONSTEXPR auto parse_nonnegative_int(const Char*& begin, const Char* end,
                                         int error_value) noexcept -> int {
  FMT_ASSERT(begin != end && '0' <= *begin && *begin <= '9', "");
  unsigned value = 0, prev = 0;
  auto p = begin;
  do {
    prev = value;
    value = value * 10 + unsigned(*p - '0');
    ++p;
  } while (p != end && '0' <= *p && *p <= '9');
  auto num_digits = p - begin;
  begin = p;
  int digits10 = static_cast<int>(sizeof(int) * CHAR_BIT * 3 / 10);
  if (num_digits <= digits10) return static_cast<int>(value);
  // Check for overflow.
  unsigned max = INT_MAX;
  return num_digits == digits10 + 1 &&
                 prev * 10ull + unsigned(p[-1] - '0') <= max
             ? static_cast<int>(value)
             : error_value;
}

FMT_CONSTEXPR inline auto parse_align(char c) -> align {
  switch (c) {
  case '<': return align::left;
  case '>': return align::right;
  case '^': return align::center;
  }
  return align::none;
}

template <typename Char> constexpr auto is_name_start(Char c) -> bool {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

template <typename Char, typename Handler>
FMT_CONSTEXPR auto parse_arg_id(const Char* begin, const Char* end,
                                Handler&& handler) -> const Char* {
  Char c = *begin;
  if (c >= '0' && c <= '9') {
    int index = 0;
    if (c != '0')
      index = parse_nonnegative_int(begin, end, INT_MAX);
    else
      ++begin;
    if (begin == end || (*begin != '}' && *begin != ':'))
      report_error("invalid format string");
    else
      handler.on_index(index);
    return begin;
  }
  if (FMT_OPTIMIZE_SIZE > 1 || !is_name_start(c)) {
    report_error("invalid format string");
    return begin;
  }
  auto it = begin;
  do {
    ++it;
  } while (it != end && (is_name_start(*it) || ('0' <= *it && *it <= '9')));
  handler.on_name({begin, to_unsigned(it - begin)});
  return it;
}

template <typename Char> struct dynamic_spec_handler {
  parse_context<Char>& ctx;
  arg_ref<Char>& ref;
  arg_id_kind& kind;

  FMT_CONSTEXPR void on_index(int id) {
    ref = id;
    kind = arg_id_kind::index;
    ctx.check_arg_id(id);
    ctx.check_dynamic_spec(id);
  }
  FMT_CONSTEXPR void on_name(basic_string_view<Char> id) {
    ref = id;
    kind = arg_id_kind::name;
    ctx.check_arg_id(id);
  }
};

template <typename Char> struct parse_dynamic_spec_result {
  const Char* end;
  arg_id_kind kind;
};

// Parses integer | "{" [arg_id] "}".
template <typename Char>
FMT_CONSTEXPR auto parse_dynamic_spec(const Char* begin, const Char* end,
                                      int& value, arg_ref<Char>& ref,
                                      parse_context<Char>& ctx)
    -> parse_dynamic_spec_result<Char> {
  FMT_ASSERT(begin != end, "");
  auto kind = arg_id_kind::none;
  if ('0' <= *begin && *begin <= '9') {
    int val = parse_nonnegative_int(begin, end, -1);
    if (val == -1) report_error("number is too big");
    value = val;
  } else {
    if (*begin == '{') {
      ++begin;
      if (begin != end) {
        Char c = *begin;
        if (c == '}' || c == ':') {
          int id = ctx.next_arg_id();
          ref = id;
          kind = arg_id_kind::index;
          ctx.check_dynamic_spec(id);
        } else {
          begin = parse_arg_id(begin, end,
                               dynamic_spec_handler<Char>{ctx, ref, kind});
        }
      }
      if (begin != end && *begin == '}') return {++begin, kind};
    }
    report_error("invalid format string");
  }
  return {begin, kind};
}

template <typename Char>
FMT_CONSTEXPR auto parse_width(const Char* begin, const Char* end,
                               format_specs& specs, arg_ref<Char>& width_ref,
                               parse_context<Char>& ctx) -> const Char* {
  auto result = parse_dynamic_spec(begin, end, specs.width, width_ref, ctx);
  specs.set_dynamic_width(result.kind);
  return result.end;
}

template <typename Char>
FMT_CONSTEXPR auto parse_precision(const Char* begin, const Char* end,
                                   format_specs& specs,
                                   arg_ref<Char>& precision_ref,
                                   parse_context<Char>& ctx) -> const Char* {
  ++begin;
  if (begin == end) {
    report_error("invalid precision");
    return begin;
  }
  auto result =
      parse_dynamic_spec(begin, end, specs.precision, precision_ref, ctx);
  specs.set_dynamic_precision(result.kind);
  return result.end;
}

enum class state { start, align, sign, hash, zero, width, precision, locale };

// Parses standard format specifiers.
template <typename Char>
FMT_CONSTEXPR auto parse_format_specs(const Char* begin, const Char* end,
                                      dynamic_format_specs<Char>& specs,
                                      parse_context<Char>& ctx, type arg_type)
    -> const Char* {
  auto c = '\0';
  if (end - begin > 1) {
    auto next = to_ascii(begin[1]);
    c = parse_align(next) == align::none ? to_ascii(*begin) : '\0';
  } else {
    if (begin == end) return begin;
    c = to_ascii(*begin);
  }

  struct {
    state current_state = state::start;
    FMT_CONSTEXPR void operator()(state s, bool valid = true) {
      if (current_state >= s || !valid)
        report_error("invalid format specifier");
      current_state = s;
    }
  } enter_state;

  using pres = presentation_type;
  constexpr auto integral_set = sint_set | uint_set | bool_set | char_set;
  struct {
    const Char*& begin;
    format_specs& specs;
    type arg_type;

    FMT_CONSTEXPR auto operator()(pres pres_type, int set) -> const Char* {
      if (!in(arg_type, set)) report_error("invalid format specifier");
      specs.set_type(pres_type);
      return begin + 1;
    }
  } parse_presentation_type{begin, specs, arg_type};

  for (;;) {
    switch (c) {
    case '<':
    case '>':
    case '^':
      enter_state(state::align);
      specs.set_align(parse_align(c));
      ++begin;
      break;
    case '+':
    case ' ':
      specs.set_sign(c == ' ' ? sign::space : sign::plus);
      FMT_FALLTHROUGH;
    case '-':
      enter_state(state::sign, in(arg_type, sint_set | float_set));
      ++begin;
      break;
    case '#':
      enter_state(state::hash, is_arithmetic_type(arg_type));
      specs.set_alt();
      ++begin;
      break;
    case '0':
      enter_state(state::zero);
      if (!is_arithmetic_type(arg_type))
        report_error("format specifier requires numeric argument");
      if (specs.align() == align::none) {
        // Ignore 0 if align is specified for compatibility with std::format.
        specs.set_align(align::numeric);
        specs.set_fill('0');
      }
      ++begin;
      break;
      // clang-format off
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9': case '{':
      // clang-format on
      enter_state(state::width);
      begin = parse_width(begin, end, specs, specs.width_ref, ctx);
      break;
    case '.':
      enter_state(state::precision,
                  in(arg_type, float_set | string_set | cstring_set));
      begin = parse_precision(begin, end, specs, specs.precision_ref, ctx);
      break;
    case 'L':
      enter_state(state::locale, is_arithmetic_type(arg_type));
      specs.set_localized();
      ++begin;
      break;
    case 'd': return parse_presentation_type(pres::dec, integral_set);
    case 'X': specs.set_upper(); FMT_FALLTHROUGH;
    case 'x': return parse_presentation_type(pres::hex, integral_set);
    case 'o': return parse_presentation_type(pres::oct, integral_set);
    case 'B': specs.set_upper(); FMT_FALLTHROUGH;
    case 'b': return parse_presentation_type(pres::bin, integral_set);
    case 'E': specs.set_upper(); FMT_FALLTHROUGH;
    case 'e': return parse_presentation_type(pres::exp, float_set);
    case 'F': specs.set_upper(); FMT_FALLTHROUGH;
    case 'f': return parse_presentation_type(pres::fixed, float_set);
    case 'G': specs.set_upper(); FMT_FALLTHROUGH;
    case 'g': return parse_presentation_type(pres::general, float_set);
    case 'A': specs.set_upper(); FMT_FALLTHROUGH;
    case 'a': return parse_presentation_type(pres::hexfloat, float_set);
    case 'c':
      if (arg_type == type::bool_type) report_error("invalid format specifier");
      return parse_presentation_type(pres::chr, integral_set);
    case 's':
      return parse_presentation_type(pres::string,
                                     bool_set | string_set | cstring_set);
    case 'p':
      return parse_presentation_type(pres::pointer, pointer_set | cstring_set);
    case '?':
      return parse_presentation_type(pres::debug,
                                     char_set | string_set | cstring_set);
    case '}': return begin;
    default:  {
      if (*begin == '}') return begin;
      // Parse fill and alignment.
      auto fill_end = begin + code_point_length(begin);
      if (end - fill_end <= 0) {
        report_error("invalid format specifier");
        return begin;
      }
      if (*begin == '{') {
        report_error("invalid fill character '{'");
        return begin;
      }
      auto alignment = parse_align(to_ascii(*fill_end));
      enter_state(state::align, alignment != align::none);
      specs.set_fill(
          basic_string_view<Char>(begin, to_unsigned(fill_end - begin)));
      specs.set_align(alignment);
      begin = fill_end + 1;
    }
    }
    if (begin == end) return begin;
    c = to_ascii(*begin);
  }
}

template <typename Char, typename Handler>
FMT_CONSTEXPR FMT_INLINE auto parse_replacement_field(const Char* begin,
                                                      const Char* end,
                                                      Handler&& handler)
    -> const Char* {
  ++begin;
  if (begin == end) {
    handler.on_error("invalid format string");
    return end;
  }
  int arg_id = 0;
  switch (*begin) {
  case '}':
    handler.on_replacement_field(handler.on_arg_id(), begin);
    return begin + 1;
  case '{': handler.on_text(begin, begin + 1); return begin + 1;
  case ':': arg_id = handler.on_arg_id(); break;
  default:  {
    struct id_adapter {
      Handler& handler;
      int arg_id;

      FMT_CONSTEXPR void on_index(int id) { arg_id = handler.on_arg_id(id); }
      FMT_CONSTEXPR void on_name(basic_string_view<Char> id) {
        arg_id = handler.on_arg_id(id);
      }
    } adapter = {handler, 0};
    begin = parse_arg_id(begin, end, adapter);
    arg_id = adapter.arg_id;
    Char c = begin != end ? *begin : Char();
    if (c == '}') {
      handler.on_replacement_field(arg_id, begin);
      return begin + 1;
    }
    if (c != ':') {
      handler.on_error("missing '}' in format string");
      return end;
    }
    break;
  }
  }
  begin = handler.on_format_specs(arg_id, begin + 1, end);
  if (begin == end || *begin != '}')
    return handler.on_error("unknown format specifier"), end;
  return begin + 1;
}

template <typename Char, typename Handler>
FMT_CONSTEXPR void parse_format_string(basic_string_view<Char> fmt,
                                       Handler&& handler) {
  auto begin = fmt.data(), end = begin + fmt.size();
  auto p = begin;
  while (p != end) {
    auto c = *p++;
    if (c == '{') {
      handler.on_text(begin, p - 1);
      begin = p = parse_replacement_field(p - 1, end, handler);
    } else if (c == '}') {
      if (p == end || *p != '}')
        return handler.on_error("unmatched '}' in format string");
      handler.on_text(begin, p);
      begin = ++p;
    }
  }
  handler.on_text(begin, end);
}

// Checks char specs and returns true iff the presentation type is char-like.
FMT_CONSTEXPR inline auto check_char_specs(const format_specs& specs) -> bool {
  auto type = specs.type();
  if (type != presentation_type::none && type != presentation_type::chr &&
      type != presentation_type::debug) {
    return false;
  }
  if (specs.align() == align::numeric || specs.sign() != sign::none ||
      specs.alt()) {
    report_error("invalid format specifier for char");
  }
  return true;
}

// A base class for compile-time strings.
struct compile_string {};

template <typename T, typename Char>
FMT_VISIBILITY("hidden")  // Suppress an ld warning on macOS (#3769).
FMT_CONSTEXPR auto invoke_parse(parse_context<Char>& ctx) -> const Char* {
  using mapped_type = remove_cvref_t<mapped_t<T, Char>>;
  constexpr bool formattable =
      std::is_constructible<formatter<mapped_type, Char>>::value;
  if (!formattable) return ctx.begin();  // Error is reported in the value ctor.
  using formatted_type = conditional_t<formattable, mapped_type, int>;
  return formatter<formatted_type, Char>().parse(ctx);
}

template <typename... T> struct arg_pack {};

template <typename Char, int NUM_ARGS, int NUM_NAMED_ARGS, bool DYNAMIC_NAMES>
class format_string_checker {
 private:
  type types_[max_of<size_t>(1, NUM_ARGS)];
  named_arg_info<Char> named_args_[max_of<size_t>(1, NUM_NAMED_ARGS)];
  compile_parse_context<Char> context_;

  using parse_func = auto (*)(parse_context<Char>&) -> const Char*;
  parse_func parse_funcs_[max_of<size_t>(1, NUM_ARGS)];

 public:
  template <typename... T>
  FMT_CONSTEXPR explicit format_string_checker(basic_string_view<Char> fmt,
                                               arg_pack<T...>)
      : types_{mapped_type_constant<T, Char>::value...},
        named_args_{},
        context_(fmt, NUM_ARGS, types_),
        parse_funcs_{&invoke_parse<T, Char>...} {
    int arg_index = 0, named_arg_index = 0;
    FMT_APPLY_VARIADIC(
        init_static_named_arg<T>(named_args_, arg_index, named_arg_index));
    ignore_unused(arg_index, named_arg_index);
  }

  FMT_CONSTEXPR void on_text(const Char*, const Char*) {}

  FMT_CONSTEXPR auto on_arg_id() -> int { return context_.next_arg_id(); }
  FMT_CONSTEXPR auto on_arg_id(int id) -> int {
    context_.check_arg_id(id);
    return id;
  }
  FMT_CONSTEXPR auto on_arg_id(basic_string_view<Char> id) -> int {
    for (int i = 0; i < NUM_NAMED_ARGS; ++i) {
      if (named_args_[i].name == id) return named_args_[i].id;
    }
    if (!DYNAMIC_NAMES) on_error("argument not found");
    return -1;
  }

  FMT_CONSTEXPR void on_replacement_field(int id, const Char* begin) {
    on_format_specs(id, begin, begin);  // Call parse() on empty specs.
  }

  FMT_CONSTEXPR auto on_format_specs(int id, const Char* begin, const Char* end)
      -> const Char* {
    context_.advance_to(begin);
    if (id >= 0 && id < NUM_ARGS) return parse_funcs_[id](context_);

    // If id is out of range, it means we do not know the type and cannot parse
    // the format at compile time. Instead, skip over content until we finish
    // the format spec, accounting for any nested replacements.
    for (int bracket_count = 0;
         begin != end && (bracket_count > 0 || *begin != '}'); ++begin) {
      if (*begin == '{')
        ++bracket_count;
      else if (*begin == '}')
        --bracket_count;
    }
    return begin;
  }

  FMT_NORETURN FMT_CONSTEXPR void on_error(const char* message) {
    report_error(message);
  }
};

/// A contiguous memory buffer with an optional growing ability. It is an
/// internal class and shouldn't be used directly, only via `memory_buffer`.
template <typename T> class buffer {
 private:
  T* ptr_;
  size_t size_;
  size_t capacity_;

  using grow_fun = void (*)(buffer& buf, size_t capacity);
  grow_fun grow_;

 protected:
  // Don't initialize ptr_ since it is not accessed to save a few cycles.
  FMT_MSC_WARNING(suppress : 26495)
  FMT_CONSTEXPR buffer(grow_fun grow, size_t sz) noexcept
      : size_(sz), capacity_(sz), grow_(grow) {}

  constexpr buffer(grow_fun grow, T* p = nullptr, size_t sz = 0,
                   size_t cap = 0) noexcept
      : ptr_(p), size_(sz), capacity_(cap), grow_(grow) {}

  FMT_CONSTEXPR20 ~buffer() = default;
  buffer(buffer&&) = default;

  /// Sets the buffer data and capacity.
  FMT_CONSTEXPR void set(T* buf_data, size_t buf_capacity) noexcept {
    ptr_ = buf_data;
    capacity_ = buf_capacity;
  }

 public:
  using value_type = T;
  using const_reference = const T&;

  buffer(const buffer&) = delete;
  void operator=(const buffer&) = delete;

  auto begin() noexcept -> T* { return ptr_; }
  auto end() noexcept -> T* { return ptr_ + size_; }

  auto begin() const noexcept -> const T* { return ptr_; }
  auto end() const noexcept -> const T* { return ptr_ + size_; }

  /// Returns the size of this buffer.
  constexpr auto size() const noexcept -> size_t { return size_; }

  /// Returns the capacity of this buffer.
  constexpr auto capacity() const noexcept -> size_t { return capacity_; }

  /// Returns a pointer to the buffer data (not null-terminated).
  FMT_CONSTEXPR auto data() noexcept -> T* { return ptr_; }
  FMT_CONSTEXPR auto data() const noexcept -> const T* { return ptr_; }

  /// Clears this buffer.
  FMT_CONSTEXPR void clear() { size_ = 0; }

  // Tries resizing the buffer to contain `count` elements. If T is a POD type
  // the new elements may not be initialized.
  FMT_CONSTEXPR void try_resize(size_t count) {
    try_reserve(count);
    size_ = min_of(count, capacity_);
  }

  // Tries increasing the buffer capacity to `new_capacity`. It can increase the
  // capacity by a smaller amount than requested but guarantees there is space
  // for at least one additional element either by increasing the capacity or by
  // flushing the buffer if it is full.
  FMT_CONSTEXPR void try_reserve(size_t new_capacity) {
    if (new_capacity > capacity_) grow_(*this, new_capacity);
  }

  FMT_CONSTEXPR void push_back(const T& value) {
    try_reserve(size_ + 1);
    ptr_[size_++] = value;
  }

  /// Appends data to the end of the buffer.
  template <typename U>
// Workaround for MSVC2019 to fix error C2893: Failed to specialize function
// template 'void fmt::v11::detail::buffer<T>::append(const U *,const U *)'.
#if !FMT_MSC_VERSION || FMT_MSC_VERSION >= 1940
  FMT_CONSTEXPR20
#endif
      void
      append(const U* begin, const U* end) {
    while (begin != end) {
      auto count = to_unsigned(end - begin);
      try_reserve(size_ + count);
      auto free_cap = capacity_ - size_;
      if (free_cap < count) count = free_cap;
      // A loop is faster than memcpy on small sizes.
      T* out = ptr_ + size_;
      for (size_t i = 0; i < count; ++i) out[i] = begin[i];
      size_ += count;
      begin += count;
    }
  }

  template <typename Idx> FMT_CONSTEXPR auto operator[](Idx index) -> T& {
    return ptr_[index];
  }
  template <typename Idx>
  FMT_CONSTEXPR auto operator[](Idx index) const -> const T& {
    return ptr_[index];
  }
};

struct buffer_traits {
  constexpr explicit buffer_traits(size_t) {}
  constexpr auto count() const -> size_t { return 0; }
  constexpr auto limit(size_t size) const -> size_t { return size; }
};

class fixed_buffer_traits {
 private:
  size_t count_ = 0;
  size_t limit_;

 public:
  constexpr explicit fixed_buffer_traits(size_t limit) : limit_(limit) {}
  constexpr auto count() const -> size_t { return count_; }
  FMT_CONSTEXPR auto limit(size_t size) -> size_t {
    size_t n = limit_ > count_ ? limit_ - count_ : 0;
    count_ += size;
    return min_of(size, n);
  }
};

// A buffer that writes to an output iterator when flushed.
template <typename OutputIt, typename T, typename Traits = buffer_traits>
class iterator_buffer : public Traits, public buffer<T> {
 private:
  OutputIt out_;
  enum { buffer_size = 256 };
  T data_[buffer_size];

  static FMT_CONSTEXPR void grow(buffer<T>& buf, size_t) {
    if (buf.size() == buffer_size) static_cast<iterator_buffer&>(buf).flush();
  }

  void flush() {
    auto size = this->size();
    this->clear();
    const T* begin = data_;
    const T* end = begin + this->limit(size);
    while (begin != end) *out_++ = *begin++;
  }

 public:
  explicit iterator_buffer(OutputIt out, size_t n = buffer_size)
      : Traits(n), buffer<T>(grow, data_, 0, buffer_size), out_(out) {}
  iterator_buffer(iterator_buffer&& other) noexcept
      : Traits(other),
        buffer<T>(grow, data_, 0, buffer_size),
        out_(other.out_) {}
  ~iterator_buffer() {
    // Don't crash if flush fails during unwinding.
    FMT_TRY { flush(); }
    FMT_CATCH(...) {}
  }

  auto out() -> OutputIt {
    flush();
    return out_;
  }
  auto count() const -> size_t { return Traits::count() + this->size(); }
};

template <typename T>
class iterator_buffer<T*, T, fixed_buffer_traits> : public fixed_buffer_traits,
                                                    public buffer<T> {
 private:
  T* out_;
  enum { buffer_size = 256 };
  T data_[buffer_size];

  static FMT_CONSTEXPR void grow(buffer<T>& buf, size_t) {
    if (buf.size() == buf.capacity())
      static_cast<iterator_buffer&>(buf).flush();
  }

  void flush() {
    size_t n = this->limit(this->size());
    if (this->data() == out_) {
      out_ += n;
      this->set(data_, buffer_size);
    }
    this->clear();
  }

 public:
  explicit iterator_buffer(T* out, size_t n = buffer_size)
      : fixed_buffer_traits(n), buffer<T>(grow, out, 0, n), out_(out) {}
  iterator_buffer(iterator_buffer&& other) noexcept
      : fixed_buffer_traits(other),
        buffer<T>(static_cast<iterator_buffer&&>(other)),
        out_(other.out_) {
    if (this->data() != out_) {
      this->set(data_, buffer_size);
      this->clear();
    }
  }
  ~iterator_buffer() { flush(); }

  auto out() -> T* {
    flush();
    return out_;
  }
  auto count() const -> size_t {
    return fixed_buffer_traits::count() + this->size();
  }
};

template <typename T> class iterator_buffer<T*, T> : public buffer<T> {
 public:
  explicit iterator_buffer(T* out, size_t = 0)
      : buffer<T>([](buffer<T>&, size_t) {}, out, 0, ~size_t()) {}

  auto out() -> T* { return &*this->end(); }
};

template <typename Container>
class container_buffer : public buffer<typename Container::value_type> {
 private:
  using value_type = typename Container::value_type;

  static FMT_CONSTEXPR void grow(buffer<value_type>& buf, size_t capacity) {
    auto& self = static_cast<container_buffer&>(buf);
    self.container.resize(capacity);
    self.set(&self.container[0], capacity);
  }

 public:
  Container& container;

  explicit container_buffer(Container& c)
      : buffer<value_type>(grow, c.size()), container(c) {}
};

// A buffer that writes to a container with the contiguous storage.
template <typename OutputIt>
class iterator_buffer<
    OutputIt,
    enable_if_t<is_back_insert_iterator<OutputIt>::value &&
                    is_contiguous<typename OutputIt::container_type>::value,
                typename OutputIt::container_type::value_type>>
    : public container_buffer<typename OutputIt::container_type> {
 private:
  using base = container_buffer<typename OutputIt::container_type>;

 public:
  explicit iterator_buffer(typename OutputIt::container_type& c) : base(c) {}
  explicit iterator_buffer(OutputIt out, size_t = 0)
      : base(get_container(out)) {}

  auto out() -> OutputIt { return OutputIt(this->container); }
};

// A buffer that counts the number of code units written discarding the output.
template <typename T = char> class counting_buffer : public buffer<T> {
 private:
  enum { buffer_size = 256 };
  T data_[buffer_size];
  size_t count_ = 0;

  static FMT_CONSTEXPR void grow(buffer<T>& buf, size_t) {
    if (buf.size() != buffer_size) return;
    static_cast<counting_buffer&>(buf).count_ += buf.size();
    buf.clear();
  }

 public:
  FMT_CONSTEXPR counting_buffer() : buffer<T>(grow, data_, 0, buffer_size) {}

  constexpr auto count() const noexcept -> size_t {
    return count_ + this->size();
  }
};

template <typename T>
struct is_back_insert_iterator<basic_appender<T>> : std::true_type {};

template <typename OutputIt, typename InputIt, typename = void>
struct has_back_insert_iterator_container_append : std::false_type {};
template <typename OutputIt, typename InputIt>
struct has_back_insert_iterator_container_append<
    OutputIt, InputIt,
    void_t<decltype(get_container(std::declval<OutputIt>())
                        .append(std::declval<InputIt>(),
                                std::declval<InputIt>()))>> : std::true_type {};

template <typename OutputIt, typename InputIt, typename = void>
struct has_back_insert_iterator_container_insert_at_end : std::false_type {};

template <typename OutputIt, typename InputIt>
struct has_back_insert_iterator_container_insert_at_end<
    OutputIt, InputIt,
    void_t<decltype(get_container(std::declval<OutputIt>())
                        .insert(get_container(std::declval<OutputIt>()).end(),
                                std::declval<InputIt>(),
                                std::declval<InputIt>()))>> : std::true_type {};

// An optimized version of std::copy with the output value type (T).
template <typename T, typename InputIt, typename OutputIt,
          FMT_ENABLE_IF(is_back_insert_iterator<OutputIt>::value&&
                            has_back_insert_iterator_container_append<
                                OutputIt, InputIt>::value)>
FMT_CONSTEXPR20 auto copy(InputIt begin, InputIt end, OutputIt out)
    -> OutputIt {
  get_container(out).append(begin, end);
  return out;
}

template <typename T, typename InputIt, typename OutputIt,
          FMT_ENABLE_IF(is_back_insert_iterator<OutputIt>::value &&
                        !has_back_insert_iterator_container_append<
                            OutputIt, InputIt>::value &&
                        has_back_insert_iterator_container_insert_at_end<
                            OutputIt, InputIt>::value)>
FMT_CONSTEXPR20 auto copy(InputIt begin, InputIt end, OutputIt out)
    -> OutputIt {
  auto& c = get_container(out);
  c.insert(c.end(), begin, end);
  return out;
}

template <typename T, typename InputIt, typename OutputIt,
          FMT_ENABLE_IF(!(is_back_insert_iterator<OutputIt>::value &&
                          (has_back_insert_iterator_container_append<
                               OutputIt, InputIt>::value ||
                           has_back_insert_iterator_container_insert_at_end<
                               OutputIt, InputIt>::value)))>
FMT_CONSTEXPR auto copy(InputIt begin, InputIt end, OutputIt out) -> OutputIt {
  while (begin != end) *out++ = static_cast<T>(*begin++);
  return out;
}

template <typename T, typename V, typename OutputIt>
FMT_CONSTEXPR auto copy(basic_string_view<V> s, OutputIt out) -> OutputIt {
  return copy<T>(s.begin(), s.end(), out);
}

template <typename It, typename Enable = std::true_type>
struct is_buffer_appender : std::false_type {};
template <typename It>
struct is_buffer_appender<
    It, bool_constant<
            is_back_insert_iterator<It>::value &&
            std::is_base_of<buffer<typename It::container_type::value_type>,
                            typename It::container_type>::value>>
    : std::true_type {};

// Maps an output iterator to a buffer.
template <typename T, typename OutputIt,
          FMT_ENABLE_IF(!is_buffer_appender<OutputIt>::value)>
auto get_buffer(OutputIt out) -> iterator_buffer<OutputIt, T> {
  return iterator_buffer<OutputIt, T>(out);
}
template <typename T, typename OutputIt,
          FMT_ENABLE_IF(is_buffer_appender<OutputIt>::value)>
auto get_buffer(OutputIt out) -> buffer<T>& {
  return get_container(out);
}

template <typename Buf, typename OutputIt>
auto get_iterator(Buf& buf, OutputIt) -> decltype(buf.out()) {
  return buf.out();
}
template <typename T, typename OutputIt>
auto get_iterator(buffer<T>&, OutputIt out) -> OutputIt {
  return out;
}

// This type is intentionally undefined, only used for errors.
template <typename T, typename Char> struct type_is_unformattable_for;

template <typename Char> struct string_value {
  const Char* data;
  size_t size;
  auto str() const -> basic_string_view<Char> { return {data, size}; }
};

template <typename Context> struct custom_value {
  using char_type = typename Context::char_type;
  void* value;
  void (*format)(void* arg, parse_context<char_type>& parse_ctx, Context& ctx);
};

template <typename Char> struct named_arg_value {
  const named_arg_info<Char>* data;
  size_t size;
};

struct custom_tag {};

#if !FMT_BUILTIN_TYPES
#  define FMT_BUILTIN , monostate
#else
#  define FMT_BUILTIN
#endif

// A formatting argument value.
template <typename Context> class value {
 public:
  using char_type = typename Context::char_type;

  union {
    monostate no_value;
    int int_value;
    unsigned uint_value;
    long long long_long_value;
    unsigned long long ulong_long_value;
    int128_opt int128_value;
    uint128_opt uint128_value;
    bool bool_value;
    char_type char_value;
    float float_value;
    double double_value;
    long double long_double_value;
    const void* pointer;
    string_value<char_type> string;
    custom_value<Context> custom;
    named_arg_value<char_type> named_args;
  };

  constexpr FMT_INLINE value() : no_value() {}
  constexpr FMT_INLINE value(signed char x) : int_value(x) {}
  constexpr FMT_INLINE value(unsigned char x FMT_BUILTIN) : uint_value(x) {}
  constexpr FMT_INLINE value(signed short x) : int_value(x) {}
  constexpr FMT_INLINE value(unsigned short x FMT_BUILTIN) : uint_value(x) {}
  constexpr FMT_INLINE value(int x) : int_value(x) {}
  constexpr FMT_INLINE value(unsigned x FMT_BUILTIN) : uint_value(x) {}
  FMT_CONSTEXPR FMT_INLINE value(long x FMT_BUILTIN) : value(long_type(x)) {}
  FMT_CONSTEXPR FMT_INLINE value(unsigned long x FMT_BUILTIN)
      : value(ulong_type(x)) {}
  constexpr FMT_INLINE value(long long x FMT_BUILTIN) : long_long_value(x) {}
  constexpr FMT_INLINE value(unsigned long long x FMT_BUILTIN)
      : ulong_long_value(x) {}
  FMT_INLINE value(int128_opt x FMT_BUILTIN) : int128_value(x) {}
  FMT_INLINE value(uint128_opt x FMT_BUILTIN) : uint128_value(x) {}
  constexpr FMT_INLINE value(bool x FMT_BUILTIN) : bool_value(x) {}

  template <int N>
  constexpr FMT_INLINE value(bitint<N> x FMT_BUILTIN) : long_long_value(x) {
    static_assert(N <= 64, "unsupported _BitInt");
  }
  template <int N>
  constexpr FMT_INLINE value(ubitint<N> x FMT_BUILTIN) : ulong_long_value(x) {
    static_assert(N <= 64, "unsupported _BitInt");
  }

  template <typename T, FMT_ENABLE_IF(is_char<T>::value)>
  constexpr FMT_INLINE value(T x FMT_BUILTIN) : char_value(x) {
    static_assert(
        std::is_same<T, char>::value || std::is_same<T, char_type>::value,
        "mixing character types is disallowed");
  }

  constexpr FMT_INLINE value(float x FMT_BUILTIN) : float_value(x) {}
  constexpr FMT_INLINE value(double x FMT_BUILTIN) : double_value(x) {}
  FMT_INLINE value(long double x FMT_BUILTIN) : long_double_value(x) {}

  FMT_CONSTEXPR FMT_INLINE value(char_type* x FMT_BUILTIN) {
    string.data = x;
    if (is_constant_evaluated()) string.size = 0;
  }
  FMT_CONSTEXPR FMT_INLINE value(const char_type* x FMT_BUILTIN) {
    string.data = x;
    if (is_constant_evaluated()) string.size = 0;
  }
  template <typename T, typename C = char_t<T>,
            FMT_ENABLE_IF(!std::is_pointer<T>::value)>
  FMT_CONSTEXPR value(const T& x FMT_BUILTIN) {
    static_assert(std::is_same<C, char_type>::value,
                  "mixing character types is disallowed");
    auto sv = to_string_view(x);
    string.data = sv.data();
    string.size = sv.size();
  }
  FMT_INLINE value(void* x FMT_BUILTIN) : pointer(x) {}
  FMT_INLINE value(const void* x FMT_BUILTIN) : pointer(x) {}
  FMT_INLINE value(volatile void* x FMT_BUILTIN)
      : pointer(const_cast<const void*>(x)) {}
  FMT_INLINE value(const volatile void* x FMT_BUILTIN)
      : pointer(const_cast<const void*>(x)) {}
  FMT_INLINE value(nullptr_t) : pointer(nullptr) {}

  template <typename T, FMT_ENABLE_IF(std::is_pointer<T>::value ||
                                      std::is_member_pointer<T>::value)>
  value(const T&) {
    // Formatting of arbitrary pointers is disallowed. If you want to format a
    // pointer cast it to `void*` or `const void*`. In particular, this forbids
    // formatting of `[const] volatile char*` printed as bool by iostreams.
    static_assert(sizeof(T) == 0,
                  "formatting of non-void pointers is disallowed");
  }

  template <typename T, FMT_ENABLE_IF(use_format_as<T>::value)>
  value(const T& x) : value(format_as(x)) {}
  template <typename T, FMT_ENABLE_IF(use_format_as_member<T>::value)>
  value(const T& x) : value(formatter<T>::format_as(x)) {}

  template <typename T, FMT_ENABLE_IF(is_named_arg<T>::value)>
  value(const T& named_arg) : value(named_arg.value) {}

  template <typename T,
            FMT_ENABLE_IF(use_formatter<T>::value || !FMT_BUILTIN_TYPES)>
  FMT_CONSTEXPR20 FMT_INLINE value(T& x) : value(x, custom_tag()) {}

  FMT_ALWAYS_INLINE value(const named_arg_info<char_type>* args, size_t size)
      : named_args{args, size} {}

 private:
  template <typename T, FMT_ENABLE_IF(has_formatter<T, char_type>())>
  FMT_CONSTEXPR value(T& x, custom_tag) {
    using value_type = remove_const_t<T>;
    // T may overload operator& e.g. std::vector<bool>::reference in libc++.
    if (!is_constant_evaluated()) {
      custom.value =
          const_cast<char*>(&reinterpret_cast<const volatile char&>(x));
    } else {
      custom.value = nullptr;
#if defined(__cpp_if_constexpr)
      if constexpr (std::is_same<decltype(&x), remove_reference_t<T>*>::value)
        custom.value = const_cast<value_type*>(&x);
#endif
    }
    custom.format = format_custom<value_type, formatter<value_type, char_type>>;
  }

  template <typename T, FMT_ENABLE_IF(!has_formatter<T, char_type>())>
  FMT_CONSTEXPR value(const T&, custom_tag) {
    // Cannot format an argument; to make type T formattable provide a
    // formatter<T> specialization: https://fmt.dev/latest/api.html#udt.
    type_is_unformattable_for<T, char_type> _;
  }

  // Formats an argument of a custom type, such as a user-defined class.
  // DEPRECATED! Formatter template parameter will be removed.
  template <typename T, typename Formatter>
  static void format_custom(void* arg, parse_context<char_type>& parse_ctx,
                            Context& ctx) {
    auto f = Formatter();
    parse_ctx.advance_to(f.parse(parse_ctx));
    using qualified_type =
        conditional_t<has_formatter<const T, char_type>(), const T, T>;
    // format must be const for compatibility with std::format and compilation.
    const auto& cf = f;
    ctx.advance_to(cf.format(*static_cast<qualified_type*>(arg), ctx));
  }
};

enum { packed_arg_bits = 4 };
// Maximum number of arguments with packed types.
enum { max_packed_args = 62 / packed_arg_bits };
enum : unsigned long long { is_unpacked_bit = 1ULL << 63 };
enum : unsigned long long { has_named_args_bit = 1ULL << 62 };

template <typename It, typename T, typename Enable = void>
struct is_output_iterator : std::false_type {};

template <> struct is_output_iterator<appender, char> : std::true_type {};

template <typename It, typename T>
struct is_output_iterator<
    It, T,
    enable_if_t<std::is_assignable<decltype(*std::declval<decay_t<It>&>()++),
                                   T>::value>> : std::true_type {};

#ifndef FMT_USE_LOCALE
#  define FMT_USE_LOCALE (FMT_OPTIMIZE_SIZE <= 1)
#endif

// A type-erased reference to an std::locale to avoid a heavy <locale> include.
class locale_ref {
#if FMT_USE_LOCALE
 private:
  const void* locale_;  // A type-erased pointer to std::locale.

 public:
  constexpr locale_ref() : locale_(nullptr) {}
  template <typename Locale> locale_ref(const Locale& loc);

  inline explicit operator bool() const noexcept { return locale_ != nullptr; }
#endif  // FMT_USE_LOCALE

 public:
  template <typename Locale> auto get() const -> Locale;
};

template <typename> constexpr auto encode_types() -> unsigned long long {
  return 0;
}

template <typename Context, typename Arg, typename... Args>
constexpr auto encode_types() -> unsigned long long {
  return static_cast<unsigned>(stored_type_constant<Arg, Context>::value) |
         (encode_types<Context, Args...>() << packed_arg_bits);
}

template <typename Context, typename... T, size_t NUM_ARGS = sizeof...(T)>
constexpr auto make_descriptor() -> unsigned long long {
  return NUM_ARGS <= max_packed_args ? encode_types<Context, T...>()
                                     : is_unpacked_bit | NUM_ARGS;
}

template <typename Context, int NUM_ARGS>
using arg_t = conditional_t<NUM_ARGS <= max_packed_args, value<Context>,
                            basic_format_arg<Context>>;

template <typename Context, int NUM_ARGS, int NUM_NAMED_ARGS,
          unsigned long long DESC>
struct named_arg_store {
  // args_[0].named_args points to named_args to avoid bloating format_args.
  arg_t<Context, NUM_ARGS> args[1u + NUM_ARGS];
  named_arg_info<typename Context::char_type>
      named_args[static_cast<size_t>(NUM_NAMED_ARGS)];

  template <typename... T>
  FMT_CONSTEXPR FMT_ALWAYS_INLINE named_arg_store(T&... values)
      : args{{named_args, NUM_NAMED_ARGS}, values...} {
    int arg_index = 0, named_arg_index = 0;
    FMT_APPLY_VARIADIC(
        init_named_arg(named_args, arg_index, named_arg_index, values));
  }

  named_arg_store(named_arg_store&& rhs) {
    args[0] = {named_args, NUM_NAMED_ARGS};
    for (size_t i = 1; i < sizeof(args) / sizeof(*args); ++i)
      args[i] = rhs.args[i];
    for (size_t i = 0; i < NUM_NAMED_ARGS; ++i)
      named_args[i] = rhs.named_args[i];
  }

  named_arg_store(const named_arg_store& rhs) = delete;
  named_arg_store& operator=(const named_arg_store& rhs) = delete;
  named_arg_store& operator=(named_arg_store&& rhs) = delete;
  operator const arg_t<Context, NUM_ARGS>*() const { return args + 1; }
};

// An array of references to arguments. It can be implicitly converted to
// `basic_format_args` for passing into type-erased formatting functions
// such as `vformat`. It is a plain struct to reduce binary size in debug mode.
template <typename Context, int NUM_ARGS, int NUM_NAMED_ARGS,
          unsigned long long DESC>
struct format_arg_store {
  // +1 to workaround a bug in gcc 7.5 that causes duplicated-branches warning.
  using type =
      conditional_t<NUM_NAMED_ARGS == 0,
                    arg_t<Context, NUM_ARGS>[max_of<size_t>(1, NUM_ARGS)],
                    named_arg_store<Context, NUM_ARGS, NUM_NAMED_ARGS, DESC>>;
  type args;
};

// TYPE can be different from type_constant<T>, e.g. for __float128.
template <typename T, typename Char, type TYPE> struct native_formatter {
 private:
  dynamic_format_specs<Char> specs_;

 public:
  using nonlocking = void;

  FMT_CONSTEXPR auto parse(parse_context<Char>& ctx) -> const Char* {
    if (ctx.begin() == ctx.end() || *ctx.begin() == '}') return ctx.begin();
    auto end = parse_format_specs(ctx.begin(), ctx.end(), specs_, ctx, TYPE);
    if (const_check(TYPE == type::char_type)) check_char_specs(specs_);
    return end;
  }

  template <type U = TYPE,
            FMT_ENABLE_IF(U == type::string_type || U == type::cstring_type ||
                          U == type::char_type)>
  FMT_CONSTEXPR void set_debug_format(bool set = true) {
    specs_.set_type(set ? presentation_type::debug : presentation_type::none);
  }

  FMT_PRAGMA_CLANG(diagnostic ignored "-Wundefined-inline")
  template <typename FormatContext>
  FMT_CONSTEXPR auto format(const T& val, FormatContext& ctx) const
      -> decltype(ctx.out());
};

template <typename T, typename Enable = void>
struct locking
    : bool_constant<mapped_type_constant<T>::value == type::custom_type> {};
template <typename T>
struct locking<T, void_t<typename formatter<remove_cvref_t<T>>::nonlocking>>
    : std::false_type {};

template <typename T = int> FMT_CONSTEXPR inline auto is_locking() -> bool {
  return locking<T>::value;
}
template <typename T1, typename T2, typename... Tail>
FMT_CONSTEXPR inline auto is_locking() -> bool {
  return locking<T1>::value || is_locking<T2, Tail...>();
}

FMT_API void vformat_to(buffer<char>& buf, string_view fmt, format_args args,
                        locale_ref loc = {});

#if FMT_WIN32
FMT_API void vprint_mojibake(FILE*, string_view, format_args, bool);
#else  // format_args is passed by reference since it is defined later.
inline void vprint_mojibake(FILE*, string_view, const format_args&, bool) {}
#endif
}  // namespace detail

// The main public API.

template <typename Char>
FMT_CONSTEXPR void parse_context<Char>::do_check_arg_id(int arg_id) {
  // Argument id is only checked at compile time during parsing because
  // formatting has its own validation.
  if (detail::is_constant_evaluated() && use_constexpr_cast) {
    auto ctx = static_cast<detail::compile_parse_context<Char>*>(this);
    if (arg_id >= ctx->num_args()) report_error("argument not found");
  }
}

template <typename Char>
FMT_CONSTEXPR void parse_context<Char>::check_dynamic_spec(int arg_id) {
  using detail::compile_parse_context;
  if (detail::is_constant_evaluated() && use_constexpr_cast)
    static_cast<compile_parse_context<Char>*>(this)->check_dynamic_spec(arg_id);
}

FMT_BEGIN_EXPORT

// An output iterator that appends to a buffer. It is used instead of
// back_insert_iterator to reduce symbol sizes and avoid <iterator> dependency.
template <typename T> class basic_appender {
 protected:
  detail::buffer<T>* container;

 public:
  using container_type = detail::buffer<T>;

  FMT_CONSTEXPR basic_appender(detail::buffer<T>& buf) : container(&buf) {}

  FMT_CONSTEXPR20 auto operator=(T c) -> basic_appender& {
    container->push_back(c);
    return *this;
  }
  FMT_CONSTEXPR20 auto operator*() -> basic_appender& { return *this; }
  FMT_CONSTEXPR20 auto operator++() -> basic_appender& { return *this; }
  FMT_CONSTEXPR20 auto operator++(int) -> basic_appender { return *this; }
};

// A formatting argument. Context is a template parameter for the compiled API
// where output can be unbuffered.
template <typename Context> class basic_format_arg {
 private:
  detail::value<Context> value_;
  detail::type type_;

  friend class basic_format_args<Context>;

  using char_type = typename Context::char_type;

 public:
  class handle {
   private:
    detail::custom_value<Context> custom_;

   public:
    explicit handle(detail::custom_value<Context> custom) : custom_(custom) {}

    void format(parse_context<char_type>& parse_ctx, Context& ctx) const {
      custom_.format(custom_.value, parse_ctx, ctx);
    }
  };

  constexpr basic_format_arg() : type_(detail::type::none_type) {}
  basic_format_arg(const detail::named_arg_info<char_type>* args, size_t size)
      : value_(args, size) {}
  template <typename T>
  basic_format_arg(T&& val)
      : value_(val), type_(detail::stored_type_constant<T, Context>::value) {}

  constexpr explicit operator bool() const noexcept {
    return type_ != detail::type::none_type;
  }
  auto type() const -> detail::type { return type_; }

  /**
   * Visits an argument dispatching to the appropriate visit method based on
   * the argument type. For example, if the argument type is `double` then
   * `vis(value)` will be called with the value of type `double`.
   */
  template <typename Visitor>
  FMT_CONSTEXPR FMT_INLINE auto visit(Visitor&& vis) const -> decltype(vis(0)) {
    using detail::map;
    switch (type_) {
    case detail::type::none_type:        break;
    case detail::type::int_type:         return vis(value_.int_value);
    case detail::type::uint_type:        return vis(value_.uint_value);
    case detail::type::long_long_type:   return vis(value_.long_long_value);
    case detail::type::ulong_long_type:  return vis(value_.ulong_long_value);
    case detail::type::int128_type:      return vis(map(value_.int128_value));
    case detail::type::uint128_type:     return vis(map(value_.uint128_value));
    case detail::type::bool_type:        return vis(value_.bool_value);
    case detail::type::char_type:        return vis(value_.char_value);
    case detail::type::float_type:       return vis(value_.float_value);
    case detail::type::double_type:      return vis(value_.double_value);
    case detail::type::long_double_type: return vis(value_.long_double_value);
    case detail::type::cstring_type:     return vis(value_.string.data);
    case detail::type::string_type:      return vis(value_.string.str());
    case detail::type::pointer_type:     return vis(value_.pointer);
    case detail::type::custom_type:      return vis(handle(value_.custom));
    }
    return vis(monostate());
  }

  auto format_custom(const char_type* parse_begin,
                     parse_context<char_type>& parse_ctx, Context& ctx)
      -> bool {
    if (type_ != detail::type::custom_type) return false;
    parse_ctx.advance_to(parse_begin);
    value_.custom.format(value_.custom.value, parse_ctx, ctx);
    return true;
  }
};

/**
 * A view of a collection of formatting arguments. To avoid lifetime issues it
 * should only be used as a parameter type in type-erased functions such as
 * `vformat`:
 *
 *     void vlog(fmt::string_view fmt, fmt::format_args args);  // OK
 *     fmt::format_args args = fmt::make_format_args();  // Dangling reference
 */
template <typename Context> class basic_format_args {
 private:
  // A descriptor that contains information about formatting arguments.
  // If the number of arguments is less or equal to max_packed_args then
  // argument types are passed in the descriptor. This reduces binary code size
  // per formatting function call.
  unsigned long long desc_;
  union {
    // If is_packed() returns true then argument values are stored in values_;
    // otherwise they are stored in args_. This is done to improve cache
    // locality and reduce compiled code size since storing larger objects
    // may require more code (at least on x86-64) even if the same amount of
    // data is actually copied to stack. It saves ~10% on the bloat test.
    const detail::value<Context>* values_;
    const basic_format_arg<Context>* args_;
  };

  constexpr auto is_packed() const -> bool {
    return (desc_ & detail::is_unpacked_bit) == 0;
  }
  constexpr auto has_named_args() const -> bool {
    return (desc_ & detail::has_named_args_bit) != 0;
  }

  FMT_CONSTEXPR auto type(int index) const -> detail::type {
    int shift = index * detail::packed_arg_bits;
    unsigned mask = (1 << detail::packed_arg_bits) - 1;
    return static_cast<detail::type>((desc_ >> shift) & mask);
  }

  template <int NUM_ARGS, int NUM_NAMED_ARGS, unsigned long long DESC>
  using store =
      detail::format_arg_store<Context, NUM_ARGS, NUM_NAMED_ARGS, DESC>;

 public:
  using format_arg = basic_format_arg<Context>;

  constexpr basic_format_args() : desc_(0), args_(nullptr) {}

  /// Constructs a `basic_format_args` object from `format_arg_store`.
  template <int NUM_ARGS, int NUM_NAMED_ARGS, unsigned long long DESC,
            FMT_ENABLE_IF(NUM_ARGS <= detail::max_packed_args)>
  constexpr FMT_ALWAYS_INLINE basic_format_args(
      const store<NUM_ARGS, NUM_NAMED_ARGS, DESC>& s)
      : desc_(DESC | (NUM_NAMED_ARGS != 0 ? +detail::has_named_args_bit : 0)),
        values_(s.args) {}

  template <int NUM_ARGS, int NUM_NAMED_ARGS, unsigned long long DESC,
            FMT_ENABLE_IF(NUM_ARGS > detail::max_packed_args)>
  constexpr basic_format_args(const store<NUM_ARGS, NUM_NAMED_ARGS, DESC>& s)
      : desc_(DESC | (NUM_NAMED_ARGS != 0 ? +detail::has_named_args_bit : 0)),
        args_(s.args) {}

  /// Constructs a `basic_format_args` object from a dynamic list of arguments.
  constexpr basic_format_args(const format_arg* args, int count,
                              bool has_named = false)
      : desc_(detail::is_unpacked_bit | detail::to_unsigned(count) |
              (has_named ? +detail::has_named_args_bit : 0)),
        args_(args) {}

  /// Returns the argument with the specified id.
  FMT_CONSTEXPR auto get(int id) const -> format_arg {
    auto arg = format_arg();
    if (!is_packed()) {
      if (id < max_size()) arg = args_[id];
      return arg;
    }
    if (static_cast<unsigned>(id) >= detail::max_packed_args) return arg;
    arg.type_ = type(id);
    if (arg.type_ != detail::type::none_type) arg.value_ = values_[id];
    return arg;
  }

  template <typename Char>
  auto get(basic_string_view<Char> name) const -> format_arg {
    int id = get_id(name);
    return id >= 0 ? get(id) : format_arg();
  }

  template <typename Char>
  FMT_CONSTEXPR auto get_id(basic_string_view<Char> name) const -> int {
    if (!has_named_args()) return -1;
    const auto& named_args =
        (is_packed() ? values_[-1] : args_[-1].value_).named_args;
    for (size_t i = 0; i < named_args.size; ++i) {
      if (named_args.data[i].name == name) return named_args.data[i].id;
    }
    return -1;
  }

  auto max_size() const -> int {
    unsigned long long max_packed = detail::max_packed_args;
    return static_cast<int>(is_packed() ? max_packed
                                        : desc_ & ~detail::is_unpacked_bit);
  }
};

// A formatting context.
class context {
 private:
  appender out_;
  format_args args_;
  FMT_NO_UNIQUE_ADDRESS detail::locale_ref loc_;

 public:
  /// The character type for the output.
  using char_type = char;

  using iterator = appender;
  using format_arg = basic_format_arg<context>;
  using parse_context_type FMT_DEPRECATED = parse_context<>;
  template <typename T> using formatter_type FMT_DEPRECATED = formatter<T>;
  enum { builtin_types = FMT_BUILTIN_TYPES };

  /// Constructs a `context` object. References to the arguments are stored
  /// in the object so make sure they have appropriate lifetimes.
  FMT_CONSTEXPR context(iterator out, format_args args,
                        detail::locale_ref loc = {})
      : out_(out), args_(args), loc_(loc) {}
  context(context&&) = default;
  context(const context&) = delete;
  void operator=(const context&) = delete;

  FMT_CONSTEXPR auto arg(int id) const -> format_arg { return args_.get(id); }
  inline auto arg(string_view name) const -> format_arg {
    return args_.get(name);
  }
  FMT_CONSTEXPR auto arg_id(string_view name) const -> int {
    return args_.get_id(name);
  }
  auto args() const -> const format_args& { return args_; }

  // Returns an iterator to the beginning of the output range.
  FMT_CONSTEXPR auto out() const -> iterator { return out_; }

  // Advances the begin iterator to `it`.
  FMT_CONSTEXPR void advance_to(iterator) {}

  FMT_CONSTEXPR auto locale() const -> detail::locale_ref { return loc_; }
};

template <typename Char = char> struct runtime_format_string {
  basic_string_view<Char> str;
};

/**
 * Creates a runtime format string.
 *
 * **Example**:
 *
 *     // Check format string at runtime instead of compile-time.
 *     fmt::print(fmt::runtime("{:d}"), "I am not a number");
 */
inline auto runtime(string_view s) -> runtime_format_string<> { return {{s}}; }

/// A compile-time format string. Use `format_string` in the public API to
/// prevent type deduction.
template <typename... T> struct fstring {
 private:
  static constexpr int num_static_named_args =
      detail::count_static_named_args<T...>();

  using checker = detail::format_string_checker<
      char, static_cast<int>(sizeof...(T)), num_static_named_args,
      num_static_named_args != detail::count_named_args<T...>()>;

  using arg_pack = detail::arg_pack<T...>;

 public:
  string_view str;
  using t = fstring;

  // Reports a compile-time error if S is not a valid format string for T.
  template <size_t N>
  FMT_CONSTEVAL FMT_ALWAYS_INLINE fstring(const char (&s)[N]) : str(s, N - 1) {
    using namespace detail;
    static_assert(count<(is_view<remove_cvref_t<T>>::value &&
                         std::is_reference<T>::value)...>() == 0,
                  "passing views as lvalues is disallowed");
    if (FMT_USE_CONSTEVAL) parse_format_string<char>(s, checker(s, arg_pack()));
#ifdef FMT_ENFORCE_COMPILE_STRING
    static_assert(
        FMT_USE_CONSTEVAL && sizeof(s) != 0,
        "FMT_ENFORCE_COMPILE_STRING requires format strings to use FMT_STRING");
#endif
  }
  template <typename S,
            FMT_ENABLE_IF(std::is_convertible<const S&, string_view>::value)>
  FMT_CONSTEVAL FMT_ALWAYS_INLINE fstring(const S& s) : str(s) {
    auto sv = string_view(str);
    if (FMT_USE_CONSTEVAL)
      detail::parse_format_string<char>(sv, checker(sv, arg_pack()));
#ifdef FMT_ENFORCE_COMPILE_STRING
    static_assert(
        FMT_USE_CONSTEVAL && sizeof(s) != 0,
        "FMT_ENFORCE_COMPILE_STRING requires format strings to use FMT_STRING");
#endif
  }
  template <typename S,
            FMT_ENABLE_IF(std::is_base_of<detail::compile_string, S>::value&&
                              std::is_same<typename S::char_type, char>::value)>
  FMT_ALWAYS_INLINE fstring(const S&) : str(S()) {
    FMT_CONSTEXPR auto sv = string_view(S());
    FMT_CONSTEXPR int unused =
        (parse_format_string(sv, checker(sv, arg_pack())), 0);
    detail::ignore_unused(unused);
  }
  fstring(runtime_format_string<> fmt) : str(fmt.str) {}

  // Returning by reference generates better code in debug mode.
  FMT_ALWAYS_INLINE operator const string_view&() const { return str; }
  auto get() const -> string_view { return str; }
};

template <typename... T> using format_string = typename fstring<T...>::t;

template <typename T, typename Char = char>
using is_formattable = bool_constant<!std::is_same<
    detail::mapped_t<conditional_t<std::is_void<T>::value, int*, T>, Char>,
    void>::value>;
#ifdef __cpp_concepts
template <typename T, typename Char = char>
concept formattable = is_formattable<remove_reference_t<T>, Char>::value;
#endif

template <typename T, typename Char>
using has_formatter FMT_DEPRECATED = std::is_constructible<formatter<T, Char>>;

// A formatter specialization for natively supported types.
template <typename T, typename Char>
struct formatter<T, Char,
                 enable_if_t<detail::type_constant<T, Char>::value !=
                             detail::type::custom_type>>
    : detail::native_formatter<T, Char, detail::type_constant<T, Char>::value> {
};

/**
 * Constructs an object that stores references to arguments and can be
 * implicitly converted to `format_args`. `Context` can be omitted in which case
 * it defaults to `context`. See `arg` for lifetime considerations.
 */
// Take arguments by lvalue references to avoid some lifetime issues, e.g.
//   auto args = make_format_args(std::string());
template <typename Context = context, typename... T,
          int NUM_ARGS = sizeof...(T),
          int NUM_NAMED_ARGS = detail::count_named_args<T...>(),
          unsigned long long DESC = detail::make_descriptor<Context, T...>()>
constexpr FMT_ALWAYS_INLINE auto make_format_args(T&... args)
    -> detail::format_arg_store<Context, NUM_ARGS, NUM_NAMED_ARGS, DESC> {
  // Suppress warnings for pathological types convertible to detail::value.
  FMT_PRAGMA_GCC(diagnostic ignored "-Wconversion")
  return {{args...}};
}

template <typename... T>
using vargs =
    detail::format_arg_store<context, sizeof...(T),
                             detail::count_named_args<T...>(),
                             detail::make_descriptor<context, T...>()>;

/**
 * Returns a named argument to be used in a formatting function.
 * It should only be used in a call to a formatting function.
 *
 * **Example**:
 *
 *     fmt::print("The answer is {answer}.", fmt::arg("answer", 42));
 */
template <typename Char, typename T>
inline auto arg(const Char* name, const T& arg) -> detail::named_arg<Char, T> {
  return {name, arg};
}

/// Formats a string and writes the output to `out`.
template <typename OutputIt,
          FMT_ENABLE_IF(detail::is_output_iterator<remove_cvref_t<OutputIt>,
                                                   char>::value)>
auto vformat_to(OutputIt&& out, string_view fmt, format_args args)
    -> remove_cvref_t<OutputIt> {
  auto&& buf = detail::get_buffer<char>(out);
  detail::vformat_to(buf, fmt, args, {});
  return detail::get_iterator(buf, out);
}

/**
 * Formats `args` according to specifications in `fmt`, writes the result to
 * the output iterator `out` and returns the iterator past the end of the output
 * range. `format_to` does not append a terminating null character.
 *
 * **Example**:
 *
 *     auto out = std::vector<char>();
 *     fmt::format_to(std::back_inserter(out), "{}", 42);
 */
template <typename OutputIt, typename... T,
          FMT_ENABLE_IF(detail::is_output_iterator<remove_cvref_t<OutputIt>,
                                                   char>::value)>
FMT_INLINE auto format_to(OutputIt&& out, format_string<T...> fmt, T&&... args)
    -> remove_cvref_t<OutputIt> {
  return vformat_to(out, fmt.str, vargs<T...>{{args...}});
}

template <typename OutputIt> struct format_to_n_result {
  /// Iterator past the end of the output range.
  OutputIt out;
  /// Total (not truncated) output size.
  size_t size;
};

template <typename OutputIt, typename... T,
          FMT_ENABLE_IF(detail::is_output_iterator<OutputIt, char>::value)>
auto vformat_to_n(OutputIt out, size_t n, string_view fmt, format_args args)
    -> format_to_n_result<OutputIt> {
  using traits = detail::fixed_buffer_traits;
  auto buf = detail::iterator_buffer<OutputIt, char, traits>(out, n);
  detail::vformat_to(buf, fmt, args, {});
  return {buf.out(), buf.count()};
}

/**
 * Formats `args` according to specifications in `fmt`, writes up to `n`
 * characters of the result to the output iterator `out` and returns the total
 * (not truncated) output size and the iterator past the end of the output
 * range. `format_to_n` does not append a terminating null character.
 */
template <typename OutputIt, typename... T,
          FMT_ENABLE_IF(detail::is_output_iterator<OutputIt, char>::value)>
FMT_INLINE auto format_to_n(OutputIt out, size_t n, format_string<T...> fmt,
                            T&&... args) -> format_to_n_result<OutputIt> {
  return vformat_to_n(out, n, fmt.str, vargs<T...>{{args...}});
}

struct format_to_result {
  /// Pointer to just after the last successful write in the array.
  char* out;
  /// Specifies if the output was truncated.
  bool truncated;

  FMT_CONSTEXPR operator char*() const {
    // Report truncation to prevent silent data loss.
    if (truncated) report_error("output is truncated");
    return out;
  }
};

template <size_t N>
auto vformat_to(char (&out)[N], string_view fmt, format_args args)
    -> format_to_result {
  auto result = vformat_to_n(out, N, fmt, args);
  return {result.out, result.size > N};
}

template <size_t N, typename... T>
FMT_INLINE auto format_to(char (&out)[N], format_string<T...> fmt, T&&... args)
    -> format_to_result {
  auto result = vformat_to_n(out, N, fmt.str, vargs<T...>{{args...}});
  return {result.out, result.size > N};
}

/// Returns the number of chars in the output of `format(fmt, args...)`.
template <typename... T>
FMT_NODISCARD FMT_INLINE auto formatted_size(format_string<T...> fmt,
                                             T&&... args) -> size_t {
  auto buf = detail::counting_buffer<>();
  detail::vformat_to(buf, fmt.str, vargs<T...>{{args...}}, {});
  return buf.count();
}

FMT_API void vprint(string_view fmt, format_args args);
FMT_API void vprint(FILE* f, string_view fmt, format_args args);
FMT_API void vprintln(FILE* f, string_view fmt, format_args args);
FMT_API void vprint_buffered(FILE* f, string_view fmt, format_args args);

/**
 * Formats `args` according to specifications in `fmt` and writes the output
 * to `stdout`.
 *
 * **Example**:
 *
 *     fmt::print("The answer is {}.", 42);
 */
template <typename... T>
FMT_INLINE void print(format_string<T...> fmt, T&&... args) {
  vargs<T...> va = {{args...}};
  if (detail::const_check(!detail::use_utf8))
    return detail::vprint_mojibake(stdout, fmt.str, va, false);
  return detail::is_locking<T...>() ? vprint_buffered(stdout, fmt.str, va)
                                    : vprint(fmt.str, va);
}

/**
 * Formats `args` according to specifications in `fmt` and writes the
 * output to the file `f`.
 *
 * **Example**:
 *
 *     fmt::print(stderr, "Don't {}!", "panic");
 */
template <typename... T>
FMT_INLINE void print(FILE* f, format_string<T...> fmt, T&&... args) {
  vargs<T...> va = {{args...}};
  if (detail::const_check(!detail::use_utf8))
    return detail::vprint_mojibake(f, fmt.str, va, false);
  return detail::is_locking<T...>() ? vprint_buffered(f, fmt.str, va)
                                    : vprint(f, fmt.str, va);
}

/// Formats `args` according to specifications in `fmt` and writes the output
/// to the file `f` followed by a newline.
template <typename... T>
FMT_INLINE void println(FILE* f, format_string<T...> fmt, T&&... args) {
  vargs<T...> va = {{args...}};
  return detail::const_check(detail::use_utf8)
             ? vprintln(f, fmt.str, va)
             : detail::vprint_mojibake(f, fmt.str, va, true);
}

/// Formats `args` according to specifications in `fmt` and writes the output
/// to `stdout` followed by a newline.
template <typename... T>
FMT_INLINE void println(format_string<T...> fmt, T&&... args) {
  return fmt::println(stdout, fmt, static_cast<T&&>(args)...);
}

FMT_END_EXPORT
FMT_PRAGMA_CLANG(diagnostic pop)
FMT_PRAGMA_GCC(pop_options)
FMT_END_NAMESPACE


/* - fmt/format.h - */
/*
  Formatting library for C++

  Copyright (c) 2012 - present, Victor Zverovich

  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  --- Optional exception to the license ---

  As an exception, if, as a result of your compiling your source code, portions
  of this Software are embedded into a machine-executable object form of such
  source code, you may redistribute such embedded portions in such object form
  without including the above copyright and permission notices.
 */

#ifndef _LIBCPP_REMOVE_TRANSITIVE_INCLUDES
#  define _LIBCPP_REMOVE_TRANSITIVE_INCLUDES
#  define FMT_REMOVE_TRANSITIVE_INCLUDES
#endif

#ifndef FMT_MODULE
#  include <cmath>    // std::signbit
#  include <cstddef>  // std::byte
#  include <cstdint>  // uint32_t
#  include <cstdlib>  // std::malloc, std::free
#  include <cstring>  // std::memcpy
#  include <limits>   // std::numeric_limits
#  include <new>      // std::bad_alloc
/*
#  if defined(__GLIBCXX__) && !defined(_GLIBCXX_USE_DUAL_ABI)
// Workaround for pre gcc 5 libstdc++.
#    include <memory>  // std::allocator_traits
#  endif
*/
#  if FMT_USE_EXCEPTIONS
#    include <stdexcept>     // std::runtime_error
#    include <system_error>  // std::system_error
#  endif
#  if FMT_USE_STD_STRING
#    include <string>        // std::string
#  endif

// Check FMT_CPLUSPLUS to avoid a warning in MSVC.
#  if FMT_HAS_INCLUDE(<bit>) && FMT_CPLUSPLUS > 201703L
#    include <bit>  // std::bit_cast
#  endif

// libc++ supports string_view in pre-c++17.
#  if FMT_HAS_INCLUDE(<string_view>) && \
      (FMT_CPLUSPLUS >= 201703L || defined(_LIBCPP_VERSION))
#    include <string_view>
#    define FMT_USE_STRING_VIEW
#  endif

#  if FMT_MSC_VERSION
#    include <intrin.h>  // _BitScanReverse[64], _umul128
#  endif
#endif  // FMT_MODULE

#if defined(FMT_USE_NONTYPE_TEMPLATE_ARGS)
// Use the provided definition.
#elif defined(__NVCOMPILER)
#  define FMT_USE_NONTYPE_TEMPLATE_ARGS 0
#elif FMT_GCC_VERSION >= 903 && FMT_CPLUSPLUS >= 201709L
#  define FMT_USE_NONTYPE_TEMPLATE_ARGS 1
#elif defined(__cpp_nontype_template_args) && \
    __cpp_nontype_template_args >= 201911L
#  define FMT_USE_NONTYPE_TEMPLATE_ARGS 1
#elif FMT_CLANG_VERSION >= 1200 && FMT_CPLUSPLUS >= 202002L
#  define FMT_USE_NONTYPE_TEMPLATE_ARGS 1
#else
#  define FMT_USE_NONTYPE_TEMPLATE_ARGS 0
#endif

#if defined __cpp_inline_variables && __cpp_inline_variables >= 201606L
#  define FMT_INLINE_VARIABLE inline
#else
#  define FMT_INLINE_VARIABLE
#endif

// Check if RTTI is disabled.
#ifdef FMT_USE_RTTI
// Use the provided definition.
#elif defined(__GXX_RTTI) || FMT_HAS_FEATURE(cxx_rtti) || defined(_CPPRTTI) || \
    defined(__INTEL_RTTI__) || defined(__RTTI)
// __RTTI is for EDG compilers. _CPPRTTI is for MSVC.
#  define FMT_USE_RTTI 1
#else
#  define FMT_USE_RTTI 0
#endif

// Visibility when compiled as a shared library/object.
#if defined(FMT_LIB_EXPORT) || defined(FMT_SHARED)
#  define FMT_SO_VISIBILITY(value) FMT_VISIBILITY(value)
#else
#  define FMT_SO_VISIBILITY(value)
#endif

#if FMT_GCC_VERSION || FMT_CLANG_VERSION
#  define FMT_NOINLINE __attribute__((noinline))
#else
#  define FMT_NOINLINE
#endif

// Detect constexpr std::string.
#if !FMT_USE_CONSTEVAL
#  define FMT_USE_CONSTEXPR_STRING 0
#elif defined(__cpp_lib_constexpr_string) && \
    __cpp_lib_constexpr_string >= 201907L
#  if FMT_CLANG_VERSION && FMT_GLIBCXX_RELEASE
// clang + libstdc++ are able to work only starting with gcc13.3
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=113294
#    if FMT_GLIBCXX_RELEASE < 13
#      define FMT_USE_CONSTEXPR_STRING 0
#    elif FMT_GLIBCXX_RELEASE == 13 && __GLIBCXX__ < 20240521
#      define FMT_USE_CONSTEXPR_STRING 0
#    else
#      define FMT_USE_CONSTEXPR_STRING 1
#    endif
#  else
#    define FMT_USE_CONSTEXPR_STRING 1
#  endif
#else
#  define FMT_USE_CONSTEXPR_STRING 0
#endif
#if FMT_USE_CONSTEXPR_STRING
#  define FMT_CONSTEXPR_STRING constexpr
#else
#  define FMT_CONSTEXPR_STRING
#endif

// GCC 4.9 doesn't support qualified names in specializations.
namespace std {
template <typename T> struct iterator_traits<fmt::basic_appender<T>> {
  using iterator_category = output_iterator_tag;
  using value_type = T;
  using difference_type =
      decltype(static_cast<int*>(nullptr) - static_cast<int*>(nullptr));
  using pointer = void;
  using reference = void;
};
}  // namespace std

#ifndef FMT_THROW
#  if FMT_USE_EXCEPTIONS
#    if FMT_MSC_VERSION || defined(__NVCC__)
FMT_BEGIN_NAMESPACE
namespace detail {
template <typename Exception> inline void do_throw(const Exception& x) {
  // Silence unreachable code warnings in MSVC and NVCC because these
  // are nearly impossible to fix in a generic code.
  volatile bool b = true;
  if (b) throw x;
}
}  // namespace detail
FMT_END_NAMESPACE
#      define FMT_THROW(x) detail::do_throw(x)
#    else
#      define FMT_THROW(x) throw x
#    endif
#  else
#    define FMT_THROW(x) \
      ::fmt::detail::assert_fail(__FILE__, __LINE__, (x).what())
#  endif  // FMT_USE_EXCEPTIONS
#endif    // FMT_THROW

#if FMT_USE_EXCEPTIONS
#  define FMT_THROW_RUNTIME_ERROR(x) FMT_THROW(std::runtime_error(x))
#  define FMT_THROW_SYSTEM_ERROR(e, ...) FMT_THROW(system_error(e, __VA_ARGS__))
#  define FMT_THROW_FORMAT_ERROR(x) FMT_THROW(format_error(x))
#  define FMT_THROW_BAD_ALLOC() FMT_THROW(std::bad_alloc())
#else
#  define FMT_THROW_RUNTIME_ERROR(x) \
    ::fmt::detail::assert_fail(__FILE__, __LINE__, (x))
#  define FMT_THROW_SYSTEM_ERROR(e, x) \
    ::fmt::detail::assert_fail(__FILE__, __LINE__, (x), (e));
#  define FMT_THROW_FORMAT_ERROR(x) \
    ::fmt::detail::assert_fail(__FILE__, __LINE__, (x))
#  define FMT_THROW_BAD_ALLOC() \
    ::fmt::detail::assert_fail(__FILE__, __LINE__, "std::bad_alloc")
#endif

// Defining FMT_REDUCE_INT_INSTANTIATIONS to 1, will reduce the number of
// integer formatter template instantiations to just one by only using the
// largest integer type. This results in a reduction in binary size but will
// cause a decrease in integer formatting performance.
#if !defined(FMT_REDUCE_INT_INSTANTIATIONS)
#  define FMT_REDUCE_INT_INSTANTIATIONS 0
#endif

FMT_BEGIN_NAMESPACE

#if FMT_USE_STD_STRING
template <typename Char, typename Traits = std::char_traits<Char>, typename Allocator = std::allocator<Char>>
using basic_string = std::basic_string<Char, Traits, Allocator>;
using string = std::string;
using wstring = std::wstring;
#else
template <typename Char, typename Traits = void, typename Allocator = void>
struct basic_string : public tesl::BasicStrT<Char> {
  using tesl::BasicStrT<Char>::BasicStrT;
};
using string = fmt::basic_string<char>;
using wstring = fmt::basic_string<wchar_t>;
#endif // FMT_USE_STD_STRING

template <typename Char, typename Traits, typename Allocator>
struct is_contiguous<fmt::basic_string<Char, Traits, Allocator>>
    : std::true_type {};

namespace detail {

// __builtin_clz is broken in clang with Microsoft codegen:
// https://github.com/fmtlib/fmt/issues/519.
#if !FMT_MSC_VERSION
#  if FMT_HAS_BUILTIN(__builtin_clz) || FMT_GCC_VERSION || FMT_ICC_VERSION
#    define FMT_BUILTIN_CLZ(n) __builtin_clz(n)
#  endif
#  if FMT_HAS_BUILTIN(__builtin_clzll) || FMT_GCC_VERSION || FMT_ICC_VERSION
#    define FMT_BUILTIN_CLZLL(n) __builtin_clzll(n)
#  endif
#endif

// Some compilers masquerade as both MSVC and GCC but otherwise support
// __builtin_clz and __builtin_clzll, so only define FMT_BUILTIN_CLZ using the
// MSVC intrinsics if the clz and clzll builtins are not available.
#if FMT_MSC_VERSION && !defined(FMT_BUILTIN_CLZLL)
// Avoid Clang with Microsoft CodeGen's -Wunknown-pragmas warning.
#  ifndef __clang__
#    pragma intrinsic(_BitScanReverse)
#    ifdef _WIN64
#      pragma intrinsic(_BitScanReverse64)
#    endif
#  endif

inline auto clz(uint32_t x) -> int {
  FMT_ASSERT(x != 0, "");
  FMT_MSC_WARNING(suppress : 6102)  // Suppress a bogus static analysis warning.
  unsigned long r = 0;
  _BitScanReverse(&r, x);
  return 31 ^ static_cast<int>(r);
}
#  define FMT_BUILTIN_CLZ(n) detail::clz(n)

inline auto clzll(uint64_t x) -> int {
  FMT_ASSERT(x != 0, "");
  FMT_MSC_WARNING(suppress : 6102)  // Suppress a bogus static analysis warning.
  unsigned long r = 0;
#  ifdef _WIN64
  _BitScanReverse64(&r, x);
#  else
  // Scan the high 32 bits.
  if (_BitScanReverse(&r, static_cast<uint32_t>(x >> 32)))
    return 63 ^ static_cast<int>(r + 32);
  // Scan the low 32 bits.
  _BitScanReverse(&r, static_cast<uint32_t>(x));
#  endif
  return 63 ^ static_cast<int>(r);
}
#  define FMT_BUILTIN_CLZLL(n) detail::clzll(n)
#endif  // FMT_MSC_VERSION && !defined(FMT_BUILTIN_CLZLL)

FMT_CONSTEXPR inline void abort_fuzzing_if(bool condition) {
  ignore_unused(condition);
#ifdef FMT_FUZZ
  if (condition) throw std::runtime_error("fuzzing limit reached");
#endif
}

#if defined(FMT_USE_STRING_VIEW)
template <typename Char> using std_string_view = std::basic_string_view<Char>;
#else
template <typename Char> struct std_string_view {
  operator basic_string_view<Char>() const;
};
#endif

template <typename Char, Char... C> struct string_literal {
  static constexpr Char value[sizeof...(C)] = {C...};
  constexpr operator basic_string_view<Char>() const {
    return {value, sizeof...(C)};
  }
};
#if FMT_CPLUSPLUS < 201703L
template <typename Char, Char... C>
constexpr Char string_literal<Char, C...>::value[sizeof...(C)];
#endif

// Implementation of std::bit_cast for pre-C++20.
template <typename To, typename From, FMT_ENABLE_IF(sizeof(To) == sizeof(From))>
FMT_CONSTEXPR20 auto bit_cast(const From& from) -> To {
#ifdef __cpp_lib_bit_cast
  if (is_constant_evaluated()) return std::bit_cast<To>(from);
#endif
  auto to = To();
  // The cast suppresses a bogus -Wclass-memaccess on GCC.
  std::memcpy(static_cast<void*>(&to), &from, sizeof(to));
  return to;
}

inline auto is_big_endian() -> bool {
#ifdef _WIN32
  return false;
#elif defined(__BIG_ENDIAN__)
  return true;
#elif defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__)
  return __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__;
#else
  struct bytes {
    char data[sizeof(int)];
  };
  return bit_cast<bytes>(1).data[0] == 0;
#endif
}

class uint128_fallback {
 private:
  uint64_t lo_, hi_;

 public:
  constexpr uint128_fallback(uint64_t hi, uint64_t lo) : lo_(lo), hi_(hi) {}
  constexpr uint128_fallback(uint64_t value = 0) : lo_(value), hi_(0) {}

  constexpr auto high() const noexcept -> uint64_t { return hi_; }
  constexpr auto low() const noexcept -> uint64_t { return lo_; }

  template <typename T, FMT_ENABLE_IF(std::is_integral<T>::value)>
  constexpr explicit operator T() const {
    return static_cast<T>(lo_);
  }

  friend constexpr auto operator==(const uint128_fallback& lhs,
                                   const uint128_fallback& rhs) -> bool {
    return lhs.hi_ == rhs.hi_ && lhs.lo_ == rhs.lo_;
  }
  friend constexpr auto operator!=(const uint128_fallback& lhs,
                                   const uint128_fallback& rhs) -> bool {
    return !(lhs == rhs);
  }
  friend constexpr auto operator>(const uint128_fallback& lhs,
                                  const uint128_fallback& rhs) -> bool {
    return lhs.hi_ != rhs.hi_ ? lhs.hi_ > rhs.hi_ : lhs.lo_ > rhs.lo_;
  }
  friend constexpr auto operator|(const uint128_fallback& lhs,
                                  const uint128_fallback& rhs)
      -> uint128_fallback {
    return {lhs.hi_ | rhs.hi_, lhs.lo_ | rhs.lo_};
  }
  friend constexpr auto operator&(const uint128_fallback& lhs,
                                  const uint128_fallback& rhs)
      -> uint128_fallback {
    return {lhs.hi_ & rhs.hi_, lhs.lo_ & rhs.lo_};
  }
  friend constexpr auto operator~(const uint128_fallback& n)
      -> uint128_fallback {
    return {~n.hi_, ~n.lo_};
  }
  friend FMT_CONSTEXPR auto operator+(const uint128_fallback& lhs,
                                      const uint128_fallback& rhs)
      -> uint128_fallback {
    auto result = uint128_fallback(lhs);
    result += rhs;
    return result;
  }
  friend FMT_CONSTEXPR auto operator*(const uint128_fallback& lhs, uint32_t rhs)
      -> uint128_fallback {
    FMT_ASSERT(lhs.hi_ == 0, "");
    uint64_t hi = (lhs.lo_ >> 32) * rhs;
    uint64_t lo = (lhs.lo_ & ~uint32_t()) * rhs;
    uint64_t new_lo = (hi << 32) + lo;
    return {(hi >> 32) + (new_lo < lo ? 1 : 0), new_lo};
  }
  friend constexpr auto operator-(const uint128_fallback& lhs, uint64_t rhs)
      -> uint128_fallback {
    return {lhs.hi_ - (lhs.lo_ < rhs ? 1 : 0), lhs.lo_ - rhs};
  }
  FMT_CONSTEXPR auto operator>>(int shift) const -> uint128_fallback {
    if (shift == 64) return {0, hi_};
    if (shift > 64) return uint128_fallback(0, hi_) >> (shift - 64);
    return {hi_ >> shift, (hi_ << (64 - shift)) | (lo_ >> shift)};
  }
  FMT_CONSTEXPR auto operator<<(int shift) const -> uint128_fallback {
    if (shift == 64) return {lo_, 0};
    if (shift > 64) return uint128_fallback(lo_, 0) << (shift - 64);
    return {hi_ << shift | (lo_ >> (64 - shift)), (lo_ << shift)};
  }
  FMT_CONSTEXPR auto operator>>=(int shift) -> uint128_fallback& {
    return *this = *this >> shift;
  }
  FMT_CONSTEXPR void operator+=(uint128_fallback n) {
    uint64_t new_lo = lo_ + n.lo_;
    uint64_t new_hi = hi_ + n.hi_ + (new_lo < lo_ ? 1 : 0);
    FMT_ASSERT(new_hi >= hi_, "");
    lo_ = new_lo;
    hi_ = new_hi;
  }
  FMT_CONSTEXPR void operator&=(uint128_fallback n) {
    lo_ &= n.lo_;
    hi_ &= n.hi_;
  }

  FMT_CONSTEXPR20 auto operator+=(uint64_t n) noexcept -> uint128_fallback& {
    if (is_constant_evaluated()) {
      lo_ += n;
      hi_ += (lo_ < n ? 1 : 0);
      return *this;
    }
#if FMT_HAS_BUILTIN(__builtin_addcll) && !defined(__ibmxl__)
    unsigned long long carry;
    lo_ = __builtin_addcll(lo_, n, 0, &carry);
    hi_ += carry;
#elif FMT_HAS_BUILTIN(__builtin_ia32_addcarryx_u64) && !defined(__ibmxl__)
    unsigned long long result;
    auto carry = __builtin_ia32_addcarryx_u64(0, lo_, n, &result);
    lo_ = result;
    hi_ += carry;
#elif defined(_MSC_VER) && defined(_M_X64)
    auto carry = _addcarry_u64(0, lo_, n, &lo_);
    _addcarry_u64(carry, hi_, 0, &hi_);
#else
    lo_ += n;
    hi_ += (lo_ < n ? 1 : 0);
#endif
    return *this;
  }
};

using uint128_t = conditional_t<FMT_USE_INT128, uint128_opt, uint128_fallback>;

#ifdef UINTPTR_MAX
using uintptr_t = ::uintptr_t;
#else
using uintptr_t = uint128_t;
#endif

// Returns the largest possible value for type T. Same as
// std::numeric_limits<T>::max() but shorter and not affected by the max macro.
template <typename T> constexpr auto max_value() -> T {
  return (std::numeric_limits<T>::max)();
}
template <typename T> constexpr auto num_bits() -> int {
  return std::numeric_limits<T>::digits;
}
// std::numeric_limits<T>::digits may return 0 for 128-bit ints.
template <> constexpr auto num_bits<int128_opt>() -> int { return 128; }
template <> constexpr auto num_bits<uint128_opt>() -> int { return 128; }
template <> constexpr auto num_bits<uint128_fallback>() -> int { return 128; }

// A heterogeneous bit_cast used for converting 96-bit long double to uint128_t
// and 128-bit pointers to uint128_fallback.
template <typename To, typename From, FMT_ENABLE_IF(sizeof(To) > sizeof(From))>
inline auto bit_cast(const From& from) -> To {
  constexpr auto size = static_cast<int>(sizeof(From) / sizeof(unsigned short));
  struct data_t {
    unsigned short value[static_cast<unsigned>(size)];
  } data = bit_cast<data_t>(from);
  auto result = To();
  if (const_check(is_big_endian())) {
    for (int i = 0; i < size; ++i)
      result = (result << num_bits<unsigned short>()) | data.value[i];
  } else {
    for (int i = size - 1; i >= 0; --i)
      result = (result << num_bits<unsigned short>()) | data.value[i];
  }
  return result;
}

template <typename UInt>
FMT_CONSTEXPR20 inline auto countl_zero_fallback(UInt n) -> int {
  int lz = 0;
  constexpr UInt msb_mask = static_cast<UInt>(1) << (num_bits<UInt>() - 1);
  for (; (n & msb_mask) == 0; n <<= 1) lz++;
  return lz;
}

FMT_CONSTEXPR20 inline auto countl_zero(uint32_t n) -> int {
#ifdef FMT_BUILTIN_CLZ
  if (!is_constant_evaluated()) return FMT_BUILTIN_CLZ(n);
#endif
  return countl_zero_fallback(n);
}

FMT_CONSTEXPR20 inline auto countl_zero(uint64_t n) -> int {
#ifdef FMT_BUILTIN_CLZLL
  if (!is_constant_evaluated()) return FMT_BUILTIN_CLZLL(n);
#endif
  return countl_zero_fallback(n);
}

FMT_INLINE void assume(bool condition) {
  (void)condition;
#if FMT_HAS_BUILTIN(__builtin_assume) && !FMT_ICC_VERSION
  __builtin_assume(condition);
#elif FMT_GCC_VERSION
  if (!condition) __builtin_unreachable();
#endif
}

// Attempts to reserve space for n extra characters in the output range.
// Returns a pointer to the reserved range or a reference to it.
template <typename OutputIt,
          FMT_ENABLE_IF(is_back_insert_iterator<OutputIt>::value&&
                            is_contiguous<typename OutputIt::container>::value)>
#if FMT_CLANG_VERSION >= 307 && !FMT_ICC_VERSION
__attribute__((no_sanitize("undefined")))
#endif
FMT_CONSTEXPR20 inline auto
reserve(OutputIt it, size_t n) -> typename OutputIt::value_type* {
  auto& c = get_container(it);
  size_t size = c.size();
  c.resize(size + n);
  return &c[size];
}

template <typename T>
FMT_CONSTEXPR20 inline auto reserve(basic_appender<T> it, size_t n)
    -> basic_appender<T> {
  buffer<T>& buf = get_container(it);
  buf.try_reserve(buf.size() + n);
  return it;
}

template <typename Iterator>
constexpr auto reserve(Iterator& it, size_t) -> Iterator& {
  return it;
}

template <typename OutputIt>
using reserve_iterator =
    remove_reference_t<decltype(reserve(std::declval<OutputIt&>(), 0))>;

template <typename T, typename OutputIt>
constexpr auto to_pointer(OutputIt, size_t) -> T* {
  return nullptr;
}
template <typename T>
FMT_CONSTEXPR20 auto to_pointer(basic_appender<T> it, size_t n) -> T* {
  buffer<T>& buf = get_container(it);
  buf.try_reserve(buf.size() + n);
  auto size = buf.size();
  if (buf.capacity() < size + n) return nullptr;
  buf.try_resize(size + n);
  return buf.data() + size;
}

template <typename OutputIt,
          FMT_ENABLE_IF(is_back_insert_iterator<OutputIt>::value&&
                            is_contiguous<typename OutputIt::container>::value)>
inline auto base_iterator(OutputIt it,
                          typename OutputIt::container_type::value_type*)
    -> OutputIt {
  return it;
}

template <typename Iterator>
constexpr auto base_iterator(Iterator, Iterator it) -> Iterator {
  return it;
}

// <algorithm> is spectacularly slow to compile in C++20 so use a simple fill_n
// instead (#1998).
template <typename OutputIt, typename Size, typename T>
FMT_CONSTEXPR auto fill_n(OutputIt out, Size count, const T& value)
    -> OutputIt {
  for (Size i = 0; i < count; ++i) *out++ = value;
  return out;
}
template <typename T, typename Size>
FMT_CONSTEXPR20 auto fill_n(T* out, Size count, char value) -> T* {
  if (is_constant_evaluated()) return fill_n<T*, Size, T>(out, count, value);
  static_assert(sizeof(T) == 1,
                "sizeof(T) must be 1 to use char for initialization");
  std::memset(out, value, to_unsigned(count));
  return out + count;
}

template <typename OutChar, typename InputIt, typename OutputIt>
FMT_CONSTEXPR FMT_NOINLINE auto copy_noinline(InputIt begin, InputIt end,
                                              OutputIt out) -> OutputIt {
  return copy<OutChar>(begin, end, out);
}

// A public domain branchless UTF-8 decoder by Christopher Wellons:
// https://github.com/skeeto/branchless-utf8
/* Decode the next character, c, from s, reporting errors in e.
 *
 * Since this is a branchless decoder, four bytes will be read from the
 * buffer regardless of the actual length of the next character. This
 * means the buffer _must_ have at least three bytes of zero padding
 * following the end of the data stream.
 *
 * Errors are reported in e, which will be non-zero if the parsed
 * character was somehow invalid: invalid byte sequence, non-canonical
 * encoding, or a surrogate half.
 *
 * The function returns a pointer to the next character. When an error
 * occurs, this pointer will be a guess that depends on the particular
 * error, but it will always advance at least one byte.
 */
FMT_CONSTEXPR inline auto utf8_decode(const char* s, uint32_t* c, int* e)
    -> const char* {
  constexpr int masks[] = {0x00, 0x7f, 0x1f, 0x0f, 0x07};
  constexpr uint32_t mins[] = {4194304, 0, 128, 2048, 65536};
  constexpr int shiftc[] = {0, 18, 12, 6, 0};
  constexpr int shifte[] = {0, 6, 4, 2, 0};

  int len = "\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\0\0\0\0\0\0\0\0\2\2\2\2\3\3\4"
      [static_cast<unsigned char>(*s) >> 3];
  // Compute the pointer to the next character early so that the next
  // iteration can start working on the next character. Neither Clang
  // nor GCC figure out this reordering on their own.
  const char* next = s + len + !len;

  using uchar = unsigned char;

  // Assume a four-byte character and load four bytes. Unused bits are
  // shifted out.
  *c = uint32_t(uchar(s[0]) & masks[len]) << 18;
  *c |= uint32_t(uchar(s[1]) & 0x3f) << 12;
  *c |= uint32_t(uchar(s[2]) & 0x3f) << 6;
  *c |= uint32_t(uchar(s[3]) & 0x3f) << 0;
  *c >>= shiftc[len];

  // Accumulate the various error conditions.
  *e = (*c < mins[len]) << 6;       // non-canonical encoding
  *e |= ((*c >> 11) == 0x1b) << 7;  // surrogate half?
  *e |= (*c > 0x10FFFF) << 8;       // out of range?
  *e |= (uchar(s[1]) & 0xc0) >> 2;
  *e |= (uchar(s[2]) & 0xc0) >> 4;
  *e |= uchar(s[3]) >> 6;
  *e ^= 0x2a;  // top two bits of each tail byte correct?
  *e >>= shifte[len];

  return next;
}

constexpr FMT_INLINE_VARIABLE uint32_t invalid_code_point = ~uint32_t();

// Invokes f(cp, sv) for every code point cp in s with sv being the string view
// corresponding to the code point. cp is invalid_code_point on error.
template <typename F>
FMT_CONSTEXPR void for_each_codepoint(string_view s, F f) {
  auto decode = [f](const char* buf_ptr, const char* ptr) {
    auto cp = uint32_t();
    auto error = 0;
    auto end = utf8_decode(buf_ptr, &cp, &error);
    bool result = f(error ? invalid_code_point : cp,
                    string_view(ptr, error ? 1 : to_unsigned(end - buf_ptr)));
    return result ? (error ? buf_ptr + 1 : end) : nullptr;
  };

  auto p = s.data();
  const size_t block_size = 4;  // utf8_decode always reads blocks of 4 chars.
  if (s.size() >= block_size) {
    for (auto end = p + s.size() - block_size + 1; p < end;) {
      p = decode(p, p);
      if (!p) return;
    }
  }
  auto num_chars_left = to_unsigned(s.data() + s.size() - p);
  if (num_chars_left == 0) return;

  // Suppress bogus -Wstringop-overflow.
  if (FMT_GCC_VERSION) num_chars_left &= 3;
  char buf[2 * block_size - 1] = {};
  copy<char>(p, p + num_chars_left, buf);
  const char* buf_ptr = buf;
  do {
    auto end = decode(buf_ptr, p);
    if (!end) return;
    p += end - buf_ptr;
    buf_ptr = end;
  } while (buf_ptr < buf + num_chars_left);
}

FMT_CONSTEXPR inline auto display_width_of(uint32_t cp) noexcept -> size_t {
  return to_unsigned(
      1 + (cp >= 0x1100 &&
           (cp <= 0x115f ||  // Hangul Jamo init. consonants
            cp == 0x2329 ||  // LEFT-POINTING ANGLE BRACKET
            cp == 0x232a ||  // RIGHT-POINTING ANGLE BRACKET
            // CJK ... Yi except IDEOGRAPHIC HALF FILL SPACE:
            (cp >= 0x2e80 && cp <= 0xa4cf && cp != 0x303f) ||
            (cp >= 0xac00 && cp <= 0xd7a3) ||    // Hangul Syllables
            (cp >= 0xf900 && cp <= 0xfaff) ||    // CJK Compatibility Ideographs
            (cp >= 0xfe10 && cp <= 0xfe19) ||    // Vertical Forms
            (cp >= 0xfe30 && cp <= 0xfe6f) ||    // CJK Compatibility Forms
            (cp >= 0xff00 && cp <= 0xff60) ||    // Fullwidth Forms
            (cp >= 0xffe0 && cp <= 0xffe6) ||    // Fullwidth Forms
            (cp >= 0x20000 && cp <= 0x2fffd) ||  // CJK
            (cp >= 0x30000 && cp <= 0x3fffd) ||
            // Miscellaneous Symbols and Pictographs + Emoticons:
            (cp >= 0x1f300 && cp <= 0x1f64f) ||
            // Supplemental Symbols and Pictographs:
            (cp >= 0x1f900 && cp <= 0x1f9ff))));
}

template <typename T> struct is_integral : std::is_integral<T> {};
template <> struct is_integral<int128_opt> : std::true_type {};
template <> struct is_integral<uint128_t> : std::true_type {};

template <typename T>
using is_signed =
    std::integral_constant<bool, std::numeric_limits<T>::is_signed ||
                                     std::is_same<T, int128_opt>::value>;

template <typename T>
using is_integer =
    bool_constant<is_integral<T>::value && !std::is_same<T, bool>::value &&
                  !std::is_same<T, char>::value &&
                  !std::is_same<T, wchar_t>::value>;

#if defined(FMT_USE_FLOAT128)
// Use the provided definition.
#elif FMT_CLANG_VERSION >= 309 && FMT_HAS_INCLUDE(<quadmath.h>)
#  define FMT_USE_FLOAT128 1
#elif FMT_GCC_VERSION && defined(_GLIBCXX_USE_FLOAT128) && \
    !defined(__STRICT_ANSI__)
#  define FMT_USE_FLOAT128 1
#else
#  define FMT_USE_FLOAT128 0
#endif
#if FMT_USE_FLOAT128
using float128 = __float128;
#else
struct float128 {};
#endif

template <typename T> using is_float128 = std::is_same<T, float128>;

template <typename T> struct is_floating_point : std::is_floating_point<T> {};
template <> struct is_floating_point<float128> : std::true_type {};

template <typename T, bool = is_floating_point<T>::value>
struct is_fast_float : bool_constant<std::numeric_limits<T>::is_iec559 &&
                                     sizeof(T) <= sizeof(double)> {};
template <typename T> struct is_fast_float<T, false> : std::false_type {};

template <typename T>
using fast_float_t = conditional_t<sizeof(T) == sizeof(double), double, float>;

template <typename T>
using is_double_double = bool_constant<std::numeric_limits<T>::digits == 106>;

#ifndef FMT_USE_FULL_CACHE_DRAGONBOX
#  define FMT_USE_FULL_CACHE_DRAGONBOX 0
#endif

// An allocator that uses malloc/free to allow removing dependency on the C++
// standard libary runtime. std::decay is used for back_inserter to be found by
// ADL when applied to memory_buffer.
template <typename T> struct allocator : private std::decay<void> {
  using value_type = T;

  T* allocate(size_t n) {
    FMT_ASSERT(n <= max_value<size_t>() / sizeof(T), "");
    T* p = static_cast<T*>(std::malloc(n * sizeof(T)));
    if (!p) FMT_THROW_BAD_ALLOC();
    return p;
  }

  void deallocate(T* p, size_t) { std::free(p); }
};

}  // namespace detail

FMT_BEGIN_EXPORT

// The number of characters to store in the basic_memory_buffer object itself
// to avoid dynamic memory allocation.
enum { inline_buffer_size = 500 };

/**
 * A dynamically growing memory buffer for trivially copyable/constructible
 * types with the first `SIZE` elements stored in the object itself. Most
 * commonly used via the `memory_buffer` alias for `char`.
 *
 * **Example**:
 *
 *     auto out = fmt::memory_buffer();
 *     fmt::format_to(std::back_inserter(out), "The answer is {}.", 42);
 *
 * This will append "The answer is 42." to `out`. The buffer content can be
 * converted to `fmt::string` with `to_string(out)`.
 */
template <typename T, size_t SIZE = inline_buffer_size,
          typename Allocator = detail::allocator<T>>
class basic_memory_buffer : public detail::buffer<T> {
 private:
  T store_[SIZE];

  // Don't inherit from Allocator to avoid generating type_info for it.
  FMT_NO_UNIQUE_ADDRESS Allocator alloc_;

  // Deallocate memory allocated by the buffer.
  FMT_CONSTEXPR20 void deallocate() {
    T* data = this->data();
    if (data != store_) alloc_.deallocate(data, this->capacity());
  }

  static FMT_CONSTEXPR20 void grow(detail::buffer<T>& buf, size_t size) {
    detail::abort_fuzzing_if(size > 5000);
    auto& self = static_cast<basic_memory_buffer&>(buf);
    //const size_t max_size =
    //    std::allocator_traits<Allocator>::max_size(self.alloc_);
    size_t old_capacity = buf.capacity();
    size_t new_capacity = old_capacity + old_capacity / 2;
    if (size > new_capacity)
      new_capacity = size;
    //else if (new_capacity > max_size)
    //  new_capacity = max_of(size, max_size);
    T* old_data = buf.data();
    T* new_data = self.alloc_.allocate(new_capacity);
    // Suppress a bogus -Wstringop-overflow in gcc 13.1 (#3481).
    detail::assume(buf.size() <= new_capacity);
    // The following code doesn't throw, so the raw pointer above doesn't leak.
    memcpy(new_data, old_data, buf.size() * sizeof(T));
    self.set(new_data, new_capacity);
    // deallocate must not throw according to the standard, but even if it does,
    // the buffer already uses the new storage and will deallocate it in
    // destructor.
    if (old_data != self.store_) self.alloc_.deallocate(old_data, old_capacity);
  }

 public:
  using value_type = T;
  using const_reference = const T&;

  FMT_CONSTEXPR explicit basic_memory_buffer(
      const Allocator& alloc = Allocator())
      : detail::buffer<T>(grow), alloc_(alloc) {
    this->set(store_, SIZE);
    if (detail::is_constant_evaluated()) detail::fill_n(store_, SIZE, T());
  }
  FMT_CONSTEXPR20 ~basic_memory_buffer() { deallocate(); }

 private:
  // Move data from other to this buffer.
  FMT_CONSTEXPR20 void move(basic_memory_buffer& other) {
    alloc_ = std::move(other.alloc_);
    T* data = other.data();
    size_t size = other.size(), capacity = other.capacity();
    if (data == other.store_) {
      this->set(store_, capacity);
      detail::copy<T>(other.store_, other.store_ + size, store_);
    } else {
      this->set(data, capacity);
      // Set pointer to the inline array so that delete is not called
      // when deallocating.
      other.set(other.store_, 0);
      other.clear();
    }
    this->resize(size);
  }

 public:
  /// Constructs a `basic_memory_buffer` object moving the content of the other
  /// object to it.
  FMT_CONSTEXPR20 basic_memory_buffer(basic_memory_buffer&& other) noexcept
      : detail::buffer<T>(grow) {
    move(other);
  }

  /// Moves the content of the other `basic_memory_buffer` object to this one.
  auto operator=(basic_memory_buffer&& other) noexcept -> basic_memory_buffer& {
    FMT_ASSERT(this != &other, "");
    deallocate();
    move(other);
    return *this;
  }

  // Returns a copy of the allocator associated with this buffer.
  auto get_allocator() const -> Allocator { return alloc_; }

  /// Resizes the buffer to contain `count` elements. If T is a POD type new
  /// elements may not be initialized.
  FMT_CONSTEXPR void resize(size_t count) { this->try_resize(count); }

  /// Increases the buffer capacity to `new_capacity`.
  void reserve(size_t new_capacity) { this->try_reserve(new_capacity); }

  using detail::buffer<T>::append;
  template <typename ContiguousRange>
  FMT_CONSTEXPR20 void append(const ContiguousRange& range) {
    append(range.data(), range.data() + range.size());
  }
};

using memory_buffer = basic_memory_buffer<char>;

template <size_t SIZE>
FMT_NODISCARD auto to_string(const basic_memory_buffer<char, SIZE>& buf)
    -> fmt::string {
  auto size = buf.size();
  detail::assume(size < fmt::string().max_size());
  return {buf.data(), size};
}

// A writer to a buffered stream. It doesn't own the underlying stream.
class writer {
 private:
  detail::buffer<char>* buf_;

  // We cannot create a file buffer in advance because any write to a FILE may
  // invalidate it.
  FILE* file_;

 public:
  inline writer(FILE* f) : buf_(nullptr), file_(f) {}
  inline writer(detail::buffer<char>& buf) : buf_(&buf) {}

  /// Formats `args` according to specifications in `fmt` and writes the
  /// output to the file.
  template <typename... T> void print(format_string<T...> fmt, T&&... args) {
    if (buf_)
      fmt::format_to(appender(*buf_), fmt, std::forward<T>(args)...);
    else
      fmt::print(file_, fmt, std::forward<T>(args)...);
  }
};

class string_buffer {
 private:
  fmt::string str_;
  detail::container_buffer<fmt::string> buf_;

 public:
  inline string_buffer() : buf_(str_) {}

  inline operator writer() { return buf_; }
  inline fmt::string& str() { return str_; }
};

template <typename T, size_t SIZE, typename Allocator>
struct is_contiguous<basic_memory_buffer<T, SIZE, Allocator>> : std::true_type {
};

// Suppress a misleading warning in older versions of clang.
FMT_PRAGMA_CLANG(diagnostic ignored "-Wweak-vtables")

#if FMT_USE_EXCEPTIONS
/// An error reported from a formatting function.
class FMT_SO_VISIBILITY("default") format_error : public std::runtime_error {
 public:
  using std::runtime_error::runtime_error;
};
#endif

class loc_value;

FMT_END_EXPORT
namespace detail {
FMT_API auto write_console(int fd, string_view text) -> bool;
FMT_API void print(FILE*, string_view);
}  // namespace detail

namespace detail {
template <typename Char, size_t N> struct fixed_string {
  FMT_CONSTEXPR20 fixed_string(const Char (&s)[N]) {
    detail::copy<Char, const Char*, Char*>(static_cast<const Char*>(s), s + N,
                                           data);
  }
  Char data[N] = {};
};

// Converts a compile-time string to basic_string_view.
FMT_EXPORT template <typename Char, size_t N>
constexpr auto compile_string_to_view(const Char (&s)[N])
    -> basic_string_view<Char> {
  // Remove trailing NUL character if needed. Won't be present if this is used
  // with a raw character array (i.e. not defined as a string).
  return {s, N - (std::char_traits<Char>::to_int_type(s[N - 1]) == 0 ? 1 : 0)};
}
FMT_EXPORT template <typename Char>
constexpr auto compile_string_to_view(basic_string_view<Char> s)
    -> basic_string_view<Char> {
  return s;
}

// Returns true if value is negative, false otherwise.
// Same as `value < 0` but doesn't produce warnings if T is an unsigned type.
template <typename T, FMT_ENABLE_IF(is_signed<T>::value)>
constexpr auto is_negative(T value) -> bool {
  return value < 0;
}
template <typename T, FMT_ENABLE_IF(!is_signed<T>::value)>
constexpr auto is_negative(T) -> bool {
  return false;
}

// Smallest of uint32_t, uint64_t, uint128_t that is large enough to
// represent all values of an integral type T.
template <typename T>
using uint32_or_64_or_128_t =
    conditional_t<num_bits<T>() <= 32 && !FMT_REDUCE_INT_INSTANTIATIONS,
                  uint32_t,
                  conditional_t<num_bits<T>() <= 64, uint64_t, uint128_t>>;
template <typename T>
using uint64_or_128_t = conditional_t<num_bits<T>() <= 64, uint64_t, uint128_t>;

#define FMT_POWERS_OF_10(factor)                                  \
  factor * 10, (factor) * 100, (factor) * 1000, (factor) * 10000, \
      (factor) * 100000, (factor) * 1000000, (factor) * 10000000, \
      (factor) * 100000000, (factor) * 1000000000

// Converts value in the range [0, 100) to a string.
// GCC generates slightly better code when value is pointer-size.
inline auto digits2(size_t value) -> const char* {
  // Align data since unaligned access may be slower when crossing a
  // hardware-specific boundary.
  alignas(2) static const char data[] =
      "0001020304050607080910111213141516171819"
      "2021222324252627282930313233343536373839"
      "4041424344454647484950515253545556575859"
      "6061626364656667686970717273747576777879"
      "8081828384858687888990919293949596979899";
  return &data[value * 2];
}

template <typename Char> constexpr auto getsign(sign s) -> Char {
  return static_cast<char>(((' ' << 24) | ('+' << 16) | ('-' << 8)) >>
                           (static_cast<int>(s) * 8));
}

template <typename T> FMT_CONSTEXPR auto count_digits_fallback(T n) -> int {
  int count = 1;
  for (;;) {
    // Integer division is slow so do it for a group of four digits instead
    // of for every digit. The idea comes from the talk by Alexandrescu
    // "Three Optimization Tips for C++". See speed-test for a comparison.
    if (n < 10) return count;
    if (n < 100) return count + 1;
    if (n < 1000) return count + 2;
    if (n < 10000) return count + 3;
    n /= 10000u;
    count += 4;
  }
}
#if FMT_USE_INT128
FMT_CONSTEXPR inline auto count_digits(uint128_opt n) -> int {
  return count_digits_fallback(n);
}
#endif

#ifdef FMT_BUILTIN_CLZLL
// It is a separate function rather than a part of count_digits to workaround
// the lack of static constexpr in constexpr functions.
inline auto do_count_digits(uint64_t n) -> int {
  // This has comparable performance to the version by Kendall Willets
  // (https://github.com/fmtlib/format-benchmark/blob/master/digits10)
  // but uses smaller tables.
  // Maps bsr(n) to ceil(log10(pow(2, bsr(n) + 1) - 1)).
  static constexpr uint8_t bsr2log10[] = {
      1,  1,  1,  2,  2,  2,  3,  3,  3,  4,  4,  4,  4,  5,  5,  5,
      6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  9,  9,  9,  10, 10, 10,
      10, 11, 11, 11, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 15, 15,
      15, 16, 16, 16, 16, 17, 17, 17, 18, 18, 18, 19, 19, 19, 19, 20};
  auto t = bsr2log10[FMT_BUILTIN_CLZLL(n | 1) ^ 63];
  static constexpr uint64_t zero_or_powers_of_10[] = {
      0, 0, FMT_POWERS_OF_10(1U), FMT_POWERS_OF_10(1000000000ULL),
      10000000000000000000ULL};
  return t - (n < zero_or_powers_of_10[t]);
}
#endif

// Returns the number of decimal digits in n. Leading zeros are not counted
// except for n == 0 in which case count_digits returns 1.
FMT_CONSTEXPR20 inline auto count_digits(uint64_t n) -> int {
#ifdef FMT_BUILTIN_CLZLL
  if (!is_constant_evaluated() && !FMT_OPTIMIZE_SIZE) return do_count_digits(n);
#endif
  return count_digits_fallback(n);
}

// Counts the number of digits in n. BITS = log2(radix).
template <int BITS, typename UInt>
FMT_CONSTEXPR auto count_digits(UInt n) -> int {
#ifdef FMT_BUILTIN_CLZ
  if (!is_constant_evaluated() && num_bits<UInt>() == 32)
    return (FMT_BUILTIN_CLZ(static_cast<uint32_t>(n) | 1) ^ 31) / BITS + 1;
#endif
  // Lambda avoids unreachable code warnings from NVHPC.
  return [](UInt m) {
    int num_digits = 0;
    do {
      ++num_digits;
    } while ((m >>= BITS) != 0);
    return num_digits;
  }(n);
}

#ifdef FMT_BUILTIN_CLZ
// It is a separate function rather than a part of count_digits to workaround
// the lack of static constexpr in constexpr functions.
FMT_INLINE auto do_count_digits(uint32_t n) -> int {
// An optimization by Kendall Willets from https://bit.ly/3uOIQrB.
// This increments the upper 32 bits (log10(T) - 1) when >= T is added.
#  define FMT_INC(T) (((sizeof(#T) - 1ull) << 32) - T)
  static constexpr uint64_t table[] = {
      FMT_INC(0),          FMT_INC(0),          FMT_INC(0),           // 8
      FMT_INC(10),         FMT_INC(10),         FMT_INC(10),          // 64
      FMT_INC(100),        FMT_INC(100),        FMT_INC(100),         // 512
      FMT_INC(1000),       FMT_INC(1000),       FMT_INC(1000),        // 4096
      FMT_INC(10000),      FMT_INC(10000),      FMT_INC(10000),       // 32k
      FMT_INC(100000),     FMT_INC(100000),     FMT_INC(100000),      // 256k
      FMT_INC(1000000),    FMT_INC(1000000),    FMT_INC(1000000),     // 2048k
      FMT_INC(10000000),   FMT_INC(10000000),   FMT_INC(10000000),    // 16M
      FMT_INC(100000000),  FMT_INC(100000000),  FMT_INC(100000000),   // 128M
      FMT_INC(1000000000), FMT_INC(1000000000), FMT_INC(1000000000),  // 1024M
      FMT_INC(1000000000), FMT_INC(1000000000)                        // 4B
  };
  auto inc = table[FMT_BUILTIN_CLZ(n | 1) ^ 31];
  return static_cast<int>((n + inc) >> 32);
}
#endif

// Optional version of count_digits for better performance on 32-bit platforms.
FMT_CONSTEXPR20 inline auto count_digits(uint32_t n) -> int {
#ifdef FMT_BUILTIN_CLZ
  if (!is_constant_evaluated() && !FMT_OPTIMIZE_SIZE) return do_count_digits(n);
#endif
  return count_digits_fallback(n);
}

template <typename Int> constexpr auto digits10() noexcept -> int {
  return std::numeric_limits<Int>::digits10;
}
template <> constexpr auto digits10<int128_opt>() noexcept -> int { return 38; }
template <> constexpr auto digits10<uint128_t>() noexcept -> int { return 38; }

template <typename Char> struct thousands_sep_result {
  fmt::string grouping;
  Char thousands_sep;
};

template <typename Char>
FMT_API auto thousands_sep_impl(locale_ref loc) -> thousands_sep_result<Char>;
template <typename Char>
inline auto thousands_sep(locale_ref loc) -> thousands_sep_result<Char> {
  auto result = thousands_sep_impl<char>(loc);
  return {result.grouping, Char(result.thousands_sep)};
}
template <>
inline auto thousands_sep(locale_ref loc) -> thousands_sep_result<wchar_t> {
  return thousands_sep_impl<wchar_t>(loc);
}

template <typename Char>
FMT_API auto decimal_point_impl(locale_ref loc) -> Char;
template <typename Char> inline auto decimal_point(locale_ref loc) -> Char {
  return Char(decimal_point_impl<char>(loc));
}
template <> inline auto decimal_point(locale_ref loc) -> wchar_t {
  return decimal_point_impl<wchar_t>(loc);
}

#ifndef FMT_HEADER_ONLY
FMT_BEGIN_EXPORT
extern template FMT_API auto thousands_sep_impl<char>(locale_ref)
    -> thousands_sep_result<char>;
extern template FMT_API auto thousands_sep_impl<wchar_t>(locale_ref)
    -> thousands_sep_result<wchar_t>;
extern template FMT_API auto decimal_point_impl(locale_ref) -> char;
extern template FMT_API auto decimal_point_impl(locale_ref) -> wchar_t;
FMT_END_EXPORT
#endif  // FMT_HEADER_ONLY

// Compares two characters for equality.
template <typename Char> auto equal2(const Char* lhs, const char* rhs) -> bool {
  return lhs[0] == Char(rhs[0]) && lhs[1] == Char(rhs[1]);
}
inline auto equal2(const char* lhs, const char* rhs) -> bool {
  return memcmp(lhs, rhs, 2) == 0;
}

// Writes a two-digit value to out.
template <typename Char>
FMT_CONSTEXPR20 FMT_INLINE void write2digits(Char* out, size_t value) {
  if (!is_constant_evaluated() && std::is_same<Char, char>::value &&
      !FMT_OPTIMIZE_SIZE) {
    memcpy(out, digits2(value), 2);
    return;
  }
  *out++ = static_cast<Char>('0' + value / 10);
  *out = static_cast<Char>('0' + value % 10);
}

// Formats a decimal unsigned integer value writing to out pointing to a buffer
// of specified size. The caller must ensure that the buffer is large enough.
template <typename Char, typename UInt>
FMT_CONSTEXPR20 auto do_format_decimal(Char* out, UInt value, int size)
    -> Char* {
  FMT_ASSERT(size >= count_digits(value), "invalid digit count");
  unsigned n = to_unsigned(size);
  while (value >= 100) {
    // Integer division is slow so do it for a group of two digits instead
    // of for every digit. The idea comes from the talk by Alexandrescu
    // "Three Optimization Tips for C++". See speed-test for a comparison.
    n -= 2;
    write2digits(out + n, static_cast<unsigned>(value % 100));
    value /= 100;
  }
  if (value >= 10) {
    n -= 2;
    write2digits(out + n, static_cast<unsigned>(value));
  } else {
    out[--n] = static_cast<Char>('0' + value);
  }
  return out + n;
}

template <typename Char, typename UInt>
FMT_CONSTEXPR FMT_INLINE auto format_decimal(Char* out, UInt value,
                                             int num_digits) -> Char* {
  do_format_decimal(out, value, num_digits);
  return out + num_digits;
}

template <typename Char, typename UInt, typename OutputIt,
          FMT_ENABLE_IF(!std::is_pointer<remove_cvref_t<OutputIt>>::value)>
FMT_CONSTEXPR auto format_decimal(OutputIt out, UInt value, int num_digits)
    -> OutputIt {
  if (auto ptr = to_pointer<Char>(out, to_unsigned(num_digits))) {
    do_format_decimal(ptr, value, num_digits);
    return out;
  }
  // Buffer is large enough to hold all digits (digits10 + 1).
  char buffer[digits10<UInt>() + 1];
  if (is_constant_evaluated()) fill_n(buffer, sizeof(buffer), '\0');
  do_format_decimal(buffer, value, num_digits);
  return copy_noinline<Char>(buffer, buffer + num_digits, out);
}

template <typename Char, typename UInt>
FMT_CONSTEXPR auto do_format_base2e(int base_bits, Char* out, UInt value,
                                    int size, bool upper = false) -> Char* {
  out += size;
  do {
    const char* digits = upper ? "0123456789ABCDEF" : "0123456789abcdef";
    unsigned digit = static_cast<unsigned>(value & ((1u << base_bits) - 1));
    *--out = static_cast<Char>(base_bits < 4 ? static_cast<char>('0' + digit)
                                             : digits[digit]);
  } while ((value >>= base_bits) != 0);
  return out;
}

// Formats an unsigned integer in the power of two base (binary, octal, hex).
template <typename Char, typename UInt>
FMT_CONSTEXPR auto format_base2e(int base_bits, Char* out, UInt value,
                                 int num_digits, bool upper = false) -> Char* {
  do_format_base2e(base_bits, out, value, num_digits, upper);
  return out + num_digits;
}

template <typename Char, typename OutputIt, typename UInt,
          FMT_ENABLE_IF(is_back_insert_iterator<OutputIt>::value)>
FMT_CONSTEXPR inline auto format_base2e(int base_bits, OutputIt out, UInt value,
                                        int num_digits, bool upper = false)
    -> OutputIt {
  if (auto ptr = to_pointer<Char>(out, to_unsigned(num_digits))) {
    format_base2e(base_bits, ptr, value, num_digits, upper);
    return out;
  }
  // Make buffer large enough for any base.
  char buffer[num_bits<UInt>()];
  if (is_constant_evaluated()) fill_n(buffer, sizeof(buffer), '\0');
  format_base2e(base_bits, buffer, value, num_digits, upper);
  return detail::copy_noinline<Char>(buffer, buffer + num_digits, out);
}

// A converter from UTF-8 to UTF-16.
class utf8_to_utf16 {
 private:
  basic_memory_buffer<wchar_t> buffer_;

 public:
  FMT_API explicit utf8_to_utf16(string_view s);
  inline operator basic_string_view<wchar_t>() const {
    return {&buffer_[0], size()};
  }
  inline auto size() const -> size_t { return buffer_.size() - 1; }
  inline auto c_str() const -> const wchar_t* { return &buffer_[0]; }
  inline auto str() const -> fmt::wstring { return {&buffer_[0], size()}; }
};

enum class to_utf8_error_policy { abort, replace };

// A converter from UTF-16/UTF-32 (host endian) to UTF-8.
template <typename WChar, typename Buffer = memory_buffer> class to_utf8 {
 private:
  Buffer buffer_;

 public:
  to_utf8() {}
  explicit to_utf8(basic_string_view<WChar> s,
                   to_utf8_error_policy policy = to_utf8_error_policy::abort) {
    static_assert(sizeof(WChar) == 2 || sizeof(WChar) == 4,
                  "Expect utf16 or utf32");
    if (!convert(s, policy))
      FMT_THROW_RUNTIME_ERROR(sizeof(WChar) == 2 ? "invalid utf16"
                                                 : "invalid utf32");
  }
  operator string_view() const { return string_view(&buffer_[0], size()); }
  auto size() const -> size_t { return buffer_.size() - 1; }
  auto c_str() const -> const char* { return &buffer_[0]; }
  auto str() const -> fmt::string { return fmt::string(&buffer_[0], size()); }

  // Performs conversion returning a bool instead of throwing exception on
  // conversion error. This method may still throw in case of memory allocation
  // error.
  auto convert(basic_string_view<WChar> s,
               to_utf8_error_policy policy = to_utf8_error_policy::abort)
      -> bool {
    if (!convert(buffer_, s, policy)) return false;
    buffer_.push_back(0);
    return true;
  }
  static auto convert(Buffer& buf, basic_string_view<WChar> s,
                      to_utf8_error_policy policy = to_utf8_error_policy::abort)
      -> bool {
    for (auto p = s.begin(); p != s.end(); ++p) {
      uint32_t c = static_cast<uint32_t>(*p);
      if (sizeof(WChar) == 2 && c >= 0xd800 && c <= 0xdfff) {
        // Handle a surrogate pair.
        ++p;
        if (p == s.end() || (c & 0xfc00) != 0xd800 || (*p & 0xfc00) != 0xdc00) {
          if (policy == to_utf8_error_policy::abort) return false;
          buf.append(string_view("\xEF\xBF\xBD"));
          --p;
          continue;
        } else {
          c = (c << 10) + static_cast<uint32_t>(*p) - 0x35fdc00;
        }
      }
      if (c < 0x80) {
        buf.push_back(static_cast<char>(c));
      } else if (c < 0x800) {
        buf.push_back(static_cast<char>(0xc0 | (c >> 6)));
        buf.push_back(static_cast<char>(0x80 | (c & 0x3f)));
      } else if ((c >= 0x800 && c <= 0xd7ff) || (c >= 0xe000 && c <= 0xffff)) {
        buf.push_back(static_cast<char>(0xe0 | (c >> 12)));
        buf.push_back(static_cast<char>(0x80 | ((c & 0xfff) >> 6)));
        buf.push_back(static_cast<char>(0x80 | (c & 0x3f)));
      } else if (c >= 0x10000 && c <= 0x10ffff) {
        buf.push_back(static_cast<char>(0xf0 | (c >> 18)));
        buf.push_back(static_cast<char>(0x80 | ((c & 0x3ffff) >> 12)));
        buf.push_back(static_cast<char>(0x80 | ((c & 0xfff) >> 6)));
        buf.push_back(static_cast<char>(0x80 | (c & 0x3f)));
      } else {
        return false;
      }
    }
    return true;
  }
};

// Computes 128-bit result of multiplication of two 64-bit unsigned integers.
inline auto umul128(uint64_t x, uint64_t y) noexcept -> uint128_fallback {
#if FMT_USE_INT128
  auto p = static_cast<uint128_opt>(x) * static_cast<uint128_opt>(y);
  return {static_cast<uint64_t>(p >> 64), static_cast<uint64_t>(p)};
#elif defined(_MSC_VER) && defined(_M_X64)
  auto hi = uint64_t();
  auto lo = _umul128(x, y, &hi);
  return {hi, lo};
#else
  const uint64_t mask = static_cast<uint64_t>(max_value<uint32_t>());

  uint64_t a = x >> 32;
  uint64_t b = x & mask;
  uint64_t c = y >> 32;
  uint64_t d = y & mask;

  uint64_t ac = a * c;
  uint64_t bc = b * c;
  uint64_t ad = a * d;
  uint64_t bd = b * d;

  uint64_t intermediate = (bd >> 32) + (ad & mask) + (bc & mask);

  return {ac + (intermediate >> 32) + (ad >> 32) + (bc >> 32),
          (intermediate << 32) + (bd & mask)};
#endif
}

namespace dragonbox {
// Computes floor(log10(pow(2, e))) for e in [-2620, 2620] using the method from
// https://fmt.dev/papers/Dragonbox.pdf#page=28, section 6.1.
inline auto floor_log10_pow2(int e) noexcept -> int {
  FMT_ASSERT(e <= 2620 && e >= -2620, "too large exponent");
  static_assert((-1 >> 1) == -1, "right shift is not arithmetic");
  return (e * 315653) >> 20;
}

inline auto floor_log2_pow10(int e) noexcept -> int {
  FMT_ASSERT(e <= 1233 && e >= -1233, "too large exponent");
  return (e * 1741647) >> 19;
}

// Computes upper 64 bits of multiplication of two 64-bit unsigned integers.
inline auto umul128_upper64(uint64_t x, uint64_t y) noexcept -> uint64_t {
#if FMT_USE_INT128
  auto p = static_cast<uint128_opt>(x) * static_cast<uint128_opt>(y);
  return static_cast<uint64_t>(p >> 64);
#elif defined(_MSC_VER) && defined(_M_X64)
  return __umulh(x, y);
#else
  return umul128(x, y).high();
#endif
}

// Computes upper 128 bits of multiplication of a 64-bit unsigned integer and a
// 128-bit unsigned integer.
inline auto umul192_upper128(uint64_t x, uint128_fallback y) noexcept
    -> uint128_fallback {
  uint128_fallback r = umul128(x, y.high());
  r += umul128_upper64(x, y.low());
  return r;
}

FMT_API auto get_cached_power(int k) noexcept -> uint128_fallback;

// Type-specific information that Dragonbox uses.
template <typename T, typename Enable = void> struct float_info;

template <> struct float_info<float> {
  using carrier_uint = uint32_t;
  static const int exponent_bits = 8;
  static const int kappa = 1;
  static const int big_divisor = 100;
  static const int small_divisor = 10;
  static const int min_k = -31;
  static const int max_k = 46;
  static const int shorter_interval_tie_lower_threshold = -35;
  static const int shorter_interval_tie_upper_threshold = -35;
};

template <> struct float_info<double> {
  using carrier_uint = uint64_t;
  static const int exponent_bits = 11;
  static const int kappa = 2;
  static const int big_divisor = 1000;
  static const int small_divisor = 100;
  static const int min_k = -292;
  static const int max_k = 341;
  static const int shorter_interval_tie_lower_threshold = -77;
  static const int shorter_interval_tie_upper_threshold = -77;
};

// An 80- or 128-bit floating point number.
template <typename T>
struct float_info<T, enable_if_t<std::numeric_limits<T>::digits == 64 ||
                                 std::numeric_limits<T>::digits == 113 ||
                                 is_float128<T>::value>> {
  using carrier_uint = detail::uint128_t;
  static const int exponent_bits = 15;
};

// A double-double floating point number.
template <typename T>
struct float_info<T, enable_if_t<is_double_double<T>::value>> {
  using carrier_uint = detail::uint128_t;
};

template <typename T> struct decimal_fp {
  using significand_type = typename float_info<T>::carrier_uint;
  significand_type significand;
  int exponent;
};

template <typename T> FMT_API auto to_decimal(T x) noexcept -> decimal_fp<T>;
}  // namespace dragonbox

// Returns true iff Float has the implicit bit which is not stored.
template <typename Float> constexpr auto has_implicit_bit() -> bool {
  // An 80-bit FP number has a 64-bit significand an no implicit bit.
  return std::numeric_limits<Float>::digits != 64;
}

// Returns the number of significand bits stored in Float. The implicit bit is
// not counted since it is not stored.
template <typename Float> constexpr auto num_significand_bits() -> int {
  // std::numeric_limits may not support __float128.
  return is_float128<Float>() ? 112
                              : (std::numeric_limits<Float>::digits -
                                 (has_implicit_bit<Float>() ? 1 : 0));
}

template <typename Float>
constexpr auto exponent_mask() ->
    typename dragonbox::float_info<Float>::carrier_uint {
  using float_uint = typename dragonbox::float_info<Float>::carrier_uint;
  return ((float_uint(1) << dragonbox::float_info<Float>::exponent_bits) - 1)
         << num_significand_bits<Float>();
}
template <typename Float> constexpr auto exponent_bias() -> int {
  // std::numeric_limits may not support __float128.
  return is_float128<Float>() ? 16383
                              : std::numeric_limits<Float>::max_exponent - 1;
}

FMT_CONSTEXPR inline auto compute_exp_size(int exp) -> int {
  auto prefix_size = 2;  // sign + 'e'
  auto abs_exp = exp >= 0 ? exp : -exp;
  if (exp < 100) return prefix_size + 2;
  return prefix_size + (abs_exp >= 1000 ? 4 : 3);
}

// Writes the exponent exp in the form "[+-]d{2,3}" to buffer.
template <typename Char, typename OutputIt>
FMT_CONSTEXPR auto write_exponent(int exp, OutputIt out) -> OutputIt {
  FMT_ASSERT(-10000 < exp && exp < 10000, "exponent out of range");
  if (exp < 0) {
    *out++ = static_cast<Char>('-');
    exp = -exp;
  } else {
    *out++ = static_cast<Char>('+');
  }
  auto uexp = static_cast<uint32_t>(exp);
  if (is_constant_evaluated()) {
    if (uexp < 10) *out++ = '0';
    return format_decimal<Char>(out, uexp, count_digits(uexp));
  }
  if (uexp >= 100u) {
    const char* top = digits2(uexp / 100);
    if (uexp >= 1000u) *out++ = static_cast<Char>(top[0]);
    *out++ = static_cast<Char>(top[1]);
    uexp %= 100;
  }
  const char* d = digits2(uexp);
  *out++ = static_cast<Char>(d[0]);
  *out++ = static_cast<Char>(d[1]);
  return out;
}

// A floating-point number f * pow(2, e) where F is an unsigned type.
template <typename F> struct basic_fp {
  F f;
  int e;

  static constexpr int num_significand_bits =
      static_cast<int>(sizeof(F) * num_bits<unsigned char>());

  constexpr basic_fp() : f(0), e(0) {}
  constexpr basic_fp(uint64_t f_val, int e_val) : f(f_val), e(e_val) {}

  // Constructs fp from an IEEE754 floating-point number.
  template <typename Float> FMT_CONSTEXPR basic_fp(Float n) { assign(n); }

  // Assigns n to this and return true iff predecessor is closer than successor.
  template <typename Float, FMT_ENABLE_IF(!is_double_double<Float>::value)>
  FMT_CONSTEXPR auto assign(Float n) -> bool {
    static_assert(std::numeric_limits<Float>::digits <= 113, "unsupported FP");
    // Assume Float is in the format [sign][exponent][significand].
    using carrier_uint = typename dragonbox::float_info<Float>::carrier_uint;
    const auto num_float_significand_bits =
        detail::num_significand_bits<Float>();
    const auto implicit_bit = carrier_uint(1) << num_float_significand_bits;
    const auto significand_mask = implicit_bit - 1;
    auto u = bit_cast<carrier_uint>(n);
    f = static_cast<F>(u & significand_mask);
    auto biased_e = static_cast<int>((u & exponent_mask<Float>()) >>
                                     num_float_significand_bits);
    // The predecessor is closer if n is a normalized power of 2 (f == 0)
    // other than the smallest normalized number (biased_e > 1).
    auto is_predecessor_closer = f == 0 && biased_e > 1;
    if (biased_e == 0)
      biased_e = 1;  // Subnormals use biased exponent 1 (min exponent).
    else if (has_implicit_bit<Float>())
      f += static_cast<F>(implicit_bit);
    e = biased_e - exponent_bias<Float>() - num_float_significand_bits;
    if (!has_implicit_bit<Float>()) ++e;
    return is_predecessor_closer;
  }

  template <typename Float, FMT_ENABLE_IF(is_double_double<Float>::value)>
  FMT_CONSTEXPR auto assign(Float n) -> bool {
    static_assert(std::numeric_limits<double>::is_iec559, "unsupported FP");
    return assign(static_cast<double>(n));
  }
};

using fp = basic_fp<unsigned long long>;

// Normalizes the value converted from double and multiplied by (1 << SHIFT).
template <int SHIFT = 0, typename F>
FMT_CONSTEXPR auto normalize(basic_fp<F> value) -> basic_fp<F> {
  // Handle subnormals.
  const auto implicit_bit = F(1) << num_significand_bits<double>();
  const auto shifted_implicit_bit = implicit_bit << SHIFT;
  while ((value.f & shifted_implicit_bit) == 0) {
    value.f <<= 1;
    --value.e;
  }
  // Subtract 1 to account for hidden bit.
  const auto offset = basic_fp<F>::num_significand_bits -
                      num_significand_bits<double>() - SHIFT - 1;
  value.f <<= offset;
  value.e -= offset;
  return value;
}

// Computes lhs * rhs / pow(2, 64) rounded to nearest with half-up tie breaking.
FMT_CONSTEXPR inline auto multiply(uint64_t lhs, uint64_t rhs) -> uint64_t {
#if FMT_USE_INT128
  auto product = static_cast<__uint128_t>(lhs) * rhs;
  auto f = static_cast<uint64_t>(product >> 64);
  return (static_cast<uint64_t>(product) & (1ULL << 63)) != 0 ? f + 1 : f;
#else
  // Multiply 32-bit parts of significands.
  uint64_t mask = (1ULL << 32) - 1;
  uint64_t a = lhs >> 32, b = lhs & mask;
  uint64_t c = rhs >> 32, d = rhs & mask;
  uint64_t ac = a * c, bc = b * c, ad = a * d, bd = b * d;
  // Compute mid 64-bit of result and round.
  uint64_t mid = (bd >> 32) + (ad & mask) + (bc & mask) + (1U << 31);
  return ac + (ad >> 32) + (bc >> 32) + (mid >> 32);
#endif
}

FMT_CONSTEXPR inline auto operator*(fp x, fp y) -> fp {
  return {multiply(x.f, y.f), x.e + y.e + 64};
}

template <typename T, bool doublish = num_bits<T>() == num_bits<double>()>
using convert_float_result =
    conditional_t<std::is_same<T, float>::value || doublish, double, T>;

template <typename T>
constexpr auto convert_float(T value) -> convert_float_result<T> {
  return static_cast<convert_float_result<T>>(value);
}

template <typename Char, typename OutputIt>
FMT_CONSTEXPR FMT_NOINLINE auto fill(OutputIt it, size_t n,
                                     const basic_specs& specs) -> OutputIt {
  auto fill_size = specs.fill_size();
  if (fill_size == 1) return detail::fill_n(it, n, specs.fill_unit<Char>());
  if (const Char* data = specs.fill<Char>()) {
    for (size_t i = 0; i < n; ++i) it = copy<Char>(data, data + fill_size, it);
  }
  return it;
}

// Writes the output of f, padded according to format specifications in specs.
// size: output size in code units.
// width: output display width in (terminal) column positions.
template <typename Char, align default_align = align::left, typename OutputIt,
          typename F>
FMT_CONSTEXPR auto write_padded(OutputIt out, const format_specs& specs,
                                size_t size, size_t width, F&& f) -> OutputIt {
  static_assert(default_align == align::left || default_align == align::right,
                "");
  unsigned spec_width = to_unsigned(specs.width);
  size_t padding = spec_width > width ? spec_width - width : 0;
  // Shifts are encoded as string literals because static constexpr is not
  // supported in constexpr functions.
  auto* shifts =
      default_align == align::left ? "\x1f\x1f\x00\x01" : "\x00\x1f\x00\x01";
  size_t left_padding = padding >> shifts[static_cast<int>(specs.align())];
  size_t right_padding = padding - left_padding;
  auto it = reserve(out, size + padding * specs.fill_size());
  if (left_padding != 0) it = fill<Char>(it, left_padding, specs);
  it = f(it);
  if (right_padding != 0) it = fill<Char>(it, right_padding, specs);
  return base_iterator(out, it);
}

template <typename Char, align default_align = align::left, typename OutputIt,
          typename F>
constexpr auto write_padded(OutputIt out, const format_specs& specs,
                            size_t size, F&& f) -> OutputIt {
  return write_padded<Char, default_align>(out, specs, size, size, f);
}

template <typename Char, align default_align = align::left, typename OutputIt>
FMT_CONSTEXPR auto write_bytes(OutputIt out, string_view bytes,
                               const format_specs& specs = {}) -> OutputIt {
  return write_padded<Char, default_align>(
      out, specs, bytes.size(), [bytes](reserve_iterator<OutputIt> it) {
        const char* data = bytes.data();
        return copy<Char>(data, data + bytes.size(), it);
      });
}

template <typename Char, typename OutputIt, typename UIntPtr>
auto write_ptr(OutputIt out, UIntPtr value, const format_specs* specs)
    -> OutputIt {
  int num_digits = count_digits<4>(value);
  auto size = to_unsigned(num_digits) + size_t(2);
  auto write = [=](reserve_iterator<OutputIt> it) {
    *it++ = static_cast<Char>('0');
    *it++ = static_cast<Char>('x');
    return format_base2e<Char>(4, it, value, num_digits);
  };
  return specs ? write_padded<Char, align::right>(out, *specs, size, write)
               : base_iterator(out, write(reserve(out, size)));
}

// Returns true iff the code point cp is printable.
FMT_API auto is_printable(uint32_t cp) -> bool;

inline auto needs_escape(uint32_t cp) -> bool {
  if (cp < 0x20 || cp == 0x7f || cp == '"' || cp == '\\') return true;
  if (const_check(FMT_OPTIMIZE_SIZE > 1)) return false;
  return !is_printable(cp);
}

template <typename Char> struct find_escape_result {
  const Char* begin;
  const Char* end;
  uint32_t cp;
};

template <typename Char>
auto find_escape(const Char* begin, const Char* end)
    -> find_escape_result<Char> {
  for (; begin != end; ++begin) {
    uint32_t cp = static_cast<unsigned_char<Char>>(*begin);
    if (const_check(sizeof(Char) == 1) && cp >= 0x80) continue;
    if (needs_escape(cp)) return {begin, begin + 1, cp};
  }
  return {begin, nullptr, 0};
}

inline auto find_escape(const char* begin, const char* end)
    -> find_escape_result<char> {
  if (const_check(!use_utf8)) return find_escape<char>(begin, end);
  auto result = find_escape_result<char>{end, nullptr, 0};
  for_each_codepoint(string_view(begin, to_unsigned(end - begin)),
                     [&](uint32_t cp, string_view sv) {
                       if (needs_escape(cp)) {
                         result = {sv.begin(), sv.end(), cp};
                         return false;
                       }
                       return true;
                     });
  return result;
}

template <size_t width, typename Char, typename OutputIt>
auto write_codepoint(OutputIt out, char prefix, uint32_t cp) -> OutputIt {
  *out++ = static_cast<Char>('\\');
  *out++ = static_cast<Char>(prefix);
  Char buf[width];
  fill_n(buf, width, static_cast<Char>('0'));
  format_base2e(4, buf, cp, width);
  return copy<Char>(buf, buf + width, out);
}

template <typename OutputIt, typename Char>
auto write_escaped_cp(OutputIt out, const find_escape_result<Char>& escape)
    -> OutputIt {
  auto c = static_cast<Char>(escape.cp);
  switch (escape.cp) {
  case '\n':
    *out++ = static_cast<Char>('\\');
    c = static_cast<Char>('n');
    break;
  case '\r':
    *out++ = static_cast<Char>('\\');
    c = static_cast<Char>('r');
    break;
  case '\t':
    *out++ = static_cast<Char>('\\');
    c = static_cast<Char>('t');
    break;
  case '"':  FMT_FALLTHROUGH;
  case '\'': FMT_FALLTHROUGH;
  case '\\': *out++ = static_cast<Char>('\\'); break;
  default:
    if (escape.cp < 0x100) return write_codepoint<2, Char>(out, 'x', escape.cp);
    if (escape.cp < 0x10000)
      return write_codepoint<4, Char>(out, 'u', escape.cp);
    if (escape.cp < 0x110000)
      return write_codepoint<8, Char>(out, 'U', escape.cp);
    for (Char escape_char : basic_string_view<Char>(
             escape.begin, to_unsigned(escape.end - escape.begin))) {
      out = write_codepoint<2, Char>(out, 'x',
                                     static_cast<uint32_t>(escape_char) & 0xFF);
    }
    return out;
  }
  *out++ = c;
  return out;
}

template <typename Char, typename OutputIt>
auto write_escaped_string(OutputIt out, basic_string_view<Char> str)
    -> OutputIt {
  *out++ = static_cast<Char>('"');
  auto begin = str.begin(), end = str.end();
  do {
    auto escape = find_escape(begin, end);
    out = copy<Char>(begin, escape.begin, out);
    begin = escape.end;
    if (!begin) break;
    out = write_escaped_cp<OutputIt, Char>(out, escape);
  } while (begin != end);
  *out++ = static_cast<Char>('"');
  return out;
}

template <typename Char, typename OutputIt>
auto write_escaped_char(OutputIt out, Char v) -> OutputIt {
  Char v_array[1] = {v};
  *out++ = static_cast<Char>('\'');
  if ((needs_escape(static_cast<uint32_t>(v)) && v != static_cast<Char>('"')) ||
      v == static_cast<Char>('\'')) {
    out = write_escaped_cp(out,
                           find_escape_result<Char>{v_array, v_array + 1,
                                                    static_cast<uint32_t>(v)});
  } else {
    *out++ = v;
  }
  *out++ = static_cast<Char>('\'');
  return out;
}

template <typename Char, typename OutputIt>
FMT_CONSTEXPR auto write_char(OutputIt out, Char value,
                              const format_specs& specs) -> OutputIt {
  bool is_debug = specs.type() == presentation_type::debug;
  return write_padded<Char>(out, specs, 1, [=](reserve_iterator<OutputIt> it) {
    if (is_debug) return write_escaped_char(it, value);
    *it++ = value;
    return it;
  });
}
template <typename Char, typename OutputIt>
FMT_CONSTEXPR auto write(OutputIt out, Char value, const format_specs& specs,
                         locale_ref loc = {}) -> OutputIt {
  // char is formatted as unsigned char for consistency across platforms.
  using unsigned_type =
      conditional_t<std::is_same<Char, char>::value, unsigned char, unsigned>;
  return check_char_specs(specs)
             ? write_char<Char>(out, value, specs)
             : write<Char>(out, static_cast<unsigned_type>(value), specs, loc);
}

template <typename Char> class digit_grouping {
 private:
  fmt::string grouping_;
  fmt::basic_string<Char> thousands_sep_;

  struct next_state {
    fmt::string::const_iterator group;
    int pos;
  };
  auto initial_state() const -> next_state { return {grouping_.begin(), 0}; }

  // Returns the next digit group separator position.
  auto next(next_state& state) const -> int {
    if (thousands_sep_.empty()) return max_value<int>();
    if (state.group == grouping_.end()) return state.pos += grouping_.back();
    if (*state.group <= 0 || *state.group == max_value<char>())
      return max_value<int>();
    state.pos += *state.group++;
    return state.pos;
  }

 public:
  template <typename Locale,
            FMT_ENABLE_IF(std::is_same<Locale, locale_ref>::value)>
  explicit digit_grouping(Locale loc, bool localized = true) {
    if (!localized) return;
    auto sep = thousands_sep<Char>(loc);
    grouping_ = sep.grouping;
    if (sep.thousands_sep) thousands_sep_.assign(1, sep.thousands_sep);
  }
  digit_grouping(fmt::string grouping, fmt::basic_string<Char> sep)
      : grouping_(std::move(grouping)), thousands_sep_(std::move(sep)) {}

  auto has_separator() const -> bool { return !thousands_sep_.empty(); }

  auto count_separators(int num_digits) const -> int {
    int count = 0;
    auto state = initial_state();
    while (num_digits > next(state)) ++count;
    return count;
  }

  // Applies grouping to digits and write the output to out.
  template <typename Out, typename C>
  auto apply(Out out, basic_string_view<C> digits) const -> Out {
    auto num_digits = static_cast<int>(digits.size());
    auto separators = basic_memory_buffer<int>();
    separators.push_back(0);
    auto state = initial_state();
    while (int i = next(state)) {
      if (i >= num_digits) break;
      separators.push_back(i);
    }
    for (int i = 0, sep_index = static_cast<int>(separators.size() - 1);
         i < num_digits; ++i) {
      if (num_digits - i == separators[sep_index]) {
        out = copy<Char>(thousands_sep_.data(),
                         thousands_sep_.data() + thousands_sep_.size(), out);
        --sep_index;
      }
      *out++ = static_cast<Char>(digits[to_unsigned(i)]);
    }
    return out;
  }
};

FMT_CONSTEXPR inline void prefix_append(unsigned& prefix, unsigned value) {
  prefix |= prefix != 0 ? value << 8 : value;
  prefix += (1u + (value > 0xff ? 1 : 0)) << 24;
}

// Writes a decimal integer with digit grouping.
template <typename OutputIt, typename UInt, typename Char>
auto write_int(OutputIt out, UInt value, unsigned prefix,
               const format_specs& specs, const digit_grouping<Char>& grouping)
    -> OutputIt {
  static_assert(std::is_same<uint64_or_128_t<UInt>, UInt>::value, "");
  int num_digits = 0;
  auto buffer = memory_buffer();
  switch (specs.type()) {
  default: FMT_ASSERT(false, ""); FMT_FALLTHROUGH;
  case presentation_type::none:
  case presentation_type::dec:
    num_digits = count_digits(value);
    format_decimal<char>(appender(buffer), value, num_digits);
    break;
  case presentation_type::hex:
    if (specs.alt())
      prefix_append(prefix, unsigned(specs.upper() ? 'X' : 'x') << 8 | '0');
    num_digits = count_digits<4>(value);
    format_base2e<char>(4, appender(buffer), value, num_digits, specs.upper());
    break;
  case presentation_type::oct:
    num_digits = count_digits<3>(value);
    // Octal prefix '0' is counted as a digit, so only add it if precision
    // is not greater than the number of digits.
    if (specs.alt() && specs.precision <= num_digits && value != 0)
      prefix_append(prefix, '0');
    format_base2e<char>(3, appender(buffer), value, num_digits);
    break;
  case presentation_type::bin:
    if (specs.alt())
      prefix_append(prefix, unsigned(specs.upper() ? 'B' : 'b') << 8 | '0');
    num_digits = count_digits<1>(value);
    format_base2e<char>(1, appender(buffer), value, num_digits);
    break;
  case presentation_type::chr:
    return write_char<Char>(out, static_cast<Char>(value), specs);
  }

  unsigned size = (prefix != 0 ? prefix >> 24 : 0) + to_unsigned(num_digits) +
                  to_unsigned(grouping.count_separators(num_digits));
  return write_padded<Char, align::right>(
      out, specs, size, size, [&](reserve_iterator<OutputIt> it) {
        for (unsigned p = prefix & 0xffffff; p != 0; p >>= 8)
          *it++ = static_cast<Char>(p & 0xff);
        return grouping.apply(it, string_view(buffer.data(), buffer.size()));
      });
}

#if FMT_USE_LOCALE
// Writes a localized value.
FMT_API auto write_loc(appender out, loc_value value, const format_specs& specs,
                       locale_ref loc) -> bool;
#endif
template <typename OutputIt>
inline auto write_loc(OutputIt, const loc_value&, const format_specs&,
                      locale_ref) -> bool {
  return false;
}

template <typename UInt> struct write_int_arg {
  UInt abs_value;
  unsigned prefix;
};

template <typename T>
FMT_CONSTEXPR auto make_write_int_arg(T value, sign s)
    -> write_int_arg<uint32_or_64_or_128_t<T>> {
  auto prefix = 0u;
  auto abs_value = static_cast<uint32_or_64_or_128_t<T>>(value);
  if (is_negative(value)) {
    prefix = 0x01000000 | '-';
    abs_value = 0 - abs_value;
  } else {
    constexpr unsigned prefixes[4] = {0, 0, 0x1000000u | '+', 0x1000000u | ' '};
    prefix = prefixes[static_cast<int>(s)];
  }
  return {abs_value, prefix};
}

template <typename Char = char> struct loc_writer {
  basic_appender<Char> out;
  const format_specs& specs;
  fmt::basic_string<Char> sep;
  fmt::string grouping;
  fmt::basic_string<Char> decimal_point;

  template <typename T, FMT_ENABLE_IF(is_integer<T>::value)>
  auto operator()(T value) -> bool {
    auto arg = make_write_int_arg(value, specs.sign());
    write_int(out, static_cast<uint64_or_128_t<T>>(arg.abs_value), arg.prefix,
              specs, digit_grouping<Char>(grouping, sep));
    return true;
  }

  template <typename T, FMT_ENABLE_IF(!is_integer<T>::value)>
  auto operator()(T) -> bool {
    return false;
  }
};

// Size and padding computation separate from write_int to avoid template bloat.
struct size_padding {
  unsigned size;
  unsigned padding;

  FMT_CONSTEXPR size_padding(int num_digits, unsigned prefix,
                             const format_specs& specs)
      : size((prefix >> 24) + to_unsigned(num_digits)), padding(0) {
    if (specs.align() == align::numeric) {
      auto width = to_unsigned(specs.width);
      if (width > size) {
        padding = width - size;
        size = width;
      }
    } else if (specs.precision > num_digits) {
      size = (prefix >> 24) + to_unsigned(specs.precision);
      padding = to_unsigned(specs.precision - num_digits);
    }
  }
};

template <typename Char, typename OutputIt, typename T>
FMT_CONSTEXPR FMT_INLINE auto write_int(OutputIt out, write_int_arg<T> arg,
                                        const format_specs& specs) -> OutputIt {
  static_assert(std::is_same<T, uint32_or_64_or_128_t<T>>::value, "");

  constexpr size_t buffer_size = num_bits<T>();
  char buffer[buffer_size];
  if (is_constant_evaluated()) fill_n(buffer, buffer_size, '\0');
  const char* begin = nullptr;
  const char* end = buffer + buffer_size;

  auto abs_value = arg.abs_value;
  auto prefix = arg.prefix;
  switch (specs.type()) {
  default: FMT_ASSERT(false, ""); FMT_FALLTHROUGH;
  case presentation_type::none:
  case presentation_type::dec:
    begin = do_format_decimal(buffer, abs_value, buffer_size);
    break;
  case presentation_type::hex:
    begin = do_format_base2e(4, buffer, abs_value, buffer_size, specs.upper());
    if (specs.alt())
      prefix_append(prefix, unsigned(specs.upper() ? 'X' : 'x') << 8 | '0');
    break;
  case presentation_type::oct: {
    begin = do_format_base2e(3, buffer, abs_value, buffer_size);
    // Octal prefix '0' is counted as a digit, so only add it if precision
    // is not greater than the number of digits.
    auto num_digits = end - begin;
    if (specs.alt() && specs.precision <= num_digits && abs_value != 0)
      prefix_append(prefix, '0');
    break;
  }
  case presentation_type::bin:
    begin = do_format_base2e(1, buffer, abs_value, buffer_size);
    if (specs.alt())
      prefix_append(prefix, unsigned(specs.upper() ? 'B' : 'b') << 8 | '0');
    break;
  case presentation_type::chr:
    return write_char<Char>(out, static_cast<Char>(abs_value), specs);
  }

  // Write an integer in the format
  //   <left-padding><prefix><numeric-padding><digits><right-padding>
  // prefix contains chars in three lower bytes and the size in the fourth byte.
  int num_digits = static_cast<int>(end - begin);
  // Slightly faster check for specs.width == 0 && specs.precision == -1.
  if ((specs.width | (specs.precision + 1)) == 0) {
    auto it = reserve(out, to_unsigned(num_digits) + (prefix >> 24));
    for (unsigned p = prefix & 0xffffff; p != 0; p >>= 8)
      *it++ = static_cast<Char>(p & 0xff);
    return base_iterator(out, copy<Char>(begin, end, it));
  }
  auto sp = size_padding(num_digits, prefix, specs);
  unsigned padding = sp.padding;
  return write_padded<Char, align::right>(
      out, specs, sp.size, [=](reserve_iterator<OutputIt> it) {
        for (unsigned p = prefix & 0xffffff; p != 0; p >>= 8)
          *it++ = static_cast<Char>(p & 0xff);
        it = detail::fill_n(it, padding, static_cast<Char>('0'));
        return copy<Char>(begin, end, it);
      });
}

template <typename Char, typename OutputIt, typename T>
FMT_CONSTEXPR FMT_NOINLINE auto write_int_noinline(OutputIt out,
                                                   write_int_arg<T> arg,
                                                   const format_specs& specs)
    -> OutputIt {
  return write_int<Char>(out, arg, specs);
}

template <typename Char, typename T,
          FMT_ENABLE_IF(is_integral<T>::value &&
                        !std::is_same<T, bool>::value &&
                        !std::is_same<T, Char>::value)>
FMT_CONSTEXPR FMT_INLINE auto write(basic_appender<Char> out, T value,
                                    const format_specs& specs, locale_ref loc)
    -> basic_appender<Char> {
  if (specs.localized() && write_loc(out, value, specs, loc)) return out;
  return write_int_noinline<Char>(out, make_write_int_arg(value, specs.sign()),
                                  specs);
}

// An inlined version of write used in format string compilation.
template <typename Char, typename OutputIt, typename T,
          FMT_ENABLE_IF(is_integral<T>::value &&
                        !std::is_same<T, bool>::value &&
                        !std::is_same<T, Char>::value &&
                        !std::is_same<OutputIt, basic_appender<Char>>::value)>
FMT_CONSTEXPR FMT_INLINE auto write(OutputIt out, T value,
                                    const format_specs& specs, locale_ref loc)
    -> OutputIt {
  if (specs.localized() && write_loc(out, value, specs, loc)) return out;
  return write_int<Char>(out, make_write_int_arg(value, specs.sign()), specs);
}

template <typename Char, typename OutputIt,
          FMT_ENABLE_IF(std::is_same<Char, char>::value)>
FMT_CONSTEXPR auto write(OutputIt out, basic_string_view<Char> s,
                         const format_specs& specs) -> OutputIt {
  bool is_debug = specs.type() == presentation_type::debug;
  if (specs.precision < 0 && specs.width == 0) {
    auto&& it = reserve(out, s.size());
    return is_debug ? write_escaped_string(it, s) : copy<char>(s, it);
  }

  size_t display_width_limit =
      specs.precision < 0 ? SIZE_MAX : to_unsigned(specs.precision);
  size_t display_width =
      !is_debug || specs.precision == 0 ? 0 : 1;  // Account for opening '"'.
  size_t size = !is_debug || specs.precision == 0 ? 0 : 1;
  for_each_codepoint(s, [&](uint32_t cp, string_view sv) {
    if (is_debug && needs_escape(cp)) {
      counting_buffer<char> buf;
      write_escaped_cp(basic_appender<char>(buf),
                       find_escape_result<char>{sv.begin(), sv.end(), cp});
      // We're reinterpreting bytes as display width. That's okay
      // because write_escaped_cp() only writes ASCII characters.
      size_t cp_width = buf.count();
      if (display_width + cp_width <= display_width_limit) {
        display_width += cp_width;
        size += cp_width;
        // If this is the end of the string, account for closing '"'.
        if (display_width < display_width_limit && sv.end() == s.end()) {
          ++display_width;
          ++size;
        }
        return true;
      }

      size += display_width_limit - display_width;
      display_width = display_width_limit;
      return false;
    }

    size_t cp_width = display_width_of(cp);
    if (cp_width + display_width <= display_width_limit) {
      display_width += cp_width;
      size += sv.size();
      // If this is the end of the string, account for closing '"'.
      if (is_debug && display_width < display_width_limit &&
          sv.end() == s.end()) {
        ++display_width;
        ++size;
      }
      return true;
    }

    return false;
  });

  struct bounded_output_iterator {
    reserve_iterator<OutputIt> underlying_iterator;
    size_t bound;

    FMT_CONSTEXPR auto operator*() -> bounded_output_iterator& { return *this; }
    FMT_CONSTEXPR auto operator++() -> bounded_output_iterator& {
      return *this;
    }
    FMT_CONSTEXPR auto operator++(int) -> bounded_output_iterator& {
      return *this;
    }
    FMT_CONSTEXPR auto operator=(char c) -> bounded_output_iterator& {
      if (bound > 0) {
        *underlying_iterator++ = c;
        --bound;
      }
      return *this;
    }
  };

  return write_padded<char>(
      out, specs, size, display_width, [=](reserve_iterator<OutputIt> it) {
        return is_debug
                   ? write_escaped_string(bounded_output_iterator{it, size}, s)
                         .underlying_iterator
                   : copy<char>(s.data(), s.data() + size, it);
      });
}

template <typename Char, typename OutputIt,
          FMT_ENABLE_IF(!std::is_same<Char, char>::value)>
FMT_CONSTEXPR auto write(OutputIt out, basic_string_view<Char> s,
                         const format_specs& specs) -> OutputIt {
  auto data = s.data();
  auto size = s.size();
  if (specs.precision >= 0 && to_unsigned(specs.precision) < size)
    size = to_unsigned(specs.precision);

  bool is_debug = specs.type() == presentation_type::debug;
  if (is_debug) {
    auto buf = counting_buffer<Char>();
    write_escaped_string(basic_appender<Char>(buf), s);
    size = buf.count();
  }

  return write_padded<Char>(
      out, specs, size, [=](reserve_iterator<OutputIt> it) {
        return is_debug ? write_escaped_string(it, s)
                        : copy<Char>(data, data + size, it);
      });
}

template <typename Char, typename OutputIt>
FMT_CONSTEXPR auto write(OutputIt out, basic_string_view<Char> s,
                         const format_specs& specs, locale_ref) -> OutputIt {
  return write<Char>(out, s, specs);
}

template <typename Char, typename OutputIt>
FMT_CONSTEXPR auto write(OutputIt out, const Char* s, const format_specs& specs,
                         locale_ref) -> OutputIt {
  if (specs.type() == presentation_type::pointer)
    return write_ptr<Char>(out, bit_cast<uintptr_t>(s), &specs);
  if (!s) report_error("string pointer is null");
  return write<Char>(out, basic_string_view<Char>(s), specs, {});
}

template <typename Char, typename OutputIt, typename T,
          FMT_ENABLE_IF(is_integral<T>::value &&
                        !std::is_same<T, bool>::value &&
                        !std::is_same<T, Char>::value)>
FMT_CONSTEXPR auto write(OutputIt out, T value) -> OutputIt {
  auto abs_value = static_cast<uint32_or_64_or_128_t<T>>(value);
  bool negative = is_negative(value);
  // Don't do -abs_value since it trips unsigned-integer-overflow sanitizer.
  if (negative) abs_value = ~abs_value + 1;
  int num_digits = count_digits(abs_value);
  auto size = (negative ? 1 : 0) + static_cast<size_t>(num_digits);
  if (auto ptr = to_pointer<Char>(out, size)) {
    if (negative) *ptr++ = static_cast<Char>('-');
    format_decimal<Char>(ptr, abs_value, num_digits);
    return out;
  }
  if (negative) *out++ = static_cast<Char>('-');
  return format_decimal<Char>(out, abs_value, num_digits);
}

template <typename Char>
FMT_CONSTEXPR auto parse_align(const Char* begin, const Char* end,
                               format_specs& specs) -> const Char* {
  FMT_ASSERT(begin != end, "");
  auto alignment = align::none;
  auto p = begin + code_point_length(begin);
  if (end - p <= 0) p = begin;
  for (;;) {
    switch (to_ascii(*p)) {
    case '<': alignment = align::left; break;
    case '>': alignment = align::right; break;
    case '^': alignment = align::center; break;
    }
    if (alignment != align::none) {
      if (p != begin) {
        auto c = *begin;
        if (c == '}') return begin;
        if (c == '{') {
          report_error("invalid fill character '{'");
          return begin;
        }
        specs.set_fill(basic_string_view<Char>(begin, to_unsigned(p - begin)));
        begin = p + 1;
      } else {
        ++begin;
      }
      break;
    } else if (p == begin) {
      break;
    }
    p = begin;
  }
  specs.set_align(alignment);
  return begin;
}

template <typename Char, typename OutputIt>
FMT_CONSTEXPR20 auto write_nonfinite(OutputIt out, bool isnan,
                                     format_specs specs, sign s) -> OutputIt {
  auto str =
      isnan ? (specs.upper() ? "NAN" : "nan") : (specs.upper() ? "INF" : "inf");
  constexpr size_t str_size = 3;
  auto size = str_size + (s != sign::none ? 1 : 0);
  // Replace '0'-padding with space for non-finite values.
  const bool is_zero_fill =
      specs.fill_size() == 1 && specs.fill_unit<Char>() == '0';
  if (is_zero_fill) specs.set_fill(' ');
  return write_padded<Char>(out, specs, size,
                            [=](reserve_iterator<OutputIt> it) {
                              if (s != sign::none)
                                *it++ = detail::getsign<Char>(s);
                              return copy<Char>(str, str + str_size, it);
                            });
}

// A decimal floating-point number significand * pow(10, exp).
struct big_decimal_fp {
  const char* significand;
  int significand_size;
  int exponent;
};

constexpr auto get_significand_size(const big_decimal_fp& f) -> int {
  return f.significand_size;
}
template <typename T>
inline auto get_significand_size(const dragonbox::decimal_fp<T>& f) -> int {
  return count_digits(f.significand);
}

template <typename Char, typename OutputIt>
constexpr auto write_significand(OutputIt out, const char* significand,
                                 int significand_size) -> OutputIt {
  return copy<Char>(significand, significand + significand_size, out);
}
template <typename Char, typename OutputIt, typename UInt>
inline auto write_significand(OutputIt out, UInt significand,
                              int significand_size) -> OutputIt {
  return format_decimal<Char>(out, significand, significand_size);
}
template <typename Char, typename OutputIt, typename T, typename Grouping>
FMT_CONSTEXPR20 auto write_significand(OutputIt out, T significand,
                                       int significand_size, int exponent,
                                       const Grouping& grouping) -> OutputIt {
  if (!grouping.has_separator()) {
    out = write_significand<Char>(out, significand, significand_size);
    return detail::fill_n(out, exponent, static_cast<Char>('0'));
  }
  auto buffer = memory_buffer();
  write_significand<char>(appender(buffer), significand, significand_size);
  detail::fill_n(appender(buffer), exponent, '0');
  return grouping.apply(out, string_view(buffer.data(), buffer.size()));
}

template <typename Char, typename UInt,
          FMT_ENABLE_IF(std::is_integral<UInt>::value)>
inline auto write_significand(Char* out, UInt significand, int significand_size,
                              int integral_size, Char decimal_point) -> Char* {
  if (!decimal_point) return format_decimal(out, significand, significand_size);
  out += significand_size + 1;
  Char* end = out;
  int floating_size = significand_size - integral_size;
  for (int i = floating_size / 2; i > 0; --i) {
    out -= 2;
    write2digits(out, static_cast<size_t>(significand % 100));
    significand /= 100;
  }
  if (floating_size % 2 != 0) {
    *--out = static_cast<Char>('0' + significand % 10);
    significand /= 10;
  }
  *--out = decimal_point;
  format_decimal(out - integral_size, significand, integral_size);
  return end;
}

template <typename OutputIt, typename UInt, typename Char,
          FMT_ENABLE_IF(!std::is_pointer<remove_cvref_t<OutputIt>>::value)>
inline auto write_significand(OutputIt out, UInt significand,
                              int significand_size, int integral_size,
                              Char decimal_point) -> OutputIt {
  // Buffer is large enough to hold digits (digits10 + 1) and a decimal point.
  Char buffer[digits10<UInt>() + 2];
  auto end = write_significand(buffer, significand, significand_size,
                               integral_size, decimal_point);
  return detail::copy_noinline<Char>(buffer, end, out);
}

template <typename OutputIt, typename Char>
FMT_CONSTEXPR auto write_significand(OutputIt out, const char* significand,
                                     int significand_size, int integral_size,
                                     Char decimal_point) -> OutputIt {
  out = detail::copy_noinline<Char>(significand, significand + integral_size,
                                    out);
  if (!decimal_point) return out;
  *out++ = decimal_point;
  return detail::copy_noinline<Char>(significand + integral_size,
                                     significand + significand_size, out);
}

template <typename OutputIt, typename Char, typename T, typename Grouping>
FMT_CONSTEXPR20 auto write_significand(OutputIt out, T significand,
                                       int significand_size, int integral_size,
                                       Char decimal_point,
                                       const Grouping& grouping) -> OutputIt {
  if (!grouping.has_separator()) {
    return write_significand(out, significand, significand_size, integral_size,
                             decimal_point);
  }
  auto buffer = basic_memory_buffer<Char>();
  write_significand(basic_appender<Char>(buffer), significand, significand_size,
                    integral_size, decimal_point);
  grouping.apply(
      out, basic_string_view<Char>(buffer.data(), to_unsigned(integral_size)));
  return detail::copy_noinline<Char>(buffer.data() + integral_size,
                                     buffer.end(), out);
}

// Numbers with exponents greater or equal to the returned value will use
// the exponential notation.
template <typename T> FMT_CONSTEVAL auto exp_upper() -> int {
  return std::numeric_limits<T>::digits10 != 0
             ? min_of(16, std::numeric_limits<T>::digits10 + 1)
             : 16;
}

// Use the fixed notation if the exponent is in [-4, exp_upper),
// e.g. 0.0001 instead of 1e-04. Otherwise use the exponent notation.
constexpr auto use_fixed(int exp, int exp_upper) -> bool {
  return exp >= -4 && exp < exp_upper;
}

template <typename Char> class fallback_digit_grouping {
 public:
  constexpr fallback_digit_grouping(locale_ref, bool) {}

  constexpr auto has_separator() const -> bool { return false; }

  constexpr auto count_separators(int) const -> int { return 0; }

  template <typename Out, typename C>
  constexpr auto apply(Out out, basic_string_view<C>) const -> Out {
    return out;
  }
};

template <typename Char, typename Grouping, typename OutputIt,
          typename DecimalFP>
FMT_CONSTEXPR20 auto write_fixed(OutputIt out, const DecimalFP& f,
                                 int significand_size, Char decimal_point,
                                 const format_specs& specs, sign s,
                                 locale_ref loc = {}) -> OutputIt {
  using iterator = reserve_iterator<OutputIt>;

  int exp = f.exponent + significand_size;
  long long size = significand_size + (s != sign::none ? 1 : 0);
  if (f.exponent >= 0) {
    // 1234e5 -> 123400000[.0+]
    size += f.exponent;
    int num_zeros = specs.precision - exp;
    abort_fuzzing_if(num_zeros > 5000);
    if (specs.alt()) {
      ++size;
      if (num_zeros <= 0 && specs.type() != presentation_type::fixed)
        num_zeros = 0;
      if (num_zeros > 0) size += num_zeros;
    }
    auto grouping = Grouping(loc, specs.localized());
    size += grouping.count_separators(exp);
    return write_padded<Char, align::right>(
        out, specs, to_unsigned(size), [&](iterator it) {
          if (s != sign::none) *it++ = detail::getsign<Char>(s);
          it = write_significand<Char>(it, f.significand, significand_size,
                                       f.exponent, grouping);
          if (!specs.alt()) return it;
          *it++ = decimal_point;
          return num_zeros > 0 ? detail::fill_n(it, num_zeros, Char('0')) : it;
        });
  }
  if (exp > 0) {
    // 1234e-2 -> 12.34[0+]
    int num_zeros = specs.alt() ? specs.precision - significand_size : 0;
    size += 1 + max_of(num_zeros, 0);
    auto grouping = Grouping(loc, specs.localized());
    size += grouping.count_separators(exp);
    return write_padded<Char, align::right>(
        out, specs, to_unsigned(size), [&](iterator it) {
          if (s != sign::none) *it++ = detail::getsign<Char>(s);
          it = write_significand(it, f.significand, significand_size, exp,
                                 decimal_point, grouping);
          return num_zeros > 0 ? detail::fill_n(it, num_zeros, Char('0')) : it;
        });
  }
  // 1234e-6 -> 0.001234
  int num_zeros = -exp;
  if (significand_size == 0 && specs.precision >= 0 &&
      specs.precision < num_zeros) {
    num_zeros = specs.precision;
  }
  bool pointy = num_zeros != 0 || significand_size != 0 || specs.alt();
  size += 1 + (pointy ? 1 : 0) + num_zeros;
  return write_padded<Char, align::right>(
      out, specs, to_unsigned(size), [&](iterator it) {
        if (s != sign::none) *it++ = detail::getsign<Char>(s);
        *it++ = Char('0');
        if (!pointy) return it;
        *it++ = decimal_point;
        it = detail::fill_n(it, num_zeros, Char('0'));
        return write_significand<Char>(it, f.significand, significand_size);
      });
}

template <typename Char, typename Grouping, typename OutputIt,
          typename DecimalFP>
FMT_CONSTEXPR20 auto do_write_float(OutputIt out, const DecimalFP& f,
                                    const format_specs& specs, sign s,
                                    int exp_upper, locale_ref loc) -> OutputIt {
  Char point = specs.localized() ? detail::decimal_point<Char>(loc) : Char('.');
  int significand_size = get_significand_size(f);
  int exp = f.exponent + significand_size - 1;
  if (specs.type() == presentation_type::fixed ||
      (specs.type() != presentation_type::exp &&
       use_fixed(exp, specs.precision > 0 ? specs.precision : exp_upper))) {
    return write_fixed<Char, Grouping>(out, f, significand_size, point, specs,
                                       s, loc);
  }

  // Write value in the exponential format.
  int num_zeros = 0;
  long long size = significand_size + (s != sign::none ? 1 : 0);
  if (specs.alt()) {
    num_zeros = max_of(specs.precision - significand_size, 0);
    size += num_zeros;
  } else if (significand_size == 1) {
    point = Char();
  }
  size += (point ? 1 : 0) + compute_exp_size(exp);
  char exp_char = specs.upper() ? 'E' : 'e';
  auto write = [=](reserve_iterator<OutputIt> it) {
    if (s != sign::none) *it++ = detail::getsign<Char>(s);
    // Insert a decimal point after the first digit and add an exponent.
    it = write_significand(it, f.significand, significand_size, 1, point);
    if (num_zeros > 0) it = detail::fill_n(it, num_zeros, Char('0'));
    *it++ = Char(exp_char);
    return write_exponent<Char>(exp, it);
  };
  auto usize = to_unsigned(size);
  return specs.width > 0
             ? write_padded<Char, align::right>(out, specs, usize, write)
             : base_iterator(out, write(reserve(out, usize)));
}

template <typename Char, typename OutputIt, typename DecimalFP>
FMT_CONSTEXPR20 auto write_float(OutputIt out, const DecimalFP& f,
                                 const format_specs& specs, sign s,
                                 int exp_upper, locale_ref loc) -> OutputIt {
  if (is_constant_evaluated()) {
    return do_write_float<Char, fallback_digit_grouping<Char>>(out, f, specs, s,
                                                               exp_upper, loc);
  } else {
    return do_write_float<Char, digit_grouping<Char>>(out, f, specs, s,
                                                      exp_upper, loc);
  }
}

template <typename T> constexpr auto isnan(T value) -> bool {
  return value != value;  // std::isnan doesn't support __float128.
}

template <typename T, typename Enable = void>
struct has_isfinite : std::false_type {};

template <typename T>
struct has_isfinite<T, enable_if_t<sizeof(std::isfinite(T())) != 0>>
    : std::true_type {};

template <typename T,
          FMT_ENABLE_IF(is_floating_point<T>::value&& has_isfinite<T>::value)>
FMT_CONSTEXPR20 auto isfinite(T value) -> bool {
  constexpr T inf = T(std::numeric_limits<double>::infinity());
  if (is_constant_evaluated())
    return !detail::isnan(value) && value < inf && value > -inf;
  return std::isfinite(value);
}
template <typename T, FMT_ENABLE_IF(!has_isfinite<T>::value)>
FMT_CONSTEXPR auto isfinite(T value) -> bool {
  T inf = T(std::numeric_limits<double>::infinity());
  // std::isfinite doesn't support __float128.
  return !detail::isnan(value) && value < inf && value > -inf;
}

template <typename T, FMT_ENABLE_IF(is_floating_point<T>::value)>
FMT_INLINE FMT_CONSTEXPR bool signbit(T value) {
  if (is_constant_evaluated()) {
#ifdef __cpp_if_constexpr
    if constexpr (std::numeric_limits<double>::is_iec559) {
      auto bits = detail::bit_cast<uint64_t>(static_cast<double>(value));
      return (bits >> (num_bits<uint64_t>() - 1)) != 0;
    }
#endif
  }
  return std::signbit(static_cast<double>(value));
}

inline FMT_CONSTEXPR20 void adjust_precision(int& precision, int exp10) {
  // Adjust fixed precision by exponent because it is relative to decimal
  // point.
  if (exp10 > 0 && precision > max_value<int>() - exp10)
    FMT_THROW_FORMAT_ERROR("number is too big");
  precision += exp10;
}

class bigint {
 private:
  // A bigint is a number in the form bigit_[N - 1] ... bigit_[0] * 32^exp_.
  using bigit = uint32_t;  // A big digit.
  using double_bigit = uint64_t;
  enum { bigit_bits = num_bits<bigit>() };
  enum { bigits_capacity = 32 };
  basic_memory_buffer<bigit, bigits_capacity> bigits_;
  int exp_;

  friend struct formatter<bigint>;

  FMT_CONSTEXPR auto get_bigit(int i) const -> bigit {
    return i >= exp_ && i < num_bigits() ? bigits_[i - exp_] : 0;
  }

  FMT_CONSTEXPR void subtract_bigits(int index, bigit other, bigit& borrow) {
    auto result = double_bigit(bigits_[index]) - other - borrow;
    bigits_[index] = static_cast<bigit>(result);
    borrow = static_cast<bigit>(result >> (bigit_bits * 2 - 1));
  }

  FMT_CONSTEXPR void remove_leading_zeros() {
    int num_bigits = static_cast<int>(bigits_.size()) - 1;
    while (num_bigits > 0 && bigits_[num_bigits] == 0) --num_bigits;
    bigits_.resize(to_unsigned(num_bigits + 1));
  }

  // Computes *this -= other assuming aligned bigints and *this >= other.
  FMT_CONSTEXPR void subtract_aligned(const bigint& other) {
    FMT_ASSERT(other.exp_ >= exp_, "unaligned bigints");
    FMT_ASSERT(compare(*this, other) >= 0, "");
    bigit borrow = 0;
    int i = other.exp_ - exp_;
    for (size_t j = 0, n = other.bigits_.size(); j != n; ++i, ++j)
      subtract_bigits(i, other.bigits_[j], borrow);
    if (borrow != 0) subtract_bigits(i, 0, borrow);
    FMT_ASSERT(borrow == 0, "");
    remove_leading_zeros();
  }

  FMT_CONSTEXPR void multiply(uint32_t value) {
    bigit carry = 0;
    const double_bigit wide_value = value;
    for (size_t i = 0, n = bigits_.size(); i < n; ++i) {
      double_bigit result = bigits_[i] * wide_value + carry;
      bigits_[i] = static_cast<bigit>(result);
      carry = static_cast<bigit>(result >> bigit_bits);
    }
    if (carry != 0) bigits_.push_back(carry);
  }

  template <typename UInt, FMT_ENABLE_IF(std::is_same<UInt, uint64_t>::value ||
                                         std::is_same<UInt, uint128_t>::value)>
  FMT_CONSTEXPR void multiply(UInt value) {
    using half_uint =
        conditional_t<std::is_same<UInt, uint128_t>::value, uint64_t, uint32_t>;
    const int shift = num_bits<half_uint>() - bigit_bits;
    const UInt lower = static_cast<half_uint>(value);
    const UInt upper = value >> num_bits<half_uint>();
    UInt carry = 0;
    for (size_t i = 0, n = bigits_.size(); i < n; ++i) {
      UInt result = lower * bigits_[i] + static_cast<bigit>(carry);
      carry = (upper * bigits_[i] << shift) + (result >> bigit_bits) +
              (carry >> bigit_bits);
      bigits_[i] = static_cast<bigit>(result);
    }
    while (carry != 0) {
      bigits_.push_back(static_cast<bigit>(carry));
      carry >>= bigit_bits;
    }
  }

  template <typename UInt, FMT_ENABLE_IF(std::is_same<UInt, uint64_t>::value ||
                                         std::is_same<UInt, uint128_t>::value)>
  FMT_CONSTEXPR void assign(UInt n) {
    size_t num_bigits = 0;
    do {
      bigits_[num_bigits++] = static_cast<bigit>(n);
      n >>= bigit_bits;
    } while (n != 0);
    bigits_.resize(num_bigits);
    exp_ = 0;
  }

 public:
  FMT_CONSTEXPR bigint() : exp_(0) {}
  explicit bigint(uint64_t n) { assign(n); }

  bigint(const bigint&) = delete;
  void operator=(const bigint&) = delete;

  FMT_CONSTEXPR void assign(const bigint& other) {
    auto size = other.bigits_.size();
    bigits_.resize(size);
    auto data = other.bigits_.data();
    copy<bigit>(data, data + size, bigits_.data());
    exp_ = other.exp_;
  }

  template <typename Int> FMT_CONSTEXPR void operator=(Int n) {
    FMT_ASSERT(n > 0, "");
    assign(uint64_or_128_t<Int>(n));
  }

  FMT_CONSTEXPR auto num_bigits() const -> int {
    return static_cast<int>(bigits_.size()) + exp_;
  }

  FMT_CONSTEXPR auto operator<<=(int shift) -> bigint& {
    FMT_ASSERT(shift >= 0, "");
    exp_ += shift / bigit_bits;
    shift %= bigit_bits;
    if (shift == 0) return *this;
    bigit carry = 0;
    for (size_t i = 0, n = bigits_.size(); i < n; ++i) {
      bigit c = bigits_[i] >> (bigit_bits - shift);
      bigits_[i] = (bigits_[i] << shift) + carry;
      carry = c;
    }
    if (carry != 0) bigits_.push_back(carry);
    return *this;
  }

  template <typename Int> FMT_CONSTEXPR auto operator*=(Int value) -> bigint& {
    FMT_ASSERT(value > 0, "");
    multiply(uint32_or_64_or_128_t<Int>(value));
    return *this;
  }

  friend FMT_CONSTEXPR auto compare(const bigint& b1, const bigint& b2) -> int {
    int num_bigits1 = b1.num_bigits(), num_bigits2 = b2.num_bigits();
    if (num_bigits1 != num_bigits2) return num_bigits1 > num_bigits2 ? 1 : -1;
    int i = static_cast<int>(b1.bigits_.size()) - 1;
    int j = static_cast<int>(b2.bigits_.size()) - 1;
    int end = i - j;
    if (end < 0) end = 0;
    for (; i >= end; --i, --j) {
      bigit b1_bigit = b1.bigits_[i], b2_bigit = b2.bigits_[j];
      if (b1_bigit != b2_bigit) return b1_bigit > b2_bigit ? 1 : -1;
    }
    if (i != j) return i > j ? 1 : -1;
    return 0;
  }

  // Returns compare(lhs1 + lhs2, rhs).
  friend FMT_CONSTEXPR auto add_compare(const bigint& lhs1, const bigint& lhs2,
                                        const bigint& rhs) -> int {
    int max_lhs_bigits = max_of(lhs1.num_bigits(), lhs2.num_bigits());
    int num_rhs_bigits = rhs.num_bigits();
    if (max_lhs_bigits + 1 < num_rhs_bigits) return -1;
    if (max_lhs_bigits > num_rhs_bigits) return 1;
    double_bigit borrow = 0;
    int min_exp = min_of(min_of(lhs1.exp_, lhs2.exp_), rhs.exp_);
    for (int i = num_rhs_bigits - 1; i >= min_exp; --i) {
      double_bigit sum = double_bigit(lhs1.get_bigit(i)) + lhs2.get_bigit(i);
      bigit rhs_bigit = rhs.get_bigit(i);
      if (sum > rhs_bigit + borrow) return 1;
      borrow = rhs_bigit + borrow - sum;
      if (borrow > 1) return -1;
      borrow <<= bigit_bits;
    }
    return borrow != 0 ? -1 : 0;
  }

  // Assigns pow(10, exp) to this bigint.
  FMT_CONSTEXPR20 void assign_pow10(int exp) {
    FMT_ASSERT(exp >= 0, "");
    if (exp == 0) return *this = 1;
    int bitmask = 1 << (num_bits<unsigned>() -
                        countl_zero(static_cast<uint32_t>(exp)) - 1);
    // pow(10, exp) = pow(5, exp) * pow(2, exp). First compute pow(5, exp) by
    // repeated squaring and multiplication.
    *this = 5;
    bitmask >>= 1;
    while (bitmask != 0) {
      square();
      if ((exp & bitmask) != 0) *this *= 5;
      bitmask >>= 1;
    }
    *this <<= exp;  // Multiply by pow(2, exp) by shifting.
  }

  FMT_CONSTEXPR20 void square() {
    int num_bigits = static_cast<int>(bigits_.size());
    int num_result_bigits = 2 * num_bigits;
    basic_memory_buffer<bigit, bigits_capacity> n(std::move(bigits_));
    bigits_.resize(to_unsigned(num_result_bigits));
    auto sum = uint128_t();
    for (int bigit_index = 0; bigit_index < num_bigits; ++bigit_index) {
      // Compute bigit at position bigit_index of the result by adding
      // cross-product terms n[i] * n[j] such that i + j == bigit_index.
      for (int i = 0, j = bigit_index; j >= 0; ++i, --j) {
        // Most terms are multiplied twice which can be optimized in the future.
        sum += double_bigit(n[i]) * n[j];
      }
      bigits_[bigit_index] = static_cast<bigit>(sum);
      sum >>= num_bits<bigit>();  // Compute the carry.
    }
    // Do the same for the top half.
    for (int bigit_index = num_bigits; bigit_index < num_result_bigits;
         ++bigit_index) {
      for (int j = num_bigits - 1, i = bigit_index - j; i < num_bigits;)
        sum += double_bigit(n[i++]) * n[j--];
      bigits_[bigit_index] = static_cast<bigit>(sum);
      sum >>= num_bits<bigit>();
    }
    remove_leading_zeros();
    exp_ *= 2;
  }

  // If this bigint has a bigger exponent than other, adds trailing zero to make
  // exponents equal. This simplifies some operations such as subtraction.
  FMT_CONSTEXPR void align(const bigint& other) {
    int exp_difference = exp_ - other.exp_;
    if (exp_difference <= 0) return;
    int num_bigits = static_cast<int>(bigits_.size());
    bigits_.resize(to_unsigned(num_bigits + exp_difference));
    for (int i = num_bigits - 1, j = i + exp_difference; i >= 0; --i, --j)
      bigits_[j] = bigits_[i];
    fill_n(bigits_.data(), to_unsigned(exp_difference), 0U);
    exp_ -= exp_difference;
  }

  // Divides this bignum by divisor, assigning the remainder to this and
  // returning the quotient.
  FMT_CONSTEXPR auto divmod_assign(const bigint& divisor) -> int {
    FMT_ASSERT(this != &divisor, "");
    if (compare(*this, divisor) < 0) return 0;
    FMT_ASSERT(divisor.bigits_[divisor.bigits_.size() - 1u] != 0, "");
    align(divisor);
    int quotient = 0;
    do {
      subtract_aligned(divisor);
      ++quotient;
    } while (compare(*this, divisor) >= 0);
    return quotient;
  }
};

// format_dragon flags.
enum dragon {
  predecessor_closer = 1,
  fixup = 2,  // Run fixup to correct exp10 which can be off by one.
  fixed = 4,
};

// Formats a floating-point number using a variation of the Fixed-Precision
// Positive Floating-Point Printout ((FPP)^2) algorithm by Steele & White:
// https://fmt.dev/papers/p372-steele.pdf.
FMT_CONSTEXPR20 inline void format_dragon(basic_fp<uint128_t> value,
                                          unsigned flags, int num_digits,
                                          buffer<char>& buf, int& exp10) {
  bigint numerator;    // 2 * R in (FPP)^2.
  bigint denominator;  // 2 * S in (FPP)^2.
  // lower and upper are differences between value and corresponding boundaries.
  bigint lower;             // (M^- in (FPP)^2).
  bigint upper_store;       // upper's value if different from lower.
  bigint* upper = nullptr;  // (M^+ in (FPP)^2).
  // Shift numerator and denominator by an extra bit or two (if lower boundary
  // is closer) to make lower and upper integers. This eliminates multiplication
  // by 2 during later computations.
  bool is_predecessor_closer = (flags & dragon::predecessor_closer) != 0;
  int shift = is_predecessor_closer ? 2 : 1;
  if (value.e >= 0) {
    numerator = value.f;
    numerator <<= value.e + shift;
    lower = 1;
    lower <<= value.e;
    if (is_predecessor_closer) {
      upper_store = 1;
      upper_store <<= value.e + 1;
      upper = &upper_store;
    }
    denominator.assign_pow10(exp10);
    denominator <<= shift;
  } else if (exp10 < 0) {
    numerator.assign_pow10(-exp10);
    lower.assign(numerator);
    if (is_predecessor_closer) {
      upper_store.assign(numerator);
      upper_store <<= 1;
      upper = &upper_store;
    }
    numerator *= value.f;
    numerator <<= shift;
    denominator = 1;
    denominator <<= shift - value.e;
  } else {
    numerator = value.f;
    numerator <<= shift;
    denominator.assign_pow10(exp10);
    denominator <<= shift - value.e;
    lower = 1;
    if (is_predecessor_closer) {
      upper_store = 1ULL << 1;
      upper = &upper_store;
    }
  }
  int even = static_cast<int>((value.f & 1) == 0);
  if (!upper) upper = &lower;
  bool shortest = num_digits < 0;
  if ((flags & dragon::fixup) != 0) {
    if (add_compare(numerator, *upper, denominator) + even <= 0) {
      --exp10;
      numerator *= 10;
      if (num_digits < 0) {
        lower *= 10;
        if (upper != &lower) *upper *= 10;
      }
    }
    if ((flags & dragon::fixed) != 0) adjust_precision(num_digits, exp10 + 1);
  }
  // Invariant: value == (numerator / denominator) * pow(10, exp10).
  if (shortest) {
    // Generate the shortest representation.
    num_digits = 0;
    char* data = buf.data();
    for (;;) {
      int digit = numerator.divmod_assign(denominator);
      bool low = compare(numerator, lower) - even < 0;  // numerator <[=] lower.
      // numerator + upper >[=] pow10:
      bool high = add_compare(numerator, *upper, denominator) + even > 0;
      data[num_digits++] = static_cast<char>('0' + digit);
      if (low || high) {
        if (!low) {
          ++data[num_digits - 1];
        } else if (high) {
          int result = add_compare(numerator, numerator, denominator);
          // Round half to even.
          if (result > 0 || (result == 0 && (digit % 2) != 0))
            ++data[num_digits - 1];
        }
        buf.try_resize(to_unsigned(num_digits));
        exp10 -= num_digits - 1;
        return;
      }
      numerator *= 10;
      lower *= 10;
      if (upper != &lower) *upper *= 10;
    }
  }
  // Generate the given number of digits.
  exp10 -= num_digits - 1;
  if (num_digits <= 0) {
    auto digit = '0';
    if (num_digits == 0) {
      denominator *= 10;
      digit = add_compare(numerator, numerator, denominator) > 0 ? '1' : '0';
    }
    buf.push_back(digit);
    return;
  }
  buf.try_resize(to_unsigned(num_digits));
  for (int i = 0; i < num_digits - 1; ++i) {
    int digit = numerator.divmod_assign(denominator);
    buf[i] = static_cast<char>('0' + digit);
    numerator *= 10;
  }
  int digit = numerator.divmod_assign(denominator);
  auto result = add_compare(numerator, numerator, denominator);
  if (result > 0 || (result == 0 && (digit % 2) != 0)) {
    if (digit == 9) {
      const auto overflow = '0' + 10;
      buf[num_digits - 1] = overflow;
      // Propagate the carry.
      for (int i = num_digits - 1; i > 0 && buf[i] == overflow; --i) {
        buf[i] = '0';
        ++buf[i - 1];
      }
      if (buf[0] == overflow) {
        buf[0] = '1';
        if ((flags & dragon::fixed) != 0)
          buf.push_back('0');
        else
          ++exp10;
      }
      return;
    }
    ++digit;
  }
  buf[num_digits - 1] = static_cast<char>('0' + digit);
}

// Formats a floating-point number using the hexfloat format.
template <typename Float, FMT_ENABLE_IF(!is_double_double<Float>::value)>
FMT_CONSTEXPR20 void format_hexfloat(Float value, format_specs specs,
                                     buffer<char>& buf) {
  // float is passed as double to reduce the number of instantiations and to
  // simplify implementation.
  static_assert(!std::is_same<Float, float>::value, "");

  using info = dragonbox::float_info<Float>;

  // Assume Float is in the format [sign][exponent][significand].
  using carrier_uint = typename info::carrier_uint;

  const auto num_float_significand_bits = detail::num_significand_bits<Float>();

  basic_fp<carrier_uint> f(value);
  f.e += num_float_significand_bits;
  if (!has_implicit_bit<Float>()) --f.e;

  const auto num_fraction_bits =
      num_float_significand_bits + (has_implicit_bit<Float>() ? 1 : 0);
  const auto num_xdigits = (num_fraction_bits + 3) / 4;

  const auto leading_shift = ((num_xdigits - 1) * 4);
  const auto leading_mask = carrier_uint(0xF) << leading_shift;
  const auto leading_xdigit =
      static_cast<uint32_t>((f.f & leading_mask) >> leading_shift);
  if (leading_xdigit > 1) f.e -= (32 - countl_zero(leading_xdigit) - 1);

  int print_xdigits = num_xdigits - 1;
  if (specs.precision >= 0 && print_xdigits > specs.precision) {
    const int shift = ((print_xdigits - specs.precision - 1) * 4);
    const auto mask = carrier_uint(0xF) << shift;
    const auto v = static_cast<uint32_t>((f.f & mask) >> shift);

    if (v >= 8) {
      const auto inc = carrier_uint(1) << (shift + 4);
      f.f += inc;
      f.f &= ~(inc - 1);
    }

    // Check long double overflow
    if (!has_implicit_bit<Float>()) {
      const auto implicit_bit = carrier_uint(1) << num_float_significand_bits;
      if ((f.f & implicit_bit) == implicit_bit) {
        f.f >>= 4;
        f.e += 4;
      }
    }

    print_xdigits = specs.precision;
  }

  char xdigits[num_bits<carrier_uint>() / 4];
  detail::fill_n(xdigits, sizeof(xdigits), '0');
  format_base2e(4, xdigits, f.f, num_xdigits, specs.upper());

  // Remove zero tail
  while (print_xdigits > 0 && xdigits[print_xdigits] == '0') --print_xdigits;

  buf.push_back('0');
  buf.push_back(specs.upper() ? 'X' : 'x');
  buf.push_back(xdigits[0]);
  if (specs.alt() || print_xdigits > 0 || print_xdigits < specs.precision)
    buf.push_back('.');
  buf.append(xdigits + 1, xdigits + 1 + print_xdigits);
  for (; print_xdigits < specs.precision; ++print_xdigits) buf.push_back('0');

  buf.push_back(specs.upper() ? 'P' : 'p');

  uint32_t abs_e;
  if (f.e < 0) {
    buf.push_back('-');
    abs_e = static_cast<uint32_t>(-f.e);
  } else {
    buf.push_back('+');
    abs_e = static_cast<uint32_t>(f.e);
  }
  format_decimal<char>(appender(buf), abs_e, detail::count_digits(abs_e));
}

template <typename Float, FMT_ENABLE_IF(is_double_double<Float>::value)>
FMT_CONSTEXPR20 void format_hexfloat(Float value, format_specs specs,
                                     buffer<char>& buf) {
  format_hexfloat(static_cast<double>(value), specs, buf);
}

constexpr auto fractional_part_rounding_thresholds(int index) -> uint32_t {
  // For checking rounding thresholds.
  // The kth entry is chosen to be the smallest integer such that the
  // upper 32-bits of 10^(k+1) times it is strictly bigger than 5 * 10^k.
  // It is equal to ceil(2^31 + 2^32/10^(k + 1)).
  // These are stored in a string literal because we cannot have static arrays
  // in constexpr functions and non-static ones are poorly optimized.
  return U"\x9999999a\x828f5c29\x80418938\x80068db9\x8000a7c6\x800010c7"
         U"\x800001ae\x8000002b"[index];
}

template <typename Float>
FMT_CONSTEXPR20 auto format_float(Float value, int precision,
                                  const format_specs& specs, bool binary32,
                                  buffer<char>& buf) -> int {
  // float is passed as double to reduce the number of instantiations.
  static_assert(!std::is_same<Float, float>::value, "");
  auto converted_value = convert_float(value);

  const bool fixed = specs.type() == presentation_type::fixed;
  if (value == 0) {
    if (precision <= 0 || !fixed) {
      buf.push_back('0');
      return 0;
    }
    buf.try_resize(to_unsigned(precision));
    fill_n(buf.data(), precision, '0');
    return -precision;
  }

  int exp = 0;
  bool use_dragon = true;
  unsigned dragon_flags = 0;
  if (!is_fast_float<Float>() || is_constant_evaluated()) {
    const auto inv_log2_10 = 0.3010299956639812;  // 1 / log2(10)
    using info = dragonbox::float_info<decltype(converted_value)>;
    const auto f = basic_fp<typename info::carrier_uint>(converted_value);
    // Compute exp, an approximate power of 10, such that
    //   10^(exp - 1) <= value < 10^exp or 10^exp <= value < 10^(exp + 1).
    // This is based on log10(value) == log2(value) / log2(10) and approximation
    // of log2(value) by e + num_fraction_bits idea from double-conversion.
    auto e = (f.e + count_digits<1>(f.f) - 1) * inv_log2_10 - 1e-10;
    exp = static_cast<int>(e);
    if (e > exp) ++exp;  // Compute ceil.
    dragon_flags = dragon::fixup;
  } else {
    // Extract significand bits and exponent bits.
    using info = dragonbox::float_info<double>;
    auto br = bit_cast<uint64_t>(static_cast<double>(value));

    const uint64_t significand_mask =
        (static_cast<uint64_t>(1) << num_significand_bits<double>()) - 1;
    uint64_t significand = (br & significand_mask);
    int exponent = static_cast<int>((br & exponent_mask<double>()) >>
                                    num_significand_bits<double>());

    if (exponent != 0) {  // Check if normal.
      exponent -= exponent_bias<double>() + num_significand_bits<double>();
      significand |=
          (static_cast<uint64_t>(1) << num_significand_bits<double>());
      significand <<= 1;
    } else {
      // Normalize subnormal inputs.
      FMT_ASSERT(significand != 0, "zeros should not appear here");
      int shift = countl_zero(significand);
      FMT_ASSERT(shift >= num_bits<uint64_t>() - num_significand_bits<double>(),
                 "");
      shift -= (num_bits<uint64_t>() - num_significand_bits<double>() - 2);
      exponent = (std::numeric_limits<double>::min_exponent -
                  num_significand_bits<double>()) -
                 shift;
      significand <<= shift;
    }

    // Compute the first several nonzero decimal significand digits.
    // We call the number we get the first segment.
    const int k = info::kappa - dragonbox::floor_log10_pow2(exponent);
    exp = -k;
    const int beta = exponent + dragonbox::floor_log2_pow10(k);
    uint64_t first_segment;
    bool has_more_segments;
    int digits_in_the_first_segment;
    {
      const auto r = dragonbox::umul192_upper128(
          significand << beta, dragonbox::get_cached_power(k));
      first_segment = r.high();
      has_more_segments = r.low() != 0;

      // The first segment can have 18 ~ 19 digits.
      if (first_segment >= 1000000000000000000ULL) {
        digits_in_the_first_segment = 19;
      } else {
        // When it is of 18-digits, we align it to 19-digits by adding a bogus
        // zero at the end.
        digits_in_the_first_segment = 18;
        first_segment *= 10;
      }
    }

    // Compute the actual number of decimal digits to print.
    if (fixed) adjust_precision(precision, exp + digits_in_the_first_segment);

    // Use Dragon4 only when there might be not enough digits in the first
    // segment.
    if (digits_in_the_first_segment > precision) {
      use_dragon = false;

      if (precision <= 0) {
        exp += digits_in_the_first_segment;

        if (precision < 0) {
          // Nothing to do, since all we have are just leading zeros.
          buf.try_resize(0);
        } else {
          // We may need to round-up.
          buf.try_resize(1);
          if ((first_segment | static_cast<uint64_t>(has_more_segments)) >
              5000000000000000000ULL) {
            buf[0] = '1';
          } else {
            buf[0] = '0';
          }
        }
      }  // precision <= 0
      else {
        exp += digits_in_the_first_segment - precision;

        // When precision > 0, we divide the first segment into three
        // subsegments, each with 9, 9, and 0 ~ 1 digits so that each fits
        // in 32-bits which usually allows faster calculation than in
        // 64-bits. Since some compiler (e.g. MSVC) doesn't know how to optimize
        // division-by-constant for large 64-bit divisors, we do it here
        // manually. The magic number 7922816251426433760 below is equal to
        // ceil(2^(64+32) / 10^10).
        const uint32_t first_subsegment = static_cast<uint32_t>(
            dragonbox::umul128_upper64(first_segment, 7922816251426433760ULL) >>
            32);
        const uint64_t second_third_subsegments =
            first_segment - first_subsegment * 10000000000ULL;

        uint64_t prod;
        uint32_t digits;
        bool should_round_up;
        int number_of_digits_to_print = min_of(precision, 9);

        // Print a 9-digits subsegment, either the first or the second.
        auto print_subsegment = [&](uint32_t subsegment, char* buffer) {
          int number_of_digits_printed = 0;

          // If we want to print an odd number of digits from the subsegment,
          if ((number_of_digits_to_print & 1) != 0) {
            // Convert to 64-bit fixed-point fractional form with 1-digit
            // integer part. The magic number 720575941 is a good enough
            // approximation of 2^(32 + 24) / 10^8; see
            // https://jk-jeon.github.io/posts/2022/12/fixed-precision-formatting/#fixed-length-case
            // for details.
            prod = ((subsegment * static_cast<uint64_t>(720575941)) >> 24) + 1;
            digits = static_cast<uint32_t>(prod >> 32);
            *buffer = static_cast<char>('0' + digits);
            number_of_digits_printed++;
          }
          // If we want to print an even number of digits from the
          // first_subsegment,
          else {
            // Convert to 64-bit fixed-point fractional form with 2-digits
            // integer part. The magic number 450359963 is a good enough
            // approximation of 2^(32 + 20) / 10^7; see
            // https://jk-jeon.github.io/posts/2022/12/fixed-precision-formatting/#fixed-length-case
            // for details.
            prod = ((subsegment * static_cast<uint64_t>(450359963)) >> 20) + 1;
            digits = static_cast<uint32_t>(prod >> 32);
            write2digits(buffer, digits);
            number_of_digits_printed += 2;
          }

          // Print all digit pairs.
          while (number_of_digits_printed < number_of_digits_to_print) {
            prod = static_cast<uint32_t>(prod) * static_cast<uint64_t>(100);
            digits = static_cast<uint32_t>(prod >> 32);
            write2digits(buffer + number_of_digits_printed, digits);
            number_of_digits_printed += 2;
          }
        };

        // Print first subsegment.
        print_subsegment(first_subsegment, buf.data());

        // Perform rounding if the first subsegment is the last subsegment to
        // print.
        if (precision <= 9) {
          // Rounding inside the subsegment.
          // We round-up if:
          //  - either the fractional part is strictly larger than 1/2, or
          //  - the fractional part is exactly 1/2 and the last digit is odd.
          // We rely on the following observations:
          //  - If fractional_part >= threshold, then the fractional part is
          //    strictly larger than 1/2.
          //  - If the MSB of fractional_part is set, then the fractional part
          //    must be at least 1/2.
          //  - When the MSB of fractional_part is set, either
          //    second_third_subsegments being nonzero or has_more_segments
          //    being true means there are further digits not printed, so the
          //    fractional part is strictly larger than 1/2.
          if (precision < 9) {
            uint32_t fractional_part = static_cast<uint32_t>(prod);
            should_round_up =
                fractional_part >= fractional_part_rounding_thresholds(
                                       8 - number_of_digits_to_print) ||
                ((fractional_part >> 31) &
                 ((digits & 1) | (second_third_subsegments != 0) |
                  has_more_segments)) != 0;
          }
          // Rounding at the subsegment boundary.
          // In this case, the fractional part is at least 1/2 if and only if
          // second_third_subsegments >= 5000000000ULL, and is strictly larger
          // than 1/2 if we further have either second_third_subsegments >
          // 5000000000ULL or has_more_segments == true.
          else {
            should_round_up = second_third_subsegments > 5000000000ULL ||
                              (second_third_subsegments == 5000000000ULL &&
                               ((digits & 1) != 0 || has_more_segments));
          }
        }
        // Otherwise, print the second subsegment.
        else {
          // Compilers are not aware of how to leverage the maximum value of
          // second_third_subsegments to find out a better magic number which
          // allows us to eliminate an additional shift. 1844674407370955162 =
          // ceil(2^64/10) < ceil(2^64*(10^9/(10^10 - 1))).
          const uint32_t second_subsegment =
              static_cast<uint32_t>(dragonbox::umul128_upper64(
                  second_third_subsegments, 1844674407370955162ULL));
          const uint32_t third_subsegment =
              static_cast<uint32_t>(second_third_subsegments) -
              second_subsegment * 10;

          number_of_digits_to_print = precision - 9;
          print_subsegment(second_subsegment, buf.data() + 9);

          // Rounding inside the subsegment.
          if (precision < 18) {
            // The condition third_subsegment != 0 implies that the segment was
            // of 19 digits, so in this case the third segment should be
            // consisting of a genuine digit from the input.
            uint32_t fractional_part = static_cast<uint32_t>(prod);
            should_round_up =
                fractional_part >= fractional_part_rounding_thresholds(
                                       8 - number_of_digits_to_print) ||
                ((fractional_part >> 31) &
                 ((digits & 1) | (third_subsegment != 0) |
                  has_more_segments)) != 0;
          }
          // Rounding at the subsegment boundary.
          else {
            // In this case, the segment must be of 19 digits, thus
            // the third subsegment should be consisting of a genuine digit from
            // the input.
            should_round_up = third_subsegment > 5 ||
                              (third_subsegment == 5 &&
                               ((digits & 1) != 0 || has_more_segments));
          }
        }

        // Round-up if necessary.
        if (should_round_up) {
          ++buf[precision - 1];
          for (int i = precision - 1; i > 0 && buf[i] > '9'; --i) {
            buf[i] = '0';
            ++buf[i - 1];
          }
          if (buf[0] > '9') {
            buf[0] = '1';
            if (fixed)
              buf[precision++] = '0';
            else
              ++exp;
          }
        }
        buf.try_resize(to_unsigned(precision));
      }
    }  // if (digits_in_the_first_segment > precision)
    else {
      // Adjust the exponent for its use in Dragon4.
      exp += digits_in_the_first_segment - 1;
    }
  }
  if (use_dragon) {
    auto f = basic_fp<uint128_t>();
    bool is_predecessor_closer = binary32 ? f.assign(static_cast<float>(value))
                                          : f.assign(converted_value);
    if (is_predecessor_closer) dragon_flags |= dragon::predecessor_closer;
    if (fixed) dragon_flags |= dragon::fixed;
    // Limit precision to the maximum possible number of significant digits in
    // an IEEE754 double because we don't need to generate zeros.
    const int max_double_digits = 767;
    if (precision > max_double_digits) precision = max_double_digits;
    format_dragon(f, dragon_flags, precision, buf, exp);
  }
  if (!fixed && !specs.alt()) {
    // Remove trailing zeros.
    auto num_digits = buf.size();
    while (num_digits > 0 && buf[num_digits - 1] == '0') {
      --num_digits;
      ++exp;
    }
    buf.try_resize(num_digits);
  }
  return exp;
}

template <typename Char, typename OutputIt, typename T,
          FMT_ENABLE_IF(is_floating_point<T>::value)>
FMT_CONSTEXPR20 auto write(OutputIt out, T value, format_specs specs,
                           locale_ref loc = {}) -> OutputIt {
  if (specs.localized() && write_loc(out, value, specs, loc)) return out;

  // Use signbit because value < 0 is false for NaN.
  sign s = detail::signbit(value) ? sign::minus : specs.sign();

  if (!detail::isfinite(value))
    return write_nonfinite<Char>(out, detail::isnan(value), specs, s);

  if (specs.align() == align::numeric && s != sign::none) {
    *out++ = detail::getsign<Char>(s);
    s = sign::none;
    if (specs.width != 0) --specs.width;
  }

  const int exp_upper = detail::exp_upper<T>();
  int precision = specs.precision;
  if (precision < 0) {
    if (specs.type() != presentation_type::none) {
      precision = 6;
    } else if (is_fast_float<T>::value && !is_constant_evaluated()) {
      // Use Dragonbox for the shortest format.
      auto dec = dragonbox::to_decimal(static_cast<fast_float_t<T>>(value));
      return write_float<Char>(out, dec, specs, s, exp_upper, loc);
    }
  }

  memory_buffer buffer;
  if (specs.type() == presentation_type::hexfloat) {
    if (s != sign::none) buffer.push_back(detail::getsign<char>(s));
    format_hexfloat(convert_float(value), specs, buffer);
    return write_bytes<Char, align::right>(out, {buffer.data(), buffer.size()},
                                           specs);
  }

  if (specs.type() == presentation_type::exp) {
    if (precision == max_value<int>())
      report_error("number is too big");
    else
      ++precision;
    if (specs.precision != 0) specs.set_alt();
  } else if (specs.type() == presentation_type::fixed) {
    if (specs.precision != 0) specs.set_alt();
  } else if (precision == 0) {
    precision = 1;
  }
  int exp = format_float(convert_float(value), precision, specs,
                         std::is_same<T, float>(), buffer);

  specs.precision = precision;
  auto f = big_decimal_fp{buffer.data(), static_cast<int>(buffer.size()), exp};
  return write_float<Char>(out, f, specs, s, exp_upper, loc);
}

template <typename Char, typename OutputIt, typename T,
          FMT_ENABLE_IF(is_fast_float<T>::value)>
FMT_CONSTEXPR20 auto write(OutputIt out, T value) -> OutputIt {
  if (is_constant_evaluated()) return write<Char>(out, value, format_specs());

  auto s = detail::signbit(value) ? sign::minus : sign::none;
  auto mask = exponent_mask<fast_float_t<T>>();
  if ((bit_cast<decltype(mask)>(value) & mask) == mask)
    return write_nonfinite<Char>(out, std::isnan(value), {}, s);

  auto dec = dragonbox::to_decimal(static_cast<fast_float_t<T>>(value));
  int significand_size = count_digits(dec.significand);
  int exp = dec.exponent + significand_size - 1;
  if (use_fixed(exp, detail::exp_upper<T>())) {
    return write_fixed<Char, fallback_digit_grouping<Char>>(
        out, dec, significand_size, Char('.'), {}, s);
  }

  // Write value in the exponential format.
  auto has_decimal_point = significand_size != 1;
  size_t size =
      to_unsigned((s != sign::none ? 1 : 0) + significand_size +
                  (has_decimal_point ? 1 : 0) + compute_exp_size(exp));
  auto it = reserve(out, size);
  if (s != sign::none) *it++ = Char('-');
  // Insert a decimal point after the first digit and add an exponent.
  it = write_significand(it, dec.significand, significand_size, 1,
                         has_decimal_point ? Char('.') : Char());
  *it++ = Char('e');
  it = write_exponent<Char>(exp, it);
  return base_iterator(out, it);
}

template <typename Char, typename OutputIt, typename T,
          FMT_ENABLE_IF(is_floating_point<T>::value &&
                        !is_fast_float<T>::value)>
inline auto write(OutputIt out, T value) -> OutputIt {
  return write<Char>(out, value, {});
}

template <typename Char, typename OutputIt>
auto write(OutputIt out, monostate, format_specs = {}, locale_ref = {})
    -> OutputIt {
  FMT_ASSERT(false, "");
  return out;
}

template <typename Char, typename OutputIt>
FMT_CONSTEXPR auto write(OutputIt out, basic_string_view<Char> value)
    -> OutputIt {
  return copy_noinline<Char>(value.begin(), value.end(), out);
}

template <typename Char, typename OutputIt, typename T,
          FMT_ENABLE_IF(has_to_string_view<T>::value)>
constexpr auto write(OutputIt out, const T& value) -> OutputIt {
  return write<Char>(out, to_string_view(value));
}

// FMT_ENABLE_IF() condition separated to workaround an MSVC bug.
template <
    typename Char, typename OutputIt, typename T,
    bool check = std::is_enum<T>::value && !std::is_same<T, Char>::value &&
                 mapped_type_constant<T, Char>::value != type::custom_type,
    FMT_ENABLE_IF(check)>
FMT_CONSTEXPR auto write(OutputIt out, T value) -> OutputIt {
  return write<Char>(out, static_cast<underlying_t<T>>(value));
}

template <typename Char, typename OutputIt, typename T,
          FMT_ENABLE_IF(std::is_same<T, bool>::value)>
FMT_CONSTEXPR auto write(OutputIt out, T value, const format_specs& specs = {},
                         locale_ref = {}) -> OutputIt {
  return specs.type() != presentation_type::none &&
                 specs.type() != presentation_type::string
             ? write<Char>(out, value ? 1 : 0, specs, {})
             : write_bytes<Char>(out, value ? "true" : "false", specs);
}

template <typename Char, typename OutputIt>
FMT_CONSTEXPR auto write(OutputIt out, Char value) -> OutputIt {
  auto it = reserve(out, 1);
  *it++ = value;
  return base_iterator(out, it);
}

template <typename Char, typename OutputIt>
FMT_CONSTEXPR20 auto write(OutputIt out, const Char* value) -> OutputIt {
  if (value) return write(out, basic_string_view<Char>(value));
  report_error("string pointer is null");
  return out;
}

template <typename Char, typename OutputIt, typename T,
          FMT_ENABLE_IF(std::is_same<T, void>::value)>
auto write(OutputIt out, const T* value, const format_specs& specs = {},
           locale_ref = {}) -> OutputIt {
  return write_ptr<Char>(out, bit_cast<uintptr_t>(value), &specs);
}

template <typename Char, typename OutputIt, typename T,
          FMT_ENABLE_IF(mapped_type_constant<T, Char>::value ==
                            type::custom_type &&
                        !std::is_fundamental<T>::value)>
FMT_CONSTEXPR auto write(OutputIt out, const T& value) -> OutputIt {
  auto f = formatter<T, Char>();
  auto parse_ctx = parse_context<Char>({});
  f.parse(parse_ctx);
  auto ctx = basic_format_context<OutputIt, Char>(out, {}, {});
  return f.format(value, ctx);
}

template <typename T>
using is_builtin =
    bool_constant<std::is_same<T, int>::value || FMT_BUILTIN_TYPES>;

// An argument visitor that formats the argument and writes it via the output
// iterator. It's a class and not a generic lambda for compatibility with C++11.
template <typename Char> struct default_arg_formatter {
  using context = buffered_context<Char>;

  basic_appender<Char> out;

  void operator()(monostate) { report_error("argument not found"); }

  template <typename T, FMT_ENABLE_IF(is_builtin<T>::value)>
  void operator()(T value) {
    write<Char>(out, value);
  }

  template <typename T, FMT_ENABLE_IF(!is_builtin<T>::value)>
  void operator()(T) {
    FMT_ASSERT(false, "");
  }

  void operator()(typename basic_format_arg<context>::handle h) {
    // Use a null locale since the default format must be unlocalized.
    auto parse_ctx = parse_context<Char>({});
    auto format_ctx = context(out, {}, {});
    h.format(parse_ctx, format_ctx);
  }
};

template <typename Char> struct arg_formatter {
  basic_appender<Char> out;
  const format_specs& specs;
  FMT_NO_UNIQUE_ADDRESS locale_ref locale;

  template <typename T, FMT_ENABLE_IF(is_builtin<T>::value)>
  FMT_CONSTEXPR FMT_INLINE void operator()(T value) {
    detail::write<Char>(out, value, specs, locale);
  }

  template <typename T, FMT_ENABLE_IF(!is_builtin<T>::value)>
  void operator()(T) {
    FMT_ASSERT(false, "");
  }

  void operator()(typename basic_format_arg<buffered_context<Char>>::handle) {
    // User-defined types are handled separately because they require access
    // to the parse context.
  }
};

struct dynamic_spec_getter {
  template <typename T, FMT_ENABLE_IF(is_integer<T>::value)>
  FMT_CONSTEXPR auto operator()(T value) -> unsigned long long {
    return is_negative(value) ? ~0ull : static_cast<unsigned long long>(value);
  }

  template <typename T, FMT_ENABLE_IF(!is_integer<T>::value)>
  FMT_CONSTEXPR auto operator()(T) -> unsigned long long {
    report_error("width/precision is not integer");
    return 0;
  }
};

template <typename Context, typename ID>
FMT_CONSTEXPR auto get_arg(Context& ctx, ID id) -> basic_format_arg<Context> {
  auto arg = ctx.arg(id);
  if (!arg) report_error("argument not found");
  return arg;
}

template <typename Context>
FMT_CONSTEXPR int get_dynamic_spec(
    arg_id_kind kind, const arg_ref<typename Context::char_type>& ref,
    Context& ctx) {
  FMT_ASSERT(kind != arg_id_kind::none, "");
  auto arg =
      kind == arg_id_kind::index ? ctx.arg(ref.index) : ctx.arg(ref.name);
  if (!arg) report_error("argument not found");
  unsigned long long value = arg.visit(dynamic_spec_getter());
  if (value > to_unsigned(max_value<int>()))
    report_error("width/precision is out of range");
  return static_cast<int>(value);
}

template <typename Context>
FMT_CONSTEXPR void handle_dynamic_spec(
    arg_id_kind kind, int& value,
    const arg_ref<typename Context::char_type>& ref, Context& ctx) {
  if (kind != arg_id_kind::none) value = get_dynamic_spec(kind, ref, ctx);
}

#if FMT_USE_NONTYPE_TEMPLATE_ARGS
template <typename T, typename Char, size_t N,
          fmt::detail::fixed_string<Char, N> Str>
struct static_named_arg : view {
  static constexpr auto name = Str.data;

  const T& value;
  static_named_arg(const T& v) : value(v) {}
};

template <typename T, typename Char, size_t N,
          fmt::detail::fixed_string<Char, N> Str>
struct is_named_arg<static_named_arg<T, Char, N, Str>> : std::true_type {};

template <typename T, typename Char, size_t N,
          fmt::detail::fixed_string<Char, N> Str>
struct is_static_named_arg<static_named_arg<T, Char, N, Str>> : std::true_type {
};

template <typename Char, size_t N, fmt::detail::fixed_string<Char, N> Str>
struct udl_arg {
  template <typename T> auto operator=(T&& value) const {
    return static_named_arg<T, Char, N, Str>(std::forward<T>(value));
  }
};
#else
template <typename Char> struct udl_arg {
  const Char* str;

  template <typename T> auto operator=(T&& value) const -> named_arg<Char, T> {
    return {str, std::forward<T>(value)};
  }
};
#endif  // FMT_USE_NONTYPE_TEMPLATE_ARGS

template <typename Char> struct format_handler {
  parse_context<Char> parse_ctx;
  buffered_context<Char> ctx;

  void on_text(const Char* begin, const Char* end) {
    copy_noinline<Char>(begin, end, ctx.out());
  }

  FMT_CONSTEXPR auto on_arg_id() -> int { return parse_ctx.next_arg_id(); }
  FMT_CONSTEXPR auto on_arg_id(int id) -> int {
    parse_ctx.check_arg_id(id);
    return id;
  }
  FMT_CONSTEXPR auto on_arg_id(basic_string_view<Char> id) -> int {
    parse_ctx.check_arg_id(id);
    int arg_id = ctx.arg_id(id);
    if (arg_id < 0) report_error("argument not found");
    return arg_id;
  }

  FMT_INLINE void on_replacement_field(int id, const Char*) {
    ctx.arg(id).visit(default_arg_formatter<Char>{ctx.out()});
  }

  auto on_format_specs(int id, const Char* begin, const Char* end)
      -> const Char* {
    auto arg = get_arg(ctx, id);
    // Not using a visitor for custom types gives better codegen.
    if (arg.format_custom(begin, parse_ctx, ctx)) return parse_ctx.begin();

    auto specs = dynamic_format_specs<Char>();
    begin = parse_format_specs(begin, end, specs, parse_ctx, arg.type());
    if (specs.dynamic()) {
      handle_dynamic_spec(specs.dynamic_width(), specs.width, specs.width_ref,
                          ctx);
      handle_dynamic_spec(specs.dynamic_precision(), specs.precision,
                          specs.precision_ref, ctx);
    }

    arg.visit(arg_formatter<Char>{ctx.out(), specs, ctx.locale()});
    return begin;
  }

  FMT_NORETURN void on_error(const char* message) { report_error(message); }
};

using format_func = void (*)(detail::buffer<char>&, int, const char*);
FMT_API void do_report_error(format_func func, int error_code,
                             const char* message) noexcept;

FMT_API void format_error_code(buffer<char>& out, int error_code,
                               string_view message) noexcept;

template <typename T, typename Char, type TYPE>
template <typename FormatContext>
FMT_CONSTEXPR auto native_formatter<T, Char, TYPE>::format(
    const T& val, FormatContext& ctx) const -> decltype(ctx.out()) {
  if (!specs_.dynamic())
    return write<Char>(ctx.out(), val, specs_, ctx.locale());
  auto specs = format_specs(specs_);
  handle_dynamic_spec(specs.dynamic_width(), specs.width, specs_.width_ref,
                      ctx);
  handle_dynamic_spec(specs.dynamic_precision(), specs.precision,
                      specs_.precision_ref, ctx);
  return write<Char>(ctx.out(), val, specs, ctx.locale());
}

// DEPRECATED! https://github.com/fmtlib/fmt/issues/4292.
template <typename T, typename Enable = void>
struct is_locale : std::false_type {};
template <typename T>
struct is_locale<T, void_t<decltype(T::classic())>> : std::true_type {};

// DEPRECATED!
template <typename Char = char> struct vformat_args {
  using type = basic_format_args<buffered_context<Char>>;
};
template <> struct vformat_args<char> {
  using type = format_args;
};

template <typename Char>
void vformat_to(buffer<Char>& buf, basic_string_view<Char> fmt,
                typename vformat_args<Char>::type args, locale_ref loc = {}) {
  auto out = basic_appender<Char>(buf);
  parse_format_string(
      fmt, format_handler<Char>{parse_context<Char>(fmt), {out, args, loc}});
}
}  // namespace detail

FMT_BEGIN_EXPORT

// A generic formatting context with custom output iterator and character
// (code unit) support. Char is the format string code unit type which can be
// different from OutputIt::value_type.
template <typename OutputIt, typename Char> class generic_context {
 private:
  OutputIt out_;
  basic_format_args<generic_context> args_;
  detail::locale_ref loc_;

 public:
  using char_type = Char;
  using iterator = OutputIt;
  using parse_context_type FMT_DEPRECATED = parse_context<Char>;
  template <typename T>
  using formatter_type FMT_DEPRECATED = formatter<T, Char>;
  enum { builtin_types = FMT_BUILTIN_TYPES };

  constexpr generic_context(OutputIt out,
                            basic_format_args<generic_context> args,
                            detail::locale_ref loc = {})
      : out_(out), args_(args), loc_(loc) {}
  generic_context(generic_context&&) = default;
  generic_context(const generic_context&) = delete;
  void operator=(const generic_context&) = delete;

  constexpr auto arg(int id) const -> basic_format_arg<generic_context> {
    return args_.get(id);
  }
  auto arg(basic_string_view<Char> name) const
      -> basic_format_arg<generic_context> {
    return args_.get(name);
  }
  constexpr auto arg_id(basic_string_view<Char> name) const -> int {
    return args_.get_id(name);
  }

  constexpr auto out() const -> iterator { return out_; }

  void advance_to(iterator it) {
    if (!detail::is_back_insert_iterator<iterator>()) out_ = it;
  }

  constexpr auto locale() const -> detail::locale_ref { return loc_; }
};

class loc_value {
 private:
  basic_format_arg<context> value_;

 public:
  template <typename T, FMT_ENABLE_IF(!detail::is_float128<T>::value)>
  loc_value(T value) : value_(value) {}

  template <typename T, FMT_ENABLE_IF(detail::is_float128<T>::value)>
  loc_value(T) {}

  template <typename Visitor> auto visit(Visitor&& vis) -> decltype(vis(0)) {
    return value_.visit(vis);
  }
};

#if FMT_USE_LOCALE
// A locale facet that formats values in UTF-8.
// It is parameterized on the locale to avoid the heavy <locale> include.
template <typename Locale> class format_facet : public Locale::facet {
 private:
  fmt::string separator_;
  fmt::string grouping_;
  fmt::string decimal_point_;

 protected:
  virtual auto do_put(appender out, loc_value val,
                      const format_specs& specs) const -> bool;

 public:
  static FMT_API typename Locale::id id;

  explicit format_facet(Locale& loc);
  explicit format_facet(string_view sep = "", fmt::string grouping = "\3",
                        fmt::string decimal_point = ".")
      : separator_(sep.data(), sep.size()),
        grouping_(grouping),
        decimal_point_(decimal_point) {}

  auto put(appender out, loc_value val, const format_specs& specs) const
      -> bool {
    return do_put(out, val, specs);
  }
};
#endif // FMT_USE_LOCALE

#define FMT_FORMAT_AS(Type, Base)                                   \
  template <typename Char>                                          \
  struct formatter<Type, Char> : formatter<Base, Char> {            \
    template <typename FormatContext>                               \
    FMT_CONSTEXPR auto format(Type value, FormatContext& ctx) const \
        -> decltype(ctx.out()) {                                    \
      return formatter<Base, Char>::format(value, ctx);             \
    }                                                               \
  }

FMT_FORMAT_AS(signed char, int);
FMT_FORMAT_AS(unsigned char, unsigned);
FMT_FORMAT_AS(short, int);
FMT_FORMAT_AS(unsigned short, unsigned);
FMT_FORMAT_AS(long, detail::long_type);
FMT_FORMAT_AS(unsigned long, detail::ulong_type);
FMT_FORMAT_AS(Char*, const Char*);
FMT_FORMAT_AS(detail::std_string_view<Char>, basic_string_view<Char>);
FMT_FORMAT_AS(std::nullptr_t, const void*);
FMT_FORMAT_AS(void*, const void*);

template <typename Char, size_t N>
struct formatter<Char[N], Char> : formatter<basic_string_view<Char>, Char> {};

template <typename Char, typename Traits, typename Allocator>
class formatter<fmt::basic_string<Char, Traits, Allocator>, Char>
    : public formatter<basic_string_view<Char>, Char> {};

template <int N, typename Char>
struct formatter<detail::bitint<N>, Char> : formatter<long long, Char> {};
template <int N, typename Char>
struct formatter<detail::ubitint<N>, Char>
    : formatter<unsigned long long, Char> {};

template <typename Char>
struct formatter<detail::float128, Char>
    : detail::native_formatter<detail::float128, Char,
                               detail::type::float_type> {};

template <typename T, typename Char>
struct formatter<T, Char, void_t<detail::format_as_result<T>>>
    : formatter<detail::format_as_result<T>, Char> {
  template <typename FormatContext>
  FMT_CONSTEXPR auto format(const T& value, FormatContext& ctx) const
      -> decltype(ctx.out()) {
    auto&& val = format_as(value);  // Make an lvalue reference for format.
    return formatter<detail::format_as_result<T>, Char>::format(val, ctx);
  }
};

/**
 * Converts `p` to `const void*` for pointer formatting.
 *
 * **Example**:
 *
 *     auto s = fmt::format("{}", fmt::ptr(p));
 */
template <typename T> auto ptr(T p) -> const void* {
  static_assert(std::is_pointer<T>::value, "fmt::ptr used with non-pointer");
  return detail::bit_cast<const void*>(p);
}

/**
 * Converts `e` to the underlying type.
 *
 * **Example**:
 *
 *     enum class color { red, green, blue };
 *     auto s = fmt::format("{}", fmt::underlying(color::red));  // s == "0"
 */
template <typename Enum>
constexpr auto underlying(Enum e) noexcept -> underlying_t<Enum> {
  return static_cast<underlying_t<Enum>>(e);
}

namespace enums {
template <typename Enum, FMT_ENABLE_IF(std::is_enum<Enum>::value)>
constexpr auto format_as(Enum e) noexcept -> underlying_t<Enum> {
  return static_cast<underlying_t<Enum>>(e);
}
}  // namespace enums

#ifdef __cpp_lib_byte
template <typename Char>
struct formatter<std::byte, Char> : formatter<unsigned, Char> {
  static auto format_as(std::byte b) -> unsigned char {
    return static_cast<unsigned char>(b);
  }
  template <typename Context>
  auto format(std::byte b, Context& ctx) const -> decltype(ctx.out()) {
    return formatter<unsigned, Char>::format(format_as(b), ctx);
  }
};
#endif

struct bytes {
  string_view data;

  inline explicit bytes(string_view s) : data(s) {}
};

template <> struct formatter<bytes> {
 private:
  detail::dynamic_format_specs<> specs_;

 public:
  FMT_CONSTEXPR auto parse(parse_context<>& ctx) -> const char* {
    return parse_format_specs(ctx.begin(), ctx.end(), specs_, ctx,
                              detail::type::string_type);
  }

  template <typename FormatContext>
  auto format(bytes b, FormatContext& ctx) const -> decltype(ctx.out()) {
    auto specs = specs_;
    detail::handle_dynamic_spec(specs.dynamic_width(), specs.width,
                                specs.width_ref, ctx);
    detail::handle_dynamic_spec(specs.dynamic_precision(), specs.precision,
                                specs.precision_ref, ctx);
    return detail::write_bytes<char>(ctx.out(), b.data, specs);
  }
};

// group_digits_view is not derived from view because it copies the argument.
template <typename T> struct group_digits_view {
  T value;
};

/**
 * Returns a view that formats an integer value using ',' as a
 * locale-independent thousands separator.
 *
 * **Example**:
 *
 *     fmt::print("{}", fmt::group_digits(12345));
 *     // Output: "12,345"
 */
template <typename T> auto group_digits(T value) -> group_digits_view<T> {
  return {value};
}

template <typename T> struct formatter<group_digits_view<T>> : formatter<T> {
 private:
  detail::dynamic_format_specs<> specs_;

 public:
  FMT_CONSTEXPR auto parse(parse_context<>& ctx) -> const char* {
    return parse_format_specs(ctx.begin(), ctx.end(), specs_, ctx,
                              detail::type::int_type);
  }

  template <typename FormatContext>
  auto format(group_digits_view<T> view, FormatContext& ctx) const
      -> decltype(ctx.out()) {
    auto specs = specs_;
    detail::handle_dynamic_spec(specs.dynamic_width(), specs.width,
                                specs.width_ref, ctx);
    detail::handle_dynamic_spec(specs.dynamic_precision(), specs.precision,
                                specs.precision_ref, ctx);
    auto arg = detail::make_write_int_arg(view.value, specs.sign());
    return detail::write_int(
        ctx.out(), static_cast<detail::uint64_or_128_t<T>>(arg.abs_value),
        arg.prefix, specs, detail::digit_grouping<char>("\3", ","));
  }
};

template <typename T, typename Char> struct nested_view {
  const formatter<T, Char>* fmt;
  const T* value;
};

template <typename T, typename Char>
struct formatter<nested_view<T, Char>, Char> {
  FMT_CONSTEXPR auto parse(parse_context<Char>& ctx) -> const Char* {
    return ctx.begin();
  }
  template <typename FormatContext>
  auto format(nested_view<T, Char> view, FormatContext& ctx) const
      -> decltype(ctx.out()) {
    return view.fmt->format(*view.value, ctx);
  }
};

template <typename T, typename Char = char> struct nested_formatter {
 private:
  basic_specs specs_;
  int width_;
  formatter<T, Char> formatter_;

 public:
  constexpr nested_formatter() : width_(0) {}

  FMT_CONSTEXPR auto parse(parse_context<Char>& ctx) -> const Char* {
    auto it = ctx.begin(), end = ctx.end();
    if (it == end) return it;
    auto specs = format_specs();
    it = detail::parse_align(it, end, specs);
    specs_ = specs;
    Char c = *it;
    auto width_ref = detail::arg_ref<Char>();
    if ((c >= '0' && c <= '9') || c == '{') {
      it = detail::parse_width(it, end, specs, width_ref, ctx);
      width_ = specs.width;
    }
    ctx.advance_to(it);
    return formatter_.parse(ctx);
  }

  template <typename FormatContext, typename F>
  auto write_padded(FormatContext& ctx, F write) const -> decltype(ctx.out()) {
    if (width_ == 0) return write(ctx.out());
    auto buf = basic_memory_buffer<Char>();
    write(basic_appender<Char>(buf));
    auto specs = format_specs();
    specs.width = width_;
    specs.copy_fill_from(specs_);
    specs.set_align(specs_.align());
    return detail::write<Char>(
        ctx.out(), basic_string_view<Char>(buf.data(), buf.size()), specs);
  }

  auto nested(const T& value) const -> nested_view<T, Char> {
    return nested_view<T, Char>{&formatter_, &value};
  }
};

inline namespace literals {
#if FMT_USE_NONTYPE_TEMPLATE_ARGS
template <detail::fixed_string S> constexpr auto operator""_a() {
  using char_t = remove_cvref_t<decltype(*S.data)>;
  return detail::udl_arg<char_t, sizeof(S.data) / sizeof(char_t), S>();
}
#else
/**
 * User-defined literal equivalent of `fmt::arg`.
 *
 * **Example**:
 *
 *     using namespace fmt::literals;
 *     fmt::print("The answer is {answer}.", "answer"_a=42);
 */
constexpr auto operator""_a(const char* s, size_t) -> detail::udl_arg<char> {
  return {s};
}
#endif  // FMT_USE_NONTYPE_TEMPLATE_ARGS
}  // namespace literals

/// A fast integer formatter.
class format_int {
 private:
  // Buffer should be large enough to hold all digits (digits10 + 1),
  // a sign and a null character.
  enum { buffer_size = std::numeric_limits<unsigned long long>::digits10 + 3 };
  mutable char buffer_[buffer_size];
  char* str_;

  template <typename UInt>
  FMT_CONSTEXPR20 auto format_unsigned(UInt value) -> char* {
    auto n = static_cast<detail::uint32_or_64_or_128_t<UInt>>(value);
    return detail::do_format_decimal(buffer_, n, buffer_size - 1);
  }

  template <typename Int>
  FMT_CONSTEXPR20 auto format_signed(Int value) -> char* {
    auto abs_value = static_cast<detail::uint32_or_64_or_128_t<Int>>(value);
    bool negative = value < 0;
    if (negative) abs_value = 0 - abs_value;
    auto begin = format_unsigned(abs_value);
    if (negative) *--begin = '-';
    return begin;
  }

 public:
  FMT_CONSTEXPR20 explicit format_int(int value) : str_(format_signed(value)) {}
  FMT_CONSTEXPR20 explicit format_int(long value)
      : str_(format_signed(value)) {}
  FMT_CONSTEXPR20 explicit format_int(long long value)
      : str_(format_signed(value)) {}
  FMT_CONSTEXPR20 explicit format_int(unsigned value)
      : str_(format_unsigned(value)) {}
  FMT_CONSTEXPR20 explicit format_int(unsigned long value)
      : str_(format_unsigned(value)) {}
  FMT_CONSTEXPR20 explicit format_int(unsigned long long value)
      : str_(format_unsigned(value)) {}

  /// Returns the number of characters written to the output buffer.
  FMT_CONSTEXPR20 auto size() const -> size_t {
    return detail::to_unsigned(buffer_ - str_ + buffer_size - 1);
  }

  /// Returns a pointer to the output buffer content. No terminating null
  /// character is appended.
  FMT_CONSTEXPR20 auto data() const -> const char* { return str_; }

  /// Returns a pointer to the output buffer content with terminating null
  /// character appended.
  FMT_CONSTEXPR20 auto c_str() const -> const char* {
    buffer_[buffer_size - 1] = '\0';
    return str_;
  }

  /// Returns the content of the output buffer as an `fmt::string`.
  inline auto str() const -> fmt::string { return {str_, size()}; }
};

#define FMT_STRING_IMPL(s, base)                                              \
  [] {                                                                        \
    /* Use the hidden visibility as a workaround for a GCC bug (#1973). */    \
    /* Use a macro-like name to avoid shadowing warnings. */                  \
    struct FMT_VISIBILITY("hidden") FMT_COMPILE_STRING : base {               \
      using char_type = fmt::remove_cvref_t<decltype(s[0])>;                  \
      constexpr explicit operator fmt::basic_string_view<char_type>() const { \
        return fmt::detail::compile_string_to_view<char_type>(s);             \
      }                                                                       \
    };                                                                        \
    using FMT_STRING_VIEW =                                                   \
        fmt::basic_string_view<typename FMT_COMPILE_STRING::char_type>;       \
    fmt::detail::ignore_unused(FMT_STRING_VIEW(FMT_COMPILE_STRING()));        \
    return FMT_COMPILE_STRING();                                              \
  }()

/**
 * Constructs a legacy compile-time format string from a string literal `s`.
 *
 * **Example**:
 *
 *     // A compile-time error because 'd' is an invalid specifier for strings.
 *     fmt::string s = fmt::format(FMT_STRING("{:d}"), "foo");
 */
#define FMT_STRING(s) FMT_STRING_IMPL(s, fmt::detail::compile_string)

#if FMT_USE_EXCEPTIONS
FMT_API auto vsystem_error(int error_code, string_view fmt, format_args args)
    -> std::system_error;

/**
 * Constructs `fmt::system_error` with a message formatted with
 * `fmt::format(fmt, args...)`.
 * `error_code` is a system error code as given by `errno`.
 *
 * **Example**:
 *
 *     // This throws fmt::system_error with the description
 *     //   cannot open file 'madeup': No such file or directory
 *     // or similar (system message may vary).
 *     const char* filename = "madeup";
 *     FILE* file = fopen(filename, "r");
 *     if (!file)
 *       throw fmt::system_error(errno, "cannot open file '{}'", filename);
 */
template <typename... T>
auto system_error(int error_code, format_string<T...> fmt, T&&... args)
    -> std::system_error {
  return vsystem_error(error_code, fmt.str, vargs<T...>{{args...}});
}
#endif // FMT_USE_EXCEPTIONS

/**
 * Formats an error message for an error returned by an operating system or a
 * language runtime, for example a file opening error, and writes it to `out`.
 * The format is the same as the one used by `std::system_error(ec, message)`
 * where `ec` is `std::error_code(error_code, std::generic_category())`.
 * It is implementation-defined but normally looks like:
 *
 *     <message>: <system-message>
 *
 * where `<message>` is the passed message and `<system-message>` is the system
 * message corresponding to the error code.
 * `error_code` is a system error code as given by `errno`.
 */
FMT_API void format_system_error(detail::buffer<char>& out, int error_code,
                                 const char* message) noexcept;

// Reports a system error without throwing an exception.
// Can be used to report errors from destructors.
FMT_API void report_system_error(int error_code, const char* message) noexcept;

template <typename Locale, FMT_ENABLE_IF(detail::is_locale<Locale>::value)>
inline auto vformat(const Locale& loc, string_view fmt, format_args args)
    -> fmt::string {
  auto buf = memory_buffer();
  detail::vformat_to(buf, fmt, args, detail::locale_ref(loc));
  return {buf.data(), buf.size()};
}

template <typename Locale, typename... T,
          FMT_ENABLE_IF(detail::is_locale<Locale>::value)>
FMT_INLINE auto format(const Locale& loc, format_string<T...> fmt, T&&... args)
    -> fmt::string {
  return vformat(loc, fmt.str, vargs<T...>{{args...}});
}

template <typename OutputIt, typename Locale,
          FMT_ENABLE_IF(detail::is_output_iterator<OutputIt, char>::value)>
auto vformat_to(OutputIt out, const Locale& loc, string_view fmt,
                format_args args) -> OutputIt {
  auto&& buf = detail::get_buffer<char>(out);
  detail::vformat_to(buf, fmt, args, detail::locale_ref(loc));
  return detail::get_iterator(buf, out);
}

template <typename OutputIt, typename Locale, typename... T,
          FMT_ENABLE_IF(detail::is_output_iterator<OutputIt, char>::value&&
                            detail::is_locale<Locale>::value)>
FMT_INLINE auto format_to(OutputIt out, const Locale& loc,
                          format_string<T...> fmt, T&&... args) -> OutputIt {
  return fmt::vformat_to(out, loc, fmt.str, vargs<T...>{{args...}});
}

template <typename Locale, typename... T,
          FMT_ENABLE_IF(detail::is_locale<Locale>::value)>
FMT_NODISCARD FMT_INLINE auto formatted_size(const Locale& loc,
                                             format_string<T...> fmt,
                                             T&&... args) -> size_t {
  auto buf = detail::counting_buffer<>();
  detail::vformat_to(buf, fmt.str, vargs<T...>{{args...}},
                     detail::locale_ref(loc));
  return buf.count();
}

FMT_API auto vformat(string_view fmt, format_args args) -> fmt::string;

/**
 * Formats `args` according to specifications in `fmt` and returns the result
 * as a string.
 *
 * **Example**:
 *
 *     #include <fmt/format.h>
 *     fmt::string message = fmt::format("The answer is {}.", 42);
 */
template <typename... T>
FMT_NODISCARD FMT_INLINE auto format(format_string<T...> fmt, T&&... args)
    -> fmt::string {
  return vformat(fmt.str, vargs<T...>{{args...}});
}

/**
 * Converts `value` to `fmt::string` using the default format for type `T`.
 *
 * **Example**:
 *
 *     fmt::string answer = fmt::to_string(42);
 */
template <typename T, FMT_ENABLE_IF(std::is_integral<T>::value)>
FMT_NODISCARD FMT_CONSTEXPR_STRING auto to_string(T value) -> fmt::string {
  // The buffer should be large enough to store the number including the sign
  // or "false" for bool.
  char buffer[max_of(detail::digits10<T>() + 2, 5)];
  return {buffer, detail::write<char>(buffer, value)};
}

template <typename T, FMT_ENABLE_IF(detail::use_format_as<T>::value)>
FMT_NODISCARD FMT_CONSTEXPR_STRING auto to_string(const T& value)
    -> fmt::string {
  return to_string(format_as(value));
}

template <typename T, FMT_ENABLE_IF(!std::is_integral<T>::value &&
                                    !detail::use_format_as<T>::value)>
FMT_NODISCARD FMT_CONSTEXPR_STRING auto to_string(const T& value)
    -> fmt::string {
  auto buffer = memory_buffer();
  detail::write<char>(appender(buffer), value);
  return {buffer.data(), buffer.size()};
}

FMT_END_EXPORT
FMT_END_NAMESPACE

#ifdef FMT_HEADER_ONLY
#  define FMT_FUNC inline
#  include "format-inl.h"
#endif

// Restore _LIBCPP_REMOVE_TRANSITIVE_INCLUDES.
#ifdef FMT_REMOVE_TRANSITIVE_INCLUDES
#  undef _LIBCPP_REMOVE_TRANSITIVE_INCLUDES
#endif

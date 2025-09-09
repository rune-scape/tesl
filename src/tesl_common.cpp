#include "tesl_common.hpp"

#include "tesl_bootstrap.hpp"
#include "tesl_error.hpp"
#include "tesl_operator.hpp"
#include "tesl_type.hpp"
#include <fmt/format.h>

namespace tesl {
  namespace detail {
    static void raw_convert_float_to_int(FnContext * context, void * pArgs, void * pRet) {
      struct Args {
        IntT v;
      };

      Args * args = reinterpret_cast<Args *>(pArgs);
      FloatT * ret = reinterpret_cast<FloatT *>(pRet);
      *ret = (args->v);
    }
  }

  template<>
  TypeRef make_type_info<Null>() {
    Ref<TypeInfo> ref = new_ref<TypeInfo>(
      TESL_STRVIEW("Null"),
      TESL_STRVIEW("Null"),
      TESL_SIGNATURE_INDEX("->Null"),
      0,
      1,
      FnPtrBare{empty_fn},
      FnPtrBare{empty_fn},
      FnPtrBare{empty_fn},
      FnPtrBare{empty_fn}
    );
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Null>() {
    static TypeRef ret = make_type_info<Null>();
    return ret;
  }

  template<>
  TypeRef make_type_info<Bool>() {
    Ref<TypeInfo> ref = TESL_NEW_BUILTIN_TYPE_INFO(Bool, "Bool");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<Bool>() {
    static TypeRef ret = make_type_info<Bool>();
    return ret;
  }

  template<>
  TypeRef make_type_info<IntT>() {
    Ref<TypeInfo> ref = TESL_NEW_BUILTIN_TYPE_INFO(IntT, "Int");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<IntT>() {
    static TypeRef ret = make_type_info<IntT>();
    return ret;
  }

  template<>
  TypeRef make_type_info<FloatT>() {
    Ref<TypeInfo> ref = TESL_NEW_BUILTIN_TYPE_INFO(FloatT, "Float");
    TESL_BIND_BASIC_BARE(ref, detail::raw_convert_float_to_int, "->Int");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<FloatT>() {
    static TypeRef ret = make_type_info<FloatT>();
    return ret;
  }

  template<>
  TypeRef make_type_info<TypeRef>() {
    Ref<TypeInfo> ref = TESL_NEW_BUILTIN_TYPE_INFO(TypeRef, "Type");
    return ref;
  }

  template<>
  TypeRef get_builtin_type_info_of<TypeRef>() {
    static TypeRef ret = make_type_info<TypeRef>();
    return ret;
  }

  void print_error_sourcev(FILE * file, int line_num, const char * line_start, const char * start, const char * point, const char * const end) {
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

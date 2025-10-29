#include "tesl_fn.hpp"

namespace tesl {
    void CallError::format(FmtParseContext & parse_ctx, FmtFormatContext & ctx) const {
      switch (why) {
        case CallError::UNSPECIFIED:
          ctx.advance_to(format_to(ctx.out(), "unspecified call error"));
        case CallError::COULD_NOT_FIND:
          ctx.advance_to(format_to(ctx.out(), "could not find '{}' in '{}'", signature, type));
      }

      TESL_UNREACHABLE;
    }

}

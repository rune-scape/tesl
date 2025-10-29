#pragma once

#include "tesl_common.hpp"
#include <fmt/format.h>

namespace tesl {
  struct Error {
    fmt::memory_buffer _buffer;
    bool _has_error = false;

    operator bool() {
      return _has_error;
    }

    void diffuse() {
      _buffer.clear();
    }
    
    template <typename FStringT, typename ... T>
    Error(FStringT && fstr, T && ... args) : _has_error(true) {
      fmt::format_to(_buffer, FWD(fstr), FWD(args)...);
    }

    Error() = default;

    ~Error() {
      if (_buffer.size() > 0) {
        fmt::print("error: {}", CharStrView{_buffer.data(), _buffer.size()});
      }
    }
  };
}
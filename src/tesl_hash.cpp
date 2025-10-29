#include "tesl_hash.hpp"

extern "C" {
#define WYHASH_32BIT_MUM 1
#include "wyhash.h"
}

namespace tesl {
  namespace detail {
    uint64_t hash_mix(uint64_t a, uint64_t b) {
      return _wymix(a, b);
    }

    HashT hash_buffer(const void * buffer, IntT len) {
      return wyhash(buffer, len, _wyp[0], _wyp);
    }
  }
}
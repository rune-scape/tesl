#include "tesl_hash.hpp"

extern "C" {
#define RAPIDHASH_COMPACT
#define RAPIDHASH_PROTECTED
#ifndef TESL_USE_64_BIT_NUMBERS
#define WYHASH_32BIT_MUM
#endif
#include "wyhash.h"
}

namespace tesl {
  namespace detail {
    HashT hash_mix(uint64_t a, HashT b) {
      return _wymix(a, b);
    }

    HashT hash_buffer(const void * buffer, IntT len) {
      return wyhash(buffer, len, _wyp[0], _wyp);
    }
  }
}
#pragma once

#include "tesl_common.hpp"
#include "tesl_fmt_fwd.hpp"
#include "tesl_hash.hpp"
#include "tesl_type.hpp"
#include "hash_table8.hpp"

namespace tesl {
  namespace detail {
    struct CompareEqual {
      template<typename LhsT, typename RhsT>
      bool operator()(LhsT && lhs, RhsT && rhs) const {
        return FWD(lhs) == FWD(rhs);
      }
    };

    struct Hasher {
      template<typename T>
      uint64_t operator()(T && v) const {
        return tesl::hash(FWD(v));
      }
    };
  }

  template<typename KeyT, typename ValueT>
  using Map = ::emhash8::HashMap<KeyT, ValueT, detail::Hasher, detail::CompareEqual>;
}
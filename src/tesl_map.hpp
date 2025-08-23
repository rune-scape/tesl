#pragma once

#include "tesl_common.hpp"
#include "tesl_hash.hpp"
#include "tesl_type.hpp"
#include "hash_table8.hpp"
#include <fmt/fwd.h>

namespace tesl {
  namespace detail {
    struct MapCompareEqual {
      template<typename LhsT, typename RhsT>
      bool operator()(LhsT && lhs, RhsT && rhs) const {
        return FWD(lhs) == FWD(rhs);
      }
    };

    struct MapHasher {
      template<typename T>
      uint64_t operator()(T && v) const {
        return tesl::hash(FWD(v));
      }
    };
  }

  template<typename KeyT, typename ValueT>
  struct Map : public ::emhash8::HashMap<KeyT, ValueT, detail::MapHasher, detail::MapCompareEqual> {
    using base = ::emhash8::HashMap<KeyT, ValueT, detail::MapHasher, detail::MapCompareEqual>;
    using base::base;
  };
}
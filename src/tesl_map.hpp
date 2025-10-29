#pragma once

#include "tesl_common.hpp"
#include "tesl_hash.hpp"
#include "tesl_type.hpp"
#include "hash_table8.hpp"

namespace tesl {
  namespace detail {
    struct MapHasher {
      template<typename T>
      HashT operator()(T && v) const {
        return ::tesl::hash(FWD(v));
      }
    };

    struct MapCompareEqual {
      template<typename LhsT, typename RhsT>
      bool operator()(LhsT && lhs, RhsT && rhs) const {
        return FWD(lhs) == FWD(rhs);
      }
    };

  }

  template<typename KeyT, typename ValueT, typename HasherT, typename EqualsT>
  struct Map : public ::emhash8::HashMap<KeyT, ValueT, HasherT, EqualsT> {
    using base = ::emhash8::HashMap<KeyT, ValueT, HasherT, EqualsT>;
    using base::base;
  };
}
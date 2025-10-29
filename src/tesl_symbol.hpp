#pragma once

#include "tesl_common.hpp"

namespace tesl {
  template<typename T, typename IndexT>
  struct Symbol {
    using IndexType = IndexT;
    static constexpr IndexT max_index = std::numeric_limits<IndexT>::max();
    static constexpr IndexT invalid_index = max_index;
    IndexT index = invalid_index;

    constexpr bool is_valid() const { return index != invalid_index; }

    constexpr bool operator==(Symbol other) { return index == other.index; }
    constexpr bool operator!=(Symbol other) { return !this->operator==(other); }

    constexpr Symbol & operator=(const Symbol &) = default;
    constexpr Symbol & operator=(Symbol &&) = default;
    constexpr Symbol(const Symbol &) = default;
    constexpr Symbol(Symbol &&) = default;

    constexpr explicit Symbol(IndexT i) : index(i) {}

    explicit Symbol(std::size_t i) : index(static_cast<IndexT>(i)) {
      TESL_ASSERT(i <= invalid_index);
    }

    explicit Symbol(IntT i) : index(static_cast<IndexT>(i)) {
      TESL_ASSERT(i >= 0 && i <= invalid_index);
    }

    constexpr Symbol() {}
  };

  using GlobalSymbolIndexT = std::uint32_t;
  using LocalSymbolIndexT = std::uint32_t;

  template<typename T>
  struct GlobalSymbol : Symbol<T, GlobalSymbolIndexT> {
    using Symbol<T, GlobalSymbolIndexT>::Symbol;

    constexpr bool operator==(GlobalSymbol other) const { return this->index == other.index; }
    constexpr bool operator!=(GlobalSymbol other) const { return !this->operator==(other); }
  };

  template<typename T>
  struct LocalSymbol : Symbol<T, LocalSymbolIndexT> {
    using Symbol<T, LocalSymbolIndexT>::Symbol;

    constexpr bool operator==(LocalSymbol other) { return this->index == other.index; }
    constexpr bool operator!=(LocalSymbol other) { return !this->operator==(other); }
  };

  using GlobalNameSymbol = GlobalSymbol<Name>;
  using GlobalSignatureSymbol = GlobalSymbol<Signature>;
  using GlobalTypeSymbol = GlobalSymbol<Type>;

  using LocalNameSymbol = LocalSymbol<Name>;
  using LocalSignatureSymbol = LocalSymbol<Signature>;
  using LocalTypeSymbol = LocalSymbol<Type>;
}

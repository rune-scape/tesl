#pragma once

#include "tesl_common.hpp"
#include "tesl_array.hpp"
#include "tesl_str.hpp"
#include "tesl_optional.hpp"
#include "tesl_symbol.hpp"

namespace tesl {
  template<typename SymbolT, typename ValueT>
  struct FlatSymbolTable {
    Array<ValueT> _data;

    auto size() const {
      return _data.size();
    }

    bool has(SymbolT sym) const {
      return sym.index < _data.size();
    }

    bool is_full() const {
      return _data.size() == SymbolT::max_index;
    }

    ValueT & _get_unchecked(SymbolT sym) {
      return _data[sym.index];
    }

    const ValueT & _get_unchecked(SymbolT sym) const {
      return _data[sym.index];
    }

    Optional<ValueT &> get(SymbolT sym) {
      if (sym.index < _data.size()) {
        return _data[sym.index];
      }
      return {};
    }

    Optional<const ValueT &> get(SymbolT sym) const {
      if (sym.index < _data.size()) {
        return _data[sym.index];
      }
      return {};
    }

    template<typename T1>
    SymbolT push_back(T1 && v) {
      TESL_ASSERT(!is_full());
      SymbolT sym{_data.size()};
      _data.emplace_back(FWD(v));
      return sym;
    }

    template<typename T1>
    void set(SymbolT sym, T1 && v) {
      TESL_ASSERT(sym.index < _data.size());
      _data[sym.index] = FWD(v);
    }
  };

  template<typename KeyT, typename ValueT>
  using MaterSymbolTable = FlatSymbolTable<GlobalSymbol<KeyT>, ValueT>;

  template<typename GlobalSymbolT, typename LocalSymbolT, typename ValueT>
  struct LayeredSymbolTable : public FlatSymbolTable<LocalSymbolT, ValueT> {
    using base = FlatSymbolTable<LocalSymbolT, ValueT>;

    Array<LocalSymbolT> _indices;

    bool has(GlobalSymbolT sym) const {
      if (sym.index < _indices.size()) {
        return base::has(_indices[sym.index]);
      }
      return false;
    }

    ValueT & _get_unchecked(GlobalSymbolT sym) {
      return base::get_unchecked(_indices[sym.index]);
    }

    const ValueT & _get_unchecked(GlobalSymbolT sym) const {
      return base::get_unchecked(_indices[sym.index]);
    }

    Optional<ValueT &> get(GlobalSymbolT sym) {
      if (sym.index < _indices.size()) {
        return base::get(_indices[sym.index]);
      }
      return {};
    }

    Optional<const ValueT &> get(GlobalSymbolT sym) const {
      if (sym.index < _indices.size()) {
        return base::get(_indices[sym.index]);
      }
      return {};
    }

    LocalSymbolT get_local_symbol(GlobalSymbolT sym) const {
      if (sym.index < _indices.size()) {
        return _indices[sym.index];
      }
      return {};
    }

    template<typename T1>
    LocalSymbolT insert(GlobalSymbolT sym, T1 && v) {
      if (sym.index >= _indices.size()) {
        _indices.resize(sym.index + 1);
      }

      return _indices[sym.index] = base::push_back(FWD(v));
    }

    template<typename T1>
    bool set(GlobalSymbolT sym, T1 && v) {
      if (sym.index < _indices.size()) {
        base::set(_indices[sym.index], FWD(v));
        return true;
      }

      return false;
    }
  };

  template<typename KeyT, typename ValueT>
  using InstanceSymbolTable = LayeredSymbolTable<GlobalSymbol<KeyT>, LocalSymbol<KeyT>, ValueT>;
}

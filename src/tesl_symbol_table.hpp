#pragma once

#include "tesl_common.hpp"
#include "tesl_array.hpp"
#include "tesl_str.hpp"

namespace tesl {
  template<typename ValueT>
  struct LocalSymbolTable {
    Array<ValueT> _data;

    IntT size() const {
      return _data.size();
    }

    bool has(LocalSymbolIndex sym) const {
      return sym.index < _data.size();
    }

    bool is_full() const {
      return _data.size() == LocalSymbolIndex::max_index;
    }

    ValueT * get(LocalSymbolIndex sym) {
      if (sym.index < _data.size()) {
        return &_data[sym.index];
      }
      return nullptr;
    }

    const ValueT * get(LocalSymbolIndex sym) const {
      if (sym.index < _data.size()) {
        return &_data[sym.index];
      }
      return nullptr;
    }

    template<typename T1>
    LocalSymbolIndex insert(T1 && v) {
      TESL_ASSERT(!is_full());
      LocalSymbolIndex sym{static_cast<LocalSymbolIndex::IndexT>(_data.size())};
      _data.emplace_back(FWD(v));
      return sym;
    }

    template<typename T1>
    void set(LocalSymbolIndex sym, T1 && v) {
      TESL_ASSERT(sym.index < _data.size());
      _data[sym.index] = FWD(v);
    }
  };

  template<typename ValueT>
  struct SymbolTable : public LocalSymbolTable<ValueT> {
    Array<LocalSymbolIndex> _indices;

    bool has(GlobalSymbolIndex sym) const {
      if (sym.index < _indices.size()) {
        return LocalSymbolTable<ValueT>::has(_indices[sym.index]);
      }
      return false;
    }

    ValueT * get(GlobalSymbolIndex sym) {
      if (sym.index < _indices.size()) {
        return LocalSymbolTable<ValueT>::get(_indices[sym.index]);
      }
      return nullptr;
    }

    const ValueT * get(GlobalSymbolIndex sym) const {
      if (sym.index < _indices.size()) {
        return LocalSymbolTable<ValueT>::get(_indices[sym.index]);
      }
      return nullptr;
    }

    template<typename T1>
    LocalSymbolIndex insert(GlobalSymbolIndex sym, T1 && v) {
      if (sym.index >= _indices.size()) {
        _indices.resize(sym.index + 1);
      }

      LocalSymbolIndex lsym = LocalSymbolTable<ValueT>::insert(FWD(v));
      _indices[sym.index] = lsym;
      return lsym;
    }

    template<typename T1>
    bool set(GlobalSymbolIndex sym, T1 && v) {
      if (sym.index < _indices.size()) {
        LocalSymbolTable<ValueT>::set(_indices[sym.index], FWD(v));
        return true;
      }

      return false;
    }
  };
}

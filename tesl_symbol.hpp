#pragma once

#include "tesl_str.hpp"
#include "tesl_vector.hpp"
#include "tesl_map.hpp"

namespace tesl {
  using SymbolIndexT = IntT;

  struct Symbol {
    SymbolIndexT index;
    StrView name;
  };

  // todo: global state
  Map<Str, Symbol> s; 

  struct SymbolRef {
    SymbolIndexT index;
    StrView name;
  };

#ifdef TESL_COMPACT_METHOD_TABLE
  struct MethodTableImpl {
    Vector<FnObjBase> _functions;
    Vector<SymbolIndexT> _fn_indices;

    FnObjBase * operator[](const Symbol & sym) {
      if (sym.index < _fn_indices.size()) {
        SymbolIndexT i = _fn_indices[sym.index];
        if (i < _functions.size()) {
          return &_functions[i];
        }
      }
      return nullptr;
    }

    const FnObjBase * operator[](const Symbol & sym) const {
      if (sym.index < _fn_indices.size()) {
        SymbolIndexT i = _fn_indices[sym.index];
        if (i < _functions.size()) {
          return &_functions[i];
        }
      }
      return nullptr;
    }
  };
#else
  struct MethodTableImpl {
    Vector<FnObjBase> _functions;

    FnObjBase * operator[](SymbolRef sym) {
      if (sym.index < _functions.size()) {
        return &_functions[sym.index];
      }
      return {};
    }

    const FnObjBase * operator[](SymbolRef sym) const {
      if (sym.index < _functions.size()) {
        return &_functions[sym.index];
      }
      return {};
    }
  };
#endif
}

#pragma once

#include "tesl_common.hpp"
#include "tesl_ref.hpp"
#include "tesl_symbol_table.hpp"
#include <fmt/fwd.h>

namespace tesl {
  struct TypeInfo : RefCounted<TypeInfo> {
    Str local_name;
    Str global_name;
    GlobalSymbolIndex coersion_signature;
    IntT size;
    IntT align;

    // initializes the data at the return address (no args)
    FnPtrBare _init;

    // copies the argument into the return address
    FnPtrBare _copy;

    // moves the argument into the return address
    FnPtrBare _move;

    // deinitializes the argument (no return)
    FnPtrBare _deinit;

    SymbolTable<Member> members;
    SymbolTable<Operator> operators;

    bool coerce(FnContext * context, const VarRef & v, void * ret);

    void * allocate() const {
      return operator new(size, std::align_val_t{static_cast<size_t>(align)});
    }

    void deallocate(void * ptr) const {
      operator delete(ptr);
    }

    void init(void * ptr) const {
      _init(nullptr, nullptr, ptr);
    }

    void copy(void * from, void * to) const {
      _copy(nullptr, from, to);
    }

    void move(void * from, void * to) const {
      _move(nullptr, from, to);
    }

    void deinit(void * ptr) const {
      _deinit(nullptr, ptr, nullptr);
    }

    void * new_() const {
      void * ret = allocate();
      init(ret);
      return ret;
    }

    void delete_(void * ptr) const {
      deinit(ptr);
      deallocate(ptr);
    }

    TESL_ALWAYS_INLINE bool operator==(const TypeInfo & other) const { return this == &other; }

    TypeInfo() = delete;
    TypeInfo(const TypeInfo &) = delete;
    TypeInfo(TypeInfo &&) = delete;
    TypeInfo & operator=(const TypeInfo &) = delete;
    TypeInfo & operator=(TypeInfo &&) = delete;

    TypeInfo(
        StrView pLocalName,
        StrView pGlobalName,
        GlobalSymbolIndex pCoersionSignature,
        IntT pSize,
        IntT pAlign,
        FnPtrBare pInit,
        FnPtrBare pCopy,
        FnPtrBare pMove,
        FnPtrBare pDeinit);
    ~TypeInfo();
  };

  inline const tesl::TypeInfo & format_as(TypeRef t) { return *t; }
  inline const tesl::TypeInfo & format_as(const TypeInfo * t) { return *t; }
}

template<typename CharT>
class fmt::formatter<tesl::TypeInfo, CharT> {
public:
  template<typename Context>
  constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context>
  constexpr auto format(const tesl::TypeInfo & type, Context & ctx) const {
    return format_to(ctx.out(), "{}", type.global_name);
  }
};

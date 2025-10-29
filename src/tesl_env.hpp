#pragma once

#include "tesl_common.hpp"
#include "tesl_symbol.hpp"
#include "tesl_name.hpp"
#include "tesl_signature.hpp"
#include "tesl_type.hpp"
#include "tesl_map.hpp"
#include "tesl_ref.hpp"
#include "tesl_optional.hpp"

namespace tesl {
  /*namespace detail {
    struct EnvNameMapCompareEqual : MapCompareEqual {
      TESL_ALWAYS_INLINE bool operator()(const Str & lhs, const SignatureRef & rhs) const;
      TESL_ALWAYS_INLINE bool operator()(const SignatureRef & lhs, CharStrView rhs) const;
      TESL_ALWAYS_INLINE bool operator()(CharStrView lhs, const SignatureRef & rhs) const;
    };

    struct EnvSignatureMapCompareEqual : MapCompareEqual {
      TESL_ALWAYS_INLINE bool operator()(const SignatureRef & lhs, const SignatureRef & rhs) const;
      TESL_ALWAYS_INLINE bool operator()(const SignatureRef & lhs, CharStrView rhs) const;
      TESL_ALWAYS_INLINE bool operator()(CharStrView lhs, const SignatureRef & rhs) const;
    };

    struct EnvTypeMapCompareEqual : MapCompareEqual {
      TESL_ALWAYS_INLINE bool operator()(const TypeRef & lhs, const TypeRef & rhs) const;
      TESL_ALWAYS_INLINE bool operator()(const TypeRef & lhs, CharStrView rhs) const;
      TESL_ALWAYS_INLINE bool operator()(CharStrView lhs, const TypeRef & rhs) const;
    };

    using EnvNameMapT = Map<Str, GlobalNameSymbol, detail::MapHasher, detail::EnvNameMapCompareEqual>;
    using EnvSignatureMapT = Map<Str, GlobalSignatureSymbol, detail::MapHasher, detail::EnvSignatureMapCompareEqual>;
    using EnvTypeMapT = Map<Str, GlobalTypeSymbol, detail::MapHasher, detail::EnvTypeMapCompareEqual>;
  }*/

  struct Env : RefCounted<Env> {
    static EnvRef _get_shallow_builtin_env();
    static EnvRef current;
    static const EnvRef builtin;

    FlatSymbolTable<GlobalNameSymbol, NameData> names;
    Map<Str, GlobalNameSymbol> name_map;
    FlatSymbolTable<GlobalSignatureSymbol, SignatureData> signatures;
    Map<Str, GlobalSignatureSymbol> signature_map;
    FlatSymbolTable<GlobalTypeSymbol, TypeInfo> types;
    Map<Str, GlobalTypeSymbol> type_map;

    GlobalNameSymbol add_name(NameData name);
    GlobalSignatureSymbol add_signature(SignatureData sig);
    GlobalTypeSymbol add_type(TypeInfo type);

    Optional<NameData &> get_name_data(GlobalNameSymbol sym);
    Optional<SignatureData &> get_signature_data(GlobalSignatureSymbol sym);
    Optional<TypeInfo &> get_type_data(GlobalTypeSymbol sym);

    Env();
    ~Env();
  };
}

/*template<typename CharT>
class fmt::formatter<tesl::GlobalNameSymbol, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::GlobalNameSymbol & sym, Context & ctx) const {
    using namespace tesl;

    auto name = Env::current->get_name_data(sym);
    if (name.has_value()) {
      return format_to(ctx.out(), "{}", name.unwrap());
    }

    return format_to(ctx.out(), "<invalid-global-name-symbol>");
  }
};

template<typename CharT>
class fmt::formatter<tesl::GlobalSignatureSymbol, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::GlobalSignatureSymbol & sym, Context & ctx) const {
    using namespace tesl;

    auto signature = Env::current->get_signature_data(sym);
    if (signature.has_value()) {
      return format_to(ctx.out(), "{}", signature.unwrap());
    }

    return format_to(ctx.out(), "<invalid-global-signature-symbol>");
  }
};

template<typename CharT>
class fmt::formatter<tesl::GlobalTypeSymbol, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::GlobalTypeSymbol & sym, Context & ctx) const {
    using namespace tesl;

    auto type = Env::current->get_type_data(sym);
    if (type.has_value()) {
      return format_to(ctx.out(), "{}", type.unwrap());
    }

    return format_to(ctx.out(), "<invalid-global-type-symbol>");
  }
};*/

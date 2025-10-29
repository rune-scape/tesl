#pragma once

#include "tesl_common.hpp"
#include "tesl_hash.hpp"
#include "tesl_str.hpp"
#include "tesl_symbol.hpp"

namespace tesl {
  struct NameData {
    Str str;
    HashT hash;

    TESL_ALWAYS_INLINE bool operator==(StrView p_str) { return str == p_str; }
    TESL_ALWAYS_INLINE bool operator!=(StrView p_str) { return str != p_str; }
    TESL_ALWAYS_INLINE friend bool operator==(StrView p_str, const NameData & name) { return p_str == name.str; }
    TESL_ALWAYS_INLINE friend bool operator!=(StrView p_str, const NameData & name) { return p_str != name.str; }

    NameData & operator=(const NameData & other) = default;
    NameData & operator=(NameData && other) = default;

    TESL_ALWAYS_INLINE friend HashT hash(const NameData & v) { return v.hash; }

    NameData(NameData & other) : str(other.str), hash(other.hash) {}
    NameData(NameData && other) : str(MOV(other.str)), hash(MOV(other.hash)) {}
    NameData(StrView p_str) : str(p_str), hash(::tesl::hash(p_str)) {}
    // todo: add consteval static string ctor for reduced dynamic storage
  };

  struct Name {
    GlobalNameSymbol symbol;
    HashT _hash;

    NameData & get_data() const;
    bool is_valid() const;
    StrView str() const { return get_data().str; }

    TESL_ALWAYS_INLINE bool operator==(Name other) const { return symbol == other.symbol; }
    TESL_ALWAYS_INLINE bool operator!=(Name other) const { return symbol != other.symbol; }

    TESL_ALWAYS_INLINE bool operator==(StrView p_str) const { return get_data() == p_str; }
    TESL_ALWAYS_INLINE bool operator!=(StrView p_str) const { return get_data() != p_str; }
    TESL_ALWAYS_INLINE friend bool operator==(StrView p_str, Name name) { return p_str == name.get_data(); }
    TESL_ALWAYS_INLINE friend bool operator!=(StrView p_str, Name name) { return p_str != name.get_data(); }

    TESL_ALWAYS_INLINE friend HashT hash(const Name & v) { return v._hash; }

    TESL_ALWAYS_INLINE friend const NameData & format_as(const Name & v) { return v.get_data(); }

    Name & operator=(const Name & other) = default;
    Name & operator=(Name && other) = default;

    Name(const Name & other) = default;
    Name(Name && other) = default;

    Name(GlobalNameSymbol s);
  };
}

template<>
struct fmt::formatter<tesl::NameData, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::NameData & v, Context & ctx) const {
    return format_to(ctx.out(), "{}", v.str);
  }
};

template<>
struct fmt::formatter<tesl::Name, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Name & name, Context & ctx) const {
    if (!name.is_valid()) {
      return format_to(ctx.out(), "<invalid-name>");
    }

    return format_to(ctx.out(), "{}", name.get_data());
  }
};

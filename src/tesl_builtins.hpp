#pragma once

#include "tesl_common.hpp"
#include "tesl_env.hpp"
#include "tesl_symbol.hpp"
#include "tesl_type.hpp"
#include <array>
#include <type_traits>
#include <utility>

namespace tesl {
  namespace builtin {
    constexpr StrView name_strings[] = {
#define TESL_BUILTIN_NAME_DEF(str) #str,
#include "tesl_builtin_names.inc"
    };
    constexpr auto name_count = std::size(name_strings);
    static_assert(std::cmp_less_equal(name_count, GlobalNameSymbol::max_index));

    consteval GlobalNameSymbol find_name_symbol(StrView str) {
      for (GlobalSymbolIndexT i = 0; i < builtin::name_count; ++i) {
        if (str == builtin::name_strings[i]) {
          return GlobalNameSymbol{i};
        }
      }

      return {};
    }

    constexpr StrView signature_strings[] = {
#define TESL_BUILTIN_SIGNATURE_DEF(str) str,
#include "tesl_builtin_signatures.inc"
    };
    constexpr auto signature_count = std::size(signature_strings);
    static_assert(std::cmp_less_equal(signature_count, GlobalSignatureSymbol::max_index));

    consteval GlobalSignatureSymbol find_signature_symbol(StrView str) {
      for (GlobalSymbolIndexT i = 0; i < builtin::signature_count; ++i) {
        if (str == builtin::signature_strings[i]) {
          return GlobalSignatureSymbol{i};
        }
      }

      return {};
    }
    
    constexpr StrView global_type_name_strings[] = {
#define TESL_BUILTIN_TYPE_DEF(type, local_name, global_name) global_name,
#include "tesl_builtin_types.inc"
    };
    constexpr auto type_count = std::size(global_type_name_strings);
    static_assert(std::cmp_less_equal(type_count, GlobalTypeSymbol::max_index));

    consteval GlobalTypeSymbol find_type_symbol(StrView str) {
      for (GlobalSymbolIndexT i = 0; i < builtin::type_count; ++i) {
        if (str == builtin::global_type_name_strings[i]) {
          return GlobalTypeSymbol{i};
        }
      }

      return {};
    }

    extern TypeInfoData type_info_data[type_count];

    template<typename T> consteval GlobalTypeSymbol get_type_symbol_of() {
      static_assert(false, "unknown type");
      return {};
    }

#define TESL_BUILTIN_TYPE_DEF(type, local_name, global_name) \
  template<> consteval GlobalTypeSymbol get_type_symbol_of<type>() { \
    return find_type_symbol(global_name); \
  }
#include "tesl_builtin_types.inc"

    template<typename T> consteval GlobalNameSymbol get_local_type_name_of() {
      static_assert(false, "unknown type");
      return {};
    }

#define TESL_BUILTIN_TYPE_DEF(type, local_name, global_name) \
  template<> consteval GlobalNameSymbol get_local_type_name_of<type>() { \
    return find_name_symbol(local_name); \
  }
#include "tesl_builtin_types.inc"

    template<typename T> consteval GlobalNameSymbol get_global_type_name_of() {
      static_assert(false, "unknown type");
      return {};
    }

#define TESL_BUILTIN_TYPE_DEF(type, local_name, global_name) \
  template<> consteval GlobalNameSymbol get_global_type_name_of<type>() { \
    return find_name_symbol(global_name); \
  }
#include "tesl_builtin_types.inc"

    template<typename T> constexpr TypeInfoData make_type_info_of();

/*#define TESL_BUILTIN_TYPE_DEF(type, local_name, global_name) \
  template<> TypeInfoData make_type_info_of<type>();
#include "tesl_builtin_types.inc"*/

  }

  namespace detail {
    template<typename T>
    void default_init(void * this_, void * args, void * ret) {
      new (this_) T();
    }

    template<typename T>
    void default_copy_init(void * this_, void * args, void * ret) {
      if constexpr (std::is_copy_constructible_v<T>) {
        new (this_) T(*reinterpret_cast<const T *>(args));
      } else {
        TESL_FAIL_MSG(return, "type is not copyable");
      }
    }

    template<typename T>
    void default_move_init(void * this_, void * args, void * ret) {
      if constexpr (std::is_move_constructible_v<T>) {
        new (this_) T(MOV(*reinterpret_cast<T *>(args)));
      } else {
        TESL_FAIL_MSG(return, "type is not movable");
      }
    }

    template<typename T>
    void default_deinit(void * this_, void * args, void * ret) {
      reinterpret_cast<T *>(this_)->~T();
    }
  }
}

#define TESL_BUILTIN_NAME(name) \
  ([]() -> GlobalNameSymbol { \
    constexpr GlobalNameSymbol _name_symbol =::tesl::builtin::find_name_symbol(name); \
    static_assert(_name_symbol.is_valid(), "unknown name '" name "' (a new entry may be needed in tesl_builtin_names.inc)"); \
    return _name_symbol; \
  }())

#define TESL_BUILTIN_SIGNATURE(sig) \
  ([]() -> GlobalSignatureSymbol { \
    constexpr GlobalSignatureSymbol _signature_symbol = ::tesl::builtin::find_signature_symbol(sig); \
    static_assert(_signature_symbol.is_valid(), "unknown signature '" sig "' (a new entry may be needed in tesl_builtin_signatures.inc)"); \
    return _signature_symbol; \
  }())

#define TESL_BUILTIN_TYPE(typename) \
  ([]() -> GlobalTypeSymbol { \
    constexpr GlobalTypeSymbol _type_symbol = ::tesl::builtin::find_type_symbol(typename); \
    static_assert(_type_symbol.is_valid(), "unknown type '" typename "' (a new entry may be needed in tesl_builtin_types.inc)"); \
    return _type_symbol; \
  }())

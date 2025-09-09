#pragma once

#include "tesl_common.hpp"
#include "tesl_error.hpp"
#include "tesl_type.hpp"
#include <array>
#include <utility>

namespace tesl {
  namespace detail {
    constexpr const char * basic_signature_strings[] = {
#define TESL_SYMBOL_SIGNATURE_DEF(str) str,
#include "tesl_signatures.inc"
    };
    constexpr size_t basic_signature_count = std::size(basic_signature_strings);

    constexpr GlobalSymbolIndex find_basic_signature_index(CharStrView str) {
      size_t i = 0;
      for (const auto & sig : basic_signature_strings) {
        if (str == sig) {
          return {static_cast<GlobalSymbolIndex::IndexT>(i)};
        }
        i++;
      }

      return {};
    }

    struct BasicSignatureIndex {
      GlobalSymbolIndex value;
      constexpr BasicSignatureIndex(GlobalSymbolIndex i) : value(i) {}
      constexpr BasicSignatureIndex(CharStrView str) : value(find_basic_signature_index(str)) {}
      constexpr BasicSignatureIndex(const char * str) : value(find_basic_signature_index(str)) {}
    };

    SignatureRef get_basic_signature(detail::BasicSignatureIndex index);

    constexpr const char * basic_names_strings[] = {
#define TESL_SYMBOL_NAME_DEF(str) str,
#include "tesl_names.inc"
    };
    constexpr size_t basic_name_count = std::size(basic_names_strings);

    constexpr GlobalSymbolIndex find_basic_name_index(CharStrView str) {
      size_t i = 0;
      for (const auto & sig : basic_names_strings) {
        if (str == sig) {
          return {static_cast<GlobalSymbolIndex::IndexT>(i)};
        }
        i++;
      }

      return {};
    }

    struct BasicNameIndex {
      GlobalSymbolIndex value;
      constexpr BasicNameIndex(GlobalSymbolIndex i) : value(i) {}
      constexpr BasicNameIndex(CharStrView str) : value(find_basic_name_index(str)) {}
      constexpr BasicNameIndex(const char * str) : value(find_basic_name_index(str)) {}
    };

    NameRef get_basic_name(detail::BasicNameIndex index);
  }

  namespace detail {
    template<typename T>
    void default_init(FnContext * context, void * args, void * ret) {
      new (ret) T();
    }

    template<typename T>
    void default_copy(FnContext * context, void * args, void * ret) {
      new (ret) T(*reinterpret_cast<const T *>(args));
    }

    template<typename T>
    void default_move(FnContext * context, void * args, void * ret) {
      new (ret) T(MOV(*reinterpret_cast<T *>(args)));
    }

    template<typename T>
    void default_deinit(FnContext * context, void * args, void * ret) {
      reinterpret_cast<T *>(args)->~T();
    }

    template<typename T>
    Ref<TypeInfo> new_default_type_info(StrView pLocalName, StrView pGlobalName, GlobalSymbolIndex pCoersionSignature) {
      return new_ref<TypeInfo>(
        pLocalName,
        pGlobalName,
        pCoersionSignature,
        sizeof(T),
        alignof(T),
        FnPtrBare{default_init<T>},
        FnPtrBare{default_copy<T>},
        FnPtrBare{default_move<T>},
        FnPtrBare{default_deinit<T>}
      );
    }
  }
}

#define TESL_NAME_INDEX(name) \
  []() { \
    constexpr auto ret = ::tesl::detail::find_basic_name_index(name); \
    static_assert(ret.is_valid(), "unknown name '" name "' (a new entry may be needed in tesl_names.inc)"); \
    return ret; \
  }()

#define TESL_SIGNATURE_INDEX(sig) \
  []() { \
    constexpr auto ret = ::tesl::detail::find_basic_signature_index(sig); \
    static_assert(ret.is_valid(), "unknown signature '" sig "' (a new entry may be needed in tesl_signatures.inc)"); \
    return ret; \
  }()

#define TESL_NEW_BUILTIN_TYPE_INFO(type, name) \
  ::tesl::detail::new_default_type_info<type>( \
    TESL_STRVIEW(name), \
    TESL_STRVIEW(name), \
    TESL_SIGNATURE_INDEX("->" name) \
  )

#define TESL_BIND_BASIC_BARE(type, fn, sig) \
  do { \
    constexpr auto _signature_index = ::tesl::detail::find_basic_signature_index(sig); \
    static_assert(_signature_index.is_valid(), "unknown signature '" sig "' (a new entry may be needed in tesl_signatures.inc)"); \
    TESL_FAIL_COND_MSG(type->operators.is_full(), "too many operators for type '{}' ({})", type->global_name, type->operators.size()); \
    TESL_FAIL_COND_MSG(type->operators.has(_signature_index), "operator '{}' already defined in type '{}'", sig, type->global_name); \
    type->operators.insert(_signature_index, Operator::make_bare({fn, detail::get_basic_signature(_signature_index)})); \
  } while (false);

#pragma once

#include "tesl_common.hpp"
#include "tesl_fmt.hpp"
#include <memory>
#include <type_traits>

namespace tesl {
  namespace detail {
    template<typename T>
    struct ResultStorageT {
      struct InconvertableType {};

      using storage_type = std::conditional_t<std::is_reference_v<T>, std::remove_reference_t<T> *, T>;
      using ref_type = std::conditional_t<std::is_reference_v<T>, T, T &>;
      using const_ref_type = std::conditional_t<std::is_reference_v<T>, T, const T &>;

      using rvalue_ref_type = std::conditional_t<std::is_reference_v<T>, InconvertableType, T &&>;

      using ptr_type = std::remove_reference_t<T> *;
      using const_ptr_type = std::remove_reference_t<const T> *;


      storage_type data;

      constexpr ref_type get() {
        if constexpr (std::is_reference_v<T>) {
          return *data;
        } else {
          return data;
        }
      }

      constexpr ref_type get() const {
        if constexpr (std::is_reference_v<T>) {
          return *data;
        } else {
          return data;
        }
      }

      template<typename ... Args>
      constexpr void emplace_data(Args && ... args) {
        std::construct_at(std::addressof(data), FWD(args)...);
      }

      template<typename ... Args>
      constexpr void emplace(Args && ... args) {
        if constexpr (std::is_reference_v<T>) {
          data = std::addressof(T{FWD(args)...});
        } else {
          std::construct_at(std::addressof(data), FWD(args)...);
        }
      }

      template<typename U>
      constexpr void assign_data(U && v) {
        data = FWD(v);
      }

      template<typename U>
      constexpr void assign(U && v) {
        if constexpr (std::is_reference_v<T>) {
          data = std::addressof(T{FWD(v)});
        } else {
          data = FWD(v);
        }
      }

      constexpr void destroy() {
        if constexpr (std::is_reference_v<T>) {
          // if we accidentally try to dereference after this, at least it will segfault
          data = nullptr;
        } else {
          data.~T();
        }
      }

      template<typename ... Args>
      static constexpr storage_type _make_data(Args && ... args) {
        if constexpr (std::is_reference_v<T>) {
          return std::addressof(static_cast<T>(FWD(args))...);
        } else {
          return T{FWD(args)...};
        }
      }

      constexpr ResultStorageT & operator=(const ResultStorageT & other) = default;
      constexpr ResultStorageT & operator=(ResultStorageT && other) = default;

      constexpr ResultStorageT(const ResultStorageT & other) = default;
      constexpr ResultStorageT(ResultStorageT && other) = default;

      template<typename ... Args>
      constexpr ResultStorageT(Args && ... args) : data(_make_data(FWD(args)...)) {}
      constexpr ~ResultStorageT() = default;
    };
  }

  template<typename ValueT, typename ErrorT>
  struct [[nodiscard]] Result {
  private:
    using ValueStorageT = detail::ResultStorageT<ValueT>;
    using ErrorStorageT = detail::ResultStorageT<ErrorT>;

    bool _is_ok;
    union {
      ValueStorageT _value;
      ErrorStorageT _error;
    };

    template<typename VDT>
    constexpr void _assign_value_data(VDT && value_data) {
      if (_is_ok) {
        _value.assign_data(FWD(value_data));
      } else {
        _error.destroy();
        _is_ok = true;
        _value.emplace_data(FWD(value_data));
      }
    }

    template<typename EDT>
    constexpr void _assign_error_data(EDT && error_data) {
      if (!_is_ok) {
        _error.assign_data(FWD(error_data));
      } else {
        _value.destroy();
        _is_ok = false;
        _error.emplace_data(FWD(error_data));
      }
    }

    template<typename VT>
    constexpr void _assign_value(VT && value) {
      if (_is_ok) {
        _value.assign(FWD(value));
      } else {
        _error.destroy();
        _is_ok = true;
        _value.emplace(FWD(value));
      }
    }

    template<typename ET>
    constexpr void _assign_error(ET && error) {
      if (!_is_ok) {
        _error.assign(FWD(error));
      } else {
        _value.destroy();
        _is_ok = false;
        _error.emplace(FWD(error));
      }
    }

  public:
    constexpr bool is_ok() const {
      return _is_ok;
    }

    constexpr bool is_err() const {
      return !_is_ok;
    }

    constexpr operator bool() const {
      return is_ok();
    }

    constexpr void swap(Result & other) {
      if (other._is_ok) {
        if (_is_ok) {
          swap(_value.data, other._value.data);
        } else {
          ErrorStorageT tmp_error{MOV(_error)};
          _error.destroy();
          _is_ok = true;
          _value.emplace_data(MOV(other._value));
          other._value.destroy();
          other._is_ok = false;
          other._error.emplace_data(tmp_error);
        }
      } else {
        if (_is_ok) {
          ValueStorageT tmp_value{MOV(_value)};
          _value.destroy();
          _is_ok = false;
          _error.emplace_data(MOV(other._error));
          other._error.destroy();
          other._is_ok = true;
          other._value.emplace_data(tmp_value);
        } else {
          swap(_error.data, other._error.data);
        }
      }
    }

    constexpr friend void swap(Result & a, Result & b) { a.swap(b); }

    constexpr ValueStorageT::ref_type unwrap() {
      TESL_ASSERT_MSG(_is_ok, "result is not ok: {}", _error.get());
      return _value.get();
    }

    constexpr ValueStorageT::const_ref_type unwrap() const {
      TESL_ASSERT_MSG(_is_ok, "result is not ok: {}", _error.get());
      return _value.get();
    }

    constexpr ErrorStorageT::ref_type unwrap_err() {
      TESL_ASSERT_MSG(!_is_ok, "result is ok: {}", _value.get());
      return _error.get();
    }

    constexpr ErrorStorageT::const_ref_type unwrap_err() const {
      TESL_ASSERT_MSG(!_is_ok, "result is ok: {}", _value.get());
      return _error.get();
    }

    constexpr auto operator*() {
      return unwrap();
    }

    constexpr auto operator*() const {
      return unwrap();
    }

    constexpr auto operator->() {
      return &unwrap();
    }

    constexpr auto operator->() const {
      return &unwrap();
    }

    constexpr auto operator=(ValueStorageT::ref_type v) {
      _assign_value(FWD(v));
      return *this;
    }

    constexpr auto operator=(ValueStorageT::rvalue_ref_type v) {
      _assign_value(FWD(v));
      return *this;
    }

    constexpr auto operator=(ErrorStorageT::ref_type v) {
      _assign_error(FWD(v));
      return *this;
    }

    constexpr auto operator=(ErrorStorageT::rvalue_ref_type v) {
      _assign_error(FWD(v));
      return *this;
    }

    template<typename V, typename E>
    constexpr Result & operator=(const Result<V, E> & other) {
      if (other._is_ok) {
        _assign_value_data(other._value.data);
      } else {
        _assign_error_data(other._error.data);
      }
      return *this;
    }

    template<typename V, typename E>
    constexpr Result & operator=(Result<V, E> && other) {
      if (other._is_ok) {
        _assign_value_data(MOV(other._value.data));
      } else {
        _assign_error_data(MOV(other._error.data));
      }
      return *this;
    }

    constexpr Result(ValueStorageT::ref_type value) : _is_ok(true), _value(FWD(value)) {}
    constexpr Result(ValueStorageT::rvalue_ref_type value) : _is_ok(true), _value(FWD(value)) {}
    
    constexpr Result(ErrorStorageT::ref_type error) : _is_ok(false), _error(FWD(error)) {}
    constexpr Result(ErrorStorageT::rvalue_ref_type error) : _is_ok(false), _error(FWD(error)) {}
    
    constexpr ~Result() {
      if (_is_ok) {
        _value.destroy();
      } else {
        _error.destroy();
      }
    }
  };
}

template<typename V, typename E>
class fmt::formatter<tesl::Result<V, E>, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Result<V, E> & result, Context & ctx) const {
    if (result.is_ok()) {
      return format_to(ctx.out(), "{}", result.unwrap());
    } else {
      return format_to(ctx.out(), "{}", result.unwrap());
    }
  }
};

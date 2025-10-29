#pragma once

#include "tesl_common.hpp"
#include "tesl_result.hpp"

namespace tesl {
  namespace detail {
    struct OptionalNullT {
      template<typename ParseCtx, typename FormatCtx>
      void format(ParseCtx & parse_ctx, FormatCtx & ctx) {
        ctx.advance_to(format_to(ctx.out(), "null option"));
      }
    };
  }

  template<typename T>
  struct Optional {
  private:
    Result<T, detail::OptionalNullT> _impl{detail::OptionalNullT{}};

  public:
    constexpr bool has_value() {
      return _impl.is_ok();
    }

    constexpr bool has_value() const {
      return _impl.is_ok();
    }

    constexpr auto && unwrap() {
      return _impl.unwrap();
    }

    constexpr auto && unwrap() const {
      return _impl.unwrap();
    }

    constexpr void reset() {
      _impl = Null{};
    }

    constexpr void swap(Optional & other) {
      _impl.swap(other._impl);
    }

    template<typename U>
    constexpr Optional & operator=(U && value) {
      _impl = FWD(value);
      return *this;
    }

    template<typename U>
    constexpr Optional & operator=(const Optional<U> & other) {
      _impl = other._impl;
      return *this;
    }

    template<typename U>
    constexpr Optional & operator=(Optional<U> && other) {
      _impl = MOV(other._impl);
      return *this;
    }

    template<typename U>
    constexpr Optional(U && value) : _impl(FWD(value)) {}

    constexpr Optional() = default;
    constexpr ~Optional() = default;
  };
  /*template<typename T>
  struct Optional {
  private:
    using storage_type = std::conditional_t<std::is_reference_v<T>, std::remove_reference_t<T> *, T>;
    using ref_type = std::conditional_t<std::is_reference_v<T>, T, T &>;
    using const_ref_type = std::conditional_t<std::is_reference_v<T>, T, const T &>;
    using ptr_type = std::remove_reference_t<T> *;
    using const_ptr_type = std::remove_reference_t<const T> *;

    std::aligned_storage_t<sizeof(storage_type), alignof(storage_type)> _value_data;
    bool _has_value = false;

    constexpr storage_type & _get_value() {
      return *reinterpret_cast<storage_type *>(&_value_data);
    }

    constexpr const storage_type & _get_value() const {
      return *reinterpret_cast<const storage_type *>(&_value_data);
    }

    template<typename ... Args>
    constexpr void _emplace_unchecked(Args && ... args) {
      if constexpr (std::is_reference_v<T>) {
        _get_value() = &(T{FWD(args)...});
      } else {
        new(&_get_value()) T{FWD(args)...};
      }
      _has_value = true;
    }

    template<typename U>
    constexpr void _assign_storage(U && value) {
      if (_has_value) {
          _get_value() = FWD(value);
      } else {
        _emplace_unchecked(FWD(value));
      }
    }

  public:
    constexpr bool has_value() {
      return _has_value;
    }

    constexpr bool has_value() const {
      return _has_value;
    }

    constexpr operator bool() {
      return has_value();
    }

    constexpr operator bool() const {
      return has_value();
    }

    constexpr void reset() {
      // no need to destroy if its a reference
      if constexpr (!std::is_reference_v<T>) {
        if (_has_value) {
          _get_value().~T();
        }
      }
    }

    constexpr void swap(Optional & other) {
      if (_has_value && other._has_value) {
        swap(_get_value(), other._get_value());
      } else if (_has_value) {
        new(&other._get_value()) T(MOV(_get_value()));
        reset();
      } else if (other._has_value) {
        new(&_get_value()) T(MOV(other._get_value()));
        reset();
      }
    }

    template<typename ... Args>
    constexpr void emplace(Args && ... args) {
      reset();
      _emplace_unchecked(FWD(args)...);
      return *this;
    }

    constexpr ref_type value() {
      TESL_ASSERT_MSG(_has_value, "optional value is empty!");
      if constexpr (std::is_reference_v<T>) {
        // extra dereference bc we store references as pointers
        return *_get_value();
      } else {
        return _get_value();
      }
    }

    constexpr const_ref_type value() const {
      TESL_ASSERT_MSG(_has_value, "optional value is empty!");
      if constexpr (std::is_reference_v<T>) {
        // extra dereference bc we store references as pointers
        return *_get_value();
      } else {
        return _get_value();
      }
    }

    constexpr ref_type operator*() {
      return value();
    }

    constexpr const_ref_type operator*() const {
      return value();
    }

    constexpr ptr_type operator->() {
      return &value();
    }

    constexpr const_ptr_type operator->() const {
      return &value();
    }

    template<typename U>
    constexpr Optional & operator=(U && value) {
      if constexpr (std::is_reference_v<T>) {
        static_assert(std::is_reference_v<U>);
        _assign_storage(&value);
      } else {
        _assign_storage(FWD(value));
      }
      return *this;
    }

    template<typename U>
    constexpr Optional & operator=(const Optional<U> & other) {
      if (other._has_value) {
        _assign_storage(other._get_value());
      } else {
        reset();
      }
      return *this;
    }

    template<typename U>
    constexpr Optional & operator=(Optional<U> && other) {
      if (other._has_value) {
        _assign_storage(MOV(other._get_value()));
      } else {
        reset();
      }
      return *this;
    }

    template<typename U>
    constexpr Optional(U && value) {
      _emplace_unchecked(FWD(value));
    }

    constexpr Optional() = default;

    constexpr ~Optional() {
      reset();
    }
  };*/
}
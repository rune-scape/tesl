#pragma once

#include "tesl_common.hpp"

#include "tesl_optional.hpp"
#include <coroutine>

namespace tesl::coroutine {
  template<typename T>
  struct Generator {
    struct promise_type {
      union {
        int _uninitialized = 0;
        T _value;
      };
      bool _has_value = false;

      std::suspend_always yield_value(T && v) {
        _value = MOV(v);
        _has_value = true;
        return {};
      }
      std::suspend_always yield_value(const T & v) {
        _value = v;
        _has_value = true;
        return {};
      }
      void return_void() {}
      std::suspend_always initial_suspend() { return {}; }
      std::suspend_always final_suspend() noexcept { return {}; }
      Generator get_return_object() { return {std::coroutine_handle<promise_type>::from_promise(*this)}; }
      void unhandled_exception() {
        TESL_FAIL_MSG(std::terminate(), "unhandled exception in coroutine");
      }

      promise_type() {}

      ~promise_type() {
        if (_has_value) {
          _value.~T();
        }
      }
    };

    using HandleT = std::coroutine_handle<promise_type>;

    struct Sentinel;

    struct Iterator {
      HandleT _coro;

      Iterator & operator++() {
        _coro.resume();
        return *this;
      }

      T & operator*() const {
        // shouldnt be possible to trigger this assert, just for safety
        TESL_ASSERT(_coro.promise()._has_value);
        return _coro.promise()._value;
      }

      T * operator->() const {
        // shouldnt be possible to trigger this assert, just for safety
        TESL_ASSERT(_coro.promise()._has_value);
        return &_coro.promise()._value;
      }

      bool operator==(const Iterator & rhs) const { return true; }
      bool operator!=(const Iterator & rhs) const { return true; }
      bool operator==(Sentinel rhs) const { return !_coro || _coro.done(); }
      bool operator!=(Sentinel rhs) const { return !this->operator==(rhs); }
      friend bool operator==(Sentinel lhs, const Iterator & rhs) { return rhs.operator==(lhs); }
      friend bool operator!=(Sentinel lhs, const Iterator & rhs) { return rhs.operator!=(lhs); }

      Iterator(HandleT h) : _coro(h) {}
    };

    struct ConstIterator {
      HandleT _coro;

      ConstIterator & operator++() {
        _coro.resume();
        return *this;
      }

      const T & operator*() const {
        // shouldnt be possible to trigger this assert, just for safety
        TESL_ASSERT(_coro.promise()._has_value);
        return _coro.promise()._value;
      }

      const T * operator->() const {
        // shouldnt be possible to trigger this assert, just for safety
        TESL_ASSERT(_coro.promise()._has_value);
        return &_coro.promise()._value;
      }

      bool operator==(const ConstIterator & rhs) const { return true; }
      bool operator!=(const ConstIterator & rhs) const { return true; }
      bool operator==(Sentinel rhs) const { return !_coro || _coro.done(); }
      bool operator!=(Sentinel rhs) const { return !this->operator==(rhs); }
      friend bool operator==(Sentinel lhs, const ConstIterator & rhs) { return rhs.operator==(lhs); }
      friend bool operator!=(Sentinel lhs, const ConstIterator & rhs) { return rhs.operator!=(lhs); }

      ConstIterator(HandleT h) : _coro(h) {}
    };

    struct Sentinel {};
    
    HandleT _coro;
    mutable bool _started = false;

    bool done() {
      return !_coro || _coro.done();
    }

    Optional<T &> current() {
      TESL_ASSERT(_coro);
      if (!_started) {
        _started = true;
        _coro.resume();
      }
      if (done()) {
        return {};
      }
      auto & promise = _coro.promise();
      TESL_ASSERT(promise._has_value);
      return promise._value;
    }

    Optional<T &> next() {
      TESL_ASSERT(_coro);
      if (done()) {
        return {};
      }
      _coro.resume();
      auto & promise = _coro.promise();
      TESL_ASSERT(promise._has_value);
      return promise._value;
    }

    Iterator begin() {
      if (_coro && !_started) {
        _started = true;
        _coro.resume();
      }
      return {_coro};
    }

    ConstIterator begin() const {
      if (_coro && !_started) {
        _started = true;
        _coro.resume();
      }
      return {_coro};
    }

    Sentinel end() {
      return {};
    }

    Sentinel end() const {
      return {};
    }

    Generator & operator=(const Generator &) = delete;
    Generator & operator=(Generator && other) {
      swap(_coro, other._coro);
      swap(_started, other._started);
      return *this;
    }

    Generator(const Generator &) = delete;
    Generator(Generator && other) : _coro(MOV(other._coro)), _started(other._started) {
      other._coro = HandleT{};
      other._started = false;
    }

    Generator() {}

    ~Generator() {
      if (_coro) {
        _coro.destroy();
      }
    }

  private:
    Generator(HandleT h) : _coro(h) {}
  };
}
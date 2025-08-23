#pragma once

#include "tesl_common.hpp"

#include <vector>

namespace tesl {
  /*template<typename T, bool = (std::is_trivially_default_constructible_v<T> && std::is_trivially_destructible_v<T>)>
  union ArrayStorageT {
    char _storage[sizeof(T)];
    T value;
  };

  template<typename T>
  union ArrayStorageT<T, false> {
    char _storage[sizeof(T)];
    T value;

    constexpr ArrayStorageT() {}
    constexpr ~ArrayStorageT() {}
  };

  namespace detail {
    template<typename T, IntT LocalCapacity = 0>
    struct ArrayBaseT {
    protected:
      ArrayStorageT<T> * _data = _local_storage;
      IntT _size = 0;
      IntT _capacity = LocalCapacity;
      ArrayStorageT<T> _local_storage[LocalCapacity];
      constexpr bool is_heap_allocated() { return _data != _local_storage; }
      constexpr void reset_data() {
        _capacity = LocalCapacity;
        _data = _local_storage;
      }
      void swap(ArrayBaseT & other) {
        if (!is_heap_allocated()) {
          _data = other._local_storage;
          if (!other.is_heap_allocated()) {
            other._data = _local_storage;
            // both are local
            if (_size > other._size) {
              for (IntT i = 0; i < other._size; ++i) {
                tesl::swap(other._local_storage[i].value, _local_storage[i].value);
              }
              for (IntT i = other._size; i < _size; ++i) {
                new(&other._local_storage[i].value) T(MOV(_local_storage[i].value));
              }
            } else {
              for (IntT i = 0; i < _size; ++i) {
                tesl::swap(_local_storage[i].value, other._local_storage[i].value);
              }
              for (IntT i = _size; i < other._size; ++i) {
                new(&_local_storage[i].value) T(MOV(other._local_storage[i].value));
              }
            }
          } else {
            // only this is local
            for (IntT i = 0; i < _size; ++i) {
              new(&other._local_storage[i].value) T(MOV(_local_storage[i].value));
            }
          }
        } else if (!other.is_heap_allocated()) {
          other._data = _local_storage;
          // only other is local
          for (IntT i = 0; i < _size; ++i) {
            new(&_local_storage[i].value) T(MOV(other._local_storage[i].value));
          }
        }
        memswap(_data, other._data);
        memswap(_size, other._size);
        memswap(_capacity, other._capacity);
      }
    };

    template<typename T>
    struct ArrayBaseT<T, 0> {
    protected:
      ArrayStorageT<T> * _data = nullptr;
      IntT _size = 0;
      IntT _capacity = 0;
      constexpr bool is_heap_allocated() { return _data != nullptr; }
      constexpr void reset_data() {
        _capacity = 0;
        _data = nullptr;
      }
      void swap(ArrayBaseT & other) {
        memswap(*this, other);
      }
    };
  } // namespace detail

  // capacity doesn't shink unless cleared
  template<typename T, IntT LocalCapacity = 1>
  struct Array : detail::ArrayBaseT<T, LocalCapacity> {
    using base = detail::ArrayBaseT<T, LocalCapacity>;

    TESL_ALWAYS_INLINE T * data() { return reinterpret_cast<T *>(base::_data); }
    TESL_ALWAYS_INLINE const T * data() const { return reinterpret_cast<const T *>(base::_data); }
    TESL_ALWAYS_INLINE constexpr IntT size() const { return base::_size; }
    TESL_ALWAYS_INLINE constexpr IntT is_empty() const { return base::_size <= 0; }

    constexpr TESL_ALWAYS_INLINE T * begin() { return data(); }
    constexpr TESL_ALWAYS_INLINE const T * begin() const { return data(); }
    constexpr TESL_ALWAYS_INLINE T * end() { return data() + base::_size; }
    constexpr TESL_ALWAYS_INLINE const T * end() const { return data() + base::_size; }

    TESL_ALWAYS_INLINE constexpr T & operator[](IntT i) {
      TESL_ASSERT(base::_data != nullptr);
      TESL_ASSERT(i >= 0 && i < base::_size);
      return base::_data[i].value;
    }

    TESL_ALWAYS_INLINE constexpr const T & operator[](IntT i) const {
      TESL_ASSERT(base::_data != nullptr);
      TESL_ASSERT(i >= 0 && i < base::_size);
      return base::_data[i].value;
    }

    constexpr TESL_ALWAYS_INLINE T & front() { return operator[](0); }
    constexpr TESL_ALWAYS_INLINE const T & front() const { return operator[](0); }
    constexpr TESL_ALWAYS_INLINE T & back() { return operator[](base::_size - 1); }
    constexpr TESL_ALWAYS_INLINE const T & back() const { return operator[](base::_size - 1); }

    constexpr void reserve(IntT requested_capacity, bool big_grow = false) {
      if (requested_capacity <= base::_capacity) {
        return;
      }

      if (big_grow) {
        IntT min_capacity = base::_capacity + (base::_capacity >> 1) + 1;
        requested_capacity = max(min_capacity, requested_capacity);
      }

      ArrayStorageT<T> *m_old_data = base::_data;
      bool was_heap_allocated = base::is_heap_allocated();
      base::_data = new ArrayStorageT<T>[requested_capacity];
      for (IntT i = 0; i < base::_size; ++i) {
        new(&base::_data[i].value) T(MOV(m_old_data[i].value));
      }
      if (was_heap_allocated) {
        delete[] reinterpret_cast<char *>(m_old_data);
      }

      base::_capacity = requested_capacity;
    }

    constexpr void resize(IntT requested_size) {
      reserve(requested_size);
      for (IntT i = base::_size; i < requested_size; ++i) {
        base::_size = i + 1;
        new(&base::_data[i].value) T();
      }
    }

    template<typename VT>
    constexpr IntT find(VT && v, IntT from = 0) const {
      for (IntT i = from; i < base::_size; ++i) {
        if (FWD(v) == base::_data[i].value) {
          return i;
        }
      }

      return -1;
    }

    template<typename VT>
    TESL_ALWAYS_INLINE constexpr bool has(VT && v) const {
      return find(FWD(v)) >= 0;
    }

    constexpr T & insert(IntT at, const T & v) {
      TESL_ASSERT(base::_data != nullptr);
      TESL_ASSERT(at >= 0 && at <= base::_size);

      reserve(base::_size + 1, true);

      TESL_ASSERT(base::_capacity > base::_size);

      IntT old_size = base::_size++;
      for (IntT i = old_size; i > at; --i) {
        base::_data[i].value = MOV(base::_data[i - 1].value);
      }

      return *new(&base::_data[at].value) T(FWD(v));
    }

    constexpr T & insert(IntT at, T && v) {
      TESL_ASSERT(base::_data != nullptr);
      TESL_ASSERT(at >= 0 && at <= base::_size);

      reserve(base::_size + 1, true);

      TESL_ASSERT(base::_capacity > base::_size);

      IntT old_size = base::_size++;
      for (IntT i = old_size; i > at; --i) {
        base::_data[i].value = MOV(base::_data[i - 1].value);
      }

      return *new(&base::_data[at].value) T(FWD(v));
    }

    constexpr void remove_at(IntT at) {
      TESL_ASSERT(base::_data != nullptr);
      TESL_ASSERT(at >= 0 && at < base::_size);

      if constexpr (!std::is_trivially_destructible_v<T>) {
        base::_data[at].~T();
      }

      for (IntT i = at; i < base::_size; ++i) {
        base::_data[i].value = MOV(base::_data[i + 1].value);
      }

      base::_size--;
    }

    TESL_ALWAYS_INLINE constexpr T & push_back(T && v) {
      return insert(base::_size, MOV(v));
    }

    TESL_ALWAYS_INLINE constexpr T & push_back(const T & v) {
      return insert(base::_size, v);
    }

    TESL_ALWAYS_INLINE constexpr T & push_front(T && v) {
      return insert(0, MOV(v));
    }

    TESL_ALWAYS_INLINE constexpr T & push_front(const T & v) {
      return insert(0, v);
    }

    constexpr void clear(bool deallocate = true) {
      if constexpr (std::is_trivially_destructible_v<T>) {
        base::_size = 0;
      } else {
        for (IntT i = base::_size - 1; i >= 0; --i) {
          base::_data[i].value.~T();
          base::_size = i;
        }
      }
      if (deallocate) {
        if (base::is_heap_allocated()) {
          delete[] base::_data;
        }
        base::reset_data();
      }
    }

    template<IntT LC>
    constexpr Array & operator+=(const Array<T, LC> & other) {
      reserve(base::_size + other.size());
      for (IntT i = base::_size, j = 0; j < other.size(); ++i, ++j) {
        base::_size = i + 1;
        new(&base::_data[i].value) T(other._data[j].value);
      }
      return *this;
    }

    constexpr Array & operator+=(const ArrayView<T> & buffer) {
      reserve(base::_size + buffer.size());
      for (IntT i = base::_size, j = 0; j < buffer.size(); ++i, ++j) {
        base::_size = i + 1;
        new(&base::_data[i].value) T(buffer[j]);
      }
      return *this;
    }

    constexpr Array & operator+=(const ArrayView<const T> & buffer) {
      reserve(base::_size + buffer.size());
      for (IntT i = base::_size, j = 0; j < buffer.size(); ++i, ++j) {
        base::_size = i + 1;
        new(&base::_data[i].value) T(buffer[j]);
      }
      return *this;
    }

    template<typename TOther>
    TESL_ALWAYS_INLINE constexpr void operator+(const TOther & other) const {
      Array result = *this;
      return result += other;
    }

    constexpr bool operator==(const Array & other) const {
      if (base::_size != other._size) return false;

      for (IntT i = 0; i < base::_size; ++i) {
        if (base::_data[i].value != other.base::_data[i].value) {
          return false;
        }
      }

      return true;
    }

    TESL_ALWAYS_INLINE constexpr bool operator!=(const Array & other) const {
      return !this->operator==(other);      
    }

    TESL_ALWAYS_INLINE constexpr Array & operator=(const ArrayView<T> & buffer) {
      clear(false);
      return this->operator+=(buffer);
    }

    TESL_ALWAYS_INLINE constexpr Array & operator=(const ArrayView<const T> & buffer) {
      clear(false);
      return this->operator+=(buffer);
    }

    constexpr Array & operator=(const Array & other) {
      clear(false);
      reserve(other._size);
      for (IntT i = 0; i < other._size; ++i) {
        base::_size = i + 1;
        new(&base::_data[i].value) T(other._data[i].value);
      }
      return *this;
    }

    TESL_ALWAYS_INLINE constexpr Array & operator=(Array && other) {
      base::swap(other);
      return *this;
    }

    TESL_ALWAYS_INLINE constexpr Array(const ArrayView<T> & buffer) { this->operator+=(buffer); }
    TESL_ALWAYS_INLINE constexpr Array(const ArrayView<const T> & buffer) { this->operator+=(buffer); }
    TESL_ALWAYS_INLINE constexpr Array(const Array & other) { this->operator=(other); }
    TESL_ALWAYS_INLINE constexpr Array(Array && other) { this->operator=(other); }
    TESL_ALWAYS_INLINE constexpr Array() {}
    TESL_ALWAYS_INLINE constexpr ~Array() { clear(); }
  };*/

  template<typename T, typename Alloc = std::allocator<T> >
  struct Array : public std::vector<T, Alloc> {
    using base = std::vector<T, Alloc>;
    using base::base;

    template<typename VT>
    constexpr IntT find(VT && v, IntT from = 0) const {
      IntT s = static_cast<IntT>(base::size());
      for (IntT i = 0; i < s; ++i) {
        if (FWD(v) == this->operator[](i)) {
          return i;
        }
      }

      return -1;
    }

    template<typename VT>
    TESL_ALWAYS_INLINE constexpr bool has(VT && v) const {
      return find(FWD(v)) >= 0;
    }

    constexpr Array & operator+=(const ArrayView<T> & buffer) {
      IntT s = static_cast<IntT>(base::size());
      IntT buffer_s = buffer.size();
      base::reserve(s + buffer_s);
      for (IntT i = s, j = 0; j < buffer_s; ++i, ++j) {
        base::push_back(buffer[j]);
      }
      return *this;
    }

    constexpr Array & operator+=(const ArrayView<const T> & buffer) {
      IntT s = static_cast<IntT>(base::size());
      IntT buffer_s = buffer.size();
      base::reserve(s + buffer_s);
      for (IntT i = s, j = 0; j < buffer_s; ++i, ++j) {
        base::push_back(buffer[j]);
      }
      return *this;
    }

    TESL_ALWAYS_INLINE constexpr Array & operator=(const ArrayView<T> & buffer) {
      base::clear();
      return this->operator+=(buffer);
    }

    TESL_ALWAYS_INLINE constexpr Array & operator=(const ArrayView<const T> & buffer) {
      base::clear();
      return this->operator+=(buffer);
    }

    TESL_ALWAYS_INLINE constexpr Array(const ArrayView<T> & buffer) { this->operator+=(buffer); }
    TESL_ALWAYS_INLINE constexpr Array(const ArrayView<const T> & buffer) { this->operator+=(buffer);}
  };
} // namespace tesl

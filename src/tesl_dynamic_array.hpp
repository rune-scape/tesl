#pragma once

#include "tesl_common.hpp"
#include "tesl_type.hpp"
#include "tesl_var.hpp"

namespace tesl {
  struct DynamicArray {};
  /*struct DynamicArray {
    TypeRef _type = get_builtin_type_info_of<Null>();
    void * _data = nullptr;
    IntT _size = 0;
    IntT _capacity = 0;

    bool _is_heap_allocated() const {
      return _data != nullptr;
    }

    void reset_data() {
      _capacity = 0;
      _data = nullptr;
    }

    void swap(DynamicArray & other) {
      memswap(*this, other);
    }

    struct iterator {
      TypeRef _type = get_builtin_type_info_of<Null>();
      void * _data = nullptr;
      iterator & operator++() {
        _data += _type->size;
        return *this;
      }
      iterator operator++(int) {
        void * prev_data = _data;
        _data += _type->size;
        return {_type, prev_data};
      }
      iterator & operator--() {
        _data -= _type->size;
        return *this;
      }
      iterator operator--(int) {
        void * prev_data = _data;
        _data -= _type->size;
        return {_type, prev_data};
      }
      bool operator==(const iterator & other) const {
        return _data == other._data;
      }
      void * operator*() {
        return _data;
      }
    };
    
    struct const_iterator {
      TypeRef _type = get_builtin_type_info_of<Null>();
      const void * _data = nullptr;
      const_iterator & operator++() {
        _data += _type->size;
        return *this;
      }
      const_iterator operator++(int) {
        const void * prev_data = _data;
        _data += _type->size;
        return {_type, prev_data};
      }
      const_iterator & operator--() {
        _data -= _type->size;
        return *this;
      }
      const_iterator operator--(int) {
        const void * prev_data = _data;
        _data -= _type->size;
        return {_type, prev_data};
      }
      bool operator==(const const_iterator & other) const {
        return _data == other._data;
      }
      const void * operator*() {
        return _data;
      }
    };
    
    TESL_ALWAYS_INLINE void * data() { return _data; }
    TESL_ALWAYS_INLINE const void * data() const { return _data; }
    TESL_ALWAYS_INLINE IntT size() const { return _size; }
    TESL_ALWAYS_INLINE bool is_empty() const { return _size <= 0; }

    TESL_ALWAYS_INLINE iterator begin() { return {_type, _data}; }
    TESL_ALWAYS_INLINE const_iterator begin() const { return {_type, _data}; }
    TESL_ALWAYS_INLINE iterator end() { return {_type, _data + _size * _type->size}; }
    TESL_ALWAYS_INLINE const_iterator end() const { return {_type, _data + _size * _type->size}; }

    TESL_ALWAYS_INLINE void * operator[](IntT i) {
      TESL_ASSERT(_data != nullptr);
      TESL_ASSERT(i >= 0 && i < _size);
      return _data + i * _type->size;
    }

    TESL_ALWAYS_INLINE const void * operator[](IntT i) const {
      TESL_ASSERT(_data != nullptr);
      TESL_ASSERT(i >= 0 && i < _size);
      return _data + i * _type->size;
    }

    TESL_ALWAYS_INLINE void * front() { return _data; }
    TESL_ALWAYS_INLINE const void * front() const { return _data; }
    TESL_ALWAYS_INLINE void * back() { return _data + (_size - 1) * _type->size; }
    TESL_ALWAYS_INLINE const void * back() const { return _data + (_size - 1) * _type->size; }

    void reserve(IntT requested_capacity, bool big_grow = false);

    void resize(IntT requested_size) {
      reserve(requested_size);
      for (IntT i = _size; i < requested_size; ++i) {
        _size = i + 1;
        _type->init(nullptr, _data + i * _type->size);
      }
    }

    //IntT find(const VarRef & var, IntT from = 0) const {
    //  for (IntT i = from; i < _size; ++i) {
    //    // todo: equality comparison 
    //    if (var.data == operator[](i)) {
    //      return i;
    //    }
    //  }
    //
    //  return -1;
    //}

    //template<typename VT>
    //TESL_ALWAYS_INLINE constexpr bool has(VT && v) const {
    //  return find(FWD(v)) >= 0;
    //}

    constexpr void * insert(IntT at, const VarRef & v) {
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

}
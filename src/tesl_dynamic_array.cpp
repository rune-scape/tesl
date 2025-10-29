#include "tesl_dynamic_array.hpp"

namespace tesl {
  /*void DynamicArray::reserve(IntT requested_capacity, bool big_grow = false) {
    if (requested_capacity <= _capacity) {
      return;
    }

    if (big_grow) {
      IntT min_capacity = _capacity + (_capacity >> 1) + 1;
      requested_capacity = max(min_capacity, requested_capacity);
    }

    void * m_old_data = _data;
    bool was_heap_allocated = _is_heap_allocated();
    _data = operator new(requested_capacity * _type->size, std::align_val_t{_type->align});
    for (IntT i = 0; i < _size; ++i) {
      _type->move(m_old_data + i * _type->size, _data + i * _type->size);
    }
    if (was_heap_allocated) {
      delete m_old_data;
    }

    _capacity = requested_capacity;
  }*/

}
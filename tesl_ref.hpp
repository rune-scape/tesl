#pragma once

#include "tesl_common.hpp"
#include <intrusive_shared_ptr/refcnt_ptr.h>
#include <intrusive_shared_ptr/ref_counted.h>

namespace tesl {
  template<typename T>
  using Ref = isptr::refcnt_ptr<T>;

  template<typename T>
  using RefCounted = isptr::ref_counted<T, isptr::ref_counted_flags::single_threaded>;

  template<typename T, typename ... Args>
  inline Ref<T> new_ref(Args && ... args) {
    return Ref<T>::noref(new T(FWD(args)...));
  }
}
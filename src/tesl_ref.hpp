#pragma once

#include "tesl_common.hpp"
#include <intrusive_shared_ptr/refcnt_ptr.h>
#include <intrusive_shared_ptr/ref_counted.h>

namespace tesl {
  template<typename T>
  struct Ref : public isptr::refcnt_ptr<T> {
    using base = isptr::refcnt_ptr<T>;
    using base::base;
    Ref(base && other) : base(other) {}
    Ref(const base & other) : base(other) {}
  };

  template<typename T>
  struct RefCounted : public isptr::ref_counted<T, isptr::ref_counted_flags::single_threaded> {
    using base = isptr::ref_counted<T, isptr::ref_counted_flags::single_threaded>;
    using base::base;
  };

  template<typename T, typename ... Args>
  inline Ref<T> new_ref(Args && ... args) {
    return Ref<T>::noref(new T(FWD(args)...));
  }

  template<typename T, typename U>
  inline Ref<T> get_ref(U & v) {
    return Ref<T>::ref(v);
  }
}
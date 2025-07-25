#pragma once

#include "tesl_common.hpp"
#include "tesl_type.hpp"
#include "tesl_array.hpp"

namespace tesl {
  struct FnObj : public FnObjBase {
    const TypeInfo * return_type = type_info_of<Null>;
    Array<const TypeInfo *, 1> param_types;
  };


  template<> inline const TypeInfo * type_info_of<FnObj> = &typeInfoFn;

  namespace detail {
    template<IntT ... I>
    using index_sequence = std::integer_sequence<IntT, I...>;
    template<IntT Size>
    using index_sequence_helper = std::make_integer_sequence<IntT, Size>;

    //template<typename T, typename ... Next> using first_type = T;

    template<IntT I, bool Last, typename ... Next>
    struct offset_of0;
    template<IntT I, typename T, typename ... Next>
    struct offset_of0<I, false, T, Next...> {
      static constexpr IntT call(IntT in) {
        return offset_of0<I - 1, I == 1, Next...>::call(in + sizeof(T));
      }
    };
    template<typename ... Next>
    struct offset_of0<0, true, Next...> {
      static constexpr IntT call(IntT in) { return in; }
    };
    template<IntT I, typename ... Next> constexpr IntT offset_of = offset_of0<I, I == 0, Next...>::call(0);

    template<typename ISequence, auto Fn, typename R, typename ... Ps>
    struct function0;

    template<std::size_t ... Is, auto Fn, typename R, typename ... Ps>
    struct function0<index_sequence<Is...>, Fn, R, Ps...> {
      static void call(void *, void * args, void * ret) {
        *reinterpret_cast<R *>(ret) = Fn(*reinterpret_cast<Ps *>(reinterpret_cast<char *>(args) + offset_of < Is, Ps... >)...);
      }
    };

    template<auto Fn, typename R, typename ... Ps>
    struct function : public function0<index_sequence_helper<sizeof...(Ps)>, Fn, R, Ps...> {};

    constexpr FnObj make_function_raw(FnPtr fn, void * context, bool pure, TypeIndex return_type, ArrayView<TypeIndex> param_types) {
      FnObj ret;
      ret.ptr = fn;
      ret.context = context;
      ret.return_type = return_type;
      ret.param_types = param_types;
      return ret;
    };

    template<typename T, IntT N>
    constexpr FnObj make_function_raw(FnPtr fn, void * context, bool pure, TypeIndex return_type, const T (&param_types)[N]) {
      return make_function_raw(fn, context, pure, return_type, {param_types, N});
    }

    template<auto Fn, bool Pure, typename R, typename ... Ps>
    struct make_function_impl {
      static constexpr FnObj call() {
        const TypeInfo * param_types[] = {type_info_of<Ps>...};
        return make_function_raw(function<Fn, R, Ps...>::call, nullptr, Pure, type_info_of<R>, {param_types, sizeof...(Ps)});
      }
    };

    template<auto Fn, bool Pure>
    struct make_function {};

    template<bool Pure, typename R, typename ... Ps, R(*Fn)(Ps...)>
    struct make_function<Fn, Pure> {
      static constexpr FnObj call() {
        return make_function_impl<Fn, Pure, R, Ps...>::call();
      }
    };
  }

  template<auto Fn>
  constexpr FnObj make_function() {
    return detail::make_function<Fn, false>::call(Fn);
  }

  template<auto Fn>
  constexpr FnObj make_pure_function() {
    return detail::make_function<Fn, true>::call(Fn);
  }
}

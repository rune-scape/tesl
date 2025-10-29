#pragma once

#include "tesl_common.hpp"
#include "tesl_signature.hpp"
#include "tesl_type.hpp"

namespace tesl {
  struct CallError {
    enum Why {
      UNSPECIFIED,
      COULD_NOT_FIND,
    } why;

    static CallError could_not_find(Signature sig, Type t) { return CallError{COULD_NOT_FIND, MOV(sig), MOV(t)}; }
    
    void format(FmtParseContext & parse_ctx, FmtFormatContext & ctx) const;

    Signature signature;
    Type type;
  };

  /*struct ClosureBase {
    FnPtr _impl;
    SignatureRef signature;
    void operator()(FnContext * context, void * args, void * ret) const {
      // todo: finish
    }
  };

  struct Closure : ClosureBase, RefCounted<Closure> {};

  struct FnBare {
    FnPtrBare _impl;
    SignatureRef signature;
    void operator()(FnContext * context, void * args, void * ret) const {
      _impl(context, args, ret);
    }
  };

  struct FnNative {
    FnPtr _impl;
    SignatureRef signature;
    void operator()(FnContext * context, void * args, void * ret) const {
      _impl(context, args, ret);
    }
  };*/
//lalalala i am the secret code demon ^w^ i am hiding in between your lines of code
  /*namespace detail {
    template<IntT ... I>
    using index_sequence = std::integer_sequence<IntT, I...>;
    template<IntT Size>
    using index_sequence_helper = std::make_integer_sequence<IntT, Size>;

    //template<typename T, typename ... Next> using first_type = T;

    template<IntT I, typename ... Next>
    struct offset_of0;
    template<IntT I, typename T, typename ... Next>
    struct offset_of0<I, T, Next...> {
      static constexpr IntT call(IntT in) {
        return offset_of0<I - 1, Next...>::call(in + sizeof(T));
      }
    };
    template<typename ... Next>
    struct offset_of0<0, Next...> {
      static constexpr IntT call(IntT in) { return in; }
    };
    template<IntT I, typename ... Next> constexpr IntT offset_of = offset_of0<I, Next...>::call(0);

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
#if __cpp_constexpr >= 202110L
    constexpr
#endif
    inline FnObj make_function_raw(FnPtrBare fn, void * context, bool pure, TypeRef return_type, ArrayView<TypeRef> param_types) {
      FnObj ret;
      ret.ptr = fn;
      ret.context = context;
      ret.return_types.resize(1);
      ret.return_types[0] = return_type;
      ret.param_types = param_types;
      return ret;
    };

    template<typename T, IntT N>
#if __cpp_constexpr >= 202110L
    constexpr
#endif
    inline FnObj make_function_raw(FnPtrBare fn, void * context, bool pure, TypeRef return_type, const T (&param_types)[N]) {
      return make_function_raw(fn, context, pure, return_type, {param_types, N});
    }

    template<auto Fn, bool Pure, typename R, typename ... Ps>
    struct make_function_impl {
#if __cpp_constexpr >= 202110L
      constexpr
#endif
      inline static FnObj call() {
        TypeRef param_types[] = {get_builtin_type_info_of<Ps>()...};
        return make_function_raw(function<Fn, R, Ps...>::call, nullptr, Pure, get_builtin_type_info_of<R>(), {param_types, sizeof...(Ps)});
      }
    };

    template<auto Fn, bool Pure>
    struct make_function {};

    template<bool Pure, typename R, typename ... Ps, R(*Fn)(Ps...)>
    struct make_function<Fn, Pure> {
#if __cpp_constexpr >= 202110L
      constexpr
#endif
      inline static FnObj call() {
        return make_function_impl<Fn, Pure, R, Ps...>::call();
      }
    };
  }

  template<auto Fn>
#if __cpp_constexpr >= 202110L
  constexpr
#endif
  inline FnObj make_function() {
    return detail::make_function<Fn, false>::call(Fn);
  }

  template<auto Fn>
#if __cpp_constexpr >= 202110L
  constexpr
#endif
  inline FnObj make_pure_function() {
    return detail::make_function<Fn, true>::call(Fn);
  }*/
}

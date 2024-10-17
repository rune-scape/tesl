/*
 * TINYEXPR - Tiny recursive descent GLSL parser and evaluation engine in C
 *
 * Copyright (c) 2015-2020 Lewis Van Winkle
 *
 * http://CodePlea.com
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 * claim that you wrote the original software. If you use this software
 * in a product, an acknowledgement in the product documentation would be
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 */

#ifndef TESL_HPP
#define TESL_HPP

#include <cmath>
#include <climits>
#include <cstring>
#include <cstdint>
#include <cstdio>

#include <utility>

#include "printf.h"

#define te_printf printf

//#define TE_DEBUG_PEDANTIC
#define TE_DEBUG_COMPILE
#define TE_DEBUG_EVAL
#define TE_DEBUG_EXPR
#define TE_MAX_VAR_COUNT 128
#define TE_MAX_STMT_COUNT 256

#define TE_TYPE_MASK_BIT_COUNT 7

#define TE_PARAM_COUNT_MAX (22)

#define TE_TYPE_COUNT_MAX (1ul << TE_TYPE_MASK_BIT_COUNT)

enum te_type : uint8_t {
  TE_ERROR = 0,
  TE_FLOAT_REF,
  TE_VEC2_REF,
  TE_VEC3_REF,
  TE_VEC4_REF,
  TE_INT_REF,
  TE_MAT2_REF,
  TE_MAT3_REF,
  TE_MAT4_REF,
  TE_STR_REF,
  TE_FUNCTION,
  TE_TYPE_COUNT,

  TE_CONSTANT = 1ul << 7,
  TE_FLAG_PURE = TE_CONSTANT,
  TE_PURE_FUNCTION = TE_FUNCTION | TE_FLAG_PURE,

  TE_NULL = TE_CONSTANT,
  TE_FLOAT = TE_FLOAT_REF | TE_CONSTANT,
  TE_VEC2 = TE_VEC2_REF | TE_CONSTANT,
  TE_VEC3 = TE_VEC3_REF | TE_CONSTANT,
  TE_VEC4 = TE_VEC4_REF | TE_CONSTANT,
  TE_INT = TE_INT_REF | TE_CONSTANT,
  TE_MAT2 = TE_MAT2_REF | TE_CONSTANT,
  TE_MAT3 = TE_MAT3_REF | TE_CONSTANT,
  TE_MAT4 = TE_MAT4_REF | TE_CONSTANT,
  TE_STR = TE_STR_REF | TE_CONSTANT,
};

enum te_opcode : uint8_t {
  TE_OP_JMP,
  TE_OP_JMP_REF,
  TE_OP_JMP_IF,
  TE_OP_JMP_IF_NOT,
  TE_OP_RETURN,
  TE_OP_VALUE,
  TE_OP_DEREF,
  TE_OP_FUNCTION,
  TE_OP_SUITE,
  TE_OP_STACK_REF,
  TE_OP_ASSIGN,
};

static_assert(sizeof(float) == 4);
static_assert(TE_TYPE_COUNT <= TE_TYPE_COUNT_MAX);

#define TE_IS_FUNCTION(TYPE) (((TYPE) & ~TE_FLAG_PURE) == TE_FUNCTION)
#define TE_IS_CONSTANT(TYPE) (((TYPE) & (TE_CONSTANT)) && !TE_IS_FUNCTION(TYPE))
#define TE_IS_REF(TYPE) (!((TYPE) & (TE_CONSTANT)) && !TE_IS_FUNCTION(TYPE))

struct te_vec2 {
  union {
    float elements[2]{0.0f};
    float arr[2];
    struct { float x, y; };
    struct { float r, g; };
  };
};

struct te_vec3 {
  union {
    float elements[3]{0.0f};
    float arr[3];
    struct { float x, y, z; };
    struct { float r, g, b; };
  };
};

struct te_vec4 {
  union {
    float elements[4] = {0.0f};
    float arr[4];
    struct { float x, y, z, w; };
    struct { float r, g, b, a; };
  };
};

union te_mat2 {
  float elements[2 * 2];
  te_vec2 arr[2]{};
};

union te_mat3 {
  float elements[3 * 3];
  te_vec3 arr[3]{};
};

union te_mat4 {
  float elements[4 * 4];
  te_vec4 arr[4]{};
};

struct te_string {
  const char * ptr = "<uninitialized-str>";
  int32_t length = strlen(ptr);

  te_string(const char * str, const int32_t p_length) {
    ptr = str;
    length = p_length;
  }

  explicit te_string(const char * str) {
    ptr = str;
    length = strlen(str);
  }
};

static_assert(sizeof(void *) == 4, "made for 32bit :/ srry didnt have the energy to um yea");

inline void te_noop(void * context, void * args, void * ret) {}

typedef void (* te_function)(void * context, void * args, void * ret);

struct te_expr;

struct te_fn_obj {
  te_function ptr = nullptr;
  void * context = nullptr;
  te_type return_type = TE_ERROR;
  int8_t param_count = 0;
  te_type param_types[TE_PARAM_COUNT_MAX]{TE_ERROR};

  [[nodiscard]] bool is_valid() const {
    return ptr != nullptr;
  }
};
static_assert(sizeof(te_fn_obj) == 32);

struct te_fn_obj_with_args : te_fn_obj {
  te_expr * args[1];

  te_fn_obj_with_args & operator=(const te_fn_obj &other) {
    ptr = other.ptr;
    context = other.context;
    return_type = other.return_type;
    param_count = other.param_count;
    for (int i = 0; i < param_count; ++i) {
      param_types[i] = other.param_types[i];
    }
    return *this;
  }
};

struct te_value {
  union {
    te_fn_obj fn;

    struct { float x, y, z, w; };
    struct { float r, g, b, a; };
    float float_;
    te_vec2 vec2;
    te_vec3 vec3;
    te_vec4 vec4;
    int32_t int_;
    te_mat2 mat2;
    te_mat3 mat3;
    te_mat4 mat4;
    te_string str;
    float elements[16];

    void * ptr;
    te_value * ref;
    float * float_ref;
    te_vec2 * vec2_ref;
    te_vec3 * vec3_ref;
    te_vec4 * vec4_ref;
    int32_t * int_ref;
    te_mat2 * mat2_ref;
    te_mat3 * mat3_ref;
    te_mat4 * mat4_ref;
    te_string * str_ref;
  };

  te_value() {
    static_assert(sizeof(te_value) == 64);
    memset(this, 0, 64);
  };

  te_value(te_fn_obj p_fn) : fn(p_fn) {};
  te_value(float p_float) : float_(p_float) {};
  te_value(int32_t p_int) : int_(p_int) {};
  te_value(te_vec2 p_vec) : vec2(p_vec) {};
  te_value(te_vec3 p_vec) : vec3(p_vec) {};
  te_value(te_vec4 p_vec) : vec4(p_vec) {};
  te_value(te_mat2 p_vec) : mat2(p_vec) {};
  te_value(te_mat3 p_vec) : mat3(p_vec) {};
  te_value(te_mat4 p_vec) : mat4(p_vec) {};

  te_value(void * p_ptr) : ptr(p_ptr) {};
  te_value(te_value * p_ref) : ref(p_ref) {};
};

struct te_variable {
  const char * name = "<unnamed>";
  te_type type = TE_ERROR;
  union {
    te_value value;
    te_fn_obj fn;
  };
};

// DO NOT STATICALLY ALLOCATE EXPR OBJS
struct te_expr {
  te_opcode opcode = TE_OP_VALUE;
  te_type type = TE_ERROR;
  union {
    uint16_t offset = 0;
    uint16_t size;
  };
  union {
    te_value value;
    te_fn_obj_with_args fn;
    te_expr * opargs[1];
  };
};
static_assert((sizeof(te_opcode) + sizeof(te_type) + sizeof(uint16_t)) == 4);
static_assert(sizeof(te_expr) == (1 + 1 + 2 + 64));

te_type te_expr_result_type(const te_expr * expr);

/* Parses the input expression, evaluates it, and frees it. */
/* result_type is TE_ERROR on error. */
te_value te_interp(const char * expression, te_type * result_type, int * error = nullptr);

/* Parses the input expression and binds variables. */
/* Returns nullptr on error. */
te_expr * te_compile(const char * expression, const te_variable * variables, int var_count, int * error = nullptr, bool optimize = true);

/* Evaluates the expression. */
te_value te_eval(const te_expr * n);

/* Prints debugging information on the syntax tree. */
void te_print_expr(const te_expr * n);

void te_print_value(te_type t, const te_value & v);
void te_print_type_name(te_type type);

/* Frees the expression. */
/* This is safe to call on NULL pointers. */
void te_free(te_expr * n);

namespace te {
  template<typename T1, typename T2> constexpr bool is_same = false;
  template<typename T> constexpr bool is_same<T, T> = true;

  template<typename T> inline constexpr te_type type_value_of = TE_ERROR;
  template<typename T> inline constexpr te_type type_value_of<T *> = te_type(type_value_of<T> & (~TE_CONSTANT));
  template<> inline constexpr te_type type_value_of<void> = TE_NULL;
  template<> inline constexpr te_type type_value_of<float> = TE_FLOAT;
  template<> inline constexpr te_type type_value_of<te_vec2> = TE_VEC2;
  template<> inline constexpr te_type type_value_of<te_vec3> = TE_VEC3;
  template<> inline constexpr te_type type_value_of<te_vec4> = TE_VEC4;
  template<> inline constexpr te_type type_value_of<int32_t> = TE_INT;
  template<> inline constexpr te_type type_value_of<te_mat2> = TE_MAT2;
  template<> inline constexpr te_type type_value_of<te_mat3> = TE_MAT3;
  template<> inline constexpr te_type type_value_of<te_mat4> = TE_MAT4;
  template<> inline constexpr te_type type_value_of<te_string> = TE_STR;

  template<te_type T>
  struct type_of_impl { using type = typename type_of_impl<te_type(T | TE_CONSTANT)>::type *; };
  template<>
  struct type_of_impl<TE_NULL> { using type = void; };
  template<>
  struct type_of_impl<TE_FLOAT> { using type = float; };
  template<>
  struct type_of_impl<TE_VEC2> { using type = te_vec2; };
  template<>
  struct type_of_impl<TE_VEC3> { using type = te_vec3; };
  template<>
  struct type_of_impl<TE_VEC4> { using type = te_vec4; };
  template<>
  struct type_of_impl<TE_INT> { using type = int32_t; };
  template<>
  struct type_of_impl<TE_MAT2> { using type = te_mat2; };
  template<>
  struct type_of_impl<TE_MAT3> { using type = te_mat3; };
  template<>
  struct type_of_impl<TE_MAT4> { using type = te_mat4; };
  template<>
  struct type_of_impl<TE_STR> { using type = te_string; };

  template<>
  struct type_of_impl<TE_FUNCTION> { using type = te_fn_obj; };
  template<>
  struct type_of_impl<TE_PURE_FUNCTION> { using type = te_fn_obj; };

  template<te_type T> using type_of = typename type_of_impl<T>::type;

  template<te_type Type>
  constexpr bool is_ref = Type & TE_CONSTANT;

  template<te_type Type>
  constexpr bool is_mat = Type == TE_MAT2 || Type == TE_MAT3 || Type == TE_MAT4;

  template<te_type Type>
  constexpr bool is_vec = Type == TE_VEC2 || Type == TE_VEC3 || Type == TE_VEC4;

  template<typename T>
  T value_get(const te_value & val);
  template<>
  inline void value_get<void>(const te_value & val) {}

  template<te_type Type>
  inline type_of<te_type(Type | TE_CONSTANT)> value_deref(const te_value & val);

  template<typename T>
  inline void value_set(te_value & tev, T v);

#define MAKE_VALUE_IMPL(tetype, member_name, ref_tetype, ref_member_name)\
    template<> inline type_of<tetype> value_get<type_of<tetype>>(const te_value &val) { return val.member_name; }\
    template<> inline type_of<tetype> value_deref<tetype>(const te_value &val) { return val.member_name; }\
    template<> inline void value_set<type_of<tetype>>(te_value &tev, type_of<tetype> v) {\
        tev.member_name = v;\
    }\
    template<> inline type_of<ref_tetype> value_get<type_of<ref_tetype>>(const te_value &val) { return val.ref_member_name; }\
    template<> inline type_of<tetype> value_deref<ref_tetype>(const te_value &val) { return *val.ref_member_name; }\
    template<> inline void value_set<type_of<ref_tetype>>(te_value &tev, type_of<ref_tetype> v) {\
        tev.ref_member_name = v;\
    }

  MAKE_VALUE_IMPL(TE_INT, int_, TE_INT_REF, int_ref)
  MAKE_VALUE_IMPL(TE_FLOAT, float_, TE_FLOAT_REF, float_ref)
  MAKE_VALUE_IMPL(TE_VEC2, vec2, TE_VEC2_REF, vec2_ref)
  MAKE_VALUE_IMPL(TE_VEC3, vec3, TE_VEC3_REF, vec3_ref)
  MAKE_VALUE_IMPL(TE_VEC4, vec4, TE_VEC4_REF, vec4_ref)
  MAKE_VALUE_IMPL(TE_MAT2, mat2, TE_MAT2_REF, mat2_ref)
  MAKE_VALUE_IMPL(TE_MAT3, mat3, TE_MAT3_REF, mat3_ref)
  MAKE_VALUE_IMPL(TE_MAT4, mat4, TE_MAT4_REF, mat4_ref)
  MAKE_VALUE_IMPL(TE_STR, str, TE_STR_REF, str_ref)

#undef MAKE_VALUE_IMPL

  template<typename T>
  te_value make_value(T v) {
    te_value ret;
    value_set(ret, v);
    return ret;
  }

  namespace detail {
    template<auto I, typename T> using idk = T;

    //template<int ... I>
    //struct index_sequence {};
    //template<int Size, int ... Next>
    //struct index_sequence_helper0 { using type = typename index_sequence_helper0<Size - 1, Size - 1, Next...>::type; };
    //template<int ... Next>
    //struct index_sequence_helper0<0, Next...> { using type = index_sequence<Next...>; };
    //template<int Size>
    //using index_sequence_helper = typename index_sequence_helper0<Size>::type;

    template<int ... I>
    using index_sequence = std::index_sequence<I...>;
    template<int Size>
    using index_sequence_helper = std::make_index_sequence<Size>;

    //template <typename T, typename ... Next> using first_type = T;

    template<int I, bool Last, typename ... Next>
    struct offset_of0;
    template<int I, typename T, typename ... Next>
    struct offset_of0<I, false, T, Next...> {
      inline static constexpr int call(int in) {
        return offset_of0<I - 1, I == 1, Next...>::call(in + sizeof(T));
      }
    };
    template<typename ... Next>
    struct offset_of0<0, true, Next...> {
      inline static constexpr int call(int in) { return in; }
    };
    template<int I, typename ... Next> constexpr int offset_of = offset_of0<I, I == 0, Next...>::call(0);

    template<typename ISequence, auto Fn, typename R, typename ... Ps>
    struct function0;

    template<auto ... Is, auto Fn, typename R, typename ... Ps>
    struct function0<index_sequence<Is...>, Fn, R, Ps...> {
      inline static void call(void *, void * args, void * ret) {
        *reinterpret_cast<R *>(ret) = Fn(*reinterpret_cast<Ps *>(reinterpret_cast<char *>(args) + offset_of < Is, Ps... >)...);
      }
    };

    template<auto Fn, typename R, typename ... Ps>
    struct function : public function0<index_sequence_helper<sizeof...(Ps)>, Fn, R, Ps...> {};

    template<te_function Fn, te_type RT, te_type ... PTs>
    struct make_function_raw {
      inline static constexpr te_fn_obj call() {
        static_assert(sizeof...(PTs) <= TE_PARAM_COUNT_MAX, "too many parameters!");
        te_fn_obj ret;
        ret.ptr = Fn;
        ret.context = nullptr;
        ret.param_count = sizeof...(PTs);
        ret.return_type = RT;

        te_type ps[] = {PTs ...};
        for (int i = 0; i < sizeof...(PTs); ++i) {
          ret.param_types[i] = ps[i];
        }
        return ret;
      }
    };

    template<auto Fn, typename R, typename ... Ps>
    struct make_function_impl : public make_function_raw<function<Fn, R, Ps...>::call, type_value_of<R>, type_value_of<Ps>...> {};

    template<auto Fn>
    struct make_function;

    template<typename R, typename ... Ps, R (* Fn)(Ps...)>
    struct make_function<Fn> {
      inline static constexpr te_fn_obj call() {
        return make_function_impl<Fn, R, Ps...>::call();
      }
    };
  }

  template<auto Fn>
  inline constexpr te_fn_obj make_function() {
    return detail::make_function<Fn>::call();
  }
}

inline constexpr int te_size_of(te_type type) {
#define CASE_(tetype) case tetype: return sizeof(te::type_of<tetype>);
  switch (type) {
    CASE_(TE_INT)
    CASE_(TE_FLOAT)
    CASE_(TE_VEC2)
    CASE_(TE_VEC3)
    CASE_(TE_VEC4)
    CASE_(TE_MAT2)
    CASE_(TE_MAT3)
    CASE_(TE_MAT4)
    CASE_(TE_STR)
    CASE_(TE_INT_REF)
    CASE_(TE_FLOAT_REF)
    CASE_(TE_VEC2_REF)
    CASE_(TE_VEC3_REF)
    CASE_(TE_VEC4_REF)
    CASE_(TE_MAT2_REF)
    CASE_(TE_MAT3_REF)
    CASE_(TE_MAT4_REF)
    CASE_(TE_STR_REF)
    CASE_(TE_FUNCTION)
    CASE_(TE_PURE_FUNCTION)
    case TE_NULL:
    case TE_ERROR:
      return 0;
    default:
      return sizeof(te_value);
  }
#undef CASE_
}

#endif /*TESL_HPP*/

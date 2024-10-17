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

#include <type_traits>
#include <utility>

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

inline te_type te_expr_result_type(const te_expr * expr) {
  if (!expr) {
    return TE_ERROR;
  } else if (expr->opcode == TE_OP_STACK_REF) {
    // always yields a ref
    return te_type(expr->type & ~TE_CONSTANT);
  } else if (expr->opcode == TE_OP_FUNCTION) {
    return expr->fn.return_type;
  } else {
    return expr->type;
  }
}

/* Parses the input expression, evaluates it, and frees it. */
/* result_type is TE_ERROR on error. */
te_value te_interp(const char * expression, te_type * result_type, int * error = nullptr);

/* Parses the input expression and binds variables. */
/* Returns nullptr on error. */
te_expr * te_compile(const char * expression, const te_variable * variables, int var_count, int * error = nullptr);

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

#endif /*TESL_HPP*/


/*
 * TESL - Tiny recursive descent GLSL parser and evaluation engine in C
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

/* COMPILE TIME OPTIONS */

/* Logarithms
For log = base 10 log do nothing
For log = natural log uncomment the next line. */
/* #define TE_NAT_LOG */

//#include <Arduino.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>

#ifdef __linux__
#include <alloca.h>
#elif _WIN32
#include <malloc.h>
#endif

#include "printf.h"

#if false
extern "C" {
  void _putchar(char character) {
    Serial.write(character);
  }
}
#else
extern "C" {
  void _putchar(char character) {
    fputc(character, stdout);
  }
}
#endif

#define te_printf printf


#ifndef PI
#define PI 3.14159265358979323846f
#endif

constexpr uint16_t TE_UNKNOWN_OFFSET = 0xffffu;

#ifdef TE_DEBUG_COMPILE
#define TE_FAIL_COMPILE_COND(cond, action, ...)\
    if (cond) {\
        te_printf(__VA_ARGS__);\
        te_printf("\n");\
        action;\
    } else ((void)0)
#else
#define TE_FAIL_COMPILE_COND(cond, action, ...) if (true) {} else ((void)0)
#endif

#define TE_ASSERT(cond) if (!(cond)) { std::exit(-1); } else ((void)0)

enum te_token : char {
  TOK_NULL,
  TOK_END,
  TOK_SEP,
  TOK_DOT,
  TOK_SEMICOLON,
  TOK_TYPENAME,
  TOK_IDENTIFIER,
  TOK_OPEN_PAREN,
  TOK_CLOSE_PAREN,
  TOK_OPEN_SQUARE_BRACKET,
  TOK_CLOSE_SQUARE_BRACKET,
  TOK_OPEN_CURLY_BRACKET,
  TOK_CLOSE_CURLY_BRACKET,
  TOK_LITERAL,
  TOK_UNIFORM,
  TOK_IN,
  TOK_OUT,
  TOK_INOUT,
  TOK_IF,
  TOK_ELSE,
  TOK_SWITCH,
  TOK_CASE,
  TOK_FOR,
  TOK_WHILE,
  TOK_DO,
  TOK_BREAK,
  TOK_CONTINUE,
  TOK_RETURN,
  TOK_DISCARD,
  TOK_AND,
  TOK_AND_AND,
  TOK_OR,
  TOK_OR_OR,
  TOK_EQUAL,
  TOK_EQUAL_EQUAL,
  TOK_BANG,
  TOK_BANG_EQUAL,
  TOK_LESS,
  TOK_LESS_EQUAL,
  TOK_GREATER,
  TOK_GREATER_EQUAL,
  TOK_ADD,
  TOK_ADD_EQUAL,
  TOK_SUB,
  TOK_SUB_EQUAL,
  TOK_MUL,
  TOK_MUL_EQUAL,
  TOK_DIV,
  TOK_DIV_EQUAL,
  TOK_MOD,
  TOK_MOD_EQUAL,
  TOK_INC,
  TOK_DEC,
};

extern "C" {
  int _handle_extra_vsnprintf_spec(char spec, out_fct_type out, void* buffer, size_t idx, size_t maxlen, va_list *va) {
    static const char * hex_chars = "0123456789abcdef";

#define OUT_STR_(str) for (int i = 0; i < (sizeof(str) - 1); ++i) { out(str[i], buffer, idx++, maxlen); }
    switch (spec) {
      case 'y': {
        te_type type = (te_type)va_arg(*va, int);
        if (TE_IS_FUNCTION(type)) {
          if (type & TE_FLAG_PURE) {
            OUT_STR_("<function:pure>");
          } else {
            OUT_STR_("<function>");
          }
        } else {
          switch (type) {
            case TE_ERROR:
              OUT_STR_("<error>");
              break;
            case TE_INT_REF:
              OUT_STR_("int&");
              break;
            case TE_FLOAT_REF:
              OUT_STR_("float&");
              break;
            case TE_VEC2_REF:
              OUT_STR_("vec2&");
              break;
            case TE_VEC3_REF:
              OUT_STR_("vec3&");
              break;
            case TE_VEC4_REF:
              OUT_STR_("vec4&");
              break;
            case TE_MAT2_REF:
              OUT_STR_("mat2&");
              break;
            case TE_MAT3_REF:
              OUT_STR_("mat3&");
              break;
            case TE_MAT4_REF:
              OUT_STR_("mat4&");
              break;
            case TE_STR_REF:
              OUT_STR_("str&");
              break;
            case TE_NULL:
              OUT_STR_("void");
              break;
            case TE_INT:
              OUT_STR_("int");
              break;
            case TE_FLOAT:
              OUT_STR_("float");
              break;
            case TE_VEC2:
              OUT_STR_("vec2");
              break;
            case TE_VEC3:
              OUT_STR_("vec3");
              break;
            case TE_VEC4:
              OUT_STR_("vec4");
              break;
            case TE_MAT2:
              OUT_STR_("mat2");
              break;
            case TE_MAT3:
              OUT_STR_("mat3");
              break;
            case TE_MAT4:
              OUT_STR_("mat4");
              break;
            case TE_STR:
              OUT_STR_("str");
              break;
            default:
              OUT_STR_("<unknown_type:0x");
              out(hex_chars[(type >> 4) & 0xf], buffer, idx++, maxlen);
              out(hex_chars[(type) & 0xf], buffer, idx++, maxlen);
              out('>', buffer, idx++, maxlen);
              break;
          }
        }
      } break;
      case 'k': {
        te_token tok = (te_token)va_arg(*va, int);
        switch (tok) {
          case TOK_NULL:
            OUT_STR_("<null>");
            break;
          case TOK_END:
            OUT_STR_("<end>");
            break;
          case TOK_SEP:
            OUT_STR_("','");
            break;
          case TOK_DOT:
            OUT_STR_("'.'");
            break;
          case TOK_SEMICOLON:
            OUT_STR_("';'");
            break;
          case TOK_TYPENAME:
            OUT_STR_("<typename>");
            break;
          case TOK_IDENTIFIER:
            OUT_STR_("<identifier>");
            break;
          case TOK_OPEN_PAREN:
            OUT_STR_("'('");
            break;
          case TOK_CLOSE_PAREN:
            OUT_STR_("')'");
            break;
          case TOK_OPEN_SQUARE_BRACKET:
            OUT_STR_("'['");
            break;
          case TOK_CLOSE_SQUARE_BRACKET:
            OUT_STR_("']'");
            break;
          case TOK_OPEN_CURLY_BRACKET:
            OUT_STR_("'{'");
            break;
          case TOK_CLOSE_CURLY_BRACKET:
            OUT_STR_("'}'");
            break;
          case TOK_LITERAL:
            OUT_STR_("<literal>");
            break;
          case TOK_UNIFORM:
            OUT_STR_("'uniform'");
            break;
          case TOK_IN:
            OUT_STR_("'in'");
            break;
          case TOK_OUT:
            OUT_STR_("'out'");
            break;
          case TOK_INOUT:
            OUT_STR_("'inout'");
            break;
          case TOK_IF:
            OUT_STR_("'if'");
            break;
          case TOK_ELSE:
            OUT_STR_("'else'");
            break;
          case TOK_SWITCH:
            OUT_STR_("'switch'");
            break;
          case TOK_CASE:
            OUT_STR_("'case'");
            break;
          case TOK_FOR:
            OUT_STR_("'for'");
            break;
          case TOK_WHILE:
            OUT_STR_("'while'");
            break;
          case TOK_DO:
            OUT_STR_("'do'");
            break;
          case TOK_BREAK:
            OUT_STR_("'break'");
            break;
          case TOK_CONTINUE:
            OUT_STR_("'continue'");
            break;
          case TOK_RETURN:
            OUT_STR_("'return'");
            break;
          case TOK_DISCARD:
            OUT_STR_("'discard'");
            break;
          case TOK_AND:
            OUT_STR_("'&'");
            break;
          case TOK_AND_AND:
            OUT_STR_("'&&'");
            break;
          case TOK_OR:
            OUT_STR_("'|'");
            break;
          case TOK_OR_OR:
            OUT_STR_("'||'");
            break;
          case TOK_EQUAL:
            OUT_STR_("'='");
            break;
          case TOK_EQUAL_EQUAL:
            OUT_STR_("'=='");
            break;
          case TOK_BANG:
            OUT_STR_("'!'");
            break;
          case TOK_BANG_EQUAL:
            OUT_STR_("'!='");
            break;
          case TOK_LESS:
            OUT_STR_("'<'");
            break;
          case TOK_LESS_EQUAL:
            OUT_STR_("'<='");
            break;
          case TOK_GREATER:
            OUT_STR_("'>'");
            break;
          case TOK_GREATER_EQUAL:
            OUT_STR_("'>='");
            break;
          case TOK_ADD:
            OUT_STR_("'+'");
            break;
          case TOK_ADD_EQUAL:
            OUT_STR_("'+='");
            break;
          case TOK_SUB:
            OUT_STR_("'-'");
            break;
          case TOK_SUB_EQUAL:
            OUT_STR_("'-='");
            break;
          case TOK_MUL:
            OUT_STR_("'*'");
            break;
          case TOK_MUL_EQUAL:
            OUT_STR_("'*='");
            break;
          case TOK_DIV:
            OUT_STR_("'/'");
            break;
          case TOK_DIV_EQUAL:
            OUT_STR_("'/='");
            break;
          case TOK_MOD:
            OUT_STR_("'%'");
            break;
          case TOK_MOD_EQUAL:
            OUT_STR_("'%='");
            break;
          default:
            OUT_STR_("<unknown_token:0x");
            out(hex_chars[(tok >> 4) & 0xf], buffer, idx++, maxlen);
            out(hex_chars[(tok) & 0xf], buffer, idx++, maxlen);
            out('>', buffer, idx++, maxlen);
            break;
        }
      } break;
    }

    out(spec, buffer, idx++, maxlen);

    return idx;
#undef OUT_STR_
  }
}

inline void te_print_type_name(te_type type) {
  te_printf("%y", type);
}

inline void te_print_token(te_token tok) {
  te_printf("%k", tok);
}

struct te_error_record;

struct te_parser_state {
  struct var_ref {
    const char * start = nullptr;
    uint8_t len = 0;
    te_type type = TE_ERROR;
    uint16_t offset = 0;
  };

  te_parser_state * parent = nullptr;
  te_error_record * error = nullptr;
  const char * program = nullptr;
  const char * line_start = nullptr;
  const char * start = nullptr;
  const char * prev_end = nullptr;
  const char * end = nullptr;
  int line_num = 0;
  te_token token = TOK_NULL;
  te_type type = TE_NULL;
  bool parse_error = false;
  te_value value;

  te_type return_type = TE_NULL;
  int stmt_count = 0;

  const te_variable * lookup = nullptr;
  int lookup_len = 0;
  int stack_size = 0;
  int stack_offset = 0;
  int var_count = 0;
  var_ref vars[TE_MAX_VAR_COUNT];
  te_expr * stmts[TE_MAX_STMT_COUNT];
};

struct te_error_record {
  te_parser_state & state;
  te_error_record * prev;
  const char * line_start = nullptr;
  const char * start = nullptr;
  const char * point = nullptr;
  const char * end = nullptr;
  int line_num = 0;

  explicit te_error_record(te_parser_state & s) : state(s) {
    prev = s.error;
    s.error = this;
    line_start = s.line_start;
    start = s.start;
    point = s.start;
    end = s.end;
    line_num = s.line_num;
  }

  ~te_error_record() {
    state.error = prev;
  }

  void set_point(const char * p_point) {
    point = p_point;
  }

  void set_end(const char * p_end) {
    end = p_end;
  }
};

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

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

inline uint16_t te_make_allocate_var(te_parser_state * s, const char * start, uint8_t len, te_type type) {
  int var_size = te_size_of(type);
  uint16_t offset = s->stack_offset;
  s->vars[s->var_count].start = start;
  s->vars[s->var_count].len = len;
  s->vars[s->var_count].type = type;
  s->vars[s->var_count].offset = offset;
  s->stack_offset += var_size;
  s->stack_size = MAX(s->stack_size, s->stack_offset);
  s->var_count++;
  return offset;
}

inline void te_make_deallocate_var(te_parser_state * s, uint16_t offset) {
  s->var_count--;
  s->stack_offset = s->vars[s->var_count].offset;

#ifdef TE_DEBUG_COMPILE
  if (offset != s->stack_offset) {
    te_printf("internal error: stack var deallocated out of order!");
  }
#endif
}

inline void te_print_error(int line_num, const char * line_start, const char * start, const char * point, const char * const end) {
  const char error_point_highlight_str[] =
      "^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>";
  const int error_point_highlight_str_size = sizeof(error_point_highlight_str) - 1;
  const char * error_highlight_str = error_point_highlight_str + 1;
  const int error_highlight_str_size = error_point_highlight_str_size - 1;
  const int error_highlight_start_str_size = error_highlight_str_size - 1;
  const char * line_end = line_start;
  do {
    while (*line_end != '\0' && *line_end != '\r' && *line_end != '\n') line_end++;
    const int line_length = line_end - line_start;
    te_printf("%4d | %.*s\n", line_num, line_length, line_start);
    if (start <= line_end && end > line_start) {
      const char * highlight_start = MAX(start, line_start);
      const char * highlight_end = MIN(line_end, end);
      te_printf("     | %*s", highlight_start - line_start, "");
      if (line_start <= point && point <= line_end) {
        int highlight_begin_length = point - highlight_start;
        const int highlight_end_length = MAX(1, highlight_end - point);
        for (; highlight_begin_length > 0; highlight_begin_length -= error_highlight_start_str_size)
          te_printf("%.*s", MIN(highlight_begin_length, error_highlight_start_str_size), error_highlight_str);
        te_printf("%.*s\n", MIN(highlight_end_length, error_point_highlight_str_size), error_point_highlight_str);
      } else {
        const int highlight_length = highlight_end - highlight_start;
        te_printf("%.*s\n", MIN(highlight_length, error_highlight_str_size), error_highlight_str);
      }
    }
    if (*line_end == '\0') {
      break;
    } else if (line_end[0] == '\r' && line_end[1] == '\n') {
      line_start = line_end + 2;
    } else {
      line_start = line_end + 1;
    }
    line_end = line_start;
    line_num++;
  } while (line_end < end);
}

inline void te_print_error(const te_parser_state * s) {
  if (s->error != nullptr) {
    te_print_error(s->error->line_num, s->error->line_start, s->error->start, s->error->point, s->error->end);
  } else {
    te_print_error(s->line_num, s->line_start, s->start, s->start, s->end);
  }
}

static_assert(sizeof(te_expr) == (sizeof(te_opcode) + sizeof(te_type) + sizeof(uint16_t) + sizeof(te_value)));
inline te_expr * new_value_expr(te_type type, te_value value) {
  const int value_size = te_size_of(type);
  const int size = 4 + value_size;
  te_expr * ret = static_cast<te_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = type;
  ret->size = value_size;
  memcpy(&ret->value, &value, value_size);
  return ret;
}

inline te_expr * new_deref_expr(te_type target_type, te_expr * e) {
  if (e == nullptr) {
    return nullptr;
  }
  te_type result_type = te_expr_result_type(e);
  if (result_type == target_type || result_type == TE_ERROR) {
    return e;
  } else if ((result_type | TE_CONSTANT) != target_type) {
#ifdef TE_DEBUG_COMPILE
    te_printf("internal error: cannot deref ");
    te_print_type_name(result_type);
    te_printf(" to ");
    te_print_type_name(target_type);
    te_printf("\n");
#endif
    e->opcode = TE_OP_VALUE;
    e->type = TE_ERROR;
    e->size = 0;
    return e;
  }

  const int value_size = te_size_of(static_cast<te_type>(result_type | TE_CONSTANT));
  const int size = 4 + value_size;
  te_expr * ret = static_cast<te_expr *>(malloc(size));
  ret->opcode = TE_OP_DEREF;
  ret->type = static_cast<te_type>(result_type | TE_CONSTANT);
  ret->size = value_size;
  ret->opargs[0] = e;
  return ret;
}

inline te_expr * new_jmp_expr(uint16_t offset) {
  const int size = 4;
  te_expr * ret = static_cast<te_expr *>(malloc(size));
  ret->opcode = TE_OP_JMP;
  ret->offset = offset;
  return ret;
}

inline te_expr * new_jmp_ref_expr(uint16_t * offset_ptr) {
  const int size = 4 + sizeof(offset_ptr);
  te_expr * ret = static_cast<te_expr *>(malloc(size));
  ret->opcode = TE_OP_JMP_REF;
  ret->value.ptr = offset_ptr;
  return ret;
}

inline te_expr * new_jmp_if_expr(uint16_t offset, te_expr * e) {
  e = new_deref_expr(TE_INT, e);
  if (e == nullptr) {
    return nullptr;
  }
  const int size = 4 + sizeof(te_expr *);
  te_expr * ret = static_cast<te_expr *>(malloc(size));
  ret->opcode = TE_OP_JMP_IF;
  ret->offset = offset;
  ret->opargs[0] = e;
  return ret;
}

inline te_expr * new_jmp_if_not_expr(uint16_t offset, te_expr * e) {
  e = new_deref_expr(TE_INT, e);
  if (e == nullptr) {
    return nullptr;
  }
  const int size = 4 + sizeof(te_expr *);
  te_expr * ret = static_cast<te_expr *>(malloc(size));
  ret->opcode = TE_OP_JMP_IF_NOT;
  ret->offset = offset;
  ret->opargs[0] = e;
  return ret;
}

inline te_expr * new_return_expr(te_type return_type, te_expr * e) {
  e = new_deref_expr(return_type, e);
  if (e == nullptr) {
    return nullptr;
  }
  const int size = 4 + sizeof(te_expr *);
  te_expr * ret = static_cast<te_expr *>(malloc(size));
  ret->opcode = TE_OP_RETURN;
  ret->type = e->type;
  ret->size = te_size_of(e->type);
  ret->opargs[0] = e;
  return ret;
}

inline te_expr * new_function_value_expr(te_type type, te_fn_obj fn, te_expr * args[], const int arg_count) {
  const int args_size = arg_count * sizeof(te_expr *);
  const int size = 4 + sizeof(te_fn_obj) + args_size;
  te_expr * ret = static_cast<te_expr *>(malloc(size));
  ret->opcode = TE_OP_FUNCTION;
  ret->type = type;
  ret->size = 0;
  for (int i = 0; i < arg_count; ++i) {
    ret->size += te_size_of(fn.param_types[i]);
  }
#ifdef TE_DEBUG_COMPILE
  if (fn.param_count != arg_count) {
    te_printf("internal error: expected %d args, got %d!\n", int(fn.param_count), int(arg_count));
    ret->opcode = TE_OP_VALUE;
    ret->type = TE_ERROR;
    ret->size = 0;
    return ret;
  }

  if (arg_count > TE_PARAM_COUNT_MAX) {
    te_printf("internal error: %d is too many parameters!\n", int(arg_count));
    ret->opcode = TE_OP_VALUE;
    ret->type = TE_ERROR;
    ret->size = 0;
    return ret;
  }

  for (int i = 0; i < arg_count; ++i) {
    if (args[i] == nullptr) {
      te_printf("internal error: arg %d is null!\n", i);
      ret->opcode = TE_OP_VALUE;
      ret->type = TE_ERROR;
      ret->size = 0;
      return ret;
    }
  }
#endif
  ret->fn = fn;

  for (int i = 0; i < arg_count; ++i) {
    ret->fn.args[i] = new_deref_expr(fn.param_types[i], args[i]);
#ifdef TE_DEBUG_COMPILE
    if (ret->fn.args[i] == nullptr) {
      te_printf("internal error: arg %d does not match parameter! expected ", i);
      te_print_type_name(fn.param_types[i]);
      te_printf(", got ");
      te_print_type_name(te_expr_result_type(args[i]));
      te_printf("\n");
      ret->opcode = TE_OP_VALUE;
      ret->type = TE_ERROR;
      ret->size = 0;
      return ret;
    }
#endif
  }

  return ret;
}

inline te_expr * new_function_expr(te_type type, te_function fnptr, void * context, te_type return_type, const te_type param_types[], te_expr * args[], const int arg_count) {
  te_fn_obj fn;
  fn.ptr = fnptr;
  fn.context = context;
  fn.param_count = arg_count;
  fn.return_type = return_type;
  for (int i = 0; i < arg_count; ++i) {
    fn.param_types[i] = param_types[i];
  }
  return new_function_value_expr(type, fn, args, arg_count);
}

inline te_expr * new_suite_expr(te_type return_type, uint16_t stack_size, te_expr * stmts[], const int stmt_count) {
  const int size = 4 + sizeof(te_expr *) * (stmt_count + 1);
  te_expr * ret = static_cast<te_expr *>(malloc(size));
  memset(ret, 0, size);
  ret->opcode = TE_OP_SUITE;
  ret->type = return_type;
  ret->size = stack_size;
  memcpy(ret->opargs, stmts, stmt_count * sizeof(te_expr *));
  return ret;
}

inline void te_free_args(te_expr * n) {
  if (!n) return;
  switch (n->opcode) {
    case TE_OP_VALUE:
      break;
    case TE_OP_DEREF:
      te_free(n->opargs[0]);
      break;
    case TE_OP_FUNCTION: {
      for (int i = n->fn.param_count - 1; i >= 0; --i) {
        te_free(n->fn.args[i]);
      }
    }
      break;
    case TE_OP_SUITE: {
      for (te_expr * const * it = n->opargs; *it; ++it) {
        te_free(*it);
      }
    }
      break;
    case TE_OP_STACK_REF:
      break;
    case TE_OP_ASSIGN: {
      te_free(n->opargs[1]);
      te_free(n->opargs[0]);
    }
      break;
    case TE_OP_JMP:
    case TE_OP_JMP_REF:
      break;
    case TE_OP_JMP_IF:
    case TE_OP_JMP_IF_NOT: {
      te_free(n->opargs[0]);
    }
      break;
    case TE_OP_RETURN: {
      te_free(n->opargs[0]);
    }
      break;
  }
}

inline void te_free(te_expr * n) {
  if (!n) return;
  te_free_args(n);
  free((void *) n);
}

namespace te {
  // template<te_type RefType>
  // type_of<te_type(RefType | TE_CONSTANT)> dereference_value(type_of<RefType> v) {
  //     return *v;
  // }

  template<te_type Type>
  constexpr int element_count = 0;

  template<> inline constexpr int element_count<TE_FLOAT> = 1;
  template<> inline constexpr int element_count<TE_VEC2> = 2;
  template<> inline constexpr int element_count<TE_VEC3> = 3;
  template<> inline constexpr int element_count<TE_VEC4> = 4;
  template<> inline constexpr int element_count<TE_MAT2> = 4;
  template<> inline constexpr int element_count<TE_MAT3> = 9;
  template<> inline constexpr int element_count<TE_MAT4> = 16;

  template<te_type T>
  constexpr te_type element_tetype = TE_ERROR;

  template<> inline constexpr te_type element_tetype<TE_VEC2> = TE_FLOAT;
  template<> inline constexpr te_type element_tetype<TE_VEC3> = TE_FLOAT;
  template<> inline constexpr te_type element_tetype<TE_VEC4> = TE_FLOAT;
  template<> inline constexpr te_type element_tetype<TE_MAT2> = TE_VEC2;
  template<> inline constexpr te_type element_tetype<TE_MAT3> = TE_VEC3;
  template<> inline constexpr te_type element_tetype<TE_MAT4> = TE_VEC4;
  template<> inline constexpr te_type element_tetype<TE_VEC2_REF> = TE_FLOAT_REF;
  template<> inline constexpr te_type element_tetype<TE_VEC3_REF> = TE_FLOAT_REF;
  template<> inline constexpr te_type element_tetype<TE_VEC4_REF> = TE_FLOAT_REF;
  template<> inline constexpr te_type element_tetype<TE_MAT2_REF> = TE_VEC2_REF;
  template<> inline constexpr te_type element_tetype<TE_MAT3_REF> = TE_VEC3_REF;
  template<> inline constexpr te_type element_tetype<TE_MAT4_REF> = TE_VEC4_REF;

  template<typename T> using element_type = type_of<element_tetype<type_value_of<T>>>;

  template<te_type RefType, te_type IsConstant = te_type(RefType & TE_CONSTANT)>
  struct index_value {
    static type_of<element_tetype<RefType>> call(type_of<RefType> v, int32_t idx) {
      return &v->arr[idx];
    }
  };
  template<te_type Type>
  struct index_value<Type, TE_CONSTANT> {
    static type_of<element_tetype<Type>> call(type_of<Type> v, int32_t idx) {
      return v.arr[idx];
    }
  };

  template<te_type RefType, te_type IsConstant = te_type(RefType & TE_CONSTANT)>
  struct negate_value {
    static type_of<te_type(RefType | TE_CONSTANT)> call(type_of<RefType> v) {
      type_of<te_type(RefType | TE_CONSTANT)> vc;
      for (int i = 0; i < element_count<te_type(RefType | TE_CONSTANT)>; ++i) {
        vc.elements[i] = -v->elements[i];
      }
      return vc;
    }
  };
  template<te_type Type>
  struct negate_value<Type, TE_CONSTANT> {
    static type_of<Type> call(type_of<Type> v) {
      for (int i = 0; i < element_count<Type>; ++i) {
        v.elements[i] = -v.elements[i];
      }
      return v;
    }
  };

  template<>
  struct negate_value<TE_FLOAT_REF, te_type(0)> {
    static type_of<TE_FLOAT> call(type_of<TE_FLOAT_REF> v) {
      return -*v;
    }
  };
  template<>
  struct negate_value<TE_FLOAT, TE_CONSTANT> {
    static type_of<TE_FLOAT> call(type_of<TE_FLOAT> v) {
      return -v;
    }
  };

  template<>
  struct negate_value<TE_INT_REF, te_type(0)> {
    static type_of<TE_INT> call(type_of<TE_INT_REF> v) {
      return -*v;
    }
  };
  template<>
  struct negate_value<TE_INT, TE_CONSTANT> {
    static type_of<TE_INT> call(type_of<TE_INT> v) {
      return -v;
    }
  };

  template<te_type RefType, te_type IsConstant = te_type(RefType & TE_CONSTANT)>
  struct swizzle2_value {
    static te_vec2 call(type_of<RefType> v, int32_t i, int32_t j) {
      return {v->arr[i], v->arr[j]};
    }
  };
  template<te_type Type>
  struct swizzle2_value<Type, TE_CONSTANT> {
    static te_vec2 call(type_of<Type> v, int32_t i, int32_t j) {
      return {v.arr[i], v.arr[j]};
    }
  };

  template<te_type RefType, te_type IsConstant = te_type(RefType & TE_CONSTANT)>
  struct swizzle3_value {
    static te_vec3 call(type_of<RefType> v, int32_t i, int32_t j, int32_t k) {
      return {v->arr[i], v->arr[j], v->arr[k]};
    }
  };
  template<te_type Type>
  struct swizzle3_value<Type, TE_CONSTANT> {
    static te_vec3 call(type_of<Type> v, int32_t i, int32_t j, int32_t k) {
      return {v.arr[i], v.arr[j], v.arr[k]};
    }
  };

  template<te_type RefType, te_type IsConstant = te_type(RefType & TE_CONSTANT)>
  struct swizzle4_value {
    static te_vec4 call(type_of<RefType> v, int32_t i, int32_t j, int32_t k, int32_t l) {
      return {v->arr[i], v->arr[j], v->arr[k], v->arr[l]};
    }
  };
  template<te_type Type>
  struct swizzle4_value<Type, TE_CONSTANT> {
    static te_vec4 call(type_of<Type> v, int32_t i, int32_t j, int32_t k, int32_t l) {
      return {v.arr[i], v.arr[j], v.arr[k], v.arr[l]};
    }
  };

  namespace detail {
    inline float fac(float a) {/* simplest version of fac */
      if (a < 0.0)
        return NAN;
      if (a > UINT_MAX)
        return INFINITY;
      unsigned int ua = (unsigned int) (a);
      unsigned long int result = 1, i;
      for (i = 1; i <= ua; i++) {
        if (i > ULONG_MAX / result)
          return INFINITY;
        result *= i;
      }
      return (float) result;
    }
    inline float ncr(float n, float r) {
      if (n < 0.0 || r < 0.0 || n < r) return NAN;
      if (n > UINT_MAX || r > UINT_MAX) return INFINITY;
      unsigned long int un = (unsigned int) (n), ur = (unsigned int) (r), i;
      unsigned long int result = 1;
      if (ur > un / 2) ur = un - ur;
      for (i = 1; i <= ur; i++) {
        if (result > ULONG_MAX / (un - ur + i))
          return INFINITY;
        result *= un - ur + i;
        result /= i;
      }
      return result;
    }

    inline float npr(float n, float r) { return ncr(n, r) * fac(r); }

    //float abs2(float n) {return (n<0 ? -n : n);}

    //float log2(float n) {const float ln2=log(2); return log(n)/ln2;}

    template<typename T>
    inline T add(T a, T b) { return a + b; }
    template<typename T>
    inline T sub(T a, T b) { return a - b; }
    template<typename T>
    inline T mul(T a, T b) { return a * b; }
    template<typename T>
    inline T div(T a, T b) { return a / b; }
    template<typename T>
    inline T mod(T a, T b) { return a % b; }
    template<>
    inline float mod<float>(float a, float b) { return fmodf(a, b); }

    inline float to_radians(float deg) { return deg * (PI / 180.0f); }
    inline float to_degrees(float rad) { return rad * (180.0f / PI); }

    inline float sign(float v) {
      if (v > 0.0f) {
        return 1.0f;
      } else if (v < 0.0f) {
        return -1.0f;
      } else {
        return v;
      }
    }

    inline void int_to_float(void *, void * args, void * ret) {
      *static_cast<float *>(ret) = static_cast<float>(*static_cast<int *>(args));
    }

    inline void float_to_int(void *, void * args, void * ret) {
      *static_cast<int *>(ret) = static_cast<int>(*static_cast<float *>(args));
    }

    // rest of the code in here only works because of how arguments are packed on the 'stack' and how vecs and mats are stored
    inline void printf(void *, void * args, void * ret) {
      te_string str = *static_cast<te_string *>(args);
    }

    template<int Size>
    inline void mem_copy(void *, void * args, void * ret) { memcpy(ret, args, Size); }

    template<int Size>
    inline void init_mem(void *, void *, void * ret) { memset(ret, 0, Size); }

    template<te_type Type>
    void float_to_vec(void *, void * args, void * ret) {
      static_assert(te::is_vec<Type>);
      float v = *static_cast<float *>(args);
      for (int i = 0; i < te::element_count<Type>; ++i) {
        static_cast<float *>(ret)[i] = v;
      }
    }

    template<te_type Type>
    void float_to_mat(void * ctx, void * args, void * ret) {
      static_assert(te::is_mat<Type>);

      init_mem<te_size_of(Type)>(ctx, args, ret);
      constexpr int MatSize = te::element_count<te::element_tetype<Type>>;
      float v = *static_cast<float *>(args);
      for (int i = 0; i < MatSize; ++i) {
        static_cast<te::type_of<Type> *>(ret)->arr[i].arr[i] = v;
      }
    }

    template<te_type FromType, te_type ToType>
    void downgrade_mat(void *, void * args, void * ret) {
      static_assert(te::is_mat<FromType>);
      static_assert(te::is_mat<ToType>);
      static_assert(FromType > ToType);
      constexpr int ToMatSize = te::element_count<te::element_tetype<ToType>>;

      for (int i = 0; i < ToMatSize; ++i) {
        for (int j = 0; j < ToMatSize; ++j) {
          static_cast<te::type_of<ToType> *>(ret)->arr[j].arr[i] = static_cast<te::type_of<FromType> *>(args)->arr[j].arr[i];
        }
      }
    }

    template<te_type FromType, te_type ToType>
    void upgrade_mat(void * ctx, void * args, void * ret) {
      static_assert(te::is_mat<FromType>);
      static_assert(te::is_mat<ToType>);
      static_assert(FromType < ToType);
      constexpr int FromMatSize = te::element_count<te::element_tetype<FromType>>;
      constexpr int ToMatSize = te::element_count<te::element_tetype<ToType>>;

      init_mem<te_size_of(ToType)>(ctx, args, ret);
      for (int i = 0; i < ToMatSize; ++i) {
        static_cast<te::type_of<ToType> *>(ret)->arr[i].arr[i] = 1.0f;
      }
      for (int i = 0; i < FromMatSize; ++i) {
        for (int j = 0; j < FromMatSize; ++j) {
          static_cast<te::type_of<ToType> *>(ret)->arr[j].arr[i] = static_cast<te::type_of<FromType> *>(args)->arr[j].arr[i];
        }
      }
    }
  }
}

template<te_type TType, bool LockType, te_type ... ETypes>
inline te_function find_variadic_constructor(const te_type * param_types, int param_count) {
  constexpr int target_size = te_size_of(TType);
  int args_size = 0;
  te_type locked_tetype = TE_NULL;
  for (int i = 0; i < param_count; ++i) {
    te_type pt = static_cast<te_type>(param_types[i] | TE_CONSTANT);
    if (!LockType || locked_tetype == TE_NULL) {
      if (!((pt == ETypes) || ...)) {
        return nullptr;
      }
      locked_tetype = pt;
    } else if (pt != locked_tetype) {
      return nullptr;
    }
    args_size += te_size_of(pt);
  }

  if (args_size == target_size) {
    return te::detail::mem_copy<target_size>;
  }

  return nullptr;
}

inline te_function find_constructor_fn(te_type type, const te_type * param_types, int param_count) {
  // single arg constructors
  if (param_count == 1) {
    te_type p0 = static_cast<te_type>(param_types[0] | TE_CONSTANT);
    if (p0 == TE_FLOAT) {
      switch (type) {
        case TE_INT:
          return te::detail::float_to_int;
          break;
        case TE_VEC2:
          return te::detail::float_to_vec<TE_VEC2>;
          break;
        case TE_VEC3:
          return te::detail::float_to_vec<TE_VEC3>;
          break;
        case TE_VEC4:
          return te::detail::float_to_vec<TE_VEC4>;
          break;
        case TE_MAT2:
          return te::detail::float_to_mat<TE_MAT2>;
          break;
        case TE_MAT3:
          return te::detail::float_to_mat<TE_MAT3>;
          break;
        case TE_MAT4:
          return te::detail::float_to_mat<TE_MAT4>;
          break;
        default:
          break;
      }
    }

    switch (type) {
      case TE_FLOAT: {
        if (p0 == TE_INT) {
          return te::detail::int_to_float;
        }
      }
        break;
      case TE_VEC2: {
        if (p0 == TE_VEC2 || p0 == TE_VEC3 || p0 == TE_VEC4) {
          return te::detail::mem_copy<te_size_of(TE_VEC2)>;
        }
      }
        break;
      case TE_VEC3: {
        if (p0 == TE_VEC3 || p0 == TE_VEC4) {
          return te::detail::mem_copy<te_size_of(TE_VEC3)>;
        }
      }
        break;
      case TE_VEC4: {
        if (p0 == TE_VEC4) {
          return te::detail::mem_copy<te_size_of(TE_VEC4)>;
        }
      }
        break;
      case TE_MAT2: {
        switch (p0) {
          case TE_MAT2:
            return te::detail::mem_copy<te_size_of(TE_MAT2)>;
          case TE_MAT3:
            return te::detail::downgrade_mat<TE_MAT3, TE_MAT2>;
          case TE_MAT4:
            return te::detail::downgrade_mat<TE_MAT4, TE_MAT2>;
          default:
            break;
        }
      }
        break;
      case TE_MAT3: {
        switch (p0) {
          case TE_MAT2:
            return te::detail::upgrade_mat<TE_MAT2, TE_MAT3>;
          case TE_MAT3:
            return te::detail::mem_copy<te_size_of(TE_MAT3)>;
          case TE_MAT4:
            return te::detail::downgrade_mat<TE_MAT4, TE_MAT3>;
          default:
            break;
        }
      }
        break;
      case TE_MAT4: {
        switch (p0) {
          case TE_MAT2:
            return te::detail::upgrade_mat<TE_MAT2, TE_MAT4>;
          case TE_MAT3:
            return te::detail::upgrade_mat<TE_MAT3, TE_MAT4>;
          case TE_MAT4:
            return te::detail::mem_copy<te_size_of(TE_MAT4)>;
          default:
            break;
        }
      }
        break;
      default:
        break;
    }
  }

  // other constructors
  te_function ret = nullptr;
  te_type element_type = TE_NULL;
  switch (type) {
    case TE_VEC2:
      return find_variadic_constructor<TE_VEC2, false, TE_FLOAT, TE_VEC2>(param_types, param_count);
    case TE_VEC3:
      return find_variadic_constructor<TE_VEC3, false, TE_FLOAT, TE_VEC2, TE_VEC3>(param_types, param_count);
    case TE_VEC4:
      return find_variadic_constructor<TE_VEC4, false, TE_FLOAT, TE_VEC2, TE_VEC3, TE_VEC4>(param_types, param_count);
    case TE_MAT2:
      return find_variadic_constructor<TE_MAT2, true, TE_FLOAT, TE_VEC2>(param_types, param_count);
    case TE_MAT3:
      return find_variadic_constructor<TE_MAT3, true, TE_FLOAT, TE_VEC3>(param_types, param_count);
    case TE_MAT4:
      return find_variadic_constructor<TE_MAT4, true, TE_FLOAT, TE_VEC4>(param_types, param_count);
    default:
      break;
  }

  return nullptr;
}

inline te_fn_obj find_constructor(te_parser_state * s, te_type type, const te_type * arg_types, int8_t arg_count) {
  for (int i = 0; i < arg_count; ++i) {
    if (arg_types[i] == TE_ERROR) {
      return te_fn_obj{};
    }
  }

  te_fn_obj fn;
  fn.ptr = find_constructor_fn(type, arg_types, arg_count);
  fn.context = nullptr;
  fn.return_type = type;
  fn.param_count = arg_count;
  for (int i = 0; i < arg_count; ++i) {
    fn.param_types[i] = static_cast<te_type>(arg_types[i] | TE_CONSTANT);
  }

#ifdef TE_DEBUG_COMPILE
  if (!fn.is_valid()) {
    te_printf("error: could not find constructor for ");
    te_print_type_name(type);
    te_printf("(");
    for (int i = 0; i < arg_count; ++i) {
      if (i != 0) {
        te_printf(", ");
      }
      te_print_type_name(arg_types[i]);
    }
    te_printf(")\n");
    te_print_error(s);
    s->parse_error = true;
  }
#endif

  return fn;
}

inline const te_variable te_builtins[] = {
    {"true", TE_INT, te::make_value<int32_t>(1)},
    {"false", TE_INT, te::make_value<int32_t>(0)},
    {"E", TE_FLOAT, te::make_value<float>(2.71828182845904523536f)},
    {"INF", TE_FLOAT, te::make_value<float>(INFINITY)},
    {"NAN", TE_FLOAT, te::make_value<float>(NAN)},
    {"PI", TE_FLOAT, te::make_value<float>(PI)},
    {"TAU", TE_FLOAT, te::make_value<float>(PI * 2.0f)},
    {"abs", TE_PURE_FUNCTION, te::make_function<fabsf>()},
    {"acos", TE_PURE_FUNCTION, te::make_function<acosf>()},
    {"asin", TE_PURE_FUNCTION, te::make_function<asinf>()},
    {"atan", TE_PURE_FUNCTION, te::make_function<atanf>()},
    {"atan2", TE_PURE_FUNCTION, te::make_function<atan2f>()},
    {"ceil", TE_PURE_FUNCTION, te::make_function<ceilf>()},
    {"cos", TE_PURE_FUNCTION, te::make_function<cosf>()},
    {"cosh", TE_PURE_FUNCTION, te::make_function<coshf>()},
    {"degrees", TE_PURE_FUNCTION, te::make_function<te::detail::to_degrees>()},
    {"exp", TE_PURE_FUNCTION, te::make_function<expf>()},
    {"fac", TE_PURE_FUNCTION, te::make_function<te::detail::fac>()},
    {"floor", TE_PURE_FUNCTION, te::make_function<floorf>()},
    {"ln", TE_PURE_FUNCTION, te::make_function<logf>()},
#ifdef TE_NAT_LOG
    {"log",     TE_PURE_FUNCTION, te::make_function<logf>()},
#else
    {"log", TE_PURE_FUNCTION, te::make_function<log10f>()},
#endif
    {"log10", TE_PURE_FUNCTION, te::make_function<log10f>()},
    {"log2", TE_PURE_FUNCTION, te::make_function<log2f>()},
    {"mod", TE_PURE_FUNCTION, te::make_function<fmodf>()},
    {"ncr", TE_PURE_FUNCTION, te::make_function<te::detail::ncr>()},
    {"npr", TE_PURE_FUNCTION, te::make_function<te::detail::npr>()},
    {"pow", TE_PURE_FUNCTION, te::make_function<powf>()},
    {"radians", TE_PURE_FUNCTION, te::make_function<te::detail::to_radians>()},
    {"round", TE_PURE_FUNCTION, te::make_function<roundf>()},
    {"sign", TE_PURE_FUNCTION, te::make_function<te::detail::sign>()},
    {"sin", TE_PURE_FUNCTION, te::make_function<sinf>()},
    {"sinh", TE_PURE_FUNCTION, te::make_function<sinhf>()},
    {"sqrt", TE_PURE_FUNCTION, te::make_function<sqrtf>()},
    {"tan", TE_PURE_FUNCTION, te::make_function<tanf>()},
    {"tanh", TE_PURE_FUNCTION, te::make_function<tanhf>()},
};

inline int te_builtins_count = sizeof(te_builtins) / sizeof(te_variable);

// static const te_variable *find_builtin(const char *name, int len) {
//     int imin = 0;
//     int imax = te_builtins_count - 1;
//
//     /*Binary search.*/
//     while (imax >= imin) {
//         const int i = (imin + ((imax-imin)/2));
//         int c = strncmp(name, te_builtins[i].name, len);
//         if (!c) c = '\0' - te_builtins[i].name[len];
//         if (c == 0) {
//             return te_builtins + i;
//         } else if (c > 0) {
//             imin = i + 1;
//         } else {
//             imax = i - 1;
//         }
//     }
//
//     return 0;
// }

inline const te_variable * find_lookup1(const te_variable * lookup, int lookup_len, const char * name, const char * end, const te_type * arg_types = nullptr, int arg_count = 0) {
  const int len = end - name;
  int iters;
  const te_variable * var;
  if (lookup) {
    for (var = lookup, iters = lookup_len; iters; ++var, --iters) {
      if (strncmp(name, var->name, len) == 0 && strlen(var->name) == len) {
        if (arg_types) {
          if (TE_IS_FUNCTION(var->type) && arg_count == var->fn.param_count) {
            for (int i = 0; i < arg_count; ++i) {
              if ((arg_types[i] | (var->fn.param_types[i] & TE_CONSTANT)) != var->fn.param_types[i]) {
                goto continue_outer;
              }
            }

            return var;
          }
        } else {
          return var;
        }
      }
    continue_outer:;
    }
  }

  return nullptr;
}

inline const te_variable * find_lookup(const te_parser_state * s, const char * name, const char * end, te_type * arg_types = nullptr, int arg_count = 0) {
  for (int i = 0; i < arg_count; ++i) {
    if (arg_types[i] == TE_ERROR) {
      return nullptr;
    }
  }

  const te_variable * result = find_lookup1(s->lookup, s->lookup_len, name, end, arg_types, arg_count);
  if (result) {
    return result;
  }

  if (s->parent) {
    return find_lookup(s->parent, name, end, arg_types, arg_count);
  }

  const te_variable * ret = find_lookup1(te_builtins, te_builtins_count, name, end, arg_types, arg_count);

#ifdef TE_DEBUG_COMPILE
  if (!ret) {
    if (arg_types) {
      te_printf("error: could not find function matching '%.*s", end - name, name);
      te_printf("(");
      for (int i = 0; i < arg_count; ++i) {
        if (i != 0) {
          te_printf(", ");
        }
        te_print_type_name(arg_types[i]);
      }
      te_printf(")'\n");
      te_print_error(s);
    } else {
      te_printf("error: could not find '%.*s'!\n", end - name, name);
      te_print_error(s);
    }
  }
#endif

  return ret;
}

inline void next_token(te_parser_state * s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered next_token\n");
#endif

  s->prev_end = s->end;

  // TODO: include directive
  bool new_line = s->end == s->program;
  do {
    s->type = TE_NULL;
    s->token = TOK_NULL;
    s->start = s->end;

    if (s->start[0] == '\0') {
      s->token = TOK_END;
      break;
    }

    /* Try reading a number. */
    if (isdigit(s->start[0]) || (s->start[0] == '.' && isdigit(s->start[1]))) {
      char * parsed_int_end;
      int32_t parsed_int = strtol(s->start, &parsed_int_end, 0);
      char * parsed_float_end;
      float parsed_float = strtof(s->start, &parsed_float_end);
      if (parsed_float_end > parsed_int_end) {
        // strtof parsed more, use that
        s->end = parsed_float_end;
        s->value.float_ = parsed_float;
        s->type = TE_FLOAT;
        s->token = TOK_LITERAL;
        break;
      } else if (parsed_int_end > s->end) {
        // strtol parsed something..
        s->end = parsed_int_end;
        s->value.int_ = parsed_int;
        s->type = TE_INT;
        s->token = TOK_LITERAL;
        break;
      }
    }

    if (s->start[0] == '"') {
      const char * end_quote = strchr(s->start, '"') + 1;
      s->end = end_quote + 1;

      s->token = TOK_LITERAL;
      s->type = TE_STR;
      s->value.str.ptr = s->start + 1;
      s->value.str.length = end_quote - s->value.str.ptr;
    }

    if (isalpha(s->start[0]) || s->start[0] == '_') {
      while (isalpha(s->end[0]) || isdigit(s->end[0]) || s->end[0] == '_') s->end++;
      const int id_len = s->end - s->start;

#define MATCHES_TOKEN_(str) (id_len == (sizeof(str) - 1) && strncmp(str, s->start, id_len) == 0)
      switch (*s->start) {
        case 'b': {
          if (MATCHES_TOKEN_("break")) {
            s->token = TOK_BREAK;
          }
        }
          break;
        case 'c': {
          if (MATCHES_TOKEN_("case")) {
            s->token = TOK_CASE;
          } else if (MATCHES_TOKEN_("continue")) {
            s->token = TOK_CONTINUE;
          }
        }
          break;
        case 'd': {
          if (MATCHES_TOKEN_("discard")) {
            s->token = TOK_DISCARD;
          } else if (MATCHES_TOKEN_("do")) {
            s->token = TOK_DO;
          }
        }
          break;
        case 'e': {
          if (MATCHES_TOKEN_("else")) {
            s->token = TOK_ELSE;
          }
        }
          break;
        case 'f': {
          if (MATCHES_TOKEN_("float")) {
            s->type = TE_FLOAT;
            s->token = TOK_TYPENAME;
          } else if (MATCHES_TOKEN_("for")) {
            s->token = TOK_FOR;
          }
        }
          break;
        case 'i': {
          if (MATCHES_TOKEN_("if")) {
            s->token = TOK_IF;
          } else if (MATCHES_TOKEN_("in")) {
            s->token = TOK_IN;
          } else if (MATCHES_TOKEN_("inout")) {
            s->token = TOK_INOUT;
          } else if (MATCHES_TOKEN_("int")) {
            s->type = TE_INT;
            s->token = TOK_TYPENAME;
          }
        }
          break;
        case 'm': {
          if (MATCHES_TOKEN_("mat2")) {
            s->type = TE_MAT2;
            s->token = TOK_TYPENAME;
          } else if (MATCHES_TOKEN_("mat3")) {
            s->type = TE_MAT3;
            s->token = TOK_TYPENAME;
          } else if (MATCHES_TOKEN_("mat4")) {
            s->type = TE_MAT4;
            s->token = TOK_TYPENAME;
          }
        }
          break;
        case 'o': {
          if (MATCHES_TOKEN_("out")) {
            s->token = TOK_OUT;
          }
        }
          break;
        case 'r': {
          if (MATCHES_TOKEN_("return")) {
            s->token = TOK_RETURN;
          }
        }
          break;
        case 's': {
          if (MATCHES_TOKEN_("switch")) {
            s->token = TOK_SWITCH;
          } else if (MATCHES_TOKEN_("str")) {
            s->type = TE_STR;
            s->token = TOK_TYPENAME;
          }
        }
          break;
        case 'u': {
          if (MATCHES_TOKEN_("uniform")) {
            s->token = TOK_UNIFORM;
          }
        }
          break;
        case 'v': {
          if (MATCHES_TOKEN_("vec2")) {
            s->type = TE_VEC2;
            s->token = TOK_TYPENAME;
          } else if (MATCHES_TOKEN_("vec3")) {
            s->type = TE_VEC3;
            s->token = TOK_TYPENAME;
          } else if (MATCHES_TOKEN_("vec4")) {
            s->type = TE_VEC4;
            s->token = TOK_TYPENAME;
          }
        }
          break;
        case 'w': {
          if (MATCHES_TOKEN_("while")) {
            s->token = TOK_WHILE;
          }
        }
          break;
      }
#undef MATCHES_TOKEN_

      if (s->token == TOK_NULL) {
        s->token = TOK_IDENTIFIER;
      }

      break;
    }

    /* Look for an operator or special character. */
    switch (s->end++[0]) {
      case '&': {
        if (*s->end == '&') {
          s->end++;
          s->token = TOK_AND_AND;
        } else {
          s->token = TOK_AND;
        }
      }
        break;
      case '|': {
        if (*s->end == '|') {
          s->end++;
          s->token = TOK_OR_OR;
        } else {
          s->token = TOK_OR;
        }
      }
        break;
      case '=': {
        if (*s->end == '=') {
          s->end++;
          s->token = TOK_EQUAL_EQUAL;
        } else {
          s->token = TOK_EQUAL;
        }
      }
        break;
      case '!': {
        if (*s->end == '=') {
          s->end++;
          s->token = TOK_BANG_EQUAL;
        } else {
          s->token = TOK_BANG;
        }
      }
        break;
      case '<': {
        if (*s->end == '=') {
          s->end++;
          s->token = TOK_LESS_EQUAL;
        } else {
          s->token = TOK_LESS;
        }
      }
        break;
      case '>': {
        if (*s->end == '=') {
          s->end++;
          s->token = TOK_GREATER_EQUAL;
        } else {
          s->token = TOK_GREATER;
        }
      }
        break;
      case '+': {
        if (*s->end == '+') {
          s->end++;
          s->token = TOK_INC;
        } else if (*s->end == '=') {
          s->end++;
          s->token = TOK_ADD_EQUAL;
        } else {
          s->token = TOK_ADD;
        }
      }
        break;
      case '-': {
        if (*s->end == '-') {
          s->end++;
          s->token = TOK_DEC;
        } else if (*s->end == '=') {
          s->end++;
          s->token = TOK_SUB_EQUAL;
        } else {
          s->token = TOK_SUB;
        }
      }
        break;
      case '*': {
        if (*s->end == '=') {
          s->end++;
          s->token = TOK_MUL_EQUAL;
        } else {
          s->token = TOK_MUL;
        }
      }
        break;
      case '/': {
        if (*s->end == '=') {
          s->end++;
          s->token = TOK_DIV_EQUAL;
        } else {
          s->token = TOK_DIV;
        }
      }
        break;
      case '%': {
        if (*s->end == '=') {
          s->end++;
          s->token = TOK_MOD_EQUAL;
        } else {
          s->token = TOK_MOD;
        }
      }
        break;
      case '(':
        s->token = TOK_OPEN_PAREN;
        break;
      case ')':
        s->token = TOK_CLOSE_PAREN;
        break;
      case '[':
        s->token = TOK_OPEN_SQUARE_BRACKET;
        break;
      case ']':
        s->token = TOK_CLOSE_SQUARE_BRACKET;
        break;
      case '{':
        s->token = TOK_OPEN_CURLY_BRACKET;
        break;
      case '}':
        s->token = TOK_CLOSE_CURLY_BRACKET;
        break;
      case ',':
        s->token = TOK_SEP;
        break;
      case '.':
        s->token = TOK_DOT;
        break;
      case ';':
        s->token = TOK_SEMICOLON;
        break;
      case '\r':
        if (s->end[0] == '\n') s->end++; // fallthrough
      case '\n':
        new_line = true;
        s->line_num++;
        s->line_start = s->end;
        // fallthrough
      case ' ':
      case '\t':
        break;
      default: {
#ifdef TE_DEBUG_COMPILE
        te_error_record er(*s);
        te_printf("error: unrecognised token!\n");
        te_print_error(s);
#endif
        s->parse_error = true;
      }
        break;
    }
  } while (s->token == TOK_NULL);
#ifdef TE_DEBUG_PEDANTIC
  te_printf("parsed token '%.*s'\n", s->end - s->start, s->start);
#endif
}

inline te_fn_obj te_get_index_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_VEC2:
      return te::make_function<te::index_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_function<te::index_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_function<te::index_value<TE_VEC4>::call>();
    case TE_MAT2:
      return te::make_function<te::index_value<TE_MAT2>::call>();
    case TE_MAT3:
      return te::make_function<te::index_value<TE_MAT3>::call>();
    case TE_MAT4:
      return te::make_function<te::index_value<TE_MAT4>::call>();
    case TE_VEC2_REF:
      return te::make_function<te::index_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_function<te::index_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_function<te::index_value<TE_VEC4_REF>::call>();
    case TE_MAT2_REF:
      return te::make_function<te::index_value<TE_MAT2_REF>::call>();
    case TE_MAT3_REF:
      return te::make_function<te::index_value<TE_MAT3_REF>::call>();
    case TE_MAT4_REF:
      return te::make_function<te::index_value<TE_MAT4_REF>::call>();
    default:
      break;
  }

  return ret;
}

inline te_fn_obj te_get_swizzle2_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_VEC2_REF:
      return te::make_function<te::swizzle2_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_function<te::swizzle2_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_function<te::swizzle2_value<TE_VEC4_REF>::call>();
    case TE_VEC2:
      return te::make_function<te::swizzle2_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_function<te::swizzle2_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_function<te::swizzle2_value<TE_VEC4>::call>();
    default:
      break;
  }

  return ret;
}

inline te_fn_obj te_get_swizzle3_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_VEC2_REF:
      return te::make_function<te::swizzle3_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_function<te::swizzle3_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_function<te::swizzle3_value<TE_VEC4_REF>::call>();
    case TE_VEC2:
      return te::make_function<te::swizzle3_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_function<te::swizzle3_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_function<te::swizzle3_value<TE_VEC4>::call>();
    default:
      break;
  }

  return ret;
}

inline te_fn_obj te_get_swizzle4_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_VEC2_REF:
      return te::make_function<te::swizzle4_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_function<te::swizzle4_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_function<te::swizzle4_value<TE_VEC4_REF>::call>();
    case TE_VEC2:
      return te::make_function<te::swizzle4_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_function<te::swizzle4_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_function<te::swizzle4_value<TE_VEC4>::call>();
    default:
      break;
  }

  return ret;
}

inline te_fn_obj te_get_negate_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_INT:
      return te::make_function<te::negate_value<TE_INT>::call>();
    case TE_FLOAT:
      return te::make_function<te::negate_value<TE_FLOAT>::call>();
    case TE_VEC2:
      return te::make_function<te::negate_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_function<te::negate_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_function<te::negate_value<TE_VEC4>::call>();
    case TE_MAT2:
      return te::make_function<te::negate_value<TE_MAT2>::call>();
    case TE_MAT3:
      return te::make_function<te::negate_value<TE_MAT3>::call>();
    case TE_MAT4:
      return te::make_function<te::negate_value<TE_MAT4>::call>();
    case TE_INT_REF:
      return te::make_function<te::negate_value<TE_INT_REF>::call>();
    case TE_FLOAT_REF:
      return te::make_function<te::negate_value<TE_FLOAT_REF>::call>();
    case TE_VEC2_REF:
      return te::make_function<te::negate_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_function<te::negate_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_function<te::negate_value<TE_VEC4_REF>::call>();
    case TE_MAT2_REF:
      return te::make_function<te::negate_value<TE_MAT2_REF>::call>();
    case TE_MAT3_REF:
      return te::make_function<te::negate_value<TE_MAT3_REF>::call>();
    case TE_MAT4_REF:
      return te::make_function<te::negate_value<TE_MAT4_REF>::call>();
    default:
      break;
  }

  return ret;
}

namespace te::detail {
  template<auto Op>
  struct fn_scalar_op_scalar {
    template<typename TRet, typename TA, typename TB>
    inline static void call(void *, void * args, void * retvp) {
      static_assert(te::is_same<TA, TB>);
      TRet * ret = (TRet *) retvp;
      TA * ap = (TA *) args;
      TB * bp = (TB *) (&ap[1]);
      TA a = *ap;
      TB b = *bp;
      *ret = Op(a, b);
    }
  };

  template<auto Op>
  struct fn_vec_op_vec {
    template<typename TRet, typename TA, typename TB>
    inline static void call(void *, void * args, void * retvp) {
      static_assert(te::is_same<TA, TB>);
      TRet * ret = (TRet *) retvp;
      TA * ap = (TA *) args;
      TB * bp = (TB *) (&ap[1]);
      TA a = *ap;
      TB b = *bp;
      for (int i = 0; i < te::element_count<te::type_value_of<TB>>; ++i) {
        ret->elements[i] = Op(a.elements[i], b.elements[i]);
      }
    }
  };

  template<auto Op>
  struct fn_scalar_op_vec {
    template<typename TRet, typename TA, typename TB>
    inline static void call(void *, void * args, void * retvp) {
      TRet * ret = (TRet *) retvp;
      TA * ap = (TA *) args;
      TB * bp = (TB *) (&ap[1]);
      TA a = *ap;
      TB b = *bp;
      for (int i = 0; i < te::element_count<te::type_value_of<TB>>; ++i) {
        ret->elements[i] = Op(a, b.elements[i]);
      }
    }
  };

  template<auto Op>
  struct fn_vec_op_scalar {
    template<typename TRet, typename TA, typename TB>
    inline static void call(void *, void * args, void * retvp) {
      TRet * ret = (TRet *) retvp;
      TA * ap = (TA *) args;
      TB * bp = (TB *) (&ap[1]);
      TA a = *ap;
      TB b = *bp;
      for (int i = 0; i < te::element_count<te::type_value_of<TA>>; ++i) {
        ret->elements[i] = Op(a.elements[i], b);
      }
    }
  };

  template<typename TRet, typename TA, typename TB>
  inline void fn_matrix_mul_vec(void *, void * args, void * retvp) {
    constexpr auto MatSize = te::element_count<te::element_tetype<te::type_value_of<TA>>>;
    static_assert(MatSize == te::element_count<te::type_value_of<TB>>);
    TRet * ret = (TRet *) retvp;
    TA * ap = (TA *) args;
    TB * bp = (TB *) (&ap[1]);
    TA a = *ap;
    TB b = *bp;
    for (int i = 0; i < MatSize; ++i) {
      float sum = 0.0f;
      for (int j = 0; j < MatSize; ++j) {
        sum += te::detail::mul(a.arr[i].arr[j], b.arr[j]);
      }
      ret->arr[i] = sum;
    }
  }

  template<typename TRet, typename TA, typename TB>
  inline void fn_vec_mul_matrix(void *, void * args, void * retvp) {
    constexpr auto MatSize = te::element_count<te::element_tetype<te::type_value_of<TB>>>;
    static_assert(te::element_count<te::type_value_of<TA>> == MatSize);
    TRet * ret = (TRet *) retvp;
    TA * ap = (TA *) args;
    TB * bp = (TB *) (&ap[1]);
    TA a = *ap;
    TB b = *bp;
    for (int i = 0; i < MatSize; ++i) {
      float sum = 0.0f;
      for (int j = 0; j < MatSize; ++j) {
        sum += te::detail::mul(a.arr[i], b.arr[i].arr[j]);
      }
      ret->arr[i] = sum;
    }
  }

  template<typename TRet, typename TA, typename TB>
  inline void fn_matrix_mul_matrix(void *, void * args, void * retvp) {
    static_assert(te::is_same<TA, TB>);
    constexpr auto MatSize = te::element_count<te::element_tetype<te::type_value_of<TA>>>;
    TRet * ret = (TRet *) retvp;
    TA * ap = (TA *) args;
    TB * bp = (TB *) (&ap[1]);
    TA a = *ap;
    TB b = *bp;
    for (int i = 0; i < MatSize; ++i) {
      for (int j = 0; j < MatSize; ++j) {
        float sum = 0.0f;
        for (int k = 0; k < MatSize; ++k) {
          sum += te::detail::mul(a.arr[i].arr[k], b.arr[k].arr[j]);
        }
        ret->arr[i].arr[j] = sum;
      }
    }
  }
}

#define TE_OP_FUNCTION(FNAME, TRET, TA, TB) te::detail::make_function_raw<FNAME<te::type_of<(TRET)>, te::type_of<(TA)>, te::type_of<(TB)>>, (TA), (TA), (TB)>::call()

#define CASE_SCALAR_SCALAR_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_OP_FUNCTION(te::detail::fn_scalar_op_scalar<(OP)>::call, TA, TA, TB)
#define CASE_VEC_VEC_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_OP_FUNCTION(te::detail::fn_vec_op_vec<(OP)>::call, TA, TA, TB)
#define CASE_SCALAR_VEC_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_OP_FUNCTION(te::detail::fn_scalar_op_vec<(OP)>::call, TB, TA, TB)
#define CASE_VEC_SCALAR_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_OP_FUNCTION(te::detail::fn_vec_op_scalar<(OP)>::call, TA, TA, TB)
#define CASE_MATRIX_VEC_MUL_(TA, TB) if (typeA == (TA) && typeB == (TB)) return TE_OP_FUNCTION(te::detail::fn_matrix_mul_vec, TB, TA, TB)
#define CASE_VEC_MATRIX_MUL_(TA, TB) if (typeA == (TA) && typeB == (TB)) return TE_OP_FUNCTION(te::detail::fn_vec_mul_matrix, TA, TA, TB)
#define CASE_MATRIX_MATRIX_MUL_(T) if (typeA == (T) && typeB == (T)) return TE_OP_FUNCTION(te::detail::fn_matrix_mul_matrix, T, T, T)

// void mm4_mul_(void*, void *args, void *retvp) {
//     te_mat4 *ret = (te_mat4*)ret;
//     te_mat4 *ap = (te_mat4*)args;
//     te_mat4 a = *ap;
//     te_mat4 b = *(te_mat4*)(&ap[1]);
//     for (int i = 0; i < 4; ++i) {
//         for (int j = 0; j < 4; ++j) {
//             float sum = 0.0f;
//             for (int k = 0; k < 4; ++k) {
//                 sum += te::detail::mul(a.arr[i].arr[k], b.arr[k].arr[j]);
//             }
//             ret->arr[i].arr[j] = sum;
//         }
//     }
// };

inline te_fn_obj te_get_add_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::add<int32_t>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::add<float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::add<float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::add<float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::add<float>, TE_VEC4);

  return ret;
}

inline te_fn_obj te_get_sub_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::sub<int32_t>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::sub<float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::sub<float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::sub<float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::sub<float>, TE_VEC4);

  return ret;
}

inline te_fn_obj te_get_mod_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::mod<int32_t>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::mod<float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::mod<float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::mod<float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::mod<float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_VEC2, te::detail::mod<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC3, te::detail::mod<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC4, te::detail::mod<float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mod<float>, TE_VEC2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mod<float>, TE_VEC3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mod<float>, TE_VEC4);

  return ret;
}

inline te_fn_obj te_get_mul_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::mul<int32_t>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::mul<float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::mul<float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::mul<float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::mul<float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_VEC2, te::detail::mul<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC3, te::detail::mul<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC4, te::detail::mul<float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<float>, TE_VEC2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<float>, TE_VEC3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_MAT2, te::detail::mul<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_MAT3, te::detail::mul<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_MAT4, te::detail::mul<float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<float>, TE_MAT2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<float>, TE_MAT3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<float>, TE_MAT4);
  CASE_MATRIX_VEC_MUL_(TE_MAT2, TE_VEC2);
  CASE_MATRIX_VEC_MUL_(TE_MAT3, TE_VEC3);
  CASE_MATRIX_VEC_MUL_(TE_MAT4, TE_VEC4);
  CASE_VEC_MATRIX_MUL_(TE_VEC2, TE_MAT2);
  CASE_VEC_MATRIX_MUL_(TE_VEC3, TE_MAT3);
  CASE_VEC_MATRIX_MUL_(TE_VEC4, TE_MAT4);
  CASE_MATRIX_MATRIX_MUL_(TE_MAT2);
  CASE_MATRIX_MATRIX_MUL_(TE_MAT3);
  CASE_MATRIX_MATRIX_MUL_(TE_MAT4);

  return ret;
}

inline te_fn_obj te_get_div_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::div<int32_t>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::div<float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::div<float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::div<float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::div<float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_VEC2, te::detail::div<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC3, te::detail::div<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC4, te::detail::div<float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<float>, TE_VEC2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<float>, TE_VEC3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_MAT2, te::detail::div<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_MAT3, te::detail::div<float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_MAT4, te::detail::div<float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<float>, TE_MAT2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<float>, TE_MAT3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<float>, TE_MAT4);

  return ret;
}

#undef CASE_MATRIX_MUL_
#undef CASE_VEC_SCALAR_
#undef CASE_SCALAR_VEC_
#undef CASE_VEC_VEC_
#undef CASE_SCALAR_SCALAR_

inline te_expr * expr(te_parser_state * s);
inline te_expr * power(te_parser_state * s);

inline te_expr * base(te_parser_state * s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered base\n");
#endif

  /* <base>      =    <literal> | <variable> | <function-X> "(" {<expr> {"," | "," <expr>}+} ")" | "(" <expr> ")" */
  te_error_record er(*s);
  te_expr * ret = nullptr;
  switch (s->token) {
    case TOK_LITERAL: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("base is literal\n");
#endif
      ret = new_value_expr(s->type, s->value);
      next_token(s);
    }
      break;
    case TOK_TYPENAME:
    case TOK_IDENTIFIER: {
      bool is_typename = s->token == TOK_TYPENAME;
      te_type type = s->type;
      const char * tok_start = s->start;
      const char * tok_end = s->end;

      next_token(s);
      if (s->token == TOK_OPEN_PAREN) {
#ifdef TE_DEBUG_PEDANTIC
        te_printf("base is function call\n");
#endif
        te_expr * args[TE_PARAM_COUNT_MAX]{nullptr};

        next_token(s);
        int arg_count = 0;
        te_type arg_types[TE_PARAM_COUNT_MAX]{TE_NULL};
        bool valid = true;
        while (s->token != TOK_CLOSE_PAREN && s->token != TOK_END) {
          te_expr * arg = expr(s);

          if (arg_count < TE_PARAM_COUNT_MAX) {
            arg_types[arg_count] = te_expr_result_type(arg);
            args[arg_count] = arg;
            ++arg_count;
          } else {
            valid = false;
            te_free(arg);
            arg = nullptr;
          }

          if (s->token == TOK_SEP) {
            next_token(s);
          }
        }

        te_type fn_type = TE_FUNCTION;
        te_fn_obj fn;
        if (s->token == TOK_CLOSE_PAREN) {
          er.set_end(s->end);
          if (is_typename) {
            fn_type = TE_PURE_FUNCTION;
            fn = find_constructor(s, type, arg_types, arg_count);

            if (!fn.is_valid()) {
              valid = false;
              s->parse_error = true;
            }
          } else {
            const te_variable * var = find_lookup(s, tok_start, tok_end, arg_types, arg_count);

            if (!var) {
              valid = false;
              s->parse_error = true;
            } else {
              fn_type = var->type;
              fn = var->fn;
            }
          }

          next_token(s);
        } else {
#ifdef TE_DEBUG_COMPILE
          te_error_record er2(*s);
          te_printf("error: missing ')'!\n");
          te_print_error(s);
#endif
          valid = false;
          s->parse_error = true;
        }

        if (valid) {
          ret = new_function_value_expr(fn_type, fn, args, arg_count);
#ifdef TE_DEBUG_PEDANTIC
          te_printf("function value: ");
          te_print_value(ret->type, ret->value);
          te_printf("\n");
#endif
        } else {
          for (int j = arg_count - 1; j >= 0; --j) {
            te_free(args[j]);
            args[j] = nullptr;
          }
          ret = nullptr;
        }
      } else {
        const te_variable * var = find_lookup(s, tok_start, tok_end);

        if (var) {
          ret = new_value_expr(var->type, var->value);
#ifdef TE_DEBUG_PEDANTIC
          te_printf("base is variable, value: ");
          te_print_expr(ret);
          te_printf("\n");
#endif
        } else {
          s->parse_error = true;
        }

      }
    }
      break;
    case TOK_OPEN_PAREN: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("base is grouping expr\n");
#endif
      next_token(s);
      ret = expr(s);

      if (s->token == TOK_CLOSE_PAREN) {
        next_token(s);
      } else if (ret != nullptr) {
#ifdef TE_DEBUG_COMPILE
        te_error_record er2(*s);
        te_printf("error: expected ')', got ");
        te_print_token(s->token);
        te_printf("!\n");
        te_print_error(s);
#endif
        te_free(ret);
        ret = nullptr;
        s->parse_error = true;
        return nullptr;
      }
    }
      break;

    default:
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected expression, got ");
      te_print_token(s->token);
      te_printf("!\n");
      te_print_error(s);
#endif
      next_token(s);
      s->parse_error = true;
      return nullptr;
  }

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited base\n");
#endif

  return ret;
}

inline te_expr * index(te_parser_state * s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered index\n");
#endif

  /* <index>     =    <base> { "[" <int> "]" } */
  te_error_record er(*s);
  te_expr * ret = base(s);

  if (s->token == TOK_OPEN_SQUARE_BRACKET) {
    te_type ret_type = te_expr_result_type(ret);

    next_token(s);

    te_expr * idx_expr = nullptr;

    if (s->token == TOK_CLOSE_SQUARE_BRACKET) {
#ifdef TE_DEBUG_COMPILE
      te_error_record er2(*s);
      te_printf("error: missing index!\n");
      te_print_error(s);
#endif
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
      return nullptr;
    }

    idx_expr = expr(s);

    te_type idx_type = te_expr_result_type(idx_expr);

    te_fn_obj index_fn;
    if ((idx_type | TE_CONSTANT) == TE_INT) {
      index_fn = te_get_index_func(ret_type);
    } else {
      // set to a valid noop fn to avoid double error print
      index_fn = te_fn_obj{};
      index_fn.ptr = &te_noop;
#ifdef TE_DEBUG_COMPILE
      if (idx_type != TE_ERROR) {
        te_error_record er2(*s);
        te_printf("error: index must be an int! type: ");
        te_print_type_name(idx_type);
        te_printf("\n");
        te_print_error(s);
      }
#endif
      te_free(idx_expr);
      idx_expr = nullptr;
      idx_type = te_expr_result_type(idx_expr);
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
    }

    if (s->token != TOK_CLOSE_SQUARE_BRACKET) {
#ifdef TE_DEBUG_COMPILE
      te_error_record er2(*s);
      te_printf("error: expected ']', got ");
      te_print_token(s->token);
      te_printf("!\n");
      te_print_error(s);
#endif
      te_free(idx_expr);
      idx_expr = nullptr;
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
      return nullptr;
    }

    next_token(s);

    if (!index_fn.is_valid()) {
#ifdef TE_DEBUG_COMPILE
      er.set_end(s->prev_end);
      te_printf("error: cannot index! type: ");
      te_print_type_name(idx_type);
      te_printf("\n");
      te_print_error(s);
#endif
      te_free(idx_expr);
      idx_expr = nullptr;
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
      return nullptr;
    }
    te_expr * args[] = {ret, idx_expr};
    ret = new_function_value_expr(TE_PURE_FUNCTION, index_fn, args, 2);
  }

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited index\n");
#endif

  return ret;
}

inline te_expr * attr(te_parser_state * s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered attr\n");
#endif

  /* <attr>      =    <index> { "." ("x" | "y" | "z" | "w" | "r" | "g" | "b" | "a")[1-4] } */
  te_error_record er(*s);
  te_expr * ret = index(s);
  te_type ret_type = te_expr_result_type(ret);

  if (s->token == TOK_DOT) {
    next_token(s);
    if (s->token != TOK_IDENTIFIER) {
#ifdef TE_DEBUG_COMPILE
      te_error_record er2(*s);
      te_printf("error: expected swizzlers after '.' got ");
      te_print_token(s->token);
      te_printf("!\n");
      te_print_error(s);
#endif
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
      return nullptr;
    }

    te_type base_type = te_expr_result_type(ret);
    int swizzle_max = 0;
    switch (base_type) {
      case TE_VEC2:
        swizzle_max = 2;
        break;
      case TE_VEC3:
        swizzle_max = 3;
        break;
      case TE_VEC4:
        swizzle_max = 4;
        break;
      default: {
#ifdef TE_DEBUG_COMPILE
        te_error_record er2(*s);
        te_printf("error: cannot swizzle ");
        te_print_type_name(static_cast<te_type>(base_type | TE_CONSTANT));
        te_printf("!\n");
        te_print_error(s);
#endif
        te_free(ret);
        ret = nullptr;
        s->parse_error = true;
        return nullptr;
      }
    }

    int indices[4];
    int i = 0;
    for (const char * c = s->start; c < s->end; ++c, ++i) {
      if (i < 4) {
        switch (*c) {
          case 'x':
          case 'r':
            indices[i] = 0;
            break;
          case 'y':
          case 'g':
            indices[i] = 1;
            break;
          case 'z':
          case 'b':
            indices[i] = 2;
            break;
          case 'w':
          case 'a':
            indices[i] = 3;
            break;
          default: {
#ifdef TE_DEBUG_COMPILE
            te_error_record er2(*s);
            er.start = c;
            er.end = c + 1;
            te_printf("error: unknown swizzler '%c'!\n", *c);
            te_print_error(s);
#endif
            next_token(s);
            te_free(ret);
            ret = nullptr;
            s->parse_error = true;
            return nullptr;
          }
        }
      }
    }

    if (i < 1 || i > swizzle_max) {
#ifdef TE_DEBUG_COMPILE
      te_error_record er2(*s);
      te_printf("error: too many swizzlers! type: ");
      te_print_type_name(base_type);
      te_printf("\n");
      te_print_error(s);
#endif
      next_token(s);
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
      return nullptr;
    }

    te_expr ** args = static_cast<te_expr **>(alloca(sizeof(te_expr *) * (i + 1)));
    args[0] = ret;
    for (int j = 0; j < i; ++j) {
      te_value idx_value;
      idx_value.int_ = indices[j];
      args[j + 1] = new_value_expr(TE_INT, idx_value);
    }

    te_fn_obj index_fn;
    switch (i) {
      case 1:
        index_fn = te_get_index_func(ret_type);
        break;
      case 2:
        index_fn = te_get_swizzle2_func(ret_type);
        break;
      case 3:
        index_fn = te_get_swizzle3_func(ret_type);
        break;
      case 4:
        index_fn = te_get_swizzle4_func(ret_type);
        break;
      default:
        index_fn = te_fn_obj{};
        break; // unreachable
    }

    if (!index_fn.is_valid()) {
#ifdef TE_DEBUG_COMPILE
      te_error_record er2(*s);
      te_printf("internal error: no index func! type: ");
      te_print_type_name(ret_type);
      te_printf("\n");
      te_print_error(s);
#endif
      for (int j = i - 1; j >= 0; --j) {
        te_free(args[j + 1]);
        args[j + 1] = nullptr;
      }
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
    } else {
      ret = new_function_value_expr(TE_PURE_FUNCTION, index_fn, args, i + 1);
    }

    next_token(s);
  }

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited attr\n");
#endif

  return ret;
}

inline void post_increment(void *, int32_t ** args, int32_t * ret) {
  *ret = *args[0];
  ++(*args[0]);
}

inline void post_decrement(void *, int32_t ** args, int32_t * ret) {
  *ret = *args[0];
  --(*args[0]);
}

inline void pre_increment(void *, int32_t ** args, int32_t * ret) {
  ++(*args[0]);
  *ret = *args[0];
}

inline void pre_decrement(void *, int32_t ** args, int32_t * ret) {
  --(*args[0]);
  *ret = *args[0];
}

inline void bool_not(void *, int32_t * args, int32_t * ret) {
  *ret = !args[0];
}

inline te_expr * factor(te_parser_state * s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered factor\n");
#endif

  /* <factor>    =    {("!" | "-" | "--" | "+" | "++")} <attr> {("--" | "++")} */
  te_error_record er(*s);

  int bool_not_flag = 0;
  int pos = 1;
  int pre_incdec = 0;
  if (s->token == TOK_ADD) {
    next_token(s);
  } else if (s->token == TOK_SUB) {
    pos = 0;
    next_token(s);
  } else if (s->token == TOK_INC) {
    pos = 1;
    pre_incdec = 1;
    next_token(s);
  } else if (s->token == TOK_DEC) {
    pos = 0;
    pre_incdec = 1;
    next_token(s);
  } else if (s->token == TOK_BANG) {
    bool_not_flag = 1;
    next_token(s);
  }

  te_expr * ret = attr(s);
  te_type ret_type = te_expr_result_type(ret);

  er.set_end(s->end);

  if (bool_not_flag) {
    if (ret_type == TE_INT) {
      te_type param_type = TE_INT;
      ret = new_function_expr(TE_PURE_FUNCTION, (te_function) &bool_not, 0, TE_INT, &param_type, &ret, 1);
    } else {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: '!' op only works on int! type: ");
      te_print_type_name(ret_type);
      te_printf("\n");
      te_print_error(s);
#endif
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
      return nullptr;
    }
  } else if (pre_incdec) {
    if (ret_type != TE_INT_REF) {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: only int ref can be incremented/decremented! type: ");
      te_print_type_name(ret_type);
      te_printf("\n");
      te_print_error(s);
#endif
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
      return nullptr;
    }

    te_type param_type = TE_INT_REF;
    if (pos) {
      ret = new_function_expr(TE_PURE_FUNCTION, (te_function) &pre_increment, 0, TE_INT, &param_type, &ret, 1);
    } else {
      ret = new_function_expr(TE_PURE_FUNCTION, (te_function) &pre_decrement, 0, TE_INT, &param_type, &ret, 1);
    }
  } else {
    if (pos) {
      // do nothing
    } else {
      te_fn_obj tmpfn = te_get_negate_func(ret_type);

      if (!tmpfn.is_valid()) {
#ifdef TE_DEBUG_COMPILE
        te_printf("error: cannot negate ");
        te_print_type_name(ret_type);
        te_printf("!\n");
        te_print_error(s);
#endif
        te_free(ret);
        ret = nullptr;
        s->parse_error = true;
        return nullptr;
      }

      ret = new_function_value_expr(TE_PURE_FUNCTION, tmpfn, &ret, 1);
    }
  }

  ret_type = te_expr_result_type(ret);

  int postfix = 0;
  if (s->token == TOK_INC) {
    postfix = 1;
    next_token(s);
  } else if (s->token == TOK_DEC) {
    postfix = -1;
    next_token(s);
  }

  er.set_end(s->end);

  if (postfix) {
    if (ret_type != TE_INT_REF) {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: only int ref can be incremented/decremented! type: ");
      te_print_type_name(ret_type);
      te_printf("\n");
      te_print_error(s);
#endif
      te_free(ret);
      ret = nullptr;
      s->parse_error = true;
      return nullptr;
    }

    te_type param_type = TE_INT_REF;
    if (postfix > 0) {
      ret = new_function_expr(TE_PURE_FUNCTION, (te_function) &post_increment, nullptr, TE_INT, &param_type, &ret, 1);
    } else {
      ret = new_function_expr(TE_PURE_FUNCTION, (te_function) &post_decrement, nullptr, TE_INT, &param_type, &ret, 1);
    }
  }

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited factor\n");
#endif

  return ret;
}

inline te_expr * term(te_parser_state * s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered term\n");
#endif

  /* <term>      =    <factor> {("*" | "/" | "%") <factor>}+ */
  te_error_record er(*s);
  te_expr * ret = factor(s);
  te_type ret_type = te_expr_result_type(ret);

  do {
    er.set_point(s->start);

    int tok = TOK_NULL;
    char c = '?';
    if (s->token == TOK_MUL || s->token == TOK_DIV || s->token == TOK_MOD) {
      tok = s->token;
      c = *s->start;
    } else {
      break;
    }
    next_token(s);
    te_expr * rhs = factor(s);
    te_type rhs_type = te_expr_result_type(rhs);

    er.set_end(s->prev_end);

    te_fn_obj fn;
    switch (tok) {
      case TOK_MUL:
        fn = te_get_mul_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      case TOK_DIV:
        fn = te_get_div_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      case TOK_MOD:
        fn = te_get_mod_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      default:
        fn = te_fn_obj{};
        break;
    }

    if (!fn.is_valid()) {
      if (ret_type != TE_ERROR && rhs_type != TE_ERROR) {
#ifdef TE_DEBUG_COMPILE
        te_printf("error: op '%c' not found for ", c);
        te_print_type_name(ret_type);
        te_printf(" and ");
        te_print_type_name(rhs_type);
        te_printf("!\n");
        te_print_error(s);
#endif
      }
      te_free(ret);
      ret = nullptr;
      te_free(rhs);
      rhs = nullptr;
      s->parse_error = true;
    } else {
      te_expr * args[] = {ret, rhs};
      ret = new_function_value_expr(TE_PURE_FUNCTION, fn, args, 2);
    }

    ret_type = te_expr_result_type(ret);
  } while (true);

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited term\n");
#endif

  return ret;
}

inline te_expr * expr(te_parser_state * s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered expr\n");
#endif

  /* <expr>      =    <term> {("+" | "-") <term>}+ */
  te_error_record er(*s);
  te_expr * ret = term(s);
  te_type ret_type = te_expr_result_type(ret);

  do {
    er.set_point(s->start);

    int tok = TOK_NULL;
    char c = '?';
    if (s->token == TOK_ADD || s->token == TOK_SUB) {
      tok = s->token;
      c = *s->start;
    } else {
      break;
    }
    next_token(s);
    te_expr * rhs = term(s);
    te_type rhs_type = te_expr_result_type(rhs);

    er.set_end(s->prev_end);

    te_fn_obj fn;
    switch (tok) {
      case TOK_ADD:
        fn = te_get_add_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      case TOK_SUB:
        fn = te_get_sub_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      default:
        fn = te_fn_obj{};
        break;
    }

    if (!fn.is_valid()) {
      if (ret_type != TE_ERROR && rhs_type != TE_ERROR) {
#ifdef TE_DEBUG_COMPILE
        te_printf("error: op '%c' not found for ", c);
        te_print_type_name(ret_type);
        te_printf(" and ");
        te_print_type_name(rhs_type);
        te_printf("!\n");
        te_print_error(s);
#endif
      }
      te_free(ret);
      ret = nullptr;
      te_free(rhs);
      rhs = nullptr;
      s->parse_error = true;
    } else {
      te_expr * args[] = {ret, rhs};
      ret = new_function_value_expr(TE_PURE_FUNCTION, fn, args, 2);
    }

    ret_type = te_expr_result_type(ret);
  } while (true);

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited expr\n");
#endif

  return ret;
}

inline te_expr * parse_fn(te_parser_state * s) {
  /* <fn>        =    <typename> <identifier> "(" {<typename> {<identifier>} "," }+ ")" <stmt> */
  return nullptr;
}

inline void begin_suite(te_parser_state * s) {
  s->parse_error = false;
  s->stmt_count = 0;
}

inline te_expr * end_suite(te_parser_state * s) {
  te_expr * ret = nullptr;

#ifdef TE_DEBUG_COMPILE
  if (!s->parse_error) {
    for (int i = 0; i < s->stmt_count; ++i) {
      if (s->stmts[i] == nullptr) {
        te_printf("internal error: null stmt despite no parser error!\n");
        s->parse_error = true;
      }
    }
  }
#endif

  if (s->parse_error) { // :3 was here
    for (int i = 0; i < s->stmt_count; ++i) {
      te_free(s->stmts[i]);
      s->stmts[i] = nullptr;
    }
  } else {
    ret = new_suite_expr(s->return_type, s->stack_size, s->stmts, s->stmt_count);
  }

  return ret;
}

inline void push_stmt(te_parser_state * s, te_expr * e) {
  if (s->stmt_count < TE_MAX_STMT_COUNT) {
    s->stmts[s->stmt_count++] = e;
  } else {
    te_printf("error: ran out of room for stmts! max=%d\n", TE_MAX_STMT_COUNT);
    s->parse_error = true;
  }
}

inline void parse_stmt(te_parser_state * s);

inline void parse_suite(te_parser_state * s, te_token end_token) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered suite\n");
#endif

  te_expr * ret = nullptr;
  while (true) {
    if (s->token == end_token) {
      next_token(s);
      break;
    } else if (s->token == TOK_END) {
#ifdef TE_DEBUG_COMPILE
      te_error_record er2(*s);
      te_printf("error: end of input!\n");
      te_print_error(s);
#endif
      s->parse_error = true;
      break;
    }

    parse_stmt(s);
  }

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited suite: stmt_count=%d\n", s->stmt_count);
#endif
}

inline void parse_stmt(te_parser_state * s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered stmt\n");
#endif

  /* <stmt>      =    ("{" {<stmt>}+ "}" | "if" "(" <int-expr> ")" <stmt> {"else" <stmt>} | ("return" <vexpr> | "break" | "continue" | <typename> <id> {"=" <vexpr>} | <rexpr> ("=" | "+=" | "-=" | "*=" | "/=" | "%=") <vexpr> | <expr> ";")) */
  if (s->token == TOK_OPEN_CURLY_BRACKET) {
    next_token(s);
    parse_suite(s, TOK_CLOSE_CURLY_BRACKET);
  } else if (s->token == TOK_IF) {
    next_token(s);
    if (s->token != TOK_OPEN_PAREN) {
#ifdef TE_DEBUG_COMPILE
      te_error_record er2(*s);
      te_printf("error: expected '(' got ");
      te_print_token(s->token);
      te_printf("!\n");
      te_print_error(s);
#endif
      s->parse_error = true;
    }
    next_token(s);

    te_error_record er(*s);
    te_expr * cond_expr = expr(s);
    te_type cond_expr_result_type = te_expr_result_type(cond_expr);

    er.set_end(s->end);

    if (s->token != TOK_CLOSE_PAREN) {
#ifdef TE_DEBUG_COMPILE
      te_error_record er2(*s);
      te_printf("error: expected ')' got ");
      te_print_token(s->token);
      te_printf("!\n");
      te_print_error(s);
#endif
      s->parse_error = true;
    }

    te_expr * jmp_if_expr = new_jmp_if_not_expr(TE_UNKNOWN_OFFSET, cond_expr);
    push_stmt(s, jmp_if_expr);

    if (jmp_if_expr != nullptr && (cond_expr_result_type | TE_CONSTANT) != TE_INT) {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: conditional expr must be an int! type: ");
      te_print_type_name(cond_expr_result_type);
      te_printf("\n");
      te_print_error(s);
#endif
      s->parse_error = true;
    }

    next_token(s);
    parse_stmt(s);

    if (s->token == TOK_ELSE) {
      next_token(s);
      te_expr * jmp_end_expr = new_jmp_expr(TE_UNKNOWN_OFFSET);
      push_stmt(s, jmp_end_expr);
      if (jmp_if_expr) jmp_if_expr->offset = s->stmt_count;
      parse_stmt(s);
      jmp_end_expr->offset = s->stmt_count;
    } else {
      if (jmp_if_expr) jmp_if_expr->offset = s->stmt_count;
    }
  } else if (s->token == TOK_TYPENAME) {
    te_type decl_type = s->type;
    next_token(s);
  } else {
    const char * token_start = s->start;
    if (s->token == TOK_RETURN) {
      next_token(s);

      te_error_record er(*s);

      if (s->token == TOK_SEMICOLON) {
        if (s->return_type != TE_NULL) {
#ifdef TE_DEBUG_COMPILE
          te_printf("error: expected ");
          te_print_type_name(s->return_type);
          te_printf(" expression!\n");
          te_print_error(s);
#endif
          s->parse_error = true;
        }
      } else {
        te_expr * e = expr(s);
        te_type result_type = te_expr_result_type(e);

        er.set_end(s->prev_end);

        if (result_type == TE_ERROR) {
          s->parse_error = true;
        } else if ((result_type | TE_CONSTANT) != s->return_type) {
#ifdef TE_DEBUG_COMPILE
          te_printf("error: 'return' cannot convert ");
          te_print_type_name(result_type);
          te_printf(" to ");
          te_print_type_name(s->return_type);
          te_printf("\n");
          te_print_error(s);
#endif
          s->parse_error = true;
        } else {
          push_stmt(s, new_return_expr(s->return_type, e));
        }
      }
    } else {
      push_stmt(s, expr(s));
    }

    if (s->start == token_start) {
      // don't get stuck
      next_token(s);
    }

    if (s->token == TOK_SEMICOLON) {
      next_token(s);
    } else {
#ifdef TE_DEBUG_COMPILE
      te_error_record er(*s);
      te_printf("error: expected ';', got ");
      te_print_token(s->token);
      te_printf("!\n");
      te_print_error(s);
#endif
      s->parse_error = true;
    }
  }
}

inline te_expr * parse_program(te_parser_state * s) {
  /* <program>   =    {{"const" <var> | "uniform" <var> | <fn>} ";"}+ */
  s->return_type = TE_VEC3;
  begin_suite(s);
  parse_suite(s, TOK_END);
  return end_suite(s);
}

inline te_value te_trash{};

inline void te_eval_internal(const te_expr * n, char * p_stack, te_value * ret) {
#ifdef TE_DEBUG_EVAL

#define TE_ERR_FAIL_COND(cond, fail_action, ...)\
    if (cond) {\
        te_printf(__VA_ARGS__);\
        te_printf("\n");\
        fail_action;\
    } else ((void)0)

#define TE_CHECK_ACTUAL_VS_EXPECTED_TYPE(chk_type, expected, fail_action, ...)\
    if ((chk_type) != (expected)) {\
        te_printf(__VA_ARGS__);\
        te_printf(" expected ");\
        te_print_type_name(te_type(expected));\
        te_printf(", got ");\
        te_print_type_name(te_type(chk_type));\
        te_printf("\n");\
    } else ((void)0)

#else

#define TE_ERR_FAIL_COND(cond, expected, fail_action, ...)
#define TE_CHECK_ACTUAL_VS_EXPECTED_TYPE(chk_type, expected, fail_action, ...)

#endif

  TE_ERR_FAIL_COND(!n, return, "eval error: null expr!");

  switch (n->opcode) {
    case TE_OP_VALUE: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_VALUE\n");
#endif
      memcpy(ret, &n->value, n->size);
    }
      break;
    case TE_OP_DEREF: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_DEREF\n");
#endif
      void * ref;
      te_eval_internal(n->opargs[0], p_stack, (te_value *) &ref);
      memcpy(ret, ref, n->size);
    }
      break;
    case TE_OP_FUNCTION: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_FUNCTION\n");
#endif
      char * stack = static_cast<char *>(alloca(n->size));
      {
        char * stackptr = stack;
        te_type param_type = TE_ERROR;
        int param_size = TE_ERROR;
        for (int i = 0; i < n->fn.param_count; ++i, stackptr += param_size) {
          param_type = n->fn.param_types[i];
          param_size = te_size_of(param_type);

          TE_ERR_FAIL_COND(!n->fn.args[i], continue, "eval error: fn arg expr ptr is null!");
          te_type arg_result_type = te_expr_result_type(n->fn.args[i]);

          TE_CHECK_ACTUAL_VS_EXPECTED_TYPE(arg_result_type, param_type, continue, "eval error: argument and parameter types do not match!");
          te_eval_internal(n->fn.args[i], p_stack, (te_value *) stackptr);
        }
      }

      TE_ERR_FAIL_COND(!n->fn.is_valid(), return, "eval error: null function call!");
      TE_ERR_FAIL_COND(!ret && n->fn.return_type != TE_NULL, return, "eval error: return ptr is null but function returns a value!");

      n->fn.ptr(n->fn.context, (void *) stack, ret);
    }
      break;
    case TE_OP_SUITE: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_SUITE\n");
#endif
      char * stack = static_cast<char *>(alloca(n->size));

      te_expr * const * exprs = n->opargs;
      for (te_expr * const * it = exprs; *it;) {
        te_expr * se = *it;
        switch (se->opcode) {
          case TE_OP_JMP: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_JMP\n");
#endif
            TE_ERR_FAIL_COND(se->offset == TE_UNKNOWN_OFFSET, return, "eval error: unknown offset!");
            it = exprs + se->offset;
            continue;
          }
            break;
          case TE_OP_JMP_REF: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_JMP_REF\n");
#endif
            const uint16_t offset = *static_cast<uint16_t *>(se->value.ptr);
            TE_ERR_FAIL_COND(offset == TE_UNKNOWN_OFFSET, return, "eval error: unknown offset!");
            it = exprs + offset;
            continue;
          }
            break;
          case TE_OP_JMP_IF: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_JMP_IF\n");
#endif
            TE_CHECK_ACTUAL_VS_EXPECTED_TYPE(te_expr_result_type(se->opargs[0]), TE_INT, return, "eval error: incorrect type for conditional!");
            TE_ERR_FAIL_COND(se->offset == TE_UNKNOWN_OFFSET, return, "eval error: unknown offset!");

            int32_t cond = 0;
            te_eval_internal(se->opargs[0], stack, (te_value *) &cond);

            if (cond) {
              it = exprs + se->offset;
              continue;
            }
          }
            break;
          case TE_OP_JMP_IF_NOT: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_JMP_IF_NOT\n");
#endif
            TE_CHECK_ACTUAL_VS_EXPECTED_TYPE(te_expr_result_type(se->opargs[0]), TE_INT, return, "eval error: incorrect type for conditional!");
            TE_ERR_FAIL_COND(se->offset == TE_UNKNOWN_OFFSET, return, "eval error: unknown offset!");

            int32_t cond = 0;
            te_eval_internal(se->opargs[0], stack, (te_value *) &cond);

            if (!cond) {
              it = exprs + se->offset;
              continue;
            }
          }
            break;
          case TE_OP_RETURN: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_RETURN\n");
#endif
            TE_CHECK_ACTUAL_VS_EXPECTED_TYPE(te_expr_result_type(se->opargs[0]), n->type, return, "eval error: incorrect return type!");

            te_eval_internal(se->opargs[0], stack, ret);
            return;
          }
            break;
          default: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("eval stmt expr\n");
#endif
            te_eval_internal(se, stack, &te_trash);
          }
            break;
        }
        it++;
      }
    }
      break;
    case TE_OP_STACK_REF: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_STACK_REF\n");
#endif
      TE_ERR_FAIL_COND(!p_stack, return, "eval error: stack ptr is null!");
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      char * arg = p_stack + n->offset;
      if (n->type & TE_CONSTANT) {
        ret->ref = reinterpret_cast<te_value *>(arg);
      } else {
        ret->ref = *reinterpret_cast<te_value **>(arg);
      }
    }
      break;
    case TE_OP_ASSIGN: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_ASSIGN\n");
#endif
      TE_ERR_FAIL_COND(!TE_IS_REF(te_expr_result_type(n->opargs[0])), return, "eval error: lhs is not a ref!");
      TE_CHECK_ACTUAL_VS_EXPECTED_TYPE(te_expr_result_type(n->opargs[0]) | TE_CONSTANT, te_expr_result_type(n->opargs[1]) | TE_CONSTANT, return, "eval error: assignment lhs and rhs do not match!");
      void * lhs;
      TE_ERR_FAIL_COND(!p_stack, return, "eval error: stack ptr is null!");
      te_eval_internal(n->opargs[0], p_stack, reinterpret_cast<te_value *>(&lhs));
      TE_ERR_FAIL_COND(!lhs, return, "eval error: lhs ref is null!");
      if (n->type & TE_CONSTANT) {
        te_eval_internal(n->opargs[1], p_stack, reinterpret_cast<te_value *>(lhs));
      } else {
        void * ref;
        te_eval_internal(n->opargs[1], p_stack, reinterpret_cast<te_value *>(&ref));
        TE_ERR_FAIL_COND(!ref, return, "eval error: rhs ref is null!");
        memcpy(lhs, ref, n->size);
      }
    }
      break;
    case TE_OP_JMP:
    case TE_OP_JMP_IF:
    case TE_OP_JMP_IF_NOT:
    case TE_OP_RETURN: {
      TE_ERR_FAIL_COND(true, return, "eval error: op 0x%02x evaluated as a standalone expr!", int(n->opcode));
    }
      break;
    default: {
      TE_ERR_FAIL_COND(true, return, "eval error: unknown op 0x%02x!", int(n->opcode));
    }
      break;
  }

  TE_ERR_FAIL_COND(TE_IS_REF(te_expr_result_type(n)) && !ret->ref, return, "eval error: null reference!");
}

inline void optimize(te_expr * n) {
  /* Only optimize out functions flagged as pure. */
  if (n->opcode == TE_OP_FUNCTION) {
    const int arity = n->fn.param_count;
    char known = 1;
    for (int i = 0; i < arity; ++i) {
      optimize(n->fn.args[i]);
      if (!TE_IS_CONSTANT(n->fn.args[i]->type)) {
        known = 0;
      }
    }
    if (known && n->type & TE_FLAG_PURE) {
      te_value reduced; // just to be safe, use a stack var instead of overwriting expr value
      te_eval_internal(n, nullptr, &reduced);
      n->value = reduced;
      n->type = te_expr_result_type(n);
      te_free_args(n);
    }
  }
}

inline te_value te_eval(const te_expr * n) {
  te_value ret;
  te_eval_internal(n, nullptr, &ret);
  return ret;
}

template<auto ParseFn = parse_program>
inline te_expr * te_compile_internal(const char * expression, const te_variable * variables, int var_count, int * error = nullptr) {
  te_parser_state s;
  s.program = s.line_start = s.start = s.prev_end = s.end = expression;
  s.lookup = variables;
  s.lookup_len = var_count;

  next_token(&s);
  te_expr * root = ParseFn(&s);

  if (s.token != TOK_END) {
#ifdef TE_DEBUG_COMPILE
    if (root && root->type != TE_ERROR) {
      te_error_record er(s);
      te_printf("error: expected end!\n");
      te_print_error(&s);
    }
#endif
    te_free(root);
    if (error) {
      *error = (s.end - s.program);
      if (*error == 0) *error = 1;
    }
    return nullptr;
  } else {
    if (error) *error = 0;
    return root;
  }
}

inline te_expr * te_compile(const char * expression, const te_variable * variables, int var_count, int * error) {
  te_expr * ret = te_compile_internal(expression, variables, var_count, error);
  if (ret) {
    optimize(ret);
  }
  return ret;
}

inline te_value te_interp(const char * expression, te_type * result_type, int * error) {
  te_expr * n = te_compile_internal<expr>(expression, nullptr, 0, error);
  te_value ret;
  if (n) {
    if (result_type) {
      *result_type = n->type;
    }
    te_eval_internal(n, nullptr, &ret);
    te_free(n);
  } else if (result_type) {
    *result_type = TE_ERROR;
  }
  return ret;
}

inline void te_print_value(te_type type, const te_value & v) {
  if (TE_IS_FUNCTION(type)) {
    te_printf("fn@0x%p(", v.fn.ptr);
    for (int i = 0; i < v.fn.param_count; i++) {
      if (i == 0) {
        if (v.fn.context) {
          te_printf("context=0x%p, ", v.fn.context);
        }
      } else {
        te_printf(", ");
      }
      te_print_type_name(v.fn.param_types[i]);
    }
    te_printf(") -> ");
    te_print_type_name(v.fn.return_type);
  } else if (TE_IS_REF(type)) {
    te_print_type_name(type);
    te_printf("(@0x%p)", v.ref);
  } else {
    switch (type) {
      case TE_ERROR:
        te_printf("<error>");
        break;
      case TE_NULL:
        te_printf("<null>");
        break;
      case TE_INT:
        te_printf("%ld", long(v.int_));
        break;
      case TE_FLOAT:
        te_printf("%f", v.float_);
        break;
      case TE_VEC2:
        te_printf("vec2(%f, %f)", v.vec2.x, v.vec2.y);
        break;
      case TE_VEC3:
        te_printf("vec3(%f, %f, %f)", v.vec3.x, v.vec3.y, v.vec3.z);
        break;
      case TE_VEC4:
        te_printf("vec4(%f, %f, %f, %f)", v.vec4.x, v.vec4.y, v.vec4.z, v.vec4.w);
        break;
      case TE_MAT2:
        te_printf("mat2(");
        te_print_value(TE_VEC2, (te_value &) v.mat2.arr[0]);
        te_printf(", ");
        te_print_value(TE_VEC2, (te_value &) v.mat2.arr[1]);
        te_printf(")");
        break;
      case TE_MAT3:
        te_printf("mat3(");
        te_print_value(TE_VEC3, (te_value &) v.mat3.arr[0]);
        te_printf(", ");
        te_print_value(TE_VEC3, (te_value &) v.mat3.arr[1]);
        te_printf(", ");
        te_print_value(TE_VEC3, (te_value &) v.mat3.arr[2]);
        te_printf(")");
        break;
      case TE_MAT4:
        te_printf("mat4(");
        te_print_value(TE_VEC4, (te_value &) v.mat4.arr[0]);
        te_printf(", ");
        te_print_value(TE_VEC4, (te_value &) v.mat4.arr[1]);
        te_printf(", ");
        te_print_value(TE_VEC4, (te_value &) v.mat4.arr[2]);
        te_printf(", ");
        te_print_value(TE_VEC4, (te_value &) v.mat4.arr[3]);
        te_printf(")");
        break;
      default:
        te_printf("<unknown:0x%04x>", int(type));
        break;
    }
  }
}

inline const char swizzle_chars[] = "xyzw";

inline void pn(const te_expr * n, int depth) {
  if (!n) {
    te_printf("<nullptr>\n");
    return;
  }

  te_printf("%*s", depth, "");
  switch (n->opcode) {
    case TE_OP_VALUE: {
      te_print_value(n->type, n->value);
      te_printf("\n");
    }
      break;
    case TE_OP_DEREF: {
      te_printf("deref\n");
      pn(n->opargs[0], depth + 1);
    }
      break;
    case TE_OP_FUNCTION: {
      if (n->fn.param_count == 2 && n->fn.ptr == te_get_add_func(te_expr_result_type(n->fn.args[0]), te_expr_result_type(n->fn.args[1])).ptr) {
        te_printf("add\n");
      } else if (n->fn.param_count == 2 && n->fn.ptr == te_get_sub_func(te_expr_result_type(n->fn.args[0]), te_expr_result_type(n->fn.args[1])).ptr) {
        te_printf("sub\n");
      } else if (n->fn.param_count == 2 && n->fn.ptr == te_get_mul_func(te_expr_result_type(n->fn.args[0]), te_expr_result_type(n->fn.args[1])).ptr) {
        te_printf("mul\n");
      } else if (n->fn.param_count == 2 && n->fn.ptr == te_get_div_func(te_expr_result_type(n->fn.args[0]), te_expr_result_type(n->fn.args[1])).ptr) {
        te_printf("div\n");
      } else if (n->fn.param_count == 2 && n->fn.ptr == te_get_mod_func(te_expr_result_type(n->fn.args[0]), te_expr_result_type(n->fn.args[1])).ptr) {
        te_printf("mod\n");
      } else if (n->fn.param_count == 1 && n->fn.ptr == te_get_negate_func(te_expr_result_type(n->fn.args[0])).ptr) {
        te_printf("negate\n");
      } else /*if (n->fn.param_count == 1 && n->fn.ptr == te_get_dereference_func(te_expr_result_type(n->fn.args[0])).ptr) {
                te_printf("dereference\n");
            } else*/ if (n->fn.param_count == 2 && n->fn.ptr == te_get_index_func(te_expr_result_type(n->fn.args[0])).ptr) {
        te_printf("index.%ld\n", long(n->fn.args[1]->value.int_));
      } else if (n->fn.param_count == 3 && n->fn.ptr == te_get_swizzle2_func(te_expr_result_type(n->fn.args[0])).ptr) {
        te_printf("swizzle.%c%c\n", swizzle_chars[n->fn.args[1]->value.int_], swizzle_chars[(n->fn.args[2])->value.int_]);
      } else if (n->fn.param_count == 4 && n->fn.ptr == te_get_swizzle3_func(te_expr_result_type(n->fn.args[0])).ptr) {
        te_printf("swizzle.%c%c%c\n", swizzle_chars[n->fn.args[1]->value.int_], swizzle_chars[(n->fn.args[2])->value.int_], swizzle_chars[(n->fn.args[3])->value.int_]);
      } else if (n->fn.param_count == 5 && n->fn.ptr == te_get_swizzle4_func(te_expr_result_type(n->fn.args[0])).ptr) {
        te_printf("swizzle.%c%c%c%c\n", swizzle_chars[n->fn.args[1]->value.int_], swizzle_chars[(n->fn.args[2])->value.int_], swizzle_chars[(n->fn.args[3])->value.int_], swizzle_chars[(n->fn.args[4])->value.int_]);
      } else if (n->fn.ptr == (te_function) &pre_increment) {
        te_printf("pre-increment\n");
      } else if (n->fn.ptr == (te_function) &post_increment) {
        te_printf("post-increment\n");
      } else if (n->fn.ptr == (te_function) &pre_decrement) {
        te_printf("pre-decrement\n");
      } else if (n->fn.ptr == (te_function) &post_decrement) {
        te_printf("post-decrement\n");
      } else if (n->fn.ptr == (te_function) &bool_not) {
        te_printf("not\n");
      } else {
        bool found = false;
        for (int i = 0; i < te_builtins_count; ++i) {
          if (TE_IS_FUNCTION(te_builtins[i].type) && n->fn.ptr == te_builtins[i].fn.ptr) {
            found = true;
            te_printf(te_builtins[i].name);
          }
        }

        if (!found) {
          te_print_value(n->type, n->value);
        }
        te_printf("\n");
      }
      for (int i = 0; i < n->fn.param_count; i++) {
        pn(n->fn.args[i], depth + 1);
      }
    }
      break;
    case TE_OP_SUITE: {
      te_printf("suite\n");
      for (te_expr * const * it = n->opargs; *it; ++it) {
        pn(*it, depth + 1);
      }
    }
      break;
    case TE_OP_STACK_REF: {
      te_print_type_name(te_type(n->type & ~TE_CONSTANT));
      te_printf("(stack@0x%04x)", int(n->offset));
    }
      break;
    case TE_OP_ASSIGN: {
      te_printf("assign\n");
      pn(n->opargs[0], depth + 1);
      pn(n->opargs[1], depth + 1);
    }
      break;
    case TE_OP_JMP: {
      te_printf("jmp to %d\n", int(n->offset));
    }
      break;
    case TE_OP_JMP_REF: {
      te_printf("jmp to ref %d\n", int(* n->value.int_ref));
    }
      break;
    case TE_OP_JMP_IF: {
      te_printf("jmp to %d if\n", int(n->offset));
      pn(n->opargs[0], depth + 1);
    }
      break;
    case TE_OP_JMP_IF_NOT: {
      te_printf("jmp to %d if not\n", int(n->offset));
      pn(n->opargs[0], depth + 1);
    }
      break;
    case TE_OP_RETURN: {
      te_printf("return\n");
      pn(n->opargs[0], depth + 1);
    }
      break;
  }
}

inline void te_print_expr(const te_expr * n) {
  pn(n, 0);
}

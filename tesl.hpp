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
#include <cstring>
#include <cstdint>
#include <cstdio>
#include <utility>

//#define TE_DEBUG_TOKENIZER
#define TE_DEBUG_PARSER
#define TE_DEBUG_VALUE
#define TE_DEBUG_COMPILE
//#define TE_DEBUG_EVAL
#define TE_DEBUG_EXPR

#define TE_DISABLE_COMPILER_ERRORS
//#define TE_DISABLE_PRINTF

#ifdef TE_DISABLE_PRINTF
#define te_printf(...)
#else
//#include "printf.h"
//#define te_printf printf_
#define te_printf printf
#endif

// This is used to clearly mark flexible-sized arrays that appear at the end of
// some dynamically-allocated structs, known as the "struct hack".
#define FLEXIBLE_ARRAY 0

#define TE_ALWAYS_INLINE inline __attribute__((always_inline))

#define TE_MAX_VAR_COUNT 128
#define TE_MAX_STMT_COUNT 1024

enum te_type : uint8_t {
  TE_VARIANT = 0,
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
  TE_ERROR = TE_TYPE_COUNT,

  TE_CONSTANT = 1ul << 7,

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
  TE_OP_NONE,
  TE_OP_ERROR,
  TE_OP_VALUE,
  TE_OP_STACK_REF,
  TE_OP_STACK_REF_REF,
  TE_OP_DEREF,
  TE_OP_ASSIGN,
  TE_OP_CALL,
  TE_OP_SUITE,
  TE_OP_JMP,
  TE_OP_JMP_REF,
  TE_OP_JMP_IF,
  TE_OP_JMP_IF_NOT,
  TE_OP_RETURN,
};

//static_assert(TE_TYPE_COUNT <= TE_TYPE_COUNT_MAX);

#define TE_IS_CONSTANT(TYPE) (((TYPE) & (TE_CONSTANT)) && TYPE != TE_FUNCTION)
#define TE_IS_REF(TYPE) (!((TYPE) & (TE_CONSTANT)) && TYPE != TE_FUNCTION)

static_assert(sizeof(float) == 4);
using te_float = float;
using te_int = int32_t;

struct te_vec2 {
  union {
    te_float elements[2] = {0.0f};
    te_float arr[2];
    struct { te_float x, y; };
    struct { te_float r, g; };
  };
};

struct te_vec3 {
  union {
    te_float elements[3] = {0.0f};
    te_float arr[3];
    struct { te_float x, y, z; };
    struct { te_float r, g, b; };
  };
};

struct te_vec4 {
  union {
    te_float elements[4] = {0.0f};
    te_float arr[4];
    struct { te_float x, y, z, w; };
    struct { te_float r, g, b, a; };
  };
};

union te_mat2 {
  te_float elements[2 * 2];
  te_vec2 arr[2]{};
};

union te_mat3 {
  te_float elements[3 * 3];
  te_vec3 arr[3]{};
};

union te_mat4 {
  te_float elements[4 * 4];
  te_vec4 arr[4]{};
};

constexpr int constexpr_strlen_impl(const char *str) {
  return *str ? 1 + constexpr_strlen_impl(str + 1) : 0;
}

TE_ALWAYS_INLINE constexpr int constexpr_strlen(const char * e) {
  if (std::is_constant_evaluated()) {
    return constexpr_strlen_impl(e);
  } else {
    return strlen(e);
  }
}

struct te_strview {
  const char * ptr = "";
  const char * end = ptr;

  TE_ALWAYS_INLINE bool operator==(const te_strview &other) const {
    return len() == other.len() && memcmp(ptr, other.ptr, len()) == 0;
  }

  TE_ALWAYS_INLINE te_int len() const {
    return end - ptr;
  }

  TE_ALWAYS_INLINE constexpr te_strview &operator=(const te_strview &) = default;
  TE_ALWAYS_INLINE constexpr te_strview &operator=(te_strview &&) = default;
  TE_ALWAYS_INLINE constexpr te_strview(const te_strview &) = default;
  TE_ALWAYS_INLINE constexpr te_strview(te_strview &&) = default;

  TE_ALWAYS_INLINE constexpr te_strview(const char * s, const char * e) : ptr(s), end(e) {}
  TE_ALWAYS_INLINE constexpr te_strview(const char * s, int len) : ptr(s), end(s + len) {}
  TE_ALWAYS_INLINE constexpr te_strview(const char * str) : ptr(str), end(str + constexpr_strlen(str)) {}
  TE_ALWAYS_INLINE constexpr te_strview() {}
};

//static_assert(sizeof(void *) == 4, "made for 32bit :/ srry didnt have the energy to um yea");

inline void te_noop(void * context, void * args, void * ret) {}

typedef void (* te_function)(void * context, void * args, void * ret);

struct te_expr;

struct te_fn_obj_base {
  te_function ptr = nullptr;
  void * context = nullptr;
};

//constexpr int TE_PARAM_COUNT_MAX = 16 + (((te_fn_obj0_size - 1) / sizeof(void *) + 1) * sizeof(void *)) - te_fn_obj0_size;
constexpr int te_fn_obj_size_pre = sizeof(te_fn_obj_base) + sizeof(te_type) + sizeof(bool) + sizeof(uint8_t);
constexpr int te_fn_obj_size_post = 16 + (((te_fn_obj_size_pre - 1) / sizeof(void *) + 1) * sizeof(void *));
constexpr int TE_PARAM_COUNT_MAX = te_fn_obj_size_post - te_fn_obj_size_pre;
static_assert(TE_PARAM_COUNT_MAX == 21);

struct te_fn_obj : te_fn_obj_base {
  te_type return_type = TE_ERROR;
  bool pure = true;
  uint8_t param_count = 0;
  te_type param_types[TE_PARAM_COUNT_MAX]{TE_ERROR};

  [[nodiscard]] bool is_valid() const {
    return ptr != nullptr;
  }
};
static constexpr int te_fn_obj_size = sizeof(te_fn_obj);
static_assert(sizeof(te_fn_obj) == te_fn_obj_size_post);

struct te_variant;

struct te_value {
  union {
    te_fn_obj fn;
    te_fn_obj *fn_ref;

    struct { te_float x, y, z, w; };
    struct { te_float r, g, b, a; };

    te_float float_;
    te_vec2 vec2;
    te_vec3 vec3;
    te_vec4 vec4;
    te_int int_;
    te_mat2 mat2;
    te_mat3 mat3;
    te_mat4 mat4;
    struct {
      const char * str;
      char str_storage[64 - sizeof(const char *)];
    };

    te_float elements[16];

    void * ptr;
    te_value * ref;
    te_float * float_ref;
    te_vec2 * vec2_ref;
    te_vec3 * vec3_ref;
    te_vec4 * vec4_ref;
    te_int * int_ref;
    te_mat2 * mat2_ref;
    te_mat3 * mat3_ref;
    te_mat4 * mat4_ref;
    const char ** str_ref;
  };

  constexpr te_value() : elements{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} {}
  constexpr te_value(te_fn_obj p_fn) : fn(p_fn) {};
  constexpr te_value(te_float p_float) : float_(p_float) {};
  constexpr te_value(te_int p_int) : int_(p_int) {};
  constexpr te_value(te_vec2 p_vec) : vec2(p_vec) {};
  constexpr te_value(te_vec3 p_vec) : vec3(p_vec) {};
  constexpr te_value(te_vec4 p_vec) : vec4(p_vec) {};
  constexpr te_value(te_mat2 p_vec) : mat2(p_vec) {};
  constexpr te_value(te_mat3 p_vec) : mat3(p_vec) {};
  constexpr te_value(te_mat4 p_vec) : mat4(p_vec) {};
  constexpr te_value(const char * p_str) : str(p_str) {};
  constexpr te_value(const char ** p_str_ref) : str_ref(p_str_ref) {};

  constexpr te_value(void * p_ptr) : ptr(p_ptr) {};
  constexpr te_value(te_value * p_ref) : ref(p_ref) {};
};
static_assert(sizeof(te_value) == 64);

struct te_typed_value : public te_value {
  te_type type = TE_ERROR;
};

struct te_variable {
  te_strview name = "<unnamed>";
  te_type type = TE_ERROR;
  union {
    te_value value;
    te_fn_obj fn;
  };

  te_strview get_name() const { return name; }

  constexpr te_variable(te_strview p_name, te_typed_value v) : name(p_name), type(v.type), value(v) {}
  constexpr te_variable(te_strview p_name, te_type t, te_value v) : name(p_name), type(t), value(v) {}
  constexpr te_variable(te_strview p_name, te_fn_obj p_func) : name(p_name), type(TE_FUNCTION), fn(p_func) {}
};

// DO NOT STATICALLY ALLOCATE THESE
struct te_op {
  te_opcode opcode = TE_OP_NONE;
};
static_assert(sizeof(te_op) == 1);

struct te_expr : te_op {
  te_type type = TE_ERROR;
};
static_assert(sizeof(te_expr) == 2);

struct te_error_expr : te_expr {
  uint16_t source_count = 0;
  te_op * sources[FLEXIBLE_ARRAY];
};
static_assert(sizeof(te_error_expr) == sizeof(void *));

struct te_value_expr : public te_expr {
  uint16_t size = 0;
  te_value value;
};
static_assert(sizeof(te_value_expr) == sizeof(te_value) + sizeof(void *));

struct te_stack_ref_expr : te_expr {
  uint16_t offset = 0;
};
static_assert(sizeof(te_stack_ref_expr) == 4);

struct te_deref_expr : te_expr {
  uint16_t size = 0;
  te_expr *arg = nullptr;
};
static_assert(sizeof(te_deref_expr) == sizeof(void *) * 2);

struct te_assign_expr : te_expr {
  te_expr *lhs = nullptr;
  te_expr *rhs = nullptr;
};
static_assert(sizeof(te_assign_expr) == sizeof(void *) * 3);

struct te_call_expr : te_expr {
  uint16_t arg_stack_size = 0;
  te_fn_obj fn;
  te_expr *args[FLEXIBLE_ARRAY];
};
static_assert(sizeof(te_call_expr) == sizeof(void *) * 1 + sizeof(te_fn_obj));

struct te_suite_expr : te_expr {
  uint16_t stack_size = 0;
  uint16_t stmt_count = 0;
  te_expr *stmts[FLEXIBLE_ARRAY];
};
static_assert(sizeof(te_suite_expr) == 8);

struct te_jmp_op : public te_op {
  static constexpr uint16_t unknown_offset = 0xffffu;
  uint16_t offset = unknown_offset;
};
static_assert(sizeof(te_jmp_op) == 4);

struct te_jmp_ref_op : public te_op {
  uint16_t *offset_ref = nullptr;
};
static_assert(sizeof(te_jmp_ref_op) == sizeof(void *) * 2);

struct te_jmp_if_op : public te_jmp_op {
  te_expr *condition = nullptr;
};
static_assert(sizeof(te_jmp_if_op) == sizeof(void *) * 2);

struct te_jmp_if_not_op : public te_jmp_if_op {};
static_assert(sizeof(te_jmp_if_not_op) == sizeof(void *) * 2);

struct te_return_op : public te_op {
  te_expr *arg = nullptr;
};
static_assert(sizeof(te_return_op) == sizeof(void *) * 2);

struct te_parser_state;

struct te_error_record {
private:
  te_parser_state * state = nullptr;
  te_error_record * prev = nullptr;

public:
  const char * line_start = nullptr;
  const char * start = nullptr;
  const char * point = nullptr;
  const char * end = nullptr;
  te_int line_num = 0;

  explicit te_error_record(te_parser_state & s);
  te_error_record() = default;
  ~te_error_record();
  inline int get_line() { return line_num; }
  inline int get_column() { return point - line_start; }
  void reset();
  inline void set_start(const char * p_start) { start = p_start; }
  inline void set_point(const char * p_point) { point = p_point; }
  inline void set_end(const char * p_end) { end = p_end; }
  void print() const;
};

/* Frees the expression. */
/* This is safe to call on null pointers. */
void te_free(te_op * op);

/* Evaluates the expression. */
te_typed_value te_eval(const te_expr * e);

struct te_program {
  te_expr *root_expr = nullptr;
  te_error_record error;

  bool is_valid() const { return root_expr != nullptr; }
  te_typed_value eval() const { return te_eval(root_expr); }
  void optimize() const;

  te_program(te_expr * expr, const te_error_record & er);
  te_program & operator=(te_program &&);
  te_program(te_program && other);
  te_program & operator=(const te_program &) = delete;
  te_program(const te_program &) = delete;
  te_program() = default;
  ~te_program();
};

namespace te {
  template<typename T>
  T && move(T & v) { return static_cast<T &&>(v); }

  template<typename T1, typename T2> constexpr bool is_same = false;
  template<typename T> constexpr bool is_same<T, T> = true;

  template<typename T> inline constexpr te_type type_value_of = TE_ERROR;
  template<typename T> inline constexpr te_type type_value_of<T *> = te_type(type_value_of<T> & (~TE_CONSTANT));
  template<> inline constexpr te_type type_value_of<void> = TE_NULL;
  template<> inline constexpr te_type type_value_of<te_float> = TE_FLOAT;
  template<> inline constexpr te_type type_value_of<te_vec2> = TE_VEC2;
  template<> inline constexpr te_type type_value_of<te_vec3> = TE_VEC3;
  template<> inline constexpr te_type type_value_of<te_vec4> = TE_VEC4;
  template<> inline constexpr te_type type_value_of<te_int> = TE_INT;
  template<> inline constexpr te_type type_value_of<te_mat2> = TE_MAT2;
  template<> inline constexpr te_type type_value_of<te_mat3> = TE_MAT3;
  template<> inline constexpr te_type type_value_of<te_mat4> = TE_MAT4;
  template<> inline constexpr te_type type_value_of<const char *> = TE_STR;

  template<te_type T>
  struct type_of_impl { using type = typename type_of_impl<te_type(T | TE_CONSTANT)>::type *; };
  template<>
  struct type_of_impl<TE_NULL> { using type = void; };
  template<>
  struct type_of_impl<TE_FLOAT> { using type = te_float; };
  template<>
  struct type_of_impl<TE_VEC2> { using type = te_vec2; };
  template<>
  struct type_of_impl<TE_VEC3> { using type = te_vec3; };
  template<>
  struct type_of_impl<TE_VEC4> { using type = te_vec4; };
  template<>
  struct type_of_impl<TE_INT> { using type = te_int; };
  template<>
  struct type_of_impl<TE_MAT2> { using type = te_mat2; };
  template<>
  struct type_of_impl<TE_MAT3> { using type = te_mat3; };
  template<>
  struct type_of_impl<TE_MAT4> { using type = te_mat4; };
  template<>
  struct type_of_impl<TE_STR> { using type = const char *; };

  template<>
  struct type_of_impl<TE_FUNCTION> { using type = te_fn_obj; };

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
  TE_ALWAYS_INLINE void value_get<void>(const te_value & val) {}

  template<te_type Type>
  TE_ALWAYS_INLINE type_of<te_type(Type | TE_CONSTANT)> value_deref(const te_value & val);

  template<typename T>
  TE_ALWAYS_INLINE constexpr void value_set(te_value & tev, const T & v);

  template<typename T>
  TE_ALWAYS_INLINE constexpr te_typed_value make_value(const T & v);

#define MAKE_VALUE_IMPL(tetype, member_name, ref_tetype, ref_member_name)\
  MAKE_VALUE_CONST_IMPL(tetype, member_name)\
  MAKE_VALUE_REF_IMPL(ref_tetype, ref_member_name)
#define MAKE_VALUE_CONST_IMPL(tetype, member_name)\
  template<> TE_ALWAYS_INLINE constexpr type_of<tetype> value_get<type_of<tetype>>(const te_value &val) { return val.member_name; }\
  template<> TE_ALWAYS_INLINE constexpr type_of<tetype> value_deref<tetype>(const te_value &val) { return val.member_name; }\
  template<> TE_ALWAYS_INLINE constexpr void value_set<type_of<tetype>>(te_value &tev, const type_of<tetype> & v) { tev.member_name = v; }\
  template<> TE_ALWAYS_INLINE constexpr te_typed_value make_value<type_of<tetype>>(const type_of<tetype> & v) { return {te_value{v}, tetype}; }
#define MAKE_VALUE_REF_IMPL(ref_tetype, ref_member_name)\
  template<> TE_ALWAYS_INLINE constexpr type_of<ref_tetype> value_get<type_of<ref_tetype>>(const te_value &val) { return val.ref_member_name; }\
  template<> TE_ALWAYS_INLINE constexpr type_of<te_type(ref_tetype | TE_CONSTANT)> value_deref<ref_tetype>(const te_value &val) { return *val.ref_member_name; }\
  template<> TE_ALWAYS_INLINE constexpr void value_set<type_of<ref_tetype>>(te_value &tev, const type_of<ref_tetype> & v) { tev.ref_member_name = v; }\
  template<> TE_ALWAYS_INLINE constexpr te_typed_value make_value<type_of<ref_tetype>>(const type_of<ref_tetype> & v) { return {te_value{v}, ref_tetype}; }

  //MAKE_VALUE_CONST_IMPL(TE_INT, int_)
  //MAKE_VALUE_REF_IMPL(TE_INT_REF, int_ref)
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

  namespace detail {
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
      TE_ALWAYS_INLINE static constexpr int call(int in) {
        return offset_of0<I - 1, I == 1, Next...>::call(in + sizeof(T));
      }
    };
    template<typename ... Next>
    struct offset_of0<0, true, Next...> {
      TE_ALWAYS_INLINE static constexpr int call(int in) { return in; }
    };
    template<int I, typename ... Next> constexpr int offset_of = offset_of0<I, I == 0, Next...>::call(0);

    template<typename ISequence, auto Fn, typename R, typename ... Ps>
    struct function0;

    template<int ... Is, auto Fn, typename R, typename ... Ps>
    struct function0<index_sequence<Is...>, Fn, R, Ps...> {
      static void call(void *, void * args, void * ret) {
        *reinterpret_cast<R *>(ret) = Fn(*reinterpret_cast<Ps *>(reinterpret_cast<char *>(args) + offset_of < Is, Ps... >)...);
      }
    };

    template<auto Fn, typename R, typename ... Ps>
    struct function : public function0<index_sequence_helper<sizeof...(Ps)>, Fn, R, Ps...> {};

    TE_ALWAYS_INLINE constexpr te_fn_obj make_function_raw(te_function fn, void * context, bool pure, te_type return_type, const te_type * param_types, int param_count) {
      te_fn_obj ret;
      ret.ptr = fn;
      ret.context = context;
      ret.return_type = return_type;
      ret.pure = pure;
      ret.param_count = param_count;

      for (int i = 0; i < param_count; ++i) {
        if (i >= TE_PARAM_COUNT_MAX) {
          te_printf("error: too many parameters!");
          break;
        }
        ret.param_types[i] = param_types[i];
      }
      return ret;
    };

    template<typename T, int N>
    TE_ALWAYS_INLINE constexpr te_fn_obj make_function_raw(te_function fn, void * context, bool pure, te_type return_type, const T (&param_types)[N]) {
      return make_function_raw(fn, context, pure, return_type, param_types, N);
    }

    template<auto Fn, bool Pure, typename R, typename ... Ps>
    struct make_function_impl {
      TE_ALWAYS_INLINE static constexpr te_fn_obj call() {
        static_assert(sizeof...(Ps) <= TE_PARAM_COUNT_MAX, "too many parameters!");
        return make_function_raw(function<Fn, R, Ps...>::call, nullptr, Pure, type_value_of<R>, (te_type[]){type_value_of<Ps>...});
      }
    };

    template<auto Fn, bool Pure>
    struct make_function;

    template<typename R, bool Pure, typename ... Ps, R (* Fn)(Ps...)>
    struct make_function<Fn, Pure> {
      TE_ALWAYS_INLINE static constexpr te_fn_obj call() {
        return make_function_impl<Fn, Pure, R, Ps...>::call();
      }
    };
  }

  template<auto Fn>
  TE_ALWAYS_INLINE constexpr te_fn_obj make_function() {
    return detail::make_function<Fn, false>::call();
  }

  template<auto Fn>
  TE_ALWAYS_INLINE constexpr te_fn_obj make_pure_function() {
    return detail::make_function<Fn, true>::call();
  }
}

inline constexpr int8_t te_size_of(te_type type) {
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
    case TE_STR:
      te_printf("internal error: cannot get size of dynamic type str");
      return 0;
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
    case TE_NULL:
    case TE_ERROR:
      return 0;
    default:
      return sizeof(te_value);
  }
#undef CASE_
}

/* Parses the input expression, evaluates it, and frees it. */
/* Result type is TE_ERROR on error. */
te_typed_value te_interp(const char * expression, te_error_record * error = nullptr);

/* Parses the input as a program and binds variables. */
te_program te_compile_program(const char * expression, te_variable * variables, int var_count);

/* Parses the input as a suite and binds variables. */
te_program te_compile_suite(const char * expression, te_variable * variables, int var_count, te_type result_type);

/* Parses the input as an expression and binds variables. */
te_program te_compile_expr(const char * expression, te_variable * variables, int var_count);

/* Prints debugging information on the syntax tree. */
void te_print_expr(const te_expr * n);

void te_print(const te_typed_value & v);
void te_print(te_type type);

#endif /*TESL_HPP*/

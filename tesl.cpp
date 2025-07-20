#include "tesl.hpp"
#include "tesl_common.hpp"
#include "tesl_var.hpp"
#include "tesl_parser.hpp"

#ifndef __INT8_TYPE__
#define __INT8_TYPE__
#endif

#ifndef __INT16_TYPE__
#define __INT16_TYPE__
#endif

#include <cassert>
#include <cctype>
#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <malloc.h>

#ifdef __linux__
#include <alloca.h>
#elif _WIN32
#include <malloc.h>
#endif

using namespace tesl;

te_program::te_program(te_expr * expr, const ErrorRecord & er) : root_expr(expr), error(er) {}

te_program::te_program(te_program && other) : root_expr(other.root_expr), error(MOV(other.error)) {
  other.root_expr = nullptr;
}

te_program & te_program::operator=(te_program && other) {
  root_expr = other.root_expr;
  error = static_cast<ErrorRecord &&>(other.error);
  other.root_expr = nullptr;
  return *this;
}

te_program::~te_program() {
  te_free(root_expr);
}

template<typename ... Ts>
static void _te_value_error(Ts && ... vs) {
  (te_print(vs), ...);
  te_print("\n");
}

#ifdef TESL_DEBUG_VALUE
#define te_value_error(...) _te_value_error(__VA_ARGS__)
#else
#define te_value_error(...) _te_value_error()
#endif

static void te_make_error(te_parser_state & s) {
  s.parse_error = true;
  s.print_error_location();
}


void ErrorRecord::print() const {
  tesl_print_error(line_num, line_start, start, point, end);
}

te_parser_state::local_var_t::local_var_t(te_parser_state & s, StrView name, Type p_type, uint16_t p_offset) {
  type = p_type;
  offset = p_offset;

  int32_t len = name.len();
  if (len > 0xff) {
    te_compile_error(s, "error: var name '", StrView{name.ptr, 24}, "...' too long! (max length: ", 0xff, ")");
    return;
  } else if (len < 0) {
    te_compile_error(s, "internal error: invalid var name length: ", len);
    return;
  }

  name_ptr = name.ptr;
  name_length = len;
}

te_parser_state::local_var_t *te_stack_allocate_var(te_parser_state & s, StrView str, Type type) {
  int8_t var_size = sizeof_type(type);
  uint16_t offset = s.stack_offset;
  te_parser_state::local_var_t &var = s.vars[s.var_count];
  var = {s, str, type, offset};
  s.stack_offset += var_size;
  s.stack_size = MAX(s.stack_size, s.stack_offset);
  s.var_count++;
  return &var;
}

int32_t te_begin_scope(te_parser_state & s) {
  return s.stack_offset;
}

void te_end_scope(te_parser_state & s, int32_t prev_stack_size) {
  while (s.stack_offset != prev_stack_size) {
    s.var_count--;
    s.stack_offset = s.vars[s.var_count].offset;
    if (s.stack_offset < prev_stack_size) {
      ErrorRecord er{s};
      te_compile_error(s, "internal error: stack var deallocation mismatch!");
      return;
    }
  }
}

struct te_scope_decl {
  te_parser_state & state;
  int32_t prev_stack_size;
  te_scope_decl(te_parser_state & s) : state(s), prev_stack_size(te_begin_scope(s)) {}
  ~te_scope_decl() { te_end_scope(state, prev_stack_size); }
};

constexpr int te_size_of_op(const te_op * op) {
  if (!op) {
    return -1;
  }

  switch (op->opcode) {
    case TE_OP_NONE:
      return sizeof(te_op);
    case TE_OP_ERROR:
      return sizeof(te_error_expr) + sizeof(te_expr *) * reinterpret_cast<const te_error_expr *>(op)->source_count;
    case TE_OP_VALUE:
      return sizeof(te_value_expr) - sizeof(te_value) + reinterpret_cast<const te_value_expr *>(op)->size;
    case TE_OP_STACK_REF:
    case TE_OP_STACK_REF_REF:
      return sizeof(te_stack_ref_expr);
    case TE_OP_DEREF:
      return sizeof(te_deref_expr);
    case TE_OP_ASSIGN:
      return sizeof(te_assign_expr);
    case TE_OP_CALL:
      return sizeof(te_call_expr) + sizeof(te_expr *) * reinterpret_cast<const te_call_expr *>(op)->fn.param_count;
    case TE_OP_SUITE:
      return sizeof(te_suite_expr) + sizeof(te_expr *) * reinterpret_cast<const te_suite_expr *>(op)->stmt_count;
    case TE_OP_JMP:
      return sizeof(te_jmp_op);
    case TE_OP_JMP_REF:
      return sizeof(te_jmp_ref_op);
    case TE_OP_JMP_IF:
      return sizeof(te_jmp_if_op);
    case TE_OP_JMP_IF_NOT:
      return sizeof(te_jmp_if_not_op);
    case TE_OP_RETURN:
      return sizeof(te_return_op);
  }
  
  tesl_printf("internal error: invalid opcode %02x", op->opcode);
  return -1;
}

constexpr int te_is_expr(const te_op * op) {
  if (!op) {
    return false;
  }

  switch (op->opcode) {
    case TE_OP_ERROR:
    case TE_OP_VALUE:
    case TE_OP_STACK_REF:
    case TE_OP_STACK_REF_REF:
    case TE_OP_DEREF:
    case TE_OP_ASSIGN:
    case TE_OP_CALL:
    case TE_OP_SUITE:
      return true;
    case TE_OP_NONE:
    case TE_OP_JMP:
    case TE_OP_JMP_REF:
    case TE_OP_JMP_IF:
    case TE_OP_JMP_IF_NOT:
    case TE_OP_RETURN:
      return false;
  }
  
  tesl_printf("internal error: invalid opcode %02x", op->opcode);
  return -1;
}

static te_expr * new_error_expr_v(te_op * const * sources, int source_count) {
  int sources_size = sizeof(te_op *) * source_count;

  const int size = sizeof(te_error_expr) + sources_size;
  te_error_expr * ret = static_cast<te_error_expr *>(malloc(size));
  ret->opcode = TE_OP_ERROR;
  ret->type = TYPE_ERROR;
  ret->source_count = source_count;
  memcpy(ret->sources, sources, sources_size);
  return ret;
}

template <typename ... Ts>
static te_expr * new_error_expr(Ts && ... args) {
  te_op * sources[] = {args...};
  return new_error_expr_v(sources, sizeof...(Ts));
}

static te_expr * new_error_expr() {
  return new_error_expr_v(nullptr, 0);
}

static te_expr * new_value_expr(Type type, const te_value & value) {
  const int value_size = sizeof_type(type);
  const int size = sizeof(te_value_expr) - sizeof(te_value) + value_size;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = type;
  ret->size = value_size;
  memcpy(&ret->value, &value, value_size);
  return ret;
}

static te_expr * new_value_ref_expr(Type type, te_value & value) {
  Type ref_type = Type(type & ~TYPE_FLAG_CONSTANT);
  const int value_size = sizeof_type(ref_type);
  const int size = sizeof(te_value_expr) - sizeof(te_value) + value_size;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = ref_type;
  ret->size = value_size;
  ret->value.ref = &value;
  return ret;
}

static te_expr * new_int_literal_expr(const IntT & value) {
  const Type type = TYPE_INT_VAL;
  const int value_size = sizeof_type(type);
  const int size = sizeof(te_value_expr) - sizeof(te_value) + value_size;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = type;
  ret->size = value_size;
  memcpy(&ret->value, &value, value_size);
  return ret;
}

static te_expr * new_float_literal_expr(const FloatT & value) {
  const Type type = TYPE_FLOAT_VAL;
  const int value_size = sizeof_type(type);
  const int size = sizeof(te_value_expr) - sizeof(te_value) + value_size;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = type;
  ret->size = value_size;
  memcpy(&ret->value, &value, value_size);
  return ret;
}

static te_expr * new_str_literal_expr(const StrView & str) {
  // Special dynamically allocated te_value that can be longer than 64 bytes int.
  // TODO: actually parse the literal here ...
  const int length = str.len();
  const int size = sizeof(te_value_expr) - sizeof(te_value) + sizeof(const char *) + length + 1;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = TYPE_STR_VAL;
  ret->size = length + 1;
  ret->value.str = ret->value.str_storage;
  memcpy(ret->value.str_storage, str.ptr, length);
  ret->value.str_storage[length] = 0;
  return ret;
}

static te_expr * new_stack_ref_expr(Type source_type, uint16_t offset) {
  Type result_type = Type(source_type & ~TYPE_FLAG_CONSTANT);
  const int size = sizeof(te_stack_ref_expr);
  te_stack_ref_expr * ret = static_cast<te_stack_ref_expr *>(malloc(size));
  ret->opcode = is_ref(source_type) ? TE_OP_STACK_REF_REF : TE_OP_STACK_REF;
  ret->type = result_type;
  ret->offset = offset;
  return ret;
}

static te_expr * new_deref_expr(Type target_type, te_expr * e) {
  if (e == nullptr) {
    return nullptr;
  }

  Type dereferenced_type = Type(e->type | TYPE_FLAG_CONSTANT);
  if (e->type == target_type || e->type == TYPE_ERROR) {
    return e;
  } else if (dereferenced_type != target_type || target_type == TYPE_STR_VAL) {
    te_value_error("internal error: cannot deref ", e->type, " to ", target_type);
    return new_error_expr(e);
  }

  const int value_size = sizeof_type(dereferenced_type);
  const int size = sizeof(te_deref_expr);
  te_deref_expr * ret = static_cast<te_deref_expr *>(malloc(size));
  ret->opcode = TE_OP_DEREF;
  ret->type = dereferenced_type;
  ret->size = value_size;
  ret->arg = e;
  return ret;
}

static te_expr * new_assign_expr(te_expr * lhs, te_expr * rhs) {
  Type lhs_type = lhs ? lhs->type : TYPE_ERROR;
  const int size = sizeof(te_assign_expr);
  te_assign_expr * ret = static_cast<te_assign_expr *>(malloc(size));
  ret->opcode = TE_OP_ASSIGN;
  ret->type = lhs_type;
  ret->lhs = lhs;
  ret->rhs = new_deref_expr(Type(lhs_type | TYPE_FLAG_CONSTANT), rhs);

  if (lhs_type != TYPE_ERROR && !tesl::is_ref(lhs_type)) {
    te_value_error("error: cannot assign to constant ", lhs_type);
    return new_error_expr(ret);
  }
  return ret;
}

static te_jmp_op * new_jmp_op(uint16_t offset) {
  const int size = sizeof(te_jmp_op);
  te_jmp_op * ret = static_cast<te_jmp_op *>(malloc(size));
  ret->opcode = TE_OP_JMP;
  ret->offset = offset;
  return ret;
}

static te_jmp_ref_op * new_jmp_ref_op(uint16_t * offset_ptr) {
  const int size = sizeof(te_jmp_ref_op);
  te_jmp_ref_op * ret = static_cast<te_jmp_ref_op *>(malloc(size));
  ret->opcode = TE_OP_JMP_REF;
  ret->offset_ref = offset_ptr;
  return ret;
}

static te_jmp_if_op * new_jmp_if_op(uint16_t offset, te_expr * e) {
  e = new_deref_expr(TYPE_INT_VAL, e);
  if (e == nullptr) {
    return nullptr;
  }
  const int size = sizeof(te_jmp_if_op);
  te_jmp_if_op * ret = static_cast<te_jmp_if_op *>(malloc(size));
  ret->opcode = TE_OP_JMP_IF;
  ret->offset = offset;
  ret->condition = e;
  return ret;
}

static te_jmp_if_not_op * new_jmp_if_not_op(uint16_t offset, te_expr * e) {
  e = new_deref_expr(TYPE_INT_VAL, e);
  if (e == nullptr) {
    return nullptr;
  }
  const int size = sizeof(te_jmp_if_not_op);
  te_jmp_if_not_op * ret = static_cast<te_jmp_if_not_op *>(malloc(size));
  ret->opcode = TE_OP_JMP_IF_NOT;
  ret->offset = offset;
  ret->condition = e;
  return ret;
}

static te_return_op * new_return_op(Type return_type, te_expr * e) {
  e = new_deref_expr(return_type, e);
  if (e == nullptr) {
    return nullptr;
  }
  const int size = sizeof(te_return_op);
  te_return_op * ret = static_cast<te_return_op *>(malloc(size));
  ret->opcode = TE_OP_RETURN;
  ret->arg = e;
  return ret;
}

static te_expr * new_call_expr_v(const FnObj &fn, te_expr * const * args, const int arg_count) {
  const int args_size = sizeof(te_expr *) * arg_count;
  const int size = sizeof(te_call_expr) + args_size;
  te_call_expr * ret = static_cast<te_call_expr *>(malloc(size));
  ret->opcode = TE_OP_CALL;
  ret->type = fn.return_type;
  ret->arg_stack_size = 0;
  ret->fn = {};

  if (fn.param_count != arg_count) {
    te_value_error("internal error: expected ", fn.param_count, " args, got ", arg_count);
    return new_error_expr(ret);
  }

  if (arg_count > TE_PARAM_COUNT_MAX) {
    te_value_error("error: ", arg_count, " is too many parameters! max parameters: ", TE_PARAM_COUNT_MAX);
    return new_error_expr(ret);
  }

  for (int i = 0; i < arg_count; ++i) {
    if (args[i] == nullptr) {
      te_value_error("internal error: arg %d is null!\n", i);
      return new_error_expr(ret);
    }
  }

  for (int i = 0; i < arg_count; ++i) {
    ret->arg_stack_size += sizeof_type(fn.param_types[i]);
  }
  ret->fn = fn;

  for (int i = 0; i < arg_count; ++i) {
    ret->args[i] = new_deref_expr(fn.param_types[i], args[i]);
  }

  return ret;
}

template<typename ... Ts>
static te_expr * new_call_expr(const FnObj &fn, Ts && ... vs) {
  te_expr * args[] = {vs...};
  return new_call_expr_v(fn, args, sizeof...(Ts));
}

static te_expr * new_suite_expr(Type return_type, uint16_t stack_size, te_op * stmts[], const int stmt_count) {
  const int size = sizeof(te_suite_expr) + sizeof(te_expr *) * stmt_count;
  te_suite_expr * ret = static_cast<te_suite_expr *>(malloc(size));
  memset(ret, 0, size);
  ret->opcode = TE_OP_SUITE;
  ret->type = return_type;
  ret->stack_size = stack_size;
  ret->stmt_count = stmt_count;
  memcpy(ret->stmts, stmts, stmt_count * sizeof(te_expr *));
  return ret;
}

te_expr * te_parser_state::var_ref::new_ref_expr(te_parser_state & s) {
  switch (kind) {
    case INVALID:
      return nullptr;
    case LOCAL_VAR:
      return new_stack_ref_expr(local_var->type, local_var->offset);
    case GLOBAL_CONST:
      return new_value_expr(global_const->type, global_const->value);
    case GLOBAL_VAR:
      return new_value_ref_expr(global_var->type, global_var->value);
  }

  return nullptr;
}

void te_free_args(te_op * n) {
  if (!n) return;
  switch (n->opcode) {
    case TE_OP_NONE:
      break;
    case TE_OP_ERROR: {
      te_error_expr *ee = reinterpret_cast<te_error_expr *>(n);
      for (int i = ee->source_count - 1; i >= 0; --i) {
        te_free(ee->sources[i]);
      }
    } break;
    case TE_OP_VALUE:
    case TE_OP_STACK_REF:
    case TE_OP_STACK_REF_REF:
      break;
    case TE_OP_DEREF: {
      te_free(reinterpret_cast<te_deref_expr *>(n)->arg);
    } break;
    case TE_OP_ASSIGN: {
      te_free(reinterpret_cast<te_assign_expr *>(n)->rhs);
      te_free(reinterpret_cast<te_assign_expr *>(n)->lhs);
    } break;
    case TE_OP_CALL: {
      te_call_expr *ce = reinterpret_cast<te_call_expr *>(n);
      for (int i = int(ce->fn.param_count) - 1; i >= 0; --i) {
        te_free(ce->args[i]);
      }
    } break;
    case TE_OP_SUITE: {
      te_suite_expr *se = reinterpret_cast<te_suite_expr *>(n);
      for (int i = int(se->stmt_count) - 1; i >= 0; --i) {
        te_free(se->stmts[i]);
      }
    } break;
    case TE_OP_JMP:
    case TE_OP_JMP_REF:
      break;
    case TE_OP_JMP_IF: {
      te_free(reinterpret_cast<te_jmp_if_op *>(n)->condition);
    } break;
    case TE_OP_JMP_IF_NOT: {
      te_free(reinterpret_cast<te_jmp_if_not_op *>(n)->condition);
    } break;
    case TE_OP_RETURN: {
      te_free(reinterpret_cast<te_return_op *>(n)->arg);
    } break;
  }
}

void te_free(te_op * op) {
  if (op) {
    int op_size = te_size_of_op(op);
    te_op * local_op = reinterpret_cast<te_op *>(alloca(op_size));
    memcpy(local_op, op, op_size);
    free(op);
    op = nullptr;
    te_free_args(local_op);
  }
}

namespace te {
  // template<Type RefType>
  // type_of<Type(RefType | TYPE_FLAG_CONSTANT)> dereference_value(type_of<RefType> v) {
  //     return *v;
  // }

  template<Type Ty>
  constexpr int element_count = 0;

  template<> inline constexpr int element_count<TYPE_INT_VAL> = 1;
  template<> inline constexpr int element_count<TYPE_FLOAT_VAL> = 1;
  template<> inline constexpr int element_count<TYPE_VEC2_VAL> = 2;
  template<> inline constexpr int element_count<TYPE_VEC3_VAL> = 3;
  template<> inline constexpr int element_count<TYPE_VEC4_VAL> = 4;
  template<> inline constexpr int element_count<TYPE_MAT2_VAL> = 4;
  template<> inline constexpr int element_count<TYPE_MAT3_VAL> = 9;
  template<> inline constexpr int element_count<TYPE_MAT4_VAL> = 16;

  template<Type Ty>
  constexpr Type element_tetype = TYPE_ERROR;

  template<> inline constexpr Type element_tetype<TYPE_VEC2_VAL> = TYPE_FLOAT_VAL;
  template<> inline constexpr Type element_tetype<TYPE_VEC3_VAL> = TYPE_FLOAT_VAL;
  template<> inline constexpr Type element_tetype<TYPE_VEC4_VAL> = TYPE_FLOAT_VAL;
  template<> inline constexpr Type element_tetype<TYPE_MAT2_VAL> = TYPE_VEC2_VAL;
  template<> inline constexpr Type element_tetype<TYPE_MAT3_VAL> = TYPE_VEC3_VAL;
  template<> inline constexpr Type element_tetype<TYPE_MAT4_VAL> = TYPE_VEC4_VAL;
  template<> inline constexpr Type element_tetype<TYPE_VEC2_REF> = TYPE_FLOAT_REF;
  template<> inline constexpr Type element_tetype<TYPE_VEC3_REF> = TYPE_FLOAT_REF;
  template<> inline constexpr Type element_tetype<TYPE_VEC4_REF> = TYPE_FLOAT_REF;
  template<> inline constexpr Type element_tetype<TYPE_MAT2_REF> = TYPE_VEC2_REF;
  template<> inline constexpr Type element_tetype<TYPE_MAT3_REF> = TYPE_VEC3_REF;
  template<> inline constexpr Type element_tetype<TYPE_MAT4_REF> = TYPE_VEC4_REF;

  template<typename T> using element_type = type_of<element_tetype<type_value_of<T>>>;

  void sequence(void * ctx, void * args, void * ret) {
    uint16_t * ctx_data = reinterpret_cast<uint16_t *>(&ctx);
    uint16_t offset = ctx_data[0];
    uint16_t size = ctx_data[1];
    memcpy(ret, reinterpret_cast<char *>(args) + offset, size);
  }

  template<Type RefTy, Type IsConstant = Type(RefTy & TYPE_FLAG_CONSTANT)>
  struct index_value {
    static type_of<element_tetype<RefTy>> call(type_of<RefTy> v, IntT idx) {
      return &v->arr[idx];
    }
  };
  template<Type Ty>
  struct index_value<Ty, TYPE_FLAG_CONSTANT> {
    static type_of<element_tetype<Ty>> call(type_of<Ty> v, IntT idx) {
      return v.arr[idx];
    }
  };

  template<Type RefTy, Type IsConstant = Type(RefTy & TYPE_FLAG_CONSTANT)>
  struct negate_value {
    static type_of<Type(RefTy | TYPE_FLAG_CONSTANT)> call(type_of<RefType> v) {
      type_of<Type(RefTy | TYPE_FLAG_CONSTANT)> vc;
      for (int i = 0; i < element_count<Type(RefTy | TYPE_FLAG_CONSTANT)>; ++i) {
        vc.elements[i] = -v->elements[i];
      }
      return vc;
    }
  };
  template<Type Ty>
  struct negate_value<Ty, TYPE_FLAG_CONSTANT> {
    static type_of<Ty> call(type_of<Ty> v) {
      for (int i = 0; i < element_count<Ty>; ++i) {
        v.elements[i] = -v.elements[i];
      }
      return v;
    }
  };

  template<>
  struct negate_value<TYPE_FLOAT_REF, Type(0)> {
    static type_of<TYPE_FLOAT_VAL> call(type_of<TYPE_FLOAT_REF> v) {
      return -*v;
    }
  };
  template<>
  struct negate_value<TYPE_FLOAT_VAL, TYPE_FLAG_CONSTANT> {
    static type_of<TYPE_FLOAT_VAL> call(type_of<TYPE_FLOAT_VAL> v) {
      return -v;
    }
  };

  template<>
  struct negate_value<TYPE_INT_REF, Type(0)> {
    static type_of<TYPE_INT_VAL> call(type_of<TYPE_INT_REF> v) {
      return -*v;
    }
  };
  template<>
  struct negate_value<TYPE_INT_VAL, TYPE_FLAG_CONSTANT> {
    static type_of<TYPE_INT_VAL> call(type_of<TYPE_INT_VAL> v) {
      return -v;
    }
  };

  template<Type RefTy, Type IsConstant = Type(RefType & TYPE_FLAG_CONSTANT)>
  struct swizzle2_value {
    static Vec2 call(type_of<RefTy> v, IntT i, IntT j) {
      return {v->arr[i], v->arr[j]};
    }
  };
  template<Type Ty>
  struct swizzle2_value<Ty, TYPE_FLAG_CONSTANT> {
    static Vec2 call(type_of<Ty> v, IntT i, IntT j) {
      return {v.arr[i], v.arr[j]};
    }
  };

  template<Type RefTy, Type IsConstant = Type(RefType & TYPE_FLAG_CONSTANT)>
  struct swizzle3_value {
    static Vec3 call(type_of<RefTy> v, IntT i, IntT j, IntT k) {
      return {v->arr[i], v->arr[j], v->arr[k]};
    }
  };
  template<Type Ty>
  struct swizzle3_value<Ty, TYPE_FLAG_CONSTANT> {
    static Vec3 call(type_of<Typ> v, IntT i, IntT j, IntT k) {
      return {v.arr[i], v.arr[j], v.arr[k]};
    }
  };

  template<Type RefTy, Type IsConstant = Type(RefType & TYPE_FLAG_CONSTANT)>
  struct swizzle4_value {
    static Vec4 call(type_of<RefTy> v, IntT i, IntT j, IntT k, IntT l) {
      return {v->arr[i], v->arr[j], v->arr[k], v->arr[l]};
    }
  };
  template<Type Ty>
  struct swizzle4_value<Ty, TYPE_FLAG_CONSTANT> {
    static Vec4 call(type_of<Ty> v, IntT i, IntT j, IntT k, IntT l) {
      return {v.arr[i], v.arr[j], v.arr[k], v.arr[l]};
    }
  };

  namespace detail {
    FloatT sign_float(FloatT v) {
      if (v > 0.0f) {
        return 1.0f;
      } else if (v < 0.0f) {
        return -1.0f;
      } else {
        return v;
      }
    }

    IntT sign_int(IntT v) {
      if (v > 0) {
        return 1;
      } else if (v < 0) {
        return -1;
      } else {
        return v;
      }
    }

    IntT fac(IntT a) {
      if (a < 0) {
        return -1;
      }

      IntT result = 1;
      for (IntT i = 2; i <= a; i++) {
        result *= i;
      }
      return result;
    }

    IntT ncr(IntT n, IntT r) {
      if (n < 0 || r < 0 || n < r) {
        return -1;
      }

      if (n - r < r) {
        r = n - r;
      }

      IntT result = 1;
      for (IntT i = 1; i <= r; i++) {
        result *= n - r + i;
        result /= i;
      }

      return result;
    }

    IntT npr(IntT n, IntT r) {
      return ncr(n, r) * fac(r);
    }

    //FloatT abs2(FloatT n) {return (n<0 ? -n : n);}

    //FloatT log2(FloatT n) {const FloatT ln2=log(2); return log(n)/ln2;}

    template<typename T>
    T add(T a, T b) { return a + b; }
    template<typename T>
    T sub(T a, T b) { return a - b; }
    template<typename T>
    T mul(T a, T b) { return a * b; }
    template<typename T>
    T div(T a, T b) { return a / b; }
    template<typename T>
    T mod(T a, T b) { return a % b; }
    template<>
    FloatT mod<FloatT>(FloatT a, FloatT b) { return fmodf(a, b); }

    FloatT to_radians(FloatT deg) { return deg * (PI / 180.0f); }
    FloatT to_degrees(FloatT rad) { return rad * (180.0f / PI); }

    void int_to_float(void *, void * args, void * ret) {
      *static_cast<FloatT *>(ret) = static_cast<FloatT>(*static_cast<IntT *>(args));
    }

    void float_to_int(void *, void * args, void * ret) {
      *static_cast<IntT *>(ret) = static_cast<IntT>(*static_cast<FloatT *>(args));
    }

    // rest of the code in here only works because of how arguments are packed on the 'stack' and how vecs and mats are stored
    void printf_impl(void *, void * args, void * ret) {
      const char * str = *reinterpret_cast<const char **>(args);
    }

    template<int Size>
    void mem_copy(void *, void * args, void * ret) { memcpy(ret, args, Size); }

    template<int Size>
    void init_mem(void *, void *, void * ret) { memset(ret, 0, Size); }

    template<Type Ty>
    void float_to_vec(void *, void * args, void * ret) {
      static_assert(te::is_vec<Ty>);
      FloatT v = *static_cast<FloatT *>(args);
      for (int i = 0; i < te::element_count<Ty>; ++i) {
        static_cast<FloatT *>(ret)[i] = v;
      }
    }

    template<Type Ty>
    void float_to_mat(void * ctx, void * args, void * ret) {
      static_assert(te::is_mat<Ty>);

      init_mem<sizeof_type(Ty)>(ctx, args, ret);
      constexpr int MatSize = te::element_count<te::element_tetype<Ty>>;
      FloatT v = *static_cast<FloatT *>(args);
      for (int i = 0; i < MatSize; ++i) {
        static_cast<te::type_of<Ty> *>(ret)->arr[i].arr[i] = v;
      }
    }

    template<Type FromTy, Type ToTy>
    void downgrade_mat(void *, void * args, void * ret) {
      static_assert(te::is_mat<FromTy>);
      static_assert(te::is_mat<ToTy>);
      static_assert(FromTy > ToTy);
      constexpr int ToMatSize = te::element_count<te::element_tetype<ToTy>>;

      for (int i = 0; i < ToMatSize; ++i) {
        for (int j = 0; j < ToMatSize; ++j) {
          static_cast<te::type_of<ToTy> *>(ret)->arr[j].arr[i] = static_cast<te::type_of<FromTy> *>(args)->arr[j].arr[i];
        }
      }
    }

    template<Type FromTy, Type ToTy>
    void upgrade_mat(void * ctx, void * args, void * ret) {
      static_assert(te::is_mat<FromTy>);
      static_assert(te::is_mat<ToTy>);
      static_assert(FromTy < ToTy);
      constexpr int FromMatSize = te::element_count<te::element_tetype<FromTy>>;
      constexpr int ToMatSize = te::element_count<te::element_tetype<ToTy>>;

      init_mem<sizeof_type(ToType)>(ctx, args, ret);
      for (int i = 0; i < ToMatSize; ++i) {
        static_cast<te::type_of<ToTy> *>(ret)->arr[i].arr[i] = 1.0f;
      }
      for (int i = 0; i < FromMatSize; ++i) {
        for (int j = 0; j < FromMatSize; ++j) {
          static_cast<te::type_of<ToTy> *>(ret)->arr[j].arr[i] = static_cast<te::type_of<FromTy> *>(args)->arr[j].arr[i];
        }
      }
    }
  }
}

template<Type TargetTy, bool LockType, Type ... AllowedTys>
static FnPtr find_variadic_constructor(const Type * param_types, int param_count) {
  constexpr int target_size = sizeof_type(TargetTy);
  int args_size = 0;
  Type locked_tetype = TYPE_NULL_VAL;
  for (int i = 0; i < param_count; ++i) {
    Type pt = static_cast<Type>(param_types[i] | TYPE_FLAG_CONSTANT);
    if (!LockType || locked_tetype == TYPE_NULL_VAL) {
      if (!((pt == AllowedTys) || ...)) {
        return nullptr;
      }
      locked_tetype = pt;
    } else if (pt != locked_tetype) {
      return nullptr;
    }
    args_size += sizeof_type(pt);
  }

  if (args_size == target_size) {
    return te::detail::mem_copy<target_size>;
  }

  return nullptr;
}

static FnPtr find_constructor_fn(Type type, const Type * param_types, int param_count) {
  // single arg constructors
  if (param_count == 1) {
    Type p0 = static_cast<Type>(param_types[0] | TYPE_FLAG_CONSTANT);
    if (p0 == TYPE_FLOAT_VAL) {
      switch (type) {
        case TYPE_INT_VAL:
          return te::detail::float_to_int;
          break;
        case TYPE_VEC2_VAL:
          return te::detail::float_to_vec<TYPE_VEC2_VAL>;
          break;
        case TYPE_VEC3_VAL:
          return te::detail::float_to_vec<TYPE_VEC3_VAL>;
          break;
        case TYPE_VEC4_VAL:
          return te::detail::float_to_vec<TYPE_VEC4_VAL>;
          break;
        case TYPE_MAT2_VAL:
          return te::detail::float_to_mat<TYPE_MAT2_VAL>;
          break;
        case TYPE_MAT3_VAL:
          return te::detail::float_to_mat<TYPE_MAT3_VAL>;
          break;
        case TYPE_MAT4_VAL:
          return te::detail::float_to_mat<TYPE_MAT4_VAL>;
          break;
        default:
          break;
      }
    }

    switch (type) {
      case TYPE_FLOAT_VAL: {
        if (p0 == TYPE_INT_VAL) {
          return te::detail::int_to_float;
        }
      }
        break;
      case TYPE_VEC2_VAL: {
        if (p0 == TYPE_VEC2_VAL || p0 == TYPE_VEC3_VAL || p0 == TYPE_VEC4_VAL) {
          return te::detail::mem_copy<sizeof_type(TYPE_VEC2_VAL)>;
        }
      }
        break;
      case TYPE_VEC3_VAL: {
        if (p0 == TYPE_VEC3_VAL || p0 == TYPE_VEC4_VAL) {
          return te::detail::mem_copy<sizeof_type(TYPE_VEC3_VAL)>;
        }
      }
        break;
      case TYPE_VEC4_VAL: {
        if (p0 == TYPE_VEC4_VAL) {
          return te::detail::mem_copy<sizeof_type(TYPE_VEC4_VAL)>;
        }
      }
        break;
      case TYPE_MAT2_VAL: {
        switch (p0) {
          case TYPE_MAT2_VAL:
            return te::detail::mem_copy<sizeof_type(TYPE_MAT2_VAL)>;
          case TYPE_MAT3_VAL:
            return te::detail::downgrade_mat<TYPE_MAT3_VAL, TYPE_MAT2_VAL>;
          case TYPE_MAT4_VAL:
            return te::detail::downgrade_mat<TYPE_MAT4_VAL, TYPE_MAT2_VAL>;
          default:
            break;
        }
      }
        break;
      case TYPE_MAT3_VAL: {
        switch (p0) {
          case TYPE_MAT2_VAL:
            return te::detail::upgrade_mat<TYPE_MAT2_VAL, TYPE_MAT3_VAL>;
          case TYPE_MAT3_VAL:
            return te::detail::mem_copy<sizeof_type(TYPE_MAT3_VAL)>;
          case TYPE_MAT4_VAL:
            return te::detail::downgrade_mat<TYPE_MAT4_VAL, TYPE_MAT3_VAL>;
          default:
            break;
        }
      }
        break;
      case TYPE_MAT4_VAL: {
        switch (p0) {
          case TYPE_MAT2_VAL:
            return te::detail::upgrade_mat<TYPE_MAT2_VAL, TYPE_MAT4_VAL>;
          case TYPE_MAT3_VAL:
            return te::detail::upgrade_mat<TYPE_MAT3_VAL, TYPE_MAT4_VAL>;
          case TYPE_MAT4_VAL:
            return te::detail::mem_copy<sizeof_type(TYPE_MAT4_VAL)>;
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
  FnPtr ret = nullptr;
  Type element_type = TYPE_NULL_VAL;
  switch (type) {
    case TYPE_VEC2_VAL:
      return find_variadic_constructor<TYPE_VEC2_VAL, false, TYPE_FLOAT_VAL, TYPE_VEC2_VAL>(param_types, param_count);
    case TYPE_VEC3_VAL:
      return find_variadic_constructor<TYPE_VEC3_VAL, false, TYPE_FLOAT_VAL, TYPE_VEC2_VAL, TYPE_VEC3_VAL>(param_types, param_count);
    case TYPE_VEC4_VAL:
      return find_variadic_constructor<TYPE_VEC4_VAL, false, TYPE_FLOAT_VAL, TYPE_VEC2_VAL, TYPE_VEC3_VAL, TYPE_VEC4_VAL>(param_types, param_count);
    case TYPE_MAT2_VAL:
      return find_variadic_constructor<TYPE_MAT2_VAL, true, TYPE_FLOAT_VAL, TYPE_VEC2_VAL>(param_types, param_count);
    case TYPE_MAT3_VAL:
      return find_variadic_constructor<TYPE_MAT3_VAL, true, TYPE_FLOAT_VAL, TYPE_VEC3_VAL>(param_types, param_count);
    case TYPE_MAT4_VAL:
      return find_variadic_constructor<TYPE_MAT4_VAL, true, TYPE_FLOAT_VAL, TYPE_VEC4_VAL>(param_types, param_count);
    default:
      break;
  }

  return nullptr;
}

static FnObj find_constructor(te_parser_state & s, Type type, const Type * arg_types, int8_t arg_count) {
  for (int i = 0; i < arg_count; ++i) {
    if (arg_types[i] == TYPE_ERROR) {
      return FnObj{};
    }
  }

  FnObj fn;
  fn.ptr = find_constructor_fn(type, arg_types, arg_count);
  fn.context = nullptr;
  fn.return_type = type;
  fn.param_count = arg_count;
  for (int i = 0; i < arg_count; ++i) {
    fn.param_types[i] = static_cast<Type>(arg_types[i] | TYPE_FLAG_CONSTANT);
  }

  if (!fn.is_valid()) {
#ifdef TESL_DEBUG_COMPILE
    tesl_printf("error: could not find constructor for ");
    te_print(type);
    tesl_printf("(");
    for (int i = 0; i < arg_count; ++i) {
      if (i != 0) {
        tesl_printf(", ");
      }
      te_print(arg_types[i]);
    }
    tesl_printf(")\n");
#endif
    te_make_error(s);
  }

  return fn;
}

static constexpr const Variable te_builtins[] = {
    {"true", te::make_value<IntT>(1)},
    {"false", te::make_value<IntT>(0)},
    {"E", te::make_value<FloatT>(2.71828182845904523536f)},
    {"INF", te::make_value<FloatT>(INFINITY)},
    {"NAN", te::make_value<FloatT>(NAN)},
    {"PI", te::make_value<FloatT>(PI)},
    {"TAU", te::make_value<FloatT>(PI * 2.0f)},
    {"abs", te::make_pure_function<fabsf>()},
    {"acos", te::make_pure_function<acosf>()},
    {"asin", te::make_pure_function<asinf>()},
    {"atan", te::make_pure_function<atanf>()},
    {"atan2", te::make_pure_function<atan2f>()},
    {"ceil", te::make_pure_function<ceilf>()},
    {"cos", te::make_pure_function<cosf>()},
    {"cosh", te::make_pure_function<coshf>()},
    {"degrees", te::make_pure_function<te::detail::to_degrees>()},
    {"exp", te::make_pure_function<expf>()},
    {"fac", te::make_pure_function<te::detail::fac>()},
    {"floor", te::make_pure_function<floorf>()},
    {"ln", te::make_pure_function<logf>()},
#ifdef TE_NAT_LOG
    {"log",     te::make_pure_function<logf>()},
#else
    {"log", te::make_pure_function<log10f>()},
#endif
    {"log10", te::make_pure_function<log10f>()},
    {"log2", te::make_pure_function<log2f>()},
    {"mod", te::make_pure_function<fmodf>()},
    {"ncr", te::make_pure_function<te::detail::ncr>()},
    {"npr", te::make_pure_function<te::detail::npr>()},
    {"pow", te::make_pure_function<powf>()},
    {"radians", te::make_pure_function<te::detail::to_radians>()},
    {"round", te::make_pure_function<roundf>()},
    {"sign", te::make_pure_function<te::detail::sign_int>()},
    {"sign", te::make_pure_function<te::detail::sign_float>()},
    {"sin", te::make_pure_function<sinf>()},
    {"sinh", te::make_pure_function<sinhf>()},
    {"sqrt", te::make_pure_function<sqrtf>()},
    {"tan", te::make_pure_function<tanf>()},
    {"tanh", te::make_pure_function<tanhf>()},
};

static int te_builtins_count = sizeof(te_builtins) / sizeof(Variable);

// static const Variable *find_builtin(const char *name, int len) {
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

template<typename T>
static bool test_arg_types_match(T & var, const Type * arg_types, int arg_count) {
  if (arg_types) {
    if (var.type == TYPE_FUNCTION && arg_count == var.fn.param_count) {
      for (int i = 0; i < arg_count; ++i) {
        if ((arg_types[i] | (var.fn.param_types[i] & TYPE_FLAG_CONSTANT)) != var.fn.param_types[i]) {
          return false;
        }
      }

      return true;
    }
  
    return false;
  }

  return true;
}

template<>
bool test_arg_types_match<te_parser_state::local_var_t>(te_parser_state::local_var_t & var, const Type * arg_types, int arg_count) {
  return true;
}

template<typename T>
static te_parser_state::var_ref find_var_search(T * vars, int var_count, StrView name, const Type * arg_types = nullptr, int arg_count = 0) {
  for (int i = var_count-1; i >= 0; --i) {
    T & var = vars[i];
    if (var.get_name() == name) {
      if (test_arg_types_match(var, arg_types, arg_count)) {
        return var;
      }
    }
  continue_outer:;
  }

  return {};
}

static te_parser_state::var_ref find_var(te_parser_state & s, StrView name, Type * arg_types = nullptr, int arg_count = 0) {
  for (int i = 0; i < arg_count; ++i) {
    if (arg_types[i] == TYPE_ERROR) {
      return {};
    }
  }

  te_parser_state::var_ref result = find_var_search(s.vars, s.var_count, name, arg_types, arg_count);
  if (result.is_valid()) {
    return result;
  }

  result = find_var_search(s.global_vars, s.global_var_count, name, arg_types, arg_count);
  if (result.is_valid()) {
    return result;
  }

  if (s.parent) {
    return find_var(*s.parent, name, arg_types, arg_count);
  }

  result = find_var_search(te_builtins, te_builtins_count, name, arg_types, arg_count);

  if (!result.is_valid()) {
    if (arg_types) {
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: could not find function matching '%.*s", name.len(), name.ptr);
      tesl_printf("(");
      for (int i = 0; i < arg_count; ++i) {
        if (i != 0) {
          tesl_printf(", ");
        }
        te_print(arg_types[i]);
      }
      tesl_printf(")'\n");
#endif
      te_make_error(s);
    } else {
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: could not find '%.*s'!\n", name.len(), name.ptr);
#endif
      te_make_error(s);
    }
  }

  return result;
}

static FnObj te_get_index_func(Type type) {
  FnObj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TYPE_VEC2_VAL:
      return te::make_pure_function<te::index_value<TYPE_VEC2_VAL>::call>();
    case TYPE_VEC3_VAL:
      return te::make_pure_function<te::index_value<TYPE_VEC3_VAL>::call>();
    case TYPE_VEC4_VAL:
      return te::make_pure_function<te::index_value<TYPE_VEC4_VAL>::call>();
    case TYPE_MAT2_VAL:
      return te::make_pure_function<te::index_value<TYPE_MAT2_VAL>::call>();
    case TYPE_MAT3_VAL:
      return te::make_pure_function<te::index_value<TYPE_MAT3_VAL>::call>();
    case TYPE_MAT4_VAL:
      return te::make_pure_function<te::index_value<TYPE_MAT4_VAL>::call>();
    case TYPE_VEC2_REF:
      return te::make_pure_function<te::index_value<TYPE_VEC2_REF>::call>();
    case TYPE_VEC3_REF:
      return te::make_pure_function<te::index_value<TYPE_VEC3_REF>::call>();
    case TYPE_VEC4_REF:
      return te::make_pure_function<te::index_value<TYPE_VEC4_REF>::call>();
    case TYPE_MAT2_REF:
      return te::make_pure_function<te::index_value<TYPE_MAT2_REF>::call>();
    case TYPE_MAT3_REF:
      return te::make_pure_function<te::index_value<TYPE_MAT3_REF>::call>();
    case TYPE_MAT4_REF:
      return te::make_pure_function<te::index_value<TYPE_MAT4_REF>::call>();
    default:
      break;
  }

  return ret;
}

static FnObj te_get_swizzle2_func(Type type) {
  FnObj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TYPE_VEC2_REF:
      return te::make_pure_function<te::swizzle2_value<TYPE_VEC2_REF>::call>();
    case TYPE_VEC3_REF:
      return te::make_pure_function<te::swizzle2_value<TYPE_VEC3_REF>::call>();
    case TYPE_VEC4_REF:
      return te::make_pure_function<te::swizzle2_value<TYPE_VEC4_REF>::call>();
    case TYPE_VEC2_VAL:
      return te::make_pure_function<te::swizzle2_value<TYPE_VEC2_VAL>::call>();
    case TYPE_VEC3_VAL:
      return te::make_pure_function<te::swizzle2_value<TYPE_VEC3_VAL>::call>();
    case TYPE_VEC4_VAL:
      return te::make_pure_function<te::swizzle2_value<TYPE_VEC4_VAL>::call>();
    default:
      break;
  }

  return ret;
}

static FnObj te_get_swizzle3_func(Type type) {
  FnObj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TYPE_VEC2_REF:
      return te::make_pure_function<te::swizzle3_value<TYPE_VEC2_REF>::call>();
    case TYPE_VEC3_REF:
      return te::make_pure_function<te::swizzle3_value<TYPE_VEC3_REF>::call>();
    case TYPE_VEC4_REF:
      return te::make_pure_function<te::swizzle3_value<TYPE_VEC4_REF>::call>();
    case TYPE_VEC2_VAL:
      return te::make_pure_function<te::swizzle3_value<TYPE_VEC2_VAL>::call>();
    case TYPE_VEC3_VAL:
      return te::make_pure_function<te::swizzle3_value<TYPE_VEC3_VAL>::call>();
    case TYPE_VEC4_VAL:
      return te::make_pure_function<te::swizzle3_value<TYPE_VEC4_VAL>::call>();
    default:
      break;
  }

  return ret;
}

static FnObj te_get_swizzle4_func(Type type) {
  FnObj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TYPE_VEC2_REF:
      return te::make_pure_function<te::swizzle4_value<TYPE_VEC2_REF>::call>();
    case TYPE_VEC3_REF:
      return te::make_pure_function<te::swizzle4_value<TYPE_VEC3_REF>::call>();
    case TYPE_VEC4_REF:
      return te::make_pure_function<te::swizzle4_value<TYPE_VEC4_REF>::call>();
    case TYPE_VEC2_VAL:
      return te::make_pure_function<te::swizzle4_value<TYPE_VEC2_VAL>::call>();
    case TYPE_VEC3_VAL:
      return te::make_pure_function<te::swizzle4_value<TYPE_VEC3_VAL>::call>();
    case TYPE_VEC4_VAL:
      return te::make_pure_function<te::swizzle4_value<TYPE_VEC4_VAL>::call>();
    default:
      break;
  }

  return ret;
}

static FnObj te_get_negate_func(Type type) {
  FnObj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TYPE_INT_VAL:
      return te::make_pure_function<te::negate_value<TYPE_INT_VAL>::call>();
    case TYPE_FLOAT_VAL:
      return te::make_pure_function<te::negate_value<TYPE_FLOAT_VAL>::call>();
    case TYPE_VEC2_VAL:
      return te::make_pure_function<te::negate_value<TYPE_VEC2_VAL>::call>();
    case TYPE_VEC3_VAL:
      return te::make_pure_function<te::negate_value<TYPE_VEC3_VAL>::call>();
    case TYPE_VEC4_VAL:
      return te::make_pure_function<te::negate_value<TYPE_VEC4_VAL>::call>();
    case TYPE_MAT2_VAL:
      return te::make_pure_function<te::negate_value<TYPE_MAT2_VAL>::call>();
    case TYPE_MAT3_VAL:
      return te::make_pure_function<te::negate_value<TYPE_MAT3_VAL>::call>();
    case TYPE_MAT4_VAL:
      return te::make_pure_function<te::negate_value<TYPE_MAT4_VAL>::call>();
    case TYPE_INT_REF:
      return te::make_pure_function<te::negate_value<TYPE_INT_REF>::call>();
    case TYPE_FLOAT_REF:
      return te::make_pure_function<te::negate_value<TYPE_FLOAT_REF>::call>();
    case TYPE_VEC2_REF:
      return te::make_pure_function<te::negate_value<TYPE_VEC2_REF>::call>();
    case TYPE_VEC3_REF:
      return te::make_pure_function<te::negate_value<TYPE_VEC3_REF>::call>();
    case TYPE_VEC4_REF:
      return te::make_pure_function<te::negate_value<TYPE_VEC4_REF>::call>();
    case TYPE_MAT2_REF:
      return te::make_pure_function<te::negate_value<TYPE_MAT2_REF>::call>();
    case TYPE_MAT3_REF:
      return te::make_pure_function<te::negate_value<TYPE_MAT3_REF>::call>();
    case TYPE_MAT4_REF:
      return te::make_pure_function<te::negate_value<TYPE_MAT4_REF>::call>();
    default:
      break;
  }

  return ret;
}

namespace te::detail {
  template<auto Op>
  struct fn_scalar_op_scalar {
    template<typename TRet, typename TA, typename TB>
    static void call(void *, void * args, void * retvp) {
      static_assert(std::is_same_v<TA, TB>);
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
    static void call(void *, void * args, void * retvp) {
      static_assert(std::is_same_v<TA, TB>);
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
    static void call(void *, void * args, void * retvp) {
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
    static void call(void *, void * args, void * retvp) {
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
  void fn_matrix_mul_vec(void *, void * args, void * retvp) {
    constexpr auto MatSize = te::element_count<te::element_tetype<te::type_value_of<TA>>>;
    static_assert(MatSize == te::element_count<te::type_value_of<TB>>);
    TRet * ret = (TRet *) retvp;
    TA * ap = (TA *) args;
    TB * bp = (TB *) (&ap[1]);
    TA a = *ap;
    TB b = *bp;
    for (int i = 0; i < MatSize; ++i) {
      FloatT sum = 0.0f;
      for (int j = 0; j < MatSize; ++j) {
        sum += te::detail::mul(a.arr[i].arr[j], b.arr[j]);
      }
      ret->arr[i] = sum;
    }
  }

  template<typename TRet, typename TA, typename TB>
  void fn_vec_mul_matrix(void *, void * args, void * retvp) {
    constexpr auto MatSize = te::element_count<te::element_tetype<te::type_value_of<TB>>>;
    static_assert(te::element_count<te::type_value_of<TA>> == MatSize);
    TRet * ret = (TRet *) retvp;
    TA * ap = (TA *) args;
    TB * bp = (TB *) (&ap[1]);
    TA a = *ap;
    TB b = *bp;
    for (int i = 0; i < MatSize; ++i) {
      FloatT sum = 0.0f;
      for (int j = 0; j < MatSize; ++j) {
        sum += te::detail::mul(a.arr[i], b.arr[i].arr[j]);
      }
      ret->arr[i] = sum;
    }
  }

  template<typename TRet, typename TA, typename TB>
  void fn_matrix_mul_matrix(void *, void * args, void * retvp) {
    static_assert(std::is_same_v<TA, TB>);
    constexpr auto MatSize = te::element_count<te::element_tetype<te::type_value_of<TA>>>;
    TRet * ret = (TRet *) retvp;
    TA * ap = (TA *) args;
    TB * bp = (TB *) (&ap[1]);
    TA a = *ap;
    TB b = *bp;
    for (int i = 0; i < MatSize; ++i) {
      for (int j = 0; j < MatSize; ++j) {
        FloatT sum = 0.0f;
        for (int k = 0; k < MatSize; ++k) {
          sum += te::detail::mul(a.arr[i].arr[k], b.arr[k].arr[j]);
        }
        ret->arr[i].arr[j] = sum;
      }
    }
  }
}

#define TE_BINARY_OP_FUNCTION(FNAME, TRET, TA, TB) tesl::detail::make_function_raw(FNAME<tesl::type_of<(TRET)>, tesl::type_of<(TA)>, tesl::type_of<(TB)>>, nullptr, true, (TRET), {(TA), (TB)})

#define CASE_SCALAR_SCALAR_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(tesl::detail::fn_scalar_op_scalar<(OP)>::call, TA, TA, TB)
#define CASE_VEC_VEC_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(tesl::detail::fn_vec_op_vec<(OP)>::call, TA, TA, TB)
#define CASE_SCALAR_VEC_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(tesl::detail::fn_scalar_op_vec<(OP)>::call, TB, TA, TB)
#define CASE_VEC_SCALAR_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(tesl::detail::fn_vec_op_scalar<(OP)>::call, TA, TA, TB)
#define CASE_MATRIX_VEC_MUL_(TA, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(tesl::detail::fn_matrix_mul_vec, TB, TA, TB)
#define CASE_VEC_MATRIX_MUL_(TA, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(tesl::detail::fn_vec_mul_matrix, TA, TA, TB)
#define CASE_MATRIX_MATRIX_MUL_(T) if (typeA == (T) && typeB == (T)) return TE_BINARY_OP_FUNCTION(tesl::detail::fn_matrix_mul_matrix, T, T, T)

static FnObj te_get_add_func(Type typeA, Type typeB) {
  FnObj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TYPE_INT_VAL, te::detail::add<IntT>, TYPE_INT_VAL);
  CASE_SCALAR_SCALAR_(TYPE_FLOAT_VAL, te::detail::add<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_VEC_(TYPE_VEC2_VAL, te::detail::add<FloatT>, TYPE_VEC2_VAL);
  CASE_VEC_VEC_(TYPE_VEC3_VAL, te::detail::add<FloatT>, TYPE_VEC3_VAL);
  CASE_VEC_VEC_(TYPE_VEC4_VAL, te::detail::add<FloatT>, TYPE_VEC4_VAL);

  return ret;
}

static FnObj te_get_sub_func(Type typeA, Type typeB) {
  FnObj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TYPE_INT_VAL, te::detail::sub<IntT>, TYPE_INT_VAL);
  CASE_SCALAR_SCALAR_(TYPE_FLOAT_VAL, te::detail::sub<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_VEC_(TYPE_VEC2_VAL, te::detail::sub<FloatT>, TYPE_VEC2_VAL);
  CASE_VEC_VEC_(TYPE_VEC3_VAL, te::detail::sub<FloatT>, TYPE_VEC3_VAL);
  CASE_VEC_VEC_(TYPE_VEC4_VAL, te::detail::sub<FloatT>, TYPE_VEC4_VAL);

  return ret;
}

static FnObj te_get_mod_func(Type typeA, Type typeB) {
  FnObj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TYPE_INT_VAL, te::detail::mod<IntT>, TYPE_INT_VAL);
  CASE_SCALAR_SCALAR_(TYPE_FLOAT_VAL, te::detail::mod<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_VEC_(TYPE_VEC2_VAL, te::detail::mod<FloatT>, TYPE_VEC2_VAL);
  CASE_VEC_VEC_(TYPE_VEC3_VAL, te::detail::mod<FloatT>, TYPE_VEC3_VAL);
  CASE_VEC_VEC_(TYPE_VEC4_VAL, te::detail::mod<FloatT>, TYPE_VEC4_VAL);
  CASE_VEC_SCALAR_(TYPE_VEC2_VAL, te::detail::mod<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_VEC3_VAL, te::detail::mod<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_VEC4_VAL, te::detail::mod<FloatT>, TYPE_FLOAT_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::mod<FloatT>, TYPE_VEC2_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::mod<FloatT>, TYPE_VEC3_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::mod<FloatT>, TYPE_VEC4_VAL);

  return ret;
}

static FnObj te_get_mul_func(Type typeA, Type typeB) {
  FnObj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TYPE_INT_VAL, te::detail::mul<IntT>, TYPE_INT_VAL);
  CASE_SCALAR_SCALAR_(TYPE_FLOAT_VAL, te::detail::mul<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_VEC_(TYPE_VEC2_VAL, te::detail::mul<FloatT>, TYPE_VEC2_VAL);
  CASE_VEC_VEC_(TYPE_VEC3_VAL, te::detail::mul<FloatT>, TYPE_VEC3_VAL);
  CASE_VEC_VEC_(TYPE_VEC4_VAL, te::detail::mul<FloatT>, TYPE_VEC4_VAL);
  CASE_VEC_SCALAR_(TYPE_VEC2_VAL, te::detail::mul<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_VEC3_VAL, te::detail::mul<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_VEC4_VAL, te::detail::mul<FloatT>, TYPE_FLOAT_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::mul<FloatT>, TYPE_VEC2_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::mul<FloatT>, TYPE_VEC3_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::mul<FloatT>, TYPE_VEC4_VAL);
  CASE_VEC_SCALAR_(TYPE_MAT2_VAL, te::detail::mul<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_MAT3_VAL, te::detail::mul<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_MAT4_VAL, te::detail::mul<FloatT>, TYPE_FLOAT_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::mul<FloatT>, TYPE_MAT2_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::mul<FloatT>, TYPE_MAT3_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::mul<FloatT>, TYPE_MAT4_VAL);
  CASE_MATRIX_VEC_MUL_(TYPE_MAT2_VAL, TYPE_VEC2_VAL);
  CASE_MATRIX_VEC_MUL_(TYPE_MAT3_VAL, TYPE_VEC3_VAL);
  CASE_MATRIX_VEC_MUL_(TYPE_MAT4_VAL, TYPE_VEC4_VAL);
  CASE_VEC_MATRIX_MUL_(TYPE_VEC2_VAL, TYPE_MAT2_VAL);
  CASE_VEC_MATRIX_MUL_(TYPE_VEC3_VAL, TYPE_MAT3_VAL);
  CASE_VEC_MATRIX_MUL_(TYPE_VEC4_VAL, TYPE_MAT4_VAL);
  CASE_MATRIX_MATRIX_MUL_(TYPE_MAT2_VAL);
  CASE_MATRIX_MATRIX_MUL_(TYPE_MAT3_VAL);
  CASE_MATRIX_MATRIX_MUL_(TYPE_MAT4_VAL);

  return ret;
}

static FnObj te_get_div_func(Type typeA, Type typeB) {
  FnObj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TYPE_INT_VAL, te::detail::div<IntT>, TYPE_INT_VAL);
  CASE_SCALAR_SCALAR_(TYPE_FLOAT_VAL, te::detail::div<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_VEC_(TYPE_VEC2_VAL, te::detail::div<FloatT>, TYPE_VEC2_VAL);
  CASE_VEC_VEC_(TYPE_VEC3_VAL, te::detail::div<FloatT>, TYPE_VEC3_VAL);
  CASE_VEC_VEC_(TYPE_VEC4_VAL, te::detail::div<FloatT>, TYPE_VEC4_VAL);
  CASE_VEC_SCALAR_(TYPE_VEC2_VAL, te::detail::div<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_VEC3_VAL, te::detail::div<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_VEC4_VAL, te::detail::div<FloatT>, TYPE_FLOAT_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::div<FloatT>, TYPE_VEC2_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::div<FloatT>, TYPE_VEC3_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::div<FloatT>, TYPE_VEC4_VAL);
  CASE_VEC_SCALAR_(TYPE_MAT2_VAL, te::detail::div<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_MAT3_VAL, te::detail::div<FloatT>, TYPE_FLOAT_VAL);
  CASE_VEC_SCALAR_(TYPE_MAT4_VAL, te::detail::div<FloatT>, TYPE_FLOAT_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::div<FloatT>, TYPE_MAT2_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::div<FloatT>, TYPE_MAT3_VAL);
  CASE_SCALAR_VEC_(TYPE_FLOAT_VAL, te::detail::div<FloatT>, TYPE_MAT4_VAL);

  return ret;
}

#undef CASE_MATRIX_MUL_
#undef CASE_VEC_SCALAR_
#undef CASE_SCALAR_VEC_
#undef CASE_VEC_VEC_
#undef CASE_SCALAR_SCALAR_

static void post_increment(void *, void * args, void * ret) {
  *reinterpret_cast<IntT *>(ret) = *reinterpret_cast<IntT **>(args)[0];
  ++(*reinterpret_cast<IntT **>(args)[0]);
}

static void post_decrement(void *, void * args, void * ret) {
  *reinterpret_cast<IntT *>(ret) = *reinterpret_cast<IntT **>(args)[0];
  --(*reinterpret_cast<IntT **>(args)[0]);
}

static void pre_increment(void *, void * args, void * ret) {
  ++(*reinterpret_cast<IntT **>(args)[0]);
  *reinterpret_cast<IntT *>(ret) = *reinterpret_cast<IntT **>(args)[0];
}

static void pre_decrement(void *, void * args, void * ret) {
  --(*reinterpret_cast<IntT **>(args)[0]);
  *reinterpret_cast<IntT *>(ret) = *reinterpret_cast<IntT **>(args)[0];
}

static void bool_not(void *, void * args, void * ret) {
  *reinterpret_cast<IntT *>(ret) = !reinterpret_cast<IntT *>(args)[0];
}

/*static te_expr * expr(te_parser_state & s);
static te_expr * power(te_parser_state & s);

static te_expr * base(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("entered base\n");
#endif

  // <base>      =    <literal> | <variable> | <function-X> "(" {<expr> {"," | "," <expr>}+} ")" | "(" <expr> ")"
  ErrorRecord er{s};
  te_expr * ret = nullptr;
  switch (s.token.kind) {
    case Token::LITERAL: {
#ifdef TE_DEBUG_PEDANTIC
      te_print("base is literal ");
      te_print(s.token.type);
      te_print(\n");
#endif
      switch (s.token.type) {
        case TYPE_INT_VAL:
          ret = new_int_literal_expr(s.token.int_value);
          break;
        case TE_FLOAT:
          ret = new_float_literal_expr(s.token.float_value);
          break;
        case TYPE_STR_VAL:
          ret = new_str_literal_expr(s.token.name);
          break;
        default:
          ErrorRecord er(s);
#ifdef TESL_DEBUG_COMPILE
          te_print("internal error: unknown literal type: ");
          te_print(s.token.type);
          te_print(!\n");
#endif
          te_make_error(s);
          break;
      }
      s.advance();
    } break;
    case Token::TYPENAME:
    case Token::IDENTIFIER: {
      bool is_typename = s.token == Token::TYPENAME;
      Type type = s.token.type;
      StrView id = s.token.name;

      s.advance();
      if (s.token == Token::OPEN_PAREN) {
#ifdef TE_DEBUG_PEDANTIC
        tesl_printf("base is function call\n");
#endif
        te_expr * args[TE_PARAM_COUNT_MAX]{nullptr};

        s.advance();
        int arg_count = 0;
        Type arg_types[TE_PARAM_COUNT_MAX]{TYPE_NULL_VAL};
        bool valid = true;
        while (s.token != Token::CLOSE_PAREN && s.token != Token::END) {
          te_expr * arg = expr(s);

          if (arg_count < TE_PARAM_COUNT_MAX) {
            arg_types[arg_count] = arg ? arg->type : TYPE_ERROR;
            args[arg_count] = arg;
            ++arg_count;
          } else {
            valid = false;
            te_free(arg);
            arg = nullptr;
          }

          if (s.token == Token::COMMA) {
            s.advance();
          }
        }

        FnObj fn;
        if (s.token == Token::CLOSE_PAREN) {
          er.set_end(s.token.name.end);
          if (is_typename) {
            fn = find_constructor(s, type, arg_types, arg_count);

            if (!fn.is_valid()) {
              valid = false;
              s.parse_error = true;
            }
          } else {
            te_parser_state::var_ref var = find_var(s, id, arg_types, arg_count);
            if (var.is_valid() && var.is_function()) {
              fn = var.get_function();
            } else {
              valid = false;
              s.parse_error = true;
            }
          }

          s.advance();
        } else {
          ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
          tesl_printf("error: missing ')'!\n");
#endif
          te_make_error(s);
          valid = false;
        }

        if (valid) {
          ret = new_call_expr(fn, args, arg_count);
#ifdef TE_DEBUG_PEDANTIC
          tesl_printf("function value: ");
          te_print({ret ? reinterpret_cast<te_call_expr *>(ret)->fn : FnObj{}, TYPE_FUNCTION});
          tesl_printf("\n");
#endif
        } else {
          for (int j = arg_count - 1; j >= 0; --j) {
            te_free(args[j]);
            args[j] = nullptr;
          }
          ret = nullptr;
        }
      } else {
        te_parser_state::var_ref var = find_var(s, id);
        if (var.is_valid()) {
          ret = var.new_ref_expr(s);
#ifdef TE_DEBUG_PEDANTIC
          tesl_printf("base is global var, value: ");
          te_print_expr(ret);
          tesl_printf("\n");
#endif
        } else {
          s.parse_error = true;
        }

      }
    } break;
    case Token::OPEN_PAREN: {
#ifdef TE_DEBUG_PEDANTIC
      tesl_printf("base is grouping expr\n");
#endif
      s.advance();
      ret = expr(s);

      if (s.token == Token::CLOSE_PAREN) {
        s.advance();
      } else if (ret != nullptr) {
        ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
        tesl_printf("error: expected ')', got ");
        te_print(s.token);
        tesl_printf("!\n");
#endif
        te_make_error(s);
        te_free(ret);
        ret = nullptr;
        return nullptr;
      }
    } break;
    default:
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: expected expression, got ");
      te_print(s.token);
      tesl_printf("!\n");
#endif
      te_make_error(s);
      s.advance();
      return nullptr;
  }

#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("exited base\n");
#endif

  return ret;
}

static te_expr * index(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("entered index\n");
#endif

  // <index>     =    <base> { "[" <int> "]" }
  ErrorRecord er{s};
  te_expr * ret = base(s);

  if (s.token == Token::OPEN_SQUARE_BRACKET) {
    Type ret_type = ret ? ret->type : TYPE_ERROR;

    s.advance();

    te_expr * idx_expr = nullptr;

    if (s.token == Token::CLOSE_SQUARE_BRACKET) {
      ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: missing index!\n");
#endif
      te_make_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    idx_expr = expr(s);

    Type idx_type = idx_expr ? idx_expr->type : TYPE_ERROR;

    FnObj index_fn;
    if ((idx_type | TYPE_FLAG_CONSTANT) == TYPE_INT_VAL) {
      index_fn = te_get_index_func(ret_type);
    } else {
      // set to a valid noop fn to avoid double error print
      index_fn = FnObj{};
      index_fn.ptr = &te_noop;
      if (idx_type != TYPE_ERROR) {
        ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
        tesl_printf("error: index must be an int! type: ");
        te_print(idx_type);
        tesl_printf("\n");
#endif
        te_make_error(s);
      }
      te_free(idx_expr);
      idx_expr = nullptr;
      idx_type = idx_expr ? idx_expr->type : TYPE_ERROR;
      te_free(ret);
      ret = nullptr;
    }

    if (s.token != Token::CLOSE_SQUARE_BRACKET) {
      ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: expected ']', got ");
      te_print(s.token);
      tesl_printf("!\n");
#endif
      te_make_error(s);
      te_free(idx_expr);
      idx_expr = nullptr;
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    s.advance();

    if (!index_fn.is_valid()) {
#ifdef TESL_DEBUG_COMPILE
      er.set_end(s.prev_token.name.end);
      tesl_printf("error: cannot index! type: ");
      te_print(idx_type);
      tesl_printf("\n");
#endif
      te_make_error(s);
      te_free(idx_expr);
      idx_expr = nullptr;
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }
    ret = new_call_expr(index_fn, {ret, idx_expr});
  }

#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("exited index\n");
#endif

  return ret;
}

static te_expr * attr(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("entered attr\n");
#endif

  // <attr>      =    <index> { "." ("x" | "y" | "z" | "w" | "r" | "g" | "b" | "a")[1-4] }
  ErrorRecord er{s};
  te_expr * ret = index(s);
  Type ret_type = ret ? ret->type : TYPE_ERROR;

  if (s.token == Token::DOT) {
    s.advance();
    if (s.token != Token::IDENTIFIER) {
      ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: expected swizzlers after '.' got ");
      te_print(s.token);
      tesl_printf("!\n");
#endif
      te_make_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    Type base_type = ret ? ret->type : TYPE_ERROR;
    int swizzle_max = 0;
    switch (base_type) {
      case TYPE_VEC2_VAL:
        swizzle_max = 2;
        break;
      case TYPE_VEC3_VAL:
        swizzle_max = 3;
        break;
      case TYPE_VEC4_VAL:
        swizzle_max = 4;
        break;
      default: {
        ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
        tesl_printf("error: cannot swizzle ");
        te_print(base_type);
        tesl_printf("!\n");
#endif
        te_make_error(s);
        te_free(ret);
        ret = nullptr;
        return nullptr;
      }
    }

    int indices[4];
    int i = 0;
    for (const char * c = s.token.name.ptr; c < s.token.name.end; ++c, ++i) {
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
            ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
            er.start = c;
            er.end = c + 1;
            tesl_printf("error: unknown swizzler '%c'!\n", *c);
#endif
            te_make_error(s);
            s.advance();
            te_free(ret);
            ret = nullptr;
            return nullptr;
          }
        }
      }
    }

    if (i < 1 || i > swizzle_max) {
      ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: too many swizzlers! type: ");
      te_print(base_type);
      tesl_printf("\n");
#endif
      te_make_error(s);
      s.advance();
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    te_expr * args[i + 1];
    args[0] = ret;
    for (int j = 0; j < i; ++j) {
      te_value idx_value;
      idx_value.int_ = indices[j];
      args[j + 1] = new_value_expr(TYPE_INT_VAL, idx_value);
    }

    FnObj index_fn;
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
        index_fn = FnObj{};
        break; // unreachable
    }

    if (!index_fn.is_valid()) {
      ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("internal error: no index func! type: ");
      te_print(ret_type);
      tesl_printf("\n");
#endif
      te_make_error(s);
      for (int j = i - 1; j >= 0; --j) {
        te_free(args[j + 1]);
        args[j + 1] = nullptr;
      }
      te_free(ret);
      ret = nullptr;
    } else {
      ret = new_call_expr(index_fn, args, i + 1);
    }

    s.advance();
  }

#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("exited attr\n");
#endif

  return ret;
}

static te_expr * factor(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("entered factor\n");
#endif

  // <factor>    =    {("!" | "-" | "--" | "+" | "++")} <attr> {("--" | "++")}
  ErrorRecord er{s};

  bool bool_not_flag = false;
  bool pos = true;
  bool pre_incdec = false;
  if (s.token == Token::PLUS) {
    pos = true;
    s.advance();
  } else if (s.token == Token::MINUS) {
    pos = false;
    s.advance();
  } else if (s.token == Token::PLUS_PLUS) {
    pos = true;
    pre_incdec = true;
    s.advance();
  } else if (s.token == Token::MINUS_MINUS) {
    pos = false;
    pre_incdec = true;
    s.advance();
  } else if (s.token == Token::BANG) {
    bool_not_flag = true;
    s.advance();
  }

  te_expr * ret = attr(s);
  Type ret_type = ret ? ret->type : TYPE_ERROR;

  er.set_end(s.token.name.end);

  if (bool_not_flag) {
    if (ret_type == TYPE_INT_VAL) {
      ret = new_call_expr(te::detail::make_function_raw(bool_not, nullptr, true, TYPE_INT_VAL, {TYPE_INT_VAL}), {ret});
    } else {
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: '!' op only works on int! type: ");
      te_print(ret_type);
      tesl_printf("\n");
#endif
      te_make_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }
  } else if (pre_incdec) {
    if (ret_type != TYPE_INT_REF) {
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: only int ref can be incremented/decremented! type: ");
      te_print(ret_type);
      tesl_printf("\n");
#endif
      te_make_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    if (pos) {
      ret = new_call_expr(te::detail::make_function_raw(pre_increment, nullptr, true, TYPE_INT_VAL, {TYPE_INT_REF}), {ret});
    } else {
      ret = new_call_expr(te::detail::make_function_raw(pre_decrement, nullptr, true, TYPE_INT_VAL, {TYPE_INT_REF}), {ret});
    }
  } else {
    if (pos) {
      // do nothing
    } else {
      FnObj tmpfn = te_get_negate_func(ret_type);

      if (!tmpfn.is_valid()) {
#ifdef TESL_DEBUG_COMPILE
        tesl_printf("error: cannot negate ");
        te_print(ret_type);
        tesl_printf("!\n");
#endif
        te_make_error(s);
        te_free(ret);
        ret = nullptr;
        return nullptr;
      }

      ret = new_call_expr(tmpfn, {ret});
    }
  }

  ret_type = ret ? ret->type : TYPE_ERROR;

  int postfix = 0;
  if (s.token == Token::PLUS_PLUS) {
    postfix = 1;
    s.advance();
  } else if (s.token == Token::MINUS_MINUS) {
    postfix = -1;
    s.advance();
  }

  er.set_end(s.token.name.end);

  if (postfix) {
    if (ret_type != TYPE_INT_REF) {
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: only int ref can be incremented/decremented! type: ");
      te_print(ret_type);
      tesl_printf("\n");
#endif
      te_make_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    Type param_type = TYPE_INT_REF;
    if (postfix > 0) {
      ret = new_call_expr(te::detail::make_function_raw(post_increment, nullptr, true, TYPE_INT_VAL, {TYPE_INT_REF}), {ret});
    } else {
      ret = new_call_expr(te::detail::make_function_raw(post_decrement, nullptr, true, TYPE_INT_VAL, {TYPE_INT_REF}), {ret});
    }
  }

#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("exited factor\n");
#endif

  return ret;
}

static te_expr * term(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("entered term\n");
#endif

  // <term>      =    <factor> {("*" | "/" | "%") <factor>}+
  ErrorRecord er{s};
  te_expr * ret = factor(s);
  Type ret_type = ret ? ret->type : TYPE_ERROR;

  do {
    er.set_point(s.token.name.ptr);

    Token::Kind tt = Token::NONE;
    char c = '?';
    if (s.token == Token::STAR || s.token == Token::SLASH || s.token == Token::PERCENT) {
      tt = s.token.kind;
      c = *s.token.name.ptr;
    } else {
      break;
    }
    s.advance();
    te_expr * rhs = factor(s);
    Type rhs_type = rhs ? rhs->type : TYPE_ERROR;

    er.set_end(s.prev_token.name.end);

    FnObj fn;
    switch (tt) {
      case Token::STAR:
        fn = te_get_mul_func(Type(ret_type | TYPE_FLAG_CONSTANT), Type(rhs_type | TYPE_FLAG_CONSTANT));
        break;
      case Token::SLASH:
        fn = te_get_div_func(Type(ret_type | TYPE_FLAG_CONSTANT), Type(rhs_type | TYPE_FLAG_CONSTANT));
        break;
      case Token::PERCENT:
        fn = te_get_mod_func(Type(ret_type | TYPE_FLAG_CONSTANT), Type(rhs_type | TYPE_FLAG_CONSTANT));
        break;
      default:
        fn = FnObj{};
        break;
    }

    if (!fn.is_valid()) {
      if (ret_type != TYPE_ERROR && rhs_type != TYPE_ERROR) {
#ifdef TESL_DEBUG_COMPILE
        tesl_printf("error: op '%c' not found for ", c);
        te_print(ret_type);
        tesl_printf(" and ");
        te_print(rhs_type);
        tesl_printf("!\n");
#endif
        te_make_error(s);
      }
      te_free(ret);
      ret = nullptr;
      te_free(rhs);
      rhs = nullptr;
    } else {
      te_expr * args[] = {ret, rhs};
      ret = new_call_expr(fn, {ret, rhs});
    }

    ret_type = ret ? ret->type : TYPE_ERROR;
  } while (true);

#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("exited term\n");
#endif

  return ret;
}

static te_expr * expr(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("entered expr\n");
#endif

  // <expr>      =    <term> {("+" | "-") <term>}+
  ErrorRecord er{s};
  te_expr * ret = term(s);
  Type ret_type = ret ? ret->type: TYPE_ERROR;

  do {
    er.set_point(s.token.name.ptr);

    Token::Kind tt = Token::NONE;
    char c = '?';
    if (s.token == Token::PLUS || s.token == Token::MINUS) {
      tt = s.token.kind;
      c = *s.token.name.ptr;
    } else {
      break;
    }
    s.advance();
    te_expr * rhs = term(s);
    Type rhs_type = rhs ? rhs->type : TYPE_ERROR;

    er.set_end(s.prev_token.name.end);

    FnObj fn;
    switch (tt) {
      case Token::PLUS:
        fn = te_get_add_func(Type(ret_type | TYPE_FLAG_CONSTANT), Type(rhs_type | TYPE_FLAG_CONSTANT));
        break;
      case Token::MINUS:
        fn = te_get_sub_func(Type(ret_type | TYPE_FLAG_CONSTANT), Type(rhs_type | TYPE_FLAG_CONSTANT));
        break;
      default:
        fn = FnObj{};
        break;
    }

    if (!fn.is_valid()) {
      if (ret_type != TYPE_ERROR && rhs_type != TYPE_ERROR) {
#ifdef TESL_DEBUG_COMPILE
        tesl_printf("error: op '%c' not found for ", c);
        te_print(ret_type);
        tesl_printf(" and ");
        te_print(rhs_type);
        tesl_printf("!\n");
#endif
        te_make_error(s);
      }
      te_free(ret);
      ret = nullptr;
      te_free(rhs);
      rhs = nullptr;
    } else {
      ret = new_call_expr(fn, {ret, rhs});
    }

    ret_type = ret ? ret->type : TYPE_ERROR;
  } while (true);

#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("exited expr\n");
#endif

  return ret;
}*/


namespace tesl {
  namespace util {
    /*template<typename T, auto ... Vs>
    struct first_value_of_type;

    template<typename T, auto V, auto ... NextVs>
    struct first_value_of_type<T, V, NextVs...> : public first_value_of_type<T, NextVs...> {
    };

    template<typename T, T V, auto ... NextVs>
    struct first_value_of_type<T, V, NextVs...> {
      static constexpr T value = V;
      static constexpr bool exists = true;
    };

    template<typename T>
    struct first_value_of_type<T> {
      static constexpr bool exists = false;
    };

    template<auto ... Vs>
    struct first_value_of;

    template<typename V, typename ... Vs>
    struct first_value_of<V, Vs...> {
      static constexpr auto value = V;
    };*/

  }

}

static te_expr * parse_fn(te_parser_state & s) {
  /* <fn>        =    <typename> <identifier> "(" {<typename> {<identifier>} "," }+ ")" <stmt> */
  return nullptr;
}

static void begin_function(te_parser_state & s) {
  s.parse_error = false;
}

static te_expr * end_function(te_parser_state & s) {
  te_expr * ret = nullptr;

#ifdef TESL_DEBUG_COMPILE
  if (!s.parse_error) {
    for (int i = 0; i < s.stmt_count; ++i) {
      if (s.stmts[i] == nullptr) {
        tesl_printf("internal error: null stmt despite no parser error!\n");
        s.parse_error = true;
      }
    }
    s.stmt_count = 0;
  }
#endif

  if (s.parse_error) { // :3 was here
    for (int i = 0; i < s.stmt_count; ++i) {
      te_free(s.stmts[i]);
      s.stmts[i] = nullptr;
    }
    s.stmt_count = 0;
  } else {
    ret = new_suite_expr(s.return_type, s.stack_size, s.stmts, s.stmt_count);
    s.stmt_count = 0;
  }

  return ret;
}

static void push_stmt(te_parser_state & s, te_op * e) {
  if (s.stmt_count < TE_MAX_STMT_COUNT) {
    s.stmts[s.stmt_count++] = e;
  } else {
    ErrorRecord er{s};
#ifdef TESL_DEBUG_COMPILE
    tesl_printf("error: ran out of room for stmts! max=%d\n", TE_MAX_STMT_COUNT);
#endif
    te_make_error(s);
  }
}

static void parse_stmt(te_parser_state & s);

static void parse_suite(te_parser_state & s, Token::Kind end_token) {
#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("entered suite\n");
#endif

  {
    te_scope_decl scope_decl(s);

    te_expr * ret = nullptr;
    while (true) {
      if (s.curr_token == end_token) {
        s.advance();
        break;
      } else if (s.curr_token == Token::END) {
        ErrorRecord er2{s};
  #ifdef TESL_DEBUG_COMPILE
        tesl_printf("error: end of input!\n");
  #endif
        te_make_error(s);
        break;
      }

      parse_stmt(s);
    }
  }

#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("exited suite: stmt_count=%d\n", s.stmt_count);
#endif
}

static te_expr * parse_standalone_suite(te_parser_state & s) {
  begin_function(s);
  parse_suite(s, Token::END);
  return end_function(s);
}

static void parse_if_stmt(te_parser_state & s) {
  s.advance();

  if (s.curr_token != Token::OPEN_PAREN) {
    ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
    tesl_printf("error: expected '(' got ");
    te_print(s.curr_token);
    tesl_printf("!\n");
#endif
    te_make_error(s);
  }
  s.advance();

  ErrorRecord er{s};
  te_expr * cond_expr = tesl::parse::parse_top_level_expr(s);
  er.set_end(s.curr_token.name.end);

  Type cond_expr_result_type = cond_expr ? cond_expr->type : TYPE_ERROR;
  if ((cond_expr_result_type | TYPE_FLAG_CONSTANT) != TYPE_INT_VAL) {
#ifdef TESL_DEBUG_COMPILE
    tesl_printf("error: conditional expr must be an int! type: ");
    te_print(cond_expr_result_type);
    tesl_printf("\n");
#endif
    te_make_error(s);
  }

  if (s.curr_token != Token::CLOSE_PAREN) {
    ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
    tesl_printf("error: expected ')' got ");
    te_print(s.curr_token);
    tesl_printf("!\n");
#endif
    te_make_error(s);
  }
  s.advance();

  te_jmp_if_not_op * jmp_if_expr = new_jmp_if_not_op(te_jmp_op::unknown_offset, cond_expr);
  push_stmt(s, jmp_if_expr);

  parse_stmt(s);

  if (s.curr_token == Token::ELSE) {
    s.advance();
    te_jmp_op * jmp_end_expr = new_jmp_op(te_jmp_op::unknown_offset);
    push_stmt(s, jmp_end_expr);
    if (jmp_if_expr) jmp_if_expr->offset = s.stmt_count;
    parse_stmt(s);
    jmp_end_expr->offset = s.stmt_count;
  } else {
    if (jmp_if_expr) jmp_if_expr->offset = s.stmt_count;
  }
}

static void parse_var_stmt(te_parser_state & s, bool allow_assign) {
  Type decl_type = s.curr_token.type;
  s.advance();

  if (s.curr_token != Token::IDENTIFIER) {
    ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
    tesl_printf("error: expected <identifier> got ");
    te_print(s.curr_token);
    tesl_printf("!\n");
#endif
    te_make_error(s);
    return;
  }
  te_parser_state::local_var_t * var = te_stack_allocate_var(s, s.curr_token.name, decl_type);
  s.advance();

  if (allow_assign && s.curr_token == Token::EQUAL) {
    s.advance();

    te_expr *initializer = tesl::parse::parse_expr(s, -2);
    push_stmt(s, new_assign_expr(var ? new_stack_ref_expr(var->type, var->offset) : nullptr, initializer));
  }
}

static void parse_for_stmt(te_parser_state & s) {
  s.advance();

  if (s.curr_token != Token::OPEN_PAREN) {
    ErrorRecord er{s};
#ifdef TESL_DEBUG_COMPILE
    tesl_printf("error: expected '(' got ");
    te_print(s.curr_token);
    tesl_printf("!\n");
#endif
    te_make_error(s);
  }
  s.advance();

  {
    te_scope_decl scope_decl(s);

    parse_var_stmt(s, true);

    if (s.curr_token != Token::SEMICOLON) {
      ErrorRecord er{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: expected ';' got ");
      te_print(s.curr_token);
      tesl_printf("!\n");
#endif
      te_make_error(s);
    }
    s.advance();

    ErrorRecord er{s};
    te_expr * cond_expr = tesl::parse::parse_top_level_expr(s);
    er.set_end(s.curr_token.name.end);

    Type cond_expr_result_type = cond_expr ? cond_expr->type : TYPE_ERROR;
    if ((cond_expr_result_type | TYPE_FLAG_CONSTANT) != TYPE_INT_VAL) {
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: conditional expr must be an int! type: ");
      te_print(cond_expr_result_type);
      tesl_printf("\n");
#endif
      te_make_error(s);
    }

    if (s.curr_token != Token::SEMICOLON) {
      ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: expected ';' got ");
      te_print(s.curr_token);
      tesl_printf("!\n");
#endif
      te_make_error(s);
    }
    s.advance();

    te_jmp_op * jmp_cond_expr = new_jmp_op(te_jmp_op::unknown_offset);
    push_stmt(s, jmp_cond_expr);

    te_expr * update_expr = tesl::parse::parse_top_level_expr(s);

    if (s.curr_token != Token::CLOSE_PAREN) {
      ErrorRecord er2{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: expected ')' got ");
      te_print(s.curr_token);
      tesl_printf("!\n");
#endif
      te_make_error(s);
    }
    s.advance();

    int32_t loop_start_offset = s.stmt_count;

    parse_stmt(s);

    push_stmt(s, update_expr);

    int32_t loop_cond_offset = s.stmt_count;
    te_jmp_if_op * jmp_if_expr = new_jmp_if_op(loop_start_offset, cond_expr);
    push_stmt(s, jmp_if_expr);

    if (jmp_cond_expr) {
      jmp_cond_expr->offset = loop_cond_offset;
    }
  }
}

static void parse_return_stmt(te_parser_state & s) {
  s.advance();

  ErrorRecord er{s};

  if (s.curr_token == Token::SEMICOLON) {
    if (s.return_type != TYPE_NULL_VAL) {
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: expected ");
      te_print(s.return_type);
      tesl_printf(" expression!\n");
#endif
      te_make_error(s);
    }
  } else {
    te_expr * e = tesl::parse::parse_expr(s, -2);
    Type result_type = e ? e->type : TYPE_ERROR;

    er.set_end(s.prev_token.name.end);

    if (result_type == TYPE_ERROR) {
      s.parse_error = true;
    } else if ((result_type | TYPE_FLAG_CONSTANT) != s.return_type) {
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: 'return' cannot convert ");
      te_print(result_type);
      tesl_printf(" to ");
      te_print(s.return_type);
      tesl_printf("\n");
#endif
      te_make_error(s);
    } else {
      push_stmt(s, new_return_op(s.return_type, e));
    }
  }
}

static void parse_stmt(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  tesl_printf("entered stmt\n");
#endif

  /* <stmt>      =    ("{" {<stmt>}+ "}" | "if" "(" <int-expr> ")" <stmt> {"else" <stmt>} | ("return" <vexpr> | "break" | "continue" | <typename> <id> {"=" <vexpr>} | <rexpr> ("=" | "+=" | "-=" | "*=" | "/=" | "%=") <vexpr> | <expr> ";")) */
  if (s.curr_token == Token::OPEN_CURLY_BRACKET) {
    s.advance();
    parse_suite(s, Token::CLOSE_CURLY_BRACKET);
  } else if (s.curr_token == Token::IF) {
    parse_if_stmt(s);
  } else if (s.curr_token == Token::FOR) {
    parse_for_stmt(s);
  } else {
    if (s.curr_token == Token::TYPENAME) {
      parse_var_stmt(s, true);
    } else if (s.curr_token == Token::RETURN) {
      parse_return_stmt(s);
    } else {
      const char * token_start = s.curr_token.name.ptr;
      push_stmt(s, tesl::parse::parse_expr(s, -2));
      if (s.curr_token.name.ptr == token_start) {
        // don't get stuck
        s.advance();
      }
    }

    if (s.curr_token == Token::SEMICOLON) {
      s.advance();
    } else {
      ErrorRecord er{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: expected ';', got ");
      te_print(s.curr_token);
      tesl_printf("!\n");
#endif
      te_make_error(s);
    }
  }
}

static te_expr * parse_program(te_parser_state & s) {
  /* <program>   =    {{"const" <var> | "uniform" <var> | <fn>} ";"}+ */
  // TODO: finish
  s.return_type = TYPE_VEC3_VAL;
  return parse_standalone_suite(s);
}

void te_eval_internal(const te_expr * n, char * p_stack, te_value * ret) {
#ifdef TESL_DEBUG_EVAL

#define TE_ERR_FAIL_COND(cond, fail_action, ...)\
  do {\
    if (cond) {\
        tesl_printf(__VA_ARGS__);\
        tesl_printf("\n");\
        fail_action;\
    }\
  } while(false)

#define TE_CHECK_EXPECTED_TYPE(chk_type, expected, fail_action, ...)\
  do {\
    if ((chk_type) != (expected)) {\
        tesl_printf(__VA_ARGS__);\
        tesl_printf(" expected ");\
        te_print(Type(expected));\
        tesl_printf(", got ");\
        te_print(Type(chk_type));\
        tesl_printf("\n");\
    }\
  } while(false)

#else

#define TE_ERR_FAIL_COND(cond, expected, fail_action, ...)
#define TE_CHECK_EXPECTED_TYPE(chk_type, expected, fail_action, ...)

#endif

  TE_ERR_FAIL_COND(!n, return, "eval error: null expr!");

  switch (n->opcode) {
    case TE_OP_NONE:
      break;
    case TE_OP_ERROR: {
      TE_ERR_FAIL_COND(true, return, "eval error: error opcode!");
    } break;
    case TE_OP_VALUE: {
#ifdef TE_DEBUG_PEDANTIC
      tesl_printf("TE_OP_VALUE\n");
#endif
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_value_expr * value_expr = reinterpret_cast<const te_value_expr *>(n);
      memcpy(ret, &value_expr->value, value_expr->size);
    } break;
    case TE_OP_STACK_REF: {
#ifdef TE_DEBUG_PEDANTIC
      tesl_printf("TE_OP_STACK_REF\n");
#endif
      TE_ERR_FAIL_COND(!p_stack, return, "eval error: stack ptr is null!");
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_stack_ref_expr * stack_ref_expr = reinterpret_cast<const te_stack_ref_expr *>(n);
      te_value * ref = reinterpret_cast<te_value *>(p_stack + stack_ref_expr->offset);
      ret->ref = ref;
    } break;
    case TE_OP_STACK_REF_REF: {
#ifdef TE_DEBUG_PEDANTIC
      tesl_printf("TE_OP_STACK_REF_REF\n");
#endif
      TE_ERR_FAIL_COND(!p_stack, return, "eval error: stack ptr is null!");
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_stack_ref_expr * stack_ref_expr = reinterpret_cast<const te_stack_ref_expr *>(n);
      te_value * ref = *reinterpret_cast<te_value **>(p_stack + stack_ref_expr->offset);
      ret->ref = ref;
    } break;
    case TE_OP_DEREF: {
#ifdef TE_DEBUG_PEDANTIC
      tesl_printf("TE_OP_DEREF\n");
#endif
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_deref_expr * deref_expr = reinterpret_cast<const te_deref_expr *>(n);
      void * ref;
      te_eval_internal(deref_expr->arg, p_stack, reinterpret_cast<te_value *>(&ref));
      memcpy(ret, ref, deref_expr->size);
    } break;
    case TE_OP_ASSIGN: {
#ifdef TE_DEBUG_PEDANTIC
      tesl_printf("TE_OP_ASSIGN\n");
#endif
      TE_ERR_FAIL_COND(!p_stack, return, "eval error: stack ptr is null!");
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_assign_expr * assign_expr = reinterpret_cast<const te_assign_expr *>(n);
      TE_ERR_FAIL_COND(!tesl::is_ref(assign_expr->lhs->type), return, "eval error: lhs is not a ref!");

      void * lhs;
      te_eval_internal(assign_expr->lhs, p_stack, reinterpret_cast<te_value *>(&lhs));
      TE_ERR_FAIL_COND(!lhs, return, "eval error: lhs ref is null!");

      TE_CHECK_EXPECTED_TYPE(assign_expr->lhs->type | TYPE_FLAG_CONSTANT, assign_expr->rhs->type, return, "eval error: assignment lhs and rhs do not match!");
      te_eval_internal(assign_expr->rhs, p_stack, reinterpret_cast<te_value *>(lhs));
      ret->ptr = lhs;
    } break;
    case TE_OP_CALL: {
#ifdef TE_DEBUG_PEDANTIC
      tesl_printf("TE_OP_CALL\n");
#endif
      const te_call_expr * call_expr = reinterpret_cast<const te_call_expr *>(n);
      char * stack = static_cast<char *>(alloca(call_expr->arg_stack_size));
      {
        char * stackptr = stack;
        Type param_type = TYPE_ERROR;
        uint8_t param_count = call_expr->fn.param_count;
        uint8_t param_size = 0;
        for (int i = 0; i < param_count; ++i, stackptr += param_size) {
          param_type = call_expr->fn.param_types[i];
          param_size = sizeof_type(param_type);

          TE_ERR_FAIL_COND(!call_expr->args[i], continue, "eval error: fn arg expr ptr is null!");
          TE_CHECK_EXPECTED_TYPE(call_expr->args[i]->type, param_type, continue, "eval error: argument and parameter types do not match!");
          te_eval_internal(call_expr->args[i], p_stack, reinterpret_cast<te_value *>(stackptr));
        }
      }

      TE_ERR_FAIL_COND(!call_expr->fn.is_valid(), return, "eval error: null function call!");
      TE_ERR_FAIL_COND(!ret && call_expr->type != TYPE_NULL_VAL, return, "eval error: return ptr is null but function returns a value!");

      call_expr->fn.ptr(call_expr->fn.context, stack, ret);
    } break;
    case TE_OP_SUITE: {
#ifdef TE_DEBUG_PEDANTIC
      tesl_printf("TE_OP_SUITE\n");
#endif
      const te_suite_expr * suite_expr = reinterpret_cast<const te_suite_expr *>(n);
      char * stack = static_cast<char *>(alloca(suite_expr->stack_size));

      for (int32_t i = 0; i < suite_expr->stmt_count;) {
        te_op * stmt = suite_expr->stmts[i];
        switch (stmt->opcode) {
          case TE_OP_JMP: {
#ifdef TE_DEBUG_PEDANTIC
            tesl_printf("TE_OP_JMP\n");
#endif
            const te_jmp_op * jmp_op = reinterpret_cast<const te_jmp_op *>(stmt);
            TE_ERR_FAIL_COND(jmp_op->offset == te_jmp_op::unknown_offset, return, "eval error: unknown offset!");
            i = jmp_op->offset;
            continue;
          } break;
          case TE_OP_JMP_REF: {
#ifdef TE_DEBUG_PEDANTIC
            tesl_printf("TE_OP_JMP_REF\n");
#endif
            const te_jmp_ref_op * jmp_ref_op = reinterpret_cast<const te_jmp_ref_op *>(stmt);
            const uint16_t offset = *jmp_ref_op->offset_ref;
            TE_ERR_FAIL_COND(offset == te_jmp_op::unknown_offset, return, "eval error: unknown offset!");
            i = offset;
            continue;
          } break;
          case TE_OP_JMP_IF: {
#ifdef TE_DEBUG_PEDANTIC
            tesl_printf("TE_OP_JMP_IF\n");
#endif
            const te_jmp_if_op * jmp_if_op = reinterpret_cast<const te_jmp_if_op *>(stmt);
            TE_CHECK_EXPECTED_TYPE(jmp_if_op->condition->type, TYPE_INT_VAL, return, "eval error: incorrect type for conditional!");
            TE_ERR_FAIL_COND(jmp_if_op->offset == te_jmp_op::unknown_offset, return, "eval error: unknown offset!");

            IntT cond = 0;
            te_eval_internal(jmp_if_op->condition, stack, reinterpret_cast<te_value *>(&cond));

            if (cond) {
              i = jmp_if_op->offset;
              continue;
            }
          } break;
          case TE_OP_JMP_IF_NOT: {
#ifdef TE_DEBUG_PEDANTIC
            tesl_printf("TE_OP_JMP_IF_NOT\n");
#endif
            const te_jmp_if_not_op * jmp_if_not_op = reinterpret_cast<const te_jmp_if_not_op *>(stmt);
            TE_CHECK_EXPECTED_TYPE(jmp_if_not_op->condition->type, TYPE_INT_VAL, return, "eval error: incorrect type for conditional!");
            TE_ERR_FAIL_COND(jmp_if_not_op->offset == te_jmp_op::unknown_offset, return, "eval error: unknown offset!");

            IntT cond = 0;
            te_eval_internal(jmp_if_not_op->condition, stack, reinterpret_cast<te_value *>(&cond));

            if (!cond) {
              i = jmp_if_not_op->offset;
              continue;
            }
          } break;
          case TE_OP_RETURN: {
#ifdef TE_DEBUG_PEDANTIC
            tesl_printf("TE_OP_RETURN\n");
#endif
            const te_return_op * return_expr = reinterpret_cast<const te_return_op *>(stmt);
            TE_CHECK_EXPECTED_TYPE(return_expr->arg->type, n->type, return, "eval error: incorrect return type!");

            te_eval_internal(return_expr->arg, stack, ret);
            return;
          } break;
          case TE_OP_NONE:
          case TE_OP_ERROR:
          case TE_OP_VALUE:
          case TE_OP_STACK_REF:
          case TE_OP_STACK_REF_REF:
          case TE_OP_DEREF:
          case TE_OP_ASSIGN:
          case TE_OP_CALL:
          case TE_OP_SUITE: {
#ifdef TE_DEBUG_PEDANTIC
            tesl_printf("eval stmt expr\n");
#endif
            const te_expr * expr = reinterpret_cast<const te_expr *>(stmt);
            te_value trash;
            te_eval_internal(expr, stack, &trash);
          } break;
        }
        i++;
      }
    } break;
    case TE_OP_JMP:
    case TE_OP_JMP_REF:
    case TE_OP_JMP_IF:
    case TE_OP_JMP_IF_NOT:
    case TE_OP_RETURN: {
      TE_ERR_FAIL_COND(true, return, "eval error: op 0x%02x evaluated as a standalone expr!", int(n->opcode));
    } break;
    default: {
      TE_ERR_FAIL_COND(true, return, "eval error: unknown op 0x%02x!", int(n->opcode));
    } break;
  }
  if (tesl::is_ref(n->type) && !ret->ref)
  TE_ERR_FAIL_COND(tesl::is_ref(n->type) && !ret->ref, return, "eval error: null reference!");
}

static void te_optimize(te_op * op) {
  // TODO: messes with fragmenting of heap
  /* Only optimize out functions flagged as pure. */
  if (!op) {
    return;
  }

  switch (op->opcode) {
    case TE_OP_NONE:
    case TE_OP_ERROR:
    case TE_OP_VALUE:
    case TE_OP_STACK_REF:
    case TE_OP_STACK_REF_REF:
      break;
    case TE_OP_DEREF:
      te_optimize(reinterpret_cast<te_deref_expr *>(op)->arg);
      break;
    case TE_OP_ASSIGN:
      te_optimize(reinterpret_cast<te_assign_expr *>(op)->lhs);
      te_optimize(reinterpret_cast<te_assign_expr *>(op)->rhs);
      break;
    case TE_OP_CALL: {
      te_call_expr * call_expr = reinterpret_cast<te_call_expr *>(op);
      bool are_any_args_refs = false;
      for (int i = 0; i > call_expr->fn.param_count; ++i) {
        te_optimize(call_expr->args[i]);
        if (!call_expr->args[i] || tesl::is_ref(call_expr->args[i]->type)) {
          are_any_args_refs = true;
        }
      }
      if (!are_any_args_refs && call_expr->fn.pure) {
        int value_expr_size = sizeof(te_value_expr) - sizeof(te_value) + sizeof_type(call_expr->type);
        // Only try if it fits in the already allocated obj.
        if (value_expr_size <= te_size_of_op(call_expr)) {
          te_value reduced; // just to be safe, use a stack var instead of overwriting expr value
          te_eval_internal(call_expr, nullptr, &reduced);
          te_free_args(call_expr);
          te_value_expr * value_expr = reinterpret_cast<te_value_expr *>(op);
          value_expr->opcode = TE_OP_VALUE;
          // Type is the same.
          value_expr->size = sizeof_type(value_expr->type);
          memcpy(&value_expr->value, &reduced, value_expr->size);
        }
      }
    } break;
    case TE_OP_SUITE: {
      te_suite_expr * suite_expr = reinterpret_cast<te_suite_expr *>(op);
      for (int i = 0; i < suite_expr->stmt_count; ++i) {
        te_optimize(suite_expr->stmts[i]);
      }
    } break;
    case TE_OP_JMP:
    case TE_OP_JMP_REF:
      break;
    case TE_OP_JMP_IF:
      te_optimize(reinterpret_cast<te_jmp_if_op *>(op)->condition);
      break;
    case TE_OP_JMP_IF_NOT:
      te_optimize(reinterpret_cast<te_jmp_if_not_op *>(op)->condition);
      break;
    case TE_OP_RETURN:
      te_optimize(reinterpret_cast<te_return_op *>(op)->arg);
      break;
  }
}

TypedValueT te_eval(const te_expr * e) {
  if (!e) {
    return {te_value{}, TYPE_ERROR};
  }

  TypedValueT ret;
  te_eval_internal(e, nullptr, &ret);
  ret.type = e->type;
  return ret;
}

void te_program::optimize() const {
  te_optimize(root_expr);
}

template<auto ParseFn>
static te_program te_compile_internal(const char * expression, Variable * variables, int var_count, Type result_type = TYPE_NULL_VAL) {
  te_parser_state s;
  s.return_type = result_type;
  s.program = s.line_start = expression;
  s.next_token.name = {expression, 0};
  s.curr_token = s.next_token;
  s.prev_token = s.curr_token;
  s.global_vars = variables;
  s.global_var_count = var_count;

  s.advance();
  s.advance();
  te_expr * root = ParseFn(s);

  if (s.curr_token != Token::END) {
    if (root && root->type != TYPE_ERROR) {
      ErrorRecord er{s};
#ifdef TESL_DEBUG_COMPILE
      tesl_printf("error: expected end!\n");
#endif
      te_make_error(s);
    }
    te_free(root);
    root = nullptr;
  }

  return {root, s.prev_error};
}

te_program te_compile_program(const char * expression, Variable * variables, int var_count) {
  return te_compile_internal<parse_program>(expression, variables, var_count);
}

te_program te_compile_suite(const char * expression, Variable * variables, int var_count, Type result_type) {
  return te_compile_internal<parse_standalone_suite>(expression, variables, var_count, result_type);
}

te_program te_compile_expr(const char * expression, Variable * variables, int var_count) {
  return te_compile_internal<tesl::parse::parse_top_level_expr>(expression, variables, var_count);
}

TypedValueT te_interp(const char * expression, ErrorRecord * error) {
  te_program prog = te_compile_expr(expression, nullptr, 0);
  if (error) {
    *error = prog.error;
  }

  TypedValueT ret;
  ret.type = TYPE_ERROR;
  if (prog.root_expr) {
    ret.type = prog.root_expr->type;
    te_eval_internal(prog.root_expr, nullptr, &ret);
  }

  return ret;
}

void te_print(const TypedValueT & v) {
  Type type = v.type;
  if (type == TYPE_FUNCTION) {
    tesl_printf("fn@0x%p(", v.fn.ptr);
    for (int i = 0; i < v.fn.param_count; i++) {
      if (i == 0) {
        if (v.fn.context) {
          tesl_printf("context=0x%p, ", v.fn.context);
        }
      } else {
        tesl_printf(", ");
      }
      te_print(v.fn.param_types[i]);
    }
    tesl_printf(") -> ");
    te_print(v.fn.return_type);
  } else if (tesl::is_ref(type)) {
    te_print(type);
    tesl_printf("(@0x%p)", v.ref);
  } else {
    switch (type) {
      case TYPE_ERROR:
        tesl_printf("<error>");
        break;
      case TYPE_NULL_VAL:
        tesl_printf("<null>");
        break;
      case TYPE_INT_VAL:
        tesl_printf("%ld", long(v.int_));
        break;
      case TYPE_FLOAT_VAL:
        tesl_printf("%f", v.float_);
        break;
      case TYPE_VEC2_VAL:
        tesl_printf("vec2(%f, %f)", v.vec2.x, v.vec2.y);
        break;
      case TYPE_VEC3_VAL:
        tesl_printf("vec3(%f, %f, %f)", v.vec3.x, v.vec3.y, v.vec3.z);
        break;
      case TYPE_VEC4_VAL:
        tesl_printf("vec4(%f, %f, %f, %f)", v.vec4.x, v.vec4.y, v.vec4.z, v.vec4.w);
        break;
      case TYPE_MAT2_VAL:
        tesl_printf("mat2(");
        te_print({*reinterpret_cast<const te_value *>(&v.mat2.arr[0]), TYPE_VEC2_VAL});
        tesl_printf(", ");
        te_print({*reinterpret_cast<const te_value *>(&v.mat2.arr[1]), TYPE_VEC2_VAL});
        tesl_printf(")");
        break;
      case TYPE_MAT3_VAL:
        tesl_printf("mat3(");
        te_print({*reinterpret_cast<const te_value *>(&v.mat3.arr[0]), TYPE_VEC3_VAL});
        tesl_printf(", ");
        te_print({*reinterpret_cast<const te_value *>(&v.mat3.arr[1]), TYPE_VEC3_VAL});
        tesl_printf(", ");
        te_print({*reinterpret_cast<const te_value *>(&v.mat3.arr[2]), TYPE_VEC3_VAL});
        tesl_printf(")");
        break;
      case TYPE_MAT4_VAL:
        tesl_printf("mat4(");
        te_print({*reinterpret_cast<const te_value *>(&v.mat4.arr[0]), TYPE_VEC4_VAL});
        tesl_printf(", ");
        te_print({*reinterpret_cast<const te_value *>(&v.mat4.arr[1]), TYPE_VEC4_VAL});
        tesl_printf(", ");
        te_print({*reinterpret_cast<const te_value *>(&v.mat4.arr[2]), TYPE_VEC4_VAL});
        tesl_printf(", ");
        te_print({*reinterpret_cast<const te_value *>(&v.mat4.arr[3]), TYPE_VEC4_VAL});
        tesl_printf(")");
        break;
      case TYPE_STR_VAL:
        tesl_printf(v.str);
        break;
      default:
        tesl_printf("<unknown:0x%04x>", int(type));
        break;
    }
  }
}

static const char swizzle_chars[] = "xyzw";

static void pn(const te_op * op, int depth) {
  if (!op) {
    tesl_printf("<nullptr>\n");
    return;
  }

  tesl_printf("%*s", depth, "");
  switch (op->opcode) {
    case TE_OP_NONE: {
      tesl_printf("none\n");
    } break;
    case TE_OP_ERROR: {
      tesl_printf("error\n");
    } break;
    case TE_OP_VALUE: {
      const te_value_expr * value_expr = reinterpret_cast<const te_value_expr *>(op);
      te_print({value_expr->value, value_expr->type});
      tesl_printf("\n");
    } break;
    case TE_OP_STACK_REF:
    case TE_OP_STACK_REF_REF: {
      const te_stack_ref_expr * stack_ref_expr = reinterpret_cast<const te_stack_ref_expr *>(op);
      te_print(stack_ref_expr->type);
      tesl_printf("(stack@0x%04x)", int(stack_ref_expr->offset));
    } break;
    case TE_OP_DEREF: {
      const te_deref_expr * deref_expr = reinterpret_cast<const te_deref_expr *>(op);
      tesl_printf("deref\n");
      pn(deref_expr->arg, depth + 1);
    } break;
    case TE_OP_ASSIGN: {
      const te_assign_expr * assign_expr = reinterpret_cast<const te_assign_expr *>(op);
      tesl_printf("assign\n");
      pn(assign_expr->lhs, depth + 1);
      pn(assign_expr->rhs, depth + 1);
    } break;
    case TE_OP_CALL: {
      const te_call_expr * call_expr = reinterpret_cast<const te_call_expr *>(op);
      if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_add_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        tesl_printf("add\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_sub_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        tesl_printf("sub\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_mul_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        tesl_printf("mul\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_div_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        tesl_printf("div\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_mod_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        tesl_printf("mod\n");
      } else if (call_expr->fn.param_count == 1 && call_expr->fn.ptr == te_get_negate_func(call_expr->args[0]->type).ptr) {
        tesl_printf("negate\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_index_func(call_expr->args[0]->type).ptr && call_expr->args[1]->opcode == TE_OP_VALUE && call_expr->args[1]->type == TYPE_INT_VAL) {
        tesl_printf("index.%ld\n", long(reinterpret_cast<const te_value_expr *>(call_expr->args[1])->value.int_));
      } else if (call_expr->fn.param_count == 3 && call_expr->fn.ptr == te_get_swizzle2_func(call_expr->args[0]->type).ptr && call_expr->args[1]->opcode == TE_OP_VALUE && call_expr->args[1]->type == TYPE_INT_VAL && call_expr->args[2]->opcode == TE_OP_VALUE && call_expr->args[2]->type == TYPE_INT_VAL) {
        tesl_printf("swizzle.%c%c\n", swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[1])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[2])->value.int_]);
      } else if (call_expr->fn.param_count == 4 && call_expr->fn.ptr == te_get_swizzle3_func(call_expr->args[0]->type).ptr && call_expr->args[1]->opcode == TE_OP_VALUE && call_expr->args[1]->type == TYPE_INT_VAL && call_expr->args[2]->opcode == TE_OP_VALUE && call_expr->args[2]->type == TYPE_INT_VAL && call_expr->args[3]->opcode == TE_OP_VALUE && call_expr->args[3]->type == TYPE_INT_VAL) {
        tesl_printf("swizzle.%c%c%c\n", swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[1])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[2])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[3])->value.int_]);
      } else if (call_expr->fn.param_count == 5 && call_expr->fn.ptr == te_get_swizzle4_func(call_expr->args[0]->type).ptr && call_expr->args[1]->opcode == TE_OP_VALUE && call_expr->args[1]->type == TYPE_INT_VAL && call_expr->args[2]->opcode == TE_OP_VALUE && call_expr->args[2]->type == TYPE_INT_VAL && call_expr->args[3]->opcode == TE_OP_VALUE && call_expr->args[3]->type == TYPE_INT_VAL && call_expr->args[4]->opcode == TE_OP_VALUE && call_expr->args[4]->type == TYPE_INT_VAL) {
        tesl_printf("swizzle.%c%c%c%c\n", swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[1])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[2])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[3])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[4])->value.int_]);
      } else if (call_expr->fn.ptr == (FnPtr) &pre_increment) {
        tesl_printf("pre-increment\n");
      } else if (call_expr->fn.ptr == (FnPtr) &post_increment) {
        tesl_printf("post-increment\n");
      } else if (call_expr->fn.ptr == (FnPtr) &pre_decrement) {
        tesl_printf("pre-decrement\n");
      } else if (call_expr->fn.ptr == (FnPtr) &post_decrement) {
        tesl_printf("post-decrement\n");
      } else if (call_expr->fn.ptr == (FnPtr) &bool_not) {
        tesl_printf("not\n");
      } else {
        bool found = false;
        for (int i = 0; i < te_builtins_count; ++i) {
          if (te_builtins[i].type == TYPE_FUNCTION && call_expr->fn.ptr == te_builtins[i].fn.ptr) {
            found = true;
            StrView fn_name = te_builtins[i].name;
            tesl_printf("%.*s", fn_name.len(), fn_name.ptr);
          }
        }

        if (!found) {
          te_print(TypedValueT{call_expr->fn, TYPE_FUNCTION});
        }
        tesl_printf("\n");
      }
      for (int i = 0; i < call_expr->fn.param_count; i++) {
        pn(call_expr->args[i], depth + 1);
      }
    } break;
    case TE_OP_SUITE: {
      const te_suite_expr * suite_expr = reinterpret_cast<const te_suite_expr *>(op);
      tesl_printf("suite\n");
      for (int i = 0; i < suite_expr->stmt_count; ++i) {
        pn(suite_expr->stmts[i], depth + 1);
      }
    } break;
    case TE_OP_JMP: {
      const te_jmp_op * jmp_op = reinterpret_cast<const te_jmp_op *>(op);
      tesl_printf("jmp to %d\n", int(jmp_op->offset));
    } break;
    case TE_OP_JMP_REF: {
      const te_jmp_ref_op * jmp_ref_op = reinterpret_cast<const te_jmp_ref_op *>(op);
      tesl_printf("jmp to ref %d\n", int(* jmp_ref_op->offset_ref));
    } break;
    case TE_OP_JMP_IF: {
      const te_jmp_if_op * jmp_if_op = reinterpret_cast<const te_jmp_if_op *>(op);
      tesl_printf("jmp to %d if\n", int(jmp_if_op->offset));
      pn(jmp_if_op->condition, depth + 1);
    } break;
    case TE_OP_JMP_IF_NOT: {
      const te_jmp_if_not_op * jmp_if_not_op = reinterpret_cast<const te_jmp_if_not_op *>(op);
      tesl_printf("jmp to %d if not\n", int(jmp_if_not_op->offset));
      pn(jmp_if_not_op->condition, depth + 1);
    } break;
    case TE_OP_RETURN: {
      const te_return_op * return_op = reinterpret_cast<const te_return_op *>(op);
      tesl_printf("return\n");
      pn(return_op->arg, depth + 1);
    } break;
  }
}

void te_print_expr(const te_expr * n) {
  pn(n, 0);
}

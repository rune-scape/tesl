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

//static_assert(TYPE_COUNT <= TE_TYPE_COUNT_MAX);


//static_assert(sizeof(void *) == 4, "made for 32bit :/ srry didnt have the energy to um yea");

inline void te_noop(void * context, void * args, void * ret) {}

struct te_expr;

// DO NOT STATICALLY ALLOCATE THESE
struct te_op {
  te_opcode opcode = TE_OP_NONE;
};
static_assert(sizeof(te_op) == 1);

struct te_expr : te_op {
  Type type = TYPE_ERROR;
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
  FnObj fn;
  te_expr *args[FLEXIBLE_ARRAY];
};
static_assert(sizeof(te_call_expr) == sizeof(void *) * 1 + sizeof(FnObj));

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

/* Frees the expression. */
/* This is safe to call on null pointers. */
void te_free(te_op * op);

/* Evaluates the expression. */
TypedValueT te_eval(const te_expr * e);

struct te_program {
  te_expr *root_expr = nullptr;
  ErrorRecord error;

  bool is_valid() const { return root_expr != nullptr; }
  TypedValueT eval() const { return te_eval(root_expr); }
  void optimize() const;

  te_program(te_expr * expr, const ErrorRecord & er);
  te_program & operator=(te_program &&);
  te_program(te_program && other);
  te_program & operator=(const te_program &) = delete;
  te_program(const te_program &) = delete;
  te_program() = default;
  ~te_program();
};

/* Parses the input expression, evaluates it, and frees it. */
/* Result type is TYPE_ERROR on error. */
TypedValueT te_interp(const char * expression, ErrorRecord * error = nullptr);

/* Parses the input as a program and binds variables. */
te_program te_compile_program(const char * expression, Variable * variables, int var_count);

/* Parses the input as a suite and binds variables. */
te_program te_compile_suite(const char * expression, Variable * variables, int var_count, Type result_type);

/* Parses the input as an expression and binds variables. */
te_program te_compile_expr(const char * expression, Variable * variables, int var_count);

/* Prints debugging information on the syntax tree. */
void te_print_expr(const te_expr * n);

void te_print(const TypedValueT & v);
void te_print(Type type);

#endif /*TESL_HPP*/

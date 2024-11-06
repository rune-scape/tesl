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
#include <cassert>
#include <cctype>
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

#include "tesl.hpp"

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

#ifndef PI
#define PI 3.14159265358979323846f
#endif

#ifdef TE_DEBUG_COMPILE
#define TE_FAIL_COND(cond, action)\
    if (cond) {\
        te_printf("error: \'" #cond "\' is true (at %s:%d)\n", __FILE__, __LINE__);\
        action;\
    } else ((void)0)
#define TE_FAIL_COND_MSG(cond, action, ...)\
    if (cond) {\
        te_printf(__VA_ARGS__);\
        action;\
    } else ((void)0)
#else
#define TE_FAIL_COND(cond, action) ((void)0)
#define TE_FAIL_COND_MSG(cond, action, str, ...) ((void)0)
#endif

#define TE_ASSERT(cond) if (!(cond)) { exit(-1); } else ((void)0)

struct te_token {
  enum kind_t : char {
    NONE,
    END,
    COMMA,
    DOT,
    SEMICOLON,
    COLON,
    QUESTION_MARK,
    TYPENAME,
    IDENTIFIER,
    OPEN_PAREN,
    CLOSE_PAREN,
    OPEN_SQUARE_BRACKET,
    CLOSE_SQUARE_BRACKET,
    OPEN_CURLY_BRACKET,
    CLOSE_CURLY_BRACKET,
    LITERAL,
    UNIFORM,
    IN,
    OUT,
    INOUT,
    IF,
    ELSE,
    SWITCH,
    CASE,
    FOR,
    WHILE,
    DO,
    BREAK,
    CONTINUE,
    RETURN,
    DISCARD,
    AND,
    AND_AND,
    CARET,
    CARET_CARET,
    PIPE,
    PIPE_PIPE,
    EQUAL,
    EQUAL_EQUAL,
    BANG,
    BANG_EQUAL,
    LESS,
    LESS_LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_GREATER,
    GREATER_EQUAL,
    PLUS,
    PLUS_PLUS,
    PLUS_EQUAL,
    MINUS,
    MINUS_MINUS,
    MINUS_EQUAL,
    STAR,
    STAR_EQUAL,
    SLASH,
    SLASH_EQUAL,
    PERCENT,
  } kind = NONE;
  te_type type = TE_ERROR;
  te_strview name;
  union {
    te_int int_value = 0;
    te_float float_value;
  };

  bool operator==(kind_t k) { return kind == k; }
  bool operator!=(kind_t k) { return kind != k; }

  bool operator==(te_token t) = delete;
  bool operator!=(te_token t) = delete;

  te_token & operator=(const te_token & t) = default;
  te_token(const te_token & t) = default;
  te_token() = default;
  te_token(kind_t k, te_strview n) : kind(k), name(n) {}

  static te_token make_int_literal(te_strview n, te_int i) {
    te_token ret;
    ret.kind = LITERAL;
    ret.type = TE_INT;
    ret.name = n;
    ret.int_value = i;
    return ret;
  }

  static te_token make_float_literal(te_strview n, te_float f) {
    te_token ret;
    ret.kind = LITERAL;
    ret.type = TE_FLOAT;
    ret.name = n;
    ret.float_value = f;
    return ret;
  }

  static te_token make_str_literal(te_strview n) {
    te_token ret;
    ret.kind = LITERAL;
    ret.type = TE_STR;
    ret.name = n;
    // Parse later ...
    return ret;
  }
};

bool operator==(te_token::kind_t k, te_token tok) { return k == tok.kind; }
bool operator!=(te_token::kind_t k, te_token tok) { return k != tok.kind; }

extern "C" {
  int _handle_extra_vsnprintf_spec(char spec, out_fct_type out, void* buffer, size_t idx, size_t maxlen, va_list *va) {
#define OUT_STR_(str) for (int i = 0; i < (sizeof(str) - 1); ++i) { out(str[i], buffer, idx++, maxlen); }
#define OUT_END_ return idx
#define CASE_(ty, str) case ty: OUT_STR_(str); return idx;

    static const char * hex_chars = "0123456789abcdef";

    switch (spec) {
      case 'y': {
        te_type type = (te_type)va_arg(*va, int);
        if (type == TE_FUNCTION) {
          OUT_STR_("<function>");
          OUT_END_;
        } else {
          switch (type) {
            CASE_(TE_ERROR, "<error>")
            CASE_(TE_INT_REF, "int&")
            CASE_(TE_FLOAT_REF, "float&")
            CASE_(TE_VEC2_REF, "vec2&")
            CASE_(TE_VEC3_REF, "vec3&")
            CASE_(TE_VEC4_REF, "vec4&")
            CASE_(TE_MAT2_REF, "mat2&")
            CASE_(TE_MAT3_REF, "mat3&")
            CASE_(TE_MAT4_REF, "mat4&")
            CASE_(TE_STR_REF, "str&")
            CASE_(TE_FUNCTION, "<function>")
            CASE_(TE_NULL, "void")
            CASE_(TE_INT, "int")
            CASE_(TE_FLOAT, "float")
            CASE_(TE_VEC2, "vec2")
            CASE_(TE_VEC3, "vec3")
            CASE_(TE_VEC4, "vec4")
            CASE_(TE_MAT2, "mat2")
            CASE_(TE_MAT3, "mat3")
            CASE_(TE_MAT4, "mat4")
            CASE_(TE_STR, "str")
            case TE_TYPE_COUNT:
              break;
          }
          OUT_STR_("<unknown_type:0x");
          out(hex_chars[(type >> 4) & 0xf], buffer, idx++, maxlen);
          out(hex_chars[(type) & 0xf], buffer, idx++, maxlen);
          out('>', buffer, idx++, maxlen);
          OUT_END_;
        }
      } break;
      case 'k': {
        te_token::kind_t tok = te_token::kind_t(va_arg(*va, int));
        switch (tok) {
          CASE_(te_token::NONE, "<none>")
          CASE_(te_token::END, "<end>")
          CASE_(te_token::COMMA, ",")
          CASE_(te_token::DOT, ".")
          CASE_(te_token::SEMICOLON, ";")
          CASE_(te_token::COLON, ":")
          CASE_(te_token::QUESTION_MARK, "?")
          CASE_(te_token::TYPENAME, "<typename>")
          CASE_(te_token::IDENTIFIER, "<identifier>")
          CASE_(te_token::OPEN_PAREN, "(")
          CASE_(te_token::CLOSE_PAREN, ")")
          CASE_(te_token::OPEN_SQUARE_BRACKET, "[")
          CASE_(te_token::CLOSE_SQUARE_BRACKET, "]")
          CASE_(te_token::OPEN_CURLY_BRACKET, "{")
          CASE_(te_token::CLOSE_CURLY_BRACKET, "}")
          CASE_(te_token::LITERAL, "<literal>")
          CASE_(te_token::UNIFORM, "uniform")
          CASE_(te_token::IN, "in")
          CASE_(te_token::OUT, "out")
          CASE_(te_token::INOUT, "inout")
          CASE_(te_token::IF, "if")
          CASE_(te_token::ELSE, "else")
          CASE_(te_token::SWITCH, "switch")
          CASE_(te_token::CASE, "case")
          CASE_(te_token::FOR, "for")
          CASE_(te_token::WHILE, "while")
          CASE_(te_token::DO, "do")
          CASE_(te_token::BREAK, "break")
          CASE_(te_token::CONTINUE, "continue")
          CASE_(te_token::RETURN, "return")
          CASE_(te_token::DISCARD, "discard")
          CASE_(te_token::AND, "&")
          CASE_(te_token::AND_AND, "&&")
          CASE_(te_token::CARET, "^")
          CASE_(te_token::CARET_CARET, "^^")
          CASE_(te_token::PIPE, "|")
          CASE_(te_token::PIPE_PIPE, "||")
          CASE_(te_token::EQUAL, "=")
          CASE_(te_token::EQUAL_EQUAL, "==")
          CASE_(te_token::BANG, "!")
          CASE_(te_token::BANG_EQUAL, "!=")
          CASE_(te_token::LESS, "<")
          CASE_(te_token::LESS_LESS, "<<")
          CASE_(te_token::LESS_EQUAL, "<=")
          CASE_(te_token::GREATER, ">")
          CASE_(te_token::GREATER_GREATER, ">>")
          CASE_(te_token::GREATER_EQUAL, ">=")
          CASE_(te_token::PLUS, "+")
          CASE_(te_token::PLUS_PLUS, "++")
          CASE_(te_token::PLUS_EQUAL, "+=")
          CASE_(te_token::MINUS, "-")
          CASE_(te_token::MINUS_MINUS, "--")
          CASE_(te_token::MINUS_EQUAL, "-=")
          CASE_(te_token::STAR, "*")
          CASE_(te_token::STAR_EQUAL, "*=")
          CASE_(te_token::SLASH, "/")
          CASE_(te_token::SLASH_EQUAL, "/=")
          CASE_(te_token::PERCENT, "%")
        }

        OUT_STR_("<unknown_token:0x");
        out(hex_chars[(tok >> 4) & 0xf], buffer, idx++, maxlen);
        out(hex_chars[(tok) & 0xf], buffer, idx++, maxlen);
        out('>', buffer, idx++, maxlen);
        OUT_END_;
      } break;
      default:
        out(spec, buffer, idx++, maxlen);
        OUT_END_;
    }
#undef OUT_STR_
  }
}

void te_print_type_name(te_type type) {
  te_printf("%y", type);
}

void te_print_token(te_token tok) {
  te_printf("'%k'", tok);
}

struct te_parser_state {
  struct local_var_t {
    const char * name_ptr = nullptr;
    uint8_t name_length = 0;
    te_type type = TE_ERROR;
    uint16_t offset = 0;

    te_strview get_name() const { return {name_ptr, name_length}; }

    local_var_t(te_parser_state & s, te_strview name, te_type p_type, uint16_t p_offset);
    local_var_t() = default;
  };
  struct var_ref {
    // TODO: add local constants
    enum kind_t {
      INVALID,
      LOCAL_VAR,
      GLOBAL_CONST,
      GLOBAL_VAR,
    };

    kind_t kind;
    union {
      void * _ptr;
      local_var_t * local_var = nullptr;
      te_variable * global_var;
      const te_variable * global_const;
    };

    bool is_valid() {
      return kind != INVALID && _ptr;
    }

    bool is_function() {
      return get_type() == TE_FUNCTION;
    }
    
    te_strview get_name() const {
      switch (kind) {
        case INVALID:
          return {};
        case LOCAL_VAR:
          return local_var->get_name();
        case GLOBAL_CONST:
          return global_const->get_name();
        case GLOBAL_VAR:
          return global_var->get_name();
      }

      return {};
    }
    te_type get_type() const {
      switch (kind) {
        case INVALID:
          return TE_ERROR;
        case LOCAL_VAR:
          return local_var->type;
        case GLOBAL_CONST:
          return global_const->type;
        case GLOBAL_VAR:
          return global_var->type;
      }

      return TE_ERROR;
    }
    te_value get_value() const {
      switch (kind) {
        case INVALID:
          return {};
        case LOCAL_VAR:
          return {};
        case GLOBAL_CONST:
          return global_const->value;
        case GLOBAL_VAR:
          return global_var->value;
      }

      return {};
    }
    te_fn_obj get_function() const {
      switch (kind) {
        case INVALID:
          return {};
        case LOCAL_VAR:
          return {};
        case GLOBAL_CONST:
          return global_const->fn;
        case GLOBAL_VAR:
          return global_var->fn;
      }

      return {};
    }
    te_expr * new_ref_expr(te_parser_state & s);

    var_ref(const te_variable & p_global_const) : kind(GLOBAL_CONST), global_const(&p_global_const) {}
    var_ref(te_variable & p_global_var) : kind(GLOBAL_VAR), global_var(&p_global_var) {}
    var_ref(local_var_t & p_local_var) : kind(LOCAL_VAR), local_var(&p_local_var) {}
    var_ref() : kind(INVALID) {}
  };

  te_parser_state * parent = nullptr;
  te_error_record *error = nullptr;
  te_error_record prev_error;
  const char * program = nullptr;
  const char * line_start = nullptr;
  int line_num = 1;
  te_token token;
  te_token prev_token;
  bool parse_error = false;

  te_type return_type = TE_NULL;
  int32_t stmt_count = 0;

  te_variable * global_vars = nullptr;
  int32_t global_var_count = 0;
  int32_t stack_size = 0;
  int32_t stack_offset = 0;
  int32_t var_count = 0;
  local_var_t vars[TE_MAX_VAR_COUNT];
  te_op * stmts[TE_MAX_STMT_COUNT];

  void advance();

  te_token consume() {
    te_token ret = token;
    advance();
    return ret;
  }

  te_expr * parse_precedence(int precedence);
  void print_error_location() const;
};

void te_error_record::reset() {
  if (state) {
    line_start = state->line_start;
    point = start = state->token.name.ptr;
    end = state->token.name.end;
    line_num = state->line_num;
  } else {
    line_start = nullptr;
    start = nullptr;
    point = nullptr;
    end = nullptr;
    line_num = 0;
  }
}

te_error_record::te_error_record(te_parser_state & s) : state(&s), prev(s.error) {
  line_start = s.line_start;
  point = start = s.token.name.ptr;
  end = s.token.name.end;
  line_num = s.line_num;
  s.error = this;
}

te_error_record::~te_error_record() {
  if (state) {
    state->error = prev;
    state->prev_error = *this;
    state->prev_error.state = nullptr;
    state->prev_error.prev = nullptr;
  }
}

te_program::te_program(te_expr * expr, const te_error_record & er) : root_expr(expr), error(er) {}

te_program::te_program(te_program && other) : root_expr(other.root_expr), error(te::move(other.error)) {
  other.root_expr = nullptr;
}

te_program & te_program::operator=(te_program && other) {
  root_expr = other.root_expr;
  error = static_cast<te_error_record &&>(other.error);
  other.root_expr = nullptr;
  return *this;
}

te_program::~te_program() {
  te_free(root_expr);
}

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

static void te_print_error(int line_num, const char * line_start, const char * start, const char * point, const char * const end) {
  const char error_point_highlight_str[] = "^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~>";
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

static void te_print_error(te_parser_state & s) {
  s.parse_error = true;
  s.print_error_location();
}

void te_error_record::print() const {
  te_print_error(line_num, line_start, start, point, end);
}

void te_parser_state::print_error_location() const {
#ifdef TE_DEBUG_COMPILE
  if (error != nullptr) {
    error->print();
  } else {
    te_print_error(line_num, line_start, token.name.ptr, token.name.ptr, token.name.end);
  }
#endif
}

te_parser_state::local_var_t::local_var_t(te_parser_state & s, te_strview name, te_type p_type, uint16_t p_offset) {
  type = p_type;
  offset = p_offset;

  int32_t len = name.len();
  if (len > 0xff) {
#ifdef TE_DEBUG_COMPILE
    te_printf("error: var name '%.*s...' too long! (max length: %d)", 24, name.ptr, 0xff);
#endif
    te_print_error(s);
    return;
  } else if (len < 0) {
#ifdef TE_DEBUG_COMPILE
    te_printf("internal error: invalid var name length: %d", len);
#endif
    te_print_error(s);
    return;
  }

  name_ptr = name.ptr;
  name_length = len;
}

te_parser_state::local_var_t *te_stack_allocate_var(te_parser_state & s, te_strview str, te_type type) {
  int8_t var_size = te_size_of(type);
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
      te_error_record er{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("internal error: stack var deallocated messed up!");
#endif
      te_print_error(s);
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
  
  te_printf("internal error: invalid opcode %02x", op->opcode);
  return -1;
}

static te_expr * new_error_expr(te_expr ** sources, int source_count) {
  int sources_size = sizeof(te_expr *) * source_count;

  const int size = sizeof(te_error_expr) + sources_size;
  te_error_expr * ret = static_cast<te_error_expr *>(malloc(size));
  ret->opcode = TE_OP_ERROR;
  ret->type = TE_ERROR;
  ret->source_count = source_count;
  memcpy(ret->sources, sources, sources_size);
  return ret;
}

template <auto Size>
static te_expr * new_error_expr(te_expr * const (&sources)[Size]) {
  return new_error_expr(sources, Size);
}

static te_expr * new_error_expr() {
  return new_error_expr(nullptr, 0);
}

static te_expr * new_value_expr(te_type type, const te_value & value) {
  const int value_size = te_size_of(type);
  const int size = sizeof(te_value_expr) - sizeof(te_value) + value_size;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = type;
  ret->size = value_size;
  memcpy(&ret->value, &value, value_size);
  return ret;
}

static te_expr * new_value_ref_expr(te_type type, te_value & value) {
  te_type ref_type = te_type(type & ~TE_CONSTANT);
  const int value_size = te_size_of(ref_type);
  const int size = sizeof(te_value_expr) - sizeof(te_value) + value_size;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = ref_type;
  ret->size = value_size;
  ret->value.ref = &value;
  return ret;
}

static te_expr * new_int_literal_expr(const te_int & value) {
  const te_type type = TE_INT;
  const int value_size = te_size_of(type);
  const int size = sizeof(te_value_expr) - sizeof(te_value) + value_size;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = type;
  ret->size = value_size;
  memcpy(&ret->value, &value, value_size);
  return ret;
}

static te_expr * new_float_literal_expr(const te_float & value) {
  const te_type type = TE_FLOAT;
  const int value_size = te_size_of(type);
  const int size = sizeof(te_value_expr) - sizeof(te_value) + value_size;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = type;
  ret->size = value_size;
  memcpy(&ret->value, &value, value_size);
  return ret;
}

static te_expr * new_str_literal_expr(const te_strview & str) {
  // Special dynamically allocated te_value that can be longer than 64 bytes int.
  // TODO: actually parse the literal here ...
  const int length = str.len();
  const int size = sizeof(te_value_expr) - sizeof(te_value) + sizeof(const char *) + length + 1;
  te_value_expr * ret = static_cast<te_value_expr *>(malloc(size));
  ret->opcode = TE_OP_VALUE;
  ret->type = TE_STR;
  ret->size = length + 1;
  ret->value.str = ret->value.str_storage;
  memcpy(ret->value.str_storage, str.ptr, length);
  ret->value.str_storage[length] = 0;
  return ret;
}

static te_expr * new_stack_ref_expr(te_type source_type, uint16_t offset) {
  te_type result_type = te_type(source_type & ~TE_CONSTANT);
  const int size = sizeof(te_stack_ref_expr);
  te_stack_ref_expr * ret = static_cast<te_stack_ref_expr *>(malloc(size));
  ret->opcode = TE_IS_REF(source_type) ? TE_OP_STACK_REF_REF : TE_OP_STACK_REF;
  ret->type = result_type;
  ret->offset = offset;
  return ret;
}

static te_expr * new_deref_expr(te_type target_type, te_expr * e) {
  if (e == nullptr) {
    return nullptr;
  }

  te_type dereferenced_type = te_type(e->type | TE_CONSTANT);
  if (e->type == target_type || e->type == TE_ERROR) {
    return e;
  } else if (dereferenced_type != target_type || target_type == TE_STR) {
#ifdef TE_DEBUG_COMPILE
    te_printf("internal error: cannot deref ");
    te_print_type_name(e->type);
    te_printf(" to ");
    te_print_type_name(target_type);
    te_printf("\n");
#endif
    return new_error_expr({e});
  }

  const int value_size = te_size_of(dereferenced_type);
  const int size = sizeof(te_deref_expr);
  te_deref_expr * ret = static_cast<te_deref_expr *>(malloc(size));
  ret->opcode = TE_OP_DEREF;
  ret->type = dereferenced_type;
  ret->size = value_size;
  ret->arg = e;
  return ret;
}

static te_expr * new_assign_expr(te_expr * lhs, te_expr * rhs) {
  te_type lhs_type = lhs ? lhs->type : TE_ERROR;
  const int size = sizeof(te_assign_expr);
  te_assign_expr * ret = static_cast<te_assign_expr *>(malloc(size));
  ret->opcode = TE_OP_ASSIGN;
  ret->type = lhs_type;
  ret->lhs = lhs;
  ret->rhs = new_deref_expr(te_type(lhs_type | TE_CONSTANT), rhs);

  if (lhs_type != TE_ERROR && !TE_IS_REF(lhs_type)) {
#ifdef TE_DEBUG_COMPILE
    te_printf("error: cannot assign to constant ");
    te_print_type_name(lhs_type);
    te_printf("\n");
#endif
    return new_error_expr({ret});
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
  e = new_deref_expr(TE_INT, e);
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
  e = new_deref_expr(TE_INT, e);
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

static te_return_op * new_return_op(te_type return_type, te_expr * e) {
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

static te_expr * new_call_expr(const te_fn_obj &fn, te_expr * const * args, const int arg_count) {
  const int args_size = sizeof(te_expr *) * arg_count;
  const int size = sizeof(te_call_expr) + args_size;
  te_call_expr * ret = static_cast<te_call_expr *>(malloc(size));
  ret->opcode = TE_OP_CALL;
  ret->type = fn.return_type;
  ret->arg_stack_size = 0;
  ret->fn = {};
#ifdef TE_DEBUG_COMPILE
  if (fn.param_count != arg_count) {
    te_printf("internal error: expected %d args, got %d!\n", fn.param_count, arg_count);
    return new_error_expr({ret});
  }

  if (arg_count > TE_PARAM_COUNT_MAX) {
    te_printf("error: %d is too many parameters! max parameters: %d\n", arg_count, TE_PARAM_COUNT_MAX);
    return new_error_expr({ret});
  }

  for (int i = 0; i < arg_count; ++i) {
    if (args[i] == nullptr) {
      te_printf("internal error: arg %d is null!\n", i);
      return new_error_expr({ret});
    }
  }
#endif
  for (int i = 0; i < arg_count; ++i) {
    ret->arg_stack_size += te_size_of(fn.param_types[i]);
  }
  ret->fn = fn;

  for (int i = 0; i < arg_count; ++i) {
    ret->args[i] = new_deref_expr(fn.param_types[i], args[i]);
  }

  return ret;
}

template<auto N>
static te_expr * new_call_expr(const te_fn_obj &fn, te_expr * const (&args)[N]) {
  return new_call_expr(fn, args, N);
}

static te_expr * new_suite_expr(te_type return_type, uint16_t stack_size, te_op * stmts[], const int stmt_count) {
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
  // template<te_type RefType>
  // type_of<te_type(RefType | TE_CONSTANT)> dereference_value(type_of<RefType> v) {
  //     return *v;
  // }

  template<te_type Type>
  constexpr int element_count = 0;

  template<> inline constexpr int element_count<TE_INT> = 1;
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

  void sequence(void * ctx, void * args, void * ret) {
    uint16_t * offset = reinterpret_cast<uint16_t *>(&ctx);
    uint16_t * size = &offset[1];
    memcpy(ret, reinterpret_cast<char *>(args) + *offset, *size);
  }

  template<te_type RefType, te_type IsConstant = te_type(RefType & TE_CONSTANT)>
  struct index_value {
    static type_of<element_tetype<RefType>> call(type_of<RefType> v, te_int idx) {
      return &v->arr[idx];
    }
  };
  template<te_type Type>
  struct index_value<Type, TE_CONSTANT> {
    static type_of<element_tetype<Type>> call(type_of<Type> v, te_int idx) {
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
    static te_vec2 call(type_of<RefType> v, te_int i, te_int j) {
      return {v->arr[i], v->arr[j]};
    }
  };
  template<te_type Type>
  struct swizzle2_value<Type, TE_CONSTANT> {
    static te_vec2 call(type_of<Type> v, te_int i, te_int j) {
      return {v.arr[i], v.arr[j]};
    }
  };

  template<te_type RefType, te_type IsConstant = te_type(RefType & TE_CONSTANT)>
  struct swizzle3_value {
    static te_vec3 call(type_of<RefType> v, te_int i, te_int j, te_int k) {
      return {v->arr[i], v->arr[j], v->arr[k]};
    }
  };
  template<te_type Type>
  struct swizzle3_value<Type, TE_CONSTANT> {
    static te_vec3 call(type_of<Type> v, te_int i, te_int j, te_int k) {
      return {v.arr[i], v.arr[j], v.arr[k]};
    }
  };

  template<te_type RefType, te_type IsConstant = te_type(RefType & TE_CONSTANT)>
  struct swizzle4_value {
    static te_vec4 call(type_of<RefType> v, te_int i, te_int j, te_int k, te_int l) {
      return {v->arr[i], v->arr[j], v->arr[k], v->arr[l]};
    }
  };
  template<te_type Type>
  struct swizzle4_value<Type, TE_CONSTANT> {
    static te_vec4 call(type_of<Type> v, te_int i, te_int j, te_int k, te_int l) {
      return {v.arr[i], v.arr[j], v.arr[k], v.arr[l]};
    }
  };

  namespace detail {
    te_float sign_float(te_float v) {
      if (v > 0.0f) {
        return 1.0f;
      } else if (v < 0.0f) {
        return -1.0f;
      } else {
        return v;
      }
    }

    te_int sign_int(te_int v) {
      if (v > 0) {
        return 1;
      } else if (v < 0) {
        return -1;
      } else {
        return v;
      }
    }

    te_int fac(te_int a) {
      if (a < 0) {
        return -1;
      }

      te_int result = 1;
      for (te_int i = 2; i <= a; i++) {
        result *= i;
      }
      return result;
    }

    te_int ncr(te_int n, te_int r) {
      if (n < 0 || r < 0 || n < r) {
        return -1;
      }

      if (n - r < r) {
        r = n - r;
      }

      te_int result = 1;
      for (te_int i = 1; i <= r; i++) {
        result *= n - r + i;
        result /= i;
      }

      return result;
    }

    te_int npr(te_int n, te_int r) {
      return ncr(n, r) * fac(r);
    }

    //te_float abs2(te_float n) {return (n<0 ? -n : n);}

    //te_float log2(te_float n) {const te_float ln2=log(2); return log(n)/ln2;}

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
    te_float mod<te_float>(te_float a, te_float b) { return fmodf(a, b); }

    te_float to_radians(te_float deg) { return deg * (PI / 180.0f); }
    te_float to_degrees(te_float rad) { return rad * (180.0f / PI); }

    void int_to_float(void *, void * args, void * ret) {
      *static_cast<te_float *>(ret) = static_cast<te_float>(*static_cast<te_int *>(args));
    }

    void float_to_int(void *, void * args, void * ret) {
      *static_cast<te_int *>(ret) = static_cast<te_int>(*static_cast<te_float *>(args));
    }

    // rest of the code in here only works because of how arguments are packed on the 'stack' and how vecs and mats are stored
    void printf_impl(void *, void * args, void * ret) {
      const char * str = *reinterpret_cast<const char **>(args);
    }

    template<int Size>
    void mem_copy(void *, void * args, void * ret) { memcpy(ret, args, Size); }

    template<int Size>
    void init_mem(void *, void *, void * ret) { memset(ret, 0, Size); }

    template<te_type Type>
    void float_to_vec(void *, void * args, void * ret) {
      static_assert(te::is_vec<Type>);
      te_float v = *static_cast<te_float *>(args);
      for (int i = 0; i < te::element_count<Type>; ++i) {
        static_cast<te_float *>(ret)[i] = v;
      }
    }

    template<te_type Type>
    void float_to_mat(void * ctx, void * args, void * ret) {
      static_assert(te::is_mat<Type>);

      init_mem<te_size_of(Type)>(ctx, args, ret);
      constexpr int MatSize = te::element_count<te::element_tetype<Type>>;
      te_float v = *static_cast<te_float *>(args);
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
static te_function find_variadic_constructor(const te_type * param_types, int param_count) {
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

static te_function find_constructor_fn(te_type type, const te_type * param_types, int param_count) {
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

static te_fn_obj find_constructor(te_parser_state & s, te_type type, const te_type * arg_types, int8_t arg_count) {
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

  if (!fn.is_valid()) {
#ifdef TE_DEBUG_COMPILE
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
#endif
    te_print_error(s);
  }

  return fn;
}

static const te_variable te_builtins[] = {
    {"true", te::make_value<te_int>(1)},
    {"false", te::make_value<te_int>(0)},
    {"E", te::make_value<te_float>(2.71828182845904523536f)},
    {"INF", te::make_value<te_float>(INFINITY)},
    {"NAN", te::make_value<te_float>(NAN)},
    {"PI", te::make_value<te_float>(PI)},
    {"TAU", te::make_value<te_float>(PI * 2.0f)},
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

static int te_builtins_count = sizeof(te_builtins) / sizeof(te_variable);

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

template<typename T>
static bool test_arg_types_match(T & var, const te_type * arg_types, int arg_count) {
  if (arg_types) {
    if (var.type == TE_FUNCTION && arg_count == var.fn.param_count) {
      for (int i = 0; i < arg_count; ++i) {
        if ((arg_types[i] | (var.fn.param_types[i] & TE_CONSTANT)) != var.fn.param_types[i]) {
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
bool test_arg_types_match<te_parser_state::local_var_t>(te_parser_state::local_var_t & var, const te_type * arg_types, int arg_count) {
  return true;
}

template<typename T>
static te_parser_state::var_ref find_var_search(T * vars, int var_count, te_strview name, const te_type * arg_types = nullptr, int arg_count = 0) {
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

static te_parser_state::var_ref find_var(te_parser_state & s, te_strview name, te_type * arg_types = nullptr, int arg_count = 0) {
  for (int i = 0; i < arg_count; ++i) {
    if (arg_types[i] == TE_ERROR) {
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
#ifdef TE_DEBUG_COMPILE
      te_printf("error: could not find function matching '%.*s", name.len(), name.ptr);
      te_printf("(");
      for (int i = 0; i < arg_count; ++i) {
        if (i != 0) {
          te_printf(", ");
        }
        te_print_type_name(arg_types[i]);
      }
      te_printf(")'\n");
#endif
      te_print_error(s);
    } else {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: could not find '%.*s'!\n", name.len(), name.ptr);
#endif
      te_print_error(s);
    }
  }

  return result;
}

bool try_tokenize_number_literal(const char * str, te_token & r_token) {
  char * parsed_int_end = nullptr;
  te_int parsed_int = strtol(str, &parsed_int_end, 0);
  char * parsed_float_end = nullptr;
  te_float parsed_float = strtof(str, &parsed_float_end);
  if (parsed_float_end > parsed_int_end) {
    // strtof parsed more, use that
    r_token = te_token::make_float_literal({str, parsed_float_end}, parsed_float);
    return true;
  } else if (parsed_int_end > str) {
    // strtol parsed something...
    r_token = te_token::make_int_literal({str, parsed_float_end}, parsed_float);
    return true;
  }
  
  return false;
}

void te_parser_state::advance() {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered next_token\n");
#endif

  prev_token = token;

  // TODO: include directive
  bool new_line = token.name.end == program;
  do {
    const char * tok_start = token.name.end;
    token = {};
    token.name.ptr = token.name.end = tok_start;

    if (token.name.ptr[0] == '\0') {
      token.kind = te_token::END;
      break;
    }

    /* Try reading a number. */
    if (isdigit(token.name.ptr[0]) || (token.name.ptr[0] == '.' && isdigit(token.name.ptr[1]))) {
      if (try_tokenize_number_literal(token.name.ptr, token)) {
        break;
      }
    }

    if (token.name.ptr[0] == '"') {
      const char * end_quote = token.name.ptr + 1;
      while (end_quote[0] != '"' || end_quote[-1] == '\\') {
        if (end_quote[0] == '\0') {
          te_error_record er{*this};
#ifdef TE_DEBUG_COMPILE
          te_printf("error: unexpected end of input!\n");
#endif
          te_print_error(*this);
        }
        ++end_quote;
      }
      token = te_token::make_str_literal({token.name.ptr, end_quote + 1});
      break;
    }

    if (isalpha(token.name.ptr[0]) || token.name.ptr[0] == '_') {
      while (isalpha(*token.name.end) || isdigit(*token.name.end) || *token.name.end == '_') token.name.end++;
      const int id_len = token.name.len();

#define MATCHES_TOKEN_(str) (id_len == (sizeof(str) - 1) && strncmp(str, token.name.ptr, id_len) == 0)
      switch (*token.name.ptr) {
        case 'b': {
          if (MATCHES_TOKEN_("break")) {
            token.kind = te_token::BREAK;
          }
        }
          break;
        case 'c': {
          if (MATCHES_TOKEN_("case")) {
            token.kind = te_token::CASE;
          } else if (MATCHES_TOKEN_("continue")) {
            token.kind = te_token::CONTINUE;
          }
        }
          break;
        case 'd': {
          if (MATCHES_TOKEN_("discard")) {
            token.kind = te_token::DISCARD;
          } else if (MATCHES_TOKEN_("do")) {
            token.kind = te_token::DO;
          }
        }
          break;
        case 'e': {
          if (MATCHES_TOKEN_("else")) {
            token.kind = te_token::ELSE;
          }
        }
          break;
        case 'f': {
          if (MATCHES_TOKEN_("float")) {
            token.kind = te_token::TYPENAME;
            token.type = TE_FLOAT;
          } else if (MATCHES_TOKEN_("for")) {
            token.kind = te_token::FOR;
          }
        }
          break;
        case 'i': {
          if (MATCHES_TOKEN_("if")) {
            token.kind = te_token::IF;
          } else if (MATCHES_TOKEN_("in")) {
            token.kind = te_token::IN;
          } else if (MATCHES_TOKEN_("inout")) {
            token.kind = te_token::INOUT;
          } else if (MATCHES_TOKEN_("int")) {
            token.kind = te_token::TYPENAME;
            token.type = TE_INT;
          }
        }
          break;
        case 'm': {
          if (MATCHES_TOKEN_("mat2")) {
            token.kind = te_token::TYPENAME;
            token.type = TE_MAT2;
          } else if (MATCHES_TOKEN_("mat3")) {
            token.kind = te_token::TYPENAME;
            token.type = TE_MAT3;
          } else if (MATCHES_TOKEN_("mat4")) {
            token.kind = te_token::TYPENAME;
            token.type = TE_MAT4;
          }
        }
          break;
        case 'o': {
          if (MATCHES_TOKEN_("out")) {
            token.kind = te_token::OUT;
          }
        }
          break;
        case 'r': {
          if (MATCHES_TOKEN_("return")) {
            token.kind = te_token::RETURN;
          }
        }
          break;
        case 's': {
          if (MATCHES_TOKEN_("switch")) {
            token.kind = te_token::SWITCH;
          } else if (MATCHES_TOKEN_("str")) {
            token.kind = te_token::TYPENAME;
            token.type = TE_STR;
          }
        }
          break;
        case 'u': {
          if (MATCHES_TOKEN_("uniform")) {
            token.kind = te_token::UNIFORM;
          }
        }
          break;
        case 'v': {
          if (MATCHES_TOKEN_("vec2")) {
            token.kind = te_token::TYPENAME;
            token.type = TE_VEC2;
          } else if (MATCHES_TOKEN_("vec3")) {
            token.kind = te_token::TYPENAME;
            token.type = TE_VEC3;
          } else if (MATCHES_TOKEN_("vec4")) {
            token.kind = te_token::TYPENAME;
            token.type = TE_VEC4;
          }
        }
          break;
        case 'w': {
          if (MATCHES_TOKEN_("while")) {
            token.kind = te_token::WHILE;
          }
        }
          break;
      }
#undef MATCHES_TOKEN_

      if (token.kind == te_token::NONE) {
        token.kind = te_token::IDENTIFIER;
      }

      break;
    }

    /* Look for an operator or special character. */
    switch (token.name.end++[0]) {
      case '&': {
        if (*token.name.end == '&') {
          token.name.end++;
          token.kind = te_token::AND_AND;
        } else {
          token.kind = te_token::AND;
        }
      } break;
      case '^': {
        if (*token.name.end == '^') {
          token.name.end++;
          token.kind = te_token::CARET_CARET;
        } else {
          token.kind = te_token::CARET;
        }
      } break;
      case '|': {
        if (*token.name.end == '|') {
          token.name.end++;
          token.kind = te_token::PIPE_PIPE;
        } else {
          token.kind = te_token::PIPE;
        }
      } break;
      case '=': {
        if (*token.name.end == '=') {
          token.name.end++;
          token.kind = te_token::EQUAL_EQUAL;
        } else {
          token.kind = te_token::EQUAL;
        }
      } break;
      case '!': {
        if (*token.name.end == '=') {
          token.name.end++;
          token.kind = te_token::BANG_EQUAL;
        } else {
          token.kind = te_token::BANG;
        }
      } break;
      case '<': {
        if (*token.name.end == '<') {
          token.name.end++;
          token.kind = te_token::LESS_LESS;
        } else if (*token.name.end == '=') {
          token.name.end++;
          token.kind = te_token::LESS_EQUAL;
        } else {
          token.kind = te_token::LESS;
        }
      } break;
      case '>': {
        if (*token.name.end == '>') {
          token.name.end++;
          token.kind = te_token::GREATER_GREATER;
        } else if (*token.name.end == '=') {
          token.name.end++;
          token.kind = te_token::GREATER_EQUAL;
        } else {
          token.kind = te_token::GREATER;
        }
      } break;
      case '+': {
        if (*token.name.end == '+') {
          token.name.end++;
          token.kind = te_token::PLUS_PLUS;
        } else if (*token.name.end == '=') {
          token.name.end++;
          token.kind = te_token::PLUS_EQUAL;
        } else {
          token.kind = te_token::PLUS;
        }
      } break;
      case '-': {
        if (*token.name.end == '-') {
          token.name.end++;
          token.kind = te_token::MINUS_MINUS;
        } else if (*token.name.end == '=') {
          token.name.end++;
          token.kind = te_token::MINUS_EQUAL;
        } else {
          token.kind = te_token::MINUS;
        }
      } break;
      case '*': {
        if (*token.name.end == '=') {
          token.name.end++;
          token.kind = te_token::STAR_EQUAL;
        } else {
          token.kind = te_token::STAR;
        }
      } break;
      case '/': {
        if (*token.name.end == '=') {
          token.name.end++;
          token.kind = te_token::SLASH_EQUAL;
        } else {
          token.kind = te_token::SLASH;
        }
      } break;
      case '%':
        token.kind = te_token::PERCENT;
        break;
      case '(':
        token.kind = te_token::OPEN_PAREN;
        break;
      case ')':
        token.kind = te_token::CLOSE_PAREN;
        break;
      case '[':
        token.kind = te_token::OPEN_SQUARE_BRACKET;
        break;
      case ']':
        token.kind = te_token::CLOSE_SQUARE_BRACKET;
        break;
      case '{':
        token.kind = te_token::OPEN_CURLY_BRACKET;
        break;
      case '}':
        token.kind = te_token::CLOSE_CURLY_BRACKET;
        break;
      case ',':
        token.kind = te_token::COMMA;
        break;
      case '.':
        token.kind = te_token::DOT;
        break;
      case ';':
        token.kind = te_token::SEMICOLON;
        break;
      case ':':
        token.kind = te_token::COLON;
        break;
      case '?':
        token.kind = te_token::QUESTION_MARK;
        break;
      case '\r':
        if (token.name.end[0] == '\n') token.name.end++; // fallthrough
      case '\n':
        new_line = true;
        line_num++;
        line_start = token.name.end;
        // fallthrough
      case ' ':
      case '\t':
        break;
      default: {
        te_error_record er{*this};
#ifdef TE_DEBUG_COMPILE
        te_printf("error: unrecognised token!\n");
#endif
        te_print_error(*this);
      }
        break;
    }
  } while (token.kind == te_token::NONE);
#ifdef TE_DEBUG_PEDANTIC
  te_printf("parsed token '%.*s'\n", token.name.len(), token.name.ptr);
#endif
}

static te_fn_obj te_get_index_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_VEC2:
      return te::make_pure_function<te::index_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_pure_function<te::index_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_pure_function<te::index_value<TE_VEC4>::call>();
    case TE_MAT2:
      return te::make_pure_function<te::index_value<TE_MAT2>::call>();
    case TE_MAT3:
      return te::make_pure_function<te::index_value<TE_MAT3>::call>();
    case TE_MAT4:
      return te::make_pure_function<te::index_value<TE_MAT4>::call>();
    case TE_VEC2_REF:
      return te::make_pure_function<te::index_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_pure_function<te::index_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_pure_function<te::index_value<TE_VEC4_REF>::call>();
    case TE_MAT2_REF:
      return te::make_pure_function<te::index_value<TE_MAT2_REF>::call>();
    case TE_MAT3_REF:
      return te::make_pure_function<te::index_value<TE_MAT3_REF>::call>();
    case TE_MAT4_REF:
      return te::make_pure_function<te::index_value<TE_MAT4_REF>::call>();
    default:
      break;
  }

  return ret;
}

static te_fn_obj te_get_swizzle2_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_VEC2_REF:
      return te::make_pure_function<te::swizzle2_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_pure_function<te::swizzle2_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_pure_function<te::swizzle2_value<TE_VEC4_REF>::call>();
    case TE_VEC2:
      return te::make_pure_function<te::swizzle2_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_pure_function<te::swizzle2_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_pure_function<te::swizzle2_value<TE_VEC4>::call>();
    default:
      break;
  }

  return ret;
}

static te_fn_obj te_get_swizzle3_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_VEC2_REF:
      return te::make_pure_function<te::swizzle3_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_pure_function<te::swizzle3_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_pure_function<te::swizzle3_value<TE_VEC4_REF>::call>();
    case TE_VEC2:
      return te::make_pure_function<te::swizzle3_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_pure_function<te::swizzle3_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_pure_function<te::swizzle3_value<TE_VEC4>::call>();
    default:
      break;
  }

  return ret;
}

static te_fn_obj te_get_swizzle4_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_VEC2_REF:
      return te::make_pure_function<te::swizzle4_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_pure_function<te::swizzle4_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_pure_function<te::swizzle4_value<TE_VEC4_REF>::call>();
    case TE_VEC2:
      return te::make_pure_function<te::swizzle4_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_pure_function<te::swizzle4_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_pure_function<te::swizzle4_value<TE_VEC4>::call>();
    default:
      break;
  }

  return ret;
}

static te_fn_obj te_get_negate_func(te_type type) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  switch (type) {
    case TE_INT:
      return te::make_pure_function<te::negate_value<TE_INT>::call>();
    case TE_FLOAT:
      return te::make_pure_function<te::negate_value<TE_FLOAT>::call>();
    case TE_VEC2:
      return te::make_pure_function<te::negate_value<TE_VEC2>::call>();
    case TE_VEC3:
      return te::make_pure_function<te::negate_value<TE_VEC3>::call>();
    case TE_VEC4:
      return te::make_pure_function<te::negate_value<TE_VEC4>::call>();
    case TE_MAT2:
      return te::make_pure_function<te::negate_value<TE_MAT2>::call>();
    case TE_MAT3:
      return te::make_pure_function<te::negate_value<TE_MAT3>::call>();
    case TE_MAT4:
      return te::make_pure_function<te::negate_value<TE_MAT4>::call>();
    case TE_INT_REF:
      return te::make_pure_function<te::negate_value<TE_INT_REF>::call>();
    case TE_FLOAT_REF:
      return te::make_pure_function<te::negate_value<TE_FLOAT_REF>::call>();
    case TE_VEC2_REF:
      return te::make_pure_function<te::negate_value<TE_VEC2_REF>::call>();
    case TE_VEC3_REF:
      return te::make_pure_function<te::negate_value<TE_VEC3_REF>::call>();
    case TE_VEC4_REF:
      return te::make_pure_function<te::negate_value<TE_VEC4_REF>::call>();
    case TE_MAT2_REF:
      return te::make_pure_function<te::negate_value<TE_MAT2_REF>::call>();
    case TE_MAT3_REF:
      return te::make_pure_function<te::negate_value<TE_MAT3_REF>::call>();
    case TE_MAT4_REF:
      return te::make_pure_function<te::negate_value<TE_MAT4_REF>::call>();
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
    static void call(void *, void * args, void * retvp) {
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
      te_float sum = 0.0f;
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
      te_float sum = 0.0f;
      for (int j = 0; j < MatSize; ++j) {
        sum += te::detail::mul(a.arr[i], b.arr[i].arr[j]);
      }
      ret->arr[i] = sum;
    }
  }

  template<typename TRet, typename TA, typename TB>
  void fn_matrix_mul_matrix(void *, void * args, void * retvp) {
    static_assert(te::is_same<TA, TB>);
    constexpr auto MatSize = te::element_count<te::element_tetype<te::type_value_of<TA>>>;
    TRet * ret = (TRet *) retvp;
    TA * ap = (TA *) args;
    TB * bp = (TB *) (&ap[1]);
    TA a = *ap;
    TB b = *bp;
    for (int i = 0; i < MatSize; ++i) {
      for (int j = 0; j < MatSize; ++j) {
        te_float sum = 0.0f;
        for (int k = 0; k < MatSize; ++k) {
          sum += te::detail::mul(a.arr[i].arr[k], b.arr[k].arr[j]);
        }
        ret->arr[i].arr[j] = sum;
      }
    }
  }
}

#define TE_BINARY_OP_FUNCTION(FNAME, TRET, TA, TB) te::detail::make_function_raw(FNAME<te::type_of<(TRET)>, te::type_of<(TA)>, te::type_of<(TB)>>, nullptr, true, (TRET), {(TA), (TB)})

#define CASE_SCALAR_SCALAR_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(te::detail::fn_scalar_op_scalar<(OP)>::call, TA, TA, TB)
#define CASE_VEC_VEC_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(te::detail::fn_vec_op_vec<(OP)>::call, TA, TA, TB)
#define CASE_SCALAR_VEC_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(te::detail::fn_scalar_op_vec<(OP)>::call, TB, TA, TB)
#define CASE_VEC_SCALAR_(TA, OP, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(te::detail::fn_vec_op_scalar<(OP)>::call, TA, TA, TB)
#define CASE_MATRIX_VEC_MUL_(TA, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(te::detail::fn_matrix_mul_vec, TB, TA, TB)
#define CASE_VEC_MATRIX_MUL_(TA, TB) if (typeA == (TA) && typeB == (TB)) return TE_BINARY_OP_FUNCTION(te::detail::fn_vec_mul_matrix, TA, TA, TB)
#define CASE_MATRIX_MATRIX_MUL_(T) if (typeA == (T) && typeB == (T)) return TE_BINARY_OP_FUNCTION(te::detail::fn_matrix_mul_matrix, T, T, T)

// void mm4_mul_(void*, void *args, void *retvp) {
//     te_mat4 *ret = (te_mat4*)ret;
//     te_mat4 *ap = (te_mat4*)args;
//     te_mat4 a = *ap;
//     te_mat4 b = *(te_mat4*)(&ap[1]);
//     for (int i = 0; i < 4; ++i) {
//         for (int j = 0; j < 4; ++j) {
//             te_float sum = 0.0f;
//             for (int k = 0; k < 4; ++k) {
//                 sum += te::detail::mul(a.arr[i].arr[k], b.arr[k].arr[j]);
//             }
//             ret->arr[i].arr[j] = sum;
//         }
//     }
// };

static te_fn_obj te_get_add_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::add<te_int>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::add<te_float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::add<te_float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::add<te_float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::add<te_float>, TE_VEC4);

  return ret;
}

static te_fn_obj te_get_sub_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::sub<te_int>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::sub<te_float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::sub<te_float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::sub<te_float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::sub<te_float>, TE_VEC4);

  return ret;
}

static te_fn_obj te_get_mod_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::mod<te_int>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::mod<te_float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::mod<te_float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::mod<te_float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::mod<te_float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_VEC2, te::detail::mod<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC3, te::detail::mod<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC4, te::detail::mod<te_float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mod<te_float>, TE_VEC2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mod<te_float>, TE_VEC3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mod<te_float>, TE_VEC4);

  return ret;
}

static te_fn_obj te_get_mul_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::mul<te_int>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::mul<te_float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::mul<te_float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::mul<te_float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::mul<te_float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_VEC2, te::detail::mul<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC3, te::detail::mul<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC4, te::detail::mul<te_float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<te_float>, TE_VEC2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<te_float>, TE_VEC3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<te_float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_MAT2, te::detail::mul<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_MAT3, te::detail::mul<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_MAT4, te::detail::mul<te_float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<te_float>, TE_MAT2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<te_float>, TE_MAT3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::mul<te_float>, TE_MAT4);
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

static te_fn_obj te_get_div_func(te_type typeA, te_type typeB) {
  te_fn_obj ret;
  ret.ptr = nullptr;

  CASE_SCALAR_SCALAR_(TE_INT, te::detail::div<te_int>, TE_INT);
  CASE_SCALAR_SCALAR_(TE_FLOAT, te::detail::div<te_float>, TE_FLOAT);
  CASE_VEC_VEC_(TE_VEC2, te::detail::div<te_float>, TE_VEC2);
  CASE_VEC_VEC_(TE_VEC3, te::detail::div<te_float>, TE_VEC3);
  CASE_VEC_VEC_(TE_VEC4, te::detail::div<te_float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_VEC2, te::detail::div<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC3, te::detail::div<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_VEC4, te::detail::div<te_float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<te_float>, TE_VEC2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<te_float>, TE_VEC3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<te_float>, TE_VEC4);
  CASE_VEC_SCALAR_(TE_MAT2, te::detail::div<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_MAT3, te::detail::div<te_float>, TE_FLOAT);
  CASE_VEC_SCALAR_(TE_MAT4, te::detail::div<te_float>, TE_FLOAT);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<te_float>, TE_MAT2);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<te_float>, TE_MAT3);
  CASE_SCALAR_VEC_(TE_FLOAT, te::detail::div<te_float>, TE_MAT4);

  return ret;
}

#undef CASE_MATRIX_MUL_
#undef CASE_VEC_SCALAR_
#undef CASE_SCALAR_VEC_
#undef CASE_VEC_VEC_
#undef CASE_SCALAR_SCALAR_

static te_expr * expr(te_parser_state & s);
static te_expr * power(te_parser_state & s);

static te_expr * base(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered base\n");
#endif

  /* <base>      =    <literal> | <variable> | <function-X> "(" {<expr> {"," | "," <expr>}+} ")" | "(" <expr> ")" */
  te_error_record er{s};
  te_expr * ret = nullptr;
  switch (s.token.kind) {
    case te_token::LITERAL: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("base is literal %y\n", s.token.type);
#endif
      switch (s.token.type) {
        case TE_INT:
          ret = new_int_literal_expr(s.token.int_value);
          break;
        case TE_FLOAT:
          ret = new_float_literal_expr(s.token.float_value);
          break;
        case TE_STR:
          ret = new_str_literal_expr(s.token.name);
          break;
        default:
          te_error_record er(s);
#ifdef TE_DEBUG_COMPILE
          te_printf("internal error: unknown literal type: %y!\n", s.token.type);
#endif
          te_print_error(s);
          break;
      }
      s.advance();
    } break;
    case te_token::TYPENAME:
    case te_token::IDENTIFIER: {
      bool is_typename = s.token == te_token::TYPENAME;
      te_type type = s.token.type;
      te_strview id = s.token.name;

      s.advance();
      if (s.token == te_token::OPEN_PAREN) {
#ifdef TE_DEBUG_PEDANTIC
        te_printf("base is function call\n");
#endif
        te_expr * args[TE_PARAM_COUNT_MAX]{nullptr};

        s.advance();
        int arg_count = 0;
        te_type arg_types[TE_PARAM_COUNT_MAX]{TE_NULL};
        bool valid = true;
        while (s.token != te_token::CLOSE_PAREN && s.token != te_token::END) {
          te_expr * arg = expr(s);

          if (arg_count < TE_PARAM_COUNT_MAX) {
            arg_types[arg_count] = arg ? arg->type : TE_ERROR;
            args[arg_count] = arg;
            ++arg_count;
          } else {
            valid = false;
            te_free(arg);
            arg = nullptr;
          }

          if (s.token == te_token::COMMA) {
            s.advance();
          }
        }

        te_fn_obj fn;
        if (s.token == te_token::CLOSE_PAREN) {
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
          te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
          te_printf("error: missing ')'!\n");
#endif
          te_print_error(s);
          valid = false;
        }

        if (valid) {
          ret = new_call_expr(fn, args, arg_count);
#ifdef TE_DEBUG_PEDANTIC
          te_printf("function value: ");
          te_print_value({ret ? reinterpret_cast<te_call_expr *>(ret)->fn : te_fn_obj{}, TE_FUNCTION});
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
        te_parser_state::var_ref var = find_var(s, id);
        if (var.is_valid()) {
          ret = var.new_ref_expr(s);
#ifdef TE_DEBUG_PEDANTIC
          te_printf("base is global var, value: ");
          te_print_expr(ret);
          te_printf("\n");
#endif
        } else {
          s.parse_error = true;
        }

      }
    } break;
    case te_token::OPEN_PAREN: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("base is grouping expr\n");
#endif
      s.advance();
      ret = expr(s);

      if (s.token == te_token::CLOSE_PAREN) {
        s.advance();
      } else if (ret != nullptr) {
        te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
        te_printf("error: expected ')', got ");
        te_print_token(s.token);
        te_printf("!\n");
#endif
        te_print_error(s);
        te_free(ret);
        ret = nullptr;
        return nullptr;
      }
    } break;
    default:
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected expression, got ");
      te_print_token(s.token);
      te_printf("!\n");
#endif
      te_print_error(s);
      s.advance();
      return nullptr;
  }

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited base\n");
#endif

  return ret;
}

static te_expr * index(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered index\n");
#endif

  /* <index>     =    <base> { "[" <int> "]" } */
  te_error_record er{s};
  te_expr * ret = base(s);

  if (s.token == te_token::OPEN_SQUARE_BRACKET) {
    te_type ret_type = ret ? ret->type : TE_ERROR;

    s.advance();

    te_expr * idx_expr = nullptr;

    if (s.token == te_token::CLOSE_SQUARE_BRACKET) {
      te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("error: missing index!\n");
#endif
      te_print_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    idx_expr = expr(s);

    te_type idx_type = idx_expr ? idx_expr->type : TE_ERROR;

    te_fn_obj index_fn;
    if ((idx_type | TE_CONSTANT) == TE_INT) {
      index_fn = te_get_index_func(ret_type);
    } else {
      // set to a valid noop fn to avoid double error print
      index_fn = te_fn_obj{};
      index_fn.ptr = &te_noop;
      if (idx_type != TE_ERROR) {
        te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
        te_printf("error: index must be an int! type: ");
        te_print_type_name(idx_type);
        te_printf("\n");
#endif
        te_print_error(s);
      }
      te_free(idx_expr);
      idx_expr = nullptr;
      idx_type = idx_expr ? idx_expr->type : TE_ERROR;
      te_free(ret);
      ret = nullptr;
    }

    if (s.token != te_token::CLOSE_SQUARE_BRACKET) {
      te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected ']', got ");
      te_print_token(s.token);
      te_printf("!\n");
#endif
      te_print_error(s);
      te_free(idx_expr);
      idx_expr = nullptr;
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    s.advance();

    if (!index_fn.is_valid()) {
#ifdef TE_DEBUG_COMPILE
      er.set_end(s.prev_token.name.end);
      te_printf("error: cannot index! type: ");
      te_print_type_name(idx_type);
      te_printf("\n");
#endif
      te_print_error(s);
      te_free(idx_expr);
      idx_expr = nullptr;
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }
    ret = new_call_expr(index_fn, {ret, idx_expr});
  }

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited index\n");
#endif

  return ret;
}

static te_expr * attr(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered attr\n");
#endif

  /* <attr>      =    <index> { "." ("x" | "y" | "z" | "w" | "r" | "g" | "b" | "a")[1-4] } */
  te_error_record er{s};
  te_expr * ret = index(s);
  te_type ret_type = ret ? ret->type : TE_ERROR;

  if (s.token == te_token::DOT) {
    s.advance();
    if (s.token != te_token::IDENTIFIER) {
      te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected swizzlers after '.' got ");
      te_print_token(s.token);
      te_printf("!\n");
#endif
      te_print_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    te_type base_type = ret ? ret->type : TE_ERROR;
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
        te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
        te_printf("error: cannot swizzle ");
        te_print_type_name(base_type);
        te_printf("!\n");
#endif
        te_print_error(s);
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
            te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
            er.start = c;
            er.end = c + 1;
            te_printf("error: unknown swizzler '%c'!\n", *c);
#endif
            te_print_error(s);
            s.advance();
            te_free(ret);
            ret = nullptr;
            return nullptr;
          }
        }
      }
    }

    if (i < 1 || i > swizzle_max) {
      te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("error: too many swizzlers! type: ");
      te_print_type_name(base_type);
      te_printf("\n");
#endif
      te_print_error(s);
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
      te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("internal error: no index func! type: ");
      te_print_type_name(ret_type);
      te_printf("\n");
#endif
      te_print_error(s);
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
  te_printf("exited attr\n");
#endif

  return ret;
}

static void post_increment(void *, void * args, void * ret) {
  *reinterpret_cast<te_int *>(ret) = *reinterpret_cast<te_int **>(args)[0];
  ++(*reinterpret_cast<te_int **>(args)[0]);
}

static void post_decrement(void *, void * args, void * ret) {
  *reinterpret_cast<te_int *>(ret) = *reinterpret_cast<te_int **>(args)[0];
  --(*reinterpret_cast<te_int **>(args)[0]);
}

static void pre_increment(void *, void * args, void * ret) {
  ++(*reinterpret_cast<te_int **>(args)[0]);
  *reinterpret_cast<te_int *>(ret) = *reinterpret_cast<te_int **>(args)[0];
}

static void pre_decrement(void *, void * args, void * ret) {
  --(*reinterpret_cast<te_int **>(args)[0]);
  *reinterpret_cast<te_int *>(ret) = *reinterpret_cast<te_int **>(args)[0];
}

static void bool_not(void *, void * args, void * ret) {
  *reinterpret_cast<te_int *>(ret) = !reinterpret_cast<te_int *>(args)[0];
}

static te_expr * factor(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered factor\n");
#endif

  /* <factor>    =    {("!" | "-" | "--" | "+" | "++")} <attr> {("--" | "++")} */
  te_error_record er{s};

  bool bool_not_flag = false;
  bool pos = true;
  bool pre_incdec = false;
  if (s.token == te_token::PLUS) {
    pos = true;
    s.advance();
  } else if (s.token == te_token::MINUS) {
    pos = false;
    s.advance();
  } else if (s.token == te_token::PLUS_PLUS) {
    pos = true;
    pre_incdec = true;
    s.advance();
  } else if (s.token == te_token::MINUS_MINUS) {
    pos = false;
    pre_incdec = true;
    s.advance();
  } else if (s.token == te_token::BANG) {
    bool_not_flag = true;
    s.advance();
  }

  te_expr * ret = attr(s);
  te_type ret_type = ret ? ret->type : TE_ERROR;

  er.set_end(s.token.name.end);

  if (bool_not_flag) {
    if (ret_type == TE_INT) {
      ret = new_call_expr(te::detail::make_function_raw(bool_not, nullptr, true, TE_INT, {TE_INT}), {ret});
    } else {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: '!' op only works on int! type: ");
      te_print_type_name(ret_type);
      te_printf("\n");
#endif
      te_print_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }
  } else if (pre_incdec) {
    if (ret_type != TE_INT_REF) {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: only int ref can be incremented/decremented! type: ");
      te_print_type_name(ret_type);
      te_printf("\n");
#endif
      te_print_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    if (pos) {
      ret = new_call_expr(te::detail::make_function_raw(pre_increment, nullptr, true, TE_INT, {TE_INT_REF}), {ret});
    } else {
      ret = new_call_expr(te::detail::make_function_raw(pre_decrement, nullptr, true, TE_INT, {TE_INT_REF}), {ret});
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
#endif
        te_print_error(s);
        te_free(ret);
        ret = nullptr;
        return nullptr;
      }

      ret = new_call_expr(tmpfn, {ret});
    }
  }

  ret_type = ret ? ret->type : TE_ERROR;

  int postfix = 0;
  if (s.token == te_token::PLUS_PLUS) {
    postfix = 1;
    s.advance();
  } else if (s.token == te_token::MINUS_MINUS) {
    postfix = -1;
    s.advance();
  }

  er.set_end(s.token.name.end);

  if (postfix) {
    if (ret_type != TE_INT_REF) {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: only int ref can be incremented/decremented! type: ");
      te_print_type_name(ret_type);
      te_printf("\n");
#endif
      te_print_error(s);
      te_free(ret);
      ret = nullptr;
      return nullptr;
    }

    te_type param_type = TE_INT_REF;
    if (postfix > 0) {
      ret = new_call_expr(te::detail::make_function_raw(post_increment, nullptr, true, TE_INT, {TE_INT_REF}), {ret});
    } else {
      ret = new_call_expr(te::detail::make_function_raw(post_decrement, nullptr, true, TE_INT, {TE_INT_REF}), {ret});
    }
  }

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited factor\n");
#endif

  return ret;
}

static te_expr * term(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered term\n");
#endif

  /* <term>      =    <factor> {("*" | "/" | "%") <factor>}+ */
  te_error_record er{s};
  te_expr * ret = factor(s);
  te_type ret_type = ret ? ret->type : TE_ERROR;

  do {
    er.set_point(s.token.name.ptr);

    te_token::kind_t tt = te_token::NONE;
    char c = '?';
    if (s.token == te_token::STAR || s.token == te_token::SLASH || s.token == te_token::PERCENT) {
      tt = s.token.kind;
      c = *s.token.name.ptr;
    } else {
      break;
    }
    s.advance();
    te_expr * rhs = factor(s);
    te_type rhs_type = rhs ? rhs->type : TE_ERROR;

    er.set_end(s.prev_token.name.end);

    te_fn_obj fn;
    switch (tt) {
      case te_token::STAR:
        fn = te_get_mul_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      case te_token::SLASH:
        fn = te_get_div_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      case te_token::PERCENT:
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
#endif
        te_print_error(s);
      }
      te_free(ret);
      ret = nullptr;
      te_free(rhs);
      rhs = nullptr;
    } else {
      te_expr * args[] = {ret, rhs};
      ret = new_call_expr(fn, {ret, rhs});
    }

    ret_type = ret ? ret->type : TE_ERROR;
  } while (true);

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited term\n");
#endif

  return ret;
}

static te_expr * expr(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered expr\n");
#endif

  /* <expr>      =    <term> {("+" | "-") <term>}+ */
  te_error_record er{s};
  te_expr * ret = term(s);
  te_type ret_type = ret ? ret->type: TE_ERROR;

  do {
    er.set_point(s.token.name.ptr);

    te_token::kind_t tt = te_token::NONE;
    char c = '?';
    if (s.token == te_token::PLUS || s.token == te_token::MINUS) {
      tt = s.token.kind;
      c = *s.token.name.ptr;
    } else {
      break;
    }
    s.advance();
    te_expr * rhs = term(s);
    te_type rhs_type = rhs ? rhs->type : TE_ERROR;

    er.set_end(s.prev_token.name.end);

    te_fn_obj fn;
    switch (tt) {
      case te_token::PLUS:
        fn = te_get_add_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      case te_token::MINUS:
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
#endif
        te_print_error(s);
      }
      te_free(ret);
      ret = nullptr;
      te_free(rhs);
      rhs = nullptr;
    } else {
      ret = new_call_expr(fn, {ret, rhs});
    }

    ret_type = ret ? ret->type : TE_ERROR;
  } while (true);

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited expr\n");
#endif

  return ret;
}


namespace tesl {
  static constexpr int precedence_count = 18;
  static constexpr int max_precedence = precedence_count - 1;

  namespace util {
    template<typename T, auto ... Vs>
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
    struct first_value_of<V, Vs> {
      static constexpr auto value = V;
    };

    template<auto ... Vs>
    struct value_pack {};

    template<typename ... Ts>
    struct type_pack {};
  }

  namespace parse {
    struct parsed_sequence_t {
      struct node {
        enum kind_t : int8_t {
          TOKEN,
          EXPR,
        } kind;

        union {
          te_token token{};
          struct {
            te_expr * expr;
            int8_t precedence;
          };
        };

        node(const te_token & t) : kind(TOKEN), token(t) {}
        node(te_expr * e) : kind(EXPR), expr(e) {}
        node() = default;
      };

      const node * nodes = nullptr;
      const int size = 0;

      operator const node *() { return nodes; }
    };

    static te_expr * parse_literal_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_literal_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_identifier_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_identifier_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_grouping_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_grouping_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_subscript_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_subscript_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_call_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_call_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_dot_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_dot_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_postfix_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_postfix_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_prefix_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_prefix_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_arithmetic_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_arithmetic_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_comparison_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_comparison_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_bitwise_op_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_bitwise_op_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_boolean_op_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_boolean_op_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_ternary_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_ternary_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_assignment_expr(te_parser_state & s, parsed_sequence_t sequence) {
      te_printf("parse_assignment_expr: n=%d", sequence.size);
      // TODO: finish
      return nullptr;
    }

    static te_expr * parse_sequence_expr(te_parser_state & s, parsed_sequence_t sequence) {
      TE_FAIL_COND(sequence.size >= 3, return nullptr);
      TE_FAIL_COND(sequence.size % 2 == 1, return nullptr);

      int sequence_size = (sequence.size + 1) / 2;
      TE_FAIL_COND(sequence_size <= TE_PARAM_COUNT_MAX, return nullptr);

      uint16_t offset = 0;
      uint16_t size = 0;
      te_expr * args[sequence_size];
      te_type param_types[sequence_size];
      for (int i = 0; i < sequence.size; ++i) {
        if (i % 2 == 0) {
          TE_FAIL_COND(sequence[i].kind == parsed_sequence_t::node::EXPR, return nullptr);
          TE_FAIL_COND(sequence[i].expr != nullptr, return nullptr);
          args[i / 2] = sequence[i].expr;
          param_types[i] = args[i]->type;
          if (i != (sequence.size - 1)) {
            offset += te_size_of(param_types[i]);
          } else {
            size = te_size_of(param_types[i]);
          }
        } else {
          TE_FAIL_COND(sequence[i].kind == parsed_sequence_t::node::TOKEN, return nullptr);
          TE_FAIL_COND(sequence[i].token.kind == te_token::COMMA, return nullptr);
        }
      }

      uint16_t ctx_data[2] = {offset, size};
      static_assert(sizeof(te_int) == sizeof(void *));
      void * ctx = reinterpret_cast<void *>(*reinterpret_cast<te_int *>(ctx_data));
      return new_call_expr(te::detail::make_function_raw(te::sequence, ctx, true, TE_NULL, param_types, sequence_size), args, sequence_size);
    }

    namespace rules {
      constexpr int16_t max_sequence_size = TE_PARAM_COUNT_MAX * 2 + 2 + 2;
      using parse_fn = te_expr * (*)(te_parser_state & s, parsed_sequence_t sequence);

      struct element_t {
        enum kind_t : int8_t {
          TOKEN,
          TOP_LEVEL_EXPR,
          RELATIVE_PRECEDENCE_EXPR,
          OPTIONAL,
          JUMP,
        };

        kind_t kind = TOKEN;
        union {
          te_token::kind_t token = te_token::NONE;
          int8_t n;
          int8_t offset;
          int8_t size;
        };


        constexpr element_t(const element_t &) = default;

        constexpr element_t(te_token::kind_t t) : kind(TOKEN), token(t) {};
        constexpr element_t(kind_t k, te_token::kind_t t) : kind(k), token(t) {};
        constexpr element_t(kind_t k, int8_t v) : kind(k), n(v) {};

        constexpr element_t() {};
      };

      struct pattern_ref_t {
        int16_t size = 0;
        int16_t sequence_size = 0;
        const element_t * elements = nullptr;

        constexpr operator const element_t *() const { return elements; }
      };

      struct rule_ref_storage_t {
        parse_fn parse = nullptr;
        pattern_ref_t pattern;
      };

      struct rule_ref_t {
        parse_fn parse = nullptr;
        pattern_ref_t pattern;
        int8_t precedence = -1;

        bool is_valid() const { return pattern.size > 0 && parse != nullptr; }

        constexpr rule_ref_t(rule_ref_storage_t r, int8_t p) : parse(r.parse), pattern(r.pattern), precedence(p) {}
        constexpr rule_ref_t() = default;
      };

      struct rule_ref_list_storage_t {
        const rule_ref_storage_t * rules = nullptr;
        int16_t size = 0;

        constexpr operator const rule_ref_storage_t *() const { return rules; }
      };

      struct rule_ref_list_t {
        const rule_ref_storage_t * rules = nullptr;
        int16_t size = 0;
        int8_t precedence = -1;

        constexpr rule_ref_t operator[](int i) const { return {rules[i], precedence}; }

        constexpr rule_ref_list_t(rule_ref_list_storage_t rl, int8_t p) : rules(rl.rules), size(rl.size), precedence(p) {}
      };

      template<auto ElementCount>
      struct pattern_t {
        static constexpr int size = ElementCount;
        element_t elements[ElementCount];

        constexpr operator const element_t *() const { return elements; }
      };

      template<typename ... Ts>
      constexpr pattern_t<sizeof...(Ts)> make_pattern(Ts ... elements) {
        static_assert(sizeof...(elements) > 0);
        return {element_t{elements}...};
      }

      template<auto ElementCount>
      struct rule_t {
        parse_fn parse = nullptr;
        pattern_t<ElementCount> pattern;
      };

      template<auto ElementCount>
      constexpr rule_t<ElementCount> make_rule(parse_fn parse_fn, pattern_t<ElementCount> pattern) {
        return {parse_fn, pattern};
      }

      template<typename T>
      struct rule_list_impl;

      template<auto V, auto ... NextVs>
      struct rule_list_impl<util::value_pack<V, NextVs...>> : rule_list_impl<util::value_pack<NextVs...>> {
        using base = rule_list_impl<util::value_pack<NextVs...>>;
        
        rule_t<V> this_rule;
        static constexpr int size = sizeof...(NextVs) + 1;

        constexpr rule_ref_storage_t operator[](int i) {
          if (i < 0) {
            return {};
          } else if (i == 0) {
            int16_t sequence_size = V;
            for (int i = 0; i < this_rule.pattern.size; ++i) {
              const element_t & e = this_rule.pattern.elements[i];
              if (e.kind == element_t::JUMP) {
                sequence_size = max_sequence_size;
              }
            }
            return {this_rule.parse, {V, sequence_size, this_rule.pattern.elements}};
          } else {
            return base::operator[](i - 1);
          }
        }

        constexpr rule_list_impl(rule_t<V> rule, rule_t<NextVs> ... next_rules) : base(next_rules...), this_rule(rule) {}
      };

      template<>
      struct rule_list_impl<util::value_pack<>> {
        rule_t<0> pattern;
        static constexpr int size = 0;

        constexpr rule_ref_storage_t operator[](int i) {
          return {};
        }
      };

      template<auto ... Vs>
      struct rule_list_t : rule_list_impl<util::value_pack<Vs...>> {
        using base = rule_list_impl<util::value_pack<Vs...>>;

        constexpr rule_list_t(rule_t<Vs> ... rules) : base(rules...) {}
      };
      // woof woof :3
      template<typename T>
      struct rule_library_impl;

      template<auto ... Vs, typename ... NextTs>
      struct rule_library_impl<util::type_pack<util::value_pack<Vs...>, NextTs...>> : rule_library_impl<util::type_pack<NextTs...>> {
        using base = rule_library_impl<util::type_pack<NextTs...>>;

        rule_list_impl<util::value_pack<Vs...>> this_list;
        rule_ref_storage_t list_rules[sizeof...(Vs)];
        static constexpr int size = sizeof...(NextTs) + 1;

        constexpr rule_ref_list_storage_t operator[](int i) {
          if (i < 0) {
            return {};
          } else if (i == 0) {
            return {list_rules, sizeof...(Vs)};
          } else {
            return base::operator[](i - 1);
          }
        }

        constexpr rule_library_impl(rule_list_impl<util::value_pack<Vs...>> rule_list, rule_list_impl<NextTs> ... next_lists) : base(next_lists...), this_list(rule_list) {
          for (int i = 0; i < sizeof...(Vs); ++i) {
            list_rules[i] = this_list[i];
          }
        }
      };

      template<>
      struct rule_library_impl<util::type_pack<>> {
        static constexpr int size = 0;

        constexpr rule_ref_list_storage_t operator[](int i) {
          return {};
        }
      };

      template<typename ... Ts>
      struct rule_library_t : public rule_library_impl<util::type_pack<Ts...>> {
        using base = rule_library_impl<util::type_pack<Ts...>>;

        rule_ref_list_storage_t lists[sizeof...(Ts)];
        static constexpr int size = sizeof...(Ts);

        constexpr rule_ref_list_t operator[](int8_t p) const { return {lists[p], p}; }

        constexpr rule_library_t(rule_list_impl<Ts> ... rule_lists) : base(rule_lists...) {
          for (int i = 0; i < sizeof...(Ts); ++i) {
            lists[i] = base::operator[](i);
          }
        }
      };

      template<int8_t Offset>
      constexpr element_t relative_precedence_expr = element_t{element_t::RELATIVE_PRECEDENCE_EXPR, Offset};
      template<int8_t Offset = 0>
      constexpr element_t top_level_expr = element_t{element_t::TOP_LEVEL_EXPR, Offset};

      // matches an expression with lower precedence
      constexpr element_t lower_expr = relative_precedence_expr<-1>;

      // matches an expression with same or lower precedence
      constexpr element_t similar_expr = relative_precedence_expr<0>;

      // marks a block of length N starting at the next element that is optional
      template<int8_t N>
      constexpr element_t opt = element_t{element_t::OPTIONAL, N};

      // jumps the element index to a relative offset N (used to make repeating patterns)
      template<int8_t N>
      constexpr element_t jmp = element_t{element_t::JUMP, N};

      // exactly one of the first or second element of a pattern must be a token
      // at most one of each token can appear in the first slot of all patterns (also applies to the second slot separately)
      // the element immediately after an 'opt<...>' or after the block it creates must be a token
      // the first expr must be a token or a similar_expr
      constexpr rule_library_t pattern_library{
        rule_list_t{
          make_rule(parse_literal_expr, make_pattern(te_token::LITERAL)),
          make_rule(parse_identifier_expr, make_pattern(te_token::IDENTIFIER)),
        },
        rule_list_t{
          make_rule(parse_grouping_expr, make_pattern(te_token::OPEN_PAREN, top_level_expr<>, te_token::CLOSE_PAREN)),
        },
        rule_list_t{
          make_rule(parse_subscript_expr, make_pattern(similar_expr, te_token::OPEN_SQUARE_BRACKET, top_level_expr<-1>, te_token::OPEN_SQUARE_BRACKET)),
          make_rule(parse_call_expr, make_pattern(similar_expr, te_token::OPEN_PAREN, opt<6>, top_level_expr<-1>, opt<4>, te_token::COMMA, top_level_expr<-1>, opt<1>, jmp<-2>, te_token::CLOSE_PAREN)),
          make_rule(parse_dot_expr, make_pattern(similar_expr, te_token::DOT, te_token::IDENTIFIER)),
          make_rule(parse_postfix_expr, make_pattern(similar_expr, te_token::PLUS_PLUS)),
          make_rule(parse_postfix_expr, make_pattern(similar_expr, te_token::MINUS_MINUS)),
        },
        rule_list_t{
          make_rule(parse_prefix_expr, make_pattern(te_token::PLUS_PLUS, similar_expr)),
          make_rule(parse_prefix_expr, make_pattern(te_token::MINUS_MINUS, similar_expr)),
          make_rule(parse_prefix_expr, make_pattern(te_token::PLUS, similar_expr)),
          make_rule(parse_prefix_expr, make_pattern(te_token::MINUS, similar_expr)),
          make_rule(parse_prefix_expr, make_pattern(te_token::BANG, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_arithmetic_expr, make_pattern(similar_expr, te_token::STAR, similar_expr)),
          make_rule(parse_arithmetic_expr, make_pattern(similar_expr, te_token::SLASH, similar_expr)),
          make_rule(parse_arithmetic_expr, make_pattern(similar_expr, te_token::PERCENT, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_arithmetic_expr, make_pattern(similar_expr, te_token::PLUS, similar_expr)),
          make_rule(parse_arithmetic_expr, make_pattern(similar_expr, te_token::MINUS, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_bitwise_op_expr, make_pattern(similar_expr, te_token::LESS_LESS, similar_expr)),
          make_rule(parse_bitwise_op_expr, make_pattern(similar_expr, te_token::GREATER_GREATER, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_comparison_expr, make_pattern(similar_expr, te_token::LESS, similar_expr)),
          make_rule(parse_comparison_expr, make_pattern(similar_expr, te_token::GREATER, similar_expr)),
          make_rule(parse_comparison_expr, make_pattern(similar_expr, te_token::LESS_EQUAL, similar_expr)),
          make_rule(parse_comparison_expr, make_pattern(similar_expr, te_token::GREATER_EQUAL, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_comparison_expr, make_pattern(similar_expr, te_token::EQUAL_EQUAL, similar_expr)),
          make_rule(parse_comparison_expr, make_pattern(similar_expr, te_token::BANG_EQUAL, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_bitwise_op_expr, make_pattern(similar_expr, te_token::AND, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_bitwise_op_expr, make_pattern(similar_expr, te_token::CARET, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_bitwise_op_expr, make_pattern(similar_expr, te_token::PIPE, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_boolean_op_expr, make_pattern(similar_expr, te_token::AND_AND, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_boolean_op_expr, make_pattern(similar_expr, te_token::CARET_CARET, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_boolean_op_expr, make_pattern(similar_expr, te_token::PIPE_PIPE, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_ternary_expr, make_pattern(similar_expr, te_token::QUESTION_MARK, similar_expr, te_token::COLON, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_assignment_expr, make_pattern(similar_expr, te_token::EQUAL, similar_expr)),
          make_rule(parse_assignment_expr, make_pattern(similar_expr, te_token::PLUS_EQUAL, similar_expr)),
          make_rule(parse_assignment_expr, make_pattern(similar_expr, te_token::MINUS_EQUAL, similar_expr)),
          make_rule(parse_assignment_expr, make_pattern(similar_expr, te_token::STAR_EQUAL, similar_expr)),
          make_rule(parse_assignment_expr, make_pattern(similar_expr, te_token::SLASH_EQUAL, similar_expr)),
        },
        rule_list_t{
          make_rule(parse_sequence_expr, make_pattern(similar_expr, te_token::COMMA, lower_expr, opt<1>, jmp<-3>)),
        },
      };
      static_assert(pattern_library.size == precedence_count);
      constexpr int pattern_library_size = sizeof(pattern_library);

      rule_ref_t find_rule_precedence(te_parser_state & s, int precedence, int element_index) {
        for (int p = precedence; p >= 0; --p) {
          rule_ref_list_t pattern_list = pattern_library[p];
          for (int i = 0; i < pattern_list.size; ++i) {
            rule_ref_t rule = pattern_list[i];
            if (rule.pattern[element_index].token == s.token) {
              return rule;
            }
          }
        }

        return {};
      }
    }

    te_expr * parse_precedence(te_parser_state & s, int precedence);

    te_expr * parse_precedence_impl(te_parser_state & s, rules::rule_ref_t rule, parsed_sequence_t::node initial) {
      using namespace rules;

      int sequence_size = rule.pattern.sequence_size;
      TE_FAIL_COND(sequence_size < 1, return new_error_expr());

      parsed_sequence_t::node nodes[sequence_size];
      nodes[0] = initial;
      int node_count = 1;

#define FAIL_RETURN_ERROR_EXPR\
  do {\
    te_expr * exprs[node_count];\
    int exprs_count = 0;\
    for (int i = node_count - 1; i >= 0; --i) {\
      if (nodes[i].kind == parsed_sequence_t::node::EXPR) {\
        exprs[exprs_count++] = nodes[i].expr;\
      }\
    }\
    return new_error_expr(exprs, exprs_count);\
  } while (false)

#define PUSH_NODE(n)\
  do {\
    if (node_count < sequence_size) {\
      nodes[node_count++] = n;\
    } else {\
      te_error_record er(s);\
      te_printf("internal error: reached max sequence size!\n");\
      te_print_error(s);\
      FAIL_RETURN_ERROR_EXPR;\
    }\
  } while (false)

      for (int i = 1; i < sequence_size; ++i) {
        const element_t & e = rule.pattern[i];
        switch (e.kind) {
          case rules::element_t::TOKEN: {
            if (e.token == s.token) {
              te_error_record er(s);
#ifdef TE_DEBUG_COMPILE
              te_printf("error: expected %k, got %k!\n", e.token, s.token.kind);
#endif
              te_print_error(s);
              FAIL_RETURN_ERROR_EXPR;
            }
            PUSH_NODE(s.token);
            s.advance();
          } break;
          case rules::element_t::TOP_LEVEL_EXPR: {
            PUSH_NODE(parse_precedence(s, max_precedence + e.offset));
          } break;
          case rules::element_t::RELATIVE_PRECEDENCE_EXPR: {
            PUSH_NODE(parse_precedence(s, rule.precedence + e.offset));
          } break;
          case rules::element_t::OPTIONAL: {
            const element_t & opt_e1 = rule.pattern[i + 1];
            const element_t & opt_e2 = rule.pattern[i + 1 + e.size];
            TE_FAIL_COND_MSG(opt_e1.kind == rules::element_t::TOKEN && opt_e2.kind == rules::element_t::TOKEN, FAIL_RETURN_ERROR_EXPR, "internal error: expected at least one token at beginning or end of optional block!\n");
            if (opt_e1.kind == rules::element_t::TOKEN && opt_e1.token == s.token) {
              // let it progress
              continue;
            } else if (opt_e2.kind == rules::element_t::TOKEN && opt_e2.token == s.token) {
              // jump to end of optional block
              i += e.size;
              continue;
            } else {
              te_error_record er(s);
#ifdef TE_DEBUG_COMPILE
              te_printf("error: expected %k or %k, got %k!\n", opt_e1.token, opt_e2.token, s.token.kind);
#endif
              te_print_error(s);
              FAIL_RETURN_ERROR_EXPR;
            }
          } break;
          case rules::element_t::JUMP: {
            i += e.size - 1;
            continue;
          } break;
        }
      }

      te_expr * ret = rule.parse(s, {nodes, node_count});
      if (!ret) {
        FAIL_RETURN_ERROR_EXPR;
      }

      return ret;

#undef PUSH_NODE
#undef FAIL_RETURN_ERROR_EXPR
    }

    // never returns null
    te_expr * parse_precedence(te_parser_state & s, int precedence) {
      using namespace rules;

      rule_ref_t rule = find_rule_precedence(s, precedence, 0);
      te_expr * expr = parse_precedence_impl(s, rule, s.consume());
      while (true) {
        rule = find_rule_precedence(s, precedence, 1);
        if (!rule.is_valid()) {
          break;
        }
        expr = parse_precedence_impl(s, rule, expr);
      }

      return expr;
    }
  }
}


template<typename T, int ChunkSize = 256>
class te_stack_list_base {
protected:
  te_stack_list_base * m_next = nullptr;
  T m_data[ChunkSize];
};

template<typename T, int ChunkSize = 256>
class te_stack_list : protected te_stack_list_base<T, ChunkSize> {
  using base = te_stack_list_base<T, ChunkSize>;

  int32_t m_size = 0;
  int32_t m_chunk_count = 1;

public:
  bool at_limit() {
    return m_size == (m_chunk_count * ChunkSize);
  }

  base * get_chunk_at(int i) {
    base * c = this;
    while (c && i >= ChunkSize) {
      i -= ChunkSize;
      c = c->m_next;
    }
    return i < ChunkSize ? c : nullptr;
  }

  void add_chunk(base & next_chunk) {
    base *& c = base::m_next;
    while (c) c = c->m_next;
    c = next_chunk;
    m_chunk_count++;
  }

  template<typename T2>
  void push_back(T2 &&  v) {
    assert(!at_limit());

    int i = m_size;
    base * c = this;
    while (c && i >= ChunkSize) {
      i -= ChunkSize;
      c = c->m_next;
    }
    assert(i < ChunkSize);

    c->m_data[i] = static_cast<T2>(v);
  }

  T & operator[](int32_t i) {
    base * c = this;
    while (c && i >= ChunkSize) {
      i -= ChunkSize;
      c = c->m_next;
    }

    assert(i < ChunkSize);

    if (i < ChunkSize) {
      assert(i < m_size);
      return base::m_data[i];
    }
  }
};

// TODO: finish
#define expand_stack_list(vec)

// TODO: finish
#define expand_stack_list(vec)

static te_expr * parse_fn(te_parser_state & s) {
  /* <fn>        =    <typename> <identifier> "(" {<typename> {<identifier>} "," }+ ")" <stmt> */
  return nullptr;
}

static void begin_function(te_parser_state & s) {
  s.parse_error = false;
}

static te_expr * end_function(te_parser_state & s) {
  te_expr * ret = nullptr;

#ifdef TE_DEBUG_COMPILE
  if (!s.parse_error) {
    for (int i = 0; i < s.stmt_count; ++i) {
      if (s.stmts[i] == nullptr) {
        te_printf("internal error: null stmt despite no parser error!\n");
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
    te_error_record er{s};
#ifdef TE_DEBUG_COMPILE
    te_printf("error: ran out of room for stmts! max=%d\n", TE_MAX_STMT_COUNT);
#endif
    te_print_error(s);
  }
}

static void parse_stmt(te_parser_state & s);

static void parse_suite(te_parser_state & s, te_token::kind_t end_token) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered suite\n");
#endif

  {
    te_scope_decl scope_decl(s);

    te_expr * ret = nullptr;
    while (true) {
      if (s.token == end_token) {
        s.advance();
        break;
      } else if (s.token == te_token::END) {
        te_error_record er2{s};
  #ifdef TE_DEBUG_COMPILE
        te_printf("error: end of input!\n");
  #endif
        te_print_error(s);
        break;
      }

      parse_stmt(s);
    }
  }

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited suite: stmt_count=%d\n", s.stmt_count);
#endif
}

static te_expr * parse_standalone_suite(te_parser_state & s) {
  begin_function(s);
  parse_suite(s, te_token::END);
  return end_function(s);
}

static void parse_if_stmt(te_parser_state & s) {
  s.advance();

  if (s.token != te_token::OPEN_PAREN) {
    te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
    te_printf("error: expected '(' got ");
    te_print_token(s.token);
    te_printf("!\n");
#endif
    te_print_error(s);
  }
  s.advance();

  te_error_record er{s};
  te_expr * cond_expr = expr(s);
  te_type cond_expr_result_type = cond_expr ? cond_expr->type : TE_ERROR;

  er.set_end(s.token.name.end);

  if (s.token != te_token::CLOSE_PAREN) {
    te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
    te_printf("error: expected ')' got ");
    te_print_token(s.token);
    te_printf("!\n");
#endif
    te_print_error(s);
  }
  s.advance();

  te_jmp_if_not_op * jmp_if_expr = new_jmp_if_not_op(te_jmp_op::unknown_offset, cond_expr);
  push_stmt(s, jmp_if_expr);

  if (jmp_if_expr != nullptr && (cond_expr_result_type | TE_CONSTANT) != TE_INT) {
#ifdef TE_DEBUG_COMPILE
    te_printf("error: conditional expr must be an int! type: ");
    te_print_type_name(cond_expr_result_type);
    te_printf("\n");
#endif
    te_print_error(s);
  }

  parse_stmt(s);

  if (s.token == te_token::ELSE) {
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
  te_type decl_type = s.token.type;
  s.advance();

  if (s.token != te_token::IDENTIFIER) {
    te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
    te_printf("error: expected <identifier> got ");
    te_print_token(s.token);
    te_printf("!\n");
#endif
    te_print_error(s);
    return;
  }
  te_parser_state::local_var_t * var = te_stack_allocate_var(s, s.token.name, decl_type);
  s.advance();

  if (allow_assign && s.token == te_token::EQUAL) {
    s.advance();

    te_expr *initializer = expr(s);
    push_stmt(s, new_assign_expr(var ? new_stack_ref_expr(var->type, var->offset) : nullptr, initializer));
  }
}

static void parse_for_stmt(te_parser_state & s) {
  s.advance();

  if (s.token != te_token::OPEN_PAREN) {
    te_error_record er{s};
#ifdef TE_DEBUG_COMPILE
    te_printf("error: expected '(' got ");
    te_print_token(s.token);
    te_printf("!\n");
#endif
    te_print_error(s);
  }
  s.advance();

  {
    te_scope_decl scope_decl(s);

    parse_var_stmt(s, true);

    if (s.token != te_token::SEMICOLON) {
      te_error_record er{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected ';' got ");
      te_print_token(s.token);
      te_printf("!\n");
#endif
      te_print_error(s);
    }
    s.advance();

    te_error_record er{s};
    te_expr * cond_expr = expr(s);
    te_type cond_expr_result_type = cond_expr ? cond_expr->type : TE_ERROR;
    er.set_end(s.token.name.end);

    if (s.token != te_token::SEMICOLON) {
      te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected ';' got ");
      te_print_token(s.token);
      te_printf("!\n");
#endif
      te_print_error(s);
    }
    s.advance();

    te_jmp_op * jmp_cond_expr = new_jmp_op(te_jmp_op::unknown_offset);
    push_stmt(s, jmp_cond_expr);

    te_expr * update_expr = expr(s);

    if (s.token != te_token::CLOSE_PAREN) {
      te_error_record er2{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected ')' got ");
      te_print_token(s.token);
      te_printf("!\n");
#endif
      te_print_error(s);
    }
    s.advance();

    int32_t loop_start_offset = s.stmt_count;

    parse_stmt(s);

    push_stmt(s, update_expr);

    int32_t loop_cond_offset = s.stmt_count;
    te_jmp_if_op * jmp_if_expr = new_jmp_if_op(loop_start_offset, cond_expr);
    push_stmt(s, jmp_if_expr);

    if (jmp_if_expr != nullptr && (cond_expr_result_type | TE_CONSTANT) != TE_INT) {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: conditional expr must be an int! type: ");
      te_print_type_name(cond_expr_result_type);
      te_printf("\n");
#endif
      te_print_error(s);
    }

    if (jmp_cond_expr) {
      jmp_cond_expr->offset = loop_cond_offset;
    }
  }
}

static void parse_return_stmt(te_parser_state & s) {
  s.advance();

  te_error_record er{s};

  if (s.token == te_token::SEMICOLON) {
    if (s.return_type != TE_NULL) {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected ");
      te_print_type_name(s.return_type);
      te_printf(" expression!\n");
#endif
      te_print_error(s);
    }
  } else {
    te_expr * e = expr(s);
    te_type result_type = e ? e->type : TE_ERROR;

    er.set_end(s.prev_token.name.end);

    if (result_type == TE_ERROR) {
      s.parse_error = true;
    } else if ((result_type | TE_CONSTANT) != s.return_type) {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: 'return' cannot convert ");
      te_print_type_name(result_type);
      te_printf(" to ");
      te_print_type_name(s.return_type);
      te_printf("\n");
#endif
      te_print_error(s);
    } else {
      push_stmt(s, new_return_op(s.return_type, e));
    }
  }
}

static void parse_stmt(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered stmt\n");
#endif

  /* <stmt>      =    ("{" {<stmt>}+ "}" | "if" "(" <int-expr> ")" <stmt> {"else" <stmt>} | ("return" <vexpr> | "break" | "continue" | <typename> <id> {"=" <vexpr>} | <rexpr> ("=" | "+=" | "-=" | "*=" | "/=" | "%=") <vexpr> | <expr> ";")) */
  if (s.token == te_token::OPEN_CURLY_BRACKET) {
    s.advance();
    parse_suite(s, te_token::CLOSE_CURLY_BRACKET);
  } else if (s.token == te_token::IF) {
    parse_if_stmt(s);
  } else if (s.token == te_token::FOR) {
    parse_for_stmt(s);
  } else {
    if (s.token == te_token::TYPENAME) {
      parse_var_stmt(s, true);
    } else if (s.token == te_token::RETURN) {
      parse_return_stmt(s);
    } else {
      const char * token_start = s.token.name.ptr;
      push_stmt(s, expr(s));
      if (s.token.name.ptr == token_start) {
        // don't get stuck
        s.advance();
      }
    }

    if (s.token == te_token::SEMICOLON) {
      s.advance();
    } else {
      te_error_record er{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected ';', got ");
      te_print_token(s.token);
      te_printf("!\n");
#endif
      te_print_error(s);
    }
  }
}

static te_expr * parse_program(te_parser_state & s) {
  /* <program>   =    {{"const" <var> | "uniform" <var> | <fn>} ";"}+ */
  // TODO: finish
  s.return_type = TE_VEC3;
  return parse_standalone_suite(s);
}

void te_eval_internal(const te_expr * n, char * p_stack, te_value * ret) {
#ifdef TE_DEBUG_EVAL

#define TE_ERR_FAIL_COND(cond, fail_action, ...)\
  do {\
    if (cond) {\
        te_printf(__VA_ARGS__);\
        te_printf("\n");\
        fail_action;\
    }\
  } while(false)

#define TE_CHECK_EXPECTED_TYPE(chk_type, expected, fail_action, ...)\
  do {\
    if ((chk_type) != (expected)) {\
        te_printf(__VA_ARGS__);\
        te_printf(" expected ");\
        te_print_type_name(te_type(expected));\
        te_printf(", got ");\
        te_print_type_name(te_type(chk_type));\
        te_printf("\n");\
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
      te_printf("TE_OP_VALUE\n");
#endif
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_value_expr * value_expr = reinterpret_cast<const te_value_expr *>(n);
      memcpy(ret, &value_expr->value, value_expr->size);
    } break;
    case TE_OP_STACK_REF: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_STACK_REF\n");
#endif
      TE_ERR_FAIL_COND(!p_stack, return, "eval error: stack ptr is null!");
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_stack_ref_expr * stack_ref_expr = reinterpret_cast<const te_stack_ref_expr *>(n);
      te_value * ref = reinterpret_cast<te_value *>(p_stack + stack_ref_expr->offset);
      ret->ref = ref;
    } break;
    case TE_OP_STACK_REF_REF: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_STACK_REF_REF\n");
#endif
      TE_ERR_FAIL_COND(!p_stack, return, "eval error: stack ptr is null!");
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_stack_ref_expr * stack_ref_expr = reinterpret_cast<const te_stack_ref_expr *>(n);
      te_value * ref = *reinterpret_cast<te_value **>(p_stack + stack_ref_expr->offset);
      ret->ref = ref;
    } break;
    case TE_OP_DEREF: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_DEREF\n");
#endif
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_deref_expr * deref_expr = reinterpret_cast<const te_deref_expr *>(n);
      void * ref;
      te_eval_internal(deref_expr->arg, p_stack, reinterpret_cast<te_value *>(&ref));
      memcpy(ret, ref, deref_expr->size);
    } break;
    case TE_OP_ASSIGN: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_ASSIGN\n");
#endif
      TE_ERR_FAIL_COND(!p_stack, return, "eval error: stack ptr is null!");
      TE_ERR_FAIL_COND(!ret, return, "eval error: return ptr is null!");

      const te_assign_expr * assign_expr = reinterpret_cast<const te_assign_expr *>(n);
      TE_ERR_FAIL_COND(!TE_IS_REF(assign_expr->lhs->type), return, "eval error: lhs is not a ref!");

      void * lhs;
      te_eval_internal(assign_expr->lhs, p_stack, reinterpret_cast<te_value *>(&lhs));
      TE_ERR_FAIL_COND(!lhs, return, "eval error: lhs ref is null!");

      TE_CHECK_EXPECTED_TYPE(assign_expr->lhs->type | TE_CONSTANT, assign_expr->rhs->type, return, "eval error: assignment lhs and rhs do not match!");
      te_eval_internal(assign_expr->rhs, p_stack, reinterpret_cast<te_value *>(lhs));
      ret->ptr = lhs;
    } break;
    case TE_OP_CALL: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_CALL\n");
#endif
      const te_call_expr * call_expr = reinterpret_cast<const te_call_expr *>(n);
      char * stack = static_cast<char *>(alloca(call_expr->arg_stack_size));
      {
        char * stackptr = stack;
        te_type param_type = TE_ERROR;
        uint8_t param_count = call_expr->fn.param_count;
        uint8_t param_size = 0;
        for (int i = 0; i < param_count; ++i, stackptr += param_size) {
          param_type = call_expr->fn.param_types[i];
          param_size = te_size_of(param_type);

          TE_ERR_FAIL_COND(!call_expr->args[i], continue, "eval error: fn arg expr ptr is null!");
          TE_CHECK_EXPECTED_TYPE(call_expr->args[i]->type, param_type, continue, "eval error: argument and parameter types do not match!");
          te_eval_internal(call_expr->args[i], p_stack, reinterpret_cast<te_value *>(stackptr));
        }
      }

      TE_ERR_FAIL_COND(!call_expr->fn.is_valid(), return, "eval error: null function call!");
      TE_ERR_FAIL_COND(!ret && call_expr->type != TE_NULL, return, "eval error: return ptr is null but function returns a value!");

      call_expr->fn.ptr(call_expr->fn.context, stack, ret);
    } break;
    case TE_OP_SUITE: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("TE_OP_SUITE\n");
#endif
      const te_suite_expr * suite_expr = reinterpret_cast<const te_suite_expr *>(n);
      char * stack = static_cast<char *>(alloca(suite_expr->stack_size));

      for (int32_t i = 0; i < suite_expr->stmt_count;) {
        te_op * stmt = suite_expr->stmts[i];
        switch (stmt->opcode) {
          case TE_OP_JMP: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_JMP\n");
#endif
            const te_jmp_op * jmp_op = reinterpret_cast<const te_jmp_op *>(stmt);
            TE_ERR_FAIL_COND(jmp_op->offset == te_jmp_op::unknown_offset, return, "eval error: unknown offset!");
            i = jmp_op->offset;
            continue;
          } break;
          case TE_OP_JMP_REF: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_JMP_REF\n");
#endif
            const te_jmp_ref_op * jmp_ref_op = reinterpret_cast<const te_jmp_ref_op *>(stmt);
            const uint16_t offset = *jmp_ref_op->offset_ref;
            TE_ERR_FAIL_COND(offset == te_jmp_op::unknown_offset, return, "eval error: unknown offset!");
            i = offset;
            continue;
          } break;
          case TE_OP_JMP_IF: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_JMP_IF\n");
#endif
            const te_jmp_if_op * jmp_if_op = reinterpret_cast<const te_jmp_if_op *>(stmt);
            TE_CHECK_EXPECTED_TYPE(jmp_if_op->condition->type, TE_INT, return, "eval error: incorrect type for conditional!");
            TE_ERR_FAIL_COND(jmp_if_op->offset == te_jmp_op::unknown_offset, return, "eval error: unknown offset!");

            te_int cond = 0;
            te_eval_internal(jmp_if_op->condition, stack, reinterpret_cast<te_value *>(&cond));

            if (cond) {
              i = jmp_if_op->offset;
              continue;
            }
          } break;
          case TE_OP_JMP_IF_NOT: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_JMP_IF_NOT\n");
#endif
            const te_jmp_if_not_op * jmp_if_not_op = reinterpret_cast<const te_jmp_if_not_op *>(stmt);
            TE_CHECK_EXPECTED_TYPE(jmp_if_not_op->condition->type, TE_INT, return, "eval error: incorrect type for conditional!");
            TE_ERR_FAIL_COND(jmp_if_not_op->offset == te_jmp_op::unknown_offset, return, "eval error: unknown offset!");

            te_int cond = 0;
            te_eval_internal(jmp_if_not_op->condition, stack, reinterpret_cast<te_value *>(&cond));

            if (!cond) {
              i = jmp_if_not_op->offset;
              continue;
            }
          } break;
          case TE_OP_RETURN: {
#ifdef TE_DEBUG_PEDANTIC
            te_printf("TE_OP_RETURN\n");
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
            te_printf("eval stmt expr\n");
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
  if (TE_IS_REF(n->type) && !ret->ref)
  TE_ERR_FAIL_COND(TE_IS_REF(n->type) && !ret->ref, return, "eval error: null reference!");
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
      bool args_constant = true;
      for (int i = 0; i > call_expr->fn.param_count; ++i) {
        te_optimize(call_expr->args[i]);
        if (!call_expr->args[i] || !TE_IS_CONSTANT(call_expr->args[i]->type)) {
          args_constant = false;
        }
      }
      if (args_constant && call_expr->fn.pure) {
        int value_expr_size = sizeof(te_value_expr) - sizeof(te_value) + te_size_of(call_expr->type);
        // Only try if it fits in the already allocated obj.
        if (value_expr_size <= te_size_of_op(call_expr)) {
          te_value reduced; // just to be safe, use a stack var instead of overwriting expr value
          te_eval_internal(call_expr, nullptr, &reduced);
          te_free_args(call_expr);
          te_value_expr * value_expr = reinterpret_cast<te_value_expr *>(op);
          value_expr->opcode = TE_OP_VALUE;
          // Type is the same.
          value_expr->size = te_size_of(value_expr->type);
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

te_typed_value te_eval(const te_expr * e) {
  if (!e) {
    return {te_value{}, TE_ERROR};
  }

  te_typed_value ret;
  te_eval_internal(e, nullptr, &ret);
  ret.type = e->type;
  return ret;
}

void te_program::optimize() const {
  te_optimize(root_expr);
}

template<auto ParseFn>
static te_program te_compile_internal(const char * expression, te_variable * variables, int var_count, te_type result_type = TE_NULL) {
  te_parser_state s;
  s.return_type = result_type;
  s.program = s.line_start = expression;
  s.token = {te_token::NONE, te_strview{expression, 0}};
  s.prev_token = s.token;
  s.global_vars = variables;
  s.global_var_count = var_count;

  s.advance();
  te_expr * root = ParseFn(s);

  if (s.token != te_token::END) {
    if (root && root->type != TE_ERROR) {
      te_error_record er{s};
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected end!\n");
#endif
      te_print_error(s);
    }
    te_free(root);
    root = nullptr;
  }

  return {root, s.prev_error};
}

te_program te_compile_program(const char * expression, te_variable * variables, int var_count) {
  return te_compile_internal<parse_program>(expression, variables, var_count);
}

te_program te_compile_suite(const char * expression, te_variable * variables, int var_count, te_type result_type) {
  return te_compile_internal<parse_standalone_suite>(expression, variables, var_count, result_type);
}

te_program te_compile_expr(const char * expression, te_variable * variables, int var_count) {
  return te_compile_internal<expr>(expression, variables, var_count);
}

te_typed_value te_interp(const char * expression, te_error_record * error) {
  te_program prog = te_compile_expr(expression, nullptr, 0);
  if (error) {
    *error = prog.error;
  }

  te_typed_value ret;
  ret.type = TE_ERROR;
  if (prog.root_expr) {
    ret.type = prog.root_expr->type;
    te_eval_internal(prog.root_expr, nullptr, &ret);
  }

  return ret;
}

void te_print_value(const te_typed_value & v) {
  te_type type = v.type;
  if (type == TE_FUNCTION) {
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
        te_print_value({*reinterpret_cast<const te_value *>(&v.mat2.arr[0]), TE_VEC2});
        te_printf(", ");
        te_print_value({*reinterpret_cast<const te_value *>(&v.mat2.arr[1]), TE_VEC2});
        te_printf(")");
        break;
      case TE_MAT3:
        te_printf("mat3(");
        te_print_value({*reinterpret_cast<const te_value *>(&v.mat3.arr[0]), TE_VEC3});
        te_printf(", ");
        te_print_value({*reinterpret_cast<const te_value *>(&v.mat3.arr[1]), TE_VEC3});
        te_printf(", ");
        te_print_value({*reinterpret_cast<const te_value *>(&v.mat3.arr[2]), TE_VEC3});
        te_printf(")");
        break;
      case TE_MAT4:
        te_printf("mat4(");
        te_print_value({*reinterpret_cast<const te_value *>(&v.mat4.arr[0]), TE_VEC4});
        te_printf(", ");
        te_print_value({*reinterpret_cast<const te_value *>(&v.mat4.arr[1]), TE_VEC4});
        te_printf(", ");
        te_print_value({*reinterpret_cast<const te_value *>(&v.mat4.arr[2]), TE_VEC4});
        te_printf(", ");
        te_print_value({*reinterpret_cast<const te_value *>(&v.mat4.arr[3]), TE_VEC4});
        te_printf(")");
        break;
      case TE_STR:
        te_printf(v.str);
        break;
      default:
        te_printf("<unknown:0x%04x>", int(type));
        break;
    }
  }
}

static const char swizzle_chars[] = "xyzw";

static void pn(const te_op * op, int depth) {
  if (!op) {
    te_printf("<nullptr>\n");
    return;
  }

  te_printf("%*s", depth, "");
  switch (op->opcode) {
    case TE_OP_NONE: {
      te_printf("none\n");
    } break;
    case TE_OP_ERROR: {
      te_printf("error\n");
    } break;
    case TE_OP_VALUE: {
      const te_value_expr * value_expr = reinterpret_cast<const te_value_expr *>(op);
      te_print_value({value_expr->value, value_expr->type});
      te_printf("\n");
    } break;
    case TE_OP_STACK_REF:
    case TE_OP_STACK_REF_REF: {
      const te_stack_ref_expr * stack_ref_expr = reinterpret_cast<const te_stack_ref_expr *>(op);
      te_print_type_name(stack_ref_expr->type);
      te_printf("(stack@0x%04x)", int(stack_ref_expr->offset));
    } break;
    case TE_OP_DEREF: {
      const te_deref_expr * deref_expr = reinterpret_cast<const te_deref_expr *>(op);
      te_printf("deref\n");
      pn(deref_expr->arg, depth + 1);
    } break;
    case TE_OP_ASSIGN: {
      const te_assign_expr * assign_expr = reinterpret_cast<const te_assign_expr *>(op);
      te_printf("assign\n");
      pn(assign_expr->lhs, depth + 1);
      pn(assign_expr->rhs, depth + 1);
    } break;
    case TE_OP_CALL: {
      const te_call_expr * call_expr = reinterpret_cast<const te_call_expr *>(op);
      if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_add_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        te_printf("add\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_sub_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        te_printf("sub\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_mul_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        te_printf("mul\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_div_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        te_printf("div\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_mod_func(call_expr->args[0]->type, call_expr->args[1]->type).ptr) {
        te_printf("mod\n");
      } else if (call_expr->fn.param_count == 1 && call_expr->fn.ptr == te_get_negate_func(call_expr->args[0]->type).ptr) {
        te_printf("negate\n");
      } else if (call_expr->fn.param_count == 2 && call_expr->fn.ptr == te_get_index_func(call_expr->args[0]->type).ptr && call_expr->args[1]->opcode == TE_OP_VALUE && call_expr->args[1]->type == TE_INT) {
        te_printf("index.%ld\n", long(reinterpret_cast<const te_value_expr *>(call_expr->args[1])->value.int_));
      } else if (call_expr->fn.param_count == 3 && call_expr->fn.ptr == te_get_swizzle2_func(call_expr->args[0]->type).ptr && call_expr->args[1]->opcode == TE_OP_VALUE && call_expr->args[1]->type == TE_INT && call_expr->args[2]->opcode == TE_OP_VALUE && call_expr->args[2]->type == TE_INT) {
        te_printf("swizzle.%c%c\n", swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[1])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[2])->value.int_]);
      } else if (call_expr->fn.param_count == 4 && call_expr->fn.ptr == te_get_swizzle3_func(call_expr->args[0]->type).ptr && call_expr->args[1]->opcode == TE_OP_VALUE && call_expr->args[1]->type == TE_INT && call_expr->args[2]->opcode == TE_OP_VALUE && call_expr->args[2]->type == TE_INT && call_expr->args[3]->opcode == TE_OP_VALUE && call_expr->args[3]->type == TE_INT) {
        te_printf("swizzle.%c%c%c\n", swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[1])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[2])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[3])->value.int_]);
      } else if (call_expr->fn.param_count == 5 && call_expr->fn.ptr == te_get_swizzle4_func(call_expr->args[0]->type).ptr && call_expr->args[1]->opcode == TE_OP_VALUE && call_expr->args[1]->type == TE_INT && call_expr->args[2]->opcode == TE_OP_VALUE && call_expr->args[2]->type == TE_INT && call_expr->args[3]->opcode == TE_OP_VALUE && call_expr->args[3]->type == TE_INT && call_expr->args[4]->opcode == TE_OP_VALUE && call_expr->args[4]->type == TE_INT) {
        te_printf("swizzle.%c%c%c%c\n", swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[1])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[2])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[3])->value.int_], swizzle_chars[reinterpret_cast<const te_value_expr *>(call_expr->args[4])->value.int_]);
      } else if (call_expr->fn.ptr == (te_function) &pre_increment) {
        te_printf("pre-increment\n");
      } else if (call_expr->fn.ptr == (te_function) &post_increment) {
        te_printf("post-increment\n");
      } else if (call_expr->fn.ptr == (te_function) &pre_decrement) {
        te_printf("pre-decrement\n");
      } else if (call_expr->fn.ptr == (te_function) &post_decrement) {
        te_printf("post-decrement\n");
      } else if (call_expr->fn.ptr == (te_function) &bool_not) {
        te_printf("not\n");
      } else {
        bool found = false;
        for (int i = 0; i < te_builtins_count; ++i) {
          if (te_builtins[i].type == TE_FUNCTION && call_expr->fn.ptr == te_builtins[i].fn.ptr) {
            found = true;
            te_strview fn_name = te_builtins[i].name;
            te_printf("%.*s", fn_name.len(), fn_name.ptr);
          }
        }

        if (!found) {
          te_print_value({call_expr->fn, TE_FUNCTION});
        }
        te_printf("\n");
      }
      for (int i = 0; i < call_expr->fn.param_count; i++) {
        pn(call_expr->args[i], depth + 1);
      }
    } break;
    case TE_OP_SUITE: {
      const te_suite_expr * suite_expr = reinterpret_cast<const te_suite_expr *>(op);
      te_printf("suite\n");
      for (int i = 0; i < suite_expr->stmt_count; ++i) {
        pn(suite_expr->stmts[i], depth + 1);
      }
    } break;
    case TE_OP_JMP: {
      const te_jmp_op * jmp_op = reinterpret_cast<const te_jmp_op *>(op);
      te_printf("jmp to %d\n", int(jmp_op->offset));
    } break;
    case TE_OP_JMP_REF: {
      const te_jmp_ref_op * jmp_ref_op = reinterpret_cast<const te_jmp_ref_op *>(op);
      te_printf("jmp to ref %d\n", int(* jmp_ref_op->offset_ref));
    } break;
    case TE_OP_JMP_IF: {
      const te_jmp_if_op * jmp_if_op = reinterpret_cast<const te_jmp_if_op *>(op);
      te_printf("jmp to %d if\n", int(jmp_if_op->offset));
      pn(jmp_if_op->condition, depth + 1);
    } break;
    case TE_OP_JMP_IF_NOT: {
      const te_jmp_if_not_op * jmp_if_not_op = reinterpret_cast<const te_jmp_if_not_op *>(op);
      te_printf("jmp to %d if not\n", int(jmp_if_not_op->offset));
      pn(jmp_if_not_op->condition, depth + 1);
    } break;
    case TE_OP_RETURN: {
      const te_return_op * return_op = reinterpret_cast<const te_return_op *>(op);
      te_printf("return\n");
      pn(return_op->arg, depth + 1);
    } break;
  }
}

void te_print_expr(const te_expr * n) {
  pn(n, 0);
}

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
#include <cctype>
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

struct te_token {
  enum type_t : char {
    NUL,
    END,
    SEP,
    DOT,
    SEMICOLON,
    TYPENAME,
    IDENTIFIER,
    OPEN_PAREN,
    CLOSE_PAREN,
    OPEN_SQUARE_BRACKET,
    CLOSE_SQUARE_BRACKET,
    OPEN_CURLY_BRACKET,
    CLOSE_CURLY_BRACKET,
    INT_LITERAL,
    FLOAT_LITERAL,
    STR_LITERAL,
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
    OR,
    OR_OR,
    EQUAL,
    EQUAL_EQUAL,
    BANG,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    ADD,
    ADD_EQUAL,
    SUB,
    SUB_EQUAL,
    MUL,
    MUL_EQUAL,
    DIV,
    DIV_EQUAL,
    MOD,
    MOD_EQUAL,
    INC,
    DEC,
  };

  bool operator==(type_t t) {
    return type == t;
  }

  type_t type = NUL;
  te_strview name;
};

extern "C" {
  int _handle_extra_vsnprintf_spec(char spec, out_fct_type out, void* buffer, size_t idx, size_t maxlen, va_list *va) {
    static const char * hex_chars = "0123456789abcdef";

#define OUT_STR_(str) for (int i = 0; i < (sizeof(str) - 1); ++i) { out(str[i], buffer, idx++, maxlen); }
    switch (spec) {
      case 'y': {
        te_type type = (te_type)va_arg(*va, int);
        if (type == TE_FUNCTION) {
          OUT_STR_("<function>");
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
        te_token::type_t tok = te_token::type_t(va_arg(*va, int));
        switch (tok) {
          case te_token::NUL:
            OUT_STR_("<null>");
            break;
          case te_token::END:
            OUT_STR_("<end>");
            break;
          case te_token::SEP:
            OUT_STR_("','");
            break;
          case te_token::DOT:
            OUT_STR_("'.'");
            break;
          case te_token::SEMICOLON:
            OUT_STR_("';'");
            break;
          case te_token::TYPENAME:
            OUT_STR_("<typename>");
            break;
          case te_token::IDENTIFIER:
            OUT_STR_("<identifier>");
            break;
          case te_token::OPEN_PAREN:
            OUT_STR_("'('");
            break;
          case te_token::CLOSE_PAREN:
            OUT_STR_("')'");
            break;
          case te_token::OPEN_SQUARE_BRACKET:
            OUT_STR_("'['");
            break;
          case te_token::CLOSE_SQUARE_BRACKET:
            OUT_STR_("']'");
            break;
          case te_token::OPEN_CURLY_BRACKET:
            OUT_STR_("'{'");
            break;
          case te_token::CLOSE_CURLY_BRACKET:
            OUT_STR_("'}'");
            break;
          case te_token::INT_LITERAL:
            OUT_STR_("<int-literal>");
            break;
          case te_token::FLOAT_LITERAL:
            OUT_STR_("<float-literal>");
            break;
          case te_token::STR_LITERAL:
            OUT_STR_("<str-literal>");
            break;
          case te_token::UNIFORM:
            OUT_STR_("'uniform'");
            break;
          case te_token::IN:
            OUT_STR_("'in'");
            break;
          case te_token::OUT:
            OUT_STR_("'out'");
            break;
          case te_token::INOUT:
            OUT_STR_("'inout'");
            break;
          case te_token::IF:
            OUT_STR_("'if'");
            break;
          case te_token::ELSE:
            OUT_STR_("'else'");
            break;
          case te_token::SWITCH:
            OUT_STR_("'switch'");
            break;
          case te_token::CASE:
            OUT_STR_("'case'");
            break;
          case te_token::FOR:
            OUT_STR_("'for'");
            break;
          case te_token::WHILE:
            OUT_STR_("'while'");
            break;
          case te_token::DO:
            OUT_STR_("'do'");
            break;
          case te_token::BREAK:
            OUT_STR_("'break'");
            break;
          case te_token::CONTINUE:
            OUT_STR_("'continue'");
            break;
          case te_token::RETURN:
            OUT_STR_("'return'");
            break;
          case te_token::DISCARD:
            OUT_STR_("'discard'");
            break;
          case te_token::AND:
            OUT_STR_("'&'");
            break;
          case te_token::AND_AND:
            OUT_STR_("'&&'");
            break;
          case te_token::OR:
            OUT_STR_("'|'");
            break;
          case te_token::OR_OR:
            OUT_STR_("'||'");
            break;
          case te_token::EQUAL:
            OUT_STR_("'='");
            break;
          case te_token::EQUAL_EQUAL:
            OUT_STR_("'=='");
            break;
          case te_token::BANG:
            OUT_STR_("'!'");
            break;
          case te_token::BANG_EQUAL:
            OUT_STR_("'!='");
            break;
          case te_token::LESS:
            OUT_STR_("'<'");
            break;
          case te_token::LESS_EQUAL:
            OUT_STR_("'<='");
            break;
          case te_token::GREATER:
            OUT_STR_("'>'");
            break;
          case te_token::GREATER_EQUAL:
            OUT_STR_("'>='");
            break;
          case te_token::ADD:
            OUT_STR_("'+'");
            break;
          case te_token::ADD_EQUAL:
            OUT_STR_("'+='");
            break;
          case te_token::SUB:
            OUT_STR_("'-'");
            break;
          case te_token::SUB_EQUAL:
            OUT_STR_("'-='");
            break;
          case te_token::MUL:
            OUT_STR_("'*'");
            break;
          case te_token::MUL_EQUAL:
            OUT_STR_("'*='");
            break;
          case te_token::DIV:
            OUT_STR_("'/'");
            break;
          case te_token::DIV_EQUAL:
            OUT_STR_("'/='");
            break;
          case te_token::MOD:
            OUT_STR_("'%'");
            break;
          case te_token::MOD_EQUAL:
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
      default:
        out(spec, buffer, idx++, maxlen);
        break;
    }

    return idx;
#undef OUT_STR_
  }
}

void te_print_type_name(te_type type) {
  te_printf("%y", type);
}

void te_print_token(te_token tok) {
  te_printf("%k", tok);
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
  te_type type = TE_NULL;
  bool parse_error = false;
  union {
    te_int int_literal;
    te_float float_literal;
  };

  te_type return_type = TE_NULL;
  int stmt_count = 0;

  te_variable * global_vars = nullptr;
  int32_t global_var_count = 0;
  int32_t stack_size = 0;
  int32_t stack_offset = 0;
  int32_t var_count = 0;
  local_var_t vars[TE_MAX_VAR_COUNT];
  te_op * stmts[TE_MAX_STMT_COUNT];
};

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
  error = te::move(other.error);
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

void te_print_error(int line_num, const char * line_start, const char * start, const char * point, const char * const end) {
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

void te_print_error(te_parser_state & s) {
  s.parse_error = true;
#ifdef TE_DEBUG_COMPILE
  if (s.error != nullptr) {
    te_print_error(s.error->line_num, s.error->line_start, s.error->start, s.error->point, s.error->end);
  } else {
    te_print_error(s.line_num, s.line_start, s.token.name.ptr, s.token.name.ptr, s.token.name.end);
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
      te_error_record er(s);
#ifdef TE_DEBUG_COMPILE
      te_printf("internal error: stack var deallocated messed up!");
#endif
      te_print_error(s);
      return;
    }
  }
}

constexpr te_int te_expr_size(const te_expr * expr) {
  if (!expr) {
    return -1;
  }

  switch (expr->opcode) {
    case TE_OP_NONE:
      return sizeof(te_op);
    case TE_OP_ERROR:
      return sizeof(te_error_expr);
    case TE_OP_VALUE:
      return sizeof(te_value_expr) - sizeof(te_value) + reinterpret_cast<const te_value_expr *>(expr)->size;
    case TE_OP_STACK_REF:
    case TE_OP_STACK_REF_REF:
      return sizeof(te_stack_ref_expr);
    case TE_OP_DEREF:
      return sizeof(te_deref_expr);
    case TE_OP_ASSIGN:
      return sizeof(te_assign_expr);
    case TE_OP_CALL:
      return sizeof(te_call_expr) + sizeof(te_expr *) * reinterpret_cast<const te_call_expr *>(expr)->fn.param_count;
    case TE_OP_SUITE:
      return sizeof(te_suite_expr) + sizeof(te_expr *) * reinterpret_cast<const te_suite_expr *>(expr)->stmt_count;
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
  
  te_printf("internal error: invalid opcode %02x", expr->opcode);
  return -1;
}

static te_expr * new_error_expr(te_expr * source) {
  const int size = sizeof(te_error_expr);
  te_error_expr * ret = static_cast<te_error_expr *>(malloc(size));
  ret->opcode = TE_OP_ERROR;
  ret->type = TE_ERROR;
  ret->source = source;
  return ret;
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
    return new_error_expr(e);
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

static te_expr * new_call_expr(const te_fn_obj &fn, te_expr * args[], const int arg_count) {
  const int args_size = sizeof(te_expr *) * arg_count;
  const int size = sizeof(te_call_expr) + args_size;
  te_call_expr * ret = static_cast<te_call_expr *>(malloc(size));
  ret->opcode = TE_OP_CALL;
  ret->type = fn.return_type;
  ret->arg_stack_size = 0;
  for (int i = 0; i < arg_count; ++i) {
    ret->arg_stack_size += te_size_of(fn.param_types[i]);
  }
#ifdef TE_DEBUG_COMPILE
  if (fn.param_count != arg_count) {
    te_printf("internal error: expected %d args, got %d!\n", fn.param_count, arg_count);
    return new_error_expr(ret);
  }

  if (arg_count > TE_PARAM_COUNT_MAX) {
    te_printf("error: %d is too many parameters! max parameters: %d\n", arg_count, TE_PARAM_COUNT_MAX);
    return new_error_expr(ret);
  }

  for (int i = 0; i < arg_count; ++i) {
    if (args[i] == nullptr) {
      te_printf("internal error: arg %d is null!\n", i);
      return new_error_expr(ret);
    }
  }
#endif
  ret->fn = fn;

  for (int i = 0; i < arg_count; ++i) {
    ret->args[i] = new_deref_expr(fn.param_types[i], args[i]);
#ifdef TE_DEBUG_COMPILE
    if (ret->args[i] == nullptr) {
      te_printf("internal error: arg %d does not match parameter! expected ", i);
      te_print_type_name(fn.param_types[i]);
      te_printf(", got ");
      te_print_type_name(args[i]->type);
      te_printf("\n");
      return new_error_expr(ret);
    }
#endif
  }

  return ret;
}

static te_expr * new_call_expr(te_function fnptr, void * context, bool pure, te_type return_type, const te_type param_types[], te_expr * args[], const int arg_count) {
  te_fn_obj fn;
  fn.ptr = fnptr;
  fn.context = context;
  fn.pure = pure;
  fn.param_count = arg_count;
  fn.return_type = return_type;
  for (int i = 0; i < arg_count; ++i) {
    fn.param_types[i] = param_types[i];
  }
  return new_call_expr(fn, args, arg_count);
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
      te_free(reinterpret_cast<te_error_expr *>(n)->source);
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
  if (!op) {
    return;
  }

  switch (op->opcode) {
    case TE_OP_SUITE: {
      // A suite is the only node whos size can't be know until after its args are allocated.
      // Copy stmts to stack first to avoid fragmenting heap.
      te_suite_expr *se = reinterpret_cast<te_suite_expr *>(op);
      int32_t stmt_count = se->stmt_count;
      te_expr **stmts = static_cast<te_expr **>(alloca(sizeof(te_expr *) * stmt_count));
      for (int i = 0; i < stmt_count; ++i) {
        stmts[i] = se->stmts[i];
      }
      free(op);
      for (int i = stmt_count - 1; i >= 0; --i) {
        te_free(stmts[i]);
      }
    } break;
    default: {
      te_free_args(op);
      free(op);
    } break;
  }
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

static void next_token(te_parser_state & s) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered next_token\n");
#endif

  s.prev_token = s.token;

  // TODO: include directive
  bool new_line = s.token.name.end == s.program;
  do {
    s.type = TE_NULL;
    s.token.type = te_token::NUL;
    s.token.name.ptr = s.token.name.end;

    if (s.token.name.ptr[0] == '\0') {
      s.token.type = te_token::END;
      break;
    }

    /* Try reading a number. */
    if (isdigit(s.token.name.ptr[0]) || (s.token.name.ptr[0] == '.' && isdigit(s.token.name.ptr[1]))) {
      char * parsed_int_end;
      te_int parsed_int = strtol(s.token.name.ptr, &parsed_int_end, 0);
      char * parsed_float_end;
      te_float parsed_float = strtof(s.token.name.ptr, &parsed_float_end);
      if (parsed_float_end > parsed_int_end) {
        // strtof parsed more, use that
        s.type = TE_FLOAT;
        s.token.type = te_token::FLOAT_LITERAL;
        s.token.name.end = parsed_float_end;
        s.float_literal = parsed_float;
        break;
      } else if (parsed_int_end > s.token.name.end) {
        // strtol parsed something..
        s.type = TE_INT;
        s.token.type = te_token::INT_LITERAL;
        s.token.name.end = parsed_int_end;
        s.int_literal = parsed_int;
        break;
      }
    }

    if (s.token.name.ptr[0] == '"') {
      const char * end_quote = strchr(s.token.name.ptr + 1, '"');
      while (end_quote[-1] != '\\') end_quote = strchr(end_quote + 1, '"');
      s.token.name.end = end_quote + 1;

      s.type = TE_STR;
      s.token.type = te_token::STR_LITERAL;
      // Parse later ...
    }

    if (isalpha(s.token.name.ptr[0]) || s.token.name.ptr[0] == '_') {
      while (isalpha(*s.token.name.end) || isdigit(*s.token.name.end) || *s.token.name.end == '_') s.token.name.end++;
      const int id_len = s.token.name.len();

#define MATCHES_TOKEN_(str) (id_len == (sizeof(str) - 1) && strncmp(str, s.token.name.ptr, id_len) == 0)
      switch (*s.token.name.ptr) {
        case 'b': {
          if (MATCHES_TOKEN_("break")) {
            s.token.type = te_token::BREAK;
          }
        }
          break;
        case 'c': {
          if (MATCHES_TOKEN_("case")) {
            s.token.type = te_token::CASE;
          } else if (MATCHES_TOKEN_("continue")) {
            s.token.type = te_token::CONTINUE;
          }
        }
          break;
        case 'd': {
          if (MATCHES_TOKEN_("discard")) {
            s.token.type = te_token::DISCARD;
          } else if (MATCHES_TOKEN_("do")) {
            s.token.type = te_token::DO;
          }
        }
          break;
        case 'e': {
          if (MATCHES_TOKEN_("else")) {
            s.token.type = te_token::ELSE;
          }
        }
          break;
        case 'f': {
          if (MATCHES_TOKEN_("float")) {
            s.type = TE_FLOAT;
            s.token.type = te_token::TYPENAME;
          } else if (MATCHES_TOKEN_("for")) {
            s.token.type = te_token::FOR;
          }
        }
          break;
        case 'i': {
          if (MATCHES_TOKEN_("if")) {
            s.token.type = te_token::IF;
          } else if (MATCHES_TOKEN_("in")) {
            s.token.type = te_token::IN;
          } else if (MATCHES_TOKEN_("inout")) {
            s.token.type = te_token::INOUT;
          } else if (MATCHES_TOKEN_("int")) {
            s.type = TE_INT;
            s.token.type = te_token::TYPENAME;
          }
        }
          break;
        case 'm': {
          if (MATCHES_TOKEN_("mat2")) {
            s.type = TE_MAT2;
            s.token.type = te_token::TYPENAME;
          } else if (MATCHES_TOKEN_("mat3")) {
            s.type = TE_MAT3;
            s.token.type = te_token::TYPENAME;
          } else if (MATCHES_TOKEN_("mat4")) {
            s.type = TE_MAT4;
            s.token.type = te_token::TYPENAME;
          }
        }
          break;
        case 'o': {
          if (MATCHES_TOKEN_("out")) {
            s.token.type = te_token::OUT;
          }
        }
          break;
        case 'r': {
          if (MATCHES_TOKEN_("return")) {
            s.token.type = te_token::RETURN;
          }
        }
          break;
        case 's': {
          if (MATCHES_TOKEN_("switch")) {
            s.token.type = te_token::SWITCH;
          } else if (MATCHES_TOKEN_("str")) {
            s.type = TE_STR;
            s.token.type = te_token::TYPENAME;
          }
        }
          break;
        case 'u': {
          if (MATCHES_TOKEN_("uniform")) {
            s.token.type = te_token::UNIFORM;
          }
        }
          break;
        case 'v': {
          if (MATCHES_TOKEN_("vec2")) {
            s.type = TE_VEC2;
            s.token.type = te_token::TYPENAME;
          } else if (MATCHES_TOKEN_("vec3")) {
            s.type = TE_VEC3;
            s.token.type = te_token::TYPENAME;
          } else if (MATCHES_TOKEN_("vec4")) {
            s.type = TE_VEC4;
            s.token.type = te_token::TYPENAME;
          }
        }
          break;
        case 'w': {
          if (MATCHES_TOKEN_("while")) {
            s.token.type = te_token::WHILE;
          }
        }
          break;
      }
#undef MATCHES_TOKEN_

      if (s.token.type == te_token::NUL) {
        s.token.type = te_token::IDENTIFIER;
      }

      break;
    }

    /* Look for an operator or special character. */
    switch (s.token.name.end++[0]) {
      case '&': {
        if (*s.token.name.end == '&') {
          s.token.name.end++;
          s.token.type = te_token::AND_AND;
        } else {
          s.token.type = te_token::AND;
        }
      }
        break;
      case '|': {
        if (*s.token.name.end == '|') {
          s.token.name.end++;
          s.token.type = te_token::OR_OR;
        } else {
          s.token.type = te_token::OR;
        }
      }
        break;
      case '=': {
        if (*s.token.name.end == '=') {
          s.token.name.end++;
          s.token.type = te_token::EQUAL_EQUAL;
        } else {
          s.token.type = te_token::EQUAL;
        }
      }
        break;
      case '!': {
        if (*s.token.name.end == '=') {
          s.token.name.end++;
          s.token.type = te_token::BANG_EQUAL;
        } else {
          s.token.type = te_token::BANG;
        }
      }
        break;
      case '<': {
        if (*s.token.name.end == '=') {
          s.token.name.end++;
          s.token.type = te_token::LESS_EQUAL;
        } else {
          s.token.type = te_token::LESS;
        }
      }
        break;
      case '>': {
        if (*s.token.name.end == '=') {
          s.token.name.end++;
          s.token.type = te_token::GREATER_EQUAL;
        } else {
          s.token.type = te_token::GREATER;
        }
      }
        break;
      case '+': {
        if (*s.token.name.end == '+') {
          s.token.name.end++;
          s.token.type = te_token::INC;
        } else if (*s.token.name.end == '=') {
          s.token.name.end++;
          s.token.type = te_token::ADD_EQUAL;
        } else {
          s.token.type = te_token::ADD;
        }
      }
        break;
      case '-': {
        if (*s.token.name.end == '-') {
          s.token.name.end++;
          s.token.type = te_token::DEC;
        } else if (*s.token.name.end == '=') {
          s.token.name.end++;
          s.token.type = te_token::SUB_EQUAL;
        } else {
          s.token.type = te_token::SUB;
        }
      }
        break;
      case '*': {
        if (*s.token.name.end == '=') {
          s.token.name.end++;
          s.token.type = te_token::MUL_EQUAL;
        } else {
          s.token.type = te_token::MUL;
        }
      }
        break;
      case '/': {
        if (*s.token.name.end == '=') {
          s.token.name.end++;
          s.token.type = te_token::DIV_EQUAL;
        } else {
          s.token.type = te_token::DIV;
        }
      }
        break;
      case '%': {
        if (*s.token.name.end == '=') {
          s.token.name.end++;
          s.token.type = te_token::MOD_EQUAL;
        } else {
          s.token.type = te_token::MOD;
        }
      }
        break;
      case '(':
        s.token.type = te_token::OPEN_PAREN;
        break;
      case ')':
        s.token.type = te_token::CLOSE_PAREN;
        break;
      case '[':
        s.token.type = te_token::OPEN_SQUARE_BRACKET;
        break;
      case ']':
        s.token.type = te_token::CLOSE_SQUARE_BRACKET;
        break;
      case '{':
        s.token.type = te_token::OPEN_CURLY_BRACKET;
        break;
      case '}':
        s.token.type = te_token::CLOSE_CURLY_BRACKET;
        break;
      case ',':
        s.token.type = te_token::SEP;
        break;
      case '.':
        s.token.type = te_token::DOT;
        break;
      case ';':
        s.token.type = te_token::SEMICOLON;
        break;
      case '\r':
        if (s.token.name.end[0] == '\n') s.token.name.end++; // fallthrough
      case '\n':
        new_line = true;
        s.line_num++;
        s.line_start = s.token.name.end;
        // fallthrough
      case ' ':
      case '\t':
        break;
      default: {
        te_error_record er(s);
#ifdef TE_DEBUG_COMPILE
        te_printf("error: unrecognised token!\n");
#endif
        te_print_error(s);
      }
        break;
    }
  } while (s.token.type == te_token::NUL);
#ifdef TE_DEBUG_PEDANTIC
  te_printf("parsed token '%.*s'\n", s.token.name.len(), s.token.name.ptr);
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

#define TE_OP_FUNCTION(FNAME, TRET, TA, TB) te::detail::make_function_raw<FNAME<te::type_of<(TRET)>, te::type_of<(TA)>, te::type_of<(TB)>>, true, (TA), (TA), (TB)>::call()

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
  te_error_record er(s);
  te_expr * ret = nullptr;
  switch (s.token.type) {
    case te_token::INT_LITERAL: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("base is int literal\n");
#endif
      ret = new_int_literal_expr(s.int_literal);
      next_token(s);
    } break;
    case te_token::FLOAT_LITERAL: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("base is float literal\n");
#endif
      ret = new_float_literal_expr(s.float_literal);
      next_token(s);
    } break;
    case te_token::STR_LITERAL: {
#ifdef TE_DEBUG_PEDANTIC
      te_printf("base is str literal\n");
#endif
      ret = new_str_literal_expr(s.token.name);
      next_token(s);
    } break;
    case te_token::TYPENAME:
    case te_token::IDENTIFIER: {
      bool is_typename = s.token == te_token::TYPENAME;
      te_type type = s.type;
      te_strview id = s.token.name;

      next_token(s);
      if (s.token == te_token::OPEN_PAREN) {
#ifdef TE_DEBUG_PEDANTIC
        te_printf("base is function call\n");
#endif
        te_expr * args[TE_PARAM_COUNT_MAX]{nullptr};

        next_token(s);
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

          if (s.token == te_token::SEP) {
            next_token(s);
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

          next_token(s);
        } else {
          te_error_record er2(s);
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
      next_token(s);
      ret = expr(s);

      if (s.token == te_token::CLOSE_PAREN) {
        next_token(s);
      } else if (ret != nullptr) {
        te_error_record er2(s);
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
      next_token(s);
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
  te_error_record er(s);
  te_expr * ret = base(s);

  if (s.token == te_token::OPEN_SQUARE_BRACKET) {
    te_type ret_type = ret ? ret->type : TE_ERROR;

    next_token(s);

    te_expr * idx_expr = nullptr;

    if (s.token == te_token::CLOSE_SQUARE_BRACKET) {
      te_error_record er2(s);
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
        te_error_record er2(s);
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
      te_error_record er2(s);
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

    next_token(s);

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
    te_expr * args[] = {ret, idx_expr};
    ret = new_call_expr(index_fn, args, 2);
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
  te_error_record er(s);
  te_expr * ret = index(s);
  te_type ret_type = ret ? ret->type : TE_ERROR;

  if (s.token == te_token::DOT) {
    next_token(s);
    if (s.token != te_token::IDENTIFIER) {
      te_error_record er2(s);
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
        te_error_record er2(s);
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
            te_error_record er2(s);
#ifdef TE_DEBUG_COMPILE
            er.start = c;
            er.end = c + 1;
            te_printf("error: unknown swizzler '%c'!\n", *c);
#endif
            te_print_error(s);
            next_token(s);
            te_free(ret);
            ret = nullptr;
            return nullptr;
          }
        }
      }
    }

    if (i < 1 || i > swizzle_max) {
      te_error_record er2(s);
#ifdef TE_DEBUG_COMPILE
      te_printf("error: too many swizzlers! type: ");
      te_print_type_name(base_type);
      te_printf("\n");
#endif
      te_print_error(s);
      next_token(s);
      te_free(ret);
      ret = nullptr;
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
      te_error_record er2(s);
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

    next_token(s);
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
  te_error_record er(s);

  bool bool_not_flag = false;
  bool pos = true;
  bool pre_incdec = false;
  if (s.token == te_token::ADD) {
    pos = true;
    next_token(s);
  } else if (s.token == te_token::SUB) {
    pos = false;
    next_token(s);
  } else if (s.token == te_token::INC) {
    pos = true;
    pre_incdec = true;
    next_token(s);
  } else if (s.token == te_token::DEC) {
    pos = false;
    pre_incdec = true;
    next_token(s);
  } else if (s.token == te_token::BANG) {
    bool_not_flag = true;
    next_token(s);
  }

  te_expr * ret = attr(s);
  te_type ret_type = ret ? ret->type : TE_ERROR;

  er.set_end(s.token.name.end);

  if (bool_not_flag) {
    if (ret_type == TE_INT) {
      te_type param_type = TE_INT;
      ret = new_call_expr(&bool_not, nullptr, true, TE_INT, &param_type, &ret, 1);
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

    te_type param_type = TE_INT_REF;
    if (pos) {
      ret = new_call_expr(&pre_increment, nullptr, true, TE_INT, &param_type, &ret, 1);
    } else {
      ret = new_call_expr(&pre_decrement, nullptr, true, TE_INT, &param_type, &ret, 1);
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

      ret = new_call_expr(tmpfn, &ret, 1);
    }
  }

  ret_type = ret ? ret->type : TE_ERROR;

  int postfix = 0;
  if (s.token == te_token::INC) {
    postfix = 1;
    next_token(s);
  } else if (s.token == te_token::DEC) {
    postfix = -1;
    next_token(s);
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
      ret = new_call_expr(&post_increment, nullptr, true, TE_INT, &param_type, &ret, 1);
    } else {
      ret = new_call_expr(&post_decrement, nullptr, true, TE_INT, &param_type, &ret, 1);
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
  te_error_record er(s);
  te_expr * ret = factor(s);
  te_type ret_type = ret ? ret->type : TE_ERROR;

  do {
    er.set_point(s.token.name.ptr);

    te_token::type_t tt = te_token::NUL;
    char c = '?';
    if (s.token == te_token::MUL || s.token == te_token::DIV || s.token == te_token::MOD) {
      tt = s.token.type;
      c = *s.token.name.ptr;
    } else {
      break;
    }
    next_token(s);
    te_expr * rhs = factor(s);
    te_type rhs_type = rhs ? rhs->type : TE_ERROR;

    er.set_end(s.prev_token.name.end);

    te_fn_obj fn;
    switch (tt) {
      case te_token::MUL:
        fn = te_get_mul_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      case te_token::DIV:
        fn = te_get_div_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      case te_token::MOD:
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
      ret = new_call_expr(fn, args, 2);
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
  te_error_record er(s);
  te_expr * ret = term(s);
  te_type ret_type = ret ? ret->type: TE_ERROR;

  do {
    er.set_point(s.token.name.ptr);

    te_token::type_t tt = te_token::NUL;
    char c = '?';
    if (s.token == te_token::ADD || s.token == te_token::SUB) {
      tt = s.token.type;
      c = *s.token.name.ptr;
    } else {
      break;
    }
    next_token(s);
    te_expr * rhs = term(s);
    te_type rhs_type = rhs ? rhs->type : TE_ERROR;

    er.set_end(s.prev_token.name.end);

    te_fn_obj fn;
    switch (tt) {
      case te_token::ADD:
        fn = te_get_add_func(te_type(ret_type | TE_CONSTANT), te_type(rhs_type | TE_CONSTANT));
        break;
      case te_token::SUB:
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
      te_expr * args[] = {ret, rhs};
      ret = new_call_expr(fn, args, 2);
    }

    ret_type = ret ? ret->type : TE_ERROR;
  } while (true);

#ifdef TE_DEBUG_PEDANTIC
  te_printf("exited expr\n");
#endif

  return ret;
}

static te_expr * parse_fn(te_parser_state & s) {
  /* <fn>        =    <typename> <identifier> "(" {<typename> {<identifier>} "," }+ ")" <stmt> */
  return nullptr;
}

static void begin_function(te_parser_state & s) {
  s.parse_error = false;
  s.stmt_count = 0;
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
  }
#endif

  if (s.parse_error) { // :3 was here
    for (int i = 0; i < s.stmt_count; ++i) {
      te_free(s.stmts[i]);
      s.stmts[i] = nullptr;
    }
  } else {
    ret = new_suite_expr(s.return_type, s.stack_size, s.stmts, s.stmt_count);
  }

  return ret;
}

static void push_stmt(te_parser_state & s, te_op * e) {
  if (s.stmt_count < TE_MAX_STMT_COUNT) {
    s.stmts[s.stmt_count++] = e;
  } else {
    te_error_record er(s);
#ifdef TE_DEBUG_COMPILE
    te_printf("error: ran out of room for stmts! max=%d\n", TE_MAX_STMT_COUNT);
#endif
    te_print_error(s);
  }
}

static void parse_stmt(te_parser_state & s);

static void parse_suite(te_parser_state & s, te_token::type_t end_token) {
#ifdef TE_DEBUG_PEDANTIC
  te_printf("entered suite\n");
#endif

  te_int prev_stack_size = te_begin_scope(s);

  te_expr * ret = nullptr;
  while (true) {
    if (s.token == end_token) {
      next_token(s);
      break;
    } else if (s.token == te_token::END) {
      te_error_record er2(s);
#ifdef TE_DEBUG_COMPILE
      te_printf("error: end of input!\n");
#endif
      te_print_error(s);
      break;
    }

    parse_stmt(s);
  }

  te_end_scope(s, prev_stack_size);

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
  next_token(s);
  if (s.token != te_token::OPEN_PAREN) {
    te_error_record er2(s);
#ifdef TE_DEBUG_COMPILE
    te_printf("error: expected '(' got ");
    te_print_token(s.token);
    te_printf("!\n");
#endif
    te_print_error(s);
  }
  next_token(s);

  te_error_record er(s);
  te_expr * cond_expr = expr(s);
  te_type cond_expr_result_type = cond_expr ? cond_expr->type : TE_ERROR;

  er.set_end(s.token.name.end);

  if (s.token != te_token::CLOSE_PAREN) {
    te_error_record er2(s);
#ifdef TE_DEBUG_COMPILE
    te_printf("error: expected ')' got ");
    te_print_token(s.token);
    te_printf("!\n");
#endif
    te_print_error(s);
  }

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

  next_token(s);
  parse_stmt(s);

  if (s.token == te_token::ELSE) {
    next_token(s);
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
  te_type decl_type = s.type;
  next_token(s);

  if (s.token != te_token::IDENTIFIER) {
    te_error_record er2(s);
#ifdef TE_DEBUG_COMPILE
    te_printf("error: expected <identifier> got ");
    te_print_token(s.token);
    te_printf("!\n");
#endif
    te_print_error(s);
    return;
  }
  te_parser_state::local_var_t * var = te_stack_allocate_var(s, s.token.name, decl_type);
  next_token(s);

  if (allow_assign && s.token == te_token::EQUAL) {
    next_token(s);

    te_expr *initializer = expr(s);
    push_stmt(s, new_assign_expr(var ? new_stack_ref_expr(var->type, var->offset) : nullptr, initializer));
  }
}

static void parse_return_stmt(te_parser_state & s) {
  next_token(s);

  te_error_record er(s);

  if (s.token == te_token::SEMICOLON) {
    if (s.return_type != TE_NULL) {
#ifdef TE_DEBUG_COMPILE
      te_printf("error: expected ");
      te_print_type_name(s.return_type);
      te_printf("!\n");
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
    next_token(s);
    parse_suite(s, te_token::CLOSE_CURLY_BRACKET);
  } else if (s.token == te_token::IF) {
    parse_if_stmt(s);
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
        next_token(s);
      }
    }

    if (s.token == te_token::SEMICOLON) {
      next_token(s);
    } else {
      te_error_record er(s);
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
    if (cond) {\
        te_printf(__VA_ARGS__);\
        te_printf("\n");\
        fail_action;\
    } else ((void)0)

#define TE_CHECK_EXPECTED_TYPE(chk_type, expected, fail_action, ...)\
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
        if (value_expr_size <= te_expr_size(call_expr)) {
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
  s.token = {te_token::NUL, te_strview{expression, 0}};
  s.prev_token = s.token;
  s.global_vars = variables;
  s.global_var_count = var_count;

  next_token(s);
  te_expr * root = ParseFn(s);

  if (s.token != te_token::END) {
    if (root && root->type != TE_ERROR) {
      te_error_record er(s);
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

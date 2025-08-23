#pragma once

#include "tesl_array.hpp"

namespace tesl {
  struct Compiler {
    Array<uint8_t> _bytecode;
  };
  /*struct Compiler {
    Parser & parser;
    Compiler * parent = nullptr;
    Token next_token;
    Token curr_token;
    Token prev_token;
    bool has_error = false;

    te_type return_type = TYPE_NULL_VAL;
    IntT stmt_count = 0;

    Variable * global_vars = nullptr;
    IntT global_var_count = 0;
    IntT stack_size = 0;
    IntT stack_offset = 0;
    IntT var_count = 0;
    local_var_t vars[TE_MAX_VAR_COUNT];
    te_op * stmts[TE_MAX_STMT_COUNT];

    struct local_var_t {
      const char * name_ptr = nullptr;
      uint8_t name_length = 0;
      te_type type = TYPE_ERROR;
      uint16_t offset = 0;

      StrView get_name() const { return {name_ptr, static_cast<int>(name_length)}; }

      local_var_t(te_parser_state & s, StrView name, te_type p_type, uint16_t p_offset);
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
        Variable * global_var;
        const Variable * global_const;
      };

      bool is_valid() {
        return kind != INVALID && _ptr;
      }

      bool is_function() {
        return get_type() == TYPE_FUNCTION;
      }
      
      StrView get_name() const {
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
            return TYPE_ERROR;
          case LOCAL_VAR:
            return local_var->type;
          case GLOBAL_CONST:
            return global_const->type;
          case GLOBAL_VAR:
            return global_var->type;
        }

        return TYPE_ERROR;
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
      FnObj get_function() const {
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

      var_ref(const Variable & p_global_const) : kind(GLOBAL_CONST), global_const(&p_global_const) {}
      var_ref(Variable & p_global_var) : kind(GLOBAL_VAR), global_var(&p_global_var) {}
      var_ref(local_var_t & p_local_var) : kind(LOCAL_VAR), local_var(&p_local_var) {}
      var_ref() : kind(INVALID) {}
    };

    void print_error_location() const;

    template<typename ... Ts>
    static void error(Ts && ... vs) {
#ifdef TESL_DEBUG_COMPILE
      has_error = true;
      (te_print(vs), ...);
      print("\n");
      print_error_location();
#endif
    }
  };

  struct Variable {
    StrView name = "<unnamed>";
    Type type = TYPE_ERROR;
    union {
      te_value value;
      FnObj fn;
    };

    StrView get_name() const { return name; }

    TESL_ALWAYS_INLINE constexpr Variable(StrView p_name, TypedValueT v) : name(p_name), type(v.type), value(v) {}
    TESL_ALWAYS_INLINE constexpr Variable(StrView p_name, Type t, te_value v) : name(p_name), type(t), value(v) {}
    TESL_ALWAYS_INLINE constexpr Variable(StrView p_name, FnObj p_func) : name(p_name), type(TYPE_FUNCTION), fn(p_func) {}
  };
*/
}

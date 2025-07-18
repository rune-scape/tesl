#pragma once

#include "tesl_tokenizer.hpp"
#include "tesl_compiler.hpp"

namespace tesl {
  struct Parser;

  // meant to be allocated on the stack at the moment of parsing a potential error
  struct ErrorRecord {
  private:
    Parser * parser = nullptr;
    ErrorRecord * prev = nullptr;
  
  public:
    const char * line_start = nullptr;
    const char * start = nullptr;
    const char * point = nullptr;
    const char * end = nullptr;
    IntT line_num = 0;

    inline IntT get_line() { return line_num; }
    inline IntT get_column() { return point - line_start; }
    void reset();
    inline void set_start(const char * p_start) { start = p_start; }
    inline void set_point(const char * p_point) { point = p_point; }
    inline void set_end(const char * p_end) { end = p_end; }
    void print() const;

    ErrorRecord &operator=(const ErrorRecord &) = delete;
    ErrorRecord &operator=(ErrorRecord &&) = delete;
    ErrorRecord(const ErrorRecord &) = delete;
    ErrorRecord(ErrorRecord &&) = delete;

    explicit ErrorRecord(Parser & parser);
    ErrorRecord() = default;
    ~ErrorRecord();
  };

  struct Parser {
    Tokenizer tokenizer;
    Compiler compiler;

    ErrorRecord *this_error = nullptr;
    ErrorRecord prev_error;
    bool has_error = false;

    struct ParseSequence;
    struct ParseResult;

    ParseResult parse_literal_expr(ParseSequence sequence);
    ParseResult parse_identifier_expr(ParseSequence sequence);
    ParseResult parse_grouping_expr(ParseSequence sequence);
    ParseResult parse_subscript_expr(ParseSequence sequence);
    ParseResult parse_call_expr(ParseSequence sequence);
    ParseResult parse_construct_expr(ParseSequence sequence);
    ParseResult parse_dot_expr(ParseSequence sequence);
    ParseResult parse_postfix_expr(ParseSequence sequence);
    ParseResult parse_prefix_expr(ParseSequence sequence);
    ParseResult parse_arithmetic_expr(ParseSequence sequence);
    ParseResult parse_comparison_expr(ParseSequence sequence);
    ParseResult parse_bitwise_op_expr(ParseSequence sequence);
    ParseResult parse_boolean_op_expr(ParseSequence sequence);
    ParseResult parse_ternary_expr(ParseSequence sequence);
    ParseResult parse_assignment_expr(ParseSequence sequence);
    ParseResult parse_sequence_expr(ParseSequence sequence);

    void print_error_line_info() const;

    template<typename ... Ts>
    void error(Ts && ... vs) {
      has_error = true;
      (print(vs), ...);
      print("\n");
      print_error_line_info();
    }
  };
}

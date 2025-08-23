#pragma once

#include "tesl_tokenizer.hpp"
#include "tesl_compiler.hpp"

namespace tesl {
  namespace rules {
    struct PatternElement;
    struct PatternRefStorage;
    struct RuleRefStorage;
    struct RuleRefListStorage;

    struct ElementRef;
    struct PatternRef;
    struct RuleRef;
    struct RuleRefList;

    struct RuleLibrary {
      StrView _label;
      const RuleRefListStorage * _lists;
      IntT _size;
  
      StrView get_label() const { return _label; }
      IntT size() const { return _size; }
      IntT max_precedence() const { return _size - 1; }
  
      RuleRefList operator[](IntT p) const;
      RuleRef find_rule_precedence_first(Token::Kind token, IntT precedence) const;
      RuleRef find_rule_precedence_second(const RuleRef & r, Token::Kind token, IntT precedence) const;
  
      RuleLibrary & operator=(const RuleLibrary &) = delete;
      RuleLibrary & operator=(RuleLibrary &&) = delete;
      RuleLibrary(const RuleLibrary &) = delete;
      RuleLibrary(RuleLibrary &&) = delete;

      template<typename T>
      constexpr RuleLibrary(const T & library) : _label(library.label), _lists(library.lists), _size(library.size) {}
    };
  }

  struct Parser {
    Tokenizer tokenizer;
    Token current_token;
    Compiler compiler;
    static const rules::RuleLibrary expression_library;

    bool has_error = false;

    struct ParseSequence;
    struct ParseResult;
    using ParseFn = Parser::ParseResult (Parser::*)(Parser::ParseSequence sequence);

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

    void parse_program();
    ParseResult parse_precedence(const rules::RuleLibrary & library, IntT precedence);
    ParseResult parse_precedence_impl(rules::RuleRef rule, ParseResult initial);

    void print_error_source(const Token & t) const;
    void print_error_source() const;

    Parser(Tokenizer pTokenizer, Compiler pCompiler);
  };
}

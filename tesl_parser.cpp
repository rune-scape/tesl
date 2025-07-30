#include "tesl_parser.hpp"

namespace tesl {

#ifdef TESL_DEBUG_PARSER

#define PARSE_DEBUG(s, sequence)\
  do {\
    tesl_printf("%s:\n", __func__);\
    for (int i = 0; i < sequence.size(); ++i) {\
      switch (sequence[i].kind) {\
        case Parser::ParseResult::NONE:\
          tesl_printf("  <none>");\
          break;\
        case Parser::ParseResult::TOKEN:\
          tesl_printf("  token: ");\
          print(sequence[i].token);\
          tesl_printf("\n");\
          break;\
        case Parser::ParseResult::VALUE:\
          tesl_printf("  value: ");\
          print(sequence[i].value);\
          tesl_printf("\n");\
          break;\
        case Parser::ParseResult::EXPR:\
          tesl_printf("  expr: ");\
          print(sequence[i].expr.type);\
          tesl_printf("\n");\
          break;\
      }\
    }\
  } while (false)

#else

#define PARSE_DEBUG(s, sequence)

#endif

  /*struct ParseResult {
    enum kind_t : int8_t {
      TOKEN,
      EXPR,
      OP,
    } kind;

    union {
      Token token{};
      struct {
        union {
          te_op * op;
          te_expr * expr;
        };
        uint8_t precedence;
      };
    };

    node(const Token & t) : kind(TOKEN), token(t) {}
    node(te_expr * e, uint8_t p) : kind(EXPR), expr(e), precedence(p) {}
    node(te_op * o, uint8_t p) : kind(te_is_expr(o) ? EXPR : OP), op(o), precedence(p) {}
    node() = default;
  };*/

  struct ExprResult {
    const TypeInfo * type = &typeInfoNull;
  };
  /*struct ExprResult {
    const TypeInfo * type;
    Array<uint8_t, 8> _bytecode;

    bool is_empty() { return _bytecode.is_empty(); }
    void emit(Compiler & compiler) {
      if (!is_empty()) {
        compiler._bytecode += _bytecode;
        _bytecode.clear();
      }
    }
    void discard() {
      _bytecode.clear();
    }

    ExprResult & operator=(const ExprResult & other) = default;
    ExprResult & operator=(ExprResult && other) { memswap(*this, other); return *this; }
    ExprResult(const ExprResult & other) { this->operator=(other); }
    ExprResult(ExprResult && other) { this->operator=(MOV(other)); }

    ~ExprResult() {
      assert(is_empty());
    }
  };*/

  struct Parser::ParseResult {
    enum Kind {
      NONE,
      TOKEN,
      VALUE,
      EXPR,
    };

    Kind kind = NONE;

    union {
      Variant value;
      Token::Kind token;
      ExprResult expr;
    };

    ParseResult(Token::Kind k) : kind(TOKEN), token(k) {}
    ParseResult(Variant v) : kind(VALUE), value(v) {}
    ParseResult(ExprResult e) : kind(EXPR), expr(e) {}

    ParseResult() = default;
    ParseResult(ParseResult &&) = default;
    ParseResult(const ParseResult &) = default;
    ~ParseResult() {
      switch (kind) {
        case NONE:
        case TOKEN:
          break;
        case VALUE:
          value.~Variant();
          break;
        case EXPR:
          expr.~ExprResult();
          break;
      }
    }
  };

  struct Parser::ParseSequence : public Array<Parser::ParseResult, 3> {};

  Parser::ParseResult Parser::parse_literal_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);

    assert(sequence.size() == 1);
    assert(sequence[0].kind == ParseResult::TOKEN);
    assert(sequence[0].token.kind == Token::LITERAL);

    ErrorRecord er(s);

    switch (sequence[0].token.type) {
      case TYPE_INT_VAL:
        return new_int_literal_expr(s.curr_token.int_value);
        break;
      case TYPE_FLOAT_VAL:
        return new_float_literal_expr(s.curr_token.float_value);
        break;
      case TYPE_STR_VAL:
        return new_str_literal_expr(s.curr_token.name);
        break;
      default:
        te_compile_error(s, "internal error: unknown literal type: ", s.curr_token.type);
        break;
    }

    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_identifier_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return ExprResult();
  }

  Parser::ParseResult Parser::parse_grouping_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_subscript_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_call_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_construct_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_dot_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_postfix_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_prefix_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_arithmetic_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_comparison_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_bitwise_op_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_boolean_op_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_ternary_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_assignment_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);
    // TODO: finish
    return nullptr;
  }

  Parser::ParseResult Parser::parse_sequence_expr(ParseSequence sequence) {
    PARSE_DEBUG(s, sequence);

    assert(sequence.size() >= 3);
    assert(sequence.size() % 2 == 1);

    int sequence_size = (sequence.size() + 1) / 2;

    uint16_t offset = 0;
    uint16_t size = 0;
    te_expr * args[sequence_size];
    Type param_types[sequence_size];
    for (int i = 0; i < sequence.size(); ++i) {
      if (i % 2 == 0) {
        assert(sequence[i].kind == ParseResult::EXPR);
        assert(sequence[i].expr != nullptr);
        args[i / 2] = sequence[i].expr;
        param_types[i] = args[i]->type;
        if (i != (sequence.size() - 1)) {
          offset += sizeof_type(param_types[i]);
        } else {
          size = sizeof_type(param_types[i]);
        }
      } else {
        assert(sequence[i].kind == ParseResult::TOKEN);
        assert(sequence[i].token.kind == Token::COMMA);
      }
    }

    static_assert(sizeof(uint16_t) * 2 <= sizeof(void *));
    void * ctx;
    uint16_t * ctx_data = reinterpret_cast<uint16_t *>(&ctx);
    ctx_data[0] = offset;
    ctx_data[0] = size;
    return new_call_expr_v(te::detail::make_function_raw(te::sequence, ctx, true, TYPE_NULL_VAL, param_types, sequence_size), args, sequence_size);
  }

#undef PARSE_DEBUG

  namespace rules {
    using parse_fn = Parser::ParseResult * (Parser::*)(Parser::ParseSequence sequence);

    struct element_t {
      enum kind_t : int8_t {
        TOKEN,
        ABSOLUTE_PRECEDENCE_RULE,
        RELATIVE_PRECEDENCE_RULE,
        OPTIONAL,
        JUMP,
      } kind = TOKEN;
      union {
        Token::Kind token = Token::NONE;
        int8_t n;
        int8_t offset;
        int8_t size;
      };

      constexpr element_t(const element_t &) = default;

      constexpr element_t(Token::Kind t) : kind(TOKEN), token(t) {};
      constexpr element_t(kind_t k, Token::Kind t) : kind(k), token(t) {};
      constexpr element_t(kind_t k, int8_t v) : kind(k), n(v) {};

      constexpr element_t() {};
    };

    struct pattern_ref_storage_t {
      const element_t * elements = nullptr;
      int16_t size = 0;
      bool is_valid() const { return size == 0 || (size > 0 && elements != nullptr); }
      bool operator==(const pattern_ref_storage_t &other) const { return elements == other.elements && size == other.size; }
    };

    struct rule_ref_storage_t {
      parse_fn parse = nullptr;
      pattern_ref_storage_t pattern;
      bool is_valid() const { return parse != nullptr && pattern.is_valid(); }
      bool operator==(const rule_ref_storage_t &other) const { return parse == other.parse && pattern == other.pattern; }
    };

    struct rule_ref_list_storage_t {
      const rule_ref_storage_t * rules = nullptr;
      int16_t size = 0;
      bool is_valid() const { return rules != nullptr || size == 0; }
      bool operator==(const rule_ref_list_storage_t &other) const { return rules == other.rules && size == other.size; }
    };

    struct rule_library_info_t {
      const char * rule_label;
      int size = 0;
      int max_precedence = 0;
    };
    
    struct rule_ref_list_t;
    struct rule_ref_t;
    struct pattern_ref_t;
    struct element_ref_t;

    struct rule_ref_list_t {
      rule_ref_list_storage_t storage;
      const rule_library_info_t * library_info = nullptr;
      uint8_t precedence = -1;

      bool is_valid() const { return storage.is_valid() && library_info != nullptr; }
      int16_t size() const { return storage.size; }

      bool operator==(const rule_ref_list_t &other) const { return library_info == other.library_info && storage == other.storage && precedence == other.precedence; }
      rule_ref_t operator[](int i) const;

      constexpr rule_ref_list_t(const rule_ref_list_storage_t &rl, const rule_library_info_t & info, uint8_t p) : storage(rl), library_info(&info), precedence(p) {}
      constexpr rule_ref_list_t() = default;
    };

    struct rule_ref_t {
      rule_ref_storage_t storage;
      rule_ref_list_t list;

      bool is_valid() const { return storage.is_valid() && list.is_valid(); }
      pattern_ref_t get_pattern() const;
      Parser::ParseResult parse(Parser & p, Parser::ParseSequence seq) const {
        parse_fn parse = storage.parse;
        return p.*parse(seq);
      }

      bool operator==(const rule_ref_t & other) const { return list == other.list && storage == other.storage; }

      constexpr rule_ref_t(const rule_ref_list_t & l, const rule_ref_storage_t & r) : storage(r), list(l) {}
      constexpr rule_ref_t() = default;
    };

    struct pattern_ref_t {
      pattern_ref_storage_t storage;
      rule_ref_t rule;

      bool is_valid() const { return storage.is_valid() && rule.is_valid(); }
      int16_t size() const { return storage.size; }

      bool operator==(const pattern_ref_t & other) const { return rule == other.rule && storage == other.storage; }
      element_ref_t operator[](int i) const;

      constexpr pattern_ref_t(const rule_ref_t & r, const pattern_ref_storage_t & p) : storage(p), rule(r) {}
      constexpr pattern_ref_t() = default;
    };

    struct element_ref_t {
      pattern_ref_t pattern;
      int element_index = 0;

      bool is_valid() const { return element_index > 0 && element_index < pattern.size() && pattern.is_valid(); }
      element_ref_t follow() const;

      void _get_branch_choices_impl(Array<element_ref_t, 0> & choices) const;
      Array<element_ref_t, 0> get_branch_choices() const {
        static thread_local Array<element_ref_t, 0> choices;
        choices.clear();
        _get_branch_choices_impl(choices);
        return choices;
      }

      element_ref_t choose_branch(Token::Kind tok) const;
      void print_branch_choices() const;

      bool operator==(const element_ref_t & other) const { return pattern == other.pattern && element_index == other.element_index; }
      const element_t * operator->() const { return &pattern.storage.elements[element_index]; }
      operator const element_t &() const { return pattern.storage.elements[element_index]; }
    };

    void print(const element_ref_t & e) {
      if (!e.is_valid()) {
        tesl_printf("<invalid-element>");
        return;
      }

      const rule_ref_list_t & rule_list = e.pattern.rule.list;
      const rule_library_info_t & library_info = *rule_list.library_info;
      switch (e->kind) {
        case element_t::TOKEN:
          tesl_printf("'");
          print(e->token);
          tesl_printf("'");
          break;
        case element_t::ABSOLUTE_PRECEDENCE_RULE: {
          tesl_printf("%s", library_info.rule_label);
#ifdef TESL_DEBUG_PARSER
          tesl_printf("[p%d]", e->offset);
#endif
        } break;
        case element_t::RELATIVE_PRECEDENCE_RULE:
          tesl_printf("%s", library_info.rule_label);
#ifdef TESL_DEBUG_PARSER
          tesl_printf("[p%d]", library_info.size - (rule_list.precedence + e->offset));
#endif
          break;
        case element_t::OPTIONAL:
          tesl_printf("<opt:size=%d>", e->size);
          break;
        case element_t::JUMP:
          tesl_printf("<jmp%+d>", e->offset);
          break;
      }
    }

    rule_ref_t rule_ref_list_t::operator[](int i) const { return {*this, storage.rules[i]}; }
    pattern_ref_t rule_ref_t::get_pattern() const { return {*this, storage.pattern}; }
    element_ref_t pattern_ref_t::operator[](int i) const  { return {*this, i}; }

    element_ref_t element_ref_t::follow() const {
      int loop_count = 0;
      int i = element_index;
      const element_t * e = &pattern.storage.elements[i];
      while (e && e->kind == element_t::JUMP) {
        assert(loop_count < 100);
        assert(e->offset != 0);
        i = i + e->offset;
        e = &pattern.storage.elements[i];
        loop_count++;
      }

      return {pattern, i};
    }

    void element_ref_t::_get_branch_choices_impl(Array<element_ref_t, 0> & choices) const {
      if (choices.has(*this)) {
        return;
      }

      element_ref_t points_to = follow();
      switch (points_to->kind) {
        case element_t::TOKEN:
        case element_t::ABSOLUTE_PRECEDENCE_RULE:
        case element_t::RELATIVE_PRECEDENCE_RULE:
          choices.push_back(points_to);
          return;
        case element_t::OPTIONAL:
          element_ref_t{pattern,  points_to.element_index + 1}._get_branch_choices_impl(choices);
          element_ref_t{pattern, points_to.element_index + points_to->size + 1}._get_branch_choices_impl(choices);
          return;
        case element_t::JUMP:
          __builtin_unreachable();
      }
    }

    template<typename T>
    static void validate_branch_choices(T choices) {
      bool is_first = true;
      auto choices = get_branch_choices();
      for (const element_ref_t & e : choices) {
        if (!is_first) {
          tesl_printf(" or ");
        }
        is_first = false;
        e.print();
      }
      if (found_expr) {
        tesl_printf("internal error: invalid pattern: more than one expression branch for optional!");
      }
    }

    element_ref_t element_ref_t::choose_branch(Token::Kind tok) const {
#ifdef TESL_DEBUG_PARSER
      tesl_printf("choosing between branches: ");
      print_branch_choices();
      tesl_printf("\n");
#endif
      bool found_token;
      bool found_expr;
      auto choices = get_branch_choices();
      for (const element_ref_t & e : choices) {
        switch (e->kind) {
          case element_t::TOKEN: {
            if (e->token == tok) {
#ifdef TESL_DEBUG_PARSER
              tesl_printf("matched ");
              print(e->token);
              tesl_printf("\n");
#endif
              found_token = true;
              break;
            }
          } break;
          case element_t::ABSOLUTE_PRECEDENCE_RULE:
          case element_t::RELATIVE_PRECEDENCE_RULE: {
#ifdef TESL_DEBUG_PARSER
            tesl_printf("matched ");
            print(e->);
            found_expr.print();
            tesl_printf("\n");
#endif
            found_expr = true;
          } break;
          case element_t::OPTIONAL:
          case element_t::JUMP:
            break;
        }
      }
      
      if (found_token) {
#ifdef TESL_DEBUG_PARSER
      tesl_printf("matched ");
      print();
      found_token.print();
      tesl_printf("\n");
#endif
        return found_token;
      } else {
        return found_expr;
      }
    }

    void element_ref_t::print_branch_choices() const {
      bool is_first = true;
      auto choices = get_branch_choices();
      for (const element_ref_t & e : choices) {
        if (!is_first) {
          tesl_printf(" or ");
        }
        is_first = false;
        e.print();
      }
    }

    template<int ElementCount>
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

    template<int ElementCount>
    struct rule_t {
      parse_fn parse = nullptr;
      pattern_t<ElementCount> pattern;
    };

    template<typename T>
    struct rule_list_t;

    template<int V, int ... NextVs>
    struct rule_list_t<value_pack<V, NextVs...>> : rule_list_t<value_pack<NextVs...>> {
      using base = rule_list_t<value_pack<NextVs...>>;
      
      rule_t<V> this_rule;
      static constexpr int size = sizeof...(NextVs) + 1;

      constexpr rule_ref_storage_t operator[](int i) {
        if (i < 0) {
          return {};
        } else if (i == 0) {
          return {this_rule.parse, {this_rule.pattern.elements, V}};
        } else {
          return base::operator[](i - 1);
        }
      }

      constexpr rule_list_t(rule_t<V> rule, rule_t<NextVs> ... next_rules) : base(next_rules...), this_rule(rule) {}
    };

    template<>
    struct rule_list_t<value_pack<>> {
      rule_t<0> pattern;
      static constexpr int size = 0;

      constexpr rule_ref_storage_t operator[](int i) {
        return {};
      }
    };

    // woof woof :3
    // meow meow >:3
    template<typename T>
    struct rule_library_impl;

    template<int ... Vs, typename ... NextTs>
    struct rule_library_impl<type_pack<value_pack<Vs...>, NextTs...>> : rule_library_impl<type_pack<NextTs...>> {
      using base = rule_library_impl<type_pack<NextTs...>>;

      rule_list_t<value_pack<Vs...>> this_list;
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

      constexpr rule_library_impl(rule_list_t<value_pack<Vs...>> rule_list, rule_list_t<NextTs> ... next_lists) : base(next_lists...), this_list(rule_list) {
        for (int i = 0; i < sizeof...(Vs); ++i) {
          list_rules[i] = this_list[i];
        }
      }
    };

    template<>
    struct rule_library_impl<type_pack<>> {
      static constexpr int size = 0;

      constexpr rule_ref_list_storage_t operator[](int i) {
        return {};
      }
    };

    struct rule_library_ref_t {
      const rule_library_info_t &info;
      const rule_ref_list_storage_t * lists;
      const int size;
      const int max_precedence;

      constexpr rule_ref_list_t operator[](uint8_t p) const { return {lists[p], info, p}; }

      template<typename T>
      constexpr rule_library_ref_t(const T & library) : info(library.info), lists(library.lists), size(library.size), max_precedence(library.max_precedence) {}

      rule_ref_t find_rule_precedence(Token::Kind token, int precedence, int element_index) const;
      ParseResult parse_precedence(te_parser_state & s, int precedence) const;
      ParseResult parse_precedence_impl(te_parser_state & s, rules::rule_ref_t rule, parsed_sequence_t::node initial) const;
    };

    template<typename ... Ts>
    struct rule_library_t : public rule_library_impl<util::type_pack<Ts...>> {
      using base = rule_library_impl<util::type_pack<Ts...>>;

      static constexpr int size = sizeof...(Ts);
      static constexpr int max_precedence = size - 1;

      rule_library_info_t info;
      rule_ref_list_storage_t lists[sizeof...(Ts)];
      rule_library_ref_t ref;

      static_assert(size <= 256, "too many precedence levels!");

      const rule_library_ref_t * operator->() const {
        return &ref;
      }

      constexpr rule_library_t(const char * l, rule_list_t<Ts> ... rule_lists) : base(rule_lists...), info(l, size, max_precedence), ref(*this) {
        for (int i = 0; i < sizeof...(Ts); ++i) {
          lists[i] = base::operator[](i);
        }
      }
    };

    template<int ElementCount>
    constexpr rule_t<ElementCount> make_rule(parse_fn parse_fn, pattern_t<ElementCount> pattern) {
      return {parse_fn, pattern};
    }

    template<int ... Vs>
    constexpr auto make_rule_list(rule_t<Vs> ... rules) {
      return rule_list_t<util::value_pack<Vs...>>{rules...};
    }

    template<typename ... Ts>
    constexpr auto make_rule_library(const char * label, rule_list_t<Ts> ... lists) {
      return rule_library_t<Ts...>{label, lists...};
    }

    template<int8_t Offset>
    constexpr element_t relative_precedence_rule = element_t{element_t::RELATIVE_PRECEDENCE_RULE, Offset};
    template<int8_t Precedence>
    constexpr element_t precedence_rule = element_t{element_t::ABSOLUTE_PRECEDENCE_RULE, Precedence};

    // matches an expression with lower precedence
    constexpr element_t lower_rule = relative_precedence_rule<-1>;

    // matches an expression with same or lower precedence
    constexpr element_t similar_rule = relative_precedence_rule<0>;

    // marks a block of length N starting at the next element that is optional
    template<int8_t N>
    constexpr element_t opt = element_t{element_t::OPTIONAL, N};

    // jumps the element index to a relative offset N (used to make repeating patterns)
    template<int8_t N>
    constexpr element_t jmp = element_t{element_t::JUMP, N};

    // exactly one of the first or second element of a pattern must be a token
    // at most one of each token can appear in the first slot of all patterns (also applies to the second slot separately)
    // the element immediately after an 'opt<...>' or after the block it creates must be a token
    // the first expr must be a token or a similar_rule
    constexpr auto expr_pattern_library = make_rule_library(
      "expression",
      make_rule_list(
        make_rule(parse_literal_expr, make_pattern(Token::LITERAL)),
        make_rule(parse_identifier_expr, make_pattern(Token::IDENTIFIER))
      ),
      make_rule_list(
        make_rule(parse_grouping_expr, make_pattern(Token::OPEN_PAREN, precedence_rule<-1>, Token::CLOSE_PAREN))
      ),
      make_rule_list(
        make_rule(parse_subscript_expr, make_pattern(similar_rule, Token::OPEN_SQUARE_BRACKET, precedence_rule<-2>, Token::OPEN_SQUARE_BRACKET)),
        make_rule(parse_call_expr, make_pattern(similar_rule, Token::OPEN_PAREN, opt<6>, precedence_rule<-2>, opt<4>, Token::COMMA, precedence_rule<-2>, opt<1>, jmp<-3>, Token::CLOSE_PAREN)),
        make_rule(parse_construct_expr, make_pattern(Token::TYPENAME, Token::OPEN_PAREN, opt<6>, precedence_rule<-2>, opt<4>, Token::COMMA, precedence_rule<-2>, opt<1>, jmp<-3>, Token::CLOSE_PAREN)),
        make_rule(parse_dot_expr, make_pattern(similar_rule, Token::DOT, Token::IDENTIFIER)),
        make_rule(parse_postfix_expr, make_pattern(similar_rule, Token::PLUS_PLUS)),
        make_rule(parse_postfix_expr, make_pattern(similar_rule, Token::MINUS_MINUS))
      ),
      make_rule_list(
        make_rule(parse_prefix_expr, make_pattern(Token::PLUS_PLUS, similar_rule)),
        make_rule(parse_prefix_expr, make_pattern(Token::MINUS_MINUS, similar_rule)),
        make_rule(parse_prefix_expr, make_pattern(Token::PLUS, similar_rule)),
        make_rule(parse_prefix_expr, make_pattern(Token::MINUS, similar_rule)),
        make_rule(parse_prefix_expr, make_pattern(Token::BANG, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_arithmetic_expr, make_pattern(similar_rule, Token::STAR, similar_rule)),
        make_rule(parse_arithmetic_expr, make_pattern(similar_rule, Token::SLASH, similar_rule)),
        make_rule(parse_arithmetic_expr, make_pattern(similar_rule, Token::PERCENT, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_arithmetic_expr, make_pattern(similar_rule, Token::PLUS, similar_rule)),
        make_rule(parse_arithmetic_expr, make_pattern(similar_rule, Token::MINUS, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_bitwise_op_expr, make_pattern(similar_rule, Token::LESS_LESS, similar_rule)),
        make_rule(parse_bitwise_op_expr, make_pattern(similar_rule, Token::GREATER_GREATER, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_bitwise_op_expr, make_pattern(similar_rule, Token::AND, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_bitwise_op_expr, make_pattern(similar_rule, Token::CARET, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_bitwise_op_expr, make_pattern(similar_rule, Token::PIPE, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_comparison_expr, make_pattern(similar_rule, Token::LESS, similar_rule)),
        make_rule(parse_comparison_expr, make_pattern(similar_rule, Token::GREATER, similar_rule)),
        make_rule(parse_comparison_expr, make_pattern(similar_rule, Token::LESS_EQUAL, similar_rule)),
        make_rule(parse_comparison_expr, make_pattern(similar_rule, Token::GREATER_EQUAL, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_comparison_expr, make_pattern(similar_rule, Token::EQUAL_EQUAL, similar_rule)),
        make_rule(parse_comparison_expr, make_pattern(similar_rule, Token::BANG_EQUAL, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_boolean_op_expr, make_pattern(similar_rule, Token::AND_AND, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_boolean_op_expr, make_pattern(similar_rule, Token::PIPE_PIPE, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_ternary_expr, make_pattern(similar_rule, Token::QUESTION_MARK, similar_rule, Token::COLON, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_assignment_expr, make_pattern(similar_rule, Token::EQUAL, similar_rule)),
        make_rule(parse_assignment_expr, make_pattern(similar_rule, Token::PLUS_EQUAL, similar_rule)),
        make_rule(parse_assignment_expr, make_pattern(similar_rule, Token::MINUS_EQUAL, similar_rule)),
        make_rule(parse_assignment_expr, make_pattern(similar_rule, Token::STAR_EQUAL, similar_rule)),
        make_rule(parse_assignment_expr, make_pattern(similar_rule, Token::SLASH_EQUAL, similar_rule)),
        make_rule(parse_assignment_expr, make_pattern(similar_rule, Token::PERCENT_EQUAL, similar_rule))
      ),
      make_rule_list(
        make_rule(parse_sequence_expr, make_pattern(similar_rule, Token::COMMA, lower_rule, opt<1>, jmp<-3>))
      )
    );
    constexpr int expr_pattern_library_size_bytes = sizeof(expr_pattern_library);

    rule_ref_t rule_library_ref_t::find_rule_precedence(Token::Kind token, int precedence, int element_index) const {
      if (precedence < 0) {
        precedence += size;
      }
      for (int p = precedence; p >= 0; --p) {
        rule_ref_list_t pattern_list = operator[](p);
        for (int i = 0; i < pattern_list.size(); ++i) {
          rule_ref_t rule = pattern_list[i];
          if (rule.get_pattern()[element_index]->token == token) {
            return rule;
          }
        }
      }

      return {};
    }

    ParseResult rule_library_ref_t::parse_precedence_impl(te_parser_state & s, rules::rule_ref_t rule, ParseResult initial) const {
      TE_FAIL_COND(!rule.is_valid(), return parsed_sequence_t::node(new_error_expr(), rule.list.precedence));

      auto pattern_size = rule.get_pattern().size();
      Parser::ParseSequence nodes;
      nodes.reserve(pattern_size);
      nodes.push_back(initial);

#define FAIL_RETURN_ERROR_EXPR\
do {\
  te_op * error_sources[nodes.size()];\
  int error_sources_count = 0;\
  for (int i = 0; i < nodes.size(); ++i) {\
    switch(nodes[i].kind) {\
      case ParseResult::TOKEN:\
        break;\
      case ParseResult::EXPR:\
        error_sources[error_sources_count++] = nodes[i].expr;\
        break;\
      case ParseResult::OP:\
        error_sources[error_sources_count++] = nodes[i].op;\
        break;\
    }\
  }\
  return {new_error_expr_v(error_sources, error_sources_count), rule.list.precedence};\
} while (false)

      for (int i = 1; i < pattern_size;) {
        element_ref_t e = rule.get_pattern()[i];
        switch (e->kind) {
          case rules::element_t::TOKEN: {
            if (e->token != s.curr_token) {
              ErrorRecord er(s);
#ifdef TESL_DEBUG_COMPILE
              te_print("error: expected '");
              te_print(e->token);
              te_print("', got ");
              te_print(s.curr_token);
              te_print("!\n");
#endif
              te_make_error(s);
              s.advance();
              FAIL_RETURN_ERROR_EXPR;
            }
            nodes.push_back(s.curr_token);
            s.advance();
          } break;
          case rules::element_t::ABSOLUTE_PRECEDENCE_RULE: {
            nodes.push_back(parse_precedence(s, e->offset));
          } break;
          case rules::element_t::RELATIVE_PRECEDENCE_RULE: {
            nodes.push_back(parse_precedence(s, rule.list.precedence + e->offset));
          } break;
          case rules::element_t::OPTIONAL: {
            element_ref_t b = e.choose_branch(s.curr_token.kind);
            if (!b.is_valid()) {
              ErrorRecord er(s);
#ifdef TESL_DEBUG_COMPILE
              tesl_printf("error: expected ");
              e.print_branch_choices();
              tesl_printf(", got ");
              te_print(s.curr_token);
              tesl_printf("!\n");
#endif
              te_make_error(s);
              s.advance();
              FAIL_RETURN_ERROR_EXPR;
            }

            i = b.element_index;
            continue;
          } break;
          case rules::element_t::JUMP: {
            i += e->offset;
            continue;
          } break;
        }

        i++;
      }

      te_op * ret = rule.parse(s, MOV(nodes));
      if (!ret) {
        FAIL_RETURN_ERROR_EXPR;
      }

      return {ret, rule.list.precedence};

#undef FAIL_RETURN_ERROR_EXPR
    }

    // never returns null
    parsed_sequence_t::node rule_library_ref_t::parse_precedence(te_parser_state & s, int precedence) const {
      if (precedence < 0) {
        precedence += size;
      }
      TE_FAIL_COND(precedence >= 256, return parsed_sequence_t::node(new_error_expr(), precedence));

      rule_ref_t rule = find_rule_precedence(s, precedence, 0);
      if (!rule.is_valid()) {
        ErrorRecord er(s);
#ifdef TESL_DEBUG_COMPILE
        te_print("error: expected ");
        te_print(info.rule_label);
        te_print(" got '");
        te_print(s.curr_token.kind);
        te_print("'!\n");
#endif
        te_make_error(s);
        s.advance();
        return parsed_sequence_t::node(new_error_expr(), precedence);
      }
      parsed_sequence_t::node n = parse_precedence_impl(s, rule, s.consume());
      while (true) {
        rule = find_rule_precedence(s, precedence, 1);
        if (!rule.is_valid()) {
          break;
        }
        n = parse_precedence_impl(s, rule, n);
      }

      return n;
    }
  }

  // never returns null
  te_expr * parse_expr(te_parser_state & s, int precedence) {
    using namespace rules;
    parsed_sequence_t::node result = expr_pattern_library->parse_precedence(s, precedence);
    switch (result.kind) {
      case parsed_sequence_t::node::EXPR:
        return result.expr;
      case parsed_sequence_t::node::OP:
        return new_error_expr(result.op);
      case parsed_sequence_t::node::TOKEN:
        return new_error_expr();
    }

    return new_error_expr();
  }

  // never returns null
  te_expr * parse_top_level_expr(te_parser_state & s) {
    return parse_expr(s, -1);
  }

  void ErrorRecord::reset() {
    if (parser) {
      line_start = parser->line_start;
      point = start = parser->curr_token.name.ptr;
      end = parser->curr_token.name.end;
      line_num = parser->line_num;
    } else {
      line_start = nullptr;
      start = nullptr;
      point = nullptr;
      end = nullptr;
      line_num = 0;
    }
  }

  ErrorRecord::ErrorRecord(Parser & p) : parser(&p), prev(p.this_error) {
    line_start = p.line_start;
    point = start = p.curr_token.name.ptr;
    end = p.curr_token.name.end;
    line_num = p.line_num;
    p.this_error = this;
  }

  ErrorRecord::~ErrorRecord() {
    if (parser) {
      parser->this_error = prev;
      parser->prev_error = *this;
      parser->prev_error.parser = nullptr;
      parser->prev_error.prev = nullptr;
    }
  }
  
  void Parser::print_error_line_info() const {
    if (this_error != nullptr) {
      this_error->print();
    } else {
      print_error_source(line_num, line_start, curr_token.name.ptr, curr_token.name.ptr, curr_token.name.end);
    }
  }
} // namespace tesl

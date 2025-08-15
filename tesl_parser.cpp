#include "tesl_parser.hpp"

#include "tesl_str.hpp"
#include <fmt/format.h>

namespace tesl::rules {
  struct ExprResult {
    TypeRef type = get_type_info_of<Null>();
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
      TESL_ASSERT(is_empty());
    }
  };*/

  using ParseSequence = Parser::ParseSequence;
  using ParseResult = Parser::ParseResult;
  using ParseFn = Parser::ParseFn;

  struct PatternElement;
  struct PatternRefStorage;
  struct RuleRefStorage;
  struct RuleRefListStorage;

  struct ElementRef;
  struct PatternRef;
  struct RuleRef;
  struct RuleRefList;
}

namespace tesl {
  using ExprResult = rules::ExprResult;

  struct Parser::ParseResult {
    enum Kind {
      NONE,
      TOKEN,
      VALUE,
      IDENTIFIER,
      EXPR,
    };

    Kind kind = NONE;

    union {
      Token::Kind token = Token::NONE;
      Variant value;
      CharStrView identifier;
      rules::ExprResult expr;
    };

    ParseResult & operator=(const ParseResult & other) {
      this->~ParseResult();
      new(this) ParseResult(other);
      return *this;
    }
  
    ParseResult & operator=(ParseResult && other) {
      memswap(*this, other);
      return *this;
    }

    ParseResult(const ParseResult & other) : kind(other.kind) {
      switch (other.kind) {
        case NONE:
          break;
        case TOKEN:
          token = other.token;
          break;
        case VALUE:
          new(&value) Variant(other.value);
          break;
        case IDENTIFIER:
          new(&identifier) CharStrView(other.identifier);
          break;
        case EXPR:
          new(&expr) ExprResult(other.expr);
          break;
      }
    }

    ParseResult(ParseResult && other) {
      memswap(*this, other);
    }

    explicit ParseResult(const Token & t) {
      if (t.kind == Token::LITERAL) {
        kind = VALUE;
        new(&value) Variant(t.literal);
      } else if (t.kind == Token::IDENTIFIER) {
        kind = IDENTIFIER;
        new(&identifier) CharStrView(t.span);
      } else {
        kind = TOKEN;
        token = t.kind;
      }
    }
    explicit ParseResult(Token::Kind k) : kind(TOKEN), token(k) {}
    ParseResult(rules::ExprResult e) : kind(EXPR), expr(e) {}

    ParseResult() {}
    ~ParseResult() {
      switch (kind) {
        case NONE:
        case TOKEN:
          break;
        case VALUE:
          value.~Variant();
          break;
        case IDENTIFIER:
          identifier.~CharStrView();
          break;
        case EXPR:
          expr.~ExprResult();
          break;
      }
    }
  };

  struct Parser::ParseSequence : public Array<Parser::ParseResult, 3> {};

#define parser_error(...) \
  do { \
    has_error = true; \
    fmt::print(stderr, "parser error: "); \
    fmt::println(stderr, __VA_ARGS__); \
    print_error_source(); \
  } while (false)

#define internal_parser_error(...) \
  do { \
    has_error = true; \
    fmt::print(stderr, "internal parser error: "); \
    fmt::println(stderr, __VA_ARGS__); \
    print_error_source(); \
  } while (false)

  Parser::ParseResult Parser::parse_literal_expr(ParseSequence sequence) {
    TESL_ASSERT(sequence.size() == 1);
    TESL_ASSERT(sequence[0].kind == ParseResult::VALUE);
    return {sequence[0]};
  }

  Parser::ParseResult Parser::parse_identifier_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_grouping_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_subscript_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_call_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_construct_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_dot_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_postfix_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_prefix_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_arithmetic_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_comparison_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_bitwise_op_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_boolean_op_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_ternary_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_assignment_expr(ParseSequence sequence) {
    // TODO: finish
    return {ExprResult()};
  }

  Parser::ParseResult Parser::parse_sequence_expr(ParseSequence sequence) {
    TESL_ASSERT(sequence.size() >= 3);
    TESL_ASSERT(sequence.size() % 2 == 1);

    // TODO: finish
    return {ExprResult()};

    /*IntT sequence_size = (sequence.size() + 1) / 2;

    uint16_t offset = 0;
    uint16_t size = 0;
    te_expr * args[sequence_size];
    Type param_types[sequence_size];
    for (IntT i = 0; i < sequence.size(); ++i) {
      if (i % 2 == 0) {
        TESL_ASSERT(sequence[i].kind == ParseResult::EXPR);
        TESL_ASSERT(sequence[i].expr != nullptr);
        args[i / 2] = sequence[i].expr;
        param_types[i] = args[i]->type;
        if (i != (sequence.size() - 1)) {
          offset += sizeof_type(param_types[i]);
        } else {
          size = sizeof_type(param_types[i]);
        }
      } else {
        TESL_ASSERT(sequence[i].kind == ParseResult::TOKEN);
        TESL_ASSERT(sequence[i].token.kind == Token::COMMA);
      }
    }

    static_assert(sizeof(uint16_t) * 2 <= sizeof(void *));
    void * ctx;
    uint16_t * ctx_data = reinterpret_cast<uint16_t *>(&ctx);
    ctx_data[0] = offset;
    ctx_data[0] = size;
    return new_call_expr_v(te::detail::make_function_raw(te::sequence, ctx, true, TYPE_NULL_VAL, param_types, sequence_size), args, sequence_size);*/
  }
  
  #undef TESL_PARSER_DEBUG  

  namespace rules {
    struct PatternElement {
      enum Kind : char {
        TOKEN,
        ABSOLUTE_PRECEDENCE_RULE,
        RELATIVE_PRECEDENCE_RULE,
        OPTIONAL,
        JUMP,
      } kind = TOKEN;
      union {
        Token::Kind token = Token::NONE;
        std::int8_t offset;
      };

      constexpr PatternElement(const PatternElement &) = default;
      constexpr PatternElement(Token::Kind t) : kind(TOKEN), token(t) {}
      //constexpr PatternElement(Kind k, Token::Kind t) : kind(k), token(t) {}
      constexpr PatternElement(Kind k, std::int8_t v) : kind(k), offset(v) {}
      constexpr PatternElement() = default;
    };

    template<IntT precedence>
    constexpr PatternElement make_absolute_precedence_element() {
      using limits = std::numeric_limits<decltype(PatternElement::offset)>;
      static_assert(precedence <= limits::max() && precedence >= limits::min());
      return {PatternElement::ABSOLUTE_PRECEDENCE_RULE, static_cast<std::int8_t>(precedence)};
    }

    struct PatternRefStorage {
      const PatternElement * elements = nullptr;
      IntT size = 0;

      bool is_valid() const {
        return size == 0 || (size > 0 && elements != nullptr);
      }
    };

    struct RuleRefStorage {
      CharStrView name;
      ParseFn parse = nullptr;
      PatternRefStorage pattern;

      bool is_valid() const {
        return parse != nullptr && pattern.is_valid();
      }
    };

    struct RuleRefListStorage {
      const RuleRefStorage * rules = nullptr;
      IntT size = 0;

      bool is_valid() const {
        return size == 0 || (size > 0 && rules != nullptr);
      }
    };

    struct RuleRefList {
      const RuleLibrary * _library = nullptr;
      const RuleRefListStorage * _storage = nullptr;
      IntT precedence = 0;

      bool is_valid() const {
        return _storage != nullptr && _storage->is_valid();
      }

      IntT size() const {
        TESL_ASSERT(_storage != nullptr);
        return _storage->size;
      }

      const RuleLibrary & get_library() const {
        TESL_ASSERT(_library != nullptr);
        return *_library;
      }

      bool operator==(const RuleRefList &other) const {
        return _storage == other._storage;
      }

      RuleRef operator[](IntT i) const;

      constexpr RuleRefList(const RuleLibrary * l, const RuleRefListStorage * st, IntT p) : _library(l), _storage(st), precedence(p) {}
      constexpr RuleRefList() = default;
    };

    struct RuleRef {
      RuleRefList list;
      const RuleRefStorage * _storage = nullptr;

      bool is_valid() const {
        return _storage != nullptr && _storage->is_valid();
      }

      bool operator==(const RuleRef & other) const {
        return _storage == other._storage;
      }

      CharStrView get_name() const;
      PatternRef get_pattern() const;
      ParseResult parse(Parser & p, ParseSequence && seq);

      constexpr RuleRef(const RuleRefList & l, const RuleRefStorage * r) : list(l), _storage(r) {}
      constexpr RuleRef() = default;
    };

    struct PatternRef {
      RuleRef rule;
      const PatternRefStorage * _storage = nullptr;

      bool is_valid() const {
        return _storage != nullptr && _storage->is_valid();
      }

      bool operator==(const PatternRef & other) const {
        return _storage == other._storage;
      }

      IntT size() const {
        TESL_ASSERT(_storage != nullptr);
        return _storage->size;
      }

      ElementRef operator[](IntT i) const;

      constexpr PatternRef(const RuleRef & r, const PatternRefStorage * p) : rule(r), _storage(p) {}
      constexpr PatternRef() = default;
    };

    struct ElementRef {
      PatternRef pattern;
      IntT element_index = 0;

      bool is_valid() const {
        return pattern.is_valid() && element_index >= 0 && element_index <= pattern.size();
      }

      bool operator==(const ElementRef & other) const {
        return pattern == other.pattern && element_index == other.element_index;
      }

      const PatternElement * operator->() const {
        return &pattern._storage->elements[element_index];
      }

      operator const PatternElement &() const {
        return pattern._storage->elements[element_index];
      }

      PatternElement::Kind get_kind() const {
        return (*this)->kind;
      }

      bool matches_rule() const;
      bool matches_rule(const RuleRef & r) const;
      bool matches_token(Token::Kind tk) const;
      IntT get_matched_rule_precedence() const;
      ElementRef follow() const;
#ifdef TESL_PARSER_NESTED_OPT
      void get_branch_choices_impl(Array<ElementRef> & array) const;
      Array<ElementRef> get_branch_choices() const;
#endif
      Str stringify_branch_choices() const;
      bool is_end() const;
      bool can_end_pattern() const;
      //void validate_branch_choices() const;
      ElementRef choose_branch(Token::Kind tok) const;

      constexpr ElementRef(const PatternRef & p, IntT i) : pattern(p), element_index(i) {}
      constexpr ElementRef() = default;
    };
  }
}

template<typename CharT>
class fmt::formatter<tesl::Parser::ParseResult, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::Parser::ParseResult & pr, Context & ctx) const {
    using namespace tesl;
    using namespace rules;

    switch (pr.kind) {
      case ParseResult::NONE:
        return fmt::format_to(ctx.out(), "<none>");
      case ParseResult::TOKEN:
        return fmt::format_to(ctx.out(), "token: {:?}", pr.token);
      case ParseResult::VALUE:
        return fmt::format_to(ctx.out(), "value: {}", pr.value);
      case ParseResult::IDENTIFIER:
        return fmt::format_to(ctx.out(), "identifier: {}", pr.identifier);
      case ParseResult::EXPR:
        return fmt::format_to(ctx.out(), "expr: {}", pr.expr.type);
    }

    TESL_UNREACHABLE;
  }
};

template<typename CharT>
class fmt::formatter<tesl::rules::ElementRef, CharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::rules::ElementRef & e, Context & ctx) const {
    using namespace tesl;
    using namespace rules;

    if (!e.is_valid()) {
      return fmt::format_to(ctx.out(), "<invalid-pattern-element>");
    }

    if (e.element_index == e.pattern.size()) {
      return fmt::format_to(ctx.out(), "<pattern-end>");
    }

    const RuleRefList & rule_list = e.pattern.rule.list;
    const RuleLibrary & library = rule_list.get_library();
  
    switch (e->kind) {
      case PatternElement::TOKEN:
        return fmt::format_to(ctx.out(), "{:?}", e->token);
      case PatternElement::ABSOLUTE_PRECEDENCE_RULE:
      case PatternElement::RELATIVE_PRECEDENCE_RULE: {
#ifdef TESL_DEBUG_PARSER
        return fmt::format_to(ctx.out(), "{}@p{}", library.get_label(), e.get_matched_rule_precedence());
#else
        return fmt::format_to(ctx.out(), "{}", library.get_label());
#endif
      } break;
      case PatternElement::OPTIONAL:
        return fmt::format_to(ctx.out(), "<opt{:+d}>", e->offset);
      case PatternElement::JUMP:
        return fmt::format_to(ctx.out(), "<jmp{:+d}>", e->offset);
    }
  
    TESL_UNREACHABLE;
  }
};

namespace tesl {
  namespace rules {
    RuleRefList RuleLibrary::operator[](IntT p) const {
      TESL_ASSERT(_lists != nullptr);
      TESL_ASSERT(p < _size);
      return {this, &_lists[p], p};
    }

    RuleRef RuleRefList::operator[](IntT i) const {
      TESL_ASSERT(_storage != nullptr);
      TESL_ASSERT(_storage->rules->is_valid());
      TESL_ASSERT(i < _storage->size);
      return {*this, &_storage->rules[i]};
    }

    CharStrView RuleRef::get_name() const {
      return _storage->name;
    }

    PatternRef RuleRef::get_pattern() const {
      return {*this, &_storage->pattern};
    }

    ParseResult RuleRef::parse(Parser & p, ParseSequence && seq) {
      ParseFn parsefn = _storage->parse;
      return ((&p)->*parsefn)(MOV(seq));
    }

    ElementRef PatternRef::operator[](IntT i) const {
      return {*this, i};
    }

    bool ElementRef::matches_rule() const {
      const tesl::rules::PatternElement & e = *this;
      return e.kind == PatternElement::ABSOLUTE_PRECEDENCE_RULE || e.kind == PatternElement::RELATIVE_PRECEDENCE_RULE;
    }

    bool ElementRef::matches_rule(const RuleRef & r) const {
      const tesl::rules::PatternElement & e = *this;
      if (e.kind == PatternElement::ABSOLUTE_PRECEDENCE_RULE) {
        return r.list.precedence <= e.offset;
      } else if (e.kind == PatternElement::RELATIVE_PRECEDENCE_RULE) {
        return r.list.precedence <= (pattern.rule.list.precedence + e.offset);
      }

      return false;
    }

    bool ElementRef::matches_token(Token::Kind tk) const {
      const tesl::rules::PatternElement & e = *this;
      return e.kind == PatternElement::TOKEN && e.token == tk;
    }

    IntT ElementRef::get_matched_rule_precedence() const {
      if (get_kind() == PatternElement::ABSOLUTE_PRECEDENCE_RULE) {
        IntT p = (*this)->offset;
        if (p < 0) {
          p += pattern.rule.list.get_library().size();
        }
        return p;
      } else if (get_kind() == PatternElement::RELATIVE_PRECEDENCE_RULE) {
        IntT p = pattern.rule.list.precedence + (*this)->offset;
        if (p < 0) {
          p += pattern.rule.list.get_library().size();
        }
        return p;
      }

      return -1;
    }

    ElementRef ElementRef::follow() const {
      IntT loop_count = 0;
      IntT i = element_index;
      const PatternElement * e = &pattern._storage->elements[i];
      while (e && e->kind == PatternElement::JUMP) {
        TESL_ASSERT(loop_count < 256);
        TESL_ASSERT(e->offset != 0);
        i = i + e->offset;
        e = &pattern._storage->elements[i];
        loop_count++;
      }

      return {pattern, i};
    }

    ElementRef ElementRef::choose_branch(Token::Kind tok) const {
      TESL_ASSERT(get_kind() == PatternElement::OPTIONAL);

#ifdef TESL_DEBUG_PARSER
      fmt::println("choosing between branches: {}", stringify_branch_choices());
#endif

      ElementRef result;
#ifdef TESL_PARSER_NESTED_OPT
      auto choices = get_branch_choices();
      for (const ElementRef & e : choices) {
        if (e.is_end()) {
          result = e; // possible match, but dont stop searching yet
          continue;
        }
        switch (e->kind) {
          case PatternElement::TOKEN:
            if (e->token == tok) {
              fmt::println("matched {}", e);
              return e; // stop visiting branches, we found our guy
            }
            break;
          case PatternElement::ABSOLUTE_PRECEDENCE_RULE:
          case PatternElement::RELATIVE_PRECEDENCE_RULE:
            result = e; // possible match, but dont stop searching yet
            break;
          case PatternElement::OPTIONAL:
          case PatternElement::JUMP:
            TESL_UNREACHABLE;
        }
      }
#else
      ElementRef a{pattern, element_index + 1};
      ElementRef b{pattern, element_index + (*this)->offset + 1};
      if (a->kind == PatternElement::TOKEN) {
        if (a->token == tok) {
          result = a;
        } else if (b.is_end() || b.matches_rule()) {
          result = b;
        } else if (b->kind == PatternElement::TOKEN) {
          if (b->token == tok) {
            result = b;
          }
        } else {
#ifdef TESL_DEBUG_PARSER
          fmt::println("internal parser error: incorrect opt: second option is neither token nor rule ('{}' at element {})", pattern.rule.get_name(), element_index);
#endif
        }
      } else if (b->kind == PatternElement::TOKEN) {
        if (a.is_end() || a.matches_rule()) {
          result = a;
        } else if (b->token == tok) {
          result = b;
        } else {
#ifdef TESL_DEBUG_PARSER
          fmt::println("internal parser error: incorrect opt: first option is neither token nor rule ('{}' at element {})", pattern.rule.get_name(), element_index);
#endif
        }
      } else {
#ifdef TESL_DEBUG_PARSER
        fmt::println("internal parser error: incorrect opt: no token to match ('{}' at element {})", pattern.rule.get_name(), element_index);
#endif
      }
#endif

#ifdef TESL_DEBUG_PARSER
      if (result.is_valid()) {
        fmt::println("matched {}", result);
      } else {
        fmt::println("no match");
      }
#endif

      return result;
    }

#ifdef TESL_PARSER_NESTED_OPT
    void ElementRef::get_branch_choices_impl(Array<ElementRef> & array) const {
      if (!is_valid()) {
        if (element_index == pattern.size()) {
          if (!array.has(*this)) {
            array.push_back(*this);
          }
        }
        return;
      }
      ElementRef points_to = follow();
      switch (points_to->kind) {
        case PatternElement::TOKEN:
        case PatternElement::ABSOLUTE_PRECEDENCE_RULE:
        case PatternElement::RELATIVE_PRECEDENCE_RULE:
          if (points_to.is_valid() && !array.has(points_to)) {
            array.push_back(points_to);
          }
          return;
        case PatternElement::OPTIONAL:
          ElementRef{pattern, points_to.element_index + 1}.get_branch_choices_impl(array);
          ElementRef{pattern, points_to.element_index + points_to->offset + 1}.get_branch_choices_impl(array);
          return;
        case PatternElement::JUMP:
          TESL_UNREACHABLE;
      }

      TESL_UNREACHABLE;
    }

    Array<ElementRef> ElementRef::get_branch_choices() const {
      Array<ElementRef> result;
      get_branch_choices_impl(result);
      return MOV(result);
    }

    Str ElementRef::stringify_branch_choices() const {
      Str result;
      bool is_first = true;
      for (const ElementRef & e : get_branch_choices()) {
        if (!is_first) {
          result += " or ";
        }
        is_first = false;
        result += fmt::format("{}", e);
      }
      return result;
    }

    bool ElementRef::can_end_pattern() const {
      for (const ElementRef & e : get_branch_choices()) {
        if (e.is_end()) {
          return true;
        }
      }
      return false;
    }
#else
    Str ElementRef::stringify_branch_choices() const {
      Str result;

      ElementRef a{pattern, element_index + 1};
      ElementRef b{pattern, element_index + (*this)->offset + 1};

      return fmt::format("{} or {}", a, b);
    }

    bool ElementRef::can_end_pattern() const {
      ElementRef a{pattern, element_index + 1};
      ElementRef b{pattern, element_index + (*this)->offset + 1};

      return a.is_end() || b.is_end();
    }
#endif

    bool ElementRef::is_end() const {
      return pattern.is_valid() && element_index == pattern.size();
    }

    template<int ElementCount>
    struct PatternTmpl {
      static constexpr int size = ElementCount;
      PatternElement elements[ElementCount];

      constexpr operator const PatternElement *() const { return elements; }
    };

    template<typename ... Ts>
    constexpr PatternTmpl<sizeof...(Ts)> make_pattern(Ts ... elements) {
      static_assert(sizeof...(elements) > 0);
      return {PatternElement{elements}...};
    }

    template<int ElementCount>
    struct RuleTmpl {
      CharStrView name;
      ParseFn parse = nullptr;
      PatternTmpl<ElementCount> pattern;
    };

    template<typename T>
    struct RuleListTmpl;

    template<int V, int ... NextVs>
    struct RuleListTmpl<value_pack<V, NextVs...>> : RuleListTmpl<value_pack<NextVs...>> {
      using base = RuleListTmpl<value_pack<NextVs...>>;
      
      RuleTmpl<V> this_rule;
      static constexpr IntT size = sizeof...(NextVs) + 1;

      constexpr RuleRefStorage operator[](IntT i) {
        if (i < 0) {
          return {};
        } else if (i == 0) {
          return {this_rule.name, this_rule.parse, {this_rule.pattern.elements, V}};
        } else {
          return base::operator[](i - 1);
        }
      }

      constexpr RuleListTmpl(RuleTmpl<V> rule, RuleTmpl<NextVs> ... next_rules) : base(next_rules...), this_rule(rule) {}
    };

    template<>
    struct RuleListTmpl<value_pack<>> {
      RuleTmpl<0> pattern;
      static constexpr IntT size = 0;

      constexpr RuleRefStorage operator[](IntT i) {
        return {};
      }
    };

    // woof woof :3
    // meow meow >:3
    template<typename T>
    struct RuleLibraryTmplBase;

    template<int ... Vs, typename ... NextTs>
    struct RuleLibraryTmplBase<type_pack<value_pack<Vs...>, NextTs...>> : RuleLibraryTmplBase<type_pack<NextTs...>> {
      using base = RuleLibraryTmplBase<type_pack<NextTs...>>;

      RuleListTmpl<value_pack<Vs...>> this_list;

      RuleRefStorage list_rules[sizeof...(Vs)];
      static constexpr int size = sizeof...(NextTs) + 1;

      constexpr RuleRefListStorage operator[](int i) {
        if (i < 0) {
          return {};
        } else if (i == 0) {
          return {list_rules, sizeof...(Vs)};
        } else {
          return base::operator[](i - 1);
        }
      }

      constexpr RuleLibraryTmplBase(RuleListTmpl<value_pack<Vs...>> rule_list, RuleListTmpl<NextTs> ... next_lists) : base(next_lists...), this_list(rule_list) {
        for (int i = 0; i < sizeof...(Vs); ++i) {
          list_rules[i] = this_list[i];
        }
      }
    };

    template<>
    struct RuleLibraryTmplBase<type_pack<>> {
      static constexpr int size = 0;

      constexpr RuleRefListStorage operator[](int i) {
        return {};
      }
    };

    template<typename ... Ts>
    struct RuleLibraryTmpl : public RuleLibraryTmplBase<type_pack<Ts...>> {
      using base = RuleLibraryTmplBase<type_pack<Ts...>>;

      static constexpr int size = sizeof...(Ts);

      static constexpr int max_precedence = size - 1;

      StrView label;
      RuleRefListStorage lists[sizeof...(Ts)];

      constexpr RuleLibraryTmpl(StrView l, RuleListTmpl<Ts> ... rule_lists) : base(rule_lists...), label(l) {
        for (int i = 0; i < sizeof...(Ts); ++i) {
          lists[i] = base::operator[](i);
        }
      }
    };

    template<int ElementCount>
    constexpr RuleTmpl<ElementCount> make_rule(CharStrView name, ParseFn parse_fn, PatternTmpl<ElementCount> pattern) {
      return {name, parse_fn, pattern};
    }

    template<int ... Vs>
    constexpr auto make_rule_list(RuleTmpl<Vs> ... rules) {
      return RuleListTmpl<value_pack<Vs...>>{rules...};
    }

    template<typename ... Ts>
    constexpr auto make_rule_library(StrView label, RuleListTmpl<Ts> ... lists) {
      return RuleLibraryTmpl<Ts...>{label, lists...};
    }

    template<int8_t Offset>
    constexpr PatternElement relative_precedence_rule = PatternElement{PatternElement::RELATIVE_PRECEDENCE_RULE, Offset};
    template<int8_t Precedence>
    constexpr PatternElement precedence_rule = PatternElement{PatternElement::ABSOLUTE_PRECEDENCE_RULE, Precedence};

    // matches an expression with lower precedence
    constexpr PatternElement lower_rule = relative_precedence_rule<-1>;

    // matches an expression with same or lower precedence
    constexpr PatternElement similar_rule = relative_precedence_rule<0>;

    // marks a block of length N starting at the next element that is optional
    template<int8_t N>
    constexpr PatternElement opt = PatternElement{PatternElement::OPTIONAL, N};

    // jumps the element index to a relative offset N (used to make repeating patterns)
    template<int8_t N>
    constexpr PatternElement jmp = PatternElement{PatternElement::JUMP, N};

    // exactly one of the first or second element of a pattern must be a token
    // at most one of each token can appear in the first slot of all patterns (also applies to the second slot separately)
    // the element immediately after an 'opt<...>' or after the block it creates must be at least 1 token and at most 1 expression, and no jmp or opt
    // the first expr must be a token or a similar_rule
    constexpr auto expression_library_tmpl = make_rule_library(
      TESL_STRVIEW("expression"),
      make_rule_list(
        make_rule("literal", &Parser::parse_literal_expr, make_pattern(Token::LITERAL)),
        make_rule("identifier", &Parser::parse_identifier_expr, make_pattern(Token::IDENTIFIER))
      ),
      make_rule_list(
        make_rule("grouping", &Parser::parse_grouping_expr, make_pattern(Token::OPEN_PAREN, precedence_rule<-1>, Token::CLOSE_PAREN))
      ),
      make_rule_list(
        make_rule("subscript", &Parser::parse_subscript_expr, make_pattern(similar_rule, Token::OPEN_SQUARE_BRACKET, precedence_rule<-2>, Token::OPEN_SQUARE_BRACKET)),
        make_rule("function call", &Parser::parse_call_expr, make_pattern(similar_rule, Token::OPEN_PAREN, opt<5>, precedence_rule<-2>, opt<3>, Token::COMMA, precedence_rule<-2>, jmp<-3>, Token::CLOSE_PAREN)),
        make_rule("member access", &Parser::parse_dot_expr, make_pattern(similar_rule, Token::DOT, Token::IDENTIFIER))
      ),
      make_rule_list(
        make_rule("unary plus", &Parser::parse_prefix_expr, make_pattern(Token::PLUS, similar_rule)),
        make_rule("unary minus", &Parser::parse_prefix_expr, make_pattern(Token::MINUS, similar_rule)),
        make_rule("logical not", &Parser::parse_prefix_expr, make_pattern(Token::BANG, similar_rule)),
        make_rule("bitwise not", &Parser::parse_prefix_expr, make_pattern(Token::TILDE, similar_rule))
      ),
      make_rule_list(
        make_rule("multipication", &Parser::parse_arithmetic_expr, make_pattern(similar_rule, Token::STAR, similar_rule)),
        make_rule("division", &Parser::parse_arithmetic_expr, make_pattern(similar_rule, Token::SLASH, similar_rule)),
        make_rule("remainder", &Parser::parse_arithmetic_expr, make_pattern(similar_rule, Token::PERCENT, similar_rule))
      ),
      make_rule_list(
        make_rule("addition", &Parser::parse_arithmetic_expr, make_pattern(similar_rule, Token::PLUS, similar_rule)),
        make_rule("subtraction", &Parser::parse_arithmetic_expr, make_pattern(similar_rule, Token::MINUS, similar_rule))
      ),
      make_rule_list(
        make_rule("bitwise left shift", &Parser::parse_bitwise_op_expr, make_pattern(similar_rule, Token::LESS_LESS, similar_rule)),
        make_rule("bitwise right shift", &Parser::parse_bitwise_op_expr, make_pattern(similar_rule, Token::GREATER_GREATER, similar_rule))
      ),
      make_rule_list(
        make_rule("bitwise and", &Parser::parse_bitwise_op_expr, make_pattern(similar_rule, Token::AND, similar_rule))
      ),
      make_rule_list(
        make_rule("bitwise xor", &Parser::parse_bitwise_op_expr, make_pattern(similar_rule, Token::CARET, similar_rule))
      ),
      make_rule_list(
        make_rule("bitwise or", &Parser::parse_bitwise_op_expr, make_pattern(similar_rule, Token::PIPE, similar_rule))
      ),
      make_rule_list(
        make_rule("compare less than", &Parser::parse_comparison_expr, make_pattern(similar_rule, Token::LESS, similar_rule)),
        make_rule("compare greater than", &Parser::parse_comparison_expr, make_pattern(similar_rule, Token::GREATER, similar_rule)),
        make_rule("compare less than or equal to", &Parser::parse_comparison_expr, make_pattern(similar_rule, Token::LESS_EQUAL, similar_rule)),
        make_rule("compare greater than or equal to", &Parser::parse_comparison_expr, make_pattern(similar_rule, Token::GREATER_EQUAL, similar_rule))
      ),
      make_rule_list(
        make_rule("compare equal", &Parser::parse_comparison_expr, make_pattern(similar_rule, Token::EQUAL_EQUAL, similar_rule)),
        make_rule("compare not equal", &Parser::parse_comparison_expr, make_pattern(similar_rule, Token::BANG_EQUAL, similar_rule))
      ),
      make_rule_list(
        make_rule("logical and", &Parser::parse_boolean_op_expr, make_pattern(similar_rule, Token::AND_AND, similar_rule))
      ),
      make_rule_list(
        make_rule("logical or", &Parser::parse_boolean_op_expr, make_pattern(similar_rule, Token::PIPE_PIPE, similar_rule))
      ),
      make_rule_list(
        make_rule("ternary", &Parser::parse_ternary_expr, make_pattern(similar_rule, Token::QUESTION_MARK, similar_rule, Token::COLON, similar_rule))
      ),
      make_rule_list(
        make_rule("assign", &Parser::parse_assignment_expr, make_pattern(similar_rule, Token::EQUAL, similar_rule)),
        make_rule("add assign", &Parser::parse_assignment_expr, make_pattern(similar_rule, Token::PLUS_EQUAL, similar_rule)),
        make_rule("subtract assign", &Parser::parse_assignment_expr, make_pattern(similar_rule, Token::MINUS_EQUAL, similar_rule)),
        make_rule("multiply assign", &Parser::parse_assignment_expr, make_pattern(similar_rule, Token::STAR_EQUAL, similar_rule)),
        make_rule("divide assign", &Parser::parse_assignment_expr, make_pattern(similar_rule, Token::SLASH_EQUAL, similar_rule)),
        make_rule("remainder assign", &Parser::parse_assignment_expr, make_pattern(similar_rule, Token::PERCENT_EQUAL, similar_rule))
      ),
      make_rule_list(
        make_rule("sequence", &Parser::parse_sequence_expr, make_pattern(lower_rule, Token::COMMA, lower_rule, opt<3>, Token::COMMA, lower_rule, jmp<-3>))
      )
    );

    RuleRef RuleLibrary::find_rule_precedence_first(Token::Kind token, IntT precedence) const {
      if (precedence < 0) {
        precedence += size();
      }

      for (IntT p = precedence; p >= 0; --p) {
        RuleRefList pattern_list = operator[](p);
        for (IntT i = 0; i < pattern_list.size(); ++i) {
          RuleRef rule = pattern_list[i];
          if (rule.get_pattern()[0].matches_token(token)) {
            return rule;
          }
        }
      }

      return {};
    }

    RuleRef RuleLibrary::find_rule_precedence_second(const RuleRef & r, Token::Kind token, IntT precedence) const {
      if (precedence < 0) {
        precedence += size();
      }

      for (IntT p = precedence; p >= 0; --p) {
        RuleRefList pattern_list = operator[](p);
        for (IntT i = 0; i < pattern_list.size(); ++i) {
          RuleRef rule = pattern_list[i];
          if (rule.get_pattern().size() < 2) {
            continue;
          }
          if (rule.get_pattern()[0].matches_rule(r) && rule.get_pattern()[1].matches_token(token)) {
            return rule;
          }
        }
      }

      return {};
    }
  }

  const rules::RuleLibrary Parser::expression_library = rules::expression_library_tmpl;

  void Parser::parse_program() {
    current_token = tokenizer.next_token();
    parse_precedence(expression_library, -1);
  }

  Parser::ParseResult Parser::parse_precedence(const rules::RuleLibrary & library, IntT precedence) {
    using namespace rules;

    if (precedence < 0) {
      precedence += library.size();
    }

    TESL_FAIL_COND(precedence >= library.size(), return {});

    RuleRef rule = library.find_rule_precedence_first(current_token.kind, precedence);
    if (!rule.is_valid()) {
      parser_error("expected {}[p{}], got {}!", library.get_label(), precedence, current_token);
      current_token = tokenizer.next_token();
      return {};
    }

    ParseResult result{current_token};
#ifdef TESL_DEBUG_PARSER
    fmt::println("push {}", current_token);
#endif
    current_token = tokenizer.next_token();
    do {
      result = parse_precedence_impl(rule, MOV(result));
      rule = library.find_rule_precedence_second(rule, current_token.kind, precedence);
    } while (rule.is_valid());

    return result;
  }

  Parser::ParseResult Parser::parse_precedence_impl(rules::RuleRef rule, ParseResult initial) {
    using namespace rules;

    TESL_FAIL_COND(!rule.is_valid(), return {});

    auto pattern_size = rule.get_pattern().size();
    Parser::ParseSequence parse_sequence;
    parse_sequence.push_back(MOV(initial));

    for (IntT i = 1; i < pattern_size;) {
      ElementRef e = rule.get_pattern()[i];
      switch (e->kind) {
        case rules::PatternElement::TOKEN: {
          if (e->token != current_token) {
            parser_error("expected {}, got {}!", e, current_token);
            current_token = tokenizer.next_token();
            return {};
          }
          
          parse_sequence.push_back(ParseResult{current_token});
#ifdef TESL_DEBUG_PARSER
          fmt::println("push {}", parse_sequence.back());
#endif
          current_token = tokenizer.next_token();
        } break;
        case rules::PatternElement::ABSOLUTE_PRECEDENCE_RULE:
        case rules::PatternElement::RELATIVE_PRECEDENCE_RULE: {
          parse_sequence.push_back(parse_precedence(rule.list.get_library(), e.get_matched_rule_precedence()));
        } break;
        case rules::PatternElement::OPTIONAL: {
          ElementRef b = e.choose_branch(current_token.kind);
          if (!b.is_valid()) {
            parser_error("expected {}, got {}!", e.stringify_branch_choices(), current_token);
            current_token = tokenizer.next_token();
            return {};
          }

          i = b.element_index;
          continue;
        } break;
        case rules::PatternElement::JUMP: {
          i += e->offset;
          continue;
        } break;
      }

      i++;
    }

#ifdef TESL_DEBUG_PARSER
    fmt::println("parsing {}:", rule.get_name());
    for (IntT i = 0; i < parse_sequence.size(); ++i) {
      fmt::println("  {}", parse_sequence[i]);
    }
#endif

    return rule.parse(*this, MOV(parse_sequence));
  }

  Parser::Parser(Tokenizer pTokenizer, Compiler pCompiler) : tokenizer(pTokenizer), compiler(pCompiler) {}

  void Parser::print_error_source(const Token & t) const {
    tesl::print_error_source(tokenizer.line_num, tokenizer.line_start, t.span.begin(), t.span.begin(), t.span.end());
  }

  void Parser::print_error_source() const {
    print_error_source(current_token);
  }
} // namespace tesl

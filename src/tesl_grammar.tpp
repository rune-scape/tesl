#include "tesl_builtins.hpp"
#include "tesl_grammar.hpp"

#include "tesl_symbol.hpp"

#include <array>
#include <limits>
#include <utility>

namespace tesl::grammar::tmpl {
  namespace detail {
    template<typename T, SizeT LhsSize, SizeT RhsSize, SizeT ... LhsIndices, SizeT ... RhsIndices>
    constexpr std::array<T, LhsSize + RhsSize> _concat_arrays_impl(std::array<T, LhsSize> lhs, std::array<T, RhsSize> rhs, std::index_sequence<LhsIndices...>, std::index_sequence<RhsIndices...>) {
      return {MOV(lhs[LhsIndices])..., MOV(rhs[RhsIndices])...};
    }

    template<typename T, SizeT LhsSize, SizeT RhsSize>
    constexpr std::array<T, LhsSize + RhsSize> concat_arrays(std::array<T, LhsSize> lhs, std::array<T, RhsSize> rhs) {
      return _concat_arrays_impl(lhs, rhs, std::make_index_sequence<LhsSize>(), std::make_index_sequence<RhsSize>());
    }
  }

  template<SizeT ElementCount>
  struct Pattern {
    static_assert(std::cmp_less_equal(ElementCount, std::numeric_limits<IntT>::max()));
    static constexpr SizeT element_count = ElementCount;
    std::array<PatternElement, ElementCount> elements;
  };

  template<typename ... Ts>
  consteval Pattern<sizeof...(Ts)> make_pattern(Ts ... elements) {
    static_assert(sizeof...(elements) > 0);
    return {PatternElement{elements}...};
  }

  template<SizeT ElementCount>
  struct Rule {
    static_assert(std::cmp_less_equal(ElementCount, std::numeric_limits<IntT>::max()));
    CharStrView name;
    GlobalSignatureSymbol resolve_signature;
    Pattern<ElementCount> pattern;

    consteval RuleRefStorage as_storage(SizeT elements_begin) {
      return {name, resolve_signature, {static_cast<IntT>(elements_begin), static_cast<IntT>(ElementCount)}};
    }
  };

  template<SizeT ElementCount>
  consteval Rule<ElementCount> make_rule(CharStrView name, GlobalSignatureSymbol resolve_signature, Pattern<ElementCount> && pattern) {
    return {name, resolve_signature, pattern};
  }

  template<SizeT ElementCount, SizeT RuleCount>
  struct Precedence {
    static_assert(std::cmp_less_equal(ElementCount, std::numeric_limits<IntT>::max()));
    static_assert(std::cmp_less_equal(RuleCount, std::numeric_limits<IntT>::max()));
    static constexpr SizeT element_count = ElementCount;
    static constexpr SizeT rule_count = RuleCount;
    std::array<PatternElement, ElementCount> elements;
    std::array<RuleRefStorage, RuleCount> rules;

    /*template<SizeT RuleElementCount>
    consteval void add_rule(SizeT element_index, SizeT rule_index, Rule<RuleElementCount> & rule) {
      for (SizeT i = 0; i < RuleElementCount; ++i) {
        elements[element_index + i] = rule.pattern.elements[i];
      }
      rules[rule_index] = {rule.name, rule.resolve_signature, {static_cast<IntT>(element_index), static_cast<IntT>(RuleElementCount)}};
    }

    template<SizeT RuleElementCount>
    consteval Precedence<ElementCount + RuleElementCount, RuleCount + 1> operator+(Rule<RuleElementCount> & rule) {
      Precedence<ElementCount + RuleElementCount, RuleCount + 1> result{*this, rule};
      for (SizeT i = 0; i < ElementCount; ++i) {
        result.elements[i] = elements[i];
      }
      for (SizeT i = 0; i < RuleCount; ++i) {
        result.rules[i] = rules[i];
      }
      result.add_rule(ElementCount, RuleCount, rule);
      return result;
    }*/

    consteval PrecedenceRefStorage as_storage(SizeT rules_begin) {
      return {static_cast<IntT>(rules_begin), static_cast<IntT>(RuleCount)};
    }

    template<SizeT RuleElementCount>
    consteval Precedence<ElementCount + RuleElementCount, RuleCount + 1> operator+(Rule<RuleElementCount> & rule) {
      return {*this, rule};
    }

    template<SizeT EC, SizeT RC, SizeT RuleElementCount>
    consteval Precedence(Precedence<EC, RC> & p, Rule<RuleElementCount> & r) : elements(detail::concat_arrays(p.elements, r.pattern.elements)), rules(detail::concat_arrays(p.rules, std::array<RuleRefStorage, 1>{r.as_storage(EC)})) {}
    
    template<SizeT REC>
    consteval Precedence(Rule<REC> & r) : elements(r.pattern.elements), rules{r.as_storage(0)} {}
  };

  template<>
  struct Precedence<0, 0> {
    template<SizeT RuleElementCount>
    consteval Precedence<RuleElementCount, 1> operator+(Rule<RuleElementCount> & rule) {
      return {rule};
    }
  };

  template<SizeT ... ElementCounts>
  consteval Precedence<(ElementCounts + ...), sizeof...(ElementCounts)> make_precedence(Rule<ElementCounts> && ... rules) {
    return (Precedence<0, 0>{} + ... + rules);
  }

  // woof woof :3
  // meow meow >:3
  template<SizeT ElementCount, SizeT RuleCount, SizeT PrecedenceCount>
  struct Library {
    static_assert(std::cmp_less_equal(ElementCount, std::numeric_limits<IntT>::max()));
    static_assert(std::cmp_less_equal(RuleCount, std::numeric_limits<IntT>::max()));
    static_assert(std::cmp_less_equal(PrecedenceCount, std::numeric_limits<IntT>::max()));
    static constexpr SizeT element_count = ElementCount;
    static constexpr SizeT rule_count = RuleCount;
    static constexpr SizeT precedence_count = PrecedenceCount;
    std::array<PatternElement, ElementCount> elements;
    std::array<RuleRefStorage, RuleCount> rules;
    std::array<PrecedenceRefStorage, PrecedenceCount> precedences;

    /*template<SizeT ListElementCount, SizeT ListRuleCount>
    consteval void add_rule(SizeT element_index, SizeT rule_index, Precedence<ListElementCount, ListRuleCount> & list, SizeT list_rule_index) {
      auto & rule = list.rules[list_rule_index];
      for (SizeT i = 0; i < rule.pattern.element_count; ++i) {
        elements[element_index + i] = list.elements[i];
      }
      rules[rule_index] = {rule.name, rule.resolve_signature, {static_cast<IntT>(element_index), static_cast<IntT>(rule.pattern.element_count)}};
    }

    template<SizeT ListElementCount, SizeT ListRuleCount>
    consteval void add_precedence(SizeT element_index, SizeT rule_index, SizeT precedence_index, Precedence<ListElementCount, ListRuleCount> & list) {
      for (SizeT i = 0; i < ListRuleCount; ++i) {
        add_rule(element_index, rule_index + i, list, i);
        element_index += list.rules[i].pattern.element_count;
      }
      precedences[precedence_index] = {static_cast<IntT>(rule_index), static_cast<IntT>(ListRuleCount)};
    }

    template<SizeT ListElementCount, SizeT ListRuleCount>
    consteval Library<ElementCount + ListElementCount, RuleCount + ListRuleCount, PrecedenceCount + 1> operator+(Precedence<ListElementCount, ListRuleCount> & list) {
      Library<ElementCount + ListElementCount, RuleCount + ListRuleCount, PrecedenceCount + 1> result;
      for (SizeT i = 0; i < ElementCount; ++i) {
        result.elements[i] = elements[i];
      }
      for (SizeT i = 0; i < RuleCount; ++i) {
        result.rules[i] = rules[i];
      }
      for (SizeT i = 0; i < PrecedenceCount; ++i) {
        result.precedences[i] = precedences[i];
      }
      result.add_precedence(ElementCount, RuleCount, PrecedenceCount, list);
      return result;
    }*/

    consteval void adjust_rules(SizeT elements_begin, SizeT rules_begin, SizeT rule_count) {
      for (SizeT i = 0; i < rule_count; ++i) {
        auto & pattern = rules[rules_begin + i].pattern;
        pattern.elements_begin = static_cast<IntT>(elements_begin);
        elements_begin += pattern.element_count;
      }
    }

    template<SizeT PrecedenceElementCount, SizeT PrecedenceRuleCount>
    consteval Library<ElementCount + PrecedenceElementCount, RuleCount + PrecedenceRuleCount, PrecedenceCount + 1> operator+(Precedence<PrecedenceElementCount, PrecedenceRuleCount> & p) {
      return {*this, p};
    }

    template<SizeT EC, SizeT RC, SizeT PC, SizeT PrecedenceElementCount, SizeT PrecedenceRuleCount>
    consteval Library(Library<EC, RC, PC> & l, Precedence<PrecedenceElementCount, PrecedenceRuleCount> & p) : elements(detail::concat_arrays(l.elements, p.elements)), rules(detail::concat_arrays(l.rules, p.rules)), precedences(detail::concat_arrays(l.precedences, std::array<PrecedenceRefStorage, 1>{p.as_storage(RC)})) {
      adjust_rules(EC, RC, PrecedenceRuleCount);
    }
    
    template<SizeT PrecedenceElementCount, SizeT PrecedenceRuleCount>
    consteval Library(Precedence<PrecedenceElementCount, PrecedenceRuleCount> & p) : elements(p.elements), rules(p.rules), precedences{p.as_storage(0)} {}
  };

  template<>
  struct Library<0, 0, 0> {
    template<SizeT PrecedenceElementCount, SizeT PrecedenceRuleCount>
    consteval Library<PrecedenceElementCount, PrecedenceRuleCount, 1> operator+(Precedence<PrecedenceElementCount, PrecedenceRuleCount> & p) {
      return {p};
    }
  };

  template<SizeT ... ElementCounts, SizeT ... RuleCounts>
  consteval auto make_rule_library(StrView label, Precedence<ElementCounts, RuleCounts> && ... lists) {
    return (Library<0, 0, 0>{} + ... + lists);
  }

  constexpr PatternElement _literal = PatternElement{PatternElement::LITERAL, 0};
  constexpr PatternElement _id = PatternElement{PatternElement::IDENTIFIER, 0};

  template<int8_t Offset>
  constexpr PatternElement _rel_prec_rule = PatternElement{PatternElement::RELATIVE_PRECEDENCE_RULE, Offset};
  template<int8_t Precedence>
  constexpr PatternElement _abs_prec_rule = PatternElement{PatternElement::ABSOLUTE_PRECEDENCE_RULE, Precedence};
  constexpr PatternElement _top_rule = PatternElement{PatternElement::ABSOLUTE_PRECEDENCE_RULE, -1};

  // matches an expression with same or lower precedence
  constexpr PatternElement _similar_rule = _rel_prec_rule<0>;

  // marks a block of length N starting at the next element that is optional
  template<int8_t N>
  constexpr PatternElement _opt = PatternElement{PatternElement::OPTIONAL, N};

  // jumps the element index to a relative offset N (used to make repeating patterns)
  template<int8_t N>
  constexpr PatternElement _jmp = PatternElement{PatternElement::JUMP, N};

#define RESOLVE_SIGNATURE(str) TESL_BUILTIN_SIGNATURE(str "(grammar:SyntaxStream)")

  // at least 1 token must be present in the first or second element of a pattern
  // at most 1 of each token may appear in the first slot of all patterns (also applies to the second slot separately, if the first slot is not also a token)
  // at least 1 token must be present in the 2 choices directly lead to by an 'opt<...>' element (jumps followed)
  constexpr auto expression_library_tmpl = make_rule_library("expression",
    make_precedence(
      make_rule("literal", RESOLVE_SIGNATURE("resolve_literal"), make_pattern(Token::LITERAL)),
      make_rule("identifier", RESOLVE_SIGNATURE("resolve_identifier"), make_pattern(_id))
    ),
    make_precedence(
      make_rule("grouping", RESOLVE_SIGNATURE("resolve_grouping"), make_pattern(Token::OPEN_PAREN, _top_rule, Token::CLOSE_PAREN))
    ),
    make_precedence(
      make_rule("member access", RESOLVE_SIGNATURE("resolve_member_access"), make_pattern(_similar_rule, Token::DOT, Token::IDENTIFIER)),
      make_rule("function call", RESOLVE_SIGNATURE("resolve_call"), make_pattern(_similar_rule, Token::OPEN_PAREN, _opt<5>, _top_rule, _opt<3>, Token::COMMA, _top_rule, _opt<-3>, Token::CLOSE_PAREN)),
      make_rule("subscript", RESOLVE_SIGNATURE("resolve_subscript"), make_pattern(_similar_rule, Token::OPEN_SQUARE_BRACKET, _opt<5>, _top_rule, _opt<3>, Token::COMMA, _top_rule, _opt<-3>, Token::OPEN_SQUARE_BRACKET))
    ),
    make_precedence(
      make_rule("unary plus", RESOLVE_SIGNATURE("resolve_prefix"), make_pattern(Token::PLUS, _similar_rule)),
      make_rule("unary minus", RESOLVE_SIGNATURE("resolve_prefix"), make_pattern(Token::MINUS, _similar_rule)),
      make_rule("logical not", RESOLVE_SIGNATURE("resolve_prefix"), make_pattern(Token::BANG, _similar_rule)),
      make_rule("logical not", RESOLVE_SIGNATURE("resolve_prefix"), make_pattern(Token::NOT, _similar_rule)),
      make_rule("bitwise not", RESOLVE_SIGNATURE("resolve_prefix"), make_pattern(Token::TILDE, _similar_rule))
    ),
    make_precedence(
      make_rule("multipication", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::STAR, _similar_rule)),
      make_rule("division", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::SLASH, _similar_rule)),
      make_rule("remainder", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::PERCENT, _similar_rule))
    ),
    make_precedence(
      make_rule("addition", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::PLUS, _similar_rule)),
      make_rule("subtraction", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::MINUS, _similar_rule))
    ),
    make_precedence(
      make_rule("bitwise left shift", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::LESS_LESS, _similar_rule)),
      make_rule("bitwise right shift", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::GREATER_GREATER, _similar_rule))
    ),
    make_precedence(
      make_rule("bitwise and", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::AMP, _similar_rule))
    ),
    make_precedence(
      make_rule("bitwise xor", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::CARET, _similar_rule))
    ),
    make_precedence(
      make_rule("bitwise or", RESOLVE_SIGNATURE("resolve_arithmetic"), make_pattern(_similar_rule, Token::PIPE, _similar_rule))
    ),
    make_precedence(
      make_rule("compare less than", RESOLVE_SIGNATURE("resolve_comparison"), make_pattern(_similar_rule, Token::LESS, _similar_rule)),
      make_rule("compare greater than", RESOLVE_SIGNATURE("resolve_comparison"), make_pattern(_similar_rule, Token::GREATER, _similar_rule)),
      make_rule("compare less than or equal to", RESOLVE_SIGNATURE("resolve_comparison"), make_pattern(_similar_rule, Token::LESS_EQUAL, _similar_rule)),
      make_rule("compare greater than or equal to", RESOLVE_SIGNATURE("resolve_comparison"), make_pattern(_similar_rule, Token::GREATER_EQUAL, _similar_rule))
    ),
    make_precedence(
      make_rule("compare equal", RESOLVE_SIGNATURE("resolve_comparison"), make_pattern(_similar_rule, Token::EQUAL_EQUAL, _similar_rule)),
      make_rule("compare not equal", RESOLVE_SIGNATURE("resolve_comparison"), make_pattern(_similar_rule, Token::BANG_EQUAL, _similar_rule))
    ),
    make_precedence(
      make_rule("logical and", RESOLVE_SIGNATURE("resolve_bool_arithmetic"), make_pattern(_similar_rule, Token::AMP_AMP, _similar_rule)),
      make_rule("logical and", RESOLVE_SIGNATURE("resolve_bool_arithmetic"), make_pattern(_similar_rule, Token::AND, _similar_rule))
    ),
    make_precedence(
      make_rule("logical or", RESOLVE_SIGNATURE("resolve_bool_arithmetic"), make_pattern(_similar_rule, Token::PIPE_PIPE, _similar_rule)),
      make_rule("logical or", RESOLVE_SIGNATURE("resolve_bool_arithmetic"), make_pattern(_similar_rule, Token::OR, _similar_rule))
    ),
    make_precedence(
      make_rule("assign", RESOLVE_SIGNATURE("resolve_assignment"), make_pattern(_similar_rule, Token::EQUAL, _similar_rule)),
      make_rule("add assign", RESOLVE_SIGNATURE("resolve_assignment"), make_pattern(_similar_rule, Token::PLUS_EQUAL, _similar_rule)),
      make_rule("subtract assign", RESOLVE_SIGNATURE("resolve_assignment"), make_pattern(_similar_rule, Token::MINUS_EQUAL, _similar_rule)),
      make_rule("multiply assign", RESOLVE_SIGNATURE("resolve_assignment"), make_pattern(_similar_rule, Token::STAR_EQUAL, _similar_rule)),
      make_rule("divide assign", RESOLVE_SIGNATURE("resolve_assignment"), make_pattern(_similar_rule, Token::SLASH_EQUAL, _similar_rule)),
      make_rule("remainder assign", RESOLVE_SIGNATURE("resolve_assignment"), make_pattern(_similar_rule, Token::PERCENT_EQUAL, _similar_rule))
    )
  );

#undef RESOLVE_FN_SYM

}

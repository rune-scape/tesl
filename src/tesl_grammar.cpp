#include "tesl_grammar.hpp"
//#include "tesl_grammar.tpp"
#include "tesl_builtins.hpp"
#include "tesl_common.hpp"
#include "tesl_member_function.hpp"
#include <memory>

namespace tesl {
  namespace grammar{
    SyntaxNode & SyntaxNode::operator=(const SyntaxNode & other) {
      if (kind == other.kind) {
        switch (kind) {
          case INVALID:
            break;
          case TOKEN:
            token = other.token;
            break;
          case LITERAL:
            literal = other.literal;
            break;
          case IDENTIFIER:
            identifier = other.identifier;
            break;
          case EXPRESSION:
            expression = other.expression;
            break;
        }
      } else {
        std::destroy_at(this);
        std::construct_at(this, other);
      }
      return *this;
    }

    SyntaxNode & SyntaxNode::operator=(SyntaxNode && other) {
      if (kind == other.kind) {
        switch (kind) {
          case INVALID:
            break;
          case TOKEN:
            token = MOV(other.token);
            break;
          case LITERAL:
            literal = MOV(other.literal);
            break;
          case IDENTIFIER:
            identifier = MOV(other.identifier);
            break;
          case EXPRESSION:
            expression = MOV(other.expression);
            break;
        }
      } else {
        std::destroy_at(this);
        std::construct_at(this, MOV(other));
      }
      return *this;
    }

    SyntaxNode::SyntaxNode(const SyntaxNode & other) {
      kind = other.kind;
      switch (other.kind) {
        case INVALID:
          break;
        case TOKEN:
          std::construct_at(&token, other.token);
          break;
        case LITERAL:
          std::construct_at(&literal, other.literal);
          break;
        case IDENTIFIER:
          std::construct_at(&identifier, other.identifier);
          break;
        case EXPRESSION:
          std::construct_at(&expression, other.expression);
          break;
      }
    }

    SyntaxNode::SyntaxNode(SyntaxNode && other) {
      kind = other.kind;
      switch (other.kind) {
        case INVALID:
          break;
        case TOKEN:
          std::construct_at(&token, MOV(other.token));
          break;
        case LITERAL:
          std::construct_at(&literal, MOV(other.literal));
          break;
        case IDENTIFIER:
          std::construct_at(&identifier, MOV(other.identifier));
          break;
        case EXPRESSION:
          std::construct_at(&expression, MOV(other.expression));
          break;
      }
    }

    SyntaxNode::~SyntaxNode() {
      Kind old_kind = kind;
      kind = INVALID;
      switch (old_kind) {
        case INVALID:
          break;
        case TOKEN:
          std::destroy_at(&token);
          break;
        case LITERAL:
          std::destroy_at(&literal);
          break;
        case IDENTIFIER:
          std::destroy_at(&identifier);
          break;
        case EXPRESSION:
          std::destroy_at(&expression);
          break;
      }
    }

    SyntaxNode::SyntaxNode() {}

    SyntaxNode SyntaxNode::make_token(Token::Kind t) { return {t}; }
    SyntaxNode SyntaxNode::make_literal(Variant v) { return {v}; }
    SyntaxNode SyntaxNode::make_identifier(Name id) { return {id}; }
    SyntaxNode SyntaxNode::make_expresion(ExpressionNode e) { return {e}; }

    SyntaxNode::SyntaxNode(Token::Kind t) : kind(TOKEN), token(MOV(t)) {}
    SyntaxNode::SyntaxNode(Variant v) : kind(LITERAL), literal(MOV(v)) {}
    SyntaxNode::SyntaxNode(Name id) : kind(IDENTIFIER), identifier(MOV(id)) {}
    SyntaxNode::SyntaxNode(ExpressionNode e) : kind(EXPRESSION), expression(MOV(e)) {}

  #define grammar_error(...) \
    do { \
      has_error = true; \
      TESL_ERROR_PRINT_MSG_BASE("grammar error", __VA_ARGS__); \
    } while (false)

  #ifdef TESL_DEBUG_GRAMMAR
  #define grammar_debug(...) \
    do { \
      TESL_ERROR_PRINT_MSG_BASE("grammar debug", __VA_ARGS__); \
    } while (false)
  #else
  #define grammar_debug(...) TESL_EMPTY_STATEMENT
  #endif

    SyntaxStream RuleLibrary::parse_precedence(SyntaxStream syntax, Variant resolver, IntT precedence) {
      if (precedence < 0) {
        precedence += precedence_count;
      }

      return _parse_precedence_impl(syntax, resolver, precedence);
    }

    SyntaxStream RuleLibrary::_parse_precedence_impl(SyntaxStream & syntax, Variant & resolver, IntT precedence) {
      if (precedence < 0 || precedence >= precedence_count) {
        grammar_error("invalid precedence '{}'", precedence);
      }

      RuleRef rule;
      {
        auto current_node = syntax.current();
        if (!current_node.has_value()) {
          co_return;
        }

        rule = find_rule_precedence_first(current_node.unwrap(), precedence);
        if (!rule.is_valid()) {
          grammar_error("expected {}, got {}!", PrecedenceRef{this, precedence}, current_node.unwrap());
          syntax.next();
          co_return;
        }
      }

      do {
        SyntaxStream resolved_rule = _parse_precedence_rule_impl(syntax, resolver, rule);
        std::aligned_storage_t<sizeof(SyntaxStream), alignof(SyntaxStream)> resolve_result_storage;
        auto result = resolver.call_method(rule._get_storage().resolve_signature, &resolved_rule, &resolve_result_storage);
        if (result.is_err()) {
          grammar_error("{}", result.unwrap_err());
          co_return;
        }

        SyntaxStream & resolved_result = *reinterpret_cast<SyntaxStream *>(&resolve_result_storage);

        for (auto & n : resolved_result) {
          co_yield MOV(n);
        }

        auto current_node = syntax.next();
        if (!current_node.has_value()) {
          co_return;
        }

        rule = find_rule_precedence_second(rule, current_node.unwrap(), precedence);
      } while (rule.is_valid());
    }

    SyntaxStream RuleLibrary::_parse_precedence_rule_impl(SyntaxStream & syntax, Variant & resolver, RuleRef & rule) {
      TESL_ASSERT(rule.is_valid());

      {
        auto current_node = syntax.current();
        if (!current_node.has_value()) {
          co_return;
        }

        co_yield MOV(current_node.unwrap());
      }

      auto pattern = rule.get_pattern();
      auto pattern_storage = pattern._get_storage();
      auto elements = pattern._get_elements();
      auto pattern_size = rule.get_pattern().size();
      for (IntT i = 1; i < pattern_size;) {
        auto & e_storage = elements[i];
        switch (e_storage.kind) {
          case PatternElement::TOKEN:
          case PatternElement::LITERAL:
          case PatternElement::IDENTIFIER: {
            auto current_node = syntax.next();
            if (!current_node.has_value()) {
              grammar_error("unexpected end of input");
              co_return;
            }
            ElementRef e{pattern, i};
            if (!e.matches_syntax(current_node.unwrap())) {
              grammar_error("expected {}, got {}", e, current_node.unwrap());
              syntax.next();
              co_return;
            }

            co_yield MOV(current_node.unwrap());
          } break;
          case PatternElement::ABSOLUTE_PRECEDENCE_RULE:
          case PatternElement::RELATIVE_PRECEDENCE_RULE: {
            auto current_node = syntax.next();
            if (!current_node.has_value()) {
              grammar_error("unexpected end of input");
              co_return;
            }
            ElementRef e{pattern, i};
            for (auto & n : _parse_precedence_impl(syntax, resolver, e.get_matched_rule_precedence())) {
              co_yield MOV(n);
            }
          } break;
          case PatternElement::OPTIONAL: {
            auto current_node = syntax.next();
            if (!current_node.has_value()) {
              grammar_error("unexpected end of input");
              co_return;
            }
            ElementRef e{pattern, i};
            ElementRef branch_match = e.choose_branch(current_node.unwrap());
            if (!branch_match.is_valid()) {
              grammar_error("expected {}, got {}", e.get_branch_choices(), current_node.unwrap());
              if (syntax.done()) {
                co_return;
              }
              syntax.next();
              co_return;
            }

            i = branch_match.element_index;
            continue;
          } break;
          case PatternElement::JUMP: {
            i += e_storage.offset;
            continue;
          } break;
        }

        i++;
      }
    }

    RuleRef RuleLibrary::find_rule_precedence_first(const SyntaxNode & node, IntT precedence) const {
      if (precedence < 0) {
        precedence += precedence_count;
      }

      for (IntT p = precedence; p >= 0; --p) {
        PrecedenceRef pattern_list = operator[](p);
        for (IntT i = 0; i < pattern_list.size(); ++i) {
          RuleRef rule = pattern_list[i];
          if (rule.get_pattern()[0].matches_syntax(node)) {
            return rule;
          }
        }
      }

      return {};
    }

    RuleRef RuleLibrary::find_rule_precedence_second(const RuleRef & rule, const SyntaxNode & node, IntT precedence) const {
      if (precedence < 0) {
        precedence += precedence_count;
      }

      for (IntT p = precedence; p >= 0; --p) {
        PrecedenceRef pattern_list = operator[](p);
        for (IntT i = 0; i < pattern_list.size(); ++i) {
          RuleRef rule = pattern_list[i];
          if (rule.get_pattern().size() < 2) {
            continue;
          }
          if (rule.get_pattern()[0].matches_rule(rule) && rule.get_pattern()[1].matches_syntax(node)) {
            return rule;
          }
        }
      }

      return {};
    }

    PrecedenceRef RuleLibrary::operator[](IntT p) const {
      TESL_ASSERT(_precedences != nullptr);
      TESL_ASSERT(p < precedence_count);
      return {this, p};
    }

    const PrecedenceRefStorage & PrecedenceRef::_get_storage() const {
      TESL_ASSERT(_library != nullptr);
      TESL_ASSERT(_precedence_index < _library->precedence_count);
      return _library->_precedences[_precedence_index];
    }

    const RuleRefStorage * PrecedenceRef::_get_rules() const {
      TESL_ASSERT(_library != nullptr);
      return &_library->_rules[_get_storage().rules_begin];
    }

    bool PrecedenceRef::is_valid() const {
      return _library != nullptr && _precedence_index < _library->precedence_count && _library->_precedences[_precedence_index].is_valid();
    }

    IntT PrecedenceRef::size() const {
      return _get_storage().rule_count;
    }

    const RuleLibrary & PrecedenceRef::get_library() const {
      TESL_ASSERT(_library != nullptr);
      return *_library;
    }

    bool PrecedenceRef::operator==(const PrecedenceRef &other) const {
      return _library == other._library && _precedence_index == other._precedence_index;
    }

    RuleRef PrecedenceRef::operator[](IntT i) const {
      return {*this, i};
    }

    PrecedenceRef::PrecedenceRef(const RuleLibrary * l, IntT p) : _library(l), _precedence_index(p) {
      TESL_ASSERT(_library == nullptr || p >= 0 && p < _library->precedence_count);
    }

    PrecedenceRef::PrecedenceRef() = default;
    
    const RuleRefStorage & RuleRef::_get_storage() const {
      TESL_ASSERT(_index >= 0);
      return precedence._get_rules()[_index];
    }

    bool RuleRef::is_valid() const {
      return precedence.is_valid() && _get_storage().is_valid();
    }

    bool RuleRef::operator==(const RuleRef & other) const {
      return precedence == other.precedence && _index == other._index;
    }

    RuleRef::RuleRef(const PrecedenceRef & p, IntT i) : precedence(p), _index(i) {
      TESL_ASSERT(i >= 0 && i < p.size());
    }

    RuleRef::RuleRef() = default;

    CharStrView RuleRef::get_name() const {
      return _get_storage().name;
    }

    PatternRef RuleRef::get_pattern() const {
      return {*this};
    }

    const PatternRefStorage & PatternRef::_get_storage() const {
      return rule._get_storage().pattern;
    }

    const PatternElement * PatternRef::_get_elements() const {
      return &rule.precedence.get_library()._elements[_get_storage().elements_begin];
    }

    bool PatternRef::is_valid() const {
      return rule.is_valid() && _get_storage().is_valid();
    }

    IntT PatternRef::size() const {
      return _get_storage().element_count;
    }

    bool PatternRef::operator==(const PatternRef & other) const {
      return rule == other.rule;
    }

    ElementRef PatternRef::operator[](IntT i) const {
      return {*this, i};
    }

    PatternRef::PatternRef(const RuleRef & r) : rule(r) {}
    PatternRef::PatternRef() = default;

    const PatternElement & ElementRef::_get_storage() const {
      TESL_ASSERT(element_index >= 0);
      return pattern._get_elements()[element_index];
    }

    PatternElement::Kind ElementRef::get_kind() const {
      return _get_storage().kind;
    }

    bool ElementRef::is_valid() const {
      return pattern.is_valid() && element_index >= 0;
    }

    bool ElementRef::is_token() const {
      const PatternElement & e = _get_storage();
      return e.kind == PatternElement::TOKEN;
    }

    bool ElementRef::is_literal() const {
      const PatternElement & e = _get_storage();
      return e.kind == PatternElement::LITERAL;
    }

    bool ElementRef::is_identifier() const {
      const PatternElement & e = _get_storage();
      return e.kind == PatternElement::IDENTIFIER;
    }

    bool ElementRef::is_rule() const {
      const PatternElement & e = _get_storage();
      return e.kind == PatternElement::ABSOLUTE_PRECEDENCE_RULE || e.kind == PatternElement::RELATIVE_PRECEDENCE_RULE;
    }

    bool ElementRef::is_opt() const {
      const PatternElement & e = _get_storage();
      return e.kind == PatternElement::OPTIONAL;
    }

    bool ElementRef::is_jmp() const {
      const PatternElement & e = _get_storage();
      return e.kind == PatternElement::JUMP;
    }

    bool ElementRef::is_end() const {
      return pattern.is_valid() && element_index == pattern.size();
    }

    bool ElementRef::matches_rule(const RuleRef & r) const {
      const PatternElement & e = _get_storage();
      switch (e.kind) {
        case PatternElement::ABSOLUTE_PRECEDENCE_RULE:
          return r.precedence._precedence_index <= e.offset;
        case PatternElement::RELATIVE_PRECEDENCE_RULE:
          return r.precedence._precedence_index <= (pattern.rule.precedence._precedence_index + e.offset);
        case PatternElement::TOKEN:
        case PatternElement::LITERAL:
        case PatternElement::IDENTIFIER:
        case PatternElement::OPTIONAL:
        case PatternElement::JUMP:
          return false;
      }

      TESL_UNREACHABLE;
    }

    bool ElementRef::matches_token(Token::Kind t) const {
      const PatternElement & e = _get_storage();
      return e.kind == PatternElement::TOKEN && e.token == t;
    }

    bool ElementRef::matches_syntax(const SyntaxNode & node) const {
      switch (node.kind) {
        case SyntaxNode::INVALID:
          return false;
        case SyntaxNode::TOKEN:
          return matches_token(node.token);
        case SyntaxNode::LITERAL:
          return is_literal();
        case SyntaxNode::IDENTIFIER:
          return is_identifier();
        case SyntaxNode::EXPRESSION:
          return is_rule();
      }

      TESL_UNREACHABLE;
    }

    IntT ElementRef::get_matched_rule_precedence() const {
      const RuleLibrary & library = pattern.rule.precedence.get_library();
      const PatternElement & e = _get_storage();

      if (e.kind == PatternElement::ABSOLUTE_PRECEDENCE_RULE) {
        IntT p = e.offset;
        if (p < 0) {
          p += library.precedence_count;
        }
        return p;
      } else if (e.kind == PatternElement::RELATIVE_PRECEDENCE_RULE) {
        return pattern.rule.precedence._precedence_index + e.offset;
      }

      return -1;
    }

    ElementRef ElementRef::follow() const {
      TESL_ASSERT(element_index >= 0);

      auto * elements = pattern._get_elements();
      IntT loop_count = 0;
      IntT i = element_index;
      const PatternElement * e = &elements[i];
      while (e && e->kind == PatternElement::JUMP) {
        TESL_ASSERT(loop_count < 256);
        TESL_ASSERT(e->offset != 0);
        i = i + e->offset;
        e = &elements[i];
        loop_count++;
      }

      return {pattern, i};
    }

    BranchChoices ElementRef::get_branch_choices() const {
      TESL_ASSERT(is_valid());
      auto & library = pattern.rule.precedence.get_library();

      ElementRef match = *this;
      IntT loops = 0;
      while (match.is_opt()) {
        ElementRef a = ElementRef{match.pattern, match.element_index + 1}.follow();
        ElementRef b = ElementRef{match.pattern, match.element_index + match._get_storage().offset + 1}.follow();

        auto & a_storage = a._get_storage();
        auto & b_storage = b._get_storage();

        if (a_storage.kind == PatternElement::TOKEN) {
          TESL_FAIL_COND_MSG_BASE("grammar error", !a.is_valid(), co_return, "visited invalid node while enumerating branch choices");
          co_yield a;
          match = b;
        } else if (b_storage.kind == PatternElement::TOKEN) {
          TESL_FAIL_COND_MSG_BASE("grammar error", !b.is_valid(), co_return, "visited invalid node while enumerating branch choices");
          co_yield b;
          match = a;
        } else {
          TESL_FAIL_MSG_BASE("grammar error", co_return, "invalid opt: no token to match ({} '{}' at element {})", library.get_label(), match.pattern.rule.get_name(), match.element_index);
          break;
        }

        TESL_FAIL_COND_MSG_BASE("grammar error", loops >= 127, co_return, "looped too many times while enumerating branch choices");
        TESL_FAIL_COND_MSG_BASE("grammar error", match == *this, co_return, "revisited root node while enumerating branch choices");
        TESL_FAIL_COND_MSG_BASE("grammar error", !match.is_valid(), co_return, "visited invalid node while enumerating branch choices");
        loops++;
      }

      co_yield match;
    }

    ElementRef ElementRef::choose_branch(const SyntaxNode & node) const {
      grammar_debug("choosing between branches: {}", get_branch_choices());

      for (auto & e : get_branch_choices()) {
        if (e.matches_syntax(node) || e.is_rule()) {
          grammar_debug("matched {}", e);
          return e;
        }
      }

      grammar_debug("no match");
      return {};
    }

    bool ElementRef::operator==(const ElementRef & other) const {
      return pattern == other.pattern && element_index == other.element_index;
    }

    ElementRef::ElementRef(const PatternRef & p, IntT i) : pattern(p), element_index(i) {
      TESL_ASSERT(i >= 0 && i < pattern.size());
    }

    ElementRef::ElementRef() = default;
  }

  template<>
  void bind_type_info<grammar::SyntaxNode>(TypeInfo & type) {
    // todo: finish
  }

  template<>
  void bind_type_info<grammar::SyntaxStream>(TypeInfo & type) {
    // todo: finish
  }
}
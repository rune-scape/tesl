#pragma once

#include "tesl_common.hpp"

#include "tesl_coroutine.hpp"
#include "tesl_fmt.hpp"
#include "tesl_tokenizer.hpp"
#include "tesl_symbol.hpp"
#include "tesl_name.hpp"
#include "tesl_var.hpp"
#include "tesl_type.hpp"

namespace tesl {
  namespace grammar {
    struct ExpressionNode {
      Type type = get_type_of<Null>();
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
    struct SyntaxNode {
      enum Kind {
        INVALID,
        TOKEN,
        LITERAL,
        IDENTIFIER,
        EXPRESSION,
      };

      Kind kind = INVALID;

      union {
        char _uninitialized = 0;
        // todo: make token interface generic
        Token::Kind token;
        Variant literal;
        Name identifier;
        ExpressionNode expression;
      };

      SyntaxNode & operator=(const SyntaxNode & other);
      SyntaxNode & operator=(SyntaxNode && other);

      SyntaxNode(const SyntaxNode & other);
      SyntaxNode(SyntaxNode && other);

      SyntaxNode();
      ~SyntaxNode();

      static SyntaxNode make_token(Token::Kind t);
      static SyntaxNode make_literal(Variant v);
      static SyntaxNode make_identifier(Name id);
      static SyntaxNode make_expresion(ExpressionNode e);

    protected:
      SyntaxNode(Token::Kind t);
      SyntaxNode(Variant v);
      SyntaxNode(Name id);
      SyntaxNode(ExpressionNode e);
    };

    struct SyntaxStream : coroutine::Generator<SyntaxNode> {
      SyntaxStream(coroutine::Generator<SyntaxNode> && gen) : coroutine::Generator<SyntaxNode>(MOV(gen)) {}
      SyntaxStream() = default;
    };

    struct PatternElement {
      enum Kind : char {
        TOKEN,
        LITERAL,
        IDENTIFIER,
        ABSOLUTE_PRECEDENCE_RULE,
        RELATIVE_PRECEDENCE_RULE,
        OPTIONAL,
        JUMP,
      };
    
      Kind kind;

      union {
        std::int8_t offset;
        // todo: make token interface generic
        Token::Kind token;
      };

      //constexpr PatternElement(const PatternElement &) = default;
      constexpr PatternElement(Token::Kind t) : kind(TOKEN), token(t) {}
      constexpr PatternElement(Kind k, std::int8_t v) : kind(k), offset(v) {}
      constexpr PatternElement() = delete;
    };

    struct PatternRefStorage {
      IntT elements_begin = -1;
      IntT element_count = 0;

      constexpr bool is_valid() const {
        return elements_begin >= 0;
      }
    };

    using ResolveFn = FnPtrBare;

    struct RuleRefStorage {
      CharStrView name;
      GlobalSignatureSymbol resolve_signature;
      PatternRefStorage pattern;

      constexpr bool is_valid() const {
        return resolve_signature.is_valid() && pattern.is_valid();
      }
    };

    struct PrecedenceRefStorage {
      IntT rules_begin = -1;
      IntT rule_count = 0;

      constexpr bool is_valid() const {
        return rules_begin >= 0;
      }
    };

    struct ElementRef;
    struct PatternRef;
    struct RuleRef;
    struct PrecedenceRef;

    struct RuleLibrary {
      StrView _label;
      Type _resolver_type;
      Type token_type;
      const PatternElement * _elements = nullptr;
      const RuleRefStorage * _rules = nullptr;
      const PrecedenceRefStorage * _precedences = nullptr;
      IntT precedence_count = 0;
      bool has_error = false;

      StrView get_label() const { return _label; }
      IntT max_precedence() const { return precedence_count - 1; }

      SyntaxStream parse_precedence(SyntaxStream syntax, Variant resolver, IntT precedence);
      SyntaxStream _parse_precedence_impl(SyntaxStream & syntax, Variant & resolver, IntT precedence);
      SyntaxStream _parse_precedence_rule_impl(SyntaxStream & syntax, Variant & resolver, RuleRef & rule);
      RuleRef find_rule_precedence_first(const SyntaxNode & node, IntT precedence) const;
      RuleRef find_rule_precedence_second(const RuleRef & rule, const SyntaxNode & node, IntT precedence) const;

      PrecedenceRef operator[](IntT p) const;
      RuleLibrary & operator=(const RuleLibrary &) = delete;
      RuleLibrary & operator=(RuleLibrary &&) = delete;
      RuleLibrary(const RuleLibrary &) = delete;
      RuleLibrary(RuleLibrary &&) = delete;

      template<typename T>
      constexpr RuleLibrary(const T & library) : _label(library.label), _precedences(library.lists), precedence_count(library.size) {}
    };

    struct PrecedenceRef {
      const RuleLibrary * _library = nullptr;
      IntT _precedence_index = -1;

      const PrecedenceRefStorage & _get_storage() const;
      const RuleRefStorage * _get_rules() const;
      bool is_valid() const;
      IntT size() const;
      const RuleLibrary & get_library() const;
      bool operator==(const PrecedenceRef &other) const;
      RuleRef operator[](IntT i) const;

      PrecedenceRef(const RuleLibrary * l, IntT p);
      PrecedenceRef();
    };

    struct RuleRef {
      PrecedenceRef precedence;
      IntT _index = -1;

      const RuleRefStorage & _get_storage() const;
      bool is_valid() const;
      CharStrView get_name() const;
      PatternRef get_pattern() const;
      bool operator==(const RuleRef & other) const;

      RuleRef(const PrecedenceRef & p, IntT i);
      RuleRef();
    };

    struct PatternRef {
      RuleRef rule;

      const PatternRefStorage & _get_storage() const;
      const PatternElement * _get_elements() const;
      bool is_valid() const;
      IntT size() const;
      bool operator==(const PatternRef & other) const;
      ElementRef operator[](IntT i) const;

      PatternRef(const RuleRef & r);
      PatternRef();
    };

    struct BranchChoices;
    
    struct ElementRef {
      PatternRef pattern;
      IntT element_index = -1;

      const PatternElement & _get_storage() const;
      PatternElement::Kind get_kind() const;
      bool is_valid() const;
      bool is_token() const;
      bool is_literal() const;
      bool is_identifier() const;
      bool is_rule() const;
      bool is_opt() const;
      bool is_jmp() const;
      bool is_end() const;
      bool matches_rule(const RuleRef & r) const;
      bool matches_token(Token::Kind t) const;
      bool matches_syntax(const SyntaxNode & node) const;
      IntT get_matched_rule_precedence() const;
      ElementRef follow() const;
      BranchChoices get_branch_choices() const;
      ElementRef choose_branch(const SyntaxNode & node) const;
      bool operator==(const ElementRef & other) const;

      ElementRef(const PatternRef & p, IntT i);
      ElementRef();
    };

    struct BranchChoices : coroutine::Generator<ElementRef> {
      BranchChoices(coroutine::Generator<ElementRef> && gen) : coroutine::Generator<ElementRef>(MOV(gen)) {}
      BranchChoices() = default;
    };
  }

  template<>
  void bind_type_info<grammar::SyntaxNode>(TypeInfo & type);

  template<>
  void bind_type_info<grammar::SyntaxStream>(TypeInfo & type);
}

template<>
class fmt::formatter<tesl::grammar::SyntaxNode, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::grammar::SyntaxNode & n, Context & ctx) const {
    using namespace tesl;
    using namespace grammar;

    switch (n.kind) {
      case SyntaxNode::INVALID:
        return format_to(ctx.out(), "<empty-syntax-node>");
      case SyntaxNode::TOKEN:
        return format_to(ctx.out(), "token: {:?}", n.token);
      case SyntaxNode::LITERAL:
        return format_to(ctx.out(), "literal: {}", n.literal);
      case SyntaxNode::IDENTIFIER:
        return format_to(ctx.out(), "identifier: {}", n.identifier);
      case SyntaxNode::EXPRESSION:
        return format_to(ctx.out(), "expression: {}", n.expression.type);
    }

    TESL_UNREACHABLE;
  }
};

template<>
class fmt::formatter<tesl::grammar::PrecedenceRef, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::grammar::PrecedenceRef & p, Context & ctx) const {
    using namespace tesl;
    using namespace grammar;

    auto & library = p.get_library();
    if (p._precedence_index == library.max_precedence()) {
      return format_to(ctx.out(), "{}", library.get_label());
    } else {
      return format_to(ctx.out(), "{}@p{}", library.get_label(), p._precedence_index);
    }
  }
};

template<>
class fmt::formatter<tesl::grammar::ElementRef, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> constexpr auto format(const tesl::grammar::ElementRef & e, Context & ctx) const {
    using namespace tesl;
    using namespace grammar;

    if (!e.is_valid()) {
      return format_to(ctx.out(), "<invalid-pattern-element>");
    }

    if (e.is_end()) {
      return format_to(ctx.out(), "<pattern-end>");
    }

    auto & e_storage = e._get_storage();
    auto & library = e.pattern.rule.precedence.get_library();

    switch (e_storage.kind) {
      case PatternElement::TOKEN:
        return format_to(ctx.out(), "{}", e);
      case PatternElement::LITERAL:
        return format_to(ctx.out(), "literal");
      case PatternElement::IDENTIFIER:
        return format_to(ctx.out(), "identifier");
      case PatternElement::ABSOLUTE_PRECEDENCE_RULE:
      case PatternElement::RELATIVE_PRECEDENCE_RULE:
        return format_to(ctx.out(), "{}", PrecedenceRef{e.pattern.rule.precedence._library, e.get_matched_rule_precedence()});
      case PatternElement::OPTIONAL:
        return format_to(ctx.out(), "<opt{:+d}>", e_storage.offset);
      case PatternElement::JUMP:
        return format_to(ctx.out(), "<jmp{:+d}>", e_storage.offset);
    }

    TESL_UNREACHABLE;
  }
};

template<>
class fmt::formatter<tesl::grammar::BranchChoices, tesl::CommonCharT> {
public:
  template<typename Context> constexpr auto parse(Context & ctx) const { return ctx.begin(); }
  template<typename Context> auto format(const tesl::grammar::BranchChoices & choices, Context & ctx) const {
    using namespace tesl;
    using namespace grammar;

    bool is_first = true;
    auto out = ctx.out();
    for (auto & e : choices) {
      if (!is_first) {
        out = format_to(out, " or ");
      } else {
        is_first = false;
      }
      out = format_to(out, "{}", e);
    }

    return out;
  }
};

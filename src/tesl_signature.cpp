#include "tesl_signature.hpp"

#include "tesl_fmt.hpp"
#include "tesl_env.hpp"
#include "tesl_symbol.hpp"
#include "tesl_tokenizer.hpp"

// hi lol im lala la LAAAAA
namespace tesl {
  static Result<Null, Str> parse_type_list(Array<GlobalTypeSymbol> & types, Tokenizer & tokenizer) {
    Token & id_token = tokenizer.current();
    for (; id_token == Token::IDENTIFIER; id_token = tokenizer.next()) {
      Str id_str{id_token.span};
      for (auto & sep_token = tokenizer.next(); sep_token == Token::COLON; sep_token = tokenizer.next()) {
        id_str += ":";
        id_token = tokenizer.next();
        if (id_token != Token::IDENTIFIER) {
          return format("invalid signature: expected identifier, got {}", id_token);
        }
        id_str += id_token.span;
      }

      // todo: improve this
      auto & type_map = Env::current->type_map;
      auto it = type_map.find(id_str);
      if (it == type_map.end()) {
        return format("could not find type '{}'", id_str);
      }
      types.push_back(it->second);

      if (tokenizer.current() != Token::COMMA) {
        break;
      }
    }
    return Null{};
  }

  Result<SignatureData, Str> SignatureData::parse(StrView str) {
    // todo: should be able to take non null terminated strings
    Kind kind;
    GlobalNameSymbol name;
    Array<GlobalTypeSymbol> parameter_types;
    Tokenizer tokenizer{str};
    tokenizer.strict_parsing = true;
    Token & t = tokenizer.next();
    switch (t.kind) {
      case Token::ARROW_RIGHT: {
        kind = SignatureData::COERCE;
      } break;
      case Token::IDENTIFIER: {
        kind = SignatureData::CALL;
        name = Env::current->add_name(t.span);
        t = tokenizer.next();
        if (t.kind != Token::OPEN_PAREN) {
          return format("invalid signature: expected '(', got {}", t);
        }
        t = tokenizer.next();
        auto ptypes = parse_type_list(parameter_types, tokenizer);
        if (ptypes.is_err()) {
          return MOV(ptypes.unwrap_err());
        }
        t = tokenizer.current();
        if (t.kind != Token::CLOSE_PAREN) {
          return format("invalid signature: expected ')', got {}", t);
        }
      } break;
      default:
        return format("invalid signature '{}'", str);
    }

    // todo: finish

    return SignatureData{kind, name, MOV(parameter_types), CharStrView{str}};
  }

  bool SignatureData::operator==(const SignatureData & other) {
    return hash == other.hash && kind == other.kind && name_symbol == other.name_symbol && parameter_types == other.parameter_types;
  }

  SignatureData::SignatureData(Kind p_kind, GlobalNameSymbol p_name_symbol, Array<GlobalTypeSymbol> p_parameter_type, CharStrView p_str)
    : kind(p_kind), name_symbol(p_name_symbol), parameter_types(p_parameter_type), str(p_str), hash(tesl::hash(p_str)) { }

  SignatureData::~SignatureData() = default;

  SignatureData & Signature::get_data() const {
    return Env::current->get_signature_data(symbol).unwrap();
  }

  bool Signature::is_valid() const {
    return Env::current->get_signature_data(symbol).has_value();
  }

  Signature::Signature(GlobalSignatureSymbol s) : symbol(s), _hash(Env::current->get_signature_data(s).unwrap().hash) {}
}

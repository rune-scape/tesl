#include "tesl_signature.hpp"

#include "tesl_error.hpp"
#include "tesl_tokenizer.hpp"

namespace tesl {
  static SignatureRef parse_call_signature(const Env * env, const char * str, Tokenizer & tokenizer, Signature::Kind signature_kind, Token::Kind closer) {
    SignatureRef result = new_ref<Signature>();
    result->kind = signature_kind;
    // todo: finish
    return {};
  }

  SignatureRef parse_signature(const Env * env, const char * str) {
    Tokenizer tokenizer{str};
    Token t = tokenizer.next_token();
    switch (t.kind) {
      case Token::OPEN_PAREN:
        return parse_call_signature(env, str, tokenizer, Signature::CALL, Token::CLOSE_PAREN);
      case Token::OPEN_SQUARE_BRACKET:
        return parse_call_signature(env, str, tokenizer, Signature::SUBSCRIPT, Token::CLOSE_SQUARE_BRACKET);
      default:
        TESL_FAIL_MSG(return {}, "invalid");
    }

    // todo: finish

    return {};
  }
}

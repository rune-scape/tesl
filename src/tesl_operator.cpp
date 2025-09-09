#include "tesl_operator.hpp"

namespace tesl {

  Operator Operator::make_bare(OperatorBare fn) {
    Operator ret;
    new (&ret.bare) FnPtrBare(fn.bare);
    ret.signature = fn.signature;
    ret.kind = BARE;
    return ret;
  }

  Operator Operator::make_native(OperatorNative fn) {
    Operator ret;
    new (&ret.native) FnPtr(fn.native);
    ret.signature = fn.signature;
    ret.kind = NATIVE;
    return ret;
  }

  void Operator::operator()(FnContext * context, void * args, void * ret) const {
    switch (kind) {
      case NONE:
        break;
      case BARE:
        bare(context, args, ret);
        break;
      case NATIVE:
        native(context, args, ret);
        break;
    }
  }

  Operator & Operator::operator=(const Operator & other) {
    Kind old_kind = kind;
    kind = NONE;
    signature = other.signature;
    switch (old_kind) {
      case NONE:
        break;
      case BARE:
        bare.~FnPtrBare();
        break;
      case NATIVE:
        native.~FnPtr();
        break;
    }
    switch (other.kind) {
      case NONE:
        break;
      case BARE:
        new (&bare) FnPtrBare(other.bare);
        break;
      case NATIVE:
        new (&native) FnPtr(other.native);
        break;
    }
    kind = other.kind;
    return *this;
  }

  Operator & Operator::operator=(Operator && other) {
    memswap(*this, other);
    return *this;
  }

  Operator::Operator(const Operator & other) : kind(other.kind), signature(other.signature) {
    switch (other.kind) {
      case NONE:
        break;
      case BARE:
        new (&bare) FnPtrBare(other.bare);
        break;
      case NATIVE:
        new (&native) FnPtr(other.native);
        break;
    }
  }

  Operator::Operator(Operator && other) {
    memswap(*this, other);
  }

  Operator::~Operator() {
    Kind old_kind = kind;
    kind.~Kind();
    signature.~SignatureRef();
    switch (old_kind) {
      case NONE:
        break;
      case BARE:
        bare.~FnPtrBare();
        break;
      case NATIVE:
        native.~FnPtr();
        break;
    }
  }
}
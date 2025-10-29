#include "tesl_member_function.hpp"

namespace tesl {
  Result<Null, CallError> MemberFunction::operator()(void * this_, void * args, void * ret) const {
    switch (kind) {
      case INVALID:
        return Null{};
        break;
      case BARE:
        _bare(this_, args, ret);
        break;
    }

    return Null{};
  }

  MemberFunction & MemberFunction::operator=(const MemberFunction & other) {
    this->~MemberFunction();
    std::construct_at(this, other);
    return *this;
  }

  MemberFunction & MemberFunction::operator=(MemberFunction && other) {
    if (kind == other.kind) {
      switch (other.kind) {
        case INVALID:
          break;
        case BARE:
          _bare = MOV(other._bare);
          break;
      }
    } else {
      this->~MemberFunction();
      std::construct_at(this, other);
    }
    return *this;
  }

  MemberFunction::MemberFunction(const MemberFunction & other) : kind(other.kind), return_types(other.return_types) {
    switch (other.kind) {
      case INVALID:
        break;
      case BARE:
        std::construct_at(&_bare, other._bare);
        break;
    }
  }

  MemberFunction::MemberFunction(MemberFunction && other) : kind(other.kind), return_types(other.return_types) {
    switch (other.kind) {
      case INVALID:
        break;
      case BARE:
        std::construct_at(&_bare, MOV(other._bare));
        break;
    }
  }

  MemberFunction::MemberFunction(FnPtrBare fn, ReturnTypes rts) : kind(BARE), _bare(fn), return_types(rts) { }

  MemberFunction::~MemberFunction() = default;
}
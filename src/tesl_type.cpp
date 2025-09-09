#include "tesl_type.hpp"

#include "tesl_bootstrap.hpp"
#include "tesl_fn.hpp"
#include "tesl_math.hpp"
#include "tesl_member.hpp"
#include "tesl_ref.hpp"
#include "tesl_str.hpp"

namespace tesl {
  bool TypeInfo::coerce(FnContext * context, const VarRef & v, void * ret) {
    const Operator * op = v.type->operators.get(coersion_signature);
    if (op != nullptr) {
      op->operator()(context, v.data, ret);
      return true;
    }
    return false;
  }


  TypeInfo::TypeInfo(
      StrView pLocalName,
      StrView pGlobalName,
      GlobalSymbolIndex pCoersionSignature,
      IntT pSize,
      IntT pAlign,
      FnPtrBare pInit,
      FnPtrBare pCopy,
      FnPtrBare pMove,
      FnPtrBare pDeinit)
    : local_name(pLocalName),
      global_name(pGlobalName),
      coersion_signature(pCoersionSignature),
      size(pSize),
      align(pAlign),
      _init(pInit),
      _copy(pCopy),
      _move(pMove),
      _deinit(pDeinit) {}
  TypeInfo::~TypeInfo() {}
}
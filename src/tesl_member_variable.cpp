#include "tesl_member_variable.hpp"
#include "tesl_builtins.hpp"

namespace tesl {
  MemberVariable::MemberVariable(Type t, MemberFunction get, MemberFunction set) : type(MOV(t)), getter(MOV(get)), setter(MOV(set)) {}
  MemberVariable::~MemberVariable() = default;
}
#include "tesl_name.hpp"

#include "tesl_env.hpp"

namespace tesl {
  NameData & Name::get_data() const {
    return Env::current->get_name_data(symbol).unwrap();
  }

  bool Name::is_valid() const {
    return Env::current->get_name_data(symbol).has_value();
  }

  Name::Name(GlobalNameSymbol s) : symbol(s), _hash(Env::current->get_name_data(s).unwrap().hash) {}
}
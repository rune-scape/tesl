#include "tesl_builtins.hpp"

#include "tesl_env.hpp"
#include "tesl_fmt.hpp"
#include "tesl_var.hpp"
#include "tesl_math.hpp"
#include "tesl_type.hpp"
#include "tesl_grammar.hpp"
#include "tesl_var.hpp"

namespace tesl::builtin {
  TypeInfoData type_info_data[type_count] = {

#define TESL_BUILTIN_TYPE_DEF(type, local_name, global_name) \
  { \
    TESL_BUILTIN_NAME(local_name), \
    TESL_BUILTIN_NAME(global_name), \
    sizeof(type), \
    alignof(type), \
    FnPtrBare{detail::default_init<type>}, \
    FnPtrBare{detail::default_copy_init<type>}, \
    FnPtrBare{detail::default_move_init<type>}, \
    FnPtrBare{detail::default_deinit<type>} \
  },
#include "tesl_builtin_types.inc"

  };
}

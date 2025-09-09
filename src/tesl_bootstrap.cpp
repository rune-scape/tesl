#include "tesl_bootstrap.hpp"

#include "tesl_dynamic_array.hpp"
#include "tesl_dynamic_map.hpp"
#include "tesl_fn.hpp"
#include "tesl_math.hpp"
#include "tesl_name.hpp"
#include "tesl_signature.hpp"
#include "tesl_str.hpp"
#include "tesl_tokenizer.hpp"

namespace tesl {
  namespace detail {
    SignatureRef get_basic_signature(detail::BasicSignatureIndex index) {
      static SignatureRef basic_signatures[detail::basic_signature_count] = {
        #define TESL_SYMBOL_SIGNATURE_DEF(str) parse_signature(nullptr, str),
        #include "tesl_signatures.inc"
      };
      return basic_signatures[index.value.index];
    }

    NameRef get_basic_name(detail::BasicNameIndex index) {
      static NameRef basic_names[detail::basic_name_count] = {
        #define TESL_SYMBOL_NAME_DEF(str) new_ref<Name>(str),
        #include "tesl_names.inc"
      };
      return basic_names[index.value.index];
    }
  }
}
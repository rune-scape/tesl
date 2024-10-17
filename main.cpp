#include "tesl.hpp"


int main(int argc, const char **argv) {
  te_expr *e;

  te_type user_val_type;
  te_value user_val;

  if (argc != 2) {
    te_printf("using 1 for user_val...\n");
    user_val = te_interp("0", &user_val_type);
  } else {
    user_val = te_interp(argv[1], &user_val_type);
  }

  te_variable user_input{"input", user_val_type, user_val};

  te_printf("compiling...\n");
  //if (argc > 1) {
  //  e = te_compile_internal(argv[2], &user_input, 1);
  //} else {
    //e = te_compile_internal("vec2(1.0, 1.01) % 0.7", nullptr, 0, nullptr);
    e = te_compile_internal(R"(for (int i = 0; i < input; ++i) {
  return vec3(vec2(1.01, 1.1), 1.2) * float(i);
}
)", &user_input, 1);
  //}
  te_print_expr(e);
  if (e) {
    te_printf("evaluating...\n");
    te_value result = te_eval(e);
    te_printf("result: ");
    te_print_value(te_expr_result_type(e), result);
    te_printf("\n");
    te_free(e);
  } else {
    te_printf("no parse :(");
  }
}

#include "tesl.hpp"


int main(int argc, const char **argv) {
  te_program program;

  te_typed_value user_val;

  if (argc != 2) {
    const char * exprrr = "0.9";
    te_printf("using %s for user_val...\n", exprrr);
    user_val = te_interp(exprrr);
  } else {
    user_val = te_interp(argv[1]);
  }

  te_variable user_input{"input", user_val};

  te_printf("compiling...\n");
  //if (argc > 1) {
  //  e = te_compile_internal(argv[2], &user_input, 1);
  //} else {
    //e = te_compile_internal("vec2(1.0, 1.01) % 0.7", nullptr, 0, nullptr);
    program = te_compile_suite("vec3 v = vec3(vec2(1.01, 1.1), 1.2) % input;\nreturn v;", &user_input, 1, TE_VEC3);
    //e = te_compile(R"(if (input) return vec3(vec2(1.01, 1.1), 1.2); else return vec3(vec2(1.01, 1.1), 1.2) % 0.9;)", &user_input, 1, nullptr, false);
  //}
  te_print_expr(program.root_expr);
  if (program.is_valid()) {
    te_printf("evaluating...\n");
    te_typed_value result = program.eval();
    te_printf("result: ");
    te_print_value(result);
    te_printf("\n");
  } else {
    te_printf("no parse :(");
  }
}

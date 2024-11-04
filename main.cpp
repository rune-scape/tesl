#include "tesl.hpp"

int main(int argc, const char **argv) {
  te_program program;

  te_typed_value user_val;

  if (argc != 2) {
    const char * urexprrr = "3";
    te_printf("using %s for user_val...\n", urexprrr);
    user_val = te_interp(urexprrr);
  } else {
    user_val = te_interp(argv[1]);
  }

  te_variable user_input{"input", user_val};

  //const char * prog_str = "if (input) {\n\treturn vec3(vec2(1.01, 1.1), 1.2);\n} else {\n\treturn vec3(vec2(1.01, 1.1), 1.2) % 0.9;\n}\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1), 1.2) % input;\nreturn v;\n";
  //const char * prog_str = "return \"ok\";\n";
  const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1), 1.2);\nfor (int i = input; i; --i) {\n\tv = v * 1.3;\n}\nreturn v;\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1), 1.2);\nfor (int i = 0; i < input; ++i) {\n\tv *= 1.3;\n}\nreturn v;\n";
  te_printf("compiling...\n");
  printf("\n%s\n", prog_str);
  program = te_compile_suite(prog_str, &user_input, 1, TE_VEC3);
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

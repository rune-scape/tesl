#include "tesl_tokenizer.hpp"
#include "tesl_fmt.hpp"

#include <clocale>

/*int main(int argc, const char **argv) {
  te_program program;

  TypedValueT user_val;

  if (argc != 2) {
    const char * urexprrr = "3";
    tesl_printf("using %s for user_val...\n", urexprrr);
    user_val = te_interp(urexprrr);
  } else {
    user_val = te_interp(argv[1]);
  }

  Variable user_input{"input", user_val};

  //const char * prog_str = "if (input) {\n\treturn vec3(vec2(1.01, 1.1), 1.2);\n} else {\n\treturn vec3(vec2(1.01, 1.1), 1.2) % 0.9;\n}\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1), 1.2) % input;\nreturn v;\n";
  //const char * prog_str = "return \"ok\";\n";
  const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1 0), 1.2);\nfor (int i = input; i; --i) {\n\tv = v * 1.3;\n}\nreturn v;\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1), 1.2);\nfor (int i = 0; i < input; ++i) {\n\tv *= 1.3;\n}\nreturn v;\n";
  tesl_printf("compiling...\n");
  tesl_printf("\n%s\n", prog_str);
  program = te_compile_suite(prog_str, &user_input, 1, TYPE_VEC3_VAL);
  te_print_expr(program.root_expr);
  if (program.is_valid()) {
    tesl_printf("evaluating...\n");
    TypedValueT result = program.eval();
    tesl_printf("result: ");
    te_print(result);
    tesl_printf("\n");
  } else {
    tesl_printf("no parse :(");
  }
}*/
int main(int argc, const char **argv) {
  //std::setlocale(LC_ALL, "");

  //const char * prog_str = "if (input) {\n\treturn vec3(vec2(1.01, 1.1), 1.2);\n} else {\n\treturn vec3(vec2(1.01, 1.1), 1.2) % 0.9;\n}\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1), 1.2) % input;\nreturn v;\n";
  //const char * prog_str = "# comment\r\n  return (\"ok\\z\\n\");\n";
  const char * prog_str = "\"test\"+(ifwhiledodo, \"ok\\z\\n\", \"swagever\")\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1 0), 1.2);\nfor (int i = input; i; --i) {\n\tv = v * 1.3;\n}\nreturn v;\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1), 1.2);\nfor (int i = 0; i < input; ++i) {\n\tv *= 1.3;\n}\nreturn v;\n";

  // debug tokenizer
  tesl::Tokenizer tokenizer{prog_str};
  tesl::IntT last_line = 0;
  fmt::println("--- tokenizer start ---");
  while (true) {
    tesl::Token tok = tokenizer.next_token();
    if (last_line != tokenizer.line_num) {
      if (last_line > 0) {
        fmt::println("");
      }
      last_line = tokenizer.line_num;
    } else {
      fmt::print(", ");
    }
    fmt::print("{}", tok);
    if (tok.kind == tesl::Token::LITERAL) {
      //fmt::println(" ({})", tok.literal);
    }
    if (tok.kind == tesl::Token::END) {
      fmt::println("");
      break;
    }
  }
  fmt::println("--- tokenizer end ---");

  fmt::println("--- print_source test ---");
  tesl::print_error_source(1, prog_str, &prog_str[21], &prog_str[25], &prog_str[29]);
  fmt::println("--- print_source end ---");

  /*tesl_printf("compiling...\n");
  tesl_printf("\n%s\n", prog_str);
  program = te_compile_suite(prog_str, &user_input, 1, TYPE_VEC3_VAL);
  te_print_expr(program.root_expr);
  if (program.is_valid()) {
    tesl_printf("evaluating...\n");
    TypedValueT result = program.eval();
    tesl_printf("result: ");
    tesl::print(result);
    tesl_printf("\n");
  } else {
    tesl_printf("no parse :(");
  }*/
}

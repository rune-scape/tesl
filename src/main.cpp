#include "tesl_grammar.hpp"
#include "tesl_str.hpp"
#include "tesl_tokenizer.hpp"
#include "tesl_parser.hpp"
#include "tesl_grammar.tpp"
#include "tesl_env.hpp"
#include "tesl_fmt.hpp"
#include "tesl_syntax_resolver.hpp"
#include "tesl_var.hpp"
#include <filesystem>
#include <clocale>
#include <fmt/format.h>

//#define USE_MMAP

#ifdef USE_MMAP
#include <mio/mmap.hpp>
#endif

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

/*void test_file(std::filesystem::path input_path) {
  auto file_size = std::filesystem::file_size(input_path);
  tesl::println("~ tokenize: '{}' (size={})", input_path.c_str(), file_size);
#ifdef USE_MMAP
  std::error_code error;
  auto mmap = mio::make_mmap_source(input_path.c_str(), 0, file_size, error);
  if (error) {
    tesl::println("error: could not open file {}: {}", input_path.c_str(), error.message());
    return;
  }
  tesl::CharStrView input_data{mmap.begin(), mmap.end()};
#else
  tesl::Array<char> file_data;
  file_data.resize(file_size);
  FILE * f = std::fopen(input_path.c_str(), "r");
  auto read_size = std::fread(file_data.data(), sizeof(char), file_data.size(), f);
  TESL_ASSERT(read_size == file_size);
  tesl::CharStrView input_data{file_data.data(), read_size};
#endif
  tesl::Tokenizer tokenizer{input_data};
  tesl::IntT last_line = 0;
  while (true) {
    tesl::Token tok = tokenizer.next();
    if (last_line != tokenizer.line_num) {
      if (last_line > 0) {
        tesl::println("");
      }
      last_line = tokenizer.line_num;
    } else {
      tesl::print(", ");
    }
    if (tok.kind == tesl::Token::LITERAL) {
      tesl::print("{}", tok);
    } else {
      tesl::print("{}", tok);
    }
    if (tok.kind == tesl::Token::END) {
      tesl::println("");
      break;
    }
  }
}*/

/*void test_file(std::filesystem::path input_path) {
  auto file_size = std::filesystem::file_size(input_path);
  tesl::println("~ parse: '{}' (size={})", input_path.string(), file_size);
#ifdef USE_MMAP
  std::error_code error;
  auto mmap = mio::make_mmap_source(input_path.string(), 0, file_size, error);
  if (error) {
    tesl::println("error: could not open file {}: {}", input_path.string(), error.message());
    return;
  }
  tesl::CharStrView input_data{mmap.begin(), mmap.end()};
#else
  tesl::Array<char> file_data;
  file_data.resize(file_size);
  FILE * f = std::fopen(input_path.string().c_str(), "r");
  TESL_ASSERT(f != nullptr);
  auto read_size = std::fread(file_data.data(), sizeof(char), file_data.size(), f);
  TESL_ASSERT(read_size == file_size);
  tesl::CharStrView input_data{file_data.data(), read_size};
#endif
  tesl::Parser parser{{input_data}};
  while (parser.tokenizer.current() != tesl::Token::END) {
    parser.parse_program();
  }
}*/

void test_file(std::filesystem::path input_path) {
  using namespace tesl;

  auto file_size = std::filesystem::file_size(input_path);
  println("~ parse: '{}' (size={})", input_path.string(), file_size);
#ifdef USE_MMAP
  std::error_code error;
  auto mmap = mio::make_mmap_source(input_path.string(), 0, file_size, error);
  if (error) {
    println("error: could not open file {}: {}", input_path.string(), error.message());
    return;
  }
  CharStrView input_data{mmap.begin(), mmap.end()};
#else
  Array<char> file_data;
  file_data.resize(file_size);
  FILE * f = std::fopen(input_path.string().c_str(), "r");
  TESL_ASSERT(f != nullptr);
  auto read_size = std::fread(file_data.data(), sizeof(char), file_data.size(), f);
  TESL_ASSERT(read_size == file_size);
  CharStrView input_data{file_data.data(), read_size};
#endif
  grammar::SyntaxResolver resolver;
  auto grammar = grammar::tmpl::expression_library_tmpl.make_library();
  grammar.parse_precedence(make_token_generator({input_data}), resolver, -1);
}

int main(int argc, const char * * argv) {
  //std::setlocale(LC_ALL, "");

  //tesl::IntT i = 999999999999999999;
  //tesl::Variant iv = i;

  //tesl::FloatT f;
  //if (tesl::get_type_of<tesl::FloatT>(tesl::Env::builtin)->coerce(iv, &f)) {
  //  fmt::println("i.type = {}", iv.get_type()->global_name->str);
  //  fmt::println("i = {}", iv.stringify());
  //  fmt::println("i->f = {}", f);
  //}

  //auto parsed_f = tesl::parse::parse_number("0x0000.0e4p-100");
  //fmt::println("{}", parsed_f.get_float());
  //const char * prog_str = "#comment\r\nif (input) {\n\treturn vec3(vec2(1.01, 1.1), 1.2);\n} else {\n\treturn vec3(vec2(1.01, 1.1), 1.2) % 0.9;\n}\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1), 1.2) % input;\nreturn v;\n";
  //const char * prog_str = "# comment\r\n  return (\"ok\\z\\n\");\n";
  //const char * prog_str = "\"test\"+(ifwhiledodo, \"ok\\z\\n\", \"swagever\")\n";
  //const char * prog_str = "\"test\".test(ifwhiledodo, 0x0000.0e4p10, \"ok\\z\\n\", \"swagever\")\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1 0), 1.2);\nfor (int i = input; i; --i) {\n\tv = v * 1.3;\n}\nreturn v;\n";
  //const char * prog_str = "vec3 v = vec3(vec2(1.01, 1.1), 1.2);\nfor (int i = 0; i < input; ++i) {\n\tv *= 1.3;\n}\nreturn v;\n";

  // debug tokenizer
  //tesl::Tokenizer tokenizer{prog_str};
  //tesl::IntT last_line = 0;
  //fmt::println("~ tokenizer start:");
  //while (true) {
  //  tesl::Token tok = tokenizer.next_token();
  //  if (last_line != tokenizer.line_num) {
  //    if (last_line > 0) {
  //      fmt::println("");
  //    }
  //    last_line = tokenizer.line_num;
  //  } else {
  //    fmt::print(", ");
  //  }
  //  fmt::print("{}", tok);
  //  if (tok.kind == tesl::Token::LITERAL) {
  //    //fmt::println(" ({})", tok.literal);
  //  }
  //  if (tok.kind == tesl::Token::END) {
  //    fmt::println("");
  //    break;
  //  }
  //}
  //fmt::println("~ tokenizer end.");

  //fmt::println("~ print_error_source start:");
  //tesl::print_error_source(1, prog_str, &prog_str[21], &prog_str[25], &prog_str[29]);
  //fmt::println("~ print_error_source end.");

  //tokenizer.reset();
  //fmt::println("~ parser start:");
  //tesl::Parser parser{tokenizer, tesl::Compiler{}};
  //parser.parse_program();
  //fmt::println("~ parser end.");

  TESL_ASSERT(argc == 2);

  std::filesystem::path input_path{argv[1]};
  if (std::filesystem::is_regular_file(input_path)) {
    test_file(input_path);
    return 0;
  }

  if (std::filesystem::is_directory(input_path)) {
    tesl::println("~ searching dir: {}", input_path.string());

    std::size_t total_file_size = 0;
    for (auto entry : std::filesystem::recursive_directory_iterator{input_path}) {
      std::filesystem::path entry_path{entry};
      //tesl::println("~ {} (ext={}, exists={}, is_regular_file={}) ", entry_path.c_str(), entry_path.extension().c_str(), entry.exists(), entry.is_regular_file());

      if (!entry.exists() || !entry.is_regular_file() || entry_path.extension() != ".wren") {
        continue;
      }
      total_file_size += std::filesystem::file_size(entry_path);
      test_file(entry_path);
    }

    tesl::println("~ total bytes: {} bytes", total_file_size);
    return 0;
  }
}

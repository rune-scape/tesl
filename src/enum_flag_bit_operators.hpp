// created by rune

#ifndef TEXT_EDITOR_EXAMPLE_SRC_ENUM_FLAG_BIT_OPERATORS_HPP_
#define TEXT_EDITOR_EXAMPLE_SRC_ENUM_FLAG_BIT_OPERATORS_HPP_
#pragma once

#define ENUM_FLAG_BIT_OPERATORS(enum_type_name) \
  inline constexpr bool operator!(enum_type_name a) { return !static_cast<std::underlying_type<enum_type_name>::type>(a); } \
  inline constexpr enum_type_name operator~(enum_type_name a) { return static_cast<enum_type_name>(~static_cast<std::underlying_type<enum_type_name>::type>(a)); } \
  inline constexpr enum_type_name operator|(enum_type_name a, enum_type_name b) { return static_cast<enum_type_name>(static_cast<std::underlying_type<enum_type_name>::type>(a) | static_cast<std::underlying_type<enum_type_name>::type>(b)); } \
  inline constexpr enum_type_name operator&(enum_type_name a, enum_type_name b) { return static_cast<enum_type_name>(static_cast<std::underlying_type<enum_type_name>::type>(a) & static_cast<std::underlying_type<enum_type_name>::type>(b)); } \
  inline constexpr enum_type_name operator^(enum_type_name a, enum_type_name b) { return static_cast<enum_type_name>(static_cast<std::underlying_type<enum_type_name>::type>(a) ^ static_cast<std::underlying_type<enum_type_name>::type>(b)); } \
  inline constexpr enum_type_name & operator|=(enum_type_name & a, enum_type_name b) { return a = static_cast<enum_type_name>(static_cast<std::underlying_type<enum_type_name>::type>(a) | static_cast<std::underlying_type<enum_type_name>::type>(b)); } \
  inline constexpr enum_type_name & operator&=(enum_type_name & a, enum_type_name b) { return a = static_cast<enum_type_name>(static_cast<std::underlying_type<enum_type_name>::type>(a) & static_cast<std::underlying_type<enum_type_name>::type>(b)); } \
  inline constexpr enum_type_name & operator^=(enum_type_name & a, enum_type_name b) { return a = static_cast<enum_type_name>(static_cast<std::underlying_type<enum_type_name>::type>(a) ^ static_cast<std::underlying_type<enum_type_name>::type>(b)); }

#endif //TEXT_EDITOR_EXAMPLE_SRC_ENUM_FLAG_BIT_OPERATORS_HPP_

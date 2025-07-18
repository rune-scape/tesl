// created by rune

#pragma once

#include "endian.hpp"
#include "enum_flag_bit_operators.hpp"
#include "unaligned.hpp"

#include <bit>
#include <climits>
#include <cstdint>
#include <new>
#include <type_traits>

namespace tesl::unicode {
  namespace detail {
    template<typename T, typename ... Rest>
    constexpr bool is_one_of_v = (... || std::is_same_v<T, Rest>);

    template<typename T>
    constexpr bool is_char16_like_v = is_one_of_v<T, char16_t, std::int16_t, std::uint16_t
        #if WCHAR_MAX == 0xFFFF || WCHAR_MAX == 0x7FFF
        , wchar_t
        #endif
    >;

    template<typename T>
    constexpr bool is_char32_like_v = is_one_of_v<T, char32_t, std::int32_t, std::uint32_t
        #if WCHAR_MAX == 0xFFFFFFFF || WCHAR_MAX == 0x7FFFFFFF
        , wchar_t
        #endif
    >;

    template<typename T>
    constexpr bool is_octet_v = is_one_of_v<T, std::int8_t, std::uint8_t
        #if CHAR_BIT == 8
        , char
        #ifdef __cpp_lib_byte
        , std::byte
        #endif
        #endif
        #ifdef __cpp_char8_t
        , char8_t
        #endif
    >;

    template<typename T>
    inline T * ptr_advance_bytes(T * ptr, std::ptrdiff_t bytes) {
      return reinterpret_cast<T *>(reinterpret_cast<char *>(ptr) + bytes);
    }

    template<typename T>
    inline const T * ptr_advance_bytes(const T * ptr, std::ptrdiff_t bytes) {
      return reinterpret_cast<const T *>(reinterpret_cast<const char *>(ptr) + bytes);
    }

    template<typename T>
    inline std::ptrdiff_t ptr_distance_bytes(T * a, T * b) {
      return reinterpret_cast<char *>(b) - reinterpret_cast<char *>(a);
    }

    template<typename T>
    inline std::ptrdiff_t ptr_distance_bytes(const T * a, const T * b) {
      return reinterpret_cast<const char *>(b) - reinterpret_cast<const char *>(a);
    }
  }

  using octet_t = std::uint8_t;

  enum class codec_error : int {
    ok = 0,
    incomplete_input = 1 << 0,
    insufficient_space = 1 << 1,
    illegal_byte_sequence = 1 << 2,
    invalid_codepoint = 1 << 3
  };

  ENUM_FLAG_BIT_OPERATORS(codec_error)

  enum class check_mode {
    none = 0,
    capacity = 1 << 0,
    validity = 1 << 1,
    unaligned = 1 << 2,
    normal = capacity | validity
  };

  ENUM_FLAG_BIT_OPERATORS(check_mode)

  constexpr char16_t replacement_codepoint = U'\uFFFD';

  /// LEAD_SURROGATE_MIN - (SURROGATE_CODEPOINT_START >> 10) = 0xD7C0
  static constexpr std::uint16_t lead_offset = 0xD800u - (0x10000ul >> 10);
  /// (LEAD_SURROGATE_MIN << 10) + TRAIL_SURRAGATE_MIN - SURROGATE_CODEPOINT_START = 0x035FDC00
  static constexpr std::uint32_t surrogate_offset = (0xD800ul << 10) + 0xDC00u - 0x10000ul;

  inline bool is_surrogate(char16_t c) {
    return 0xD800u <= c && c <= 0xDFFFu;
  }

  inline bool is_lead_surrogate(char16_t c) {
    return (0xD800u <= c && c <= 0xDBFFu);
  }

  inline bool is_trail_surrogate(char16_t c) {
    return (0xDC00u <= c && c <= 0xDFFFu);
  }

  inline bool is_codepoint_valid(char32_t cp) {
    return cp <= U'\U0010FFFF' && !is_surrogate(cp);
  }

  namespace utf8 {
    constexpr octet_t replacement_sequence[] = {0xEFu, 0xBFu, 0xBDu};
    constexpr auto replacement_sequence_size = sizeof(replacement_sequence) / sizeof(octet_t);

    inline bool is_trail(octet_t b) {
      return (b & 0xC0u) == 0x80u;
    }

    inline bool is_overlong_sequence(char32_t cp, int length) {
      if (cp < 0x80) {
        return length != 1;
      } else if (cp < 0x800) {
        return length != 2;
      } else if (cp < 0x10000) {
        return length != 3;
      } else if (cp < 0x200000) {
        return length != 4;
      } else if (cp < 0x4000000) {
        return length != 5;
      } else if (cp < 0x80000000) {
        return length != 6;
      }
      return false;
    }

    inline int sequence_length(octet_t lead) {
      #if __cpp_lib_bitops
      switch (std::countl_one(lead)) {
        case 0:
          return 1;
        case 2:
          return 2;
        case 3:
          return 3;
        case 4:
          return 4;
        case 5:
          return 5;
        case 6:
          return 6;
        default:
          return 0;
      }
      #else
      if (lead < 0x80u) {
        return 1;
      } else if ((lead & 0xE0u) == 0xC0u) {
        return 2;
      } else if ((lead & 0xF0u) == 0xE0u) {
        return 3;
      } else if ((lead & 0xF8u) == 0xF0u) {
        return 4;
      } else if ((lead & 0xFCu) == 0xF8u) {
        return 5;
      } else if ((lead & 0xFEu) == 0xFCu) {
        return 6;
      } else {
        return 0;
      }
      #endif
    }

    template<check_mode CheckMode = check_mode::normal, typename Char32OutT, typename OctetInT>
    inline codec_error decode_next(Char32OutT & cp, const OctetInT *& it, const OctetInT * end = nullptr) {
      static_assert(detail::is_char32_like_v<Char32OutT>);
      static_assert(detail::is_octet_v<OctetInT>);
      if constexpr(!!(CheckMode & check_mode::capacity))
        if (it == end) return codec_error::incomplete_input;

      int length = sequence_length(*it);

      #define INPUT_SIZE_CHECK(n) \
        if constexpr(!!(CheckMode & check_mode::capacity)) \
          if (end && (end - it) < (n)) { \
            it = end; \
            cp = replacement_codepoint; \
            return codec_error::incomplete_input; \
          }
      #define IS_TRAIL_CHECK(p_byte) \
        if constexpr(!!(CheckMode & check_mode::validity)) { \
          if (!is_trail(p_byte)) { \
            cp = replacement_codepoint; \
            return codec_error::illegal_byte_sequence; \
          } \
        }
      char32_t tmp_cp;
      switch (length) {
        case 1:
          INPUT_SIZE_CHECK(1)
          tmp_cp = static_cast<octet_t>(*(it++));
          break;
        case 2:
          INPUT_SIZE_CHECK(2)
          tmp_cp = static_cast<octet_t>(*(it++)) & 0x1Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3fu;
          break;
        case 3:
          INPUT_SIZE_CHECK(3)
          tmp_cp = static_cast<octet_t>(*(it++)) & 0x0Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          break;
        case 4:
          INPUT_SIZE_CHECK(4)
          tmp_cp = static_cast<octet_t>(*(it++)) & 0x07u;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          break;
        case 5:
          INPUT_SIZE_CHECK(5)
          tmp_cp = static_cast<octet_t>(*(it++)) & 0x03u;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          break;
        case 6:
          INPUT_SIZE_CHECK(6)
          tmp_cp = static_cast<octet_t>(*(it++)) & 0x03u;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          IS_TRAIL_CHECK(*it)
          (tmp_cp <<= 6) |= static_cast<octet_t>(*(it++)) & 0x3Fu;
          break;
        default:
          ++it;
          tmp_cp = replacement_codepoint;
          if constexpr(!!(CheckMode & check_mode::validity)) {
            cp = replacement_codepoint;
            return codec_error::illegal_byte_sequence;
          }
          break;
      }
      cp = tmp_cp;
      #undef IS_TRAIL_CHECK
      #undef INPUT_SIZE_CHECK

      if constexpr(!!(CheckMode & check_mode::validity))
        if (!is_codepoint_valid(cp)) {
          cp = replacement_codepoint;
          return codec_error::illegal_byte_sequence;
        }
      if constexpr(!!(CheckMode & check_mode::validity))
        if (is_overlong_sequence(cp, length)) {
          cp = replacement_codepoint;
          return codec_error::illegal_byte_sequence;
        }
      return codec_error::ok;
    }

    template<check_mode CheckMode = check_mode::normal, typename OctetInT>
    inline codec_error validate_next(const OctetInT *& it, const OctetInT * end = nullptr) {
      char32_t ignore;
      return decode_next<CheckMode>(ignore, it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char32OutT, typename OctetInT>
    inline codec_error decode_prev(Char32OutT & cp, const OctetInT *& it, const OctetInT * begin = nullptr) {
      static_assert(detail::is_char32_like_v<Char32OutT>);
      static_assert(detail::is_octet_v<OctetInT>);
      if constexpr(!!(CheckMode & check_mode::capacity))
        if (it == begin) return codec_error::incomplete_input;

      const OctetInT * next_it = it;
      for (int i = 1; i <= 6; ++i) {
        if (!is_trail(*(--next_it))) {
          const OctetInT * tmp_it = next_it;
          codec_error err = decode_next<CheckMode>(cp, tmp_it, it);
          if constexpr(!!(CheckMode & check_mode::validity)) {
            if (tmp_it == it) {
              it = next_it;
              if (!!(err & codec_error::incomplete_input)) {
                err &= ~codec_error::incomplete_input;
                err |= codec_error::illegal_byte_sequence;
              }
              return err;
            } else break;
          } else {
            it = next_it;
            return codec_error::ok;
          }
        }
        if constexpr(!!(CheckMode & check_mode::capacity))
          if (next_it == begin) break;
      }

      --it;
      cp = replacement_codepoint;
      return codec_error::illegal_byte_sequence;
    }

    template<check_mode CheckMode = check_mode::normal, typename OctetInT>
    inline codec_error validate_prev(const OctetInT *& it, const OctetInT * begin = nullptr) {
      char32_t ignore;
      return decode_prev<CheckMode>(ignore, it, begin);
    }

    template<check_mode CheckMode = check_mode::normal, typename OctetOutT>
    inline codec_error encode(char32_t cp, OctetOutT *& it, OctetOutT * end = nullptr) {
      static_assert(detail::is_octet_v<OctetOutT>);
      static_assert(!std::is_const_v<OctetOutT>);
      if constexpr(!!(CheckMode & check_mode::validity))
        if (!is_codepoint_valid(cp)) {
          if constexpr(!!(CheckMode & check_mode::capacity))
            if (end && (end - it) < replacement_sequence_size)
              return codec_error::insufficient_space | codec_error::invalid_codepoint;
          for (int i = 0; i < replacement_sequence_size; ++i) {
            *(it++) = replacement_sequence[i];
          }
          return codec_error::invalid_codepoint;
        }

      #define OUTPUT_SIZE_CHECK(n) \
        if constexpr(!!(CheckMode & check_mode::capacity)) \
          if (end && (end - it) < (n)) \
            return codec_error::insufficient_space;
      if (cp < 0x80) {
        OUTPUT_SIZE_CHECK(1)
        *(it++) = cp;
      } else if (cp < 0x800) {
        OUTPUT_SIZE_CHECK(2)
        *(it++) = 0xC0 | (cp >> 6);
        *(it++) = 0x80 | (cp & 0x3F);
      } else if (cp < 0x10000) {
        OUTPUT_SIZE_CHECK(3)
        *(it++) = 0xE0 | (cp >> 12);
        *(it++) = 0x80 | ((cp >> 6) & 0x3F);
        *(it++) = 0x80 | (cp & 0x3F);
      } else if (cp < 0x200000) {
        OUTPUT_SIZE_CHECK(4)
        *(it++) = 0xF0 | (cp >> 18);
        *(it++) = 0x80 | ((cp >> 12) & 0x3F);
        *(it++) = 0x80 | ((cp >> 6) & 0x3F);
        *(it++) = 0x80 | (cp & 0x3F);
      } else if (cp < 0x4000000) {
        OUTPUT_SIZE_CHECK(5)
        *(it++) = 0xF8 | (cp >> 24);
        *(it++) = 0x80 | ((cp >> 18) & 0x3F);
        *(it++) = 0x80 | ((cp >> 12) & 0x3F);
        *(it++) = 0x80 | ((cp >> 6) & 0x3F);
        *(it++) = 0x80 | (cp & 0x3F);
      } else {
        OUTPUT_SIZE_CHECK(6)
        *(it++) = 0xFC | (cp >> 30);
        *(it++) = 0x80 | ((cp >> 24) & 0x3F);
        *(it++) = 0x80 | ((cp >> 18) & 0x3F);
        *(it++) = 0x80 | ((cp >> 12) & 0x3F);
        *(it++) = 0x80 | ((cp >> 6) & 0x3F);
        *(it++) = 0x80 | (cp & 0x3F);
      }
      #undef OUTPUT_SIZE_CHECK

      return codec_error::ok;
    }
  }

  template<endian Endian>
  struct utf16_ordered {
    template<check_mode CheckMode, typename Char32OutT, typename InT>
    static codec_error decode_next(Char32OutT & cp, const InT *& it, const InT * end = nullptr) {
      static_assert(detail::is_char32_like_v<Char32OutT>);
      using local_char16_t = std::conditional_t<!(CheckMode & check_mode::unaligned), char16_t, unaligned<char16_t>>;
      if constexpr(!!(CheckMode & check_mode::capacity))
        if (end && detail::ptr_distance_bytes(it, end) < 2) return codec_error::incomplete_input;

      char16_t lead = etoh<Endian, char16_t>(*reinterpret_cast<const local_char16_t *>(it));
      it = detail::ptr_advance_bytes(it, 2);
      if (!is_surrogate(lead)) {
        cp = lead;
        return codec_error::ok;
      }

      if constexpr(!!(CheckMode & check_mode::validity))
        if (is_trail_surrogate(lead)) return codec_error::illegal_byte_sequence;

      if constexpr(!!(CheckMode & check_mode::capacity))
        if (end && detail::ptr_distance_bytes(it, end) < 2) {
          cp = replacement_codepoint;
          return codec_error::incomplete_input;
        }

      char16_t trail = etoh<Endian, char16_t>(*reinterpret_cast<const local_char16_t *>(it));

      if constexpr(!!(CheckMode & check_mode::validity))
        if (!is_trail_surrogate(trail)) {
          cp = replacement_codepoint;
          return codec_error::illegal_byte_sequence;
        }

      it = detail::ptr_advance_bytes(it, 2);
      cp = (static_cast<char32_t>(lead) << 10) + trail - surrogate_offset;
      return codec_error::ok;
    }

    template<check_mode CheckMode, typename InT>
    static codec_error validate_next(const InT *& it, const InT * end = nullptr) {
      char32_t ignore;
      return decode_next<CheckMode>(ignore, it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char32T, typename InT>
    static codec_error decode_prev(Char32T & cp, const InT *& it, const InT * begin = nullptr) {
      static_assert(detail::is_char32_like_v<Char32T>);
      using local_char16_t = std::conditional_t<!(CheckMode & check_mode::unaligned), char16_t, unaligned<char16_t>>;
      if constexpr(!!(CheckMode & check_mode::capacity))
        if (begin && detail::ptr_distance_bytes(begin, it) < 2) return codec_error::incomplete_input;

      it = detail::ptr_advance_bytes(it, -2);
      char16_t trail = etoh<Endian, char16_t>(*reinterpret_cast<const local_char16_t *>(it));
      if (!is_surrogate(trail)) {
        cp = trail;
        return codec_error::ok;
      }

      if constexpr(!!(CheckMode & check_mode::validity))
        if (is_lead_surrogate(trail)) {
          cp = replacement_codepoint;
          return codec_error::illegal_byte_sequence;
        }

      if constexpr(!!(CheckMode & check_mode::capacity))
        if (begin && detail::ptr_distance_bytes(begin, it) < 2) {
          cp = replacement_codepoint;
          return codec_error::incomplete_input;
        }

      const auto * lead_it = detail::ptr_advance_bytes(it, -2);
      char16_t lead = etoh<Endian, char16_t>(*reinterpret_cast<const local_char16_t *>(lead_it));

      if constexpr(!!(CheckMode & check_mode::validity)) {
        if (!is_lead_surrogate(lead)) {
          cp = replacement_codepoint;
          return codec_error::illegal_byte_sequence;
        }
      }

      it = lead_it;
      cp = (static_cast<char32_t>(lead) << 10) + trail - surrogate_offset;
      return codec_error::ok;
    }

    template<check_mode CheckMode = check_mode::normal, typename InT>
    static codec_error validate_prev(const InT *& it, const InT * begin = nullptr) {
      char32_t ignore;
      return utf8::decode_prev<CheckMode>(ignore, it, begin);
    }

    template<check_mode CheckMode = check_mode::normal, typename OutT>
    static codec_error encode(char32_t cp, OutT *& it, OutT * end = nullptr) {
      static_assert(!std::is_const_v<OutT>);
      using local_char16_t = std::conditional_t<!(CheckMode & check_mode::unaligned), char16_t, unaligned<char16_t>>;
      if constexpr(!!(CheckMode & check_mode::validity))
        if (!is_codepoint_valid(cp)) {
          if constexpr(!!(CheckMode & check_mode::capacity))
            if (end && detail::ptr_distance_bytes(it, end) < 2) return codec_error::insufficient_space | codec_error::invalid_codepoint;
          *reinterpret_cast<local_char16_t *>(it) = htoe<Endian, char16_t>(replacement_codepoint);
          it = detail::ptr_advance_bytes(it, 2);
          it = std::launder(it);
          return codec_error::invalid_codepoint;
        }

      if (cp <= 0xFFFFu) {
        if constexpr(!!(CheckMode & check_mode::capacity))
          if (end && detail::ptr_distance_bytes(it, end) < 2) return codec_error::insufficient_space;

        *reinterpret_cast<local_char16_t *>(it) = htoe<Endian, char16_t>(cp);
        it = detail::ptr_advance_bytes(it, 2);
      } else {
        if constexpr(!!(CheckMode & check_mode::capacity))
          if (end && detail::ptr_distance_bytes(it, end) < 4) return codec_error::insufficient_space;

        *reinterpret_cast<local_char16_t *>(it) = htoe<Endian, char16_t>((cp >> 10) + lead_offset);
        it = detail::ptr_advance_bytes(it, 2);
        *reinterpret_cast<local_char16_t *>(it) = htoe<Endian, char16_t>(0xDC00u + (cp & 0x3FFu));
        it = detail::ptr_advance_bytes(it, 2);
      }
      it = std::launder(it);
      return codec_error::ok;
    }
  };

  using utf16ne = utf16_ordered<endian::native>;
  using utf16le = utf16_ordered<endian::little>;
  using utf16be = utf16_ordered<endian::big>;

  namespace utf16 {
    template<check_mode CheckMode = check_mode::normal, typename Char32T, typename Char16T>
    static codec_error decode_next(Char32T & cp, const Char16T *& it, const Char16T * end = nullptr) {
      static_assert(detail::is_char16_like_v<Char16T>);
      return utf16_ordered<endian::native>::template decode_next<CheckMode>(cp, it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char16T>
    static codec_error validate_next(const Char16T *& it, const Char16T * end = nullptr) {
      static_assert(detail::is_char16_like_v<Char16T>);
      return utf16_ordered<endian::native>::template validate_next<CheckMode>(it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char32T, typename Char16T>
    static codec_error decode_prev(Char32T & cp, const Char16T *& it, const Char16T * end = nullptr) {
      static_assert(detail::is_char16_like_v<Char16T>);
      return utf16_ordered<endian::native>::template decode_prev<CheckMode>(cp, it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char16T>
    static codec_error validate_prev(const Char16T *& it, const Char16T * end = nullptr) {
      static_assert(detail::is_char16_like_v<Char16T>);
      return utf16_ordered<endian::native>::template validate_prev<CheckMode>(it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char16T>
    static codec_error encode(char32_t cp, Char16T *& it, Char16T * end = nullptr) {
      static_assert(detail::is_char16_like_v<Char16T>);
      return utf16_ordered<endian::native>::template encode<CheckMode>(cp, it, end);
    }
  }

  template<endian Endian>
  struct utf32_ordered {
    template<check_mode CheckMode, typename Char32T, typename InT>
    static codec_error decode_next(Char32T & cp, const InT *& it, const InT * end = nullptr) {
      static_assert(detail::is_char32_like_v<Char32T>);
      using local_char32_t = std::conditional_t<!(CheckMode & check_mode::unaligned), char32_t, unaligned<char32_t>>;
      if constexpr(!!(CheckMode & check_mode::capacity))
        if (end && detail::ptr_distance_bytes(it, end) < 4) return codec_error::incomplete_input;

      char32_t tmp_cp = etoh<Endian, char32_t>(*reinterpret_cast<const local_char32_t *>(it));
      it = detail::ptr_advance_bytes(it, 4);

      if constexpr(!!(CheckMode & check_mode::validity))
        if (!is_codepoint_valid(tmp_cp)) {
          cp = replacement_codepoint;
          return codec_error::illegal_byte_sequence;
        }

      cp = tmp_cp;
      return codec_error::ok;
    }

    template<check_mode CheckMode, typename InT>
    static codec_error validate_next(const InT *& it, const InT * end = nullptr) {
      char32_t ignore;
      return decode_next<CheckMode>(ignore, it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char32T, typename InT>
    static codec_error decode_prev(Char32T & cp, const InT *& it, const InT * begin = nullptr) {
      static_assert(detail::is_char32_like_v<Char32T>);
      using local_char32_t = std::conditional_t<!(CheckMode & check_mode::unaligned), char32_t, unaligned<char32_t>>;
      if constexpr(!!(CheckMode & check_mode::capacity))
        if (begin && detail::ptr_distance_bytes(begin, it) < 4) return codec_error::incomplete_input;

      it = detail::ptr_advance_bytes(it, -4);
      char32_t tmp_cp = etoh<Endian, char32_t>(*reinterpret_cast<const local_char32_t *>(it));

      if constexpr(!!(CheckMode & check_mode::validity))
        if (!is_codepoint_valid(tmp_cp)) {
          cp = replacement_codepoint;
          return codec_error::illegal_byte_sequence;
        }

      cp = tmp_cp;
      return codec_error::ok;
    }

    template<check_mode CheckMode = check_mode::normal, typename InT>
    static codec_error validate_prev(const InT *& it, const InT * begin = nullptr) {
      char32_t ignore;
      return utf8::decode_prev<CheckMode>(ignore, it, begin);
    }

    template<check_mode CheckMode = check_mode::normal, typename OutT>
    static codec_error encode(char32_t cp, OutT *& it, OutT * end = nullptr) {
      static_assert(!std::is_const_v<OutT>);
      using local_char32_t = std::conditional_t<!(CheckMode & check_mode::unaligned), char32_t, unaligned<char32_t>>;
      if constexpr(!!(CheckMode & check_mode::validity))
        if (!is_codepoint_valid(cp)) {
          if constexpr(!!(CheckMode & check_mode::capacity))
            if (end && detail::ptr_distance_bytes(it, end) < 4) return codec_error::insufficient_space | codec_error::invalid_codepoint;
          *reinterpret_cast<local_char32_t *>(it) = htoe<Endian, char32_t>(replacement_codepoint);
          it = std::launder(detail::ptr_advance_bytes(it, 4));
          return codec_error::invalid_codepoint;
        }

      if constexpr(!!(CheckMode & check_mode::capacity))
        if (end && detail::ptr_distance_bytes(it, end) < 4) return codec_error::insufficient_space;

      *reinterpret_cast<local_char32_t *>(it) = htoe<Endian, char32_t>(cp);
      it = std::launder(detail::ptr_advance_bytes(it, 4));
      return codec_error::ok;
    }
  };

  using utf32ne = utf32_ordered<endian::native>;
  using utf32le = utf32_ordered<endian::little>;
  using utf32be = utf32_ordered<endian::big>;

  namespace utf32 {
    template<check_mode CheckMode = check_mode::normal, typename Char32T1, typename Char32T2>
    static codec_error decode_next(Char32T1 & cp, const Char32T2 *& it, const Char32T2 * end = nullptr) {
      static_assert(detail::is_char32_like_v<Char32T2>);
      return utf32_ordered<endian::native>::template decode_next<CheckMode>(cp, it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char32T>
    static codec_error validate_next(const Char32T *& it, const Char32T * end = nullptr) {
      static_assert(detail::is_char32_like_v<Char32T>);
      return utf32_ordered<endian::native>::template validate_next<CheckMode>(it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char32T1, typename Char32T2>
    static codec_error decode_prev(Char32T1 & cp, const Char32T2 *& it, const Char32T2 * end = nullptr) {
      static_assert(detail::is_char32_like_v<Char32T2>);
      return utf32_ordered<endian::native>::template decode_prev<CheckMode>(cp, it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char32T>
    static codec_error validate_prev(const Char32T *& it, const Char32T * end = nullptr) {
      static_assert(detail::is_char32_like_v<Char32T>);
      return utf32_ordered<endian::native>::template validate_prev<CheckMode>(it, end);
    }

    template<check_mode CheckMode = check_mode::normal, typename Char32T>
    static codec_error encode(char32_t cp, Char32T *& it, Char32T * end = nullptr) {
      static_assert(detail::is_char32_like_v<Char32T>);
      return utf32_ordered<endian::native>::template encode<CheckMode>(cp, it, end);
    }
  }
}

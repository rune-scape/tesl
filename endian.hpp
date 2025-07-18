// created by rune

#pragma once

#include <cstddef>
#include <cstdint>
#include <bit>

#include "byteorder.h"

namespace tesl {
  #ifdef __cpp_lib_endian
  using std::endian;
  #elif defined(BYTE_ORDER)
  enum class endian {
    little = LITTLE_ENDIAN,
    big = BIG_ENDIAN,
    native = BYTE_ORDER
  };
  #else
  enum class endian {
    little,
    big,
    native
  };
  #endif

  inline constexpr bool is_little_endian() {
    if (endian::native == endian::little) return true;
    if (endian::native == endian::big) return false;
    const std::uint32_t value = 0x04030201;
    return *reinterpret_cast<const std::uint8_t *>(&value) == 0x01;
  }

  inline constexpr bool is_big_endian() {
    if (endian::native == endian::big) return true;
    if (endian::native == endian::little) return false;
    const std::uint32_t value = 0x04030201;
    return *reinterpret_cast<const std::uint8_t *>(&value) == 0x04;
  }

  inline constexpr bool is_native_endian(endian e) {
    if (e == endian::native) return true;
    if (e == endian::little && is_little_endian()) return true;
    if (e == endian::big && is_big_endian()) return true;
    return false;
  }

  #ifdef __cpp_lib_byteswap
  using std::byteswap;
  #else
  namespace detail {
    template<std::size_t Size>
    struct byteswap_dispatcher;

    template<>
    struct byteswap_dispatcher<2> {
      template<typename T>
      static T byteswap(T v) {
        static_assert(alignof(T) >= alignof(std::uint16_t));
        T result;
        *reinterpret_cast<std::uint16_t *>(&result) = bswap16(*reinterpret_cast<std::uint16_t *>(&result));
        return result;
      }
    };

    template<>
    struct byteswap_dispatcher<4> {
      template<typename T>
      static T byteswap(T v) {
        static_assert(alignof(T) >= alignof(std::uint32_t));
        T result;
        *reinterpret_cast<std::uint32_t *>(&result) = bswap32(*reinterpret_cast<std::uint32_t *>(&result));
        return result;
      }
    };

    template<>
    struct byteswap_dispatcher<8> {
      template<typename T>
      static T byteswap(T v) {
        static_assert(alignof(T) >= alignof(std::uint64_t));
        T result;
        *reinterpret_cast<std::uint64_t *>(&result) = bswap64(*reinterpret_cast<std::uint64_t *>(&result));
        return result;
      }
    };
  }

  template<typename T>
  T byteswap(T v) noexcept { return detail::byteswap_dispatcher<sizeof(T)>::byteswap(v); }
  #endif

  template<endian Endian, typename T>
  inline constexpr T htoe(T v) {
    return is_native_endian(Endian) ? v : byteswap(v);
  }

  template<endian Endian, typename T>
  inline constexpr T etoh(T v) {
    return is_native_endian(Endian) ? v : byteswap(v);
  }

  template<typename T>
  inline constexpr T htoe(T v, endian e) {
    return is_native_endian(e) ? v : byteswap(v);
  }

  template<typename T>
  inline constexpr T etoh(T v, endian e) {
    return is_native_endian(e) ? v : byteswap(v);
  }

  template<typename T>
  inline T htole(T v) { return htoe<endian::little>(v); }
  template<typename T>
  inline T htobe(T v) { return htoe<endian::big>(v); }

  template<typename T>
  inline T letoh(T v) { return etoh<endian::little>(v); }
  template<typename T>
  inline T betoh(T v) { return etoh<endian::big>(v); }

  template<typename T, endian Endian>
  struct ordered {
    template<typename U>
    ordered & operator=(const U & v) & {
      value = htoe<Endian>(v);
      return *this;
    }
    template<typename U>
    operator U() const { return etoh<Endian>(value); } // NOLINT(google-explicit-constructor)
    T value;
  };
}

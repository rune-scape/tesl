#pragma once

#include "tesl_common.hpp"
#include <type_traits>

namespace tesl {
  namespace detail {
    template<typename ... Ts>
    union UnionStorage;

    template<typename First, typename ... Rest>
    union UnionStorage<First, Rest...> {
      First first;
      UnionStorage<Rest...> next;

      template<typename T>
      TESL_ALWAYS_INLINE void destroy() {
        if constexpr (std::is_same_v<T, First>) {
          first.~First();
        } else {
          next.template destroy<T>();
        }
      }
    };

    template<>
    union UnionStorage<> {
      template<typename T>
      TESL_ALWAYS_INLINE void destroy() {}
    };
  }

  template<typename ... Ts>
  struct Union {
  private:
    using IndexT = std::conditional_t<
      (sizeof...(Ts) - 1) <= std::numeric_limits<std::uint8_t>::max(),
      std::uint8_t,
      std::conditional_t<
        (sizeof...(Ts) - 1) <= std::numeric_limits<std::uint16_t>::max(),
        std::uint16_t,
        std::conditional_t<
          (sizeof...(Ts) - 1) <= std::numeric_limits<std::uint32_t>::max(),
          std::uint32_t,
          std::uint64_t
        >
      >
    >;

    using UnionStorageT = detail::UnionStorage<Ts...>;

    using DestructorFn = void (*)();

    IndexT _index;
    UnionStorageT _storage;

  public:
    ~Union() {
      constexpr DestructorFn destructors[] = {
        [](UnionStorageT & s){ s.template destroy<Ts>(); }...
      };
      destructors[_index](_storage);
    }
  };
}
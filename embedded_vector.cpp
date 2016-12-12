#include "embedded_vector.hpp"
#include <iostream>
#include <type_traits>
#include <stl2/view/iota.hpp>
#include <stl2/view/take_exactly.hpp>
#include "../cmcstl2/test/simple_test.hpp"

static_assert(std::is_literal_type<embedded_vector<int, 16>>::value);

int main() {
    embedded_vector<int, 16> vec;
    CHECK(vec.capacity() == 16u);
    CHECK(vec.max_size() == 16u);
    CHECK(vec.size() == 0u);
    constexpr auto rng = ranges::ext::take_exactly_view<ranges::ext::iota_view<int>>{{}, 8};
    static_assert(ranges::size(rng) == 8u);
    ranges::copy(rng, ranges::back_inserter(vec));
    CHECK(vec.size() == 8u);
    ::check_equal(vec, rng);
}

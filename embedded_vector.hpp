#include <limits>
#include <experimental/ranges/iterator>
#include <experimental/ranges/memory>
#include <stl2/view/move.hpp>
#include <stl2/view/ref.hpp>
#include <stl2/view/repeat_n.hpp>
#include <stl2/detail/algorithm/copy.hpp>
#include <stl2/detail/algorithm/equal.hpp>
#include <stl2/detail/algorithm/lexicographical_compare.hpp>
#include <stl2/detail/algorithm/move.hpp>
#include <stl2/detail/algorithm/swap_ranges.hpp>

namespace ranges = std::experimental::ranges;

template <ranges::Destructible, std::size_t> class embedded_vector;

namespace embedded_vector_detail {
	template <class T>
	requires ranges::SignedIntegral<T>() || ranges::Same<void, T>()
	struct range_size {
		T size_;
	};
	template <>
	struct range_size<void> {};

	ranges::Sentinel{S, I}
	constexpr auto range_sizer(I const&, S const&) noexcept {
		return range_size<void>{};
	}
	ranges::SizedSentinel{S, I}
	constexpr range_size<ranges::difference_type_t<I>>
	range_sizer(I const& first, S const& last)
	noexcept(noexcept(last - first))
	{
		return {last - first};
	}
	constexpr auto range_sizer(ranges::Range&& rng) noexcept {
		return range_size<void>{};
	}
	template <ranges::ForwardRange Rng>
	requires !ranges::SizedRange<Rng>()
	constexpr auto range_sizer(ranges::ForwardRange&& rng) noexcept {
		return range_sizer(ranges::begin(rng), ranges::end(rng));
	}
	ranges::SizedRange{Rng}
	constexpr range_size<ranges::difference_type_t<ranges::iterator_t<Rng>>>
	range_sizer(Rng&& rng)
	noexcept(noexcept(ranges::size(rng)))
	{
		return {ranges::distance(rng)};
	}

	template <std::size_t N>
	using choose_difference_type =
		meta::if_c<N <= std::numeric_limits<int>::max(), int,
		meta::if_c<N <= std::numeric_limits<long>::max(), long, long long>>;

	template <ranges::Destructible T, std::size_t N>
	class storage {
	public:
		using difference_type = choose_difference_type<N>;
		using size_type = std::make_unsigned_t<difference_type>;

		storage() = default;
		constexpr storage(size_type n) noexcept
		: size_{(STL2_EXPECT(n <= N), n)}
		{
			ranges::uninitialized_value_construct(data(), n);
		}

		T* data() noexcept {
			return reinterpret_cast<T*>(space_ + 0);
		}
		T const* data() const noexcept {
			return reinterpret_cast<T*>(space_ + 0);
		}

		constexpr size_type size() const {
			return size_;
		}

		template<class... Args>
		requires ranges::Constructible<T, Args...>()
		auto& emplace_back(Args&&... args)
		noexcept(std::is_nothrow_constructible<T, Args...>::value)
		{
			STL2_EXPECT(size_ < N);
			auto& target = data()[size_];
			ranges::__construct_at(target, std::forward<Args>(args)...);
			++size_;
			return target;
		}

		void clear() noexcept {
			ranges::destroy(data(), data() + size());
			size_ = 0;
		}

		void swap(storage& that)
		noexcept(ranges::is_nothrow_swappable_v<T&, T&>)
		requires
			ranges::Swappable<T&>() &&
			ranges::MoveConstructible<T>()
		{
			auto my_end = data() + size_;
			auto that_end = that.data() + that.size_;
			auto result = ranges::swap_ranges(
				data(), my_end,
				that.data(), that_end
			);
			if (result.in1() != my_end) {
				ranges::uninitialized_move(result.in1(), my_end, result.in2());
			} else if (result.in2() != that_end) {
				ranges::uninitialized_move(result.in2(), that_end, result.in1());
			}
			ranges::swap(size_, that.size_);
		}
	protected:
		// FIXME: conditionally trivial destruction.
		~storage() {
			ranges::destroy(data(), data() + size());
		}
		constexpr void set_size(size_type n) {
			size_ = n;
		}
	private:
		alignas(T) unsigned char space_[N * sizeof(T)];
		size_type size_ = 0;
	};

	template <ranges::Destructible T, std::size_t N>
	requires std::is_trivial<T>::value && N > 0
	struct storage<T, N> {
	public:
		using difference_type = choose_difference_type<N>;
		using size_type = std::make_unsigned_t<difference_type>;

		constexpr storage() noexcept = default;
		constexpr storage(size_type n) noexcept
		: size_{(STL2_EXPECT(n <= N), n)}
		{}

		T* data() noexcept {
			return buffer_ + 0;
		}
		T const* data() const noexcept {
			return buffer_ + 0;
		}

		constexpr size_type size() const noexcept {
			return size_;
		}

		template<class... Args>
		requires ranges::Constructible<T, Args...>()
		auto& emplace_back(Args&&... args)
		noexcept(std::is_nothrow_constructible<T, Args...>::value)
		{
			STL2_EXPECT(size_ < N);
			auto& target = data()[size_];
			ranges::__construct_at(target, std::forward<Args>(args)...);
			++size_;
			return target;
		}

		void clear() noexcept {
			size_ = 0;
		}

		constexpr void swap(storage& that)
		noexcept(ranges::is_nothrow_swappable_v<T&, T&>)
		requires ranges::Swappable<T&>()
		{
			ranges::swap(buffer_, that.buffer_);
			ranges::swap(size_, that.size_);
		}
	protected:
		~storage() = default;
		constexpr void set_size(size_type n) {
			size_ = n;
		}
	private:
		T buffer_[N] = {};
		size_type size_ = 0;
	};

	template <ranges::Destructible T>
	struct storage<T, 0> {
	public:
		using difference_type = choose_difference_type<0>;
		using size_type = std::make_unsigned_t<difference_type>;

		constexpr storage() noexcept = default;
		constexpr storage(size_type n) noexcept {
			STL2_EXPECT(n == 0);
		}

		constexpr T* data() noexcept {
			return nullptr;
		}
		constexpr T const* data() const noexcept {
			return nullptr;
		}

		constexpr size_type size() const noexcept {
			return 0;
		}

		template<class... Args>
		requires ranges::Constructible<T, Args...>()
		auto& emplace_back(Args&&...)
		noexcept(std::is_nothrow_constructible<T, Args...>::value)
		{
			STL2_EXPECT(false);
			return *static_cast<T*>(nullptr);
		}

		void clear() noexcept
		{}
		template <class U>
		requires ranges::Swappable<T&, U&>()
		constexpr void swap(storage<U, 0>&) noexcept
		{}
	protected:
		~storage() = default;
		constexpr void set_size(size_type n) {
			STL2_EXPECT(n == 0);
		}
	};

	template <class T, class U, std::size_t N>
	void swap(embedded_vector<T, N>& x, embedded_vector<U, N>& y)
	noexcept(noexcept(x.swap(y)))
	requires requires { x.swap(y); }
	{
		x.swap(y);
	}

	template <class T, std::size_t N, class U, std::size_t M>
	requires ranges::EqualityComparable<T, U>()
	constexpr bool operator==(const embedded_vector<T, N>& a, const embedded_vector<U, M>& b) {
		return ranges::equal(a, b);
	}
	template <class T, std::size_t N, class U, std::size_t M>
	requires ranges::EqualityComparable<T, U>()
	constexpr bool operator!=(const embedded_vector<T, N>& a, const embedded_vector<U, M>& b) {
		return !(a == b);
	}
	template <class T, std::size_t N, class U, std::size_t M>
	requires ranges::StrictTotallyOrdered<T, U>()
	constexpr bool operator<(const embedded_vector<T, N>& a, const embedded_vector<U, M>& b) {
		return ranges::lexicographical_compare(a, b);
	}
	template <class T, std::size_t N, class U, std::size_t M>
	requires ranges::StrictTotallyOrdered<T, U>()
	constexpr bool operator<=(const embedded_vector<T, N>& a, const embedded_vector<U, M>& b) {
		return !(b < a);
	}
	template <class T, std::size_t N, class U, std::size_t M>
	requires ranges::StrictTotallyOrdered<T, U>()
	constexpr bool operator>(const embedded_vector<T, N>& a, const embedded_vector<U, M>& b) {
		return b < a;
	}
	template <class T, std::size_t N, class U, std::size_t M>
	requires ranges::StrictTotallyOrdered<T, U>()
	constexpr bool operator>=(const embedded_vector<T, N>& a, const embedded_vector<U, M>& b) {
		return !(a < b);
	}
} // namespace embedded_vector_detail

template <ranges::Destructible T, std::size_t N>
class embedded_vector : public embedded_vector_detail::storage<T, N> {
	using base_t = embedded_vector_detail::storage<T, N>;
public:
	using value_type = T;
	using reference = T&;
	using const_reference = T const&;
	using typename base_t::difference_type;
	using typename base_t::size_type;
	using pointer = T*;
	using const_pointer = T const*;
	using iterator = pointer;
	using const_iterator = const_pointer;
	using reverse_iterator = ranges::reverse_iterator<iterator>;
	using const_reverse_iterator = ranges::reverse_iterator<const_iterator>;

	// construct/copy/move/destroy:
	~embedded_vector() = default;

	embedded_vector() = default;
	constexpr embedded_vector(embedded_vector const& that)
	noexcept(std::is_nothrow_copy_constructible<value_type>::value)
	requires ranges::CopyConstructible<value_type>()
	: embedded_vector{
		embedded_vector_detail::range_sizer(that),
		that.begin(),
		that.end()}
	{}
	constexpr embedded_vector(embedded_vector&& that)
	noexcept(std::is_nothrow_move_constructible<value_type>::value)
	requires ranges::MoveConstructible<value_type>()
	: embedded_vector{
		embedded_vector_detail::range_sizer(that),
		ranges::make_move_iterator(that.begin()),
		ranges::make_move_iterator(that.end())}
	{}
	constexpr explicit embedded_vector(size_type n)
	requires ranges::DefaultConstructible<value_type>()
	: base_t{(STL2_EXPECT(n <= N), n)}
	{}
	constexpr embedded_vector(size_type n, const value_type& value)
	requires ranges::CopyConstructible<value_type>()
	: embedded_vector{
		ranges::ext::repeat_n_view<value_type>{value, (STL2_EXPECT(n <= N), n)}}
	{}
	template <ranges::InputIterator I, ranges::Sentinel<I> S>
	requires ranges::Constructible<value_type, ranges::reference_t<I>>()
	constexpr embedded_vector(I first, S last)
	: embedded_vector{
		embedded_vector_detail::range_sizer(first, last),
		first,
		last}
	{}
	template <ranges::InputRange Rng>
	requires
		ranges::Constructible<value_type, ranges::reference_t<ranges::iterator_t<Rng>>>()
	constexpr embedded_vector(Rng&& rng)
	: embedded_vector{embedded_vector_detail::range_sizer(rng), ranges::begin(rng), ranges::end(rng)}
	{}
	constexpr embedded_vector(std::initializer_list<value_type>&& il)
	noexcept(std::is_nothrow_copy_constructible<value_type>::value)
	requires ranges::CopyConstructible<value_type>()
	: embedded_vector{il}
	{}

	constexpr embedded_vector& operator=(embedded_vector const& that)
	noexcept(std::is_nothrow_copy_constructible<value_type>::value and
		std::is_nothrow_copy_assignable<value_type>::value)
	requires ranges::Copyable<value_type>()
	{
		assign(that);
		return *this;
	}
	constexpr embedded_vector& operator=(embedded_vector&& that)
	noexcept(std::is_nothrow_move_assignable<value_type>::value)
	{
		assign(ranges::ext::move_view<ranges::ext::ref_view<embedded_vector>>{that});
		return *this;
	}

	constexpr void assign(size_type const n, const value_type& u)
	noexcept(std::is_nothrow_copy_constructible<value_type>::value and
		std::is_nothrow_copy_assignable<value_type>::value)
	requires ranges::Copyable<value_type>()
	{
		STL2_EXPECT(n <= N);
		assign(ranges::ext::repeat_n_view<value_type>{u, n});
	}
	template <ranges::InputIterator I, ranges::Sentinel<I> S,
		class R = ranges::reference_t<I>>
	constexpr I assign(I first, S last)
	noexcept(std::is_nothrow_constructible<value_type, R>::value and
		std::is_nothrow_assignable<value_type&, R>::value)
	requires
		ranges::Constructible<value_type, R>() and
		ranges::Assignable<value_type&, R>()
	{
		auto res = ranges::ext::copy(std::move(first), last, data(), data() + size());
		if (res.in() == last) {
			erase(res.out(), this->end());
		} else {
			res.in() = insert(res.out(), std::move(res.in()), std::move(last));
		}
		return res.in();
	}
	template <ranges::InputRange Rng,
		class R = ranges::reference_t<ranges::iterator_t<Rng>>>
	constexpr ranges::safe_iterator_t<Rng> assign(Rng&& rng)
	noexcept(std::is_nothrow_constructible<value_type, R>::value and
		std::is_nothrow_assignable<value_type&, R>::value)
	requires
		ranges::Constructible<value_type, R>() and
		ranges::Assignable<value_type&, R>()
	{
		return assign(ranges::begin(rng), ranges::end(rng));
	}
	constexpr void assign(std::initializer_list<value_type>&& il)
	noexcept(std::is_nothrow_copy_constructible<value_type>::value and
		std::is_nothrow_copy_assignable<value_type>::value)
	requires ranges::Copyable<value_type>()
	{
		assign(il);
	}

	// iterators:
	constexpr iterator               begin()         noexcept { return data(); }
	constexpr const_iterator         begin()   const noexcept { return data(); }
	constexpr iterator               end()           noexcept { return data() + size(); }
	constexpr const_iterator         end()     const noexcept { return data() + size(); }

	constexpr reverse_iterator       rbegin()        noexcept { return {end()}; }
	constexpr const_reverse_iterator rbegin()  const noexcept { return {end()}; }
	constexpr reverse_iterator       rend()          noexcept { return {begin()}; }
	constexpr const_reverse_iterator rend()    const noexcept { return {begin()}; }

	constexpr const_iterator         cbegin()        noexcept { return begin(); }
	constexpr const_iterator         cend()    const noexcept { return end(); }
	constexpr const_reverse_iterator crbegin()       noexcept { return rbegin(); }
	constexpr const_reverse_iterator crend()   const noexcept { return rend(); }

	// size/capacity:
	using base_t::size;
	static constexpr size_type capacity() noexcept {
		return N;
	}
	static constexpr size_type max_size() noexcept {
		return N;
	}
	constexpr void resize(size_type sz) {
		resize_(sz, [](pointer end, pointer new_end) {
			ranges::uninitialized_value_construct(end, new_end);
		});
	}
	constexpr void resize(size_type sz, const value_type& c) {
		resize_(sz, [&c](pointer end, pointer new_end) {
			ranges::uninitialized_fill(end, new_end, c);
		});
	}
	constexpr bool empty() const noexcept {
		return size() == 0;
	}

	// element access:
	constexpr reference       operator[](size_type n) noexcept {
		STL2_EXPECT(n < size());
		return data()[n];
	}
	constexpr const_reference operator[](size_type n) const noexcept {
		STL2_EXPECT(n < size());
		return data()[n];
	}
	constexpr const_reference at(size_type n) const {
		if (n >= size()) throw std::out_of_range{};
		return (*this)[n];
	}
	constexpr reference       at(size_type n) {
		if (n >= size()) throw std::out_of_range{};
		return (*this)[n];
	}
	constexpr reference       front() noexcept {
		STL2_EXPECT(!empty());
		return *data();
	}
	constexpr const_reference front() const noexcept {
		STL2_EXPECT(!empty());
		return *data();
	}
	constexpr reference       back() noexcept {
		STL2_EXPECT(!empty());
		return *(data() + size() - 1);
	}
	constexpr const_reference back() const noexcept {
		STL2_EXPECT(!empty());
		return *(data() + size() - 1);
	}

	// data access:
	using base_t::data;

	// modifiers:
	using base_t::emplace_back;
	constexpr void push_back(const value_type& x)
	requires ranges::CopyConstructible<value_type>()
	{
		emplace_back(x);
	}
	constexpr void push_back(value_type&& x)
	requires ranges::MoveConstructible<value_type>()
	{
		emplace_back(std::move(x));
	}
	void pop_back() noexcept {
		auto sz = size();
		STL2_EXPECT(sz-- > 0);
		this->set_size(sz);
		ranges::destroy_at(data()[sz]);
	}

	template <class... Args>
	constexpr iterator emplace(const_iterator position, Args&&...args);
	constexpr iterator insert(const_iterator position, const value_type& x);
	constexpr iterator insert(const_iterator position, value_type&& x);
	constexpr iterator insert(const_iterator position, size_type n, const value_type& x);
	template <ranges::InputIterator I, ranges::Sentinel<I> S>
	constexpr iterator insert(const_iterator position, I first, S last);
	template <ranges::InputRange Rng>
	constexpr iterator insert(const_iterator position, Rng&& rng);
	constexpr iterator insert(const_iterator position, std::initializer_list<value_type>&& il) {
		return insert(std::move(position), il);
	}

	constexpr iterator erase(const_iterator position)
	noexcept(std::is_nothrow_assignable<value_type&, value_type>::value)
	requires ranges::Movable<value_type>();
	constexpr iterator erase(const_iterator first, const_iterator last)
	noexcept(std::is_nothrow_assignable<value_type&, value_type>::value)
	requires ranges::Movable<value_type>()
	{
		auto le = std::less_equal<>{};
		STL2_EXPECT(le(data(), first));
		STL2_EXPECT(le(first, last));
		STL2_EXPECT(le(last, end()));
		ranges::move(last, end(), first);
		ranges::destroy(last, end());
		this->set_size(last - data());
	}

	using base_t::clear;

	template <class U>
	requires ranges::Swappable<T&, U&>()
	constexpr void swap(embedded_vector<U, N>& that)
	noexcept(ranges::is_nothrow_swappable_v<T&, U&>)
	{
		auto res = ranges::swap_ranges(*this, that);
		if (res.in2() != that.end()) {
			insert(res.in1(), res.in2(), that.end());
			that.erase(res.in2(), that.end());
		}
		if (res.in1() != this->end()) {
			insert(res.in2(), res.in1(), this->end());
			this->erase(res.in1(), this->end());
		}
	}

private:
	template <ranges::InputIterator I, ranges::Sentinel<I> S>
	requires ranges::Constructible<value_type, ranges::reference_t<I>>()
	constexpr embedded_vector(embedded_vector_detail::range_size<void>, I first, S last)
	{
		for (; first != last; ++first) {
			STL2_EXPECT(size() < N);
			emplace_back(*first);
		}
	}
	template <ranges::InputIterator I, ranges::Sentinel<I> S>
	requires ranges::Constructible<value_type, ranges::reference_t<I>>()
	constexpr embedded_vector(embedded_vector_detail::range_size<ranges::difference_type_t<I>> sz, I first, S last)
	{
		STL2_EXPECT(sz.size_ <= N);
		for (; first != last; ++first) {
			emplace_back(*first);
		}
	}
	template <ranges::Callable<pointer, pointer> F>
	constexpr void resize_(size_type sz, F&& f) {
		STL2_EXPECT(sz <= N);
		auto end = data() + size();
		auto new_end = data() + sz;
		if (new_end <= end) {
			ranges::destroy(new_end, end);
		} else if (end < new_end) {
			ranges::invoke(f, end, new_end);
		}
		this->set_size(sz);
	}
};

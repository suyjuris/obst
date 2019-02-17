#pragma once


// The README contains some high-level remarks about the code, you might want to take a look.

// I usually do not pay attention to assertions with side-effects, so let us define them here to
// execute the expressions regardless. Also, the compiler cannot figure out that assert(false) means
// unreachable, so just search-replace all of those with assert_false once going to release.
#ifndef NDEBUG
#include <cassert>
#define assert_false assert(false)
#else
#define assert(x) (void)__builtin_expect(not (expr), 0)
#define assert_false __builtin_unreachable()
#endif

#include <cerrno>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <cstdio>

// TODO: Get rid of all C++ headers
#include <algorithm>
#include <initializer_list>

// Defer macro. Based on Jonathan Blow's code at https://pastebin.com/3YvWQa5c, although rewritten
// from scratch.
template <typename T>
struct Deferrer {
    T t;
    Deferrer(T const& t): t{t} {}
    ~Deferrer() { t(); }
};
struct Deferrer_helper {
    template <typename T>
    auto operator+ (T const& t) { return Deferrer<T> {t}; }
};
#define DEFER_NAME1(x, y) x##y
#define DEFER_NAME(x) DEFER_NAME1(_defer, x)
#define defer auto DEFER_NAME(__LINE__) = Deferrer_helper{} + [&]


// Standard integer types
using s64 = long long; // gcc and emcc (well, their shipped standard libraries) have different opinions about using long long or just long as 64-bit integer types. But for printf I just want to write one of them. Yay.
using u64 = unsigned long long;
//using s64 = std::int64_t;
//using u64 = std::uint64_t;
using s32 = std::int32_t;
using u32 = std::uint32_t;
using s16 = std::int16_t;
using u16 = std::uint16_t;
using s8 = std::int8_t;
using u8 = std::uint8_t;

// General data structures

// Array_t is just a pointer with a size, and Array_dyn a pointer with a size and capacity. They
// conform to my personal data structure invariants: Can be initialised by zeroing the memory, can
// be copied using memcpy. Obviously, this means that there is no hidden allocation happening in
// here, that is all done by the call-site. Also, no const.

template <typename T_>
struct Array_t {
    using T = T_;
    T* data = nullptr;
    s64 size = 0;

    T& operator[] (int pos) {
		assert(0 <= pos and pos < size);
		return data[pos];
	}

    // See the E macro below.
    //T& dbg(int pos, int line) {
    //    if (not (0 <= pos and pos < size)) {
    //        printf("line: %d\n", line);
    //        abort();
    //    }
	//	return data[pos];
	//}

    T* begin() { return data; }
	T* end()   { return data + size; }
};

template <typename T_>
struct Array_dyn: public Array_t<T_> {
    using T = T_;
    s64 capacity;

    Array_dyn(T* data = nullptr, s64 size = 0, s64 capacity = 0):
        Array_t<T>::Array_t{data, size},
        capacity{capacity} {}
    
    explicit Array_dyn(Array_t<T> arr) :
        Array_t<T>::Array_t{arr.data, 0},
        capacity{arr.size} {}
    
    T& operator[] (int pos) {
		assert(0 <= pos and pos < Array_t<T>::size);
		return Array_t<T>::data[pos];
	}

    // See the E macro below.
    //T& dbg (int pos, int line) {
    //    if (0 <= pos and pos < Array_t<T>::size) {
    //        return Array_t<T>::data[pos];
    //    } else {
    //        printf("out of bounds, index %d size %lld, line %d\n", pos, Array_t<T>::size, line);
    //        abort();
    //    }
    //}

    T* begin() const { return (T*)Array_t<T>::data; }
	T* end()   const { return (T*)(Array_t<T>::data + Array_t<T>::size); }
};

// This is to help debugging if the stacktraces stop working. (Which, for some reason, they do.) As
// ~90% of runtime errors are out-of-bounds accesses, I often want to know precisely which one. To
// this end, replace arr[pos] by E(arr,pos) in the places you want to monitor.
//#define E(x, y) ((x).dbg((y), __LINE__))

// Allocation. Returns zeroed memory.
template <typename T>
Array_t<T> array_create(s64 size) {
    return {(T*)calloc(sizeof(T), size), size};
}

// Take some bytes from an already existing memory location. Advance p by the number of bytes used.
template <typename T>
Array_t<T> array_create_from(u8** p, s64 size) {
    Array_t<T> result = {(T*)*p, size};
    *p += sizeof(T) * size;
    return result;
}

// Free the memory, re-initialise the array.
template <typename T>
void array_free(Array_t<T>* arr) {
    assert(arr);
    free(arr->data);
    arr->data = nullptr;
    arr->size = 0;
}
template <typename T>
void array_free(Array_dyn<T>* arr) {
    assert(arr);
    free(arr->data);
    arr->data = nullptr;
    arr->size = 0;
    arr->capacity = 0;
}

// Ensure that there is space for at least count elements.
template <typename T>
void array_reserve(Array_dyn<T>* into, s64 count) {
    if (count > into->capacity) {
        s64 capacity_new = 2 * into->capacity;
        if (capacity_new < count) {
            capacity_new = count;
        }
        if (into->data) {
            into->data = (T*)std::realloc(into->data, capacity_new * sizeof(T));
        } else {
            into->data = (T*)std::malloc(capacity_new * sizeof(T));
        }
        assert(into->data);
        into->capacity = capacity_new;
        assert(into->data);
    }
}

// Set the array's size to count, reallocate if necessary.
template <typename T>
void array_resize(Array_t<T>* arr, s64 count) {
    arr->data = (T*)realloc(arr->data, count * sizeof(T));
    if (arr->size < count) {
        memset(arr->data + arr->size, 0, (count - arr->size) * sizeof(T));
    }
    arr->size = count;
}
template <typename T>
void array_resize(Array_dyn<T>* arr, s64 count) {
    array_reserve(arr, count);
    arr->size = count;
}

// Add element to the end of an array, reallocate if necessary.
template <typename T>
void array_push_back(Array_dyn<T>* into, T elem) {
    array_reserve(into, into->size + 1);
    ++into->size;
    into->data[into->size-1] = elem;
}

// Insert an element into the array, such that its position is index. Reallocate if necessary.
template <typename T>
void array_insert(Array_dyn<T>* into, s64 index, T elem) {
    assert(into and 0 <= index and index <= into->size);
    array_reserve(into, into->size + 1);
    memmove(into->data + (index+1), into->data + index, (into->size - index) * sizeof(T));
    ++into->size;
    into->data[index] = elem;
}

// Append a number of elements to the array.
template <typename T>
void array_append(Array_dyn<T>* into, Array_t<T> data) {
    array_reserve(into, into->size + data.size);
    memcpy(into->end(), data.data, data.size * sizeof(T));
    into->size += data.size;
}
template <typename T>
void array_append(Array_dyn<T>* into, std::initializer_list<T> data) {
    array_reserve(into, into->size + data.size());
    memcpy(into->end(), data.begin(), data.size() * sizeof(T));
    into->size += data.size();
}

// Append a number of zero-initialised elements to the array.
template <typename T>
void array_append_zero(Array_dyn<T>* into, s64 size) {
    array_reserve(into, into->size + size);
    memset(into->end(), 0, size * sizeof(T));
    into->size += size;
}

// Return an array that represents the sub-range [start, end). start == end is fine (but the result
// will use a nullptr).
template <typename T>
Array_t<T> array_subarray(Array_t<T> arr, s64 start, s64 end) {
    assert(0 <= start and start <= arr.size);
    assert(0 <= end   and end   <= arr.size);
    assert(start <= end);
    if (start == end)
        return {nullptr, 0};
    else
        return {arr.data + start, end - start};
}

// These two functions implement a bitset.
void bitset_set(Array_t<u64>* bitset, u64 bit, u8 val) {
    u64 index  = bit / 64;
    u64 offset = bit % 64;
    (*bitset)[index] ^= (((*bitset)[index] >> offset & 1) ^ val) << offset;
}
bool bitset_get(Array_t<u64> bitset, u64 bit) {
    u64 index  = bit / 64;
    u64 offset = bit % 64;
    return bitset[index] >> offset & 1;
}

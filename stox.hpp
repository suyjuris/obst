
// Written by Philipp Czerner, 2018. Public Domain.
// See LICENSE.md for license information.

// Code for stand-alone test. Compile and run test using
//     g++ -x c++ -std=c++14 -DJUP_STOX_TEST stox.hpp -o stox
//     ./stox
//
#ifdef JUP_STOX_TEST
#define JUP_STOX_IMPLEMENTATION
#include "global.hpp"
#endif /* JUP_STOX_TEST */

#ifndef JUP_STOX_HEADER
#define JUP_STOX_HEADER

/**
 * General number parsing routines. The string str is parsed (specifics depend on the value of
 * flags) and an error code is returned. If the code is 0, then the operation was successful, else
 * it is an index into the jup_err_messages array, where a user-friendly message can be found. Apart
 * from being zero/nonzero the error codes are not part of the public API of these functions and
 * subject to change.
 *
 * If the operation was successful, the parsed value is placed into the parameter into, else into
 * remains unmodified.
 *
 * The currently supported data types are 8/16/32/64-bit signed/unsigned integers and 32/64-bit
 * IEEE-754 floating point numbers.
 *
 * When parsing integers, currently no flags may be specified. The value will be represented
 * exactly, if possible, else an error will be raised. Only the value must be an integer, it may be
 * written in a form usually used for floating point numbers (e.g. 3.14159e5 is a valid).
 * 
 * When parsing floating point numbers, the flags jup_sto::ALLOW_INFINITY and jup_sto::ALLOW_NAN are
 * allowed. The enable parsing of the special values infinity and NaN, respectively. Parsing a
 * floating point number is not exact, of course. However, THIS FUNCTION IS NOT GUARATEED TO ROUND
 * CORRECTLY in general. This means that it is possible for the result to be off-by-one.
 * Empirically, round-trips (converting the number to a string with enough digits and then back)
 * work without fault, and parsing of random strings is wrong only ~0.00057% of the time for 64-bit
 * floats (it has not been observed to fail for 32-bit floats).
 *
 * The following formats are supported (all matching is case-insensitive):
 *   [+-]*(?=.)[0-9]*(\.[0-9]*)?(e[+-]*[0-9]+)?
 *     Base-10 number with optional fractional part and optional exponent.
 *   [+-]*0b(?=.)[01]*(.[01]*)?
 *     Base-2 number with optional fractional part
 *   [+-]*0(?=.)[0-7]*(.[0-7]*)?
 *     Base-8 number with optional fractional part
 *   [+-]*0x(?=.)[0-9a-f]*(.[0-9a-f]*)?
 *     Base-16 number with optional fractional part
 *   [+-]*(inf|infty|infinity)
 *     Infinity (for floating point values only, jup_sto::ALLOW_INFINITY flag must be set)
 *   [+-]*nan
 *     (quiet) NaN (for floating point values only, jup_sto::ALLOW_NAN flag must be set)
 *
 * Note that the (?=.) matches everything that is followed by at least one character, i.e. that is
 * not at the end of the string. To put it differently, the base specifier, either ("", "0b", "0" or
 * "0x") must not be followed by the end of the string.
 */
u16 jup_stox(Array_t<u8> str, u8*     into, u8 flags = 0);
u16 jup_stox(Array_t<u8> str, s8*     into, u8 flags = 0);
u16 jup_stox(Array_t<u8> str, u16*    into, u8 flags = 0);
u16 jup_stox(Array_t<u8> str, s16*    into, u8 flags = 0);
u16 jup_stox(Array_t<u8> str, u32*    into, u8 flags = 0);
u16 jup_stox(Array_t<u8> str, s32*    into, u8 flags = 0);
u16 jup_stox(Array_t<u8> str, u64*    into, u8 flags = 0);
u16 jup_stox(Array_t<u8> str, s64*    into, u8 flags = 0);
u16 jup_stox(Array_t<u8> str, float*  into, u8 flags = 0);
u16 jup_stox(Array_t<u8> str, double* into, u8 flags = 0);

/**
 * Flags for jup_stox. See jup_stox for documentation.
 */
namespace jup_sto {
    enum Parse_flags: u8 {
        // The values are subject to change.
        NONE = 0,
        ALLOW_INFINITY = 1,
        ALLOW_NAN = 2,
        _ONLY_INTEGER = 4, // This is internal
    };
}

/**
 * Strings for error messages.
 */
extern char const* jup_err_messages[];
extern const int jup_err_messages_size;

/**
 * The last returned error code.
 */
extern int jup_errno;


#endif /* JUP_STOX_HEADER */

#ifdef JUP_STOX_IMPLEMENTATION
#undef JUP_STOX_IMPLEMENTATION

char const* jup_err_messages[] = {
    /* 0 */ nullptr,
    /* 1 */ "String is empty",
    /* 2 */ "Invalid character",
    /* 3 */ "Out of range (too low)",
    /* 4 */ "Out of range (too high)",
    /* 5 */ "Unexpected end of input",
    /* 6 */ "Value too close to zero",
    /* 7 */ "Extra characters",
    /* 8 */ "Expected an integer"
    /* KEEP THE SIZE UPDATED! */
};
const int jup_err_messages_size = 9;

int jup_errno;

struct Number_sci {
    enum Type: u8 {
        NORMAL, T_INFINITY, T_NAN
    };
    
    u8 type;
    bool sign;
    u64 m; // mantissa
    int e; // exponent
};

/**
 * Converts a string into a number. This function returns imprecise results!  (Well, only a little
 * bit imprecise.)
 */
static u16 jup_sto_helper(Array_t<u8> str, Number_sci* into, u8 flags = 0) {
    assert(into);
    if (str.size == 0) return 1;

    bool sign = false;
    int i = 0;
    while (i < str.size and (str[i] == '-' or str[i] == '+')) {
        sign ^= str[i] == '-';
        ++i;
    }
    if (i == str.size) return 5;

    auto cmp_ci = [&str, &i](char const* s) {
        if (i + (int)std::strlen(s) > str.size) return false;
        for (int j = 0; j < (int)std::strlen(s); ++j) {
            if (str[i + j] != s[j] and str[i+j] != s[j] + 'A' - 'a') return false;
        }
        i += std::strlen(s);
        return true;
    };

    if (flags & jup_sto::ALLOW_INFINITY) {
        if (cmp_ci("infty") or cmp_ci("infinity") or cmp_ci("inf")) {
            if (i < str.size) return 7;
            *into = {Number_sci::T_INFINITY, sign, 0, 0};
            return 0;
        }
    } else if (flags & jup_sto::ALLOW_NAN) {
        if (cmp_ci("nan")) {
            if (i < str.size) return 7;
            *into = {Number_sci::T_NAN, sign, 0, 0};
            return 0;
        }
    }
    
    u64 base = 10;
    if (str[i] == '0' and i + 1 < str.size) {
        ++i;
        if (str[i] == 'x' or str[i] == 'X') {
            base = 16; ++i;
        } else if (str[i] == 'b' or str[i] == 'B') {
            base = 2; ++i;
        } else if ('0' <= str[i] and str[i] <= '9') {
            base = 8;
        } else {
            // nothing
        }
    }
    if (i == str.size) return 5;

    u64 m = 0;
    int exp = 0;
    bool overflow = false;
    bool do_exp = false;
    bool do_frac = false;
    for (; i < str.size; ++i) {
        char c = str[i];
        u64 val = 0;
        if ('0' <= c and c <= '9') {
            val = c - '0';
        } else if (base == 16 and 'a' <= c and c <= 'z') {
            val = c - 'a' + 10;
        } else if (base == 16 and 'A' <= c and c <= 'Z') {
            val = c - 'A' + 10;
        } else if (base == 10 and (c == 'e' or c == 'E')) {
            do_exp = true; ++i; break;
        } else if (c == '.') {
            do_frac = true; ++i; break;
        } else {
            return 2;
        }
        if (val >= base) { return 2; }

        if (not overflow) {
            u64 tmp;
            if (__builtin_mul_overflow(m, base, &tmp)) {
                overflow = true;
            } else if (__builtin_add_overflow(tmp, val, &tmp)) {
                overflow = true;
            } else {
                m = tmp;
            }
        }
        
        if (overflow) {
            // If we are doing integers, this does not fit
            if (flags & jup_sto::_ONLY_INTEGER) return sign ? 3 : 4;

            // For floats, ignore the other digits
            if (__builtin_add_overflow(exp, 1, &exp)) {
                return sign ? 3 : 4;
            }
        }
    }
    overflow = false;

    u64 frac = 0;
    u64 frac_exp = 0;
    if (do_frac) {
        for (; i < str.size; ++i) {
            char c = str[i];
            u64 val = 0;
            if ('0' <= c and c <= '9') {
                val = c - '0';
            } else if (base == 16 and 'a' <= c and c <= 'z') {
                val = c - 'a';
            } else if (base == 16 and 'A' <= c and c <= 'Z') {
                val = c - 'A';
            } else if (base == 10 and (c == 'e' or c == 'E')) {
                do_exp = true; ++i; break;
            } else {
                return 2;
            }
            if (val >= base) { return 2; }

            if (not overflow) {
                u64 tmp_frac, tmp_frac_exp;
                overflow |= __builtin_mul_overflow(frac, base, &tmp_frac);
                overflow |= __builtin_add_overflow(tmp_frac, val, &tmp_frac);
                overflow |= __builtin_add_overflow(frac_exp, 1, &tmp_frac_exp);
                if (not overflow) {
                    frac = tmp_frac;
                    frac_exp = tmp_frac_exp;
                }
            }
            
            if (overflow) {
                // Unless we are parsing an integer, any leftover fractional part can be ignored. If
                // we are, we should still accept a long fractional part of 0.
                if (val != 0 and (flags & jup_sto::_ONLY_INTEGER)) {
                    return 8;
                }
            }
        }

        if (frac == 0) {
            frac_exp = 0;
        }
    }
    overflow = false;
    
    if (do_exp) {
        bool exp_sign = false;
        u64 exp_val = 0;
        while (i < str.size and (str[i] == '-' or str[i] == '+')) {
            exp_sign ^= str[i] == '-';
            ++i;
        }
        if (i == str.size) return 5;
    
        for (; i < str.size; ++i) {
            char c = str[i];
            u64 val = 0;
            if ('0' <= c and c <= '9') {
                val = c - '0';
            } else {
                return 2;
            }

            if (__builtin_mul_overflow(exp_val, 10, &exp_val)) {
                overflow = true; break;
            } else if (__builtin_add_overflow(exp_val, val, &exp_val)) {
                overflow = true; break;
            }
        }

        if (exp_sign) {
            if (__builtin_sub_overflow(exp, exp_val, &exp)) overflow = true;    
        } else {
            if (__builtin_add_overflow(exp, exp_val, &exp)) overflow = true;    
        }

        if (overflow) return exp_sign ? (sign ? 3 : 4) : 6;
    }

    // Add the fractional part
    for (u64 i = 0; i < frac_exp; ++i) {
        if (__builtin_mul_overflow(m, base, &m)) {
            overflow = true; break;
        }
    }
    if (overflow) return sign ? 3 : 4;
    if (__builtin_add_overflow(m, frac, &m)) {
        overflow = true;
    } else if (__builtin_sub_overflow(exp, frac_exp, &exp)) {
        overflow = true;
    }
    if (overflow) return sign ? 3 : 4;

    // Convert exponent into base 2
    // TODO: Implement correct rounding
    int exp_;
    if (m == 0 or exp == 0) {
        exp_ = 0;
    } else if (exp > 0 and std::log2(base) * (double)exp < __builtin_clzll(m)) {
        assert(base == 10);
        // If the number is an 64-bit integer, represent it directly
        for (int i = 0; i < exp; ++i) {
            m *= 10;
        }
        exp_ = 0;
    } else if (base == 10) {
        u64 shift = __builtin_clzll(m);
        m <<= shift;
        if (exp > (int)((double)std::numeric_limits<int>::max() / std::log2(base))
            or exp < (int)((double)std::numeric_limits<int>::min() / std::log2(base))) {
            return exp < 0 ? 6 : (sign ? 3: 4);
        }
        exp_ = (int)(std::ceil(exp * std::log2((long double)base)));

        long double m_ld = (long double)m;
        if (exp > 0) {
            long double d = 10.l;
            u64 i = (u64)exp;
            while (i) {
                if (i & 1) m_ld *= d;
                d *= d;
                i >>= 1;
            }
        } else {
            long double d = 10.l;
            u64 i = (u64)-exp;
            while (i) {
                if (i & 1) m_ld /= d;
                d *= d;
                i >>= 1;
            }
        }

        m = (u64)(std::ldexp(m_ld, -exp_));
        exp_ -= shift;
    
    } else {
        assert(exp == 0);
        exp_ = 0;
    }

    // Shift mantissa to the right
    if (m != 0) {
        exp_ += __builtin_ctzll(m);
        m >>= __builtin_ctzll(m);
    }

    if (flags & jup_sto::_ONLY_INTEGER) {
        if (exp_ != 0) {
            assert(m != 0);
            if (exp_ > 0 and __builtin_clzll(m) >= exp_) {
                m <<= exp_;
                exp_ = 0;
            } else {
                return 8;
            }
        }
    }

    *into = {Number_sci::NORMAL, sign, m, exp_};
    return 0;
}

u16 number_sci_to_real(Number_sci n, float* into) {
    // Shift to the left
    if (n.m != 0) {
        n.e -=  __builtin_clzll(n.m);
        n.m <<= __builtin_clzll(n.m);
    }
    
    // We interpret n.m as a real in [0, 2)
    n.e += 63;
    assert(n.m == 0 or (n.m & (1ull << 63)));

    union {
        u32 d = 0;
        float result;
    };

    // sign
    d ^= ((u32)n.sign << 31);
    
    if (n.type == Number_sci::T_NAN) {
        d = 0x7fc00000ull;
    } else if (n.type == Number_sci::T_INFINITY) {
        d |= 0x7f800000ull;
    } else if (n.type == Number_sci::NORMAL)  {
        // Take care of the normal-denormal cutoff
        if (n.e == -127) {
            if (n.m >= 0xffffff0000000000ull) {
                n.m = 1ull << 63;
                n.e += 1;
            }
        } else if (n.e <= -150) {
            if (n.e == -150 and n.m > (1ull << 63)) {
                n.m = 1ull << 63;
                n.e += 1;
            } else {
                n.m = 0;
                n.e = 0;
            }
        }
        
        if (n.m == 0) {
            // nothing, mantissa and exponent are already zero
        } else if ((n.e > -127 and n.e < 127) or (n.e == 127 and n.m < 0xffffff7000000000ull)) {
            // normalized
            u64 m_ = n.m >> 40;
            s64 exp_ = n.e;
            u64 round = n.m & 0xffffffffffull;

            if (not (round & 0x8000000000ull)) {
                // round down
            } else if (round & 0x7fffffffffull) {
                // round up
                m_ += 1;
            } else {
                assert(round == 0x8000000000ull);
                // round towards even
                m_ += m_ & 1;
            }
            if (m_ & (1ull << 24)) {
                m_ >>= 1;
                exp_ += 1;
            }

            assert(m_ < (1ull << 25) and exp_ >= -126 and exp_ <= 1027);
            d |= (m_ & ~(1ull << 23));
            d |= (u64)(exp_ + 127) << 23;
        } else if (n.e >= -149 and n.e <= -127) {
            // denormalized
            u64 shift = 41 - (127 + n.e);
            u64 m_ = n.m >> shift;
            u64 round = (n.m >> (shift - 41)) & 0xffffffffffull;

            if (not (round & 0x8000000000ull)) {
                // round down
            } else if (round & 0x7fffffffffull) {
                // round up
                m_ += 1;
            } else {
                assert(round == 0x8000000000ull);
                // round towards even
                m_ += m_ & 1;
            }
            
            assert(m_ < (1ull << 24));
            d |= m_;
            // exponent already 0
        } else {
            return n.e < 0 ? 6 : (n.sign ? 3 : 4);
        }
    } else {
        assert(false);
    }

    *into = result;
    return 0;
}

u16 number_sci_to_real(Number_sci n, double* into) {
    // Shift to the left
    if (n.m != 0) {
        n.e -=  __builtin_clzll(n.m);
        n.m <<= __builtin_clzll(n.m);
    }
    
    // We interpret n.m as a real in [0, 2)
    n.e += 63;
    assert(n.m == 0 or (n.m & (1ull << 63)));

    union {
        u64 d = 0;
        double result;
    };

    // sign
    d ^= ((u64)n.sign << 63);
    
    if (n.type == Number_sci::T_NAN) {
        d = 0x7ff8000000000000ull;
    } else if (n.type == Number_sci::T_INFINITY) {
        d |= 0x7ff0000000000000ull;
    } else if (n.type == Number_sci::NORMAL)  {
        // Take care of the normal-denormal cutoff
        if (n.e == -1023) {
            if (n.m >= 0xfffffffffffff800ull) {
                n.m = 1ull << 63;
                n.e += 1;
            }
        } else if (n.e <= -1075) {
            if (n.e == -1075 and n.m > (1ull << 63)) {
                n.m = 1ull << 63;
                n.e += 1;
            } else {
                n.m = 0;
                n.e = 0;
            }
        }
        
        if (n.m == 0) {
            // nothing, mantissa and exponent are already zero
        } else if ((n.e > -1023 and n.e < 1023) or (n.e == 1023 and n.m < 0xfffffffffffffc00ull)) {
            // normalized
            u64 m_ = n.m >> 11;
            s64 exp_ = n.e;
            u64 round = n.m & 0x7ffull;

            if (not (round & 0x400)) {
                // round down
            } else if (round & 0x3ff) {
                // round up
                m_ += 1;
            } else {
                assert(round == 0x400);
                // round towards even
                m_ += m_ & 1;
            }

            if (m_ & (1ull << 53ull)) {
                m_ >>= 1;
                exp_ += 1;
            }

            assert(m_ < (1ull << 54) and exp_ >= -1022 and exp_ <= 1023);
            d |= (m_ & ~(1ull << 52));
            d |= (u64)(exp_ + 1023) << 52;
        } else if (n.e >= -1074 and n.e <= -1023) {
            // denormalized
            u64 shift = 12 - (1023 + n.e);
            u64 m_ = n.m >> shift;
            u64 round = (n.m >> (shift - 12)) & 0xfffull;

            if (not (round & 0x800)) {
                // round down
            } else if (round & 0x7ff) {
                // round up
                m_ += 1;
            } else {
                assert(round == 0x800);
                // round towards even
                m_ += m_ & 1;
            }

            assert(m_ < (1ull << 53));
            d |= m_;
            // exponent already 0
        } else {
            return n.e < 0 ? 6 : (n.sign ? 3 : 4);
        }
    } else {
        assert(false);
    }

    *into = result;
    return 0;
}

template <typename T>
u16 jup_stox_helper_int(Array_t<u8> str, T* into, u8 flags) {
    static_assert(sizeof(T) <= sizeof(u64) and std::is_integral<T>::value, "");
    assert(into);
    assert(flags == 0);
    
    Number_sci n;
    if (auto code = jup_sto_helper(str, &n, flags | jup_sto::_ONLY_INTEGER)) {
        jup_errno = code;
        return jup_errno;
    }

    assert(n.e == 0); // due to the _ONLY_INTEGER flag
    assert(n.type == Number_sci::NORMAL);

    if (std::is_unsigned<T>::value and n.sign and n.m) {
        jup_errno = 3;
        return jup_errno;
    }
    if (n.m > (u64)std::numeric_limits<T>::max() + n.sign) {
        jup_errno = n.sign ? 3 : 4;
        return jup_errno;
    }

    *into = n.sign ? (T)-n.m : (T)n.m;
    jup_errno = 0;
    return jup_errno;
}

template <typename T>
u16 jup_stox_helper_real(Array_t<u8> str, T* into, u8 flags) {
    static_assert(std::numeric_limits<T>::is_iec559, "");
    assert(into);

    Number_sci n;
    if (auto code = jup_sto_helper(str, &n, flags)) {
        jup_errno = code;
        return jup_errno;
    }
    
    jup_errno = number_sci_to_real(n, into);;
    return jup_errno;
}

u16 jup_stox(Array_t<u8> str, u8*  into, u8 flags) { return jup_stox_helper_int(str, into, flags); }
u16 jup_stox(Array_t<u8> str, s8*  into, u8 flags) { return jup_stox_helper_int(str, into, flags); }
u16 jup_stox(Array_t<u8> str, u16* into, u8 flags) { return jup_stox_helper_int(str, into, flags); }
u16 jup_stox(Array_t<u8> str, s16* into, u8 flags) { return jup_stox_helper_int(str, into, flags); }
u16 jup_stox(Array_t<u8> str, u32* into, u8 flags) { return jup_stox_helper_int(str, into, flags); }
u16 jup_stox(Array_t<u8> str, s32* into, u8 flags) { return jup_stox_helper_int(str, into, flags); }
u16 jup_stox(Array_t<u8> str, u64* into, u8 flags) { return jup_stox_helper_int(str, into, flags); }
u16 jup_stox(Array_t<u8> str, s64* into, u8 flags) { return jup_stox_helper_int(str, into, flags); }
u16 jup_stox(Array_t<u8> str, float*  into, u8 flags) { return jup_stox_helper_real(str, into, flags); }
u16 jup_stox(Array_t<u8> str, double* into, u8 flags) { return jup_stox_helper_real(str, into, flags); }

#endif /* JUP_STOX_IMPLEMENTATION */

#ifdef JUP_STOX_TEST
#undef JUP_STOX_TEST

void print(double d) { fprintf(stderr, "%.20f / %.20e", d, d); }
void print(float  d) { fprintf(stderr, "%.10f / %.10e", d, d); }
void print(s64 d) { fprintf(stderr, "%lld / %llx", d, d); }
void print(u64 d) { fprintf(stderr, "%llu / %llx", d, d); }
void print(s32 d) { fprintf(stderr, "%lld / %llx", (s64)d, (s64)d); }
void print(u32 d) { fprintf(stderr, "%llu / %llx", (s64)d, (s64)d); }
void print(s16 d) { fprintf(stderr, "%lld / %llx", (s64)d, (s64)d); }
void print(u16 d) { fprintf(stderr, "%llu / %llx", (s64)d, (s64)d); }
void print(s8  d) { fprintf(stderr, "%lld / %llx", (s64)d, (s64)d); }
void print(u8  d) { fprintf(stderr, "%llu / %llx", (s64)d, (s64)d); }

template <typename T>
void check_valid(char const* s, T result) {
    T tmp;
    u16 code = jup_stox({(u8*)s, (s64)strlen(s)}, &tmp);
    if (code) {
        fprintf(stderr, "Error on testcase '%s'. Expected\n    ", s);
        print(result);
        if (code < jup_err_messages_size) {
            fprintf(stderr, "\ngot error:\n    %s  (code %d)\n", jup_err_messages[code], code);
        } else {
            fprintf(stderr, "\ngot error code out of bounds (code %d, size %d)\n", code, jup_err_messages_size);
        }
        exit(10);
    } else if (memcmp(&result, &tmp, sizeof(T))) {
        fprintf(stderr, "Error on testcase '%s'. Expected\n    ", s);
        print(result);
        fprintf(stderr, "\ngot\n    ");
        print(tmp);
        fprintf(stderr, "\n");
        exit(11);
    }
}

template <typename T>
void check_error(char const* s) {
    T tmp;
    u16 code = jup_stox({(u8*)s, (s64)strlen(s)}, &tmp);
    if (not code) {
        fprintf(stderr, "Error on testcase '%s'. Expected error but got\n    ", s);
        print(tmp);
        exit(12);
    } else if (code >= jup_err_messages_size) {
        fprintf(stderr, "Error on testcase '%s'. Expected error and got one, but the error code is"
            " out of bounds (code %d, size %d)\n", s, code, jup_err_messages_size);
        exit(13);
    }
}


int main() {
    puts("Testing special cases...");
    
    check_valid<u8>("0", 0);
    check_valid<u8>("1", 1);
    check_valid<u8>("255", 255);
    check_valid<u8>("0.0", 0);
    check_valid<u8>("1.0", 1);
    check_valid<u8>("0.", 0);
    check_valid<u8>("255.0", 255);
    check_valid<u8>("255.00000000000000000000000000", 255);
    check_valid<u8>("2.55e2", 255);
    check_valid<u8>("2.55E+2", 255);
    check_valid<u8>("2.55e--+-+--+-2", 255);
    check_valid<u8>("255000000000000000e-15", 255);
    check_valid<u8>("--1", 1);
    check_valid<u8>("--+-++-+--+-++-+-+-+--1", 1);
    check_error<u8>("1.e");
    check_error<u8>("--+-++-+--+--+-+-+-+--1");
    check_error<u8>("256");
    check_error<u8>("-1");
    check_error<u8>("65535");
    check_error<u8>("65536");
    check_error<u8>("4294967295");
    check_error<u8>("4294967296");
    check_error<u8>("18446744073709551615");
    check_error<u8>("18446744073709551616");
    check_error<u8>("-65535");
    check_error<u8>("-65536");
    check_error<u8>("-4294967295");
    check_error<u8>("-4294967296");
    check_error<u8>("-18446744073709551615");
    check_error<u8>("-18446744073709551616");
    check_error<u8>("2.5501e2");
    check_error<u8>("2.5501e-+---+-++--2");
    check_error<u8>("2.550000000000000000000000000001e2");
    check_error<u8>("2.56e2");
    check_error<u8>("255000000000000000e-16");
    check_valid<u8>("0b0", 0);
    check_valid<u8>("0B0", 0);
    check_valid<u8>("0b1", 1);
    check_valid<u8>("0b11111111", 255);
    check_valid<u8>("0b0.0", 0);
    check_valid<u8>("0b1.0", 1);
    check_valid<u8>("0b11111111.0", 255);
    check_valid<u8>("0b11111111.00000000000000000000000000", 255);
    check_error<u8>("0b100000000");
    check_error<u8>("0b1111111111111111");
    check_error<u8>("0b10000000000000000");
    check_error<u8>("0b11111111111111111111111111111111");
    check_error<u8>("0b100000000000000000000000000000000");
    check_error<u8>("0b1111111111111111111111111111111111111111111111111111111111111111");
    check_error<u8>("0b10000000000000000000000000000000000000000000000000000000000000000");
    check_error<u8>("-0b1111111111111111");
    check_error<u8>("-0b10000000000000000");
    check_error<u8>("-0b11111111111111111111111111111111");
    check_error<u8>("-0b100000000000000000000000000000000");
    check_error<u8>("-0b1111111111111111111111111111111111111111111111111111111111111111");
    check_error<u8>("-0b10000000000000000000000000000000000000000000000000000000000000000");
    check_valid<u8>("00", 0);
    check_valid<u8>("01", 1);
    check_valid<u8>("0377", 255);
    check_valid<u8>("00.0", 0);
    check_valid<u8>("01.0", 1);
    check_valid<u8>("0377.0", 255);
    check_valid<u8>("0377.00000000000000000000000000", 255);
    check_error<u8>("0100000000");
    check_error<u8>("0177777");
    check_error<u8>("010000000000000000");
    check_error<u8>("037777777777");
    check_error<u8>("0100000000000000000000000000000000");
    check_error<u8>("01777777777777777777777");
    check_error<u8>("010000000000000000000000000000000000000000000000000000000000000000");
    check_error<u8>("-0177777");
    check_error<u8>("-010000000000000000");
    check_error<u8>("-037777777777");
    check_error<u8>("-0100000000000000000000000000000000");
    check_error<u8>("-01777777777777777777777");
    check_error<u8>("-010000000000000000000000000000000000000000000000000000000000000000");
    check_valid<u8>("0x0", 0);
    check_valid<u8>("0x1", 1);
    check_valid<u8>("0xff", 255);
    check_valid<u8>("0xfF", 255);
    check_valid<u8>("0x0.0", 0);
    check_valid<u8>("0x1.0", 1);
    check_valid<u8>("0xff.0", 255);
    check_valid<u8>("0xff.00000000", 255);
    check_error<u8>("0x100");
    check_error<u8>("0xffff");
    check_error<u8>("0x10000");
    check_error<u8>("0xffffffff");
    check_error<u8>("0x100000000");
    check_error<u8>("0xffffffffffffffff");
    check_error<u8>("0x10000000000000000");
    check_error<u8>("-0xffff");
    check_error<u8>("-0x10000");
    check_error<u8>("-0xffffffff");
    check_error<u8>("-0x100000000");
    check_error<u8>("-0xffffffffffffffff");
    check_error<u8>("-0x10000000000000000");
    check_error<u8>("inf");
    check_error<u8>("inF");
    check_error<u8>("nan");
    check_error<u8>("naN");
    check_error<u8>("-inf");
    check_error<u8>("-inF");
    check_error<u8>("-nan");
    check_error<u8>("-naN");

    check_valid<s8>("0", 0);
    check_valid<s8>("1", 1);
    check_valid<s8>("127", 127);
    check_error<s8>("128");
    check_error<s8>("255");
    check_error<s8>("256");
    check_error<s8>("32767");
    check_error<s8>("32768");
    check_error<s8>("65535");
    check_error<s8>("65536");
    check_error<s8>("2147483647");
    check_error<s8>("2147483648");
    check_error<s8>("4294967295");
    check_error<s8>("4294967296");
    check_error<s8>("9223372036854775807");
    check_error<s8>("9223372036854775808");
    check_error<s8>("18446744073709551615");
    check_error<s8>("18446744073709551616");
    check_valid<s8>("-0", -0);
    check_valid<s8>("-1", -1);
    check_valid<s8>("-127", -127);
    check_valid<s8>("-128", -128);
    check_error<s8>("-255");
    check_error<s8>("-256");
    check_error<s8>("-32767");
    check_error<s8>("-32768");
    check_error<s8>("-65535");
    check_error<s8>("-65536");
    check_error<s8>("-2147483647");
    check_error<s8>("-2147483648");
    check_error<s8>("-4294967295");
    check_error<s8>("-4294967296");
    check_error<s8>("-9223372036854775807");
    check_error<s8>("-9223372036854775808");
    check_error<s8>("-18446744073709551615");
    check_error<s8>("-18446744073709551616");

    check_valid<u16>("0", 0);
    check_valid<u16>("1", 1);
    check_valid<u16>("127", 127);
    check_valid<u16>("128", 128);
    check_valid<u16>("255", 255);
    check_valid<u16>("256", 256);
    check_valid<u16>("32767", 32767);
    check_valid<u16>("32768", 32768);
    check_valid<u16>("65535", 65535);
    check_error<u16>("65536");
    check_error<u16>("2147483647");
    check_error<u16>("2147483648");
    check_error<u16>("4294967295");
    check_error<u16>("4294967296");
    check_error<u16>("9223372036854775807");
    check_error<u16>("9223372036854775808");    
    check_error<u16>("18446744073709551615");
    check_error<u16>("18446744073709551616");
    check_valid<u16>("-0", -0);
    check_error<u16>("-1");
    check_error<u16>("-127");
    check_error<u16>("-128");
    check_error<u16>("-255");
    check_error<u16>("-256");
    check_error<u16>("-32767");
    check_error<u16>("-32768");
    check_error<u16>("-65535");
    check_error<u16>("-65536");
    check_error<u16>("-2147483647");
    check_error<u16>("-2147483648");
    check_error<u16>("-4294967295");
    check_error<u16>("-4294967296");
    check_error<u16>("-9223372036854775807");
    check_error<u16>("-9223372036854775808");    
    check_error<u16>("-18446744073709551615");
    check_error<u16>("-18446744073709551616");

    check_valid<s16>("0", 0);
    check_valid<s16>("1", 1);
    check_valid<s16>("127", 127);
    check_valid<s16>("128", 128);
    check_valid<s16>("255", 255);
    check_valid<s16>("256", 256);
    check_valid<s16>("32767", 32767);
    check_error<s16>("32768");
    check_error<s16>("65535");
    check_error<s16>("65536");
    check_error<s16>("2147483647");
    check_error<s16>("2147483648");
    check_error<s16>("4294967295");
    check_error<s16>("4294967296");
    check_error<s16>("9223372036854775807");
    check_error<s16>("9223372036854775808");    
    check_error<s16>("18446744073709551615");
    check_error<s16>("18446744073709551616");
    check_valid<s16>("-0", -0);
    check_valid<s16>("-1", -1);
    check_valid<s16>("-127", -127);
    check_valid<s16>("-128", -128);
    check_valid<s16>("-255", -255);
    check_valid<s16>("-256", -256);
    check_valid<s16>("-32767", -32767);
    check_valid<s16>("-32768", -32768);
    check_error<s16>("-65535");
    check_error<s16>("-65536");
    check_error<s16>("-2147483647");
    check_error<s16>("-2147483648");
    check_error<s16>("-4294967295");
    check_error<s16>("-4294967296");
    check_error<s16>("-9223372036854775807");
    check_error<s16>("-9223372036854775808");    
    check_error<s16>("-18446744073709551615");
    check_error<s16>("-18446744073709551616");

    check_valid<u32>("0", 0);
    check_valid<u32>("1", 1);
    check_valid<u32>("127", 127);
    check_valid<u32>("128", 128);
    check_valid<u32>("255", 255);
    check_valid<u32>("256", 256);
    check_valid<u32>("32767", 32767);
    check_valid<u32>("32768", 32768);
    check_valid<u32>("65535", 65535);
    check_valid<u32>("65536", 65536);
    check_valid<u32>("2147483647", 2147483647);
    check_valid<u32>("2147483648", 2147483648ull);
    check_valid<u32>("4294967295", 4294967295ull);
    check_error<u32>("4294967296");
    check_error<u32>("9223372036854775807");
    check_error<u32>("9223372036854775808");    
    check_error<u32>("18446744073709551615");
    check_error<u32>("18446744073709551616");
    check_valid<u32>("-0", -0);
    check_error<u32>("-1");
    check_error<u32>("-127");
    check_error<u32>("-128");
    check_error<u32>("-255");
    check_error<u32>("-256");
    check_error<u32>("-32767");
    check_error<u32>("-32768");
    check_error<u32>("-65535");
    check_error<u32>("-65536");
    check_error<u32>("-2147483647");
    check_error<u32>("-2147483648");
    check_error<u32>("-4294967295");
    check_error<u32>("-4294967296");
    check_error<u32>("-9223372036854775807");
    check_error<u32>("-9223372036854775808");    
    check_error<u32>("-18446744073709551615");
    check_error<u32>("-18446744073709551616");

    check_valid<s32>("0", 0);
    check_valid<s32>("1", 1);
    check_valid<s32>("127", 127);
    check_valid<s32>("128", 128);
    check_valid<s32>("255", 255);
    check_valid<s32>("256", 256);
    check_valid<s32>("32767", 32767);
    check_valid<s32>("32768", 32768);
    check_valid<s32>("65535", 65535);
    check_valid<s32>("65536", 65536);
    check_valid<s32>("2147483647", 2147483647);
    check_error<s32>("2147483648");
    check_error<s32>("4294967295");
    check_error<s32>("4294967296");
    check_error<s32>("9223372036854775807");
    check_error<s32>("9223372036854775808");    
    check_error<s32>("18446744073709551615");
    check_error<s32>("18446744073709551616");
    check_valid<s32>("-0", -0);
    check_valid<s32>("-1", -1);
    check_valid<s32>("-127", -127);
    check_valid<s32>("-128", -128);
    check_valid<s32>("-255", -255);
    check_valid<s32>("-256", -256);
    check_valid<s32>("-32767", -32767);
    check_valid<s32>("-32768", -32768);
    check_valid<s32>("-65535", -65535);
    check_valid<s32>("-65536", -65536);
    check_valid<s32>("-2147483647", -2147483647);
    check_valid<s32>("-2147483648", -2147483648);
    check_error<s32>("-4294967295");
    check_error<s32>("-4294967296");
    check_error<s32>("-9223372036854775807");
    check_error<s32>("-9223372036854775808");    
    check_error<s32>("-18446744073709551615");
    check_error<s32>("-18446744073709551616");

    check_valid<u64>("0", 0);
    check_valid<u64>("1", 1);
    check_valid<u64>("127", 127);
    check_valid<u64>("128", 128);
    check_valid<u64>("255", 255);
    check_valid<u64>("256", 256);
    check_valid<u64>("32767", 32767);
    check_valid<u64>("32768", 32768);
    check_valid<u64>("65535", 65535);
    check_valid<u64>("65536", 65536);
    check_valid<u64>("2147483647", 2147483647);
    check_valid<u64>("2147483648", 2147483648ull);
    check_valid<u64>("4294967295", 4294967295ull);
    check_valid<u64>("4294967296", 4294967296ull);
    check_valid<u64>("9223372036854775807", 9223372036854775807ull);
    check_valid<u64>("9223372036854775808", 9223372036854775808ull);    
    check_valid<u64>("18446744073709551615", 18446744073709551615ull);
    check_error<u64>("18446744073709551616");
    check_valid<u64>("-0", -0);
    check_error<u64>("-1");
    check_error<u64>("-127");
    check_error<u64>("-128");
    check_error<u64>("-255");
    check_error<u64>("-256");
    check_error<u64>("-32767");
    check_error<u64>("-32768");
    check_error<u64>("-65535");
    check_error<u64>("-65536");
    check_error<u64>("-2147483647");
    check_error<u64>("-2147483648");
    check_error<u64>("-4294967295");
    check_error<u64>("-4294967296");
    check_error<u64>("-9223372036854775807");
    check_error<u64>("-9223372036854775808");    
    check_error<u64>("-18446744073709551615");
    check_error<u64>("-18446744073709551616");

    check_valid<s64>("0", 0);
    check_valid<s64>("1", 1);
    check_valid<s64>("127", 127);
    check_valid<s64>("128", 128);
    check_valid<s64>("255", 255);
    check_valid<s64>("256", 256);
    check_valid<s64>("32767", 32767);
    check_valid<s64>("32768", 32768);
    check_valid<s64>("65535", 65535);
    check_valid<s64>("65536", 65536);
    check_valid<s64>("2147483647", 2147483647);
    check_valid<s64>("2147483648", 2147483648ll);
    check_valid<s64>("4294967295", 4294967295ll);
    check_valid<s64>("4294967296", 4294967296ll);
    check_valid<s64>("9223372036854775807", 9223372036854775807ll);
    check_error<s64>("9223372036854775808");    
    check_error<s64>("18446744073709551615");
    check_error<s64>("18446744073709551616");
    check_valid<s64>("-0", -0);
    check_valid<s64>("-1", -1);
    check_valid<s64>("-127", -127);
    check_valid<s64>("-128", -128);
    check_valid<s64>("-255", -255);
    check_valid<s64>("-256", -256);
    check_valid<s64>("-32767", -32767);
    check_valid<s64>("-32768", -32768);
    check_valid<s64>("-65535", -65535);
    check_valid<s64>("-65536", -65536);
    check_valid<s64>("-2147483647", -2147483647);
    check_valid<s64>("-2147483648", -2147483648);
    check_valid<s64>("-4294967295", -4294967295ll);
    check_valid<s64>("-4294967296", -4294967296ll);
    check_valid<s64>("-9223372036854775807", -9223372036854775807ll);
    check_valid<s64>("-9223372036854775808", -9223372036854775808ull);    
    check_error<s64>("-18446744073709551615");
    check_error<s64>("-18446744073709551616");

    
    //puts("Starting random testing...");
    //for (s64 iter = 0;; ++iter) {
    //    printf("Iteration %lld...\n, iter");
    //    
    //}
}

#endif /* JUP_STOX_TEST */

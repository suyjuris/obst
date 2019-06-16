
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
    }
    if (flags & jup_sto::ALLOW_NAN) {
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

    if (do_frac) {
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
            } else {
                return 2;
            }
            if (val >= base) { return 2; }

            if (not overflow) {
                u64 tmp_m;
                int tmp_exp;
                overflow |= __builtin_mul_overflow(m, base, &tmp_m);
                overflow |= __builtin_add_overflow(tmp_m, val, &tmp_m);
                overflow |= __builtin_sub_overflow(exp, 1, &tmp_exp);
                if (not overflow) {
                    m = tmp_m;
                    exp = tmp_exp;
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
        if (m_ld == INFINITY or m_ld == -INFINITY or m_ld == 0) {
            return exp < 0 ? 6 : (sign ? 3: 4);
        }

        m = (u64)(std::ldexp(m_ld, -exp_));
        exp_ -= shift;
    
    } else {
        switch (base) {
        case 2: exp_ = exp; break;
        case 8: exp_ = exp * 3; break;
        case 16: exp_ = exp * 4; break;
        default: assert(false /* where is that case coming from?! */);
        }
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

#include <sstream>

void print(double d, FILE* f=stderr) { fprintf(f, "%.20f / %.20e", d, d); }
void print(float  d, FILE* f=stderr) { fprintf(f, "%.10f / %.10e", d, d); }
void print(s64 d,    FILE* f=stderr) { fprintf(f, "%lld / %llx", d, d); }
void print(u64 d,    FILE* f=stderr) { fprintf(f, "%llu / %llx", d, d); }
void print(s32 d,    FILE* f=stderr) { fprintf(f, "%lld / %llx", (s64)d, (s64)d); }
void print(u32 d,    FILE* f=stderr) { fprintf(f, "%llu / %llx", (s64)d, (s64)d); }
void print(s16 d,    FILE* f=stderr) { fprintf(f, "%lld / %llx", (s64)d, (s64)d); }
void print(u16 d,    FILE* f=stderr) { fprintf(f, "%llu / %llx", (s64)d, (s64)d); }
void print(s8  d,    FILE* f=stderr) { fprintf(f, "%lld / %llx", (s64)d, (s64)d); }
void print(u8  d,    FILE* f=stderr) { fprintf(f, "%llu / %llx", (s64)d, (s64)d); }

FILE* global_logfile;

template <typename T>
void check_valid(char const* s, T result, u8 flags = 0) {
    T tmp;
    u16 code = jup_stox({(u8*)s, (s64)strlen(s)}, &tmp, flags);
    if (code) {
        fprintf(stderr, "Error on testcase '%s'", s);
        if (flags) fprintf(stderr, " (with flags %x)", (int)flags);
        fprintf(stderr, ". Expected\n    ");
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
void check_code(char const* s, T result, u16 r_code, u8 flags = 0) {
    T tmp;
    u16 code = jup_stox({(u8*)s, (s64)strlen(s)}, &tmp, flags);
    if (code != r_code) {
        fprintf(stderr, "Error on testcase '%s'", s);
        if (flags) fprintf(stderr, " (with flags %x)", (int)flags);
        fprintf(stderr, ". Expected code %hd, got %hd\n", r_code, code);
        exit(51);
    } else if (code == 0 and memcmp(&result, &tmp, sizeof(T))) {
        fprintf(stderr, "Error on testcase '%s'. Expected\n    ", s);
        print(result);
        fprintf(stderr, "\ngot\n    ");
        print(tmp);
        fprintf(stderr, "\n");
        exit(52);
    }
}

template <typename T>
void check_error(char const* s, u8 flags = 0) {
    T tmp;
    u16 code = jup_stox({(u8*)s, (s64)strlen(s)}, &tmp, flags);
    if (not code) {
        fprintf(stderr, "Error on testcase '%s'", s);
        if (flags) fprintf(stderr, " (with flags %x)", (int)flags);
        fprintf(stderr, ". Expected error but got\n    ");
        print(tmp);
        exit(12);
    } else if (code >= jup_err_messages_size) {
        fprintf(stderr, "Error on testcase '%s'. Expected error and got one, but the error code is"
            " out of bounds (code %d, size %d)\n", s, code, jup_err_messages_size);
        exit(13);
    }
}

struct Counter { u64 total = 0, fail = 0; };

template <typename T>
void check_almost(char const* s, T result, Counter* c) {
    ++c->total;
    
    T tmp;
    u16 code = jup_stox({(u8*)s, (s64)strlen(s)}, &tmp);
    if (result == INFINITY and code == 4) {
        // For these checks we compare with strtof, which retuns inf on large values
    } else if (result == -INFINITY and code == 3) {
        // see above
    } else if (code) {
        fprintf(stderr, "Error on testcase '%s'", s);
        fprintf(stderr, ". Expected\n    ");
        print(result);
        if (code < jup_err_messages_size) {
            fprintf(stderr, "\ngot error:\n    %s  (code %d)\n", jup_err_messages[code], code);
        } else {
            fprintf(stderr, "\ngot error code out of bounds (code %d, size %d)\n", code, jup_err_messages_size);
        }
        exit(20);
    } else if (memcmp(&result, &tmp, sizeof(T))) {
        T next = std::nextafter(result, tmp);
        if (memcmp(&next, &tmp, sizeof(T))) {
            fprintf(stderr, "Error on testcase '%s'. Expected\n    ", s);
            print(result);
            fprintf(stderr, "\ngot\n    ");
            print(tmp);
            fprintf(stderr, "\n");
            exit(21);
        } else {
            if (global_logfile) {
                fprintf(global_logfile, "Error on testcase '%s'. Expected\n    ", s);
                print(result, global_logfile);
                fprintf(global_logfile, "\ngot\n    ");
                print(tmp, global_logfile);
                fprintf(global_logfile, "\n\n");
            }
            ++c->fail;
        }
    }
}

u64 rand_state = 0xd1620b2a7a243d4bull;
u64 rand_get() {
    u64 x = rand_state;
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    rand_state = x;
    return x * 0x2545f4914f6cdd1dull;
}

void print_usage(char* argv0) {
    printf("Usage:\n  %s [-o <file>]\n  %s -x\n  %s --help\n\nThis is the test for the "
        "floating point parsing routine.\n\nOptions:\n", argv0, argv0, argv0);
    puts("  -o <file>");
    puts("    Write a logfile, consisting of the inputs where the parser is off-by-one.\n");
    puts("  -x");
    puts("    Instead of running the tests, simply parse a single number from stdin and exit. (This option is provided to enable fuzz-testing.)\n");
    puts("  --help");
    puts("    You can probably guess what this option does.");
    exit(2);
}

int main(int argc, char** argv) {
    bool fuzz_mode = false;
    for (s64 i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-o") == 0) {
            if (global_logfile != nullptr) {
                fprintf(stderr, "Error: option -o specified twice (--help for help)\n");
                exit(33);
            } else if (i+1 >= argc) {
                fprintf(stderr, "Error: no argument for option -o (--help for help)\n");
                exit(34);
            }
            global_logfile = fopen(argv[i+1], "w");
            if (global_logfile == nullptr) {
                fprintf(stderr, "Error: could not open file '%s' for writing (--help for help)\n", argv[i+1]);
                exit(35);
            }
            
            ++i;
        } else if (strcmp(argv[i], "-x") == 0) {
            fuzz_mode = true;
        } else if (strcmp(argv[i], "--help") == 0 or strcmp(argv[i], "-h") == 0) {
            print_usage(argv[0]); // exits
        } else {
            fprintf(stderr, "Error: Unknown option or argument '%s' (--help for help)\n", argv[i]);
            exit(35);
        }
    }

    if (fuzz_mode) {
        char* buf = (char*)calloc(4096, 1);
        fread(buf, 4095, 1, stdin);
        
        u8 _u8; u16 _u16; u32 _u32; u64 _u64;
        s8 _s8; s16 _s16; s32 _s32; s64 _s64;
        float _float; double _double;
        Array_t<u8> s {(u8*)buf, (s64)strlen(buf)};
        jup_stox(s, &_float);
        jup_stox(s, &_double);
        jup_stox(s, &_float, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
        jup_stox(s, &_double, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
        jup_stox(s, &_u8);
        jup_stox(s, &_u16);
        jup_stox(s, &_u32);
        jup_stox(s, &_u64);
        jup_stox(s, &_s8);
        jup_stox(s, &_s16);
        jup_stox(s, &_s32);
        jup_stox(s, &_s64);
        return 0;
    }
    
    puts("Testing special cases...");
    // This is basically comparing with the compiler's floating point parser
    
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

    check_valid<float>("0", 0.f);
    check_valid<float>("+0", 0.f);
    check_valid<float>("-0", -0.f);
    check_valid<float>("1", 1.f);
    check_valid<float>("0.0", 0.f);
    check_valid<float>("1.0", 1.f);
    check_valid<float>("0.", 0.f);
    check_valid<float>("255.0", 255.f);
    check_valid<float>("255.00000000000000000000000000", 255.f);
    check_valid<float>("2.55e2", 255.f);
    check_valid<float>("2.55E+2", 255.f);
    check_valid<float>("2.55e--+-+--+-2", 255.f);
    check_valid<float>("255000000000000000e-15", 255.f);
    check_valid<float>("--1", 1.f);
    check_valid<float>("--+-++-+--+-++-+-+-+--1", 1.f);
    check_error<float>("1.e");
    check_valid<float>("-1", -1.f);
    check_valid<float>("65535", 65535.f);
    check_valid<float>("65536", 65536.f);
    check_valid<float>("4294967295", 4294967295.f);
    check_valid<float>("4294967296", 4294967296.f);
    check_valid<float>("18446744073709551615", 18446744073709551615.f);
    check_valid<float>("18446744073709551616", 18446744073709551616.f);
    check_valid<float>("-65535", -65535.f);
    check_valid<float>("-65536", -65536.f);
    check_valid<float>("-4294967295", -4294967295.f);
    check_valid<float>("-4294967296", -4294967296.f);
    check_valid<float>("-18446744073709551615", -18446744073709551615.f);
    check_valid<float>("-18446744073709551616", -18446744073709551616.f);
    check_valid<float>("2.5501e-+---+-++--2", 2.5501e-2);
    check_valid<float>("2.550000000000000000000000000001e2", 2.55e2f);
    check_valid<float>("0b0", 0.f);
    check_valid<float>("0B0", 0.f);
    check_valid<float>("0b1", 1.f);
    check_valid<float>("0b11111111", 255.f);
    check_valid<float>("0b0.0", 0.f);
    check_valid<float>("0b1.01", 1.25f);
    check_valid<float>("0b11111111.0", 255.f);
    check_valid<float>("0b11111111.00000000000000000000000000", 255.f);
    check_valid<float>("00", 0.f);
    check_valid<float>("01", 1.f);
    check_valid<float>("0377", 255.f);
    check_valid<float>("00.0", 0.f);
    check_valid<float>("01.0", 1.f);
    check_valid<float>("0377.0", 255.f);
    check_valid<float>("0377.00000000000000000000000000", 255.f);
    check_valid<float>("0x0", 0.f);
    check_valid<float>("0x1", 1.f);
    check_valid<float>("0xff", 255.f);
    check_valid<float>("0xfF", 255.f);
    check_valid<float>("0x0.0", 0.f);
    check_valid<float>("0x1.0", 1.f);
    check_valid<float>("0x1.2", 1.125f);
    check_valid<float>("0xff.0", 255.f);
    check_valid<float>("0xff.00000000", 255.f);
    check_valid<float>("inf", INFINITY, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
    check_valid<float>("inF", INFINITY, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
    check_valid<float>("nan", NAN, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
    check_valid<float>("naN", NAN, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
    check_valid<float>("-inf", -INFINITY, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
    check_valid<float>("-inF", -INFINITY, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
    check_valid<float>("-nan", NAN, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
    check_valid<float>("-naN", NAN, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
    check_valid<float>("1e37", 1e37f);
    check_error<float>("1e39");
    check_valid<float>("1e-55", 0.f);
    check_valid<float>("0.2", 0.2f);
    check_valid<float>("123456789", 123456789.f);
    check_error<float>("1234567890123456789012345678901234567890");
    check_error<float>("inf");
    check_error<float>("inF");
    check_error<float>("nan");
    check_error<float>("naN");
    check_error<float>("-inf");
    check_error<float>("-inF");
    check_error<float>("-nan");
    check_error<float>("-naN");

#define check_all(s, _u8z,_u16z,_u32z,_u64z,_s8z,_s16z,_s32z,_s64z,_floatz,_doublez,_float2z,_double2z, v) \
    check_code<u8>    (s, (u8)v,     _u8z    ); \
    check_code<u16>   (s, (u16)v,    _u16z   ); \
    check_code<u32>   (s, (u32)v,    _u32z   ); \
    check_code<u64>   (s, (u64)v,    _u64z   ); \
    check_code<s8>    (s, (s8)v,     _s8z    ); \
    check_code<s16>   (s, (s16)v,    _s16z   ); \
    check_code<s32>   (s, (s32)v,    _s32z   ); \
    check_code<s64>   (s, (s64)v,    _s64z   ); \
    check_code<float> (s, (float)v,  _floatz ); \
    check_code<double>(s, (double)v, _doublez); \
    check_code<float> (s, (float)v,  _float2z,  jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN); \
    check_code<double>(s, (double)v, _double2z, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);

    check_all("0", 0,0,0,0,0,0,0,0,0,0,0,0, 0);
    check_all("1", 0,0,0,0,0,0,0,0,0,0,0,0, 1);
    check_all("127", 0,0,0,0,0,0,0,0,0,0,0,0, 127);
    check_all("128", 0,0,0,0,4,0,0,0,0,0,0,0, 128);
    check_all("255", 0,0,0,0,4,0,0,0,0,0,0,0, 255);
    check_all("256", 4,0,0,0,4,0,0,0,0,0,0,0, 256);
    check_all("32767", 4,0,0,0,4,0,0,0,0,0,0,0, 32767);
    check_all("32768", 4,0,0,0,4,4,0,0,0,0,0,0, 32768);
    check_all("65535", 4,0,0,0,4,4,0,0,0,0,0,0, 65535);
    check_all("65536", 4,4,0,0,4,4,0,0,0,0,0,0, 65536);
    check_all("2147483647", 4,4,0,0,4,4,0,0,0,0,0,0, 2147483647);
    check_all("2147483648", 4,4,0,0,4,4,4,0,0,0,0,0, 2147483648);
    check_all("4294967295", 4,4,0,0,4,4,4,0,0,0,0,0, 4294967295);
    check_all("4294967296", 4,4,4,0,4,4,4,0,0,0,0,0, 4294967296ull);
    check_all("9223372036854775807", 4,4,4,0,4,4,4,0,0,0,0,0, 9223372036854775807ull);
    check_all("9223372036854775808", 4,4,4,0,4,4,4,4,0,0,0,0, 9223372036854775808ull);
    check_all("18446744073709551615", 4,4,4,0,4,4,4,4,0,0,0,0, 18446744073709551615ull);
    check_all("18446744073709551616", 4,4,4,4,4,4,4,4,0,0,0,0, 18446744073709551616.0);
    check_all("-0", 0,0,0,0,0,0,0,0,0,0,0,0, -0.0);
    check_all("-1", 3,3,3,3,0,0,0,0,0,0,0,0, -1);
    check_all("-127", 3,3,3,3,0,0,0,0,0,0,0,0, -127);
    check_all("-128", 3,3,3,3,0,0,0,0,0,0,0,0, -128);
    check_all("-255", 3,3,3,3,3,0,0,0,0,0,0,0, -255);
    check_all("-256", 3,3,3,3,3,0,0,0,0,0,0,0, -256);
    check_all("-32767", 3,3,3,3,3,0,0,0,0,0,0,0, -32767);
    check_all("-32768", 3,3,3,3,3,0,0,0,0,0,0,0, -32768);
    check_all("-65535", 3,3,3,3,3,3,0,0,0,0,0,0, -65535);
    check_all("-65536", 3,3,3,3,3,3,0,0,0,0,0,0, -65536);
    check_all("-2147483647", 3,3,3,3,3,3,0,0,0,0,0,0, -2147483647);
    check_all("-2147483648", 3,3,3,3,3,3,0,0,0,0,0,0, -2147483648);
    check_all("-4294967295", 3,3,3,3,3,3,3,0,0,0,0,0, -4294967295);
    check_all("-4294967296", 3,3,3,3,3,3,3,0,0,0,0,0, -4294967296);
    check_all("-9223372036854775807", 3,3,3,3,3,3,3,0,0,0,0,0, -9223372036854775807ll);
    check_all("-9223372036854775808", 3,3,3,3,3,3,3,0,0,0,0,0, -9223372036854775808.0);
    check_all("-18446744073709551615", 3,3,3,3,3,3,3,3,0,0,0,0, -18446744073709551615.0);
    check_all("-18446744073709551616", 3,3,3,3,3,3,3,3,0,0,0,0, -18446744073709551616.0);

    // The following testcases were generated using afl (http://lcamtuf.coredump.cx/afl/). They are
    // mostly regression tests (I fixed all the crashes that were discovered, of course). They
    // should cover just about everything.
    check_all("", 1,1,1,1,1,1,1,1,1,1,1,1, 0);
    check_all("+0188.0e-+20", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+3188.", 4,0,0,0,4,0,0,0,0,0,0,0, 3188);
    check_all("+3E88.0e-+20", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+318..0e-+20", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+", 5,5,5,5,5,5,5,5,5,5,5,5, 0);
    check_all("+3188.0", 4,0,0,0,4,0,0,0,0,0,0,0, 3188);
    check_all("+3188.0e", 5,5,5,5,5,5,5,5,5,5,5,5, 0);
    check_all("+3188.0e-\x01", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+-", 5,5,5,5,5,5,5,5,5,5,5,5, 0);
    check_all("+++++++++-\xff\xff+-\xe8++\x03+++", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("++20", 0,0,0,0,0,0,0,0,0,0,0,0, 20);
    check_all("0", 0,0,0,0,0,0,0,0,0,0,0,0, 0);
    check_all("E31;\x80G0e-+20", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("i%\x03\x05Q%\x05\x05{{{{", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("++++++718+s1P)", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("018", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("031N20", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+3.18", 8,8,8,8,8,8,8,8,0,0,0,0, 3.180000e+00);
    check_all("++31", 0,0,0,0,0,0,0,0,0,0,0,0, 31);
    check_all("+++\x10+++++G+++", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("n\x02\x7f\u0017d", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("++++++++++++++++++4", 0,0,0,0,0,0,0,0,0,0,0,0, 4);
    check_all("E3", 0,0,0,0,0,0,0,0,0,0,0,0, 0);
    check_all("+++++++?B8\x02\u0010e-B70\x10", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+318800e-++318800e-+", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("-8", 3,3,3,3,0,0,0,0,0,0,0,0, -8);
    check_all(".00", 0,0,0,0,0,0,0,0,0,0,0,0, 0);
    check_all("07.0\x10", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+3188.1e-++++++++++++++++20", 8,8,8,8,8,8,8,8,0,0,0,0, 3.188100e-17);
    check_all("+0BQ8.11", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+3188.0e-3206", 8,8,8,8,8,8,8,8,0,0,0,0, 0.000000e+00);
    check_all("+3188.0e-+-06", 4,4,0,0,4,4,4,0,0,0,0,0, 3188000000);
    check_all("188e9", 4,4,4,0,4,4,4,0,0,0,0,0, 188000000000);
    check_all("+3188e-+20101886", 6,6,6,6,6,6,6,6,6,6,6,6, 0);
    check_all("+3188.0188e-+201886", 6,6,6,6,6,6,6,6,6,6,6,6, 0);
    check_all("+318188e06", 4,4,4,0,4,4,4,0,0,0,0,0, 318188000000);
    check_all("+3183188.0e8", 4,4,4,0,4,4,4,0,0,0,0,0, 318318800000000);
    check_all("118E8", 4,4,4,0,4,4,4,0,0,0,0,0, 11800000000);
    check_all("+3188.0e32206", 4,4,4,4,4,4,4,4,4,4,4,4, 0);
    check_all("66666166666666666666", 4,4,4,4,4,4,4,4,0,0,0,0, 6.666616666666666e+19);
    check_all("333333333333333333333331883188.0e-+188", 4,4,4,4,4,4,4,4,0,0,0,0, 3.33333333333333333333331883188e-159);
    check_all("-.", 0,0,0,0,0,0,0,0,0,0,0,0, -0.);
    check_all("11188111111111111111111", 4,4,4,4,4,4,4,4,0,0,0,0, 1.1188111111111111111111e+22);
    check_all("+3188.0e+++++", 5,5,5,5,5,5,5,5,5,5,5,5, 0);
    check_all("+31777777777777777777777777777788.0e-+188206", 4,4,4,4,4,4,4,4,6,6,6,6, 0);
    check_all("+0e666666666666666666666666-+206", 6,6,6,6,6,6,6,6,6,6,6,6, 0);
    check_all("+3188.0e-+20660666666666666666666", 4,4,4,4,4,4,4,4,4,4,4,4, 0);
    check_all("18718818818718818", 4,4,4,0,4,4,4,0,0,0,0,0, 18718818818718818ull);
    check_all("+3188.0e-320", 8,8,8,8,8,8,8,8,0,0,0,0, 3.188000e-317);
    check_all("+3188.0e+3", 4,4,0,0,4,4,0,0,0,0,0,0, 3188000);
    check_all("++0xG", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("e+++++++++++", 5,5,5,5,5,5,5,5,5,5,5,5, 0);
    check_all("1111111111111111111110e-+", 4,4,4,4,4,4,4,4,5,5,5,5, 0);
    check_all("0XX", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X", 5,5,5,5,5,5,5,5,5,5,5,5, 0);
    check_all("8888888888888888888888888888.", 4,4,4,4,4,4,4,4,0,0,0,0, 8.888888888888888e+27);
    check_all("0000000000000000000000000000000002", 0,0,0,0,0,0,0,0,0,0,0,0, 2);
    check_all("3E3", 4,0,0,0,4,0,0,0,0,0,0,0, 3000);
    check_all("+3188.2222222222222222220e", 8,8,8,8,8,8,8,8,5,5,5,5, 0);
    check_all("18888.2222222222222222220", 8,8,8,8,8,8,8,8,0,0,0,0, 1.88882222222222222e+04);
    check_all("+3188888888888888888.0e-1{0", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+333333333333333333333333318810e-++318918860e-M", 4,4,4,4,4,4,4,4,2,2,2,2, 0);
    check_all("07.8\x10", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("07.00", 0,0,0,0,0,0,0,0,0,0,0,0, 7);
    check_all("+3E528", 8,8,8,8,8,8,8,8,4,4,4,4, 0);
    check_all("iniiiiiHG", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("9999999999999999999", 4,4,4,0,4,4,4,4,0,0,0,0, 9999999999999999999ull);
    check_all("+3188.0e8831885206", 6,6,6,6,6,6,6,6,6,6,6,6, 0);
    check_all("+378.0e-8888888885206", 4,4,4,4,4,4,4,4,4,4,4,4, 0);
    check_all("+3188.e-1881881886", 6,6,6,6,6,6,6,6,6,6,6,6, 0);
    check_all("+3188.0e-+-05", 4,4,0,0,4,4,0,0,0,0,0,0, 318800000);
    check_all("-.0e-+6666666666666666666666+\u001706", 3,3,3,3,3,3,3,3,3,3,3,3, 0);
    check_all("-38.0e66666", 3,3,3,3,3,3,3,3,3,3,3,3, 0);
    check_all("6666616666E666666666", 4,4,4,4,4,4,4,4,4,4,4,4, 0);
    check_all("0xxO188\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x8o188\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x8_188\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x8\u007f188\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x[O188\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x8a188\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0xdO188\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x\x7fO188\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x[rTTTTTTTTTTTTTTTTTTTTTT<TTTT\x18", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x\u007fdF8J68J61", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x8CCCCCCCCCCC\"CO188\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0xCA1TTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0xddddddV18d", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x8.0x@O18\u001b6<6166", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0x.O1TTT", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("Na63", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+3188.0e-327", 8,8,8,8,8,8,8,8,0,0,0,0, 4.940656e-324);
    check_all("+3188.0e13", 4,4,4,0,4,4,4,0,0,0,0,0, 31880000000000000ull);
    check_all("+3188.2222222222222220220e18818", 8,8,8,8,8,8,8,8,4,4,4,4, 0);
    check_all("07000000000000000000000000", 4,4,4,4,4,4,4,4,0,0,0,0, 33056565380087516495872.);
    check_all("0X.^E7.Z188p", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.{E7.Z188p", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.18818188p0=", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.\u007fE7.Z188p018;2", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.`E7p0M", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.IIIIII=", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X188A8E\".ZK8%p0", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.KKKK8KKKKKKKK", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.AAAA8AAAAAAAA", 8,8,8,8,8,8,8,8,0,0,0,0, 0.66666475931803370614);
    check_all("0X.88\x7f\x7f\x7f\x7f\x7f\x7f\x7f\xff\u007f8p2", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0XEEEEEEEEEEEDEEEEE6EJ.Z6EJ.Z1=1=", 4,4,4,4,4,4,4,4,2,2,2,2, 0);
    check_all("0Xa8800faaaaaaaaa00G", 4,4,4,4,4,4,4,4,2,2,2,2, 0);
    check_all("0X616E7E\xec.Z188p0=\u00042", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("iNfIiiiHG", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("iNfin\xe9iHG", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("iNfTiiiHG", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("iNfiiiiH", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("iNf", 2,2,2,2,2,2,2,2,2,2,0,0, INFINITY);
    check_all("INFxiiiHG188", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("-378.0e-8888888885206", 3,3,3,3,3,3,3,3,3,3,3,3, 0);
    check_all("+113188188.0e-52", 8,8,8,8,8,8,8,8,0,0,0,0, 113188188e-52);
    check_all("++13188188.0e-52", 8,8,8,8,8,8,8,8,0,0,0,0, 13188188e-52);
    check_all("+313188111111111881811E118800611111188111111111881811E11111.0e-52", 4,4,4,4,4,4,4,4,6,6,6,6, 0);
    check_all("+3188.0e-+-15", 4,4,4,0,4,4,4,0,0,0,0,0, 3188000000000000000ull);
    check_all("+3188.0e-+-35", 8,8,8,8,8,8,8,8,0,0,0,0, 3.188000e+38);
    check_all("18e17", 4,4,4,0,4,4,4,0,0,0,0,0, 1800000000000000000ull);
    check_all("-666616666E666666666", 3,3,3,3,3,3,3,3,3,3,3,3, 0);
    check_all("666661618818661881861666E6666666666", 4,4,4,4,4,4,4,4,6,6,6,6, 0);
    check_all("6666666666666666616666E666666616", 4,4,4,4,4,4,4,4,4,4,4,4, 0);
    check_all("0x.O1[8\xffV188\xff<TTTT\x1b", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("NAn88.0e-+20", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("NAN", 2,2,2,2,2,2,2,2,2,2,0,0, NAN);
    check_all("18818.011111111111111111111111e13", 8,8,8,8,8,8,8,8,0,0,0,0, 18818.011111111111111111111111e13);
    check_all("0X.EE000000000000000008\u007f8800=EE5", 8,8,8,8,8,8,8,8,2,2,2,2, 0);
    check_all("iNftyiiHG", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("iNftYiiHG", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("iNfiNIiHG", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("iNfiNiTHG", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("+3000000000000000000000061600188.0e-+-15", 4,4,4,4,4,4,4,4,4,0,4,0, 3.000000e+45);
    check_all("-66661666666666666666666666E667666666", 3,3,3,3,3,3,3,3,3,3,3,3, 0);
    check_all("0X.n188p0188pp0188p00A=", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.n188p0188pP0188p0001", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.d188f0188ff0188f000=", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.d188f0188fF0188f0001", 8,8,8,8,8,8,8,8,0,0,0,0, 0.81849575614609226815);
    check_all("iNfiNitYG", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("iNfiNityG", 2,2,2,2,2,2,2,2,2,2,7,7, 0);
    check_all("+++++E718+s1P)", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("+88003141988008.0e-52", 8,8,8,8,8,8,8,8,0,0,0,0, 88003141988008e-52);
    check_all("+8888003131882818281888.0e-00313003131882818281888.0", 4,4,4,4,4,4,4,4,4,4,4,4, 0);
    check_all("+88003131888003131882808281888.0e-5880031318882", 4,4,4,4,4,4,4,4,4,4,4,4, 0);
    check_all("+318831888888888888888888888888888888888.0e-++0.20", 4,4,4,4,4,4,4,4,2,2,2,2, 0);
    check_all(".1011111818811111118616111118811111181881111111861611111881111111111", 8,8,8,8,8,8,8,8,0,0,0,0, 0.10111118188111111);
    check_all("-38888888888888888888888.0e66188", 3,3,3,3,3,3,3,3,3,3,3,3, 0);
    check_all("0Xa8800300faaaaaaaa00faaaaaaaaaa00G", 4,4,4,4,4,4,4,4,2,2,2,2, 0);
    check_all("666661618818661881861666E-666666666", 4,4,4,4,4,4,4,4,6,6,6,6, 0);
    check_all("6666666666666666616666E666666666666666666166616", 4,4,4,4,4,4,4,4,6,6,6,6, 0);
    check_all("+3000000000000000000000061600188.0e-+--5", 4,4,4,4,4,4,4,4,0,0,0,0, 3e25);
    check_all("---------------------------------------------------\xee", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("8e18", 4,4,4,0,4,4,4,0,0,0,0,0, 8000000000000000000);
    check_all("0X.ppppppppppppppppppppppppphpppppppppphdhhhhhhhhhhhhhhhhhpppppppppppppphpppppppppphhp\u00892", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.fffffffffffffffffffffffffhffffffffffhdhhhhhhhhhhhhhhhhhffffffffffffffhffffffffffhhf\u00892", 8,8,8,8,8,8,8,8,2,2,2,2, 0);
    check_all("0X.paaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\x9d\x9d\x9d\x9d\x9d\x9d\x9d\x9d\x9d\x9d\x9d\u009daaaaaaaaap\u00892", 2,2,2,2,2,2,2,2,2,2,2,2, 0);

    // Some of the strings are quite long, compress them
    #define A "AAAAAAAAAAAAAAAAAAAA"
    #define D "dddddddddddddddddddd"
    #define E "EEEEEEEEEEE"
    #define M "--------------------"
    #define I "11111111111111111111"
    
    check_all("-88003133131882818281888.0e-52180000000000000000000000000888.0e-52", 3,3,3,3,3,3,3,3,3,3,3,3, 0);
    check_all("000000000000000000000000000000000000000000000313100000000000000000000000d", 4,4,4,4,4,4,4,4,2,2,2,2, 0);
    check_all("E000000000000000000000000000000000000313100000000000000000000000d", 6,6,6,6,6,6,6,6,6,6,6,6, 0);
    check_all("-38888888818661881861666E-3183888888881666666", 3,3,3,3,3,3,3,3,3,3,3,3, 0);
    check_all("0X" E E E E E"EEEEEEEEEEDp0", 4,4,4,4,4,4,4,4,2,2,2,2, 0);
    check_all("0B.1", 8,8,8,8,8,8,8,8,0,0,0,0, 5.000000e-01);
    check_all("+3188.0e" M M"---------+3188.0e-+", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.EEkkkkkkkkk" E"E18EEEE88008EEEE8" E"EEEEEE4E8" E E"EE18EEEE880088@", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0X.EEaaaaaaaaa" E"E18EEEE88008EEEE8" E"EEEEEE4E8" E E"EE18EEEE880088@", 8,8,8,8,8,8,8,8,2,2,2,2, 0);
    check_all("+3000000000000000000000061600188.0e-+---", 4,4,4,4,4,4,4,4,5,5,5,5, 0);
    check_all("0x" D D D D D D D D D D D D D D D D D D D D D D"ddddddddddV1@d", 4,4,4,4,4,4,4,4,2,2,2,2, 0);
    check_all("0x" D D D D D D D D D D D D"dddddddddddddddd", 4,4,4,4,4,4,4,4,4,0,4,0, 1.5580007168806738e+308);
    check_all("0xdddddddddddddb" D D D D D D D D D D D D"dd", 4,4,4,4,4,4,4,4,4,0,4,0, 1.5580007168806736e+308);
    check_all("0X.8888888" A A"AAAAAAAAAAAAAAK" A A A A A A A A A A A"AAAAAAAAAAAAA7" A A A A A A A A A A A"AAAAAAAAAAAAAAA0118" A A A A A A A A A A A A A A A A A A A A A A A A A A"AA\x7f\u00ff" A"AAAAAAA", 8,8,8,8,8,8,8,8,2,2,2,2, 0);
    check_all("" M M M M M M M M"----------------\xad" M M M M M M M M M M M M M M M M"---------------", 2,2,2,2,2,2,2,2,2,2,2,2, 0);
    check_all("0B." I I I I I I I I I I I I I I I I I I I"11111111", 8,8,8,8,8,8,8,8,0,0,0,0, 1);
    check_all("8e8888", 4,4,4,4,4,4,4,4,4,4,4,4, 0);
    check_all("273188.0e-272222", 6,6,6,6,6,6,6,6,6,6,6,6, 0);

    // In case there are a lot of new test-cases to add to the above, you can add them to the array
    // and run the program. The results of executing the code will be output. Of course, there is no
    // guaruantee that those results are actually correct, so you will have to verify them manually.
    char const* cases_to_generate[] = { };
    for (s64 i = 0; i < (s64)(sizeof(cases_to_generate) / sizeof(cases_to_generate[0])); ++i) {
        u8 _u8; u16 _u16; u32 _u32; u64 _u64;
        s8 _s8; s16 _s16; s32 _s32; s64 _s64 = 0;
        float _float; double _double = 0;
        Array_t<u8> s {(u8*)cases_to_generate[i], (s64)strlen(cases_to_generate[i])};
        u16 _floatc   = jup_stox(s, &_float);
        u16 _doublec  = jup_stox(s, &_double);
        u16 _floatc2  = jup_stox(s, &_float, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
        u16 _doublec2 = jup_stox(s, &_double, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
        u16 _u8c      = jup_stox(s, &_u8);
        u16 _u16c     = jup_stox(s, &_u16);
        u16 _u32c     = jup_stox(s, &_u32);
        u16 _u64c     = jup_stox(s, &_u64);
        u16 _s8c      = jup_stox(s, &_s8);
        u16 _s16c     = jup_stox(s, &_s16);
        u16 _s32c     = jup_stox(s, &_s32);
        u16 _s64c     = jup_stox(s, &_s64);
        printf("%hu %hu %hu %hu %hu %hu %hu %hu %hu %hu %hu %hu ",
        _u8c, _u16c, _u32c, _u64c, _s8c , _s16c, _s32c, _s64c, _floatc, _doublec, _floatc2, _doublec2);
        if (_s64c == 0) {
            printf("%lld.0\n", _s64);
        } else if (_u64c == 0) {
            printf("%llu.0\n", _u64);
        } else if (_doublec2 == 0) {
            printf("%.15e\n", _double);
        } else {
            printf("0\n");
        }
    }
    if (sizeof(cases_to_generate)) exit(0);
        

    puts("Starting random testing for off-by-one errors (this will never stop)...");
    // Now we compare with the runtime library

    Counter f_printf_uni, f_printf_den, f_iostream_uni, f_iostream_den;
    Counter d_printf_uni, d_printf_den, d_iostream_uni, d_iostream_den;
    Counter f_generate, d_generate;
    
    constexpr s64 buf_size = 4096;
    char* buf = (char*)calloc(buf_size, 1);
    std::stringstream os;

    union { u32 fu = 0; float  f; };
    union { u64 du = 0; double d; };

#define O(x) x.fail, x.total, (double)x.fail / (double)x.total
    
    for (u64 iter = 0;; ++iter) {
        if (!((iter-1)&iter) and iter) {
            printf("Iteration %lld (rand_state = 0x%llx)...\n", iter, rand_state);
            printf("  float,  printf,   uniform   %6lld / %10lld (%.5f%%)\n", O(f_printf_uni));
            printf("  float,  printf,   denormal  %6lld / %10lld (%.5f%%)\n", O(f_printf_den));
            printf("  float,  iostream, uniform   %6lld / %10lld (%.5f%%)\n", O(f_iostream_uni));
            printf("  float,  iostream, denormal  %6lld / %10lld (%.5f%%)\n", O(f_iostream_den));
            printf("  double, printf,   uniform   %6lld / %10lld (%.5f%%)\n", O(d_printf_uni));
            printf("  double, printf,   denormal  %6lld / %10lld (%.5f%%)\n", O(d_printf_den));
            printf("  double, iostream, uniform   %6lld / %10lld (%.5f%%)\n", O(d_iostream_uni));
            printf("  double, iostream, denormal  %6lld / %10lld (%.5f%%)\n", O(d_iostream_den));
            printf("  float,  generated           %6lld / %10lld (%.5f%%)\n", O(f_generate));
            printf("  double, generated           %6lld / %10lld (%.5f%%)\n", O(d_generate));
            puts("");
        }

        // printf / iostream tests.
        
        // Uniform over all bits
        for (s64 i = 0; i < 1000000; ++i) {
            fu = rand_get();
            fu += !(~fu & 0xffull << 23) << 23; // Do not generate inf and nan
            snprintf(buf, buf_size, "%.8e", f);
            check_almost(buf, f, &f_printf_uni);
        }
        // Denormals
        for (s64 i = 0; i < 100000; ++i) {
            fu = rand_get() & ~0x7f800000ull;
            snprintf(buf, buf_size, "%.8e", f);
            check_almost(buf, f, &f_printf_den);
        }

        // Same for iostream
        os.precision(9);
        for (s64 i = 0; i < 100000; ++i) {
            fu = rand_get();
            fu += !(~fu & 0xffull << 23) << 23; // Do not generate inf and nan
            os.clear(); os << f;
            os.read(buf, buf_size-1); buf[os.gcount()] = 0;
            check_almost(buf, f, &f_iostream_uni);
        }
        for (s64 i = 0; i < 100000; ++i) {
            fu = rand_get() & ~0x7f800000ull;
            os.clear(); os << f;
            os.read(buf, buf_size-1); buf[os.gcount()] = 0;
            check_almost(buf, f, &f_iostream_den);
        }

        // Now with doubles
        
        for (s64 i = 0; i < 1000000; ++i) {
            du = rand_get();
            du += (u64)!(~du & 0x7ffull << 52) << 52; // Do not generate inf and nan
            snprintf(buf, buf_size, "%.16e", f);
            check_almost(buf, f, &d_printf_uni);
        }
        for (s64 i = 0; i < 100000; ++i) {
            du = rand_get() & ~0x7ffull << 52;
            snprintf(buf, buf_size, "%.16e", f);
            check_almost(buf, f, &d_printf_den);
        }
        os.precision(17);
        for (s64 i = 0; i < 100000; ++i) {
            du = rand_get();
            du += (u64)!(~du & 0x7ffull << 52) << 52; // Do not generate inf and nan
            os.clear(); os << f;
            os.read(buf, buf_size-1); buf[os.gcount()] = 0;
            check_almost(buf, f, &d_iostream_uni);
        }
        for (s64 i = 0; i < 100000; ++i) {
            du = rand_get() & ~0x7ffull << 52;
            os.clear(); os << f;
            os.read(buf, buf_size-1); buf[os.gcount()] = 0;
            check_almost(buf, f, &d_iostream_den);
        }

        // Now let's generate the strings at random
        for (s64 i = 0; i < 100000; ++i) {
            // This only generates base-10 strings, as they are the only ones where precision matters.
            char* p = buf;
            u64 r = rand_get();
            if (r>>0 & 1) *p++ = '-';
            if ((r>>0 & 3) == 2) *p++ = '+';
            r += (!(r & 0xffc)) << 2;
            u64 digits = r>>2 & 31;
            if (digits > 1) {*p++ = rand_get() %  9 + '1'; --digits;}
            while (digits)  {*p++ = rand_get() % 10 + '0'; --digits;}
            u64 frac = r>>7 & 31;
            if (frac or (r>>12 & 1)) *p++ = '.';
            while (frac)     {*p++ = rand_get() % 10 + '0'; --frac;}
            if (r>>13 & 1) {
                *p++ = 'e';
                if (r>>14 & 1) *p++ = '-';
                if ((r>>14 & 3) == 2) *p++ = '+';
                u64 exp = (r>>16&1) + (r>>17&1);
                            {*p++ = rand_get() %  3 + '1';}
                while (exp) {*p++ = rand_get() % 10 + '0'; --exp;}
            }
            *p++ = 0;

            f = strtof(buf, nullptr);
            check_almost(buf, f, &f_generate);
            d = strtod(buf, nullptr);
            check_almost(buf, d, &d_generate);
        }
    }
}

#endif /* JUP_STOX_TEST */

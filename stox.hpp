
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
bool check_almost(char const* s, T result, Counter* c) {
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

    char const* tmp[] = {
        "",
        "+0188.0e-+20",
        "+3188.",
        "+3E88.0e-+20",
        "+318..0e-+20",
        "+",
        "+3188.0",
        "+3188.0e",
        "+3188.0e-\x01",
        "+-",
        "+++++++++-\xff\xff+-\xe8++\x03+++",
        "++20",
        "0",
        "E31;\x80G0e-+20",
        "i%\x03\x05Q%\x05\x05{{{{",
        "++++++718+s1P)",
        "018",
        "031N20",
        "+3.18",
        "++31",
        "+++\x10+++++G+++",
        "n\x02\x7f\u0017d",
        "++++++++++++++++++4",
        "E3",
        "+++++++?B8\x02\u0010e-B70\x10",
        "+318800e-++318800e-+",
        "-8",
        ".00",
        "07.0\x10",
        "+3188.1e-++++++++++++++++20",
        "+0BQ8.11",
        "+3188.0e-3206",
        "+3188.0e-+-06",
        "188e9",
        "+3188e-+20101886",
        "+3188.0188e-+201886",
        "+318188e06",
        "+3183188.0e8",
        "118E8",
        "+3188.0e32206",
        "66666166666666666666",
        "333333333333333333333331883188.0e-+188",
        "-.",
        "11188111111111111111111",
        "+3188.0e+++++",
        "+31777777777777777777777777777788.0e-+188206",
        "+0e666666666666666666666666-+206",
        "+3188.0e-+20660666666666666666666",
        "18718818818718818",
        "+3188.0e-320",
        "+3188.0e+3",
        "++0xG",
        "e+++++++++++",
        "1111111111111111111110e-+",
        "0XX",
        "0X",
        "8888888888888888888888888888.",
        "0000000000000000000000000000000002",
        "3E3",
        "+3188.2222222222222222220e",
        "18888.2222222222222222220",
        "+3188888888888888888.0e-1{0",
        "+333333333333333333333333318810e-++318918860e-M",
        "07.8\x10",
        "07.00",
        "+3E528",
        "iniiiiiHG",
        "9999999999999999999",
        "+3188.0e8831885206",
        "+378.0e-8888888885206",
        "+3188.e-1881881886",
        "+3188.0e-+-05",
        "-.0e-+6666666666666666666666+\u001706",
        "-38.0e66666",
        "6666616666E666666666",
        "0xxO188\xffV188\xff<TTTT\x1b",
        "0x8o188\xffV188\xff<TTTT\x1b",
        "0x8_188\xffV188\xff<TTTT\x1b",
        "0x8\u007f188\xffV188\xff<TTTT\x1b",
        "0x[O188\xffV188\xff<TTTT\x1b",
        "0x8a188\xffV188\xff<TTTT\x1b",
        "0xdO188\xffV188\xff<TTTT\x1b",
        "0x\x7fO188\xffV188\xff<TTTT\x1b",
        "0x[rTTTTTTTTTTTTTTTTTTTTTT<TTTT\x18",
        "0x\u007fdF8J68J61",
        "0x8CCCCCCCCCCC\"CO188\xffV188\xff<TTTT\x1b",
        "0xCA1TTT\x1b",
        "0xddddddV18d",
        "0x8.0x@O18\u001b6<6166",
        "0x.O1TTT",
        "Na63",
        "+3188.0e-327",
        "+3188.0e13",
        "+3188.2222222222222220220e18818",
        "07000000000000000000000000",
        "0X.^E7.Z188p",
        "0X.{E7.Z188p",
        "0X.18818188p0=",
        "0X.\u007fE7.Z188p018;2",
        "0X.`E7p0M",
        "0X.IIIIII=",
        "0X188A8E\".ZK8%p0",
        "0X.KKKK8KKKKKKKK",
        "0X.88\x7f\x7f\x7f\x7f\x7f\x7f\x7f\xff\u007f8p2",
        "0XEEEEEEEEEEEDEEEEE6EJ.Z6EJ.Z1=1=",
        "0Xa8800faaaaaaaaa00G",
        "0X616E7E\xec.Z188p0=\u00042",
        "iNfIiiiHG",
        "iNfin\xe9iHG",
        "iNfTiiiHG",
        "iNfiiiiH",
        "iNf",
        "INFxiiiHG188",
        "-378.0e-8888888885206",
        "+113188188.0e-52",
        "++13188188.0e-52",
        "+313188111111111881811E118800611111188111111111881811E11111.0e-52",
        "+3188.0e-+-15",
        "+3188.0e-+-35",
        "18e17",
        "-666616666E666666666",
        "666661618818661881861666E6666666666",
        "6666666666666666616666E666666616",
        "0x.O1[8\xffV188\xff<TTTT\x1b",
        "NAn88.0e-+20",
        "NAN",
        "18818.011111111111111111111111e13",
        "0X.EE000000000000000008\u007f8800=EE5",
        "iNftyiiHG",
        "iNftYiiHG",
        "iNfiNIiHG",
        "iNfiNiTHG",
        "+3000000000000000000000061600188.0e-+-15",
        "-66661666666666666666666666E667666666",
        "0X.n188p0188pp0188p00A=",
        "0X.n188p0188pP0188p0001",
        "iNfiNitYG",
        "iNfiNityG",
        "+++++E718+s1P)",
        "+88003131888008.0e-52",
        "+8888003131882818281888.0e-00313003131882818281888.0",
        "+88003131888003131882808281888.0e-5880031318882",
        "+318831888888888888888888888888888888888.0e-++0.20",
        ".1011111818811111118616111118811111181881111111861611111881111111111",
        "-38888888888888888888888.0e66188",
        "0Xa8800300faaaaaaaa00faaaaaaaaaa00G",
        "666661618818661881861666E-666666666",
        "6666666666666666616666E666666666666666666166616",
        "+3000000000000000000000061600188.0e-+--5",
        "---------------------------------------------------\xee",
        "8e18",
        "0X.ppppppppppppppppppppppppphpppppppppphdhhhhhhhhhhhhhhhhhpppppppppppppphpppppppppphhp\u00892",
        "0X.paaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\x9d\x9d\x9d\x9d\x9d\x9d\x9d\x9d\x9d\x9d\x9d\u009daaaaaaaaap\u00892",
        "-88003133131882818281888.0e-52180000000000000000000000000888.0e-52",
        "000000000000000000000000000000000000000000000313100000000000000000000000d",
        "E000000000000000000000000000000000000313100000000000000000000000d",
        "-38888888818661881861666E-3183888888881666666",
        "0XEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEDp0",
        "0B.1",
        "+3188.0e-------------------------------------------------+3188.0e-+",
        "0X.EEkkkkkkkkkEEEEEEEEEEEE18EEEE88008EEEE8EEEEEEEEEEEEEEEEE4E8EEEEEEEEEEEEEEEEEEEEEEEE18EEEE880088@",
        "+3000000000000000000000061600188.0e-+---",
        "0xddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddV1@d",
        "0xdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
        "0xdddddddddddddbdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
        "0X.8888888AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA7AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA0118AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\x7f\u00ffAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\xad-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------",
        "0B.1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"
    };

    for (s64 i = 0; i < sizeof(tmp) / sizeof(tmp[0]); ++i) {
        u8 _u8; u16 _u16; u32 _u32; u64 _u64;
        s8 _s8; s16 _s16; s32 _s32; s64 _s64 = 0;
        float _float; double _double = 0;
        Array_t<u8> s {(u8*)tmp[i], (s64)strlen(tmp[i])};
        u16 _floatc = jup_stox(s, &_float);
        u16 _doublec = jup_stox(s, &_double);
        u16 _floatc2 = jup_stox(s, &_float, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
        u16 _doublec2 = jup_stox(s, &_double, jup_sto::ALLOW_INFINITY | jup_sto::ALLOW_NAN);
        u16 _u8c = jup_stox(s, &_u8);
        u16 _u16c = jup_stox(s, &_u16);
        u16 _u32c = jup_stox(s, &_u32);
        u16 _u64c = jup_stox(s, &_u64);
        u16 _s8c = jup_stox(s, &_s8);
        u16 _s16c = jup_stox(s, &_s16);
        u16 _s32c = jup_stox(s, &_s32);
        u16 _s64c = jup_stox(s, &_s64);
        printf("%hu %hu %hu %hu %hu %hu %hu %hu %hu %hu %hu %hu %llu %.16e\n",
        _u8c, _u16c, _u32c, _u64c, _s8c , _s16c, _s32c, _s64c, _floatc, _doublec, _floatc2, _doublec2, _s64, _double);
    }
    exit(0);
        
        
    // The following were found by the afl fuzzer,
    check_error<u8>("8e8888");
    check_error<u8>("273188.0e-272222");
    
    puts("Starting random testing (this will never stop)...");
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
            printf("  off-by-one counts\n");
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

// This file is included by rounded.c
// FLOAT_TYPE, FLOAT_NAME, SQRT, FMA, FMAX, FMIN, EACH_ROUNDING_MODE must be defined

#ifndef CONCAT
#define CONCAT(a, b) DO_CONCAT(a, b)
#define DO_CONCAT(a, b) a ## b
#endif
#ifndef CONCAT3
#define CONCAT3(a, b, c) DO_CONCAT3(a, b, c)
#define DO_CONCAT3(a, b, c) a ## b ## c
#endif
#ifndef CONCAT4
#define CONCAT4(a, b, c, d) DO_CONCAT4(a, b, c, d)
#define DO_CONCAT4(a, b, c, d) a ## b ## c ## d
#endif

#define ADD_TYPE_SUFFIX(Name) CONCAT3(Name,_,FLOAT_NAME)

static inline FLOAT_TYPE ADD_TYPE_SUFFIX(rounded_add_impl)(native_rounding_mode mode, FLOAT_TYPE a, FLOAT_TYPE b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    FLOAT_TYPE c = a + b;
    restore_fp_reg(oldreg);
    return c;
}
EACH_ROUNDING_MODE(ADD_TYPE_SUFFIX(rounded_hw_add), ADD_TYPE_SUFFIX(rounded_add_impl), FLOAT_TYPE, (FLOAT_TYPE a, FLOAT_TYPE b), a, b)

static inline FLOAT_TYPE ADD_TYPE_SUFFIX(rounded_sub_impl)(native_rounding_mode mode, FLOAT_TYPE a, FLOAT_TYPE b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    FLOAT_TYPE c = a - b;
    restore_fp_reg(oldreg);
    return c;
}
EACH_ROUNDING_MODE(ADD_TYPE_SUFFIX(rounded_hw_sub), ADD_TYPE_SUFFIX(rounded_sub_impl), FLOAT_TYPE, (FLOAT_TYPE a, FLOAT_TYPE b), a, b)

static inline FLOAT_TYPE ADD_TYPE_SUFFIX(rounded_mul_impl)(native_rounding_mode mode, FLOAT_TYPE a, FLOAT_TYPE b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    FLOAT_TYPE c = a * b;
    restore_fp_reg(oldreg);
    return c;
}
EACH_ROUNDING_MODE(ADD_TYPE_SUFFIX(rounded_hw_mul), ADD_TYPE_SUFFIX(rounded_mul_impl), FLOAT_TYPE, (FLOAT_TYPE a, FLOAT_TYPE b), a, b)

static inline FLOAT_TYPE ADD_TYPE_SUFFIX(rounded_div_impl)(native_rounding_mode mode, FLOAT_TYPE a, FLOAT_TYPE b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    FLOAT_TYPE c = a / b;
    restore_fp_reg(oldreg);
    return c;
}
EACH_ROUNDING_MODE(ADD_TYPE_SUFFIX(rounded_hw_div), ADD_TYPE_SUFFIX(rounded_div_impl), FLOAT_TYPE, (FLOAT_TYPE a, FLOAT_TYPE b), a, b)

static inline FLOAT_TYPE ADD_TYPE_SUFFIX(rounded_sqrt_impl)(native_rounding_mode mode, FLOAT_TYPE a)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    FLOAT_TYPE c = SQRT(a);
    restore_fp_reg(oldreg);
    return c;
}
EACH_ROUNDING_MODE(ADD_TYPE_SUFFIX(rounded_hw_sqrt), ADD_TYPE_SUFFIX(rounded_sqrt_impl), FLOAT_TYPE, (FLOAT_TYPE a), a)

static inline FLOAT_TYPE ADD_TYPE_SUFFIX(rounded_fma_impl)(native_rounding_mode mode, FLOAT_TYPE a, FLOAT_TYPE b, FLOAT_TYPE c)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    FLOAT_TYPE d = FMA(a, b, c); // a * b + c
    restore_fp_reg(oldreg);
    return d;
}
EACH_ROUNDING_MODE(ADD_TYPE_SUFFIX(rounded_hw_fma), ADD_TYPE_SUFFIX(rounded_fma_impl), FLOAT_TYPE, (FLOAT_TYPE a, FLOAT_TYPE b, FLOAT_TYPE c), a, b, c)

static inline FLOAT_TYPE ADD_TYPE_SUFFIX(rounded_int64_to_impl)(native_rounding_mode mode, int64_t x)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    FLOAT_TYPE result = (FLOAT_TYPE)x;
    restore_fp_reg(oldreg);
    return result;
}
EACH_ROUNDING_MODE(ADD_TYPE_SUFFIX(rounded_hw_int64_to), ADD_TYPE_SUFFIX(rounded_int64_to_impl), FLOAT_TYPE, (int64_t x), x)

static inline FLOAT_TYPE ADD_TYPE_SUFFIX(rounded_word64_to_impl)(native_rounding_mode mode, uint64_t x)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    FLOAT_TYPE result = (FLOAT_TYPE)x;
    restore_fp_reg(oldreg);
    return result;
}
EACH_ROUNDING_MODE(ADD_TYPE_SUFFIX(rounded_hw_word64_to), ADD_TYPE_SUFFIX(rounded_word64_to_impl), FLOAT_TYPE, (uint64_t x), x)

//
// Interval arithmetic
//

extern FLOAT_TYPE CONCAT(ADD_TYPE_SUFFIX(rounded_hw_interval_mul),_up)(FLOAT_TYPE lo1, FLOAT_TYPE hi1, FLOAT_TYPE lo2, FLOAT_TYPE hi2)
{
    // TODO: zero and infinity
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    FLOAT_TYPE hi = FMAX(FMAX(lo1 * lo2, lo1 * hi2), FMAX(hi1 * lo2, hi1 * hi2));
    restore_fp_reg(oldreg);
    return hi;
}

extern FLOAT_TYPE CONCAT(ADD_TYPE_SUFFIX(rounded_hw_interval_mul),_down)(FLOAT_TYPE lo1, FLOAT_TYPE hi1, FLOAT_TYPE lo2, FLOAT_TYPE hi2)
{
    // TODO: zero and infinity
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    FLOAT_TYPE lo = FMIN(FMIN(lo1 * lo2, lo1 * hi2), FMIN(hi1 * lo2, hi1 * hi2));
    restore_fp_reg(oldreg);
    return lo;
}

extern FLOAT_TYPE CONCAT(ADD_TYPE_SUFFIX(rounded_hw_interval_div),_up)(FLOAT_TYPE lo1, FLOAT_TYPE hi1, FLOAT_TYPE lo2, FLOAT_TYPE hi2)
{
    // TODO: zero and infinity
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    FLOAT_TYPE hi = FMAX(FMAX(lo1 / lo2, lo1 / hi2), FMAX(hi1 / lo2, hi1 / hi2));
    restore_fp_reg(oldreg);
    return hi;
}

extern FLOAT_TYPE CONCAT(ADD_TYPE_SUFFIX(rounded_hw_interval_div),_down)(FLOAT_TYPE lo1, FLOAT_TYPE hi1, FLOAT_TYPE lo2, FLOAT_TYPE hi2)
{
    // TODO: zero and infinity
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    FLOAT_TYPE lo = FMIN(FMIN(lo1 / lo2, lo1 / hi2), FMIN(hi1 / lo2, hi1 / hi2));
    restore_fp_reg(oldreg);
    return lo;
}

//
// Summation
//

static inline FLOAT_TYPE ADD_TYPE_SUFFIX(rounded_sum_impl)(native_rounding_mode mode, HsInt offset, HsInt length, const FLOAT_TYPE *a)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    FLOAT_TYPE s = 0;
    for (HsInt i = 0; i < length; ++i) {
        s += a[offset + i];
    }
    restore_fp_reg(oldreg);
    return s;
}
EACH_ROUNDING_MODE(ADD_TYPE_SUFFIX(rounded_hw_sum), ADD_TYPE_SUFFIX(rounded_sum_impl), FLOAT_TYPE, (HsInt offset, HsInt length, const FLOAT_TYPE *a), offset, length, a)

#undef ADD_TYPE_SUFFIX

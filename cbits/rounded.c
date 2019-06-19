#include <math.h>
#include "HsFFI.h"

#pragma STDC FENV_ACCESS ON

#if defined(__GNUC__)
#define ALWAYS_INLINE __attribute__((always_inline))
#else
#define ALWAYS_INLINE
#endif

#if defined(USE_SSE2)

#include <x86intrin.h>

typedef unsigned int fp_reg;
typedef unsigned int native_rounding_mode;
static const native_rounding_mode ROUND_TONEAREST  = 0;
static const native_rounding_mode ROUND_DOWNWARD   = 1;
static const native_rounding_mode ROUND_UPWARD     = 2;
static const native_rounding_mode ROUND_TOWARDZERO = 3;

static inline ALWAYS_INLINE
native_rounding_mode hs_rounding_mode_to_native(HsInt mode)
{
    /*
     * The order of RoundingMode in Numeric.Rounded.Hardware.Rounding is
     * chosen so that the conversion here becomes trivial.
     */
    return (native_rounding_mode)mode;
}

static inline ALWAYS_INLINE
fp_reg get_fp_reg(void)
{
    return _mm_getcsr();
}
static inline ALWAYS_INLINE
void set_rounding(fp_reg reg, native_rounding_mode mode)
{
    _mm_setcsr((reg & ~(3u << 13)) | (mode << 13));
}
static inline ALWAYS_INLINE
void restore_fp_reg(fp_reg reg)
{
    _mm_setcsr(reg);
}

#elif defined(USE_C99)

#include <fenv.h>

typedef int fp_reg;
typedef int native_rounding_mode;
static const native_rounding_mode ROUND_TONEAREST  = FE_TONEAREST;
static const native_rounding_mode ROUND_DOWNWARD   = FE_DOWNWARD;
static const native_rounding_mode ROUND_UPWARD     = FE_UPWARD;
static const native_rounding_mode ROUND_TOWARDZERO = FE_TOWARDZERO;

static inline ALWAYS_INLINE
native_rounding_mode hs_rounding_mode_to_native(HsInt mode)
{
    switch (mode) {
    case /* TowardNearest */ 0: return FE_TONEAREST;
    case /* TowardNegInf  */ 1: return FE_DOWNWARD;
    case /* TowardInf     */ 2: return FE_UPWARD;
    case /* TowardZero    */ 3: return FE_TOWARDZERO;
    default: return FE_TONEAREST;
    }
}

static inline ALWAYS_INLINE
fp_reg get_fp_reg(void)
{
    return fegetround();
}
static inline ALWAYS_INLINE
void set_rounding(fp_reg reg, native_rounding_mode mode)
{
    fesetround(mode);
}
static inline ALWAYS_INLINE
void restore_fp_reg(fp_reg oldmode)
{
    fesetround(oldmode);
}

#else
#error Please define USE_C99 or USE_SSE2
#endif

static inline double rounded_add_impl(native_rounding_mode mode, double a, double b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    double c = a + b;
    restore_fp_reg(oldreg);
    return c;
}

extern double rounded_hw_add(HsInt mode, double a, double b)
{ return rounded_add_impl(hs_rounding_mode_to_native(mode), a, b); }
extern double rounded_hw_add_up(double a, double b)
{ return rounded_add_impl(ROUND_UPWARD, a, b); }
extern double rounded_hw_add_down(double a, double b)
{ return rounded_add_impl(ROUND_DOWNWARD, a, b); }
extern double rounded_hw_add_zero(double a, double b)
{ return rounded_add_impl(ROUND_TOWARDZERO, a, b); }

static inline double rounded_sub_impl(native_rounding_mode mode, double a, double b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    double c = a - b;
    restore_fp_reg(oldreg);
    return c;
}

extern double rounded_hw_sub(HsInt mode, double a, double b)
{ return rounded_sub_impl(hs_rounding_mode_to_native(mode), a, b); }
extern double rounded_hw_sub_up(double a, double b)
{ return rounded_sub_impl(ROUND_UPWARD, a, b); }
extern double rounded_hw_sub_down(double a, double b)
{ return rounded_sub_impl(ROUND_DOWNWARD, a, b); }
extern double rounded_hw_sub_zero(double a, double b)
{ return rounded_sub_impl(ROUND_TOWARDZERO, a, b); }

static inline double rounded_mul_impl(native_rounding_mode mode, double a, double b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    double c = a * b;
    restore_fp_reg(oldreg);
    return c;
}

extern double rounded_hw_mul(HsInt mode, double a, double b)
{ return rounded_mul_impl(hs_rounding_mode_to_native(mode), a, b); }
extern double rounded_hw_mul_up(double a, double b)
{ return rounded_mul_impl(ROUND_UPWARD, a, b); }
extern double rounded_hw_mul_down(double a, double b)
{ return rounded_mul_impl(ROUND_DOWNWARD, a, b); }
extern double rounded_hw_mul_zero(double a, double b)
{ return rounded_mul_impl(ROUND_TOWARDZERO, a, b); }

static inline double rounded_div_impl(native_rounding_mode mode, double a, double b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    double c = a / b;
    restore_fp_reg(oldreg);
    return c;
}

extern double rounded_hw_div(HsInt mode, double a, double b)
{ return rounded_div_impl(hs_rounding_mode_to_native(mode), a, b); }
extern double rounded_hw_div_up(double a, double b)
{ return rounded_div_impl(ROUND_UPWARD, a, b); }
extern double rounded_hw_div_down(double a, double b)
{ return rounded_div_impl(ROUND_DOWNWARD, a, b); }
extern double rounded_hw_div_zero(double a, double b)
{ return rounded_div_impl(ROUND_TOWARDZERO, a, b); }

static inline double rounded_sqrt_impl(native_rounding_mode mode, double a)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    double c = sqrt(a);
    restore_fp_reg(oldreg);
    return c;
}

extern double rounded_hw_sqrt(HsInt mode, double a)
{ return rounded_sqrt_impl(hs_rounding_mode_to_native(mode), a); }
extern double rounded_hw_sqrt_up(double a)
{ return rounded_sqrt_impl(ROUND_UPWARD, a); }
extern double rounded_hw_sqrt_down(double a)
{ return rounded_sqrt_impl(ROUND_DOWNWARD, a); }
extern double rounded_hw_sqrt_zero(double a)
{ return rounded_sqrt_impl(ROUND_TOWARDZERO, a); }

// TODO: FMA, remainder, int64 -> double, double -> int

extern double rounded_hw_interval_mul_up(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    double hi = fmax(fmax(lo1 * lo2, lo1 * hi2), fmax(hi1 * lo2, hi1 * hi2));
    restore_fp_reg(oldreg);
    return hi;
}

extern double rounded_hw_interval_mul_down(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    double lo = fmin(fmin(lo1 * lo2, lo1 * hi2), fmin(hi1 * lo2, hi1 * hi2));
    restore_fp_reg(oldreg);
    return lo;
}

extern double rounded_hw_interval_div_up(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_UPWARD);
    double hi = fmax(fmax(lo1 / lo2, lo1 / hi2), fmax(hi1 / lo2, hi1 / hi2));
    restore_fp_reg(oldreg);
    return hi;
}

extern double rounded_hw_interval_div_down(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, ROUND_DOWNWARD);
    double lo = fmin(fmin(lo1 / lo2, lo1 / hi2), fmin(hi1 / lo2, hi1 / hi2));
    restore_fp_reg(oldreg);
    return lo;
}

static inline double rounded_sum_impl(native_rounding_mode mode, HsInt offset, HsInt length, const double *a)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
    double s = 0;
    for(HsInt i = 0; i < length; ++i) {
        s += a[offset + i];
    }
    restore_fp_reg(oldreg);
    return s;
}

extern double rounded_hw_sum(HsInt mode, HsInt offset, HsInt length, const double *a)
{ return rounded_sum_impl(hs_rounding_mode_to_native(mode), offset, length, a); }
extern double rounded_hw_sum_up(HsInt offset, HsInt length, const double *a)
{ return rounded_sum_impl(ROUND_UPWARD, offset, length, a); }
extern double rounded_hw_sum_down(HsInt offset, HsInt length, const double *a)
{ return rounded_sum_impl(ROUND_DOWNWARD, offset, length, a); }
extern double rounded_hw_sum_zero(HsInt offset, HsInt length, const double *a)
{ return rounded_sum_impl(ROUND_TOWARDZERO, offset, length, a); }

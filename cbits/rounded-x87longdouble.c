#include <math.h>
#include <fenv.h>
#include <assert.h>
#include <stdint.h>
#include "HsFFI.h"

#pragma STDC FENV_ACCESS ON

static_assert(sizeof(long double) >= 10, "long double must be 80 bits or greater");

#if defined(__GNUC__)
#define ALWAYS_INLINE __attribute__((always_inline))
#else
#define ALWAYS_INLINE
#endif

#if defined(__GNUC__)
#define UNREACHABLE() __builtin_unreachable()
#else
#define UNREACHABLE() do {} while (0)
#endif

#if defined(USE_C99)

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
    default: UNREACHABLE(); return FE_TONEAREST;
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

#if !defined(__GNUC__)
#error "Unsupported compiler"
#endif

typedef uint16_t fp_reg;
typedef uint16_t native_rounding_mode;
static const native_rounding_mode ROUND_TONEAREST  = 0;
static const native_rounding_mode ROUND_DOWNWARD   = 1;
static const native_rounding_mode ROUND_UPWARD     = 2;
static const native_rounding_mode ROUND_TOWARDZERO = 3;

static inline ALWAYS_INLINE
native_rounding_mode hs_rounding_mode_to_native(HsInt mode)
{
    /*
     * The order of RoundingMode in Numeric.Rounded.Hardware.Internal.Rounding is
     * chosen so that the conversion here becomes trivial.
     */
    return (native_rounding_mode)mode;
}

static inline ALWAYS_INLINE
fp_reg get_fp_reg(void)
{
    uint16_t cword;
    asm("fstcw %0" : "=m"(cword));
    return cword;
}
static inline ALWAYS_INLINE
void set_rounding(fp_reg oldcword, native_rounding_mode mode)
{
    uint16_t newcword = (oldcword & ~(3u << 10) & ~(3u << 8)) | (mode << 10) | (3u << 8); // precision: double extended
    asm("fldcw %0" : : "m"(newcword));
}
static inline ALWAYS_INLINE
void restore_fp_reg(fp_reg cword)
{
    asm("fldcw %0" : : "m"(cword));
}

#endif

extern void rounded_hw_add_longdouble(HsInt mode, long double *result, const long double* a, const long double *b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, hs_rounding_mode_to_native(mode));
    *(volatile long double *)result = *a + *b;
    restore_fp_reg(oldreg);
}

extern void rounded_hw_sub_longdouble(HsInt mode, long double *result, const long double* a, const long double *b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, hs_rounding_mode_to_native(mode));
    *(volatile long double *)result = *a - *b;
    restore_fp_reg(oldreg);
}

extern void rounded_hw_mul_longdouble(HsInt mode, long double *result, const long double* a, const long double *b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, hs_rounding_mode_to_native(mode));
    *(volatile long double *)result = *a * *b;
    restore_fp_reg(oldreg);
}

extern void rounded_hw_div_longdouble(HsInt mode, long double *result, const long double* a, const long double *b)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, hs_rounding_mode_to_native(mode));
    *(volatile long double *)result = *a / *b;
    restore_fp_reg(oldreg);
}

extern void rounded_hw_sqrt_longdouble(HsInt mode, long double *result, const long double* a)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, hs_rounding_mode_to_native(mode));
    *(volatile long double *)result = sqrtl(*a);
    restore_fp_reg(oldreg);
}

extern void rounded_hw_fma_longdouble(HsInt mode, long double *result, const long double* a, const long double *b, const long double *c)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, hs_rounding_mode_to_native(mode));
    *(volatile long double *)result = fmal(*a, *b, *c);
    restore_fp_reg(oldreg);
}

extern const char *rounded_hw_backend_name_longdouble(void) {
#if defined(USE_C99)
    return "C99";
#else
    return "inline assembly";
#endif
}

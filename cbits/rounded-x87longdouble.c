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

static inline ALWAYS_INLINE
int hs_rounding_mode_to_c99(HsInt mode)
{
    switch (mode) {
    case /* TowardNearest */ 0: return FE_TONEAREST;
    case /* TowardNegInf  */ 1: return FE_DOWNWARD;
    case /* TowardInf     */ 2: return FE_UPWARD;
    case /* TowardZero    */ 3: return FE_TOWARDZERO;
    default: return FE_TONEAREST;
    }
}

/*
TODO: Add an option to use inline assembly
extern void rounded_hw_add_longdouble_2(HsInt mode, long double *result, const long double* a, const long double *b)
{
    uint16_t oldcword;
    asm("fstcw %0" : "=m"(oldcword));
    uint16_t newcword = (oldcword & ~(3u << 10)) | ((uint16_t)mode << 10);
    asm("fldcw %0" : : "m"(newcword));
    *result = *a + *b;
    asm("fldcw %0" : : "m"(oldcword));
}
*/

extern void rounded_hw_add_longdouble(HsInt mode, long double *result, const long double* a, const long double *b)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *result = *a + *b;
    fesetround(oldmode);
}

extern void rounded_hw_sub_longdouble(HsInt mode, long double *result, const long double* a, const long double *b)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *result = *a - *b;
    fesetround(oldmode);
}

extern void rounded_hw_mul_longdouble(HsInt mode, long double *result, const long double* a, const long double *b)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *result = *a * *b;
    fesetround(oldmode);
}

extern void rounded_hw_div_longdouble(HsInt mode, long double *result, const long double* a, const long double *b)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *result = *a / *b;
    fesetround(oldmode);
}

extern void rounded_hw_sqrt_longdouble(HsInt mode, long double *result, const long double* a)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *result = sqrtl(*a);
    fesetround(oldmode);
}

extern void rounded_hw_fma_longdouble(HsInt mode, long double *result, const long double* a, const long double *b, const long double *c)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *result = fmal(*a, *b, *c);
    fesetround(oldmode);
}

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <math.h> // sqrtf128, fmaf128
#include <stdint.h>
#include <fenv.h>
#include <HsFFI.h>

#pragma STDC FENV_ACCESS ON

#if defined(__GNUC__)
#define ALWAYS_INLINE __attribute__((always_inline))
#else
#define ALWAYS_INLINE
#endif

#if !defined(UNREACHABLE)
#if defined(__GNUC__)
#define UNREACHABLE() __builtin_unreachable()
#else
#define UNREACHABLE() do {} while (0)
#endif
#endif

static inline ALWAYS_INLINE
int hs_rounding_mode_to_c99(HsInt mode)
{
    switch (mode) {
    case /* TowardNearest */ 0: return FE_TONEAREST;
    case /* TowardNegInf  */ 1: return FE_DOWNWARD;
    case /* TowardInf     */ 2: return FE_UPWARD;
    case /* TowardZero    */ 3: return FE_TOWARDZERO;
    default: UNREACHABLE(); return FE_TONEAREST;
    }
}

extern void rounded_hw_add_float128(HsInt mode, _Float128 *result, const _Float128 *a, const _Float128 *b)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *(volatile _Float128 *)result = *a + *b;
    fesetround(oldmode);
}

extern void rounded_hw_sub_float128(HsInt mode, _Float128 *result, const _Float128 *a, const _Float128 *b)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *(volatile _Float128 *)result = *a - *b;
    fesetround(oldmode);
}

extern void rounded_hw_mul_float128(HsInt mode, _Float128 *result, const _Float128 *a, const _Float128 *b)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *(volatile _Float128 *)result = *a * *b;
    fesetround(oldmode);
}

extern void rounded_hw_div_float128(HsInt mode, _Float128 *result, const _Float128 *a, const _Float128 *b)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *(volatile _Float128 *)result = *a / *b;
    fesetround(oldmode);
}

extern void rounded_hw_sqrt_float128(HsInt mode, _Float128 *result, const _Float128 *a)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *(volatile _Float128 *)result = sqrtf128(*a);
    fesetround(oldmode);
}

extern void rounded_hw_fma_float128(HsInt mode, _Float128 *result, const _Float128 *a, const _Float128 *b, const _Float128 *c)
{
    int oldmode = fegetround();
    fesetround(hs_rounding_mode_to_c99(mode));
    *(volatile _Float128 *)result = fmaf128(*a, *b, *c);
    fesetround(oldmode);
}

extern const char *rounded_hw_backend_name_float128(void)
{
    return "C99";
}

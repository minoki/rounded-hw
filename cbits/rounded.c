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

#define UNPAREN(...) __VA_ARGS__

#ifndef CONCAT
#define CONCAT(a, b) DO_CONCAT(a, b)
#define DO_CONCAT(a, b) a ## b
#endif

#define EACH_ROUNDING_MODE(Name, Impl, RetTy, Params, ...)          \
    extern RetTy Name(HsInt mode, UNPAREN Params)                   \
    { return Impl(hs_rounding_mode_to_native(mode), __VA_ARGS__); } \
    extern RetTy CONCAT(Name,_up) Params                            \
    { return Impl(ROUND_UPWARD, __VA_ARGS__); }                     \
    extern RetTy CONCAT(Name,_down) Params                          \
    { return Impl(ROUND_DOWNWARD, __VA_ARGS__); }                   \
    extern RetTy CONCAT(Name,_zero) Params                          \
    { return Impl(ROUND_TOWARDZERO, __VA_ARGS__); }                 \
    /**/

#define FLOAT_TYPE float
#define FLOAT_NAME float
#define SQRT sqrtf
#define FMA fmaf
#define FMAX fmaxf
#define FMIN fminf
#include "rounded-common.inl"
#undef FLOAT_TYPE
#undef FLOAT_NAME
#undef SQRT
#undef FMA
#undef FMAX
#undef FMIN

#define FLOAT_TYPE double
#define FLOAT_NAME double
#define SQRT sqrt
#define FMA fma
#define FMAX fmax
#define FMIN fmin
#include "rounded-common.inl"
#undef FLOAT_TYPE
#undef FLOAT_NAME
#undef SQRT
#undef FMA
#undef FMAX
#undef FMIN

static inline float rounded_fma_if_fast_impl_float(native_rounding_mode mode, float a, float b, float c)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
#ifdef FP_FAST_FMAF
    float d = fmaf(a, b, c);
#else
    float d = a * b + c;
#endif
    restore_fp_reg(oldreg);
    return d;
}
EACH_ROUNDING_MODE(rounded_hw_fma_if_fast_float, rounded_fma_if_fast_impl_float, float, (float a, float b, float c), a, b, c)

static inline double rounded_fma_if_fast_impl_double(native_rounding_mode mode, double a, double b, double c)
{
    fp_reg oldreg = get_fp_reg();
    set_rounding(oldreg, mode);
#ifdef FP_FAST_FMA
    double d = fma(a, b, c);
#else
    double d = a * b + c;
#endif
    restore_fp_reg(oldreg);
    return d;
}
EACH_ROUNDING_MODE(rounded_hw_fma_if_fast_double, rounded_fma_if_fast_impl_double, double, (double a, double b, double c), a, b, c)

// TODO: remainder, double -> int

extern const char *rounded_hw_backend_name(void) {
#if defined(USE_SSE2)
    return "SSE2";
#elif defined(USE_C99)
    return "C99";
#else
#error Please define USE_C99 or USE_SSE2
#endif
}

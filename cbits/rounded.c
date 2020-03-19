#include <stdlib.h>
#include <math.h>
#include "HsFFI.h"

#pragma STDC FENV_ACCESS ON

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

/* By default, we use SSE2 if available. Define USE_C99 to override.  */

#if !defined(USE_C99) && !defined(USE_SSE2) && !defined(USE_AVX512)
// Detect what processor feature is available and make a decision.

#if defined(__AVX512F__)
// If AVX512 is available, use it.
#define USE_AVX512
#elif defined(__SSE2__)
// If SSE2 is available, use it.
#define USE_SSE2
#elif defined(__aarch64__)
// If we are on AArch64, use the control register.
#define USE_AARCH64_FPCR
#else
// Otherwise, use C99's fesetround.
#define USE_C99
#endif

#elif defined(USE_C99) && defined(USE_SSE2)
#error "Invalid configuration detected: USE_C99 and USE_SSE2 are mutually exclusive"
#elif defined(USE_C99) && defined(USE_AVX512)
#error "Invalid configuration detected: USE_C99 and USE_AVX512 are mutually exclusive"
#elif defined(USE_SSE2) && defined(USE_AVX512)
#error "Invalid configuration detected: USE_SSE2 and USE_AVX512 are mutually exclusive"
#endif

#if defined(USE_AVX512)

#include <x86intrin.h>

typedef enum {
  /* The order is same as RoundingMode in Numeric.Rounded.Hardware.Internal.Rounding */
  ROUND_TONEAREST = 0,
  ROUND_DOWNWARD,
  ROUND_UPWARD,
  ROUND_TOWARDZERO
} native_rounding_mode;

static inline ALWAYS_INLINE
native_rounding_mode hs_rounding_mode_to_native(HsInt mode)
{ return (native_rounding_mode)mode; }

static const char backend_name[] = "AVX512";

#elif defined(USE_SSE2)

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
     * The order of RoundingMode in Numeric.Rounded.Hardware.Internal.Rounding is
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

static const char backend_name[] = "SSE2";

#elif defined(USE_AARCH64_FPCR)

typedef unsigned int fp_reg;
typedef unsigned int native_rounding_mode;
static const native_rounding_mode ROUND_TONEAREST  = 0 << 22;
static const native_rounding_mode ROUND_DOWNWARD   = 2 << 22;
static const native_rounding_mode ROUND_UPWARD     = 1 << 22;
static const native_rounding_mode ROUND_TOWARDZERO = 3 << 22;

static inline ALWAYS_INLINE
native_rounding_mode hs_rounding_mode_to_native(HsInt mode)
{
    switch (mode) {
    case /* ToNearest    */ 0: return ROUND_TONEAREST;
    case /* TowardNegInf */ 1: return ROUND_DOWNWARD;
    case /* TowardInf    */ 2: return ROUND_UPWARD;
    case /* TowardZero   */ 3: return ROUND_TOWARDZERO;
    default: UNREACHABLE(); return ROUND_TONEAREST;
    }
}

static inline ALWAYS_INLINE
fp_reg get_fp_reg(void)
{
    return __builtin_aarch64_get_fpcr();
}
static inline ALWAYS_INLINE
void set_rounding(fp_reg reg, native_rounding_mode mode)
{
    __builtin_aarch64_set_fpcr((reg & ~(3u << 22)) | mode);
}
static inline ALWAYS_INLINE
void restore_fp_reg(fp_reg reg)
{
    __builtin_aarch64_set_fpcr(reg);
}

static const char backend_name[] = "AArch64 FPCR";

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
    case /* ToNearest    */ 0: return FE_TONEAREST;
    case /* TowardNegInf */ 1: return FE_DOWNWARD;
    case /* TowardInf    */ 2: return FE_UPWARD;
    case /* TowardZero   */ 3: return FE_TOWARDZERO;
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

static const char backend_name[] = "C99";

#else
#error Please define USE_C99 or USE_SSE2 or USE_AVX512
#endif

#if defined(USE_AVX512)
#include "rounded-avx512.inl"
#else
#include "rounded-common.inl"
#endif

extern const char *rounded_hw_backend_name(void) {
    return backend_name;
}

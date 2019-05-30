#include <math.h>
#include <x86intrin.h>

#pragma STDC FENV_ACCESS ON

// static const unsigned int RC_TONEAREST  = 0;
static const unsigned int RC_DOWNWARD   = 1;
static const unsigned int RC_UPWARD     = 2;
// static const unsigned int RC_TOWARDZERO = 3;

static inline double rounded_add(unsigned int mode, double a, double b)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = a + b;
    _mm_setcsr(oldmode);
    return c;
}

extern double hs_rounded_sse2_add_up(double a, double b)
{
    return rounded_add(RC_UPWARD, a, b);
}

extern double hs_rounded_sse2_add_down(double a, double b)
{
    return rounded_add(RC_DOWNWARD, a, b);
}

static inline double rounded_sub(unsigned int mode, double a, double b)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = a - b;
    _mm_setcsr(oldmode);
    return c;
}

extern double hs_rounded_sse2_sub_up(double a, double b)
{
    return rounded_sub(RC_UPWARD, a, b);
}

extern double hs_rounded_sse2_sub_down(double a, double b)
{
    return rounded_sub(RC_DOWNWARD, a, b);
}

static inline double rounded_mul(unsigned int mode, double a, double b)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = a * b;
    _mm_setcsr(oldmode);
    return c;
}

extern double hs_rounded_sse2_mul_up(double a, double b)
{
    return rounded_mul(RC_UPWARD, a, b);
}

extern double hs_rounded_sse2_mul_down(double a, double b)
{
    return rounded_mul(RC_DOWNWARD, a, b);
}

static inline double rounded_div(unsigned int mode, double a, double b)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = a / b;
    _mm_setcsr(oldmode);
    return c;
}

extern double hs_rounded_sse2_div_up(double a, double b)
{
    return rounded_div(RC_UPWARD, a, b);
}

extern double hs_rounded_sse2_div_down(double a, double b)
{
    return rounded_div(RC_DOWNWARD, a, b);
}

static inline double rounded_sqrt(unsigned int mode, double a)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = sqrt(a);
    _mm_setcsr(oldmode);
    return c;
}

extern double hs_rounded_sse2_sqrt_up(double a)
{
    return rounded_sqrt(RC_UPWARD, a);
}

extern double hs_rounded_sse2_sqrt_down(double a)
{
    return rounded_sqrt(RC_DOWNWARD, a);
}

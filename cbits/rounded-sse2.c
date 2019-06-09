#include <math.h>
#include <x86intrin.h>

#pragma STDC FENV_ACCESS ON

// static const unsigned int RC_TONEAREST  = 0;
static const unsigned int RC_DOWNWARD   = 1;
static const unsigned int RC_UPWARD     = 2;
static const unsigned int RC_TOWARDZERO = 3;

static inline double rounded_add(unsigned int mode, double a, double b)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = a + b;
    _mm_setcsr(oldmode);
    return c;
}

extern double rounded_hw_sse2_add_up(double a, double b)
{
    return rounded_add(RC_UPWARD, a, b);
}

extern double rounded_hw_sse2_add_down(double a, double b)
{
    return rounded_add(RC_DOWNWARD, a, b);
}

extern double rounded_hw_sse2_add_zero(double a, double b)
{
    return rounded_add(RC_TOWARDZERO, a, b);
}

static inline double rounded_sub(unsigned int mode, double a, double b)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = a - b;
    _mm_setcsr(oldmode);
    return c;
}

extern double rounded_hw_sse2_sub_up(double a, double b)
{
    return rounded_sub(RC_UPWARD, a, b);
}

extern double rounded_hw_sse2_sub_down(double a, double b)
{
    return rounded_sub(RC_DOWNWARD, a, b);
}

extern double rounded_hw_sse2_sub_zero(double a, double b)
{
    return rounded_sub(RC_TOWARDZERO, a, b);
}

static inline double rounded_mul(unsigned int mode, double a, double b)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = a * b;
    _mm_setcsr(oldmode);
    return c;
}

extern double rounded_hw_sse2_mul_up(double a, double b)
{
    return rounded_mul(RC_UPWARD, a, b);
}

extern double rounded_hw_sse2_mul_down(double a, double b)
{
    return rounded_mul(RC_DOWNWARD, a, b);
}

extern double rounded_hw_sse2_mul_zero(double a, double b)
{
    return rounded_mul(RC_TOWARDZERO, a, b);
}

static inline double rounded_div(unsigned int mode, double a, double b)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = a / b;
    _mm_setcsr(oldmode);
    return c;
}

extern double rounded_hw_sse2_div_up(double a, double b)
{
    return rounded_div(RC_UPWARD, a, b);
}

extern double rounded_hw_sse2_div_down(double a, double b)
{
    return rounded_div(RC_DOWNWARD, a, b);
}

extern double rounded_hw_sse2_div_zero(double a, double b)
{
    return rounded_div(RC_TOWARDZERO, a, b);
}

static inline double rounded_sqrt(unsigned int mode, double a)
{
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (mode << 13));
    double c = sqrt(a);
    _mm_setcsr(oldmode);
    return c;
}

extern double rounded_hw_sse2_sqrt_up(double a)
{
    return rounded_sqrt(RC_UPWARD, a);
}

extern double rounded_hw_sse2_sqrt_down(double a)
{
    return rounded_sqrt(RC_DOWNWARD, a);
}

extern double rounded_hw_sse2_sqrt_zero(double a)
{
    return rounded_sqrt(RC_TOWARDZERO, a);
}

extern double rounded_hw_sse2_interval_mul_up(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (RC_UPWARD << 13));
    double hi = fmax(fmax(lo1 * lo2, lo1 * hi2), fmax(hi1 * lo2, hi1 * hi2));
    _mm_setcsr(oldmode);
    return hi;
}

extern double rounded_hw_sse2_interval_mul_down(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (RC_DOWNWARD << 13));
    double lo = fmin(fmin(lo1 * lo2, lo1 * hi2), fmin(hi1 * lo2, hi1 * hi2));
    _mm_setcsr(oldmode);
    return lo;
}

extern double rounded_hw_sse2_interval_div_up(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (RC_UPWARD << 13));
    double hi = fmax(fmax(lo1 / lo2, lo1 / hi2), fmax(hi1 / lo2, hi1 / hi2));
    _mm_setcsr(oldmode);
    return hi;
}

extern double rounded_hw_sse2_interval_div_down(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    unsigned int oldmode = _mm_getcsr();
    _mm_setcsr((oldmode & ~(3u << 13)) | (RC_DOWNWARD << 13));
    double lo = fmin(fmin(lo1 / lo2, lo1 / hi2), fmin(hi1 / lo2, hi1 / hi2));
    _mm_setcsr(oldmode);
    return lo;
}

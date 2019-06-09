#include <math.h>
#include <fenv.h>

#pragma STDC FENV_ACCESS ON

static inline double rounded_add(int mode, double a, double b)
{
    int oldmode = fegetround();
    fesetround(mode);
    double c = a + b;
    fesetround(oldmode);
    return c;
}

extern double rounded_hw_add_up(double a, double b)
{
    return rounded_add(FE_UPWARD, a, b);
}

extern double rounded_hw_add_down(double a, double b)
{
    return rounded_add(FE_DOWNWARD, a, b);
}

extern double rounded_hw_add_zero(double a, double b)
{
    return rounded_add(FE_TOWARDZERO, a, b);
}

static inline double rounded_sub(int mode, double a, double b)
{
    int oldmode = fegetround();
    fesetround(mode);
    double c = a - b;
    fesetround(oldmode);
    return c;
}

extern double rounded_hw_sub_up(double a, double b)
{
    return rounded_sub(FE_UPWARD, a, b);
}

extern double rounded_hw_sub_down(double a, double b)
{
    return rounded_sub(FE_DOWNWARD, a, b);
}

extern double rounded_hw_sub_zero(double a, double b)
{
    return rounded_sub(FE_TOWARDZERO, a, b);
}

static inline double rounded_mul(int mode, double a, double b)
{
    int oldmode = fegetround();
    fesetround(mode);
    double c = a * b;
    fesetround(oldmode);
    return c;
}

extern double rounded_hw_mul_up(double a, double b)
{
    return rounded_mul(FE_UPWARD, a, b);
}

extern double rounded_hw_mul_down(double a, double b)
{
    return rounded_mul(FE_DOWNWARD, a, b);
}

extern double rounded_hw_mul_zero(double a, double b)
{
    return rounded_mul(FE_TOWARDZERO, a, b);
}

static inline double rounded_div(int mode, double a, double b)
{
    int oldmode = fegetround();
    fesetround(mode);
    double c = a / b;
    fesetround(oldmode);
    return c;
}

extern double rounded_hw_div_up(double a, double b)
{
    return rounded_div(FE_UPWARD, a, b);
}

extern double rounded_hw_div_down(double a, double b)
{
    return rounded_div(FE_DOWNWARD, a, b);
}

extern double rounded_hw_div_zero(double a, double b)
{
    return rounded_div(FE_TOWARDZERO, a, b);
}

static inline double rounded_sqrt(int mode, double a)
{
    int oldmode = fegetround();
    fesetround(mode);
    double c = sqrt(a);
    fesetround(oldmode);
    return c;
}

extern double rounded_hw_sqrt_up(double a)
{
    return rounded_sqrt(FE_UPWARD, a);
}

extern double rounded_hw_sqrt_down(double a)
{
    return rounded_sqrt(FE_DOWNWARD, a);
}

extern double rounded_hw_sqrt_zero(double a)
{
    return rounded_sqrt(FE_TOWARDZERO, a);
}

// TODO: FMA, remainder, int64 -> double, double -> int

extern double rounded_hw_interval_mul_up(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    int oldmode = fegetround();
    fesetround(FE_UPWARD);
    double hi = fmax(fmax(lo1 * lo2, lo1 * hi2), fmax(hi1 * lo2, hi1 * hi2));
    fesetround(oldmode);
    return hi;
}

extern double rounded_hw_interval_mul_down(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    int oldmode = fegetround();
    fesetround(FE_DOWNWARD);
    double lo = fmin(fmin(lo1 * lo2, lo1 * hi2), fmin(hi1 * lo2, hi1 * hi2));
    fesetround(oldmode);
    return lo;
}

extern double rounded_hw_interval_div_up(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    int oldmode = fegetround();
    fesetround(FE_UPWARD);
    double hi = fmax(fmax(lo1 / lo2, lo1 / hi2), fmax(hi1 / lo2, hi1 / hi2));
    fesetround(oldmode);
    return hi;
}

extern double rounded_hw_interval_div_down(double lo1, double hi1, double lo2, double hi2)
{
    // TODO: zero and infinity
    int oldmode = fegetround();
    fesetround(FE_DOWNWARD);
    double lo = fmin(fmin(lo1 / lo2, lo1 / hi2), fmin(hi1 / lo2, hi1 / hi2));
    fesetround(oldmode);
    return lo;
}

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

extern double rounded_hw_c99_add_up(double a, double b)
{
    return rounded_add(FE_UPWARD, a, b);
}

extern double rounded_hw_c99_add_down(double a, double b)
{
    return rounded_add(FE_DOWNWARD, a, b);
}

extern double rounded_hw_c99_add_zero(double a, double b)
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

extern double rounded_hw_c99_sub_up(double a, double b)
{
    return rounded_sub(FE_UPWARD, a, b);
}

extern double rounded_hw_c99_sub_down(double a, double b)
{
    return rounded_sub(FE_DOWNWARD, a, b);
}

extern double rounded_hw_c99_sub_zero(double a, double b)
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

extern double rounded_hw_c99_mul_up(double a, double b)
{
    return rounded_mul(FE_UPWARD, a, b);
}

extern double rounded_hw_c99_mul_down(double a, double b)
{
    return rounded_mul(FE_DOWNWARD, a, b);
}

extern double rounded_hw_c99_mul_zero(double a, double b)
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

extern double rounded_hw_c99_div_up(double a, double b)
{
    return rounded_div(FE_UPWARD, a, b);
}

extern double rounded_hw_c99_div_down(double a, double b)
{
    return rounded_div(FE_DOWNWARD, a, b);
}

extern double rounded_hw_c99_div_zero(double a, double b)
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

extern double rounded_hw_c99_sqrt_up(double a)
{
    return rounded_sqrt(FE_UPWARD, a);
}

extern double rounded_hw_c99_sqrt_down(double a)
{
    return rounded_sqrt(FE_DOWNWARD, a);
}

extern double rounded_hw_c99_sqrt_zero(double a)
{
    return rounded_sqrt(FE_TOWARDZERO, a);
}

// TODO: FMA, remainder, int64 -> double, double -> int

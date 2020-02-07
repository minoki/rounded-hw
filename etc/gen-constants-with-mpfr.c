#include <stdio.h>
#include <gmp.h>
#include <mpfr.h>

int main(void)
{
    int precs[] = {53, 24, 64};
    for (int i = 0; i < sizeof(precs)/sizeof(precs[0]); ++i) {
        int prec = precs[i];

        mpfr_t x, one, half, minus_half;

        mpfr_init2(x, prec);

        mpfr_init2(one, prec);
        mpfr_set_ui(one, 1, MPFR_RNDN);

        mpfr_init2(half, prec);
        mpfr_set_d(half, 0.5, MPFR_RNDN);

        mpfr_init2(minus_half, prec);
        mpfr_set_d(minus_half, -0.5, MPFR_RNDN);

        printf("-- prec = %d bits\n", prec);

        mpfr_const_pi(x, MPFR_RNDD);
        mpfr_printf("  pi_down = Rounded %Ra\n", x);
        mpfr_const_pi(x, MPFR_RNDU);
        mpfr_printf("  pi_up   = Rounded %Ra\n", x);

        puts("  -- 3*pi");
        mpfr_set_prec(x, prec * 2);
        mpfr_const_pi(x, MPFR_RNDD);
        mpfr_mul_ui(x, x, 3, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        mpfr_printf("  three_pi_down = Rounded %Ra\n", x);
        mpfr_set_prec(x, prec * 2);
        mpfr_const_pi(x, MPFR_RNDU);
        mpfr_mul_ui(x, x, 3, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        mpfr_printf("  three_pi_up   = Rounded %Ra\n", x);

        puts("  -- 5*pi");
        mpfr_set_prec(x, prec * 2);
        mpfr_const_pi(x, MPFR_RNDD);
        mpfr_mul_ui(x, x, 5, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        mpfr_printf("  five_pi_down = Rounded %Ra\n", x);
        mpfr_set_prec(x, prec * 2);
        mpfr_const_pi(x, MPFR_RNDU);
        mpfr_mul_ui(x, x, 5, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        mpfr_printf("  five_pi_up   = Rounded %Ra\n", x);

        puts("  -- log(2)");
        mpfr_const_log2(x, MPFR_RNDD);
        mpfr_printf("  log2_down = Rounded %Ra\n", x);
        mpfr_const_log2(x, MPFR_RNDU);
        mpfr_printf("  log2_up   = Rounded %Ra\n", x);

        puts("  -- exp(1)");
        mpfr_exp(x, one, MPFR_RNDD);
        mpfr_printf("  exp1_down = Rounded %Ra\n", x);
        mpfr_exp(x, one, MPFR_RNDU);
        mpfr_printf("  exp1_up   = Rounded %Ra\n", x);

        puts("  -- exp(1/2)");
        mpfr_exp(x, half, MPFR_RNDD);
        mpfr_printf("  exp1_2_down = Rounded %Ra\n", x);
        mpfr_exp(x, half, MPFR_RNDU);
        mpfr_printf("  exp1_2_up   = Rounded %Ra\n", x);

        puts("  -- exp(-1/2)");
        mpfr_exp(x, minus_half, MPFR_RNDD);
        mpfr_printf("  expm1_2_down = Rounded %Ra\n", x);
        mpfr_exp(x, minus_half, MPFR_RNDU);
        mpfr_printf("  expm1_2_up   = Rounded %Ra\n", x);

        puts("  -- sqrt(2)");
        mpfr_sqrt_ui(x, 2, MPFR_RNDD);
        mpfr_printf("  sqrt2_down = Rounded %Ra\n", x);
        mpfr_sqrt_ui(x, 2, MPFR_RNDU);
        mpfr_printf("  sqrt2_up   = Rounded %Ra\n", x);

        puts("  -- sqrt(1/2)");
        mpfr_set_ui(x, 2, MPFR_RNDN);
        mpfr_rec_sqrt(x, x, MPFR_RNDD);
        mpfr_printf("  sqrt1_2_down = Rounded %Ra\n", x);
        mpfr_set_ui(x, 2, MPFR_RNDN);
        mpfr_rec_sqrt(x, x, MPFR_RNDU);
        mpfr_printf("  sqrt1_2_up   = Rounded %Ra\n", x);

        puts("  -- sqrt(2)-1");
        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 2, MPFR_RNDD);
        mpfr_sub_ui(x, x, 1, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        mpfr_printf("  sqrt2m1_down = Rounded %Ra\n", x);
        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 2, MPFR_RNDU);
        mpfr_sub_ui(x, x, 1, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        mpfr_printf("  sqrt2m1_up   = Rounded %Ra\n", x);

        puts("  -- 3 - 2 * sqrt(2)");
        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 8, MPFR_RNDU);
        mpfr_ui_sub(x, 3, x, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        mpfr_printf("  three_minus_2sqrt2_down = Rounded %Ra\n", x);
        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 8, MPFR_RNDD);
        mpfr_ui_sub(x, 3, x, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        mpfr_printf("  three_minus_2sqrt2_up   = Rounded %Ra\n", x);

        puts("  -- 2 - sqrt(2)");
        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 2, MPFR_RNDU);
        mpfr_ui_sub(x, 2, x, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        mpfr_printf("  two_minus_sqrt2_down = Rounded %Ra\n", x);
        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 2, MPFR_RNDD);
        mpfr_ui_sub(x, 2, x, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        mpfr_printf("  two_minus_sqrt2_up   = Rounded %Ra\n", x);

        mpfr_clear(x);
        mpfr_clear(one);
        mpfr_clear(half);
        mpfr_clear(minus_half);
    }
}

#include <stdio.h>
#include <string.h>
#include <gmp.h>
#include <mpfr.h>

static void to_normalized_hex(char *buf, mpfr_t a, int mant)
{
    // assume a is positive
    mpz_t z;
    mpz_init(z);
    mpfr_exp_t e = mpfr_get_z_2exp(z, a);
    size_t size_in_base_2 = mpz_sizeinbase(z, 2);
    // 2^(mant+k-1) <= 1XXXXXX(in hex) < 2^(mant+k), 0 <= k < 4
    // (mant+k-1) is a multiple of 4
    // k === 1-mant mod 4
    int k = (1 + 3 * mant) % 4;
    mpz_mul_2exp(z, z, mant + k - size_in_base_2);
    char buf2[(mant + 2) / 4 + 3];
    mpz_get_str(buf2, 16, z);
    // buf2: z * 2^(mant + k - size_in_base_2)
    // z * 2^(mant + k - size_in_base_2) * 2^(mant-1-k)
    sprintf(buf, "0x%c.%sp%+d", buf2[0] /* 0 */, buf2+1, (int)e + mant - 1);
    mpz_clear(z);
}

static void print_hex(const char *s1, mpfr_t x, const char *s2, int prec)
{
    char buf[1024];
    to_normalized_hex(buf, x, prec);
    // mpfr_printf("%s%Ra%s", s1, x, s2);
    printf("%s%s%s", s1, buf, s2);
}

int main(void)
{
    int precs[] = {53, 24, 64, 113};
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
        print_hex("  pi_down = Rounded ", x, "\n", prec);

        mpfr_const_pi(x, MPFR_RNDU);
        print_hex("  pi_up   = Rounded ", x, "\n", prec);

        puts("  -- 3*pi");
        mpfr_set_prec(x, prec * 2);
        mpfr_const_pi(x, MPFR_RNDD);
        mpfr_mul_ui(x, x, 3, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        print_hex("  three_pi_down = Rounded ", x, "\n", prec);

        mpfr_set_prec(x, prec * 2);
        mpfr_const_pi(x, MPFR_RNDU);
        mpfr_mul_ui(x, x, 3, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        print_hex("  three_pi_up   = Rounded ", x, "\n", prec);

        puts("  -- 5*pi");
        mpfr_set_prec(x, prec * 2);
        mpfr_const_pi(x, MPFR_RNDD);
        mpfr_mul_ui(x, x, 5, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        print_hex("  five_pi_down = Rounded ", x, "\n", prec);

        mpfr_set_prec(x, prec * 2);
        mpfr_const_pi(x, MPFR_RNDU);
        mpfr_mul_ui(x, x, 5, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        print_hex("  five_pi_up   = Rounded ", x, "\n", prec);

        puts("  -- log(2)");
        mpfr_const_log2(x, MPFR_RNDD);
        print_hex("  log2_down = Rounded ", x, "\n", prec);

        mpfr_const_log2(x, MPFR_RNDU);
        print_hex("  log2_up   = Rounded ", x, "\n", prec);

        puts("  -- exp(1)");
        mpfr_exp(x, one, MPFR_RNDD);
        print_hex("  exp1_down = Rounded ", x, "\n", prec);

        mpfr_exp(x, one, MPFR_RNDU);
        print_hex("  exp1_up   = Rounded ", x, "\n", prec);

        puts("  -- exp(1/2)");
        mpfr_exp(x, half, MPFR_RNDD);
        print_hex("  exp1_2_down = Rounded ", x, "\n", prec);

        mpfr_exp(x, half, MPFR_RNDU);
        print_hex("  exp1_2_up   = Rounded ", x, "\n", prec);

        puts("  -- exp(-1/2)");
        mpfr_exp(x, minus_half, MPFR_RNDD);
        print_hex("  expm1_2_down = Rounded ", x, "\n", prec);

        mpfr_exp(x, minus_half, MPFR_RNDU);
        print_hex("  expm1_2_up   = Rounded ", x, "\n", prec);

        puts("  -- sqrt(2)");
        mpfr_sqrt_ui(x, 2, MPFR_RNDD);
        print_hex("  sqrt2_down = Rounded ", x, "\n", prec);

        mpfr_sqrt_ui(x, 2, MPFR_RNDU);
        print_hex("  sqrt2_up   = Rounded ", x, "\n", prec);

        puts("  -- sqrt(1/2)");
        mpfr_set_ui(x, 2, MPFR_RNDN);
        mpfr_rec_sqrt(x, x, MPFR_RNDD);
        print_hex("  sqrt1_2_down = Rounded ", x, "\n", prec);

        mpfr_set_ui(x, 2, MPFR_RNDN);
        mpfr_rec_sqrt(x, x, MPFR_RNDU);
        print_hex("  sqrt1_2_up   = Rounded ", x, "\n", prec);

        puts("  -- sqrt(2)-1");
        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 2, MPFR_RNDD);
        mpfr_sub_ui(x, x, 1, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        print_hex("  sqrt2m1_down = Rounded ", x, "\n", prec);

        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 2, MPFR_RNDU);
        mpfr_sub_ui(x, x, 1, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        print_hex("  sqrt2m1_up   = Rounded ", x, "\n", prec);

        puts("  -- 3 - 2 * sqrt(2)");
        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 8, MPFR_RNDU);
        mpfr_ui_sub(x, 3, x, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        print_hex("  three_minus_2sqrt2_down = Rounded ", x, "\n", prec);

        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 8, MPFR_RNDD);
        mpfr_ui_sub(x, 3, x, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        print_hex("  three_minus_2sqrt2_up   = Rounded ", x, "\n", prec);

        puts("  -- 2 - sqrt(2)");
        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 2, MPFR_RNDU);
        mpfr_ui_sub(x, 2, x, MPFR_RNDD);
        mpfr_prec_round(x, prec, MPFR_RNDD);
        print_hex("  two_minus_sqrt2_down = Rounded ", x, "\n", prec);

        mpfr_set_prec(x, prec * 2);
        mpfr_sqrt_ui(x, 2, MPFR_RNDD);
        mpfr_ui_sub(x, 2, x, MPFR_RNDU);
        mpfr_prec_round(x, prec, MPFR_RNDU);
        print_hex("  two_minus_sqrt2_up   = Rounded ", x, "\n", prec);

        mpfr_clear(x);
        mpfr_clear(one);
        mpfr_clear(half);
        mpfr_clear(minus_half);
    }
}

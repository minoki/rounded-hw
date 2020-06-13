/* NB: We need some tricks to include "ghcconfig.h" from assembly source files. See Setup.hs for details. */
#include "ghcconfig.h"
#if LEADING_UNDERSCORE
#define SYMBOL2(name) _##name
#define SYMBOL(name) SYMBOL2(name)
#else
#define SYMBOL(name) name
#endif

	.globl SYMBOL(rounded_hw_interval_backend_name)
SYMBOL(rounded_hw_interval_backend_name):
	.string "AVX512"

	#
	# rounded_hw_interval_add
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> Double# -- lower 2 (%xmm3)
	#   -> Double# -- upper 2 (%xmm4)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	.globl SYMBOL(rounded_hw_interval_add)
SYMBOL(rounded_hw_interval_add):
	vaddsd {rd-sae}, %xmm3, %xmm1, %xmm1  # xmm1 = xmm1[0] + xmm3[0], xmm1[1] (downward)
	vaddsd {ru-sae}, %xmm4, %xmm2, %xmm2  # xmm2 = xmm2[0] + xmm4[0], xmm2[1] (downward)
	jmp *(%rbp)

	#
	# rounded_hw_interval_sub
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> Double# -- lower 2 (%xmm3)
	#   -> Double# -- upper 2 (%xmm4)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	.globl SYMBOL(rounded_hw_interval_sub)
SYMBOL(rounded_hw_interval_sub):
	vsubsd {rd-sae}, %xmm4, %xmm1, %xmm1  # xmm1 = xmm1[0] - xmm4[0], xmm1[1] (downward)
	vsubsd {ru-sae}, %xmm3, %xmm2, %xmm2  # xmm2 = xmm2[0] - xmm3[0], xmm2[1] (upward)
	jmp *(%rbp)

	#
	# rounded_hw_interval_recip
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	.globl SYMBOL(rounded_hw_interval_recip)
SYMBOL(rounded_hw_interval_recip):
	vmovsd LC0(%rip), %xmm4               # xmm4 = 1.0, zero
	vdivsd {rd-sae}, %xmm2, %xmm4, %xmm3  # xmm3 = xmm4[0] / xmm2[0], xmm4[1] (downward)
	vdivsd {ru-sae}, %xmm1, %xmm4, %xmm2  # xmm2 = xmm4[0] / xmm1[0], xmm4[1] (upward)
	vmovapd %xmm3, %xmm1                  # xmm1 = xmm3
	jmp *(%rbp)
LC0:
	.quad 0x3FF0000000000000   # 1.0 in binary64
	# 0b0011_1111_1111_0000_..._0000
	#   ^^----+------^ ^-----+-----^
	#   |     |              +-- trailing significand field
	#   |     +-- biased exponent
	#   +-- sign

	#
	# rounded_hw_interval_sqrt
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	.globl SYMBOL(rounded_hw_interval_sqrt)
SYMBOL(rounded_hw_interval_sqrt):
	vmovq %xmm1, %xmm1                     # xmm1 = xmm1[0], zero
	vsqrtsd {rd-sae}, %xmm1, %xmm1, %xmm1  # xmm1 = sqrt(xmm1[0]), xmm1[1] (downward)
	vmovq %xmm2, %xmm2                     # xmm2 = xmm2[0], zero
	vsqrtsd {ru-sae}, %xmm2, %xmm2, %xmm2  # xmm2 = sqrt(xmm2[0]), xmm2[1] (upward)
	jmp *(%rbp)

	#
	# rounded_hw_interval_from_int64
	#   :: Int(64)# -- input (%rbx)
	#   -> (# Double# -- lower (%xmm1)
	#       , Double# -- upper (%xmm2)
	#       #)
	#
	.globl SYMBOL(rounded_hw_interval_from_int64)
SYMBOL(rounded_hw_interval_from_int64):
	vxorps %xmm2, %xmm2, %xmm2               # xmm2 = zero
	vcvtsi2sdq %rbx, {rd-sae}, %xmm2, %xmm1  # xmm1 = (double)(int64)rbx, xmm2[1] (downward)
	vcvtsi2sdq %rbx, {ru-sae}, %xmm2, %xmm2  # xmm2 = (double)(int64)rbx, xmm2[1] (upward)
	jmp *(%rbp)

	#
	# rounded_hw_interval_from_word64
	#   :: Word(64)# -- input (%rbx)
	#   -> (# Double# -- lower (%xmm1)
	#       , Double# -- upper (%xmm2)
	#       #)
	#
	.globl SYMBOL(rounded_hw_interval_from_word64)
SYMBOL(rounded_hw_interval_from_word64):
	vxorps %xmm2, %xmm2, %xmm2               # xmm2 = zero
	vcvtusi2sdq %rbx, {rd-sae}, %xmm2, %xmm1  # xmm1 = (double)(uint64)rbx, xmm2[1] (downward)
	vcvtusi2sdq %rbx, {ru-sae}, %xmm2, %xmm2  # xmm2 = (double)(uint64)rbx, xmm2[1] (upward)
	jmp *(%rbp)

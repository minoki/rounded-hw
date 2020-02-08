	# Haskell declaration:
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
	.globl _rounded_hw_interval_add
_rounded_hw_interval_add:
	vaddsd {rd-sae}, %xmm3, %xmm1, %xmm1
	vaddsd {ru-sae}, %xmm4, %xmm2, %xmm2
	jmp *(%rbp)

	# Haskell declaration:
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
	.globl _rounded_hw_interval_sub
_rounded_hw_interval_sub:
	vsubsd {rd-sae}, %xmm4, %xmm1, %xmm1
	vsubsd {ru-sae}, %xmm3, %xmm2, %xmm2
	jmp *(%rbp)

	# Haskell declaration:
	#
	# rounded_hw_interval_recip
	#   :: Double# -- lower 1 (%xmm1)
	#   -> Double# -- upper 1 (%xmm2)
	#   -> (# Double#  -- lower (%xmm1)
	#       , Double#  -- upper (%xmm2)
	#       #)
	#
	# In pseudo C code:
	#
	# struct { double lo /* xmm1 */, hi /* xmm2 */; }
	#   rounded_hw_interval_recip(double lo /* xmm1 */ , double hi /* xmm2 */)
	# {
	#     double resultLo = div_down(1.0, hi);
	#     double resultHi = div_up(1.0, lo);
	#     return {resultLo, resultHi};
	# }
	#
	.globl _rounded_hw_interval_recip
_rounded_hw_interval_recip:
	vmovsd LC0(%rip), %xmm4 ## xmm4 = mem[0],zero
	vdivsd {rd-sae}, %xmm2, %xmm4, %xmm3
	vdivsd {ru-sae}, %xmm1, %xmm4, %xmm2
	vmovapd %xmm3, %xmm1
	jmp *(%rbp)
LC0:
	.quad 0x3FF0000000000000   # 1.0 in binary64
	# 0b0011_1111_1111_0000_..._0000
	#   ^^----+------^ ^-----+-----^
	#   |     |              +-- trailing significand field
	#   |     +-- biased exponent
	#   +-- sign

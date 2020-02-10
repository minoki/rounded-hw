/* NB: We need some tricks to include "ghcconfig.h" from assembly source files. See Setup.hs for details. */
#include "ghcconfig.h"
#if LEADING_UNDERSCORE
#define SYMBOL2(name) _##name
#define SYMBOL(name) SYMBOL2(name)
#else
#define SYMBOL(name) name
#endif

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
	.globl SYMBOL(rounded_hw_interval_add)
SYMBOL(rounded_hw_interval_add):
	stmxcsr -8(%rbp)
	movl -8(%rbp), %eax
	movl %eax, %ecx
	andl $0x9FFF, %ecx
	orl $0x2000, %ecx    # downward
	movl %ecx, -4(%rbp)
	ldmxcsr -4(%rbp)
	addsd %xmm3, %xmm1
	xorl $0x6000, %ecx   # downward -> upward
	movl %ecx, -4(%rbp)
	ldmxcsr -4(%rbp)
	addsd %xmm4, %xmm2
	ldmxcsr -8(%rbp)
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
	.globl SYMBOL(rounded_hw_interval_sub)
SYMBOL(rounded_hw_interval_sub):
	stmxcsr -8(%rbp)
	movl -8(%rbp), %eax
	movl %eax, %ecx
	andl $0x9FFF, %ecx
	orl $0x2000, %ecx    # downward
	movl %ecx, -4(%rbp)
	ldmxcsr -4(%rbp)
	subsd %xmm4, %xmm1
	xorl $0x6000, %ecx   # downward -> upward
	movl %ecx, -4(%rbp)
	ldmxcsr -4(%rbp)
	subsd %xmm3, %xmm2
	ldmxcsr -8(%rbp)
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
	#     unsigned int reg = _mm_getcsr();
	#     _mm_setcsr((reg & ~(3u << 13)) | (1 << 13)); // downward
	#     double resultLo = 1.0 / hi;
	#     _mm_setcsr((reg & ~(3u << 13)) | (2 << 13)); // upward
	#     double resultHi = 1.0 / lo;
	#     _mm_setcsr(reg);
	#     return {resultLo, resultHi};
	# }
	#
	.globl SYMBOL(rounded_hw_interval_recip)
SYMBOL(rounded_hw_interval_recip):
	stmxcsr -8(%rbp)
	movl -8(%rbp), %eax
	movl %eax, %ecx
	andl $0x9FFF, %ecx
	orl $0x2000, %ecx    # downward
	movl %ecx, -4(%rbp)
	ldmxcsr -4(%rbp)
	movsd LC0(%rip), %xmm3
	movapd %xmm3, %xmm4
	divsd %xmm2, %xmm3
	xorl $0x6000, %ecx   # downward -> upward
	movl %ecx, -4(%rbp)
	ldmxcsr -4(%rbp)
	divsd %xmm1, %xmm4
	ldmxcsr -8(%rbp)
	movapd %xmm3, %xmm1
	movapd %xmm4, %xmm2
	jmp *(%rbp)
LC0:
	.quad 0x3FF0000000000000   # 1.0 in binary64
	# 0b0011_1111_1111_0000_..._0000
	#   ^^----+------^ ^-----+-----^
	#   |     |              +-- trailing significand field
	#   |     +-- biased exponent
	#   +-- sign

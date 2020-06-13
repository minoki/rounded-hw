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
	.string "SSE2"

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
	stmxcsr -8(%rbp)     # *(int32*)(rbp-8) = MXCSR
	movl -8(%rbp), %ecx  # ecx = *(int32*)(rbp-8)
	andl $0x9FFF, %ecx   # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx    # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, -4(%rbp)  # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)     # MXCSR = *(int32*)(rbp-4)
	addsd %xmm3, %xmm1   # xmm1 = xmm1[0] + xmm3[0], xmm1[1]
	xorl $0x6000, %ecx   # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, -4(%rbp)  # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)     # MXCSR = *(int32*)(rbp-4)
	addsd %xmm4, %xmm2   # xmm2 = xmm2[0] + xmm4[0], xmm2[1]
	ldmxcsr -8(%rbp)     # MXCSR = *(int32*)(rbp-8)
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
	stmxcsr -8(%rbp)     # *(int32*)(rbp-8) = MXCSR
	movl -8(%rbp), %ecx  # ecx = *(int32*)(rbp-8)
	andl $0x9FFF, %ecx   # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx    # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, -4(%rbp)  # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)     # MXCSR = *(int32*)(rbp-4)
	subsd %xmm4, %xmm1   # xmm1 = xmm1[0] - xmm4[0], xmm1[1]
	xorl $0x6000, %ecx   # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, -4(%rbp)  # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)     # MXCSR = *(int32*)(rbp-4)
	subsd %xmm3, %xmm2   # xmm2 = xmm2[0] - xmm3[0], xmm2[1]
	ldmxcsr -8(%rbp)     # MXCSR = *(int32*)(rbp-4)
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
	stmxcsr -8(%rbp)       # *(int32*)(rbp-8) = MXCSR
	movl -8(%rbp), %ecx    # ecx = *(int32*)(rbp-8)
	andl $0x9FFF, %ecx     # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx      # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, -4(%rbp)    # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)       # MXCSR = *(int32*)(rbp-4); set downward
	movsd LC0(%rip), %xmm3 # xmm3 = (double)1.0,zero
	movapd %xmm3, %xmm4    # xmm4 = xmm3
	divsd %xmm2, %xmm3     # xmm3 = xmm3[0] / xmm2[0], xmm3[1]
	xorl $0x6000, %ecx     # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, -4(%rbp)    # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)       # MXCSR = *(int32*)(rbp-4); set upward
	divsd %xmm1, %xmm4     # xmm4 = xmm4[0] / xmm1[0], xmm4[1]
	ldmxcsr -8(%rbp)       # MXCSR = *(int32*)(rbp-8); restore
	movapd %xmm3, %xmm1    # xmm1 = xmm3
	movapd %xmm4, %xmm2    # xmm2 = xmm4
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
	stmxcsr -8(%rbp)     # *(int32*)(rbp-8) = MXCSR
	movl -8(%rbp), %ecx  # ecx = *(int32*)(rbp-8)
	andl $0x9FFF, %ecx   # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx    # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, -4(%rbp)  # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)     # MXCSR = *(int32*)(rbp-4); set downward
	sqrtsd %xmm1, %xmm1  # xmm1 = sqrt(xmm1[0]), xmm1[1]
	xorl $0x6000, %ecx   # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, -4(%rbp)  # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)     # MXCSR = *(int32*)(rbp-4); set upward
	sqrtsd %xmm2, %xmm2  # xmm2 = sqrt(xmm2[0]), xmm2[1]
	ldmxcsr -8(%rbp)     # MXCSR = *(int32*)(rbp-8); restore
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
	stmxcsr -8(%rbp)      # *(int32*)(rbp-8) = MXCSR
	movl -8(%rbp), %ecx   # ecx = *(int32*)(rbp-8)
	andl $0x9FFF, %ecx    # ecx = ecx & 0x9FFF; clear Rounding Control field
	orl $0x2000, %ecx     # ecx = ecx | 0x2000; set RC = downward
	movl %ecx, -4(%rbp)   # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)      # MXCSR = *(int32*)(rbp-4); set downward
	pxor %xmm1, %xmm1     # xmm1 = zero
	cvtsi2sdq %rbx, %xmm1 # xmm1 = (double)(int64)rbx, xmm1[1]
	xorl $0x6000, %ecx    # ecx = ecx ^ 0x6000; downward -> upward
	movl %ecx, -4(%rbp)   # *(int32*)(rbp-4) = ecx
	ldmxcsr -4(%rbp)      # MXCSR = *(int32*)(rbp-4); set upward
	pxor %xmm2, %xmm2     # xmm2 = zero
	cvtsi2sdq %rbx, %xmm2 # xmm2 = (double)(int64)rbx, xmm2[1]
	ldmxcsr -8(%rbp)      # MXCSR = *(int32*)(rbp-8); restore
	jmp *(%rbp)

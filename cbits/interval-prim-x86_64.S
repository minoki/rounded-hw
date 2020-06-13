#ifdef __AVX512F__
#include "interval-prim-x86_64-avx512.S"
#else
#include "interval-prim-x86_64-sse2.S"
#endif

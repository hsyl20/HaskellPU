#ifndef CUDA_KERNELS
#define CUDA_KERNELS

void cuda_mat_sub(unsigned x, unsigned y, unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB);

void cuda_mat_duplicate(unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB);
#endif

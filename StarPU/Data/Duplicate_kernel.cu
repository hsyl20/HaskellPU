#include <cuda_runtime.h>

__global__ void MatDuplicate(unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB) {

  unsigned gx = blockDim.x * blockIdx.x + threadIdx.x;
  unsigned gy = blockDim.y * blockIdx.y + threadIdx.y;
  if (gx < w && gy < h)
    B[gy*ldB + gx] = A[gy*ldA + gx];
}


extern "C" void cuda_mat_duplicate(unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB) {

  dim3 grid((w + 15) / 15, (h + 15) / 15, 1);
  dim3 block(16,16,1);
  MatDuplicate<<<grid,block>>>(w, h, A, ldA, B, ldB);
}


#include <cuda_runtime.h>

__global__ void MatAdd(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC) {

  unsigned gx = blockDim.x * blockIdx.x + threadIdx.x;
  unsigned gy = blockDim.y * blockIdx.y + threadIdx.y;
  if (gx < w && gy < h)
    C[gy*ldC + gx] = A[gy*ldA + gx] + B[gy*ldB + gx];
}


extern "C" void cuda_mat_add(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC) {

  dim3 grid((w + 15) / 15, (h + 15) / 15, 1);
  dim3 block(16,16,1);
  MatAdd<<<grid,block>>>(w, h, A, ldA, B, ldB, C, ldC);
}

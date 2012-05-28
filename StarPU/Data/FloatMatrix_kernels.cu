#include <cuda_runtime.h>

/********** ADD **********/

__global__ void FloatMatrixAdd(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC) {
  unsigned gx = blockDim.x * blockIdx.x + threadIdx.x;
  unsigned gy = blockDim.y * blockIdx.y + threadIdx.y;
  if (gx < w && gy < h)
    C[gy*ldC + gx] = A[gy*ldA + gx] + B[gy*ldB + gx];
}

extern "C" void cuda_floatmatrix_add(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC) {
  dim3 grid((w + 15) / 15, (h + 15) / 15, 1);
  dim3 block(16,16,1);
  FloatMatrixAdd<<<grid,block>>>(w, h, A, ldA, B, ldB, C, ldC);
}

/********** SUB **********/

__global__ void FloatMatrixSub(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC) {
  unsigned gx = blockDim.x * blockIdx.x + threadIdx.x;
  unsigned gy = blockDim.y * blockIdx.y + threadIdx.y;
  if (gx < w && gy < h)
    C[gy*ldC + gx] = A[gy*ldA + gx] - B[gy*ldB + gx];
}

extern "C" void cuda_floatmatrix_sub(unsigned w, unsigned h, const float* A, unsigned ldA, const float* B, unsigned ldB, float* C, unsigned ldC) {

  dim3 grid((w + 15) / 15, (h + 15) / 15, 1);
  dim3 block(16,16,1);
  FloatMatrixSub<<<grid,block>>>(w, h, A, ldA, B, ldB, C, ldC);
}

/********** DUPLICATE **********/

__global__ void FloatMatrixDuplicate(unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB) {

  unsigned gx = blockDim.x * blockIdx.x + threadIdx.x;
  unsigned gy = blockDim.y * blockIdx.y + threadIdx.y;
  if (gx < w && gy < h)
    B[gy*ldB + gx] = A[gy*ldA + gx];
}

extern "C" void cuda_floatmatrix_duplicate(unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB) {

  dim3 grid((w + 15) / 15, (h + 15) / 15, 1);
  dim3 block(16,16,1);
  FloatMatrixDuplicate<<<grid,block>>>(w, h, A, ldA, B, ldB);
}


/********** SUB MATRIX **********/

__global__ void FloatMatrixSubMatrix(unsigned x, unsigned y, unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB) {

  unsigned gx = blockDim.x * blockIdx.x + threadIdx.x;
  unsigned gy = blockDim.y * blockIdx.y + threadIdx.y;
  if (gx < w && gy < h)
    B[gy*ldB + gx] = A[(gy+y)*ldA + gx + x];
}

extern "C" void cuda_floatmatrix_submatrix(unsigned x, unsigned y, unsigned w, unsigned h, const float* A, unsigned ldA, float* B, unsigned ldB) {

  dim3 grid((w + 15) / 15, (h + 15) / 15, 1);
  dim3 block(16,16,1);
  FloatMatrixSubMatrix<<<grid,block>>>(x, y, w, h, A, ldA, B, ldB);
}


/********** SET **********/

__global__ void FloatMatrixSet(unsigned w, unsigned h, float value, float *A, unsigned ldA) {
  unsigned gx = blockDim.x * blockIdx.x + threadIdx.x;
  unsigned gy = blockDim.y * blockIdx.y + threadIdx.y;
  if (gx < w && gy < h)
    A[gy*ldA + gx] = value;
}

extern "C" void cuda_floatmatrix_set(unsigned w, unsigned h, float value, float* A, unsigned ldA) {

  dim3 grid((w + 15) / 15, (h + 15) / 15, 1);
  dim3 block(16,16,1);
  FloatMatrixSet<<<grid,block>>>(w, h, value, A, ldA);
}

/********** TRANSPOSE **********/

__global__ void FloatMatrixTranspose(unsigned w, unsigned h, float *A, unsigned ldA, float *B, unsigned ldB) {
  unsigned gx = blockDim.x * blockIdx.x + threadIdx.x;
  unsigned gy = blockDim.y * blockIdx.y + threadIdx.y;
  if (gx < w && gy < h)
    B[gx*ldB + gy] = A[gy*ldA+gx];
}

extern "C" void cuda_floatmatrix_transpose(unsigned w, unsigned h, float* A, unsigned ldA, float* B, unsigned ldB) {

  dim3 grid((w + 15) / 15, (h + 15) / 15, 1);
  dim3 block(16,16,1);
  FloatMatrixTranspose<<<grid,block>>>(w, h, A, ldA, B, ldB);
}


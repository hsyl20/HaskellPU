#include <starpu.h>
#include <starpu_cuda.h>
#include <stdlib.h>
#include <cublas_v2.h>

#include "../Task.h"
#include "../Platform.h"
#include "FloatMatrix_kernels.h"

static struct starpu_perfmodel spotrf_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_SPOTRF"
};

static void spotrf_cuda(void *descr[], void *args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[0]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  
  cublasSetStream(cublas_handle, starpu_cuda_get_local_stream());

  cuda_floatmatrix_duplicate(w, h, a, lda, b, ldb);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());

  float *lambda11;
  cudaHostAlloc((void **)&lambda11, sizeof(float), 0);

  int z;
  for (z = 0; z < w; z++) {
    cudaMemcpyAsync(lambda11, &b[z+z*ldb], sizeof(float), cudaMemcpyDeviceToHost, starpu_cuda_get_local_stream());
    cudaStreamSynchronize(starpu_cuda_get_local_stream());

    STARPU_ASSERT(*lambda11 != 0.0f);

    *lambda11 = sqrt(*lambda11);

    cudaMemcpyAsync(&b[z+z*ldb], lambda11, sizeof(float), cudaMemcpyHostToDevice, starpu_cuda_get_local_stream());

    float v = 1.0f/(*lambda11);
    cublasSscal(cublas_handle, w - z - 1, &v, &b[(z+1)*ldb +z], ldb);

    float alpha = -1.0f;
    cublasSsyr(cublas_handle, CUBLAS_FILL_MODE_UPPER, w - z - 1, &alpha,
		    &b[(z+1)+ z*ldb], 1,
		    &b[(z+1)+(z+1)*ldb], ldb);
  }

  cudaStreamSynchronize(starpu_cuda_get_local_stream());
  cudaFreeHost(lambda11);

}

static void spotrf_cpu(void *descr[], void *_args) {
}

static struct starpu_codelet spotrf_codelet =
{
  .modes = { STARPU_R, STARPU_W },
  .where = STARPU_CUDA,
  .cpu_funcs = {spotrf_cpu, NULL},
  .cuda_funcs = {spotrf_cuda, NULL},
  .nbuffers = 2,
  .model = &spotrf_model
};

struct starpu_task * floatmatrix_spotrf_task_create(starpu_data_handle_t a, starpu_data_handle_t b) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &spotrf_codelet;
  task->handles[0] = a;
  task->handles[1] = b;

  return task;
}


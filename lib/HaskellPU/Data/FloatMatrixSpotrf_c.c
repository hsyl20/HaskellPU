#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas_v2.h>

#include "../Task.h"
#include "../Platform.h"
#include "Common.h"
#include "FloatMatrix_kernels.h"

extern int spotrf_(char *uplo, int *n, float *a, int *lda, int *info);

static double cpu_cost(struct starpu_task *task, enum starpu_perf_archtype arch, unsigned nimpl) {
	uint32_t n = starpu_matrix_get_nx(task->handles[0]);

	return (((double)(n)*n*n)/1000.0f*0.894/0.79176);
}

static double cuda_cost(struct starpu_task *task, enum starpu_perf_archtype arch, unsigned nimpl) {
	uint32_t n = starpu_matrix_get_nx(task->handles[0]);

	return (((double)(n)*n*n)/50.0f/10.75/5.088633/0.9883);
}

static struct starpu_perfmodel spotrf_model =
{
  .per_arch =
  {
    [STARPU_CPU_DEFAULT][0] = { .cost_function = cpu_cost },
    [STARPU_CUDA_DEFAULT][0] = { .cost_function = cuda_cost }
  },
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_SPOTRF"
};

static void spotrf_cuda(void *descr[], void *args) {
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[0]);

  unsigned w = STARPU_MATRIX_GET_NY(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NX(descr[0]);

  unsigned ldb = STARPU_MATRIX_GET_LD(descr[0]);
  
  cublasSetStream(cublas_handle, starpu_cuda_get_local_stream());

  float *lambda11;
  cudaHostAlloc((void **)&lambda11, sizeof(float), 0);

  unsigned z;
  for (z = 0; z < w; z++) {
    cudaMemcpyAsync(lambda11, &b[z+z*ldb], sizeof(float), cudaMemcpyDeviceToHost, starpu_cuda_get_local_stream());
    cudaStreamSynchronize(starpu_cuda_get_local_stream());

    STARPU_ASSERT(*lambda11 != 0.0f);

    *lambda11 = sqrt(*lambda11);

    cudaMemcpyAsync(&b[z+z*ldb], lambda11, sizeof(float), cudaMemcpyHostToDevice, starpu_cuda_get_local_stream());

    float alpha = 1.0f/(*lambda11);
    cublasSscal(cublas_handle, w - z - 1, &alpha, &b[(z+1)+z*ldb], 1);

    float beta = -1.0f;
    cublasSsyr(cublas_handle, CUBLAS_FILL_MODE_UPPER, w - z - 1, &beta,
        &b[(z+1)+z*ldb], 1,
        &b[(z+1)+(z+1)*ldb], ldb);
  }

  cudaStreamSynchronize(starpu_cuda_get_local_stream());
  cudaFreeHost(lambda11);

}

static void spotrf_cpu(void *descr[], void *_args) {
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[0]);

  unsigned w = STARPU_MATRIX_GET_NY(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NX(descr[0]);

  unsigned ldb = STARPU_MATRIX_GET_LD(descr[0]);

  char uplo = 'L';
  int info;
  spotrf_(&uplo, (int*)&w, b, (int*)&ldb, &info);
}

static struct starpu_codelet spotrf_codelet =
{
  .modes = { STARPU_RW },
  .where = STARPU_CUDA | STARPU_CPU,
  .cpu_funcs = {spotrf_cpu, NULL},
  .cuda_funcs = {spotrf_cuda, NULL},
  .nbuffers = 1,
  .model = &spotrf_model
};

struct starpu_task * floatmatrix_spotrf_task_create(starpu_data_handle_t a) {

  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &spotrf_codelet;
  task->handles[0] = a;

  return task;
}


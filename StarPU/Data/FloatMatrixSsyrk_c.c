#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas_v2.h>

#include "../Task.h"
#include "../Platform.h"

static double ssyrk_cpu_cost(struct starpu_task *task, enum starpu_perf_archtype arch, unsigned nimpl) {
  int32_t n = starpu_matrix_get_nx(task->handles[0]);
  double cost = (((double)(n)*n*n)/50.0f/10.75/8.0760);
  return cost;
}

static double ssyrk_cuda_cost(struct starpu_task *task, enum starpu_perf_archtype arch, unsigned nimpl) {
  uint32_t n = starpu_matrix_get_nx(task->handles[0]);
  double cost = (((double)(n)*n*n)/50.0f/10.75/76.30666);
  return cost;
}

static struct starpu_perfmodel ssyrk_model =
{
  .per_arch =
  {
    [STARPU_CPU_DEFAULT][0] = { .cost_function = ssyrk_cpu_cost },
    [STARPU_CUDA_DEFAULT][0] = { .cost_function = ssyrk_cuda_cost }
  },
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_SSYRK"
};

struct ssyrk_arg {
  int uplo;
  int trans;
};

static void ssyrk_cuda(void *descr[], void *args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *c = (float *)STARPU_MATRIX_GET_PTR(descr[1]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[0]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldc = STARPU_MATRIX_GET_LD(descr[1]);

  cublasSetStream(cublas_handle,starpu_cuda_get_local_stream());

  struct ssyrk_arg *arg = (struct ssyrk_arg*)args;

  cublasFillMode_t uplo = arg->uplo ? CUBLAS_FILL_MODE_LOWER : CUBLAS_FILL_MODE_UPPER;
  cublasOperation_t trans = arg->trans ? CUBLAS_OP_T : CUBLAS_OP_N;

  float alpha = 1.0f;
  float beta = 0.0f;
  cublasSsyrk(cublas_handle, uplo, trans, w, h, &alpha, a, lda, &beta, c, ldc);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());

  free(args);
}

static void ssyrk_cpu(void *descr[], void *args) {
}

static struct starpu_codelet ssyrk_codelet =
{
  .modes = { STARPU_R, STARPU_R, STARPU_W },
  .where = STARPU_CUDA,
  .cpu_funcs = {ssyrk_cpu, NULL},
  .cuda_funcs = {ssyrk_cuda, NULL},
  .nbuffers = 2,
  .model = &ssyrk_model
};

struct starpu_task * floatmatrix_ssyrk_task_create(int uplo, int trans, starpu_data_handle_t a, starpu_data_handle_t c) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &ssyrk_codelet;
  task->handles[0] = a;
  task->handles[1] = c;

  task->cl_arg = malloc(sizeof(struct ssyrk_arg));
  task->cl_arg_size = sizeof(struct ssyrk_arg);
  ((struct ssyrk_arg*)task->cl_arg)->uplo = uplo;
  ((struct ssyrk_arg*)task->cl_arg)->trans = trans;

  return task;
}

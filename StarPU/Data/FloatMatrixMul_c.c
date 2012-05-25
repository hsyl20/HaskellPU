#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../Task.h"

static double sgemm_cpu_cost(struct starpu_task *task, enum starpu_perf_archtype arch, unsigned nimpl) {
  int32_t n = starpu_matrix_get_nx(task->handles[0]);
  double cost = (((double)(n)*n*n)/50.0f/10.75/8.0760);
  return cost;
}

static double sgemm_cuda_cost(struct starpu_task *task, enum starpu_perf_archtype arch, unsigned nimpl) {
  uint32_t n = starpu_matrix_get_nx(task->handles[0]);
  double cost = (((double)(n)*n*n)/50.0f/10.75/76.30666);
  return cost;
}

static struct starpu_perfmodel sgemm_model =
{
  .per_arch =
  {
    [STARPU_CPU_DEFAULT][0] = { .cost_function = sgemm_cpu_cost },
    [STARPU_CUDA_DEFAULT][0] = { .cost_function = sgemm_cuda_cost }
  },
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_MUL"
};

static void sgemm_cuda(void *descr[], void *_args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  float *c = (float *)STARPU_MATRIX_GET_PTR(descr[2]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[2]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[2]);
  unsigned k = STARPU_MATRIX_GET_NX(descr[0]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  unsigned ldc = STARPU_MATRIX_GET_LD(descr[2]);

  cublasSgemm('n', 'n', w, h, k, 1.0f, b, ldb, a, lda, 0.0f, c, ldc);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());
}

static void sgemm_cpu(void *descr[], void *_args) {
}

static struct starpu_codelet sgemm_codelet =
{
  .modes = { STARPU_R, STARPU_R, STARPU_W },
  .where = STARPU_CUDA,
  .cpu_funcs = {sgemm_cpu, NULL},
  .cuda_funcs = {sgemm_cuda, NULL},
  .nbuffers = 3,
  .model = &sgemm_model
};

struct starpu_task * floatmatrix_mul_task_create(starpu_data_handle_t a, starpu_data_handle_t b, starpu_data_handle_t c) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &sgemm_codelet;
  task->handles[0] = a;
  task->handles[1] = b;
  task->handles[2] = c;

  return task;
}

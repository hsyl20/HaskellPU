#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../Task.h"

static struct starpu_perfmodel duplicate_matrix_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "DUPLICATE_MATRIX"
};

static void duplicate_matrix_cuda(void *descr[], void *_args) {
}

struct Point {
  unsigned x,y;
};

static void duplicate_matrix_cpu(void *descr[], void *args) {
  float *src = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *dst = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  unsigned l0 = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned w1 = STARPU_MATRIX_GET_NX(descr[1]);
  unsigned h1 = STARPU_MATRIX_GET_NY(descr[1]);
  unsigned l1 = STARPU_MATRIX_GET_LD(descr[1]);

  unsigned i,j;

  for (i = 0; i < h1; i++) {
    for (j = 0; j < w1; j++) {
      dst[i*l1 + j] = src[i*l0 + j];
    }
  }
}

static struct starpu_codelet duplicate_matrix_codelet =
{
  .modes = { STARPU_R, STARPU_W },
  .where = STARPU_CPU,
  .cpu_funcs = {duplicate_matrix_cpu, NULL},
  .cuda_funcs = {duplicate_matrix_cuda, NULL},
  .nbuffers = 2,
  .model = &duplicate_matrix_model
};

struct starpu_task * duplicate_matrix_task_create(starpu_data_handle_t s, starpu_data_handle_t d) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &duplicate_matrix_codelet;
  task->handles[0] = s;
  task->handles[1] = d;

  return task;
}


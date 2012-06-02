#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../Task.h"
#include "FloatMatrix_kernels.h"

static struct starpu_perfmodel set_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_SET"
};

static void set_cuda(void *descr[], void *arg) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[0]);
  float value = *(float*)arg;

  cuda_floatmatrix_set(w, h, value, a, lda);
  free(arg);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());
}

static void set_cpu(void *descr[], void *arg) {
  float *a 	= (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[0]);

  float value = *(float*)arg;
  unsigned i,j;

  for (i = 0; i < h; i++) {
    for (j = 0; j < w; j++) {
      a[i*lda + j] = value;
    }
  }

  free(arg);
}

static struct starpu_codelet set_codelet =
{
  .modes = { STARPU_W },
  .where = STARPU_CPU | STARPU_CUDA,
  .cpu_funcs = {set_cpu, NULL},
  .cuda_funcs = {set_cuda, NULL},
  .nbuffers = 1,
  .model = &set_model
};

struct starpu_task * floatmatrix_set_task_create(float value, starpu_data_handle_t d) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &set_codelet;
  task->handles[0] = d;

  float * arg = malloc(sizeof(float));
  *arg = value;
  task->cl_arg = arg;
  task->cl_arg_size = sizeof(float);

  return task;
}

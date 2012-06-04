#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../Task.h"
#include "FloatMatrix_kernels.h"

static struct starpu_perfmodel duplicate_matrix_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_DUPLICATE"
};

static void duplicate_matrix_cuda(void *descr[], void *_args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);

  unsigned w = STARPU_MATRIX_GET_NY(descr[1]);
  unsigned h = STARPU_MATRIX_GET_NX(descr[1]);

  cuda_floatmatrix_duplicate(w,h,a,lda,b,ldb);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());
}

static void duplicate_matrix_cpu(void *descr[], void *args) {
  float *src = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *dst = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  unsigned l0 = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned w = STARPU_MATRIX_GET_NY(descr[1]);
  unsigned h = STARPU_MATRIX_GET_NX(descr[1]);
  unsigned l1 = STARPU_MATRIX_GET_LD(descr[1]);

  unsigned i,j;

  for (i = 0; i < w; i++) {
    memcpy(&dst[i*l1], &src[i*l0], h*sizeof(float));
  }
}

static struct starpu_codelet duplicate_matrix_codelet =
{
  .modes = { STARPU_R, STARPU_W },
  .where = STARPU_CPU | STARPU_CUDA,
  .cpu_funcs = {duplicate_matrix_cpu, NULL},
  .cuda_funcs = {duplicate_matrix_cuda, NULL},
  .nbuffers = 2,
  .model = &duplicate_matrix_model
};

struct starpu_task * floatmatrix_duplicate_task_create(starpu_data_handle_t s, starpu_data_handle_t d) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &duplicate_matrix_codelet;
  task->handles[0] = s;
  task->handles[1] = d;

  return task;
}


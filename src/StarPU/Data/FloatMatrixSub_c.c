#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../Task.h"
#include "FloatMatrix_kernels.h"

static struct starpu_perfmodel matsub_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_SUB"
};

static void matsub_cuda(void *descr[], void *_args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  float *c = (float *)STARPU_MATRIX_GET_PTR(descr[2]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[1]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  unsigned ldc = STARPU_MATRIX_GET_LD(descr[2]);

  cuda_floatmatrix_sub(w,h,a,lda,b,ldb,c,ldc);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());
}

static void matsub_cpu(void *descr[], void *_args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  float *c = (float *)STARPU_MATRIX_GET_PTR(descr[2]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[1]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  unsigned ldc = STARPU_MATRIX_GET_LD(descr[2]);

  unsigned i,j;
  for (j=0; j<w; j++) {
    for (i=0; i<h; i++) {
      c[j*ldc+i] = a[j*lda+i] - b[j*ldb+i];
    }
  }
}

static struct starpu_codelet matsub_codelet =
{
  .modes = { STARPU_R, STARPU_R, STARPU_W },
  .where = STARPU_CPU | STARPU_CUDA,
  .cpu_funcs = {matsub_cpu, NULL},
  .cuda_funcs = {matsub_cuda, NULL},
  .nbuffers = 3,
  .model = &matsub_model
};

struct starpu_task * floatmatrix_sub_task_create(starpu_data_handle_t a, starpu_data_handle_t b, starpu_data_handle_t c) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &matsub_codelet;
  task->handles[0] = a;
  task->handles[1] = b;
  task->handles[2] = c;

  return task;
}


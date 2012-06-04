#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../Task.h"
#include "FloatMatrix_kernels.h"

static struct starpu_perfmodel transpose_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_TRANSPOSE"
};

static void transpose_cuda(void *descr[], void *_args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[0]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);

  cuda_floatmatrix_transpose(w,h,a,lda,b,ldb);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());
}

static void transpose_cpu(void *descr[], void *_args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[0]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);

  unsigned i,j;
  for (j=0; j<w; j++) {
    for (i=0; i<h; i++) {
      b[i*ldb+j] = a[j*lda+i];
    }
  }
}

static struct starpu_codelet transpose_codelet =
{
  .modes = { STARPU_R, STARPU_W },
  .where = STARPU_CUDA | STARPU_CPU,
  .cpu_funcs = {transpose_cpu, NULL},
  .cuda_funcs = {transpose_cuda, NULL},
  .nbuffers = 2,
  .model = &transpose_model
};

struct starpu_task * floatmatrix_transpose_task_create(starpu_data_handle_t a, starpu_data_handle_t b) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &transpose_codelet;
  task->handles[0] = a;
  task->handles[1] = b;

  return task;
}


#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../Task.h"
#include "FloatMatrix_kernels.h"

static struct starpu_perfmodel scale_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_SCALE"
};

static void scale_cuda(void *descr[], void *arg) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[0]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);

  float value = *(float*)arg;

  cuda_floatmatrix_scale(w,h,value,a,lda,b,ldb);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());
  free(arg);
}

static void scale_cpu(void *descr[], void *arg) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[0]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);

  float value = *(float*)arg;

  unsigned i,j;
  for (j=0; j<h; j++) {
    for (i=0; i<w; i++) {
      b[i*ldb+j] = value * a[i*lda+j];
    }
  }

  free(arg);
}

static struct starpu_codelet scale_codelet =
{
  .modes = { STARPU_R, STARPU_W },
  .where = STARPU_CUDA | STARPU_CPU,
  .cpu_funcs = {scale_cpu, NULL},
  .cuda_funcs = {scale_cuda, NULL},
  .nbuffers = 2,
  .model = &scale_model
};

struct starpu_task * floatmatrix_scale_task_create(float v, starpu_data_handle_t a, starpu_data_handle_t b) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &scale_codelet;
  task->handles[0] = a;
  task->handles[1] = b;

  task->cl_arg = malloc(sizeof(float));
  *(float*)task->cl_arg = v;
  task->cl_arg_size = sizeof(float);

  return task;
}


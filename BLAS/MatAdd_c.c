#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../StarPU/Task.h"
#include "MatAdd_kernel.h"

static struct starpu_perfmodel matadd_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "MATADD"
};

static void matadd_cuda(void *descr[], void *_args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  float *c = (float *)STARPU_MATRIX_GET_PTR(descr[2]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[1]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  unsigned ldc = STARPU_MATRIX_GET_LD(descr[2]);

  cuda_mat_add(w,h,a,lda,b,ldb,c,ldc);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());
}

static void matadd_cpu(void *descr[], void *_args) {
}

static struct starpu_codelet matadd_codelet =
{
  .modes = { STARPU_R, STARPU_R, STARPU_W },
  .where = STARPU_CUDA,
  .cpu_funcs = {matadd_cpu, NULL},
  .cuda_funcs = {matadd_cuda, NULL},
  .nbuffers = 3,
  .model = &matadd_model
};

struct starpu_task * matadd_task_create(starpu_data_handle_t a, starpu_data_handle_t b, starpu_data_handle_t c) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &matadd_codelet;
  task->handles[0] = a;
  task->handles[1] = b;
  task->handles[2] = c;

  return task;
}


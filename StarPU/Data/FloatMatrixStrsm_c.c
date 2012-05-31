#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../Task.h"
#include "FloatMatrix_kernels.h"

static struct starpu_perfmodel strsm_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_STRSM"
};

struct strsm_arg {
  int uplo;
  int unit;
  int side;
};


static void strsm_cuda(void *descr[], void *args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  float *x = (float *)STARPU_MATRIX_GET_PTR(descr[2]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[1]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  unsigned ldx = STARPU_MATRIX_GET_LD(descr[2]);
  
  struct strsm_arg * arg = (struct strsm_arg *)args;

  char side = arg->side ? 'r' : 'l';
  char uplo = arg->uplo ? 'l' : 'u';
  char diag = arg->unit ? 'u' : 'n';
  char trans = 't';

  cuda_floatmatrix_duplicate(w, h, b, ldb, x, ldx);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());

  const float factor = 1.0f;
  cublasStrsm(side, uplo, trans, diag, w, h, factor, a, lda, x, ldx);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());

  free(arg);
}

static void strsm_cpu(void *descr[], void *_args) {
}

static struct starpu_codelet strsm_codelet =
{
  .modes = { STARPU_R, STARPU_R, STARPU_W },
  .where = STARPU_CUDA,
  .cpu_funcs = {strsm_cpu, NULL},
  .cuda_funcs = {strsm_cuda, NULL},
  .nbuffers = 3,
  .model = &strsm_model
};

struct starpu_task * floatmatrix_strsm_task_create(int uplo, int unit, int side, starpu_data_handle_t a, starpu_data_handle_t b, starpu_data_handle_t x) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &strsm_codelet;
  task->handles[0] = a;
  task->handles[1] = b;
  task->handles[2] = x;

  task->cl_arg = malloc(sizeof(struct strsm_arg));
  task->cl_arg_size = sizeof(struct strsm_arg);
  ((struct strsm_arg*)task->cl_arg)->uplo = uplo;
  ((struct strsm_arg*)task->cl_arg)->unit = unit;
  ((struct strsm_arg*)task->cl_arg)->side = side;

  return task;
}


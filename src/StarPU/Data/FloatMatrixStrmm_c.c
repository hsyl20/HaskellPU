#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas_v2.h>

#include "../Task.h"
#include "../Platform.h"
#include "FloatMatrix_kernels.h"

static struct starpu_perfmodel strmm_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_STRMM"
};

struct strmm_arg {
  int uplo;
  int unit;
  int side;
};


static void strmm_cuda(void *descr[], void *args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  float *c = (float *)STARPU_MATRIX_GET_PTR(descr[2]);

  unsigned w = STARPU_MATRIX_GET_NX(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[1]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  unsigned ldc = STARPU_MATRIX_GET_LD(descr[2]);
  
  struct strmm_arg * arg = (struct strmm_arg *)args;

  cublasSideMode_t side = arg->side ? CUBLAS_SIDE_RIGHT : CUBLAS_SIDE_LEFT;
  cublasFillMode_t uplo = arg->uplo ? CUBLAS_FILL_MODE_LOWER : CUBLAS_FILL_MODE_UPPER;
  cublasDiagType_t diag = arg->unit ? CUBLAS_DIAG_UNIT : CUBLAS_DIAG_NON_UNIT;
  cublasOperation_t trans = CUBLAS_OP_T;

  const float factor = 1.0f;
  
  cublasSetStream(cublas_handle, starpu_cuda_get_local_stream());

  cublasStrmm(cublas_handle, side, uplo, trans, diag, w, h, &factor, a, lda, b, ldb, c, ldc);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());

  free(arg);
}

static void strmm_cpu(void *descr[], void *_args) {
}

static struct starpu_codelet strmm_codelet =
{
  .modes = { STARPU_R, STARPU_R, STARPU_W },
  .where = STARPU_CUDA,
  .cpu_funcs = {strmm_cpu, NULL},
  .cuda_funcs = {strmm_cuda, NULL},
  .nbuffers = 3,
  .model = &strmm_model
};

struct starpu_task * floatmatrix_strmm_task_create(int uplo, int unit, int side, starpu_data_handle_t a, starpu_data_handle_t b, starpu_data_handle_t x) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &strmm_codelet;
  task->handles[0] = a;
  task->handles[1] = b;
  task->handles[2] = x;

  task->cl_arg = malloc(sizeof(struct strmm_arg));
  task->cl_arg_size = sizeof(struct strmm_arg);
  ((struct strmm_arg*)task->cl_arg)->uplo = uplo;
  ((struct strmm_arg*)task->cl_arg)->unit = unit;
  ((struct strmm_arg*)task->cl_arg)->side = side;

  return task;
}


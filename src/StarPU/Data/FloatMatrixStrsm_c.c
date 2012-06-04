#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas_v2.h>

#include "../Task.h"
#include "../Platform.h"
#include "FloatMatrix_kernels.h"

extern void strsm_ (const char *side, const char *uplo, const char *transa, 
                   const char *diag, const int *m, const int *n,
                   const float *alpha, const float *A, const int *lda,
                   float *B, const int *ldb);

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

  unsigned w = STARPU_MATRIX_GET_NY(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NX(descr[1]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  unsigned ldx = STARPU_MATRIX_GET_LD(descr[2]);
  
  struct strsm_arg * arg = (struct strsm_arg *)args;

  cublasSideMode_t side = arg->side ? CUBLAS_SIDE_RIGHT : CUBLAS_SIDE_LEFT;
  cublasFillMode_t uplo = arg->uplo ? CUBLAS_FILL_MODE_LOWER : CUBLAS_FILL_MODE_UPPER;
  cublasDiagType_t diag = arg->unit ? CUBLAS_DIAG_UNIT : CUBLAS_DIAG_NON_UNIT;
  cublasOperation_t trans = CUBLAS_OP_N;

  cublasSetStream(cublas_handle, starpu_cuda_get_local_stream());

  cuda_floatmatrix_duplicate(w, h, b, ldb, x, ldx);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());

  const float factor = 1.0f;
  cublasStrsm(cublas_handle, side, uplo, trans, diag, w, h, &factor, a, lda, x, ldx);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());

  free(arg);
}

static void strsm_cpu(void *descr[], void *args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  float *x = (float *)STARPU_MATRIX_GET_PTR(descr[2]);

  unsigned w = STARPU_MATRIX_GET_NY(descr[0]);
  unsigned h = STARPU_MATRIX_GET_NX(descr[1]);

  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  unsigned ldx = STARPU_MATRIX_GET_LD(descr[2]);
  
  struct strsm_arg * arg = (struct strsm_arg *)args;

  const char side = arg->side ? 'R' : 'L';
  const char uplo = arg->uplo ? 'L' : 'U';
  const char diag = arg->unit ? 'U' : 'N';
  const char trans = 'N';

  unsigned i;
  for (i = 0; i < w; i++) {
    memcpy(&x[i*ldx], &b[i*ldb], h*sizeof(float));
  }

  const float factor = 1.0f;
  strsm_(&side, &uplo, &trans, &diag, (int*)&w, (int*)&h, &factor, a, (int*)&lda, x, (int*)&ldx);

  free(arg);
}

static struct starpu_codelet strsm_codelet =
{
  .modes = { STARPU_R, STARPU_R, STARPU_W },
  .where = STARPU_CUDA | STARPU_CPU,
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


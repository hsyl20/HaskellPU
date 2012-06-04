#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../Task.h"
#include "FloatMatrix_kernels.h"

static struct starpu_perfmodel sub_matrix_model =
{
  .type = STARPU_HISTORY_BASED,
  .symbol = "FLOATMATRIX_SUBMATRIX"
};

struct Point {
  unsigned x,y;
};


static void sub_matrix_cuda(void *descr[], void *args) {
  float *a = (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *b = (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  unsigned lda = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned ldb = STARPU_MATRIX_GET_LD(descr[1]);
  unsigned w = STARPU_MATRIX_GET_NX(descr[1]);
  unsigned h = STARPU_MATRIX_GET_NY(descr[1]);

  struct Point * p = (struct Point *)args;

  cuda_floatmatrix_submatrix(p->x, p->y, w, h, a, lda, b, ldb);
  free(p);
  cudaStreamSynchronize(starpu_cuda_get_local_stream());
}

static void sub_matrix_cpu(void *descr[], void *args) {
  float *src 	= (float *)STARPU_MATRIX_GET_PTR(descr[0]);
  float *dst 	= (float *)STARPU_MATRIX_GET_PTR(descr[1]);
  unsigned l0 = STARPU_MATRIX_GET_LD(descr[0]);
  unsigned w1 = STARPU_MATRIX_GET_NX(descr[1]);
  unsigned h1 = STARPU_MATRIX_GET_NY(descr[1]);
  unsigned l1 = STARPU_MATRIX_GET_LD(descr[1]);

  struct Point * p = (struct Point *)args;

  unsigned i,j;

  for (i = 0; i < h1; i++) {
    for (j = 0; j < w1; j++) {
      dst[i*l1 + j] = src[(p->y+i)*l0 + p->x + j];
    }
  }

  free(p);
}

static struct starpu_codelet sub_matrix_codelet =
{
  .modes = { STARPU_R, STARPU_W },
  .where = STARPU_CPU | STARPU_CUDA,
  .cpu_funcs = {sub_matrix_cpu, NULL},
  .cuda_funcs = {sub_matrix_cuda, NULL},
  .nbuffers = 2,
  .model = &sub_matrix_model
};

struct starpu_task * floatmatrix_submatrix_task_create(unsigned x, unsigned y, starpu_data_handle_t s, starpu_data_handle_t d) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &sub_matrix_codelet;
  task->handles[0] = s;
  task->handles[1] = d;

  struct Point * p = malloc(sizeof(struct Point));
  p->x = x;
  p->y = y;
  task->cl_arg = p;
  task->cl_arg_size = sizeof(struct Point);

  return task;
}

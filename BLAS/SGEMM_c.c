#include <starpu.h>
#include <starpu_cuda.h>
#include <cublas.h>

#include "../StarPU/Task.h"

static double sgemm_cpu_cost(struct starpu_task *task, enum starpu_perf_archtype arch, unsigned nimpl)
{
	uint32_t n = starpu_matrix_get_nx(task->handles[0]);
	double cost = (((double)(n)*n*n)/50.0f/10.75/8.0760);
	return cost;
}

static double sgemm_cuda_cost(struct starpu_task *task, enum starpu_perf_archtype arch, unsigned nimpl)
{
	uint32_t n = starpu_matrix_get_nx(task->handles[0]);
	double cost = (((double)(n)*n*n)/50.0f/10.75/76.30666);
	return cost;
}

static struct starpu_perfmodel sgemm_model =
{
	.per_arch =
	{
		[STARPU_CPU_DEFAULT][0] = { .cost_function = sgemm_cpu_cost },
		[STARPU_CUDA_DEFAULT][0] = { .cost_function = sgemm_cuda_cost }
	},
	.type = STARPU_HISTORY_BASED,
	.symbol = "SGEMM"
};

static void sgemm_cuda(void *descr[], void *_args) {
  float *left 	= (float *)STARPU_MATRIX_GET_PTR(descr[0]);
	float *right 	= (float *)STARPU_MATRIX_GET_PTR(descr[1]);
	float *center 	= (float *)STARPU_MATRIX_GET_PTR(descr[2]);

	unsigned dx = STARPU_MATRIX_GET_NY(descr[2]);
	unsigned dy = STARPU_MATRIX_GET_NX(descr[2]);
	unsigned dz = STARPU_MATRIX_GET_NY(descr[0]);

	unsigned ld21 = STARPU_MATRIX_GET_LD(descr[0]);
	unsigned ld12 = STARPU_MATRIX_GET_LD(descr[1]);
	unsigned ld22 = STARPU_MATRIX_GET_LD(descr[2]);

	cublasSgemm('n', 't', dy, dx, dz, 1.0f, left, ld21, right, ld12, 0.0f, center, ld22);
	cudaStreamSynchronize(starpu_cuda_get_local_stream());
}

static void sgemm_cpu(void *descr[], void *_args) {
}

static struct starpu_codelet sgemm_codelet =
{
  .modes = { STARPU_R, STARPU_R, STARPU_W },
  .where = STARPU_CUDA,
  .cpu_funcs = {sgemm_cpu, NULL},
  .cuda_funcs = {sgemm_cuda, NULL},
  .nbuffers = 3,
  .model = &sgemm_model
};

struct starpu_task * sgemm_task_create(starpu_data_handle_t a, starpu_data_handle_t b, starpu_data_handle_t c) {
  struct starpu_task * task = starpu_task_create_ex();
  task->cl = &sgemm_codelet;
  task->handles[0] = a;
  task->handles[1] = b;
  task->handles[2] = c;

  return task;
}

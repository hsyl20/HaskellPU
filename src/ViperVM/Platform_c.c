#include <starpu.h>
#include <cublas_v2.h>

extern struct starpu_sched_policy * _starpu_get_sched_policy(void);

static const char * unknown = "Unknown";
static const char * empty_desc = "no description available";

const char * starpu_sched_policy_name(void) {
  struct starpu_sched_policy * p = _starpu_get_sched_policy();
  return (p->policy_name != NULL ? p->policy_name : unknown);
}

const char * starpu_sched_policy_description(void) {
  struct starpu_sched_policy * p = _starpu_get_sched_policy();
  return (p->policy_description != NULL ? p->policy_description : empty_desc);
}

cublasHandle_t cublas_handle = NULL;

void starpu_cublas_init_v2(void) {
  //TODO: check return error
  cublasCreate(&cublas_handle);
}

void force_compute(starpu_data_handle_t * handle) {
  //nothing
}

#include <starpu.h>

extern struct starpu_sched_policy * _starpu_get_sched_policy(void);

const char * starpu_sched_policy_name(void) {
  struct starpu_sched_policy * p = _starpu_get_sched_policy();
  return p->policy_name;
}

#include <starpu.h>

starpu_tag_t starpu_task_tag_get(struct starpu_task *task) {
  return task->tag_id;
}

struct starpu_task* starpu_task_create_ex(void) {
  static starpu_tag_t tag = 1;
  struct starpu_task *task = starpu_task_create();
  task->tag_id = tag++;
  task->detach = 0;
  task->destroy = 0;
  return task;
}

void starpu_task_depends_on(struct starpu_task *task, starpu_tag_t tag) {
  if (tag == 0)
    return;
  starpu_tag_t t = starpu_task_tag_get(task);
  starpu_tag_declare_deps_array(t,1, &tag);
}

void * starpu_malloc_ex(size_t dim) {
  void * res;
  starpu_malloc(&res, dim);
  return res;
}

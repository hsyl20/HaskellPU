#include <starpu.h>
#include "Event.h"

static void task_callback(void * e) {
  Event event = (Event)e;
  starpu_event_trigger(event);
}


Event starpu_task_event(struct starpu_task *task) {
  return (Event)task->callback_arg;
}

static void haskellpu_task_init(struct starpu_task* task) {
  task->detach = 0;
  task->destroy = 0;

  Event e = starpu_event_create();

  task->callback_func = &task_callback;
  task->callback_arg = e;

  task->use_tag = 1;
  task->tag_id = starpu_event_tag(e);
}

struct starpu_task* starpu_task_create_ex(void) {
  struct starpu_task *task = starpu_task_create();

  haskellpu_task_init(task);

  return task;
}

/*struct starpu_task * starpu_data_duplicate_ex(starpu_data_handle_t src, starpu_data_handle_t dst) {

  struct starpu_task * task = starpu_data_duplicate(src,dst,0);
  
  haskellpu_task_init(task);

  return task;
}*/

void starpu_task_depends_on(struct starpu_task *task, Event e) {
  starpu_tag_t tag = starpu_event_tag(e);

  if (tag == 0)
    return;

  starpu_tag_t tags[] = {tag};
  starpu_tag_declare_deps_array(task->tag_id,1, tags);
}


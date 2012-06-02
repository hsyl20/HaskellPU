#include <starpu.h>

#ifndef STARPU_EVENT
#define STARPU_EVENT

typedef struct Event* Event;

Event starpu_event_create();
void starpu_event_destroy(Event e);
void starpu_event_trigger(Event e);
void starpu_event_wait(Event e);
starpu_tag_t starpu_event_tag(Event e);

#endif

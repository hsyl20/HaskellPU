#include <starpu.h>
#include <pthread.h>
#include <stdlib.h>

#define INITIAL_STATE 0
#define TRIGGERED_STATE 1

typedef struct Event {
  volatile char state;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  starpu_tag_t tag;
} * Event;

Event starpu_event_create() {
  static starpu_tag_t current_tag = 1;

  Event e = malloc(sizeof(struct Event));

  pthread_mutex_init(&e->mutex, NULL);
  pthread_cond_init(&e->cond, NULL);
  e->state = INITIAL_STATE;
  e->tag = current_tag++;
  return e;
}

void starpu_event_destroy(Event e) {
  pthread_mutex_destroy(&e->mutex);
  pthread_cond_destroy(&e->cond);
  free(e);
}


void starpu_event_trigger(Event e) {
  pthread_mutex_lock(&e->mutex);

  e->state = TRIGGERED_STATE;

  pthread_cond_broadcast(&e->cond);
  pthread_mutex_unlock(&e->mutex);
}

void starpu_event_wait(Event e) {
  pthread_mutex_lock(&e->mutex);

  while (e->state != TRIGGERED_STATE) {
    pthread_cond_wait(&e->cond, &e->mutex);
  }

  pthread_mutex_unlock(&e->mutex);
}

starpu_tag_t starpu_event_tag(Event e) {
  return e->tag;
}

static struct Event dummyEventI =  {
  .state = TRIGGERED_STATE,
  .mutex = PTHREAD_MUTEX_INITIALIZER,
  .cond = PTHREAD_COND_INITIALIZER,
  .tag = 0
};

Event starpu_event_dummy() {
  return &dummyEventI;
}

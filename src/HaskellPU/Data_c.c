#include <starpu.h>
#include <stdlib.h>

void * starpu_malloc_ex(size_t dim) {
  void * res;
  starpu_malloc(&res, dim);
  if (res == NULL)
    fprintf(stderr, "OUPS MALLOC!!\n");
  return res;
}

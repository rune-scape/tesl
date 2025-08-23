#include "errno.h"
#include "stddef.h"

int getentropy(void * buffer, size_t length) {
  buffer = buffer; length = length;
  errno = ENOSYS;
  return -1;
}

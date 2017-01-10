#include <smallOS/shutdown.h>

extern int kernel(start_info_t *si);

void kernel_init(start_info_t *si) { shutdown(kernel(si)); }
void kernel_loop(void) { return; }



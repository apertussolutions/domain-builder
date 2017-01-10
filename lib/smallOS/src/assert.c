//  BANNERSTART
//  Copyright: 2011, Galois, Inc.
//  License-file: LICENSE
//  Author: Iavor S. Diatchki (diatchki@galois.com)
//  BANNEREND

#include <smallOS/assert.h>
#include <smallOS/printk.h>
#include <smallOS/shutdown.h>

void assert_full ( int condition, const char * file
		 , const char * func, int line) {
  if (condition) return;
  printk("Assertion failed in file %s, function %s, at line %d\n"
	 , file, func, line);
  shutdown(SHUTDOWN_crash);
}

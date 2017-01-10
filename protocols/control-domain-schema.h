// BANNERSTART
// Copyright: 2011 Galois Inc.
// License-file: LICENSE
// Author: Iavor S. Diatchki (diatchki@galois.com)
// BANNEREND

#include "prelude.h"

typedef struct { char *key; char *value; } control_domain_schema_entry;

typedef GENERATE(cd_schema_encode,cd_schema_decode)
struct {
  size_t entry_num;
  control_domain_schema_entry entries[entry_num];
} control_domain_schema;



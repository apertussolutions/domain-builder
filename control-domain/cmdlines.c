#include <string.h>
#include <stdio.h>
#include <stddef.h>
#include <errno.h>

#include <smallOS/mem.h>
#include <smallOS/printk.h>

#include "cmdlines.h"

#define SVP_MODE_ARG "SVP"
#define CTRL_DOM_ARG "self"
#define DB_SERVER_ARG "db-server"
#define HS_BOOT_ARG "hs"
#define DRIVERS_ARG "drivers"
#define XENSTORE_ARG "xenstore"
#define VTPM_MANAGER_ARG "tpm"
#define INTROS_ARG "intros"
#define SCHEMA_ARG "schema"

int substitute_cmdline(char * str, domid_t ctrl_dom) {
  const char * token = "$(control)";
  char * target = strstr(str, token); 
  char dom_str[6] = "";

  if (target != NULL) {
    char * rest = target + strlen(token);
    int len     = snprintf(dom_str, sizeof(dom_str), "%hu", (short unsigned) ctrl_dom);
    strcpy(target, dom_str);
    target+=len;

    while  ( ( *target++ = *rest++) != '\0' );
  }
  return 0;
}


static
ssize_t parse_intros(char * str, cmdline_opts_t *opts) {

  long long domid;
  long long port;
  size_t offset = 0;
  int res;
  int consumed;

  while (str[offset] != ' ' || str[offset] != '\0') {

    res = sscanf(&str[offset],
             "%lli,%lli;%n",
             &domid, &port, &consumed);

    if (res != 2) {
      return offset;
    }

    printk("[CTL] Intro parsed for %lli\n", (long long)domid);

    offset += consumed;

    if (opts->intro_num == MAX_INTROS) return -E2BIG;

    opts->intros[opts->intro_num].domid = domid;
    opts->intros[opts->intro_num].mfn   = 0;
    opts->intros[opts->intro_num].port  = port;
    opts->intro_num++;
  }

  return offset;
}

static
int dom_dm_parse(char * args, domid_t *domid, char **dm_file) {
  int used = 0;
  int domid_arg;
  char * cursor = args;

  sscanf(cursor, "%d%n", &domid_arg, &used);
  cursor += used;

  if (*cursor == ',') {
    cursor++;
    *dm_file = cursor;

    while (*cursor != '\0') {
      if (*cursor == ' ') {
        *cursor++ = '\0';
        break;
      }
      cursor++;
    }
  }

  *domid = domid_arg; // separate assignment to support casting

  return cursor - args;
}

#define STRINGIFY(x) #x
#define TO_STR(x) STRINGIFY(x)
int load_cmdline(char * args, cmdline_opts_t *opts) {
  int used;

  if (strncmp(SVP_MODE_ARG, args, strlen(SVP_MODE_ARG)) == 0) {
    opts->svp_mode = 1;
    args += strlen(SVP_MODE_ARG);
  }

  while (*args != '\0') {
    used = 0;
    char * keyname = NULL;

    if (*args == ' ') {
      args++;
      continue;
    }

    keyname = args;
    args = strchr(args, '=');
    if (args == NULL) {
       printk("[CTL] Bad key-value pair on commandline, missing '='\n");
       return -1;
    }

    *args++ = '\0';

    if (0 == strcmp(CTRL_DOM_ARG, keyname)) {
      used = dom_dm_parse(args, &opts->ctrl_dom, &opts->ctrl_dm);
    } else if (0 == strcmp(VTPM_MANAGER_ARG, keyname)) {
      used = dom_dm_parse(args, &opts->vtpm_manager_dom, &opts->vtpm_manager_dm);
    } else if (0 == strcmp(DB_SERVER_ARG, keyname)) {
      used = dom_dm_parse(args, &opts->db_dom, &opts->db_dm);
    } else if (0 == strcmp(HS_BOOT_ARG, keyname)) {
      used = dom_dm_parse(args, &opts->host_store_domain, &opts->host_store_dm);
    } else if (0 == strcmp(DRIVERS_ARG, keyname)) {
      used = dom_dm_parse(args, &opts->drivers_dom, &opts->drivers_dm);
      printk("[CTL] Driver dom setup: %d\n", opts->drivers_dom);
    } else if (0 == strcmp(XENSTORE_ARG, keyname)) {
      used = dom_dm_parse(args, &opts->xenstore_domain, &opts->xenstore_dm);
    } else if (0 == strcmp(INTROS_ARG, keyname)) {
      used = parse_intros(args, opts);
      if (used < 0) return -1;
    } else if (0 == strcmp(SCHEMA_ARG, keyname)) {
      sscanf(args, "%" TO_STR(SCHEMA_FILENAME_SIZE) "s%n", opts->schema_filename, &used);
      printk("[CTL] Setting schema filename to %s\n", opts->schema_filename);
    }

    printk("[CTL] Used = %d\n", used);

    if (used == 0) {
      printk("[CTL] Bad command-line argument: %s\n",args);
      return -1;
    }

    args += used;
  }

  if (opts->db_dom == -1) {
    printk("[CTL] Domain builder domain not specified\n");
    return -1;
  }

  if (opts->ctrl_dom == -1) {
    printk("[CTL] Control domain (self) not specified\n");
    return -1;
  }

  return 0;
}



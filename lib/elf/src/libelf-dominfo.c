// BANNERSTART
// Copyright: Xen developers?
// Copyright: 2011 Galois Inc.
// License-file: LICENSE
// BANNEREND

/*
 * parse xen-specific informations out of elf kernel binaries.
 */

#include "libelf-private.h"

/* ------------------------------------------------------------------------ */
/* xen features                                                             */

const char * const elf_xen_feature_names[] = {
    [XENFEAT_writable_page_tables] = "writable_page_tables",
    [XENFEAT_writable_descriptor_tables] = "writable_descriptor_tables",
    [XENFEAT_auto_translated_physmap] = "auto_translated_physmap",
    [XENFEAT_supervisor_mode_kernel] = "supervisor_mode_kernel",
    [XENFEAT_pae_pgdir_above_4gb] = "pae_pgdir_above_4gb"
};
const int elf_xen_features =
sizeof(elf_xen_feature_names) / sizeof(elf_xen_feature_names[0]);

static
int elf_xen_parse_features(const char *features,
                           uint32_t *supported,
                           uint32_t *required)
{
    char feature[64];
    int pos, len, i;

    if ( features == NULL )
        return 0;

    for ( pos = 0; features[pos] != '\0'; pos += len ) {
        memset(feature, 0, sizeof(feature));
        for ( len = 0;; len++ )
        {
            if ( len >= sizeof(feature)-1 )
                break;
            if ( features[pos + len] == '\0' )
                break;
            if ( features[pos + len] == '|' )
            {
                len++;
                break;
            }
            feature[len] = features[pos + len];
        }

        for ( i = 0; i < elf_xen_features; i++ )
        {
            if ( !elf_xen_feature_names[i] )
                continue;
            if ( (required != NULL) && (feature[0] == '!') )
            {
                /* required */
                if ( !strcmp(feature + 1, elf_xen_feature_names[i]) )
                {
                    elf_xen_feature_set(i, supported);
                    elf_xen_feature_set(i, required);
                    break;
                }
            }
            else
            {
                /* supported */
                if ( !strcmp(feature, elf_xen_feature_names[i]) )
                {
                    elf_xen_feature_set(i, supported);
                    break;
                }
            }
        }
        if ( i == elf_xen_features )
            return -1;
    }

    return 0;
}

/* ------------------------------------------------------------------------ */
/* __xen_guest section                                                      */

static
int elf_xen_parse_guest_info(const struct elf_binary *elf,
                             struct elf_dom_parms *parms,
                             const char* h) {
    char name[32], value[128];
    int len;

    //h = parms->guest_info;
    while (*h) {
      memset(name, 0, sizeof(name));
      memset(value, 0, sizeof(value));
      for ( len = 0;; len++, h++ ) {
        if (len >= sizeof(name)-1)
          break;
        // Reached end of string.
        if ( *h == '\0' ) break;
        // Reached end of name
        if ( *h == ',' ) {
          h++;
          break;
        }
        // Parse value
        if ( *h == '=' ) {
            h++;
            for ( len = 0;; len++, h++ )
            {
                if ( len >= sizeof(value)-1 )
                    break;
                if ( *h == '\0' )
                    break;
                if ( *h == ',' )
                {
                    h++;
                    break;
                }
                value[len] = *h;
            }
            break;
        }
        name[len] = *h;
      }
      elf_msg(elf, "%s: %s=\"%s\"\n", __FUNCTION__, name, value);

      /* strings */
      if ( !strcmp(name, "LOADER") )
        safe_strcpy(parms->loader, value);
      //if ( !strcmp(name, "GUEST_OS") )
      //    safe_strcpy(parms->guest_os, value);
      if ( !strcmp(name, "GUEST_VER") )
          safe_strcpy(parms->guest_ver, value);
      if ( !strcmp(name, "XEN_VER") )
          safe_strcpy(parms->xen_ver, value);
      if ( !strcmp(name, "PAE") ) {
          if ( !strcmp(value, "yes[extended-cr3]") )
              parms->pae = 2 /* extended_cr3 */;
          else if ( !strncmp(value, "yes", 3) )
              parms->pae = 1 /* yes */;
      }
      if ( !strcmp(name, "BSD_SYMTAB") ) {
        elf_err(elf, "%s: ERROR: Not a Xen-ELF image: "
                "No ELF notes or '__xen_guest' section found.\n",
                __FUNCTION__);
        return -1;
      }

      /* longs */
      if ( !strcmp(name, "VIRT_BASE") )
          parms->virt_base = strtoull(value, NULL, 0);
      if ( !strcmp(name, "VIRT_ENTRY") )
          parms->virt_entry = strtoull(value, NULL, 0);
      if ( !strcmp(name, "ELF_PADDR_OFFSET") )
          parms->elf_paddr_offset = strtoull(value, NULL, 0);
      if ( !strcmp(name, "HYPERCALL_PAGE") )
          parms->virt_hypercall = (strtoull(value, NULL, 0) << 12)
                                + parms->virt_base;

      /* other */
      if ( !strcmp(name, "FEATURES") )
          if ( elf_xen_parse_features(value, parms->f_supported,
                                      parms->f_required) )
              return -1;
    }
    return 0;
}

#define elf_shdr_by_name(wx) ELF_POLY(elf_shdr_by_name, wx)
#define elf_note_name(wx) ELF_POLY(elf_note_name, wx)
#define elf_note_desc(wx) ELF_POLY(elf_note_desc, wx)
#define elf_note_numeric(wx) ELF_POLY(elf_note_numeric, wx)
#define elf_note_next(wx) ELF_POLY(elf_note_next, wx)
#define elf_xen_parse_note(wx) ELF_POLY(elf_xen_parse_note, wx)
#define elf_xen_parse_notes(wx) ELF_POLY(elf_xen_parse_notes, wx)
#define elf_xen_addr_calc(wx) ELF_POLY(elf_xen_addr_calc, wx)
#define elf_xen_parse(wx) ELF_POLY(elf_xen_parse, wx)
#define ElfNote(wx) ELF_TYPE(Elf, wx, Note)

#define WX 64
#include "libelf-dominfo.generic"
#undef WX

#define WX 32
#include "libelf-dominfo.generic"
#undef WX

/* ------------------------------------------------------------------------ */
/* glue it all together ...                                                 */

int elf_parse_dom_parms(const struct elf_binary* elf,
                       struct elf_dom_parms* parms) {
  return elf->elf_class == ELFCLASS64
       ? elf_xen_parse(64)(elf, parms)
       : elf_xen_parse(32)(elf, parms);
}

/* BANNERSTART
   Copyright: 2011, Galois, Inc.
   License-file: LICENSE
   Author: Iavor S. Diatchki (diatchki@galois.com)
   BANNEREND */

OUTPUT_FORMAT(elf64-x86-64)
/* OUTPUT_ARCH(i386:x86-64) */
ENTRY(_start)

PHDRS { load PT_LOAD; note PT_NOTE; }

SECTIONS
{
  /* Section "virtual" is used to describe a region of virtual memory,
     which is not backed by physical memory.  This region is useful
     for mapping memory from a foreign virtual machine into our address space.

      Currently, we start this region at 1G.
   */

  .                         = 1024M;
  _virtual_start            = .;
  virtual_noreset (NOLOAD)  : { *(virtual_noreset) }
  .                         = ALIGN(4K);
  _virtual_reset_start      = .;
  virtual_reset (NOLOAD)    : { *(virtual_reset) }
  .                         = ALIGN(2M);  /* Amount fitting in L1 table */
  _virtual_end              = .;


  /* Virtual space used by the self-reference in the L4 table.
     This create an array of size 512G which contains all page tables,
     which is useful for converting virtual to physical addresses.
     See "mem.c" in smallOS for details.
   */
  _L4_LOOP_ENTRY              = 1;
  page_table                  = _L4_LOOP_ENTRY * 512 * 1024M;

  .                           = 0x1000;
  _kernel_start               = .;

  /* code:   executable, data: non-executable
     static: read-only
     reset:  read-write, but should be reset across exections of loop.
   */

  static_code  ALIGN(4K)      : { *(.text) }                  :load
  static_data  ALIGN(4K)      : { *(.rodata) *(.rodata.*) }
  reset_data   ALIGN(4K)      : { *(.data) *(.ldata) }
  noreset_data ALIGN(4K)      : { *(noreset) }



  /* -- Page tables ------------------------------------------------------- */
  /* This section contains additional page tables that we might need.
     All strucutres should be page-aligned.
     Structures are unmapped after initialization.
   */
  page_tables ALIGN(4K)       : { *(page_tables) }

  /* Space for the L1 tables needed to map the virtual region above. */
  virtual_L1_tables           = .;
  _virtual_L1_table_num       = ((_virtual_end - _virtual_start) / 2M);
  .                          += 4K * _virtual_L1_table_num;

  /* -- Zero region ------------------------------------------------------- */
  _zero_start                 = .;
  zero_reset_pages ALIGN(4K)  : { *(bss_page) }
  zero_reset                  : { *(.bss) *(.lbss) *(COMMON) *(LARGE_COMMON) }
  .                           = ALIGN(4K);
  _zero_end                   = .;
  _kernel_end                 = .;


  /* -- Notes ------------------------------------------------------- */
  __xen_guest               : {  *(__xen_guest) }           :note





  /* Sections to be discarded */
  /DISCARD/ : { *(.text.exit) *(.data.exit) *(.exitcall.exit) *(.eh_frame) }

  /* Stabs debugging sections.  */
  .stab 0           : { *(.stab) }
  .stabstr 0        : { *(.stabstr) }
  .stab.excl 0      : { *(.stab.excl) }
  .stab.exclstr 0   : { *(.stab.exclstr) }
  .stab.index 0     : { *(.stab.index) }
  .stab.indexstr 0  : { *(.stab.indexstr) }
  .comment 0        : { *(.comment) }
}


ASSERT(_kernel_end < _virtual_start, "Kernel overflowed into virtual area.");
ASSERT(_virtual_L1_table_num <= 512, "Too many virtual structures.");


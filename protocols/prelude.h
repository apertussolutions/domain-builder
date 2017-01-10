#ifndef PRELUDE
#define PRELUDE

#define uint64_t      unsigned long
#define uint32_t      unsigned int
#define uint8_t       unsigned char
#define size_t        unsigned int
#define evtchn_port_t unsigned int
#define grant_ref_t   unsigned int

#define TAGPREFIX(x) __attribute__((tagprefix(x)))
#define GENERATE(x,y) __attribute__((codec(x,y)))

#endif


// BANNERSTART
// Copyright: 2003 - Rolf Neugebauer - Intel Research Cambridge
// License-file: miniOS-FreeBSD-LICENSE
// Author: Rolf Neugebauer (neugebar@dcs.gla.ac.uk)
// Changes: Grzegorz Milos (gm281@cam.ac.uk) 
// BANNEREND


#include <ctype.h>

/**
 * simple_strtoul - convert a string to an unsigned long
 * @cp: The start of the string
 * @endp: A pointer to the end of the parsed string will be placed here
 * @base: The number base to use
 */
unsigned long simple_strtoul(const char *cp,char **endp,unsigned int base)
{
    unsigned long result = 0,value;

    if (!base) {
        base = 10;
        if (*cp == '0') {
            base = 8;
            cp++;
            if ((*cp == 'x') && isxdigit(cp[1])) {
                cp++;
                base = 16;
            }
        }
    }
  
#ifdef CBMC
    value = nondet_value();
    SMALLLOOP {
#else
    while (isxdigit(*cp) &&
            (value = isdigit(*cp) ? *cp-'0' : toupper(*cp)-'A'+10)
	   < base) {
#endif
        result = result*base + value;
        cp++;
    }
    if (endp)
        *endp = (char *)cp;
    return result;
}

/**
 * simple_strtol - convert a string to a signed long
 * @cp: The start of the string
 * @endp: A pointer to the end of the parsed string will be placed here
 * @base: The number base to use
 */
long simple_strtol(const char *cp,char **endp,unsigned int base)
{
    if(*cp=='-')
        return -simple_strtoul(cp+1,endp,base);
    return simple_strtoul(cp,endp,base);
}

/**
 * simple_strtoull - convert a string to an unsigned long long
 * @cp: The start of the string
 * @endp: A pointer to the end of the parsed string will be placed here
 * @base: The number base to use
 */
unsigned long long simple_strtoull(const char *cp,char **endp,unsigned int base)
{
    unsigned long long result = 0,value;

    if (!base) {
        base = 10;
        if (*cp == '0') {
            base = 8;
            cp++;
            if ((*cp == 'x') && isxdigit(cp[1])) {
                cp++;
                base = 16;
            }
        }
    }

#ifdef CBMC
    value = nondet_value2();
    for(int i = 0; i < 2; i++) {
#else
    while (isxdigit(*cp) && 
	   (value = isdigit(*cp) ? *cp-'0' 
	    : (islower(*cp)
	       ? toupper(*cp) : *cp)-'A'+10) < base) {
#endif
        result = result*base + value;
        cp++;
    }
    if (endp)
        *endp = (char *)cp;
    return result;
}

/**
 * simple_strtoll - convert a string to a signed long long
 * @cp: The start of the string
 * @endp: A pointer to the end of the parsed string will be placed here
 * @base: The number base to use
 */
long long simple_strtoll(const char *cp,char **endp,unsigned int base)
{
    if(*cp=='-')
        return -simple_strtoull(cp+1,endp,base);
    return simple_strtoull(cp,endp,base);
}

int atoi(const char *str)
{
    return (int)simple_strtol(str, (char**)0, 10);
}

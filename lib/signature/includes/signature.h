#ifndef SIGNATURE_H
#define SIGNATURE_H

int check_signature(const uint8_t * hash, size_t hash_len,
                    const uint8_t * signature, size_t signature_len,
                    const char * public);

#endif

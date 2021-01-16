#ifndef ELFGEN_STRINGS_H
#define ELFGEN_STRINGS_H

#include <stddef.h>

// Original code borrowed from https://stackoverflow.com/a/52989329/6718698
const char *memmem(const char *haystack, size_t haystack_len,
                   const char *const needle, const size_t needle_len);

int mempos(const char *haystack, size_t haystack_len, const char *needle, const size_t needle_len);

#endif

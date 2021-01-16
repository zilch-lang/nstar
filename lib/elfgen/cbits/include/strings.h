#ifndef ELFGEN_STRINGS_H
#define ELFGEN_STRINGS_H

#include <stddef.h>
#include <string.h>

#if defined(_WIN32)

// Original code borrowed from https://stackoverflow.com/a/52989329/6718698
const char *memmem(const char *haystack, size_t haystack_len,
                   const char *const needle, const size_t needle_len)
{
    if (haystack == NULL) return NULL;
    if (haystack_len == 0) return NULL;
    if (needle == NULL) return NULL;
    if (needle_len == 0) return NULL;

    for (const char *h = haystack; haystack_len >= needle_len; ++h, --haystack_len)
    {
        if (memcmp(h, needle, needle_len) == 0)
        {
            return h;
        }
    }

    return NULL;
}

#endif

#endif

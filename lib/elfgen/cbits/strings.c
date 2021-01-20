#include "strings.h"

#ifdef _GNU_SOURCE
#    undef _GNU_SOURCE  // we do not want to use `memmem` in `string.h`
#endif

#include <string.h>

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

int mempos(const char *haystack, size_t haystack_len, const char *needle, const size_t needle_len)
{
    const char *p = memmem(haystack, haystack_len, needle, needle_len);

    if (p != NULL) return p - haystack;
    return -1;
}

#ifndef ELFGEN_STRINGS_H
#define ELFGEN_STRINGS_H

#include <stddef.h>

// Original code borrowed from https://stackoverflow.com/a/52989329/6718698
/**
 * @brief Searches for a needle in a haystack.
 *
 * @note Unlike `strstr`, this function does not stop once it encounters a null character.
 *
 * @param haystack The haystack to search in
 *
 * @param haystack_len The length of the haystack
 *
 * @param needle The needle to find
 *
 * @param needle_len The length of the needle
 *
 * @return `NULL` if the needle has not been found in the haystack,
 *         else a pointer to the beginning of the needle in the haystack
 * */
const char *memmem(const char *haystack, size_t haystack_len,
                   const char *const needle, const size_t needle_len);

/**
 * @brief finds the position of a needle in a haystack.
 *
 * @param haystack The haystack to search in
 *
 * @param haystack_len The length of the haystack
 *
 * @param needle The needle to find
 *
 * @param needle_len The length of the needle
 *
 * @return `-1` if the needle is not in the haystack,
 *         else the position where it was encountered in the haystack
 * */
int mempos(const char *haystack, size_t haystack_len, const char *needle, const size_t needle_len);

#endif

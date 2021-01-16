#ifndef ELFGEN_SECTION_HEADER_H
#define ELFGEN_SECTION_HEADER_H

#include <stdint.h>
#include <elf.h>

#include "symbol.h"

typedef struct
{
    enum section_type
    {
        S_NULL = SHT_NULL,
        S_PROGBITS = SHT_PROGBITS,
        S_RELA = SHT_RELA,
        S_NOBITS = SHT_NOBITS,
        S_SYMTAB = SHT_SYMTAB,
        S_STRTAB = SHT_STRTAB
    } type;

    union
    {
        struct
        {} s_null;

        struct
        {
            char const *section_name;
            uint8_t *binary_data;
            uint64_t flags;

            unsigned long binary_data_len;
        } s_progbits;

        struct {
            char const *section_name;
            elf_relocation_symbol **symbols;

            unsigned long symbols_len;
        } s_rela;

        struct {
            char const *section_name;
            uint32_t space;
            uint64_t flags;
        } s_nobits;

        struct {
            char const *section_name;
            elf_symbol **symbols;

            unsigned long symbols_len;
        } s_symtab;

        struct {
            char const *section_name;
            char const *strings;

            unsigned long strings_len;
        } s_strtab;
    } data;
} elf_section_header;

#endif

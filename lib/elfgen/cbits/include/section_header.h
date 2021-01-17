#ifndef ELFGEN_SECTION_HEADER_H
#define ELFGEN_SECTION_HEADER_H

#include <stdint.h>
#include <elf.h>

#include "symbol.h"

/**
 * @brief An abstract ELF section header.
 * */
typedef struct
{
    enum section_type
    {
        S_NULL = SHT_NULL,          //!< The null section
        S_PROGBITS = SHT_PROGBITS,  //!< Some program bits
        S_RELA = SHT_RELA,          //!< A relocation table
        S_NOBITS = SHT_NOBITS,      //!< Runtime allocated section
        S_SYMTAB = SHT_SYMTAB,      //!< A symbol table
        S_STRTAB = SHT_STRTAB       //!< A string table
    } type;   //!< The type of section stored

    union
    {
        struct
        {} s_null;

        struct
        {
            char const *section_name;        //!< The name of the section
            uint8_t *binary_data;            //!< The binary data that it contains
            uint64_t flags;                  //! Some flags

            unsigned long binary_data_len;   //! The length of the binary data it holds
        } s_progbits;

        struct {
            char const *section_name;         //!< The name of the section
            elf_relocation_symbol **symbols;  //!< The relocation symbol entries for the table

            unsigned long symbols_len;        //!< The number of relocation symbols
        } s_rela;

        struct {
            char const *section_name;     //!< The name of the section
            uint32_t space;               //!< How many bytes to allocate at run-time
            uint64_t flags;               //!< Section specific flags
        } s_nobits;

        struct {
            char const *section_name;   //!< The name of the section
            elf_symbol **symbols;       //!< The symbol entries in the symbol table

            unsigned long symbols_len;  //!< The number of symbols in the table
        } s_symtab;

        struct {
            char const *section_name;    //!< The name of the section
            char const *strings;         //!< All the strings in the table, separated by null characters

            unsigned long strings_len;   //!< The length of the string table
        } s_strtab;
    } data;   //!< Per-section data stored
} elf_section_header;

#endif

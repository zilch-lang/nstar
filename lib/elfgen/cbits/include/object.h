#ifndef ELFGEN_OBJECT_H
#define ELFGEN_OBJECT_H

#include "file_header.h"
#include "segment_header.h"
#include "section_header.h"

/**
 * @brief An abstract ELF object that needs to be compiled down to a concrete ELF object.
 * */
typedef struct
{
    elf_file_header const *file_header;    //!< The object header
    elf_segment_header const **segments;   //!< All the segments in the object
    elf_section_header const **sections;   //!< All the sections in the object

    unsigned long const segments_len;      //!< The number of segments in the object
    unsigned long const sections_len;      //!< The number of sections in the object
} elf_object;

/**
 * @brief A concrete ELF object that can be output to a file.
 * */
typedef struct
{
    Elf64_Ehdr *file_header;          //!< The object header
    Elf64_Phdr **segment_headers;     //!< The segment headers
    Elf64_Shdr **section_headers;     //!< The section headers
    Elf64_Rela **relocations;         //!< All the relocation tables merged
    Elf64_Sym **symbols;              //!< The symbol table
    unsigned char *binary_data;       //!< All the binary data to put in the object file

    unsigned long segments_len;       //!< The number of segment headers
    unsigned long sections_len;       //!< The number of section headers
    unsigned long symbols_len;        //!< The number of symbols
    unsigned long relocations_len;    //!< The number of relocation symbols
    unsigned long binary_data_len;    //!< The length of the generated binary data
} Elf64_Object;

/**
 * @brief Frees the memory allocated for a concrete object, during compilation.
 *
 * @details This function is meant to be used after the object has been dumped to a variable (in binary form)
 *          or to a binary object file.
 *
 * @param obj The object to free the memory from
 * */
void free_elf64_object(Elf64_Object *obj);

#endif

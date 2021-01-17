#ifndef ELFGEN_OBJECT_H
#define ELFGEN_OBJECT_H

#include "file_header.h"
#include "segment_header.h"
#include "section_header.h"

typedef struct
{
    elf_file_header const *file_header;
    elf_segment_header const **segments;
    elf_section_header const **sections;

    unsigned long const segments_len;
    unsigned long const sections_len;
} elf_object;

typedef struct
{
    Elf64_Ehdr *file_header;
    Elf64_Phdr **segment_headers;
    Elf64_Shdr **section_headers;
    Elf64_Rela **relocations;
    Elf64_Sym **symbols;
    unsigned char *binary_data;

    unsigned long segments_len;
    unsigned long sections_len;
    unsigned long symbols_len;
    unsigned long relocations_len;
    unsigned long binary_data_len;
} Elf64_Object;

void free_elf64_object(Elf64_Object *obj);

#endif

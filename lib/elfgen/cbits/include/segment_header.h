#ifndef ELFGEN_SEGMENT_HEADER_H
#define ELFGEN_SEGMENT_HEADER_H

#include <stdint.h>
#include <elf.h>

typedef struct
{
    enum program_header_type
    {
        P_PHDR = PT_PHDR,
        P_NULL = PT_NULL,
        P_LOAD = PT_LOAD,
        P_INTERP = PT_INTERP
    } type;

    union
    {
        struct
        {} p_phdr;

        struct
        {} p_null;

        struct
        {
            void *data;
            enum p_load_data_type { P_LOAD_SECTION, P_LOAD_DATA } data_type;
            uint32_t data_len;
            uint32_t flags;
        } p_load;

        struct
        {
            char const *interpreter_path;
            uint32_t flags;
        } p_interp;
    } data;
} elf_segment_header;

#endif

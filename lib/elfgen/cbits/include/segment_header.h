#ifndef ELFGEN_SEGMENT_HEADER_H
#define ELFGEN_SEGMENT_HEADER_H

#include <stdint.h>
#ifndef _WIN32
#  include <elf.h>
#else 
#  include "elf.h"
#endif 

/**
 * @brief An abstract ELF segment header.
 * */
typedef struct
{
    enum program_header_type
    {
        P_PHDR = PT_PHDR,     //!< The segment header table header
        P_NULL = PT_NULL,     //!< A null entry
        P_LOAD = PT_LOAD,     //!< Some runtime loadable data
        P_INTERP = PT_INTERP  //!< The file dyanmic loader/interpreter
    } type;   //!< The type of segment.

    union
    {
        struct
        {} p_phdr;

        struct
        {} p_null;

        struct
        {
            void *data;                                                        /**<
                                                                                * @brief Data held by the segment.
                                                                                *
                                                                                * @details It can either be a section name, meaning data is fetched from the section,
                                                                                *          or some binary data that must be inserted in the object.
                                                                                * */
            enum p_load_data_type { P_LOAD_SECTION, P_LOAD_DATA } data_type;   //!< The type of data held in the segment
            uint32_t data_len;                                                 //!< The length (if some binary data is held) of the data held
            uint32_t flags;                                                    //!< Segment specific flags
        } p_load;

        struct
        {
            char const *interpreter_path;    //!< The path to the dynamic loader/interpreter (most likely `/lib/ld-linux-x86-64.so.2`)
            uint32_t flags;                  //!< Segment specific flags
        } p_interp;
    } data;    //!< Data the segment represents.
} elf_segment_header;

#endif

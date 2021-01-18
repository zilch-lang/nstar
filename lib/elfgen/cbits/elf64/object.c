#include "object.h"

#include <stdlib.h>

void free_elf64_object(Elf64_Object *obj)
{
    free(obj->file_header);

    for (unsigned int i = 0; i < obj->segments_len; ++i)
    {
        free(obj->segment_headers[i]);
    }
    free(obj->segment_headers);

    for (unsigned int i = 0; i < obj->sections_len; ++i)
    {
        free(obj->section_headers[i]);
    }
    free(obj->section_headers);

    for (unsigned int i = 0; i < obj->symbols_len; ++i)
    {
        free(obj->symbols[i]);
    }
    free(obj->symbols);

    for (unsigned int i = 0; i < obj->relocations_len; ++i)
    {
        free(obj->relocations[i]);
    }
    free(obj->relocations);

    free(obj->binary_data);
}

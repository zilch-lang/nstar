#ifndef ELFGEN_ELF64_FIX_H
#define ELFGEN_ELF64_FIX_H

#include "object.h"

/**
 * @brief Runs a serie of fixes on the target ELF object to make it usable.
 *
 * @param obj The base abstract ELF object (it is needed to fetch some information in)
 *
 * @param target The ELF object to fix
 *  */
void fix_elf_object(elf_object const *obj, Elf64_Object *target);

#endif

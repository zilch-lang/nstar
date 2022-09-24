#ifndef ELFGEN_ELF64_COMPILE_H
#define ELFGEN_ELF64_COMPILE_H

#include "object.h"
#include "file_header.h"
#include "segment_header.h"
#include "section_header.h"
#include "symbol.h"

#ifndef _WIN32
#  include <elf.h>
#else 
#  include "elf.h"
#endif 

/**
 * @brief Compiles an abstract ELF file.
 *
 * The file will first roughly be compiled, then will undergo a serie of fixes to make it actually usable
 * (e.g. executable, relocatable, or something else).
 *
 * @warning This function expects a valid pointer to a Elf64_Object, but also allocates some memory for it
 *          (for the `segment_headers`, `section_headers`, `symbols` and `binary_data`).
 *          You will need to free those pointers yourself using `free_elf64_object` once you don't need the object anymore.
 *
 * @param obj The abstract ELF object
 *
 * @param dst The target ELF object to modify (must not be `NULL`)
 *  */
extern void compile_x64(elf_object const *obj, Elf64_Object *dst);

/**
 * @brief Compiles an abstract ELF file header.
 *
 * @note The generated ELF header needs to undergo various fixup steps because of the dummy values
 *       put here (number of sections, their size, etc).
 *       There isn't enough information here to be able to fille some of those fields, namely:
 *       - `e_entry`
 *       - `e_phoff`
 *       - `e_shoff`
 *       - `e_phnum`
 *       - `e_shnum`
 *       - `e_shstrndx`
 *
 *  @param fheader The ELF file header to compile (un-abstract)
 *
 *  @param header The target file header to modify (must not be `NULL`)
 *  */
void compile_file_header(elf_file_header const *fheader, Elf64_Ehdr *header);

/**
 * @brief Compiles an abstract ELF segment header.
 *
 * @note We need to apply multiple fix steps to fill the given fields:
 *       - `p_offset`
 *       - `p_vaddr`
 *       - `p_paddr`
 *       - `p_filesz`
 *       - `p_memsz`
 *
 * @param seg The ELF segment header to compile
 *
 * @param header The target segment header to modify (must not be `NULL`)
 *  */
void compile_segment_header(elf_segment_header const *seg, Elf64_Phdr *header);

/**
 * @brief Compiles an abstract ELF section header.
 *
 * @note Some fields are not filled, because of the lack of information at that current point.
 *       These are listed here:
 *       - `sh_name`
 *       - `sh_addr`
 *       - `sh_offset`
 *       - `sh_size` (this one may not be filled if the size cannot be determined from only the content of the section)
 *       - `sh_link`
 *       - `sh_info`
 *       - `sh_addralign`
 *
 * @param sect The ELF section header to compile
 *
 * @param header The target section header to modify (must not be `NULL`)
 *  */
void compile_section_header(elf_section_header const *sect, Elf64_Shdr *header);

/**
 * @brief Compiles an abstract ELF symbol.
 *
 * @note Some fields are no filled due to a lack of information:
 *       - `st_name`
 *       - `st_shndx`
 *       - `st_value`
 *       - `st_size`
 *
 * @param sym The ELF symbol to compile
 *
 * @param target The target symbol to modify (must not be `NULL`)
 * */
void compile_symbol(elf_symbol const *sym, Elf64_Sym *target);

/**
 * @brief Compiles an abstract ELF relocation symbol.
 *
 * @note Some fields must be fixed afterwards:
 *       - `r_info` (missing symbol)
 *       - `r_addend`
 *
 * @param sym The ELF relocation symbol to compile
 *
 * @param target The target relocation symbol to modif (must not be `NULL`)
 * */
void compile_relocation_symbol(elf_relocation_symbol const *sym, Elf64_Rela *target);

#endif

#ifndef ELFGEN_ELF64_INTERNAL_FIX_H
#define ELFGEN_ELF64_INTERNAL_FIX_H

#include "section_header.h"

/**
 * @brief Fetches the name of a section according to its type.
 *
 * @param section The section to get the name of
 *
 * @return The name of the section
 * */
char const *get_section_name(elf_section_header const *section);

/**
 * @brief Finds the section index in the section header table using its name.
 *
 * @param sections The section header table to search in
 *
 * @param size The size of the section header table
 *
 * @param name The name of the sectino to find
 *
 * @return `-1` if the section has not be found, else the index in the section header table
 * */
int find_section_index_by_name(elf_section_header const **sections, unsigned int size, char const *name);

/**
 * @brief Finds the index of the symbol corresponding to a section.
 *
 * @param symtab The symbol table (`.symtab` section)
 *
 * @param symtab_len The length of the symbol table
 *
 * @param section_index The index of the section to find the symbol of
 *
 * @return `-1` if no symbol has been found for the section, else the index of the symbol in the symbol table
 * */
int find_section_symbol_by_index(Elf64_Sym **symtab, unsigned int symtab_len, int section_index);

#endif

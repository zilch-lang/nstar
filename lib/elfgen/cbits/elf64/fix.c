#include "elf64/fix.h"
#include "object.h"
#include "file_header.h"
#include "section_header.h"
#include "segment_header.h"
#include "symbol.h"
#include "elf64/internal_fix.h"
#include "strings.h"

#include <elf.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#define NB_RELOCATION_TABLES (1)
static char const * const all_relocation_tables[NB_RELOCATION_TABLES] =
    { ".rela.text" };

static Elf64_Off relocation_tables_end = 0;
static Elf64_Off data_end = 0;

void fix_header_count_and_offsets(elf_object const *obj, Elf64_Object *target);
void fix_header_shstrtab_index(elf_object const *obj, Elf64_Object *target);
void fix_symtab_strtab_index(elf_object const *obj, Elf64_Object *target);
void fix_section_names(elf_object const *obj, Elf64_Object *target);
void fix_section_offsets(elf_object const *obj, Elf64_Object *target);
void fix_symtab_offset_and_shinfo(elf_object const *obj, Elf64_Object *target);
void fix_symbol_names_and_sections(elf_object const *obj, Elf64_Object *target);
void fix_symbol_values(elf_object const *obj, Elf64_Object *target);
void fix_rel_sections_shinfo_and_shlink(elf_object const *obj, Elf64_Object *target);


/*
** NOTE: we really should memoize the section indices, because we basically need to query them in all the fix functions
*/


void fix_elf_object(elf_object const *obj, Elf64_Object *target)
{
    fix_header_count_and_offsets(obj, target);
    fix_header_shstrtab_index(obj, target);
    fix_symtab_strtab_index(obj, target);
    fix_section_names(obj, target);
    fix_section_offsets(obj, target);
    fix_symtab_offset_and_shinfo(obj, target);
    fix_symbol_names_and_sections(obj, target);
    fix_symbol_values(obj, target);
    fix_rel_sections_shinfo_and_shlink(obj, target);
}


void fix_header_count_and_offsets(elf_object const *obj, Elf64_Object *target)
{
    Elf64_Off offset;
    int rel_section_index;

    target->file_header->e_phnum = target->segments_len;
    target->file_header->e_shnum = target->sections_len;

    offset = 0x0 + target->file_header->e_ehsize;
    // we put the segment table right after the file header
    target->file_header->e_phoff = target->file_header->e_phnum == 0 ? 0x0 : offset;

    offset += target->file_header->e_phentsize * target->file_header->e_phnum;
    // we put the section table right after the segment table
    target->file_header->e_shoff = offset;

    offset += target->file_header->e_shentsize * target->file_header->e_shnum;
    // we put the relocation tables right after the section table, one after the other
    for (unsigned int i = 0; i < NB_RELOCATION_TABLES; ++i)
    {
        rel_section_index = find_section_index_by_name(obj->sections, obj->sections_len, all_relocation_tables[i]);
        if (rel_section_index != -1)
        {
            Elf64_Shdr *relocation_section = target->section_headers[i];

            relocation_section->sh_offset = offset;
            offset += relocation_section->sh_size;
        }
    }
    relocation_tables_end = offset;
    // register the end of all the relocation tables, because we will need it later
}

void fix_header_shstrtab_index(elf_object const *obj, Elf64_Object *target)
{
    Elf64_Half shstrtab_index = find_section_index_by_name(obj->sections, obj->sections_len, ".shstrtab");

    assert(shstrtab_index != -1);

    target->file_header->e_shstrndx = shstrtab_index;
}

void fix_symtab_strtab_index(elf_object const *obj, Elf64_Object *target)
{
    Elf64_Word strtab_index, symtab_index;

    strtab_index = find_section_index_by_name(obj->sections, obj->sections_len, ".strtab");
    symtab_index = find_section_index_by_name(obj->sections, obj->sections_len, ".symtab");

    assert(strtab_index != -1);
    assert(symtab_index != -1);

    target->section_headers[symtab_index]->sh_link = strtab_index;
}

void fix_section_names(elf_object const *obj, Elf64_Object *target)
{
    Elf64_Word strtab_index = find_section_index_by_name(obj->sections, obj->sections_len, ".shstrtab");
    elf_section_header const *strtab = obj->sections[strtab_index];
    char const *strings = strtab->data.s_strtab.strings;

    for (unsigned int i = 0; i < obj->sections_len; ++i)
    {
        char const *section_name = get_section_name(obj->sections[i]);
        int string_index = mempos(strings, strtab->data.s_strtab.strings_len,
                                  section_name, strlen(section_name));

        if (string_index != -1) target->section_headers[i]->sh_name = string_index;
    }
}

void fix_section_offsets(elf_object const *obj, Elf64_Object *target)
{
    Elf64_Off offset = relocation_tables_end;
    // start counting offsets right after the relocation tables
    uint64_t total_data_size = 0;

    for (unsigned int i = 0; i < obj->sections_len; ++i)
    {
        elf_section_header const *section = obj->sections[i];
        Elf64_Shdr *section_header = target->section_headers[i];

        switch (section->type)
        {
            case S_PROGBITS:
                section_header->sh_size = section->data.s_progbits.binary_data_len;
                break;
            case S_NOBITS:
                section_header->sh_size = section->data.s_nobits.space;
                break;
            case S_STRTAB:
                section_header->sh_size = section->data.s_strtab.strings_len;
                break;
            default:
                // - S_NULL: has no size and no data
                // - S_RELA: offsets are already computed and no data
                continue;
        }

        section_header->sh_offset = offset;
        section_header->sh_addr = 0;
        // temporary, for a relocatable file only
        offset += section_header->sh_size;
        total_data_size += section_header->sh_size;
    }

    target->binary_data_len = total_data_size;
    target->binary_data = calloc(target->binary_data_len, sizeof(unsigned char));
    assert(target->binary_data != NULL);
    data_end = offset;
    // register the end of the binary data section of the file, for future use
    offset = 0;
    // reuse the offset to copy data into `target->binary_data`

    for (unsigned int i = 0; i < obj->sections_len; ++i)
    {
        elf_section_header const *section = obj->sections[i];

        switch (section->type)
        {
            case S_PROGBITS:
                memcpy(target->binary_data + offset, section->data.s_progbits.binary_data,
                       section->data.s_progbits.binary_data_len);
                break;
            case S_NOBITS:
                memset(target->binary_data + offset, 0, section->data.s_nobits.space);
                break;
            case S_STRTAB:
                memcpy(target->binary_data + offset, section->data.s_strtab.strings,
                       section->data.s_strtab.strings_len);
                break;
            default:
                // - S_NULL: has no size and no data
                // - S_RELA: offsets are already computed and no data
                continue;
        }

        offset += target->section_headers[i]->sh_size;
    }
}

void fix_symtab_offset_and_shinfo(elf_object const *obj, Elf64_Object *target)
{
    int symtab_index = find_section_index_by_name(obj->sections, obj->sections_len, ".symtab");
    Elf64_Word number_of_symbols = obj->sections[symtab_index]->data.s_symtab.symbols_len;
    Elf64_Shdr *symtab = target->section_headers[symtab_index];

    symtab->sh_info = number_of_symbols;
    symtab->sh_offset = data_end;
}

void fix_symbol_names_and_sections(elf_object const *obj, Elf64_Object *target)
{
    int strtab_index = find_section_index_by_name(obj->sections, obj->sections_len, ".strtab");
    int symtab_index = find_section_index_by_name(obj->sections, obj->sections_len, ".symtab");
    elf_section_header const *strtab = obj->sections[strtab_index];
    elf_section_header const *symtab = obj->sections[symtab_index];
    int text_index = find_section_index_by_name(obj->sections, obj->sections_len, ".text");
    int data_index = find_section_index_by_name(obj->sections, obj->sections_len, ".data");

    if (text_index == -1) text_index = STN_UNDEF;
    if (data_index == -1) data_index = STN_UNDEF;

    for (unsigned int i = 0; i < target->symbols_len; ++i)
    {
        elf_symbol const *s = symtab->data.s_symtab.symbols[i];
        Elf64_Sym *sym = target->symbols[i];
        int string_index = mempos(strtab->data.s_strtab.strings, strtab->data.s_strtab.strings_len,
                                  s->name, strlen(s->name));

        sym->st_name = string_index != -1 ? string_index : 0x0;

        switch (s->type->type)
        {
            case STT_OBJECT:
                sym->st_shndx = data_index;
                break;
            case STT_FUNC:
                sym->st_shndx = text_index;
                break;
            case STT_SECTION:
            {
                int section_index = find_section_index_by_name(obj->sections, obj->sections_len, s->type->data.st_section.name);

                sym->st_shndx = section_index == -1 ? STN_UNDEF : section_index;
                break;
            }
            default:
                sym->st_shndx = STN_UNDEF;
        }
    }
}

void fix_symbol_values(elf_object const *obj, Elf64_Object *target)
{
    int symtab_index = find_section_index_by_name(obj->sections, obj->sections_len, ".symtab");
    int text_index = find_section_index_by_name(obj->sections, obj->sections_len, ".text");
    int data_index = find_section_index_by_name(obj->sections, obj->sections_len, ".data");
    elf_section_header const *symtab = obj->sections[symtab_index];
    Elf64_Shdr *text = text_index != -1 ? target->section_headers[text_index] : NULL;
    Elf64_Shdr *data = data_index != -1 ? target->section_headers[data_index] : NULL;

    // Three cases:
    // - No symbols -> there's nothing to patch
    // - 1 symbol   -> its size = the size of the section it is in - its offset in this section
    // - 2+ symbols -> two consecutive symbols of the same type put the limit on the size of the first symbol
    //              => fetch all objects and all funcs separately (the list is already sorted in the Haskell-side)
    //              => we then have a correct way of identifying which symbol appeared first in the section
    //              => which means we can implement the strategy above

    int data_sym_count = 0;
    int text_sym_count = 0;

    for (unsigned int i = 0; i < target->symbols_len; ++i)
    {
        Elf64_Sym *symbol = target->symbols[i];

        if (ELF64_ST_TYPE(symbol->st_info) == STT_OBJECT) data_sym_count += 1;
        if (ELF64_ST_TYPE(symbol->st_info) == STT_FUNC) text_sym_count += 1;
    }

    struct symbol_type const **data_symbols = malloc(data_sym_count * sizeof(struct symbol_type *));
    assert(data_symbols != NULL);
    int *data_symbols_indices = malloc(data_sym_count * sizeof(int));
    assert(data_symbols_indices != NULL);
    struct symbol_type const **text_symbols = malloc(text_sym_count * sizeof(struct symbol_type *));
    assert(text_symbols != NULL);
    int *text_symbols_indices = malloc(text_sym_count * sizeof(int));
    assert(text_symbols_indices != NULL);

    for (unsigned int i = 0, j = 0, k = 0; i < target->symbols_len; ++i)
    {
        Elf64_Sym *s = target->symbols[i];
        elf_symbol const *sym = symtab->data.s_symtab.symbols[i];

        if (ELF64_ST_TYPE(s->st_info) == STT_OBJECT)
        {
            data_symbols[j] = sym->type;
            data_symbols_indices[j++] = i;
        }
        if (ELF64_ST_TYPE(s->st_info) == STT_FUNC)
        {
            text_symbols[k] = sym->type;
            text_symbols_indices[k++] = i;
        }
    }

    int current_offset_in_section = 0;

    if (data != NULL)
    {
        for (unsigned int i = 1; i < data_sym_count; ++i)
        {
            int current_symbol_index = data_symbols_indices[i - 1];
            unsigned long current_symbol_offset = data_symbols[i - 1]->data.st_object.offset;
            unsigned long next_symbol_offset = data_symbols[i]->data.st_object.offset;

            unsigned long current_symbol_size = next_symbol_offset - current_symbol_offset;

            Elf64_Sym *current_symbol = target->symbols[current_symbol_index];

            current_symbol->st_size = current_symbol_size;
            current_symbol->st_value = current_offset_in_section;

            current_offset_in_section += current_symbol_size;
        }
        if (data_sym_count > 0)
        {
            int current_symbol_index = data_symbols_indices[data_sym_count - 1];
            unsigned long current_symbol_offset = data_symbols[data_sym_count - 1]->data.st_object.offset;
            unsigned long current_symbol_size = data->sh_size - current_symbol_offset;

            Elf64_Sym *current_symbol = target->symbols[current_symbol_index];

            current_symbol->st_size = current_symbol_size;
            current_symbol->st_value = current_offset_in_section;
        }
    }

    current_offset_in_section = 0;

    if (text != NULL)
    {
        for (unsigned int i = 1; i < text_sym_count; ++i)
        {
            int current_symbol_index = text_symbols_indices[i - 1];
            unsigned long current_symbol_offset = text_symbols[i - 1]->data.st_func.offset;
            unsigned long next_symbol_offset = text_symbols[i]->data.st_func.offset;

            unsigned long current_symbol_size = next_symbol_offset - current_symbol_offset;

            Elf64_Sym *current_symbol = target->symbols[current_symbol_index];

            current_symbol->st_size = current_symbol_size;
            current_symbol->st_value = current_offset_in_section;

            current_offset_in_section += current_symbol_size;
        }
        if (text_sym_count > 0)
        {
            int current_symbol_index = text_symbols_indices[text_sym_count - 1];
            unsigned long current_symbol_offset = text_symbols[text_sym_count - 1]->data.st_func.offset;
            unsigned long current_symbol_size = text->sh_size - current_symbol_offset;

            Elf64_Sym *current_symbol = target->symbols[current_symbol_index];

            current_symbol->st_size = current_symbol_size;
            current_symbol->st_value = current_offset_in_section;
        }
    }

    free(data_symbols);
    free(data_symbols_indices);
    free(text_symbols);
    free(text_symbols_indices);
}

void fix_rel_sections_shinfo_and_shlink(elf_object const *obj, Elf64_Object *target)
{
    int symtab_index = find_section_index_by_name(obj->sections, obj->sections_len, ".symtab");
    symtab_index = symtab_index == -1 ? 0 : symtab_index;

    // .rela.text
    int relatext_index = find_section_index_by_name(obj->sections, obj->sections_len, ".rela.text");
    if (relatext_index != -1)
    {
        int text_index = find_section_index_by_name(obj->sections, obj->sections_len, ".text");
        target->section_headers[relatext_index]->sh_link = symtab_index;
        target->section_headers[relatext_index]->sh_info = text_index;
    }
}

#undef NB_RELOCATION_TABLES

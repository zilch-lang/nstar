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

#undef NB_RELOCATION_TABLES

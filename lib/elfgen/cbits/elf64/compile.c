#include "object.h"
#include "file_header.h"
#include "segment_header.h"
#include "section_header.h"
#include "symbol.h"
#include "elf64/compile.h"
#include "elf64/fix.h"
#include "elf64/internal_fix.h"

#include <elf.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

void compile_x64(elf_object const *obj, Elf64_Object *dst)
{
    dst->file_header = malloc(sizeof(Elf64_Ehdr));
    assert(dst->file_header != NULL);
    compile_file_header(obj->file_header, dst->file_header);

    int is_executable_object = dst->file_header->e_type == ET_EXEC;

    dst->segments_len = is_executable_object ? obj->segments_len : 0;
    dst->segment_headers = malloc(dst->segments_len * sizeof(Elf64_Phdr *));
    assert(dst->segment_headers != NULL);
    for (unsigned int i = 0; i < dst->segments_len; ++i)
    {
        dst->segment_headers[i] = malloc(sizeof(Elf64_Phdr));
        assert(dst->segment_headers[i] != NULL);
        compile_segment_header(obj->segments[i], dst->segment_headers[i]);
    }

    dst->sections_len = obj->sections_len;
    dst->section_headers = malloc(dst->sections_len * sizeof(Elf64_Shdr *));
    assert(dst->section_headers != NULL);
    for (unsigned int i = 0; i < dst->sections_len; ++i)
    {
        dst->section_headers[i] = malloc(sizeof(Elf64_Shdr));
        assert(dst->section_headers[i] != NULL);
        compile_section_header(obj->sections[i], dst->section_headers[i]);

        elf_section_header const *sect = obj->sections[i];
        if (sect->type == S_SYMTAB)
        {
            dst->symbols_len = sect->data.s_symtab.symbols_len;
            dst->symbols = malloc(dst->symbols_len * sizeof(Elf64_Sym *));
            assert(dst->symbols != NULL);
            for (unsigned int i = 0; i < dst->symbols_len; ++i)
            {
                dst->symbols[i] = malloc(sizeof(Elf64_Sym));
                assert(dst->symbols[i] != NULL);
                compile_symbol(sect->data.s_symtab.symbols[i], dst->symbols[i]);
            }
        }
    }

    fix_elf_object(obj, dst);
}


void compile_file_header(elf_file_header const *fheader, Elf64_Ehdr *dst)
{
    dst->e_ident[EI_MAG0] = ELFMAG0;
    dst->e_ident[EI_MAG1] = ELFMAG1;
    dst->e_ident[EI_MAG2] = ELFMAG2;
    dst->e_ident[EI_MAG3] = ELFMAG3;
    dst->e_ident[EI_CLASS] = (unsigned char) fheader->class;
    dst->e_ident[EI_DATA] = (unsigned char) fheader->encoding;
    dst->e_ident[EI_VERSION] = (unsigned char) fheader->version;
    dst->e_ident[EI_OSABI] = (unsigned char) fheader->osabi;
    dst->e_ident[EI_ABIVERSION] = (unsigned char) fheader->osabi_version;

    for (unsigned int i = EI_PAD; i < 16; ++i) dst->e_ident[i] = '\0';

    dst->e_type = (Elf64_Half) fheader->object_file_type;
    dst->e_machine = (Elf64_Half) fheader->arch;
    dst->e_version = (Elf64_Word) fheader->version;
    dst->e_entry = 0x0;
    dst->e_phoff = 0x0;
    dst->e_shoff = 0x0;
    dst->e_flags = (Elf64_Word) fheader->flags;
    dst->e_ehsize = (Elf64_Half) sizeof(Elf64_Ehdr);
    dst->e_phentsize = (Elf64_Half) sizeof(Elf64_Phdr);
    dst->e_phnum = 0x0;
    dst->e_shentsize = (Elf64_Half) sizeof(Elf64_Shdr);
    dst->e_shnum = 0x0;
    dst->e_shstrndx = 0x0;
}

void compile_segment_header(elf_segment_header const *segment, Elf64_Phdr *dst)
{
    dst->p_type = (Elf64_Word) segment->type;

    switch (segment->type)
    {
        case P_PHDR:
            dst->p_flags = PF_R;
            dst->p_align = 0x8;
            break;
        case P_NULL:
            dst->p_flags = 0x0;
            dst->p_align = 0x0;
            break;
        case P_LOAD:
            dst->p_flags = segment->data.p_load.flags;
            dst->p_align = 0x1000;
            break;
        case P_INTERP:
            dst->p_flags = segment->data.p_interp.flags;
            dst->p_align = 0x1;
            break;
    }

    dst->p_offset = 0x0;
    dst->p_vaddr = 0x0;
    dst->p_paddr = 0x0;
    dst->p_filesz = 0x0;
    dst->p_memsz = 0x0;
}

void compile_section_header(elf_section_header const *section, Elf64_Shdr *dst)
{
    dst->sh_name = 0x0;
    dst->sh_type = (Elf64_Word) section->type;

    switch (section->type)
    {
        case S_NULL:
            dst->sh_flags = 0x0;
            dst->sh_size = 0x0;
            dst->sh_addralign = 0x0;
            break;
        case S_PROGBITS:
            dst->sh_flags = section->data.s_progbits.flags;
            dst->sh_size = 0x0;
            dst->sh_addralign = 0x1;
            break;
        case S_RELA:
            dst->sh_flags = 0x0;
            dst->sh_entsize = sizeof(Elf64_Rela);
            dst->sh_size = section->data.s_rela.symbols_len * dst->sh_entsize;
            dst->sh_addralign = 0x8;
            break;
        case S_NOBITS:
            dst->sh_flags = section->data.s_nobits.flags;
            dst->sh_size = 0x0;
            dst->sh_addralign = 0x20;
            break;
        case S_SYMTAB:
            dst->sh_flags = 0x0;
            dst->sh_entsize = sizeof(Elf64_Sym);
            dst->sh_size = section->data.s_symtab.symbols_len * dst->sh_entsize;
            dst->sh_addralign = 0x8;
            break;
        case S_STRTAB:
            dst->sh_flags = 0x0;
            dst->sh_size = 0x0;
            dst->sh_addralign = 0x1;
            break;
    }

    dst->sh_addr = 0x0;
    dst->sh_offset = 0x0;
    dst->sh_link = 0x0;
    dst->sh_info = 0x0;
}

void compile_symbol(elf_symbol const *sym, Elf64_Sym *target)
{
    target->st_name = 0x0;
    target->st_info = ELF64_ST_INFO(sym->binding, sym->type->type);
    target->st_other = sym->visibility;
    target->st_shndx = 0x0;
    target->st_value = 0x0;
    target->st_size = 0x0;
}

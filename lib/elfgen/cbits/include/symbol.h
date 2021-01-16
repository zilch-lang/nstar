#ifndef ELFGEN_SYMBOL_H
#define ELFGEN_SYMBOL_H

#include <stdint.h>
#include <elf.h>

typedef struct
{
    char const *name;

    struct symbol_type
    {
        enum symbol_type_
        {
            ST_NOTYPE = STT_NOTYPE,
            ST_OBJECT = STT_OBJECT,
            ST_FUNC = STT_FUNC,
            ST_SECTION = STT_SECTION,
            ST_FILE = STT_FILE,
            ST_COMMON = STT_COMMON,
            ST_TLS = STT_TLS
        } type;

        union
        {
            struct {} st_notype;

            struct
            {
                unsigned long offset;
            } st_object;

            struct
            {
                unsigned long offset;
            } st_func;

            struct
            {
                char const *name;
            } st_section;

            struct
            {
                char const *name;
            } st_file;

            struct {} st_common;

            struct {} st_tls;
        } data;
    } *type;

    enum symbol_binding
    {
        SB_LOCAL = STB_LOCAL,
        SB_GLOBAL = STB_GLOBAL,
        SB_WEAK = STB_WEAK
    } binding;

    enum symbol_visibility
    {
        SV_DEFAULT = STV_DEFAULT,
        SV_INTERNAL = STV_INTERNAL,
        SV_HIDDEN = STV_HIDDEN,
        SV_PROTECTED = STV_PROTECTED
    } visibility;
} elf_symbol;

typedef struct
{
    struct relocation_origin
    {
        enum relocation_origin_type
        {
            ORIGIN_SECTION
        } type;

        union
        {
            struct
            {
                char const *section_name;
                uint64_t offset;
            } origin_section;
        } data;
    } *origin;

    enum symbol_relocation_type
    {
        RT_X86_64_NONE = R_X86_64_NONE,
        RT_X86_64_32 = R_X86_64_32,
        RT_X86_64_32S = R_X86_64_32S,
        RT_X86_64_64 = R_X86_64_64
    } reloc_type;

    uint64_t offset;
} elf_relocation_symbol;

#endif

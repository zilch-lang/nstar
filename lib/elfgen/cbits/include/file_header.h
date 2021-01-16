#ifndef ELFGEN_FILE_HEADER_H
#define ELFGEN_FILE_HEADER_H

#include <stdint.h>
#include <elf.h>

typedef struct
{
    enum elf_class
    {
        C_NONE = ELFCLASSNONE,
        C_32 = ELFCLASS32,
        C_64 = ELFCLASS64
    } class;

    enum elf_encoding
    {
        D_NONE = ELFDATANONE,
        D_2LSB = ELFDATA2LSB,
        D_2MSB = ELFDATA2MSB
    } encoding;

    enum elf_osabi
    {
        OSABI_NONE = ELFOSABI_NONE,
        OSABI_SYSV = ELFOSABI_SYSV
    } osabi;

    uint8_t osabi_version;

    enum elf_file_type
    {
        OFT_NONE = ET_NONE,
        OFT_REL = ET_REL,
        OFT_EXEC = ET_EXEC,
        OFT_DYN = ET_DYN,
        OFT_CORE = ET_CORE
    } object_file_type;

    enum elf_arch
    {
        MA_NONE = EM_NONE,
        MA_SPARC = EM_SPARC,
        MA_X86_64 = EM_X86_64,
        MA_ARM = EM_ARM
    } arch;

    enum elf_version
    {
        VER_NONE = EV_NONE,
        VER_CURRENT = EV_CURRENT
    } version;

    uint32_t flags;
} elf_file_header;

#endif

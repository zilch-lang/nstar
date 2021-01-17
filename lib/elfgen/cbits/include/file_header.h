#ifndef ELFGEN_FILE_HEADER_H
#define ELFGEN_FILE_HEADER_H

#include <stdint.h>
#include <elf.h>

/**
 * @brief An abstract ELF file header.
 *
 * @details Contains all of the useful information to compile to a complete ELF file header.
 * */
typedef struct
{
    enum elf_class
    {
        C_NONE = ELFCLASSNONE, //!< No class
        C_32 = ELFCLASS32,     //!< 32 bit object file
        C_64 = ELFCLASS64      //!< 64 bit object file
    } class; //!< The class of the object file to generate.

    enum elf_encoding
    {
        D_NONE = ELFDATANONE,   //!< No data encoding
        D_2LSB = ELFDATA2LSB,   //!< 2's complement, little-endian encoding
        D_2MSB = ELFDATA2MSB    //!< 2's complement, big-endian encoding
    } encoding;  //!< The encoding of the object file.

    enum elf_osabi
    {
        OSABI_NONE = ELFOSABI_NONE,   //!< No specific ABI
        OSABI_SYSV = ELFOSABI_SYSV    //!< Alias for `OSABI_NONE`
    } osabi;   //!< The ABI the object file can run on.

    uint8_t osabi_version;   //!< The version of the ABI the file is meant to be generated for.

    enum elf_file_type
    {
        OFT_NONE = ET_NONE,   //!< No file type
        OFT_REL = ET_REL,     //!< Relocatable object file
        OFT_EXEC = ET_EXEC,   //!< Executable object file
        OFT_DYN = ET_DYN,     //!< Dynamically linkable object file
        OFT_CORE = ET_CORE    //!< Core object file
    } object_file_type;    //!< The object file type.

    enum elf_arch
    {
        MA_NONE = EM_NONE,      //!< No machine
        MA_SPARC = EM_SPARC,    //!< Sun SPARC
        MA_X86_64 = EM_X86_64,  //!< AMD64
        MA_ARM = EM_ARM         //!< ARM
    } arch;   //!< The architecture the file is meant to be run on.

    enum elf_version
    {
        VER_NONE = EV_NONE,        //!< No version
        VER_CURRENT = EV_CURRENT   //!< The current version
    } version;   //!< The version of the object file.

    uint32_t flags;   //!< Processor specific flags.
} elf_file_header;

#endif

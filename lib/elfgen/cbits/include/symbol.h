#ifndef ELFGEN_SYMBOL_H
#define ELFGEN_SYMBOL_H

#include <stdint.h>
#ifndef _WIN32
#  include <elf.h>
#else 
#  include "elf.h"
#endif 

/**
 * @brief An abstract ELF symbol.
 * */
typedef struct
{
    char const *name;     //!< The name of the symbol

    struct symbol_type
    {
        enum symbol_type_
        {
            ST_NOTYPE = STT_NOTYPE,    //!< No symbol type (unspecified)
            ST_OBJECT = STT_OBJECT,    //!< A data object
            ST_FUNC = STT_FUNC,        //!< A code object
            ST_SECTION = STT_SECTION,  //!< A section
            ST_FILE = STT_FILE,        //!< A file
            ST_COMMON = STT_COMMON,    //!< Common data object
            ST_TLS = STT_TLS           //!< Thread-local data object
        } type;

        union
        {
            struct {} st_notype;

            struct
            {
                unsigned long offset;   //!< The symbol offset in a data section
            } st_object;

            struct
            {
                unsigned long offset;   //!< The symbol offset in a code section
            } st_func;

            struct
            {
                char const *name;       //!< The section name the symbol is associated with
            } st_section;

            struct
            {
                char const *name;       //!< The file name
            } st_file;

            struct {} st_common;

            struct {} st_tls;
        } data;
    } *type;  //!< The type of the symbol.

    enum symbol_binding
    {
        SB_LOCAL = STB_LOCAL,      /**<
                                    * @brief A local symbol.
                                    *
                                    * @details A local symbol cannot be referenced outside the scope of the object it is declared in.
                                    * */
        SB_GLOBAL = STB_GLOBAL,    /**<
                                    * @brief A global symbol.
                                    *
                                    * @details It can be "exported" to be used in other objects.
                                    * */
        SB_WEAK = STB_WEAK         //!< A weak symbol.
    } binding;  //!< The binding of the symbol.

    enum symbol_visibility
    {
        SV_DEFAULT = STV_DEFAULT,     //!< Default symbol visibility
        SV_INTERNAL = STV_INTERNAL,   //!< Processor specific hidden class
        SV_HIDDEN = STV_HIDDEN,       //!< Symbol unavailable in other modules
        SV_PROTECTED = STV_PROTECTED  //!< Not preemptible, not exported
    } visibility;  //!< The visiblity of the symbol.
} elf_symbol;

/**
 * @brief An abstract ELF relocation symbol.
 * */
typedef struct
{
    struct relocation_origin
    {
        enum relocation_origin_type
        {
            ORIGIN_SECTION,   //!< The symbol originates form a section
            ORIGIN_FUNCTION   //!< The symbol is a simple label
        } type;   //!< The type of origin for the symbol.

        union
        {
            struct
            {
                char const *section_name;   //!< The name of the section it originates from
                uint64_t offset;            //!< The computed offset in the section
            } origin_section;
            struct
            {
                char const *symbol_name;    //!< The name of the function
            } origin_function;
        } data;
    } *origin;  //!< The origin of the relocation symbol.

    enum symbol_relocation_type
    {
        RT_X86_64_NONE = R_X86_64_NONE,   //!< No relocation
        RT_X86_64_32 = R_X86_64_32,       //!< Direct 32 bit zero extended address
        RT_X86_64_32S = R_X86_64_32S,     //!< Direct 32 bit sign extended address
        RT_X86_64_64 = R_X86_64_64,       //!< Direct 64 bit address
        RT_X86_64_PLT32 = R_X86_64_PLT32  //!< 32-bit PLT address
    } reloc_type;  //!< The type of relocation for the symbol.

    uint64_t offset;  //!< The offset where to relocate data in the section of the relocation table.
} elf_relocation_symbol;

#endif

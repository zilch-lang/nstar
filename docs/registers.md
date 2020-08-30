<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Registers in different architectures](#registers-in-different-architectures)
    - [Registers available in architectures](#registers-available-in-architectures)
        - [x64](#x64)

<!-- markdown-toc end -->


# Registers in different architectures

Registers are fixed based on which architecture you are targetting. When the number of bits increases, the number of registers to use should also be higher.

## Registers available in architectures

### x64

| Register name | Size (B) |
|:-------------:|:--------:|
| rax           | 8        |
| rbx           | 8        |
| rcx           | 8        |
| rdx           | 8        |
| rsi           | 8        |
| rdi           | 8        |
| rsp           | 8        |
| rbp           | 8        |

> TODO: support more architectures, and more registers

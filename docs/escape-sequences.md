# Escape sequences in N*

Escape sequences are special characters prefixed with `\` that give them special power. Most of the escape sequences are used to input invisible control characters like a line feed or a null character.

Escape sequences in N* follow closely those available in C:

| Escape sequence | Description                                                                                                  |
|:---------------:|--------------------------------------------------------------------------------------------------------------|
| `\a`            | Bell, makes an audible ding                                                                                  |
| `\b`            | Backspace character, go back one character on the display                                                    |
| `\e`            | Escape character, used in Unix terminals for the ANSI color codes                                            |
| `\f`            | Form feed page break                                                                                         |
| `\n`            | Newline, Line feed                                                                                           |
| `\r`            | Carriage return                                                                                              |
| `\t`            | Horizontal tab                                                                                               |
| `\v`            | Vertical tab                                                                                                 |
| `\\`            | Backslash                                                                                                    |
| `\'`            | Apostrophe, useful in a character construct `'c'`                                                            |
| `\"`            | Double quotation mark, useful in a string construct `"..."`<br>Strings are, however, not yet available in N* |
| `\0`            | Null character                                                                                               |

> TODO: support unicode escape sequences `\uHHHH` and `\uHHHHHHHH`

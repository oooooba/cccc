#ifndef CCCC_STDIO_H
#define CCCC_STDIO_H

struct _IO_FILE;
typedef struct _IO_FILE FILE;

int printf(const char* format, ...);
int fprintf(FILE* stream, const char* format, ...);

FILE* fopen(const char* path, const char* mode);
int fclose(FILE* stream);

#endif  // CCCC_STDIO_H


// A Kit interpreter that bootstraps a self-hosted Kit compiler
// http://bootstrappable.org/best-practices.html

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "parser/parser.h"

int main(int argc, char** argv) {
  if (argc < 2) {
    puts("Usage: kitc <input.kit> [-o output_binary]");
    return EXIT_FAILURE;
  }

  char* inputFileName = argv[1];
  struct stat inputFileStats;

  if(access(inputFileName, F_OK) != 0) {
    printf("%s: cannot open '%s' (No such file or directory)", inputFileName, inputFileName);
    return EXIT_FAILURE;
  }
  if(access(inputFileName, R_OK) != 0 || stat(inputFileName, &inputFileStats) != 0) {
    printf("%s: cannot read '%s' (Insufficient read permissions)", inputFileName, inputFileName);
    return EXIT_FAILURE;
  }

  FILE* inputFile = fopen(inputFileName, "r");
  if (inputFile == NULL) {
    printf("%s: cannot open '%s'", inputFileName, inputFileName);
    return EXIT_FAILURE;
  }

  size_t inputFileLength = inputFileStats.st_size;
  char* inputFileBuf = malloc(inputFileLength);
  size_t read = fread(inputFileBuf, sizeof(char), inputFileLength, inputFile);
  assert(read == inputFileLength);

  if (ferror(inputFile)) {
    printf("%s: I/O error reading file", inputFileName);
    return EXIT_FAILURE;
  }
  if (fclose(inputFile) != 0) {
    printf("%s: cannot close '%s'", inputFileName, inputFileName);
    return EXIT_FAILURE;
  }

  String inputFileContents = toString(inputFileBuf);
  assert(inputFileContents.length == inputFileLength);

  // Parse and interpret the input file
  KitErrors errors;
  if (parseString(inputFileContents, &errors)) {
    // TODO: If successful, dynamically interpret the parsed module
  } else {
    puts("Parser failure");
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

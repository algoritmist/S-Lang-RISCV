#include <unistd.h>
#include <stdlib.h>
#include <stdio.h> 
#include <inttypes.h>

void* input(int size){
    char* arr = malloc(size + 1);
    arr[0] = (char) size;
    read(STDIN_FILENO, arr + 1, size);
    return arr;
}

ssize_t output(void *ptr){
    char* char_ptr = (char*) ptr;
    char size = char_ptr[0];
    return write(STDOUT_FILENO, char_ptr + 1, size);
}

int outputInt(__int64_t value){
    return printf("%" PRId64 "\n", value);
}

void* id(void* value){
    return value;
}
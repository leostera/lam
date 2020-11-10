#include <stdio.h>

const char bytecode[] = {LAM_BYTECODE_RAW};

extern void lam_rts__start(const char* bytecode, size_t size);

void main() {
  lam_rts__start(bytecode, LAM_BYTECODE_SIZE);
}

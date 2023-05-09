#pragma once

#include <stdio.h>
#include <string>
#include <iostream>

#include "chunk.h"

int disassembleInstruction(Chunk* chunk, int offset);
void disassembleChunk(Chunk* chunk, const char* name);

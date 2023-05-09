#pragma once

#include <stdlib.h>

#include "array.h"
#include "memory.h"
// #include "value.h"

typedef enum {
    OP_CONSTANT,
    OP_CONSTANT_LONG,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_RETURN,
    OP_NEGATE,
    OP_NOT,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_TERNARY,
    OP_PRINT,
    OP_POP,
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_JUMP_IF_FALSE,
    OP_JUMP_IF_TRUE,
    OP_JUMP,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
} OpCode;


class Chunk {
    public:
        Array<uint8_t> lines;
        Array<uint8_t> code;
        Chunk();
        void clearChunk();
        void writeChunk(uint8_t byte, unsigned line);
        void freeChunk();
        // unsigned writeConstant(Value value, unsigned line);
        // unsigned writeConstant(unsigned line, unsigned idx);
        void writeLine(unsigned line);
};
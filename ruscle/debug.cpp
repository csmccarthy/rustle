
#include "debug.h"

static int simpleInstruction(const char* name, int offset) {
    printf("%s\t\t\t", name);
    return offset + 1;
}

static int byteInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t var_idx = chunk->code[offset+1];
    printf("%-16s %4d\t\t", name, var_idx);
    return offset + 2;
}

static int constantLongInstruction(const char* name, Chunk* chunk, int offset) {
    uint16_t constant_idx = (chunk->code[offset+1] << 8) + chunk->code[offset+2];
    printf("%-16s ", name);
    // std::string str_val = chunk->constants[constant_idx].to_string(); // TODO: Fix
    std::string str_val = "";
    if (str_val.length() > 12) {
        std::cout << "'" << str_val.substr(0, 9) << "...'";
    } else {
        std::cout << "'" << str_val << "'";
    }
    return offset + 3;
}

static int jumpInstruction(const char* name, Chunk* chunk, int offset) {
    uint16_t jump_len = (chunk->code[offset+1] << 8) + chunk->code[offset+2];
    printf("%-16s  %4d\t", name, jump_len);
    return offset + 3;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant_idx = chunk->code[offset+1];
    printf("%-16s ", name);
    // std::string str_val = chunk->constants[constant_idx].to_string(); // TODO: Fix
    std::string str_val = "";
    if (str_val.length() > 12) {
        std::cout << "   '" << str_val.substr(0, 9) << "...'";
    } else {
        std::cout << "   '" << str_val << "'";
        std::string padding = std::string();
        padding.append(12 - str_val.length(), ' ');
        std::cout << padding;
    }
    return offset + 2;
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);

    unsigned run_len = 0;
    bool new_line = false;
    unsigned line_num = 0;

    for (unsigned i = 0; i < chunk->lines.count / 2; i++) {
        unsigned new_rl = chunk->lines[i * 2];
        run_len += new_rl;
        line_num = chunk->lines[(i * 2) + 1];
        if (run_len > offset) {
            new_line = run_len - new_rl == offset ? true : false;
            break;
        }
    }
    if (new_line) {
        printf("%4d ", line_num);
    } else {
        printf("\t| ");
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        case OP_TRUE:
            return constantInstruction("OP_TRUE", chunk, offset);
        case OP_FALSE:
            return constantInstruction("OP_FALSE", chunk, offset);
        case OP_NIL:
            return constantInstruction("OP_NIL", chunk, offset);
        case OP_NOT:
            return constantInstruction("OP_NOT", chunk, offset);
        case OP_EQUAL:
            return constantInstruction("OP_EQUAL", chunk, offset);
        case OP_GREATER:
            return constantInstruction("OP_GREATER", chunk, offset);
        case OP_LESS:
            return constantInstruction("OP_LESS", chunk, offset);
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OP_CONSTANT_LONG:
            return constantLongInstruction("OP_CONSTANT_LONG", chunk, offset);
        case OP_ADD:
            return simpleInstruction("OP_ADD", offset);
        case OP_SUBTRACT:
            return simpleInstruction("OP_SUBTRACT", offset);
        case OP_MULTIPLY:
            return simpleInstruction("OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return simpleInstruction("OP_DIVIDE", offset);
        case OP_PRINT:
            return simpleInstruction("OP_PRINT", offset);
        case OP_POP:
            return simpleInstruction("OP_POP", offset);
        case OP_DEFINE_GLOBAL:
            return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
        case OP_GET_GLOBAL:
            return constantInstruction("OP_GET_GLOBAL", chunk, offset);
        case OP_SET_GLOBAL:
            return constantInstruction("OP_SET_GLOBAL", chunk, offset);
        case OP_GET_LOCAL:
            return byteInstruction("OP_GET_LOCAL", chunk, offset);
        case OP_SET_LOCAL:
            return byteInstruction("OP_SET_LOCAL", chunk, offset);
        case OP_CALL:
            return byteInstruction("OP_CALL", chunk, offset);
        case OP_JUMP:
            return jumpInstruction("OP_JUMP", chunk, offset);
        case OP_JUMP_IF_FALSE:
            return jumpInstruction("OP_JUMP_IF_FALSE", chunk, offset);
        case OP_JUMP_IF_TRUE:
            return jumpInstruction("OP_JUMP_IF_TRUE", chunk, offset);
        case OP_LOOP:
            return jumpInstruction("OP_LOOP", chunk, offset);
        case OP_CLOSURE: {
            offset++;
            uint8_t constant = chunk->code[offset++];
            printf("%-16s %4d ", "OP_CLOSURE", constant);
            printf(""); // TODO: Fix
            printf("\n");
            return offset;
        }
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);
    for (int offset = 0; offset < chunk->code.count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}
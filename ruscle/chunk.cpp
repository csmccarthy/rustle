#include "chunk.h"


Chunk::Chunk() {
    this->code.initArray();
    this->lines.initArray();
}

void Chunk::clearChunk() {
    this->code.initArray();
    this->lines.initArray();
}

void Chunk::writeChunk(uint8_t byte, unsigned line) {
    this->code.writeElement(byte);
    this->writeLine(line);
}

void Chunk::freeChunk() {
    this->code.freeArray();
    this->lines.freeArray();
    this->clearChunk();
}

void Chunk::writeLine(unsigned line) {
    if (this->lines.count == 0) {
        this->lines.writeElement(1);
        this->lines.writeElement(line);
        return;
    }
    uint8_t offset = this->lines.count - 2;
    if (this->lines.elems[offset+1] == line) {
        this->lines.elems[offset]++;
        return;
    }
    this->lines.writeElement(1);
    this->lines.writeElement(line);
}
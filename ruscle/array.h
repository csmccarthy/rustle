#pragma once

#include <stdlib.h>
#include <stdio.h>

#include "memory.h"

template <typename T>
class Array {
    public:
        uint16_t count;
        uint16_t capacity;
        T* elems;
        void initArray() {
            this->count = 0;
            this->capacity = 0;
            this->elems = NULL;
        }
        void writeElement(T elem) {
            if (this->capacity < this->count + 1) {
                uint16_t oldCapacity = this->capacity;
                this->capacity = GROW_CAPACITY(oldCapacity);
                this->elems = GROW_ARRAY(T, this->elems, oldCapacity, this->capacity);
            }
            this->elems[this->count] = elem;
            this->count++;
        }
        bool pop() {
            if (this->count == 0) {
                fprintf(stderr, "Attempted to pop an empty array.");
                return false;
            }
            this->count--;
            return true;
        }
        void freeArray() {
            FREE_ARRAY(T, this->elems, this->capacity);
            this->initArray();
        }
        T operator [](int i) const {
            return this->elems[i];
        }
};
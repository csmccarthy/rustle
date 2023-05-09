#pragma once

#include <stdio.h>
#include <string.h>
#include <string>
#include <iostream>

#include "common.h"
#include "memory.h"
#include "chunk.h"
#include "value.h"

typedef enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_CLOSURE,
} ObjType;

class Obj {
    public:
        ObjType type;
        Obj* next;

        bool is_string();
        bool is_function();
        bool is_native();
        bool is_closure();
        virtual void print();
        virtual std::string to_string();
        bool operator == (Obj *obj);
        bool compare(Obj *obj);
        bool compare(Obj &obj);
        virtual void free();
};

class ObjString : public Obj {
    public:
        union {
            char* owned_chars;
            const char* chars;
        } as;
        bool owned;
        int length;
        uint32_t hash;

        static ObjString* heap_init(int length, const char* chars, uint32_t hash);
        static ObjString* heap_init_owned(int length, char* chars, uint32_t hash);
        static char* heap_cstring(int length, const char* chars);

        void hash_self();
        static uint32_t hasher(const char* chars, int length);
        void print() override;
        std::string to_string() override;
        bool operator == (ObjString *obj);
        bool compare(ObjString *obj);
        bool compare(const char* chars, int length, uint32_t hash);
        char* operator + (ObjString *obj);
        void free() override;
};

// typedef struct Chunk Chunk;
typedef struct Value Value;

class ObjFunction : public Obj {
    public:
        int arity;
        Chunk chunk;
        ObjString* name;
        ObjFunction();
        static ObjFunction* heap_init();
        void free() override;
        void print() override;
        std::string to_string() override;

        Array<Value> constants;
        unsigned addConstant(Value value);
        unsigned writeConstant(unsigned line, unsigned idx);
        unsigned writeConstant(Value value, unsigned line);
};

class ObjClosure : public Obj {
    public:
        ObjFunction* function;
        ObjClosure(ObjFunction* fxn);
        static ObjClosure *heap_init(ObjFunction *fxn);
        void free() override;
        void print() override;
        std::string to_string() override;
};

typedef Value (*NativeFn)(int arg_count, Value* args);

class ObjNative : public Obj {
    public:
        NativeFn function;
        // ObjNative();
        ObjNative(NativeFn function);
        // static ObjNative* heap_init();
        static ObjNative *heap_init(NativeFn function);
        void free() override;
        void print() override;
        std::string to_string() override;
};
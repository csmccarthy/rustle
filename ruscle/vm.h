#pragma once

#include <stdio.h>
#include <iostream>
#include <time.h>

#include "chunk.h"
#include "debug.h"
#include "compiler.h"
#include "object.h"
#include "table.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)


typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

typedef struct {
    ObjClosure* closure;
    uint8_t* ip;
    Value* slots;
} CallFrame;

// typedef struct Obj Obj;

class VM {
    public:
        CallFrame frames[FRAMES_MAX];
        CallFrame* current_frame;
        int frame_count;
        Value stack[STACK_MAX];
        Value* stack_top;
        Obj* objects;
        Table strings;
        Table globals;
        // Compiler current;

        void init_VM();
        void free_VM();
        void reset_stack();
        void push(Value value);
        Value pop();
        inline uint8_t read_byte();
        Array<Value> chunk_constants();
        inline Value read_constant();
        Value read_bool();
        Value read_var();
        uint16_t read_short();
        Value read_constant_long();
        bool call(ObjClosure *closure, uint8_t arg_count);
        // bool call(ObjFunction *fxn, uint8_t arg_count);
        bool call_const(Value fxn_const, uint8_t arg_count);
        void define_native(const char *name, NativeFn function);
        ObjString *copy_string(const char *chars, int length);
        // bool call_const(Value fxn);
        InterpretResult interpret(const char *source);
        void runtime_error(const char *err_text, ...);
        Value peek(int distance);
        Value *peek_ptr(int distance);
        InterpretResult run();
};

extern VM vm;
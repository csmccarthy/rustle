#pragma once

#include <stdio.h>
#include <string>

#include "common.h"
#include "object.h"

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ,
} ValueType;

typedef struct Obj Obj;
typedef struct ObjString ObjString;
typedef struct ObjFunction ObjFunction;
typedef struct ObjNative ObjNative;
typedef struct ObjClosure ObjClosure;

class Value {
    public:
        ValueType type;
        union {
            bool boolean;
            double number;
            Obj* obj;
        } as;

        Value();
        Value(bool boolean);
        Value(double number);
        Value(Obj* obj);

        bool as_bool();
        double as_number();
        Obj* as_obj();
        ObjString* as_string();
        const char* as_cstring();
        ObjFunction* as_function();
        ObjNative* as_native();
        ObjClosure* as_closure();

        bool is_bool();
        bool is_nil();
        bool is_number();
        bool is_obj();
        bool is_string();
        bool is_function();
        bool is_native();
        bool is_closure();

        void print();
        std::string to_string();
};

#include <stdio.h>

#include "value.h"


Value::Value() { this->type = VAL_NIL; }
Value::Value(bool boolean) {
    this->type = VAL_BOOL;
    this->as = { .boolean = boolean };
}
Value::Value(double number) {
    this->type = VAL_NUMBER;
    this->as = { .number = number };
}
Value::Value(Obj* obj) {
    this->type = VAL_OBJ;
    this->as = { .obj = obj };
}

bool Value::as_bool() { return this->as.boolean; }
double Value::as_number() { return this->as.number; }
Obj* Value::as_obj() { return this->as.obj; }
ObjString* Value::as_string() { return (ObjString*)this->as.obj; }
const char* Value::as_cstring() { return this->as_string()->as.chars; }
ObjFunction *Value::as_function() { return (ObjFunction*)this->as.obj; }
ObjNative *Value::as_native() { return (ObjNative*)this->as.obj; }
ObjClosure *Value::as_closure() { return (ObjClosure*)this->as.obj; }

bool Value::is_bool() { return this->type == VAL_BOOL; }
bool Value::is_nil() { return this->type == VAL_NIL; }
bool Value::is_number() { return this->type == VAL_NUMBER; }
bool Value::is_obj() { return this->type == VAL_OBJ; }
bool Value::is_string() { return this->is_obj() && this->as.obj->is_string(); }
bool Value::is_function() { return this->is_obj() && this->as.obj->is_function(); }
bool Value::is_native() { return this->is_obj() && this->as.obj->is_native(); }
bool Value::is_closure() { return this->is_obj() && this->as.obj->is_closure(); }

void Value::print() {
    switch (this->type) {
        case VAL_BOOL: {
            printf(this->as_bool() ? "true" : "false");
            break;
        }
        case VAL_NIL: {
            printf("nil");
            break;
        }
        case VAL_NUMBER: {
            printf("%g", this->as_number());
            break;
        }
        case VAL_OBJ: {
            this->as.obj->print();
            break;
        }
    }
}

std::string Value::to_string() {
    switch (this->type) {
        case VAL_BOOL: {
            return this->as_bool() ? "true" : "false";
        }
        case VAL_NIL: {
            return "nil";
        }
        case VAL_NUMBER: {
            return std::to_string(this->as_number()).substr(0, 4);
        }
        case VAL_OBJ: {
            return this->as.obj->to_string();
        }
        default: return "";
    }
}
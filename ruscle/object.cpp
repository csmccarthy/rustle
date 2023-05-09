
#include "object.h"

void Obj::free() {
    reallocate(this, sizeof(Obj), 0);
}
void Obj::print() {
    printf("Obj");
}
std::string Obj::to_string() {
    return "Obj";
}

bool Obj::is_string() { return this->type == OBJ_STRING; }
bool Obj::is_function() { return this->type == OBJ_FUNCTION; }
bool Obj::is_native() { return this->type == OBJ_NATIVE; }
bool Obj::is_closure() { return this->type == OBJ_CLOSURE; }
bool Obj::operator == (Obj *obj) { return this == obj; }
bool Obj::compare(Obj *obj) { return this == obj; }
bool Obj::compare(Obj &obj) { return this == &obj; }



ObjString* ObjString::heap_init(int length, const char* chars, uint32_t hash) {
    ObjString* obj_str = new ObjString; // TODO: test remove new
    obj_str->type = OBJ_STRING;
    obj_str->length = length;
    obj_str->as.chars = chars;
    obj_str->owned = false;
    obj_str->hash = hash;
    return obj_str;
}
ObjString* ObjString::heap_init_owned(int length, char* chars, uint32_t hash) {
    ObjString* obj_str = new ObjString; // TODO: test remove new
    obj_str->type = OBJ_STRING;
    obj_str->length = length;
    obj_str->as.owned_chars = chars;
    obj_str->owned = true;
    obj_str->hash = hash;
    return obj_str;
}
char* ObjString::heap_cstring(int length, const char* chars) {
    char* chars_copy = (char*)reallocate(NULL, 0, sizeof(char) * (length + 1));
    memcpy(chars_copy, chars, length);
    chars_copy[length] = '\0';
    return chars_copy;
}


void ObjString::print() {
    printf("%.*s", this->length, this->as.chars);
}
bool ObjString::operator == (ObjString *obj) {
    // printf("%i == %i", this->length, obj->length);
    if (this->length != obj->length || memcmp(this->as.chars, obj->as.chars, length)) {
        return false;
    }
    return true;
}
bool ObjString::compare(const char* chars, int length, uint32_t hash) {
    if (this->length != length || this->hash != hash || memcmp(this->as.chars, chars, length) != 0) {
        return false;
    }
    return true;
}
bool ObjString::compare(ObjString *obj) {
    if (this->length != obj->length || memcmp(this->as.chars, obj->as.chars, length) != 0) {
        return false;
    }
    return true;
}
char* ObjString::operator + (ObjString *obj) {
    int new_len = this->length + obj->length + 1;
    char* chars_concat = (char*)reallocate(NULL, 0, sizeof(char) * (new_len));
    memcpy(chars_concat, this->as.chars, this->length); // Exclude the first's null char
    memcpy(chars_concat + this->length, obj->as.chars, obj->length + 1);
    return chars_concat;
}
void ObjString::hash_self() {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < this->length; i++) {
        hash ^= (uint8_t)this->as.chars[i];
        hash *= 16777619;
    }
    this->hash = hash;
}
uint32_t ObjString::hasher(const char* chars, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)chars[i];
        hash *= 16777619;
    }
    return hash;
}
void ObjString::free() {
    if (!this->owned) { return; }
    FREE_ARRAY(char, this->as.owned_chars, this->length + 1);
    reallocate(this, sizeof(ObjString), 0);
}
std::string ObjString::to_string() {
    return std::string(this->as.chars, this->length);
}


ObjFunction::ObjFunction() {
    this->type = OBJ_FUNCTION;
    this->arity = 0;
    this->name = NULL;
    this->constants.initArray();
    this->chunk = Chunk(); // Figure out this new, probably doesn't have to be in the free store
}
ObjFunction* ObjFunction::heap_init() {
    ObjFunction* obj_fxn = new ObjFunction;
    obj_fxn->type = OBJ_FUNCTION;
    obj_fxn->constants.initArray();
    obj_fxn->arity = 0;
    obj_fxn->name = NULL;
    obj_fxn->chunk = Chunk(); // Figure out this new, probably doesn't have to be in the free store
    return obj_fxn;
}
void ObjFunction::free() {
    this->chunk.freeChunk();
    reallocate(this, sizeof(ObjFunction), 0);
}
void ObjFunction::print() { std::cout << to_string(); }
std::string ObjFunction::to_string() {
    if (this->name == NULL) {
        return "<script>";
    }
    return "<fn " + this->name->to_string() + ">";
}

unsigned ObjFunction::addConstant(Value value) {
    this->constants.writeElement(value);
    return this->constants.count - 1;
}

unsigned ObjFunction::writeConstant(Value value, unsigned line) {
    unsigned idx = this->addConstant(value);
    if (idx > 255) {
        chunk.writeChunk(OP_CONSTANT_LONG, line);
        unsigned masked = (idx & 0x0000FF00) >> 8;
        chunk.writeChunk(masked, line);
        masked = idx & 0x000000FF;
        chunk.writeChunk(masked, line);
    } else {
        chunk.writeChunk(OP_CONSTANT, line);
        chunk.writeChunk(idx, line);
    }
    return idx;
}

unsigned ObjFunction::writeConstant(unsigned line, unsigned idx) {
    if (idx > 255) {
        chunk.writeChunk(OP_CONSTANT_LONG, line);
        unsigned masked = (idx & 0x0000FF00) >> 8;
        chunk.writeChunk(masked, line);
        masked = idx & 0x000000FF;
        chunk.writeChunk(masked, line);
    } else {
        chunk.writeChunk(OP_CONSTANT, line);
        chunk.writeChunk(idx, line);
    }
    return idx;
}


ObjNative::ObjNative(NativeFn function) {
    this->type = OBJ_NATIVE;
    this->function = function;
}
ObjNative* ObjNative::heap_init(NativeFn function) {
    ObjNative* native_fxn = new ObjNative(function);
    native_fxn->type = OBJ_NATIVE;
    native_fxn->function = function;
    return native_fxn;
}
void ObjNative::free() { reallocate(this, sizeof(ObjNative), 0); }
void ObjNative::print() { std::cout << to_string(); }
std::string ObjNative::to_string() { return "<native fn>"; }


ObjClosure::ObjClosure(ObjFunction* fxn) {
    this->function = fxn;
    this->type = OBJ_CLOSURE;
}
ObjClosure* ObjClosure::heap_init(ObjFunction* fxn) {
    ObjClosure* closure = new ObjClosure(fxn);
    return closure;
}
void ObjClosure::free() { reallocate(this, sizeof(ObjClosure), 0); }
void ObjClosure::print() { std::cout << to_string(); }
std::string ObjClosure::to_string() { return this->function->to_string(); }
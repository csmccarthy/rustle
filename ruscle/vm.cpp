
#include <stdarg.h>

#include "vm.h"

#define BINARY_OP(type, op) \
    do { \
        Value b = this->pop(); \
        Value a = this->pop(); \
        if (!b.is_number() || !a.is_number()) { \
            this->runtime_error("Both binary operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        this->push(Value(a.as.type op b.as.type)); \
    } while (false)


static Value clock_native(int arg_count, Value* args) {
    return Value((double)clock() / CLOCKS_PER_SEC);
}

void VM::init_VM() {
    this->reset_stack();
    this->strings = Table();
    this->globals = Table();
    this->objects = NULL;
    this->frame_count = 0;
    define_native("clock", clock_native);
}

void VM::free_VM() {
    Obj* working = this->objects;
    while (working != NULL) {
        Obj* next  = working->next;
        working->free();
        working = working->next;
    }
    this->strings.free();
    this->globals.free();
}

void VM::reset_stack() {
    this->stack_top = this->stack;
}

void VM::push(Value value) {
    *this->stack_top = value;
    this->stack_top++;
}

Value VM::pop() {
    this->stack_top--;
    return *this->stack_top;
}

uint8_t VM::read_byte() { return *this->current_frame->ip++; }

Array<Value> VM::chunk_constants() {
    return this->current_frame->closure->function->constants;
}

Value VM::read_constant() {
    return this->chunk_constants()[this->read_byte()];
}

Value VM::read_bool() {
    return this->chunk_constants()[this->read_byte()];
}

Value VM::read_var() {
    return this->current_frame->slots[this->read_byte()];
}

uint16_t VM::read_short() {
    uint8_t big_half = this->read_byte();
    uint8_t small_half = this->read_byte();
    return (big_half << 8) + small_half;
}

Value VM::read_constant_long() {
    uint16_t constant_idx = this->read_short();
    Value value = this->chunk_constants()[constant_idx];
    return value;
}

bool VM::call(ObjClosure* closure, uint8_t arg_count) {
    if (closure->function->arity != arg_count) {
        this->runtime_error("Incorrect number of arguments passed to function.");
        return false;
    }
    if (frame_count == FRAMES_MAX) {
        this->runtime_error("Stack overflow.");
        return false;
    }
    this->current_frame = &this->frames[this->frame_count++];
    this->current_frame->closure = closure;
    this->current_frame->ip = closure->function->chunk.code.elems;
    this->current_frame->slots = this->stack_top - (arg_count);
    return true;
}

bool VM::call_const(Value fxn_const, uint8_t arg_count) {
    switch (fxn_const.as.obj->type) {
        case OBJ_CLOSURE: {
            ObjClosure* fxn = fxn_const.as_closure();
            return call(fxn, arg_count);
        }
        case OBJ_NATIVE: {
            NativeFn native = fxn_const.as_native()->function;
            Value result = native(arg_count, stack_top - arg_count);
            stack_top -= arg_count + 1;
            push(result);
            return true;
        }
    }
    return false;
}

void VM::define_native(const char* name, NativeFn function) {
    push(Value(copy_string(name, (int)strlen(name))));
    push(Value(ObjNative::heap_init(function)));
    globals.insert(peek(1).as_string(), peek(0));
    pop();
    pop();
}

ObjString* VM::copy_string(const char* chars, int length) {
    uint32_t hash = ObjString::hasher(chars, length);
    ObjString* interned = vm.strings.lookup_by_chars(chars, hash, length);
    if (interned != NULL) { return interned; }

    ObjString* new_str = ObjString::heap_init(length, chars, hash);
    new_str->next = vm.objects;
    vm.objects = new_str;
    vm.strings.insert(new_str, Value());
    return new_str;
}

InterpretResult VM::interpret(const char* source) {
    Parser compiler = Parser(source); // issue here, source overriding last one in memory
    ObjFunction* function = compiler.compile();
    if (function == NULL) { return INTERPRET_COMPILE_ERROR; }
    this->push(Value(function));
    ObjClosure* closure = ObjClosure::heap_init(function);
    pop();
    push(Value(closure));
    call(closure, 0);
    return this->run();
}

void VM::runtime_error(const char* err_text, ...) {
    va_list args;
    va_start(args, err_text);
    vfprintf(stderr, err_text, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = frame_count - 1; i >= 0; i--) {
        CallFrame* frame = &frames[i];
        ObjFunction* fxn = frame->closure->function;
        size_t instruction = frame->ip - fxn->chunk.code.elems - 1;
        fprintf(stderr, "[line %d] in ", fxn->chunk.lines[instruction]);
        fxn->print();
        printf("\n");
    }

    // CallFrame* frame = &vm.frames[vm.frameCount - 1];
    ObjFunction* fxn = this->current_frame->closure->function;
    size_t instruction = this->current_frame->ip - fxn->chunk.code.elems - 1;
    int line = fxn->chunk.lines[instruction];
    fprintf(stderr, "[line %d] in script\n", line);
    this->reset_stack();
}

Value VM::peek(int distance) {
    return this->stack_top[-1 - distance];
}

Value* VM::peek_ptr(int distance) {
    return this->stack_top -1 - distance;
}

InterpretResult VM::run() {
    this->current_frame = &this->frames[this->frame_count - 1];
    for (;;) {
        #ifdef DEBUG_TRACE_EXECUTION
            std::string stack_str = "";
            for (Value* slot = this->stack; slot < this->stack_top; slot++) {
                stack_str += "[ ";
                stack_str += slot->to_string();
                stack_str += " ]";
            }
            ObjClosure* closure = this->current_frame->closure;
            disassembleInstruction(
                &closure->function->chunk,
                (int)(this->current_frame->ip - closure->function->chunk.code.elems)
            );
            std::cout << "\t\t" << stack_str << "\n";
        #endif

        uint8_t instruction;
        switch (instruction = this->read_byte()) {
            case OP_RETURN: {
                Value result = this->pop();
                this->frame_count--;
                if (frame_count == 0) {
                    this->pop();
                    return INTERPRET_OK;
                }
                this->stack_top = current_frame->slots - 1;
                this->push(result);
                this->current_frame = &this->frames[this->frame_count - 1];
                break;
            }
            case OP_POP: { this->pop(); break; }
            case OP_PRINT: {
                printf("> ");
                this->pop().print();
                printf("\n");
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t jump_offset = this->read_short();
                Value condition = this->peek(0);
                if (!condition.is_bool()) {
                    this->runtime_error("Expression must evaluate to a boolean.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                if (!condition.as_bool()) { this->current_frame->ip += jump_offset; }
                break;
            }
            case OP_JUMP_IF_TRUE: {
                uint16_t jump_offset = this->read_short();
                Value condition = this->peek(0);
                if (!condition.is_bool()) {
                    this->runtime_error("Expression must evaluate to a boolean.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                if (condition.as_bool()) { this->current_frame->ip += jump_offset; }
                break;
            }
            case OP_JUMP: {
                uint16_t jump_offset = this->read_short();
                this->current_frame->ip += jump_offset;
                break;
            }
            case OP_LOOP: {
                uint16_t jump_offset = this->read_short();
                this->current_frame->ip -= jump_offset;
                break;
            }
            case OP_DEFINE_GLOBAL: {
                Value name_const = this->read_constant();
                if (!name_const.is_string()) {
                    this->runtime_error("Invalid variable name.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjString* name = name_const.as_string();
                this->globals.insert(name, this->peek(0));
                this->pop();
                break;
            }
            case OP_SET_GLOBAL: {
                Value name_const = this->read_constant();
                if (!name_const.is_string()) {
                    this->runtime_error("Invalid variable name.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjString* name = name_const.as_string();
                if (this->globals.insert(name, this->peek(0))) {
                    this->globals.pop(name);
                    this->runtime_error("Undefined variable setter '%s'.", name->as.chars);
                    return INTERPRET_RUNTIME_ERROR;
                };
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = this->read_constant().as_string();
                Value val;
                if (!this->globals.get(name, &val)) {
                    this->runtime_error("Use of undefined variable: '%s'.", name->as.chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                this->push(val);
                break;
            }
            case OP_CALL: {
                uint8_t arg_count = this->read_byte();
                Value fxn_const = this->stack_top[-arg_count - 1];
                if (!fxn_const.is_closure() && !fxn_const.is_native()) {
                    this->runtime_error("Object is not callable.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                // call_const(fxn_const);
                if (!call_const(fxn_const, arg_count)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_CLOSURE: {
                Value fxn_const = this->read_constant();
                ObjFunction* fxn = fxn_const.as_function();
                ObjClosure* closure = ObjClosure::heap_init(fxn);
                push(Value(closure));
                break;
            }
            case OP_GET_LOCAL: {
                this->push(this->read_var());
                break;
            }
            case OP_SET_LOCAL: {
                this->current_frame->slots[this->read_byte()] = this->peek(0);
                break;
            }
            case OP_NEGATE: {
                Value value = this->peek(0);
                if (!value.is_number()) {
                    this->runtime_error("Negation operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                value.as.number *= -1;
                break;
            }
            case OP_ADD: {
                Value b = this->pop();
                Value a = this->pop();
                if (a.is_string() && b.is_string()) {
                    ObjString* a_str = (ObjString*)a.as.obj;
                    ObjString* b_str = (ObjString*)b.as.obj;
                    ObjString* concat = Parser::copy_string_owned(*a_str + b_str, a_str->length + b_str->length);
                    this->push(Value(concat));
                } else if (b.is_number() || a.is_number()) {
                    this->push(Value(a.as.number + b.as.number));
                } else {
                    this->runtime_error("Both binary operands must be either numbers or strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SUBTRACT: { BINARY_OP(number, -); break; }
            case OP_MULTIPLY: { BINARY_OP(number, *); break; }
            case OP_DIVIDE: { BINARY_OP(number, /); break; }
            case OP_TERNARY: {
                Value condition = this->peek(2);
                if (!condition.is_bool()) {
                    this->runtime_error("Ternary conditional must evaluate to a boolean.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                if (condition.as_bool() == 1) {
                    condition = this->peek(1);
                } else {
                    condition = this->peek(0);
                }
                this->stack_top -= 2;
                break;
            }
            case OP_CONSTANT: {
                Value constant = this->read_constant();
                this->push(constant);
                break;
            }
            case OP_CONSTANT_LONG: {
                Value constant = this->read_constant_long();
                this->push(constant);
                break;
            }
            case OP_TRUE: {
                Value constant = Value(true);
                this->push(constant);
                break;
            }
            case OP_FALSE: {
                Value constant = Value(false);
                this->push(constant);
                break;
            }
            case OP_NIL: {
                Value constant = Value();
                this->push(constant);
                break;
            }
            case OP_NOT: {
                Value* value = this->peek_ptr(0);
                if (!value->is_bool()) {
                    this->runtime_error("Logical not operand must evaluate to a boolean.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                value->as.boolean = !value->as.boolean;
                break;
            }
            case OP_EQUAL: {
                Value* b = this->peek_ptr(0);
                Value* a = this->peek_ptr(1);
                if (a->type != b->type) { a->type = VAL_BOOL; a->as.boolean = false; }
                a->type = VAL_BOOL;
                switch (b->type) {
                    case VAL_BOOL: { a->as.boolean = a->as.boolean == b->as.boolean; break; }
                    case VAL_NIL: { a->as.boolean = true; break; }
                    case VAL_NUMBER: { a->as.boolean = a->as.number == b->as.number; break; }
                    case VAL_OBJ: {
                        switch (a->as.obj->type) {
                            case OBJ_STRING: {
                                a->as.boolean = a->as.obj == b->as.obj;
                                break;
                            } default: {
                                a->as.boolean = a->as.obj->compare(b->as.obj);
                                // break;
                            }
                        }
                        // a->as.boolean = a->as.obj->compare(b->as.obj); break;
                    }
                }
                this->pop();
                break;
            }
            case OP_LESS: {
                Value* b = this->peek_ptr(0);
                Value* a = this->peek_ptr(1);
                if (a->type != VAL_NUMBER || b->type != VAL_NUMBER) {
                    a->type = VAL_BOOL;
                    a->as.boolean = false;
                    this->pop();
                    break;
                }
                a->type = VAL_BOOL;
                a->as.boolean = a->as.number < b->as.number;
                this->pop();
                break;
            }
            case OP_GREATER: {
                Value* b = this->peek_ptr(0);
                Value* a = this->peek_ptr(1);
                if (a->type != VAL_NUMBER || b->type != VAL_NUMBER) {
                    a->type = VAL_BOOL;
                    a->as.boolean = false;
                    this->pop();
                    break;
                }
                a->type = VAL_BOOL;
                a->as.boolean = a->as.number > b->as.number;
                this->pop();
                break;
            }
        }
    }
}

VM vm;

#undef BINARY_OP

#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"


static ParseRule rules[44] = {
    [TOKEN_LEFT_PAREN]    = {grouping,   call,    PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL,       NULL,    PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,       NULL,    PREC_NONE}, 
    [TOKEN_RIGHT_BRACE]   = {NULL,       NULL,    PREC_NONE},
    [TOKEN_COMMA]         = {NULL,       NULL,    PREC_NONE},
    [TOKEN_DOT]           = {NULL,       NULL,    PREC_NONE},
    [TOKEN_MINUS]         = {unary,      binary,  PREC_TERM},
    [TOKEN_PLUS]          = {NULL,       binary,  PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,       NULL,    PREC_NONE},
    [TOKEN_SLASH]         = {NULL,       binary,  PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,       binary,  PREC_FACTOR},
    [TOKEN_QUESTION]      = {NULL,       ternary, PREC_TERNARY},
    [TOKEN_COLON]         = {NULL,       NULL,    PREC_NONE},
    [TOKEN_BANG]          = {unary,      NULL,    PREC_UNARY},
    [TOKEN_BANG_EQUAL]    = {NULL,       binary,  PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,       NULL,    PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,       binary,  PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,       binary,  PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,       binary,  PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,       binary,  PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,       binary,  PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable,   NULL,    PREC_NONE},
    [TOKEN_STRING]        = {string,     NULL,    PREC_NONE},
    [TOKEN_NUMBER]        = {number,     NULL,    PREC_NONE},
    [TOKEN_AND]           = {NULL,       _and,    PREC_AND},
    [TOKEN_CLASS]         = {NULL,       NULL,    PREC_NONE},
    [TOKEN_ELSE]          = {NULL,       NULL,    PREC_NONE},
    [TOKEN_FALSE]         = {literal,    NULL,    PREC_NONE},
    [TOKEN_FOR]           = {NULL,       NULL,    PREC_NONE},
    [TOKEN_FUN]           = {NULL,       NULL,    PREC_NONE},
    [TOKEN_IF]            = {NULL,       NULL,    PREC_NONE},
    [TOKEN_NIL]           = {literal,    NULL,    PREC_NONE},
    [TOKEN_OR]            = {NULL,       _or,     PREC_OR},
    [TOKEN_PRINT]         = {NULL,       NULL,    PREC_NONE},
    [TOKEN_RETURN]        = {NULL,       NULL,    PREC_NONE},
    [TOKEN_SUPER]         = {NULL,       NULL,    PREC_NONE},
    [TOKEN_THIS]          = {NULL,       NULL,    PREC_NONE},
    [TOKEN_TRUE]          = {literal,    NULL,    PREC_NONE},
    [TOKEN_LET]           = {NULL,       NULL,    PREC_NONE},
    [TOKEN_MUT]           = {NULL,       NULL,    PREC_NONE},
    [TOKEN_WHILE]         = {NULL,       NULL,    PREC_NONE},
    [TOKEN_CONTINUE]      = {NULL,       NULL,    PREC_NONE},
    [TOKEN_ERROR]         = {NULL,       NULL,    PREC_NONE},
    [TOKEN_EOF]           = {NULL,       NULL,    PREC_NONE},
};

Compiler::Compiler(FunctionType type, Compiler* enclosing) {
    function = NULL;
    this->enclosing = enclosing;
    this->type = type;
    local_count = 0;
    scope_depth = 0;
    function = ObjFunction::heap_init();
}


static ParseRule* get_rule(TokenType type) {
    return &rules[type];
}


Parser::Parser(const char* source) {
    panicking = false;
    source = source;
    had_err = false;
    scanner.init_source(source);
    continue_offsets.initArray();
    compiler = new Compiler(TYPE_SCRIPT, NULL);
    Local* local = &compiler->locals[compiler->local_count++];
    local->depth = 0;
    local->name.start = "";
    local->name.length = 0;
}

void Parser::free() {
    compiler->function->free();
}

Chunk* Parser::current_chunk() {
    return &compiler->function->chunk;
}

int Parser::emit_jump(uint8_t instruction) {
    emitByte(instruction);
    emitBytes(0xff, 0xff);
    return current_chunk()->code.count - 2;
}

int Parser::jump_guard(int offset) {
    int jump = current_chunk()->code.count - offset - 2;
    if (jump > UINT16_MAX) {
        error_at(&current, "Maximum jump instruction limit reached.");
    }
    return jump;
}

void Parser::emit_jump(uint8_t instruction, int offset) {
    int jump = jump_guard(offset);
    emitByte(instruction);
    emitBytes((jump >> 8) & 0xff, jump & 0xff);
}

void Parser::patch_jump(uint8_t offset) {
    int jump = jump_guard(offset);
    current_chunk()->code.elems[offset] = (jump >> 8) & 0xff;
    current_chunk()->code.elems[offset + 1] = jump & 0xff;
}

void Parser::emitByte(uint8_t byte) {
    current_chunk()->writeChunk(byte, previous.line);
}

void Parser::emitBytes(uint8_t byte1, uint8_t byte2) {
    current_chunk()->writeChunk(byte1, previous.line);
    current_chunk()->writeChunk(byte2, previous.line);
}

unsigned Parser::emitConstant(Value value) {
    return compiler->function->writeConstant(value, previous.line); // TODO: This can error if constant table is full
}

unsigned Parser::makeConstant(Value value) {
    return compiler->function->addConstant(value); // TODO: This can error if constant table is full
}

void Parser::emitReturn() {
    emitByte(OP_NIL);
    emitByte(OP_RETURN);
}

ObjFunction* Parser::end_compilation() {
    emitReturn(); // TODO: This fires even when we've written an explicit return statement
    ObjFunction* function = this->compiler->function;
    if (this->compiler->enclosing != NULL) {
        this->compiler = this->compiler->enclosing;
    }
    return function;
}

ObjFunction* Parser::compile() {
    advance();
    while (!match(TOKEN_EOF)) {
        declaration(this);
    }
    ObjFunction* function = end_compilation();
    return had_err ? NULL : function;
}

void Parser::consume(TokenType type, const char* err_text) {
    if (current.type != type) { return error_at(&current, err_text); }
    advance();
}

void Parser::error_at(Token* token, const char* message) {
    if (panicking) { return; }
    panicking = true;
    fprintf(stderr, "[line %d] Error", token->line);
    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {

    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }
    fprintf(stderr, ": %s\n", message);
    had_err = true;
}

void Parser::sync() {
    panicking = false;
    while (current.type != TOKEN_EOF) {
        if (previous.type == TOKEN_SEMICOLON) { return; }
        switch (current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_LET:
            case TOKEN_MUT:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
        }
        advance();
    }
}

void Parser::advance() {
    previous = current;
    for (;;) {
        current = scanner.scan_token();
        if (current.type != TOKEN_ERROR) { break; }
        error_at(&current, "Error while lexing");
    }
}

bool Parser::next_is(TokenType type) { return current.type == type; }

bool Parser::match(TokenType type) {
    if (!next_is(type)) { return false; }
    advance();
    return true;
}

uint8_t Parser::identifier_const(Token* name) {
    Value val = Value(Parser::copy_string(name->start, name->length));
    return makeConstant(val);
}

void Parser::declare_variable(bool can_mut) {
    if (compiler->scope_depth == 0) { return; }
    Token* name = &previous;
    for (int i = compiler->local_count - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (local->depth != -1 && local->depth < compiler->scope_depth) {
            break;
        }
        if (name->compare(&local->name)) {
            error_at(name, "Redeclared variable in same scope.");
        }
    }
    add_local(*name, can_mut);
}

void Parser::add_local(Token name, bool can_mut) {
    if (compiler->local_count == UINT8_COUNT) {
        error_at(&name, "Exceeded local variable limit of 256.");
        return;
    }
    Local* local = &compiler->locals[compiler->local_count++];
    local->name = name;
    local->depth = -1;
    local->can_mut = can_mut;
}

uint8_t Parser::parse_variable(const char* err_text, bool can_mut) {
    consume(TOKEN_IDENTIFIER, err_text);
    declare_variable(can_mut);
    if (compiler->scope_depth > 0) return 0;
    return identifier_const(&previous);
}

void Parser::init_local() {
    if (compiler->scope_depth == 0) { return; }
    Local* local = &compiler->locals[compiler->local_count - 1];
    local->depth = compiler->scope_depth;
}

void Parser::define_variable(uint8_t global) {
    if (compiler->scope_depth > 0) {
        init_local();
        return;
    }
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static void declaration(Parser* parser) {
    if (parser->match(TOKEN_LET) || parser->match(TOKEN_MUT)) { var_decl(parser); }
    else if (parser->match(TOKEN_FUN)) { fxn_decl(parser); }
    else { statement(parser); }
    if (parser->panicking) { parser->sync(); }
}

static void var_decl(Parser* parser) {
    bool can_mut = parser->previous.type == TOKEN_MUT;
    uint8_t global = parser->parse_variable("Expected variable name.", can_mut);
    if (parser->match(TOKEN_EQUAL)) {
        expression(parser);
    } else {
        parser->emitByte(OP_NIL);
    }
    parser->consume(TOKEN_SEMICOLON, "Expected ';' after variable declaration");
    parser->define_variable(global);
}

static void fxn_decl(Parser* parser) {
    uint8_t global = parser->parse_variable("Expected function name.", false);
    parser->init_local();
    parser->function();
    parser->define_variable(global);
}

void Parser::function() {
    Compiler* cmp = new Compiler(TYPE_FUNCTION, this->compiler);
    cmp->function->name = copy_string(previous.start, previous.length);
    this->compiler = cmp;
    nest();
    consume(TOKEN_LEFT_PAREN, "Expected '(' after function name.");
    if (!next_is(TOKEN_RIGHT_PAREN)) {
        do {
            this->compiler->function->arity++;
            if (this->compiler->function->arity > 255) {
                error_at(&current, "Exceeded parameter limit of 255.");
            }
            uint8_t constant = parse_variable("Expected parameter name.", false);
            define_variable(constant);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after parameter list.");
    consume(TOKEN_LEFT_BRACE, "Expected '{' after parameter list.");
    block(this);

    ObjFunction* function = end_compilation();
    unsigned int const_idx = makeConstant(Value(function));
    emitBytes(OP_CLOSURE, const_idx);
}

static void statement(Parser* parser) {
    if (parser->match(TOKEN_PRINT)) { print_stmt(parser); }
    else if (parser->match(TOKEN_CONTINUE)) { continue_stmt(parser); }
    else if (parser->match(TOKEN_IF)) { if_stmt(parser); }
    else if (parser->match(TOKEN_WHILE)) { while_loop(parser); }
    else if (parser->match(TOKEN_FOR)) { for_loop(parser); }
    else if (parser->match(TOKEN_RETURN)) { return_stmt(parser); }
    else if (parser->match(TOKEN_LEFT_BRACE)) {
        parser->nest();
        block(parser);
        parser->unnest();
    }
    else { expr_stmt(parser); }
}

void Parser::nest() {
    compiler->scope_depth++;
}

static void block(Parser* parser) {
    while (!parser->next_is(TOKEN_RIGHT_BRACE) && !parser->next_is(TOKEN_EOF)) {
        declaration(parser);
    }
    parser->consume(TOKEN_RIGHT_BRACE, "Expected '}' block terminator");
}

void Parser::unnest() {
    compiler->scope_depth--;
    while (
        compiler->local_count > 0 &&
        compiler->locals[compiler->local_count - 1].depth > compiler->scope_depth
    ) {
        emitByte(OP_POP);
        compiler->local_count--;
    } // TODO: optimize to a OP_POPN
}

static void return_stmt(Parser* parser) {
    if (parser->compiler->type == TYPE_SCRIPT) {
        parser->error_at(&parser->current, "Can't return from top-level code.");
    }
    if (parser->match(TOKEN_SEMICOLON)) {
        parser->emitReturn();
    } else {
        expression(parser);
        parser->consume(TOKEN_SEMICOLON, "Expected ';' return statement");
        parser->emitByte(OP_RETURN);
    }
}

static void continue_stmt(Parser* parser) {
    int len_offsets = parser->continue_offsets.count;
    if (len_offsets == 0) {
        parser->error_at(&parser->current, "Continue statement used outside of loop.");
        return;
    }
    int offset = parser->continue_offsets[len_offsets - 1];
    parser->emit_jump(OP_LOOP, offset - 5);
}

static void print_stmt(Parser* parser) {
    expression(parser);
    parser->consume(TOKEN_SEMICOLON, "Expected ';' at after print.");
    parser->emitByte(OP_PRINT);
}

static void while_loop(Parser* parser) {
    int loop_offset = parser->current_chunk()->code.count;
    expression(parser);
    int end_jump = parser->emit_jump(OP_JUMP_IF_FALSE);
    parser->emitByte(OP_POP);
    statement(parser);
    parser->emit_jump(OP_LOOP, loop_offset - 5);
    parser->patch_jump(end_jump);
    parser->emitByte(OP_POP);
}

static void for_loop(Parser* parser) {
    if (!parser->match(TOKEN_SEMICOLON)) { // Initializers
        if (parser->match(TOKEN_MUT) || parser->match(TOKEN_LET)) {
            var_decl(parser);
        } else {
            expr_stmt(parser);
        }
    }
    bool has_conditional = false;
    int cond_true_jump;
    int cond_false_jump;
    int cond_offset = parser->current_chunk()->code.count; // Conditional loop offset
    if (!parser->match(TOKEN_SEMICOLON)) {
        has_conditional = true;
        expression(parser); // Conditional
        parser->consume(TOKEN_SEMICOLON, "Expected ';' at after for condition.");
        cond_true_jump = parser->emit_jump(OP_JUMP_IF_TRUE); // Jump if true to start of loop
        cond_false_jump = parser->emit_jump(OP_JUMP_IF_FALSE); // Jump if false to end of loop
    } else {
        cond_true_jump = parser->emit_jump(OP_JUMP); // Jump to start of loop
    }
    bool has_incrementor = false;
    int inc_offset = parser->current_chunk()->code.count; // Incrementor loop offset
    if (!parser->next_is(TOKEN_LEFT_BRACE)) {
        has_incrementor = true;
        expression(parser); // Incrementor
        parser->emitByte(OP_POP);
    }
    if (has_conditional) {
        parser->emit_jump(OP_LOOP, cond_offset - 5); // Jump back to conditional loop offset
    }
    int start_offset = parser->current_chunk()->code.count; // Start of loop offset
    // Fill in cond true jump for true and missing conditionals
    parser->patch_jump(cond_true_jump);
    if (has_conditional) { parser->emitByte(OP_POP); } // Pop bool for cond true jump
    parser->continue_offsets.writeElement(inc_offset);
    statement(parser);
    parser->continue_offsets.pop();
    if (has_incrementor) {
        parser->emit_jump(OP_LOOP, inc_offset - 5); // Jump to incrementor
        if (has_conditional) {
            parser->patch_jump(cond_false_jump);
            parser->emitByte(OP_POP); // Pop bool for cond false jump
        }
    } else if (has_conditional) {
        parser->emit_jump(OP_LOOP, cond_offset - 5); // Jump to conditional
        parser->patch_jump(cond_false_jump); // Fill in false jump here, all other options are infinite loops
        parser->emitByte(OP_POP); // Pop bool for cond false jump
    } else {
        parser->emit_jump(OP_LOOP, start_offset - 5); // Jump to loop start
    }
    // End of loop offset
}

static void if_stmt(Parser* parser) {
    expression(parser);
    int then_jump = parser->emit_jump(OP_JUMP_IF_FALSE);
    parser->emitByte(OP_POP);
    statement(parser);

    if (parser->match(TOKEN_ELSE)) {
        int else_jump = parser->emit_jump(OP_JUMP);
        parser->patch_jump(then_jump);
        parser->emitByte(OP_POP);
        statement(parser);
        parser->patch_jump(else_jump);
    } else {
        parser->patch_jump(then_jump);
        parser->emitByte(OP_POP);
    }
}

static void expr_stmt(Parser* parser) {
    expression(parser);
    parser->consume(TOKEN_SEMICOLON, "Expected ';' at end of statement.");
    parser->emitByte(OP_POP);
}

static void expression(Parser* parser) {
    parser->parse_precedence(PREC_ASSIGNMENT);
}

static void number(Parser* parser, bool can_assign) {
    double value = strtod(parser->previous.start, NULL);
    parser->emitConstant(Value(value));
}

static void literal(Parser* parser, bool can_assign) {
    switch (parser->previous.type) {
        case TOKEN_TRUE: { parser->emitByte(OP_TRUE); break; }
        case TOKEN_FALSE: { parser->emitByte(OP_FALSE); break; }
        case TOKEN_NIL: { parser->emitByte(OP_NIL); break; }
    }
}

static void variable(Parser* parser, bool can_assign) {
    named_variable(parser, parser->previous, can_assign);
}

// TODO: we could get an efficiently gain by switching resolution to a hashmap
// Names would correspond to a stack of { idx, depth }
// We would need to keep track of the set of names added in current scope
// Otherwise we would need to visit entire hashmap on unnest
int Parser::resolve_local(Token name) {
    for (int i = compiler->local_count - 1; i >= 0; i--) {
        Local* local = compiler->locals + i;
        if (name.compare(&local->name)) {
            if (local->depth == -1) {
                error_at(&name, "Can't initialize a variable with itself");
            }
            return i;
        }
    }
    return -1;
}

static void named_variable(Parser* parser, Token name, bool can_assign) {
    uint8_t get_op, set_op;
    int arg = parser->resolve_local(name);
    bool can_mut = true;
    if (arg == -1) {
        // TODO: immutable globals
        arg = parser->identifier_const(&name);
        get_op = OP_GET_GLOBAL;
        set_op = OP_SET_GLOBAL;
    } else {
        can_mut = parser->compiler->locals[arg].can_mut;
        get_op = OP_GET_LOCAL;
        set_op = OP_SET_LOCAL;
    }

    if (can_assign && parser->match(TOKEN_EQUAL)) {
        if (!can_mut) {
            parser->error_at(&name, "Assignment made to immutable variable.");
        }
        expression(parser);
        parser->emitBytes(set_op, arg);
    } else {
        parser->emitBytes(get_op, arg);
    }
}

static void string(Parser* parser, bool can_assign) {
    Value value = Value(Parser::copy_string(parser->previous.start + 1, parser->previous.length - 2));
    parser->emitConstant(value);
}

ObjString* Parser::copy_string(const char* chars, int length) {
    uint32_t hash = ObjString::hasher(chars, length);
    ObjString* interned = vm.strings.lookup_by_chars(chars, hash, length);
    if (interned != NULL) { return interned; }

    ObjString* new_str = ObjString::heap_init(length, chars, hash);
    new_str->next = vm.objects;
    vm.objects = new_str;
    vm.strings.insert(new_str, Value());
    return new_str;
}

ObjString* Parser::copy_string_owned(char* chars, int length) {
    uint32_t hash = ObjString::hasher(chars, length);
    ObjString* interned = vm.strings.lookup_by_chars(chars, hash, length);
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length);
        return interned;
    }
    ObjString* new_str = ObjString::heap_init_owned(length, chars, hash);
    new_str->next = vm.objects;
    vm.objects = new_str;
    vm.strings.insert(new_str, Value());
    return new_str;
}

static void grouping(Parser* parser, bool can_assign) {
    expression(parser);
    parser->consume(TOKEN_RIGHT_PAREN, "Expected ')' after expression.");
}

static uint8_t arg_list(Parser* parser) {
    uint8_t c = 0;
    if (!parser->next_is(TOKEN_RIGHT_PAREN)) {
        do {
            expression(parser);
            if (c == 255) {
                parser->error_at(&parser->current, "Argument limit of 255 reached.");
            }
            c++;
        } while (parser->match(TOKEN_COMMA));
    }
    parser->consume(TOKEN_RIGHT_PAREN, "Expected ')' after argument list.");
    return c;
}

static void call(Parser* parser, bool can_assign) {
    uint8_t arg_count = arg_list(parser);
    parser->emitBytes(OP_CALL, arg_count);
}

static void _or(Parser* parser, bool can_assign) {
    int or_jump = parser->emit_jump(OP_JUMP_IF_TRUE);
    parser->emitByte(OP_POP);
    parser->parse_precedence(PREC_AND);
    parser->patch_jump(or_jump);
}

static void _and(Parser* parser, bool can_assign) {
    int and_jump = parser->emit_jump(OP_JUMP_IF_FALSE);
    parser->emitByte(OP_POP);
    parser->parse_precedence(PREC_EQUALITY);
    parser->patch_jump(and_jump);
}

static void unary(Parser* parser, bool can_assign) {
    TokenType op_type = parser->previous.type;
    parser->parse_precedence(PREC_UNARY);
    switch (op_type) {
        case TOKEN_MINUS: { parser->emitByte(OP_NEGATE); break; }
        case TOKEN_BANG: { parser->emitByte(OP_NOT); break; }
    }
}

static void binary(Parser* parser, bool can_assign) {
    TokenType op_type = parser->previous.type;
    ParseRule* rule = get_rule(op_type);
    parser->parse_precedence((Precedence)(rule->precedence + 1));

    switch (op_type) {
        case TOKEN_PLUS: { parser->emitByte(OP_ADD); break; }
        case TOKEN_MINUS: { parser->emitByte(OP_SUBTRACT); break; }
        case TOKEN_STAR: { parser->emitByte(OP_MULTIPLY); break; }
        case TOKEN_SLASH: { parser->emitByte(OP_DIVIDE); break; }
        case TOKEN_EQUAL_EQUAL: { parser->emitByte(OP_EQUAL); break; }
        case TOKEN_BANG_EQUAL: { parser->emitBytes(OP_EQUAL, OP_NOT); break; }
        case TOKEN_LESS: { parser->emitByte(OP_LESS); break; }
        case TOKEN_LESS_EQUAL: { parser->emitBytes(OP_GREATER, OP_NOT); break; }
        case TOKEN_GREATER: { parser->emitByte(OP_GREATER); break; }
        case TOKEN_GREATER_EQUAL: { parser->emitBytes(OP_LESS, OP_NOT); break; }
    }
}

static void ternary(Parser* parser, bool can_assign) { // TODO: fix with jumps
    TokenType op_type = parser->previous.type;
    ParseRule* rule = get_rule(op_type);
    parser->parse_precedence((Precedence)(rule->precedence + 1));
    parser->consume(TOKEN_COLON, "Expected ':' to complete ternary expression.");
    parser->parse_precedence((Precedence)(rule->precedence + 1));
    parser->emitByte(OP_TERNARY);
}

void Parser::parse_precedence(Precedence precedence) {
    advance();
    ParseFn prefix_rule = get_rule(previous.type)->prefix;
    if (prefix_rule == NULL) {
        return error_at(&current, "Expected expression.");
    }
    bool can_assign = precedence <= PREC_ASSIGNMENT;
    prefix_rule(this, can_assign);

    while (precedence <= get_rule(current.type)->precedence) {
        advance();
        ParseFn infix_rule = get_rule(previous.type)->infix;
        infix_rule(this, can_assign);
    }
    if (can_assign && match(TOKEN_EQUAL)) {
        error_at(&previous, "Invalid assignment target.");
    }
}
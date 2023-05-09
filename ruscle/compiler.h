#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "scanner.h"
#include "value.h"
#include "array.h"
#include "memory.h"
#include "vm.h"

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_TERNARY,     // ?:
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
} Precedence;

typedef struct {
    Token name;
    int depth;
    bool can_mut;
} Local;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
} FunctionType;

class Compiler {
    public:
        struct Compiler* enclosing;
        ObjFunction* function;
        FunctionType type;

        Local locals[UINT8_COUNT];
        int local_count;
        int scope_depth;

        // Compiler(FunctionType type);
        Compiler() = default;
        Compiler(FunctionType type, Compiler* enclosing);
};

class Parser {
    public:
        const char* source;
        bool panicking;
        bool had_err;
        Token previous;
        Token current;
        Compiler* compiler;
        Array<int> continue_offsets;

        Parser(const char* source);
        Chunk* current_chunk();
        void free();
        int emit_jump(uint8_t byte);
        int jump_guard(int offset);
        void emit_jump(uint8_t instruction, int offset);
        void patch_jump(uint8_t instruction);
        void function();
        void nest();
        void unnest();
        void sync();
        ObjFunction* compile();
        void consume(TokenType type, const char *err_text);
        void advance();
        bool next_is(TokenType type);
        bool match(TokenType type);
        uint8_t identifier_const(Token *name);
        int resolve_local(Token name);
        void parse_precedence(Precedence precedence);
        void error_at(Token* token, const char* message);
        void emitByte(uint8_t byte);
        // void emit_jump(uint8_t byte);
        void emitReturn();
        ObjFunction *end_compilation();
        void emitBytes(uint8_t byte1, uint8_t byte2);

        unsigned emitConstant(Value value);
        unsigned makeConstant(Value value);
        void define_variable(uint8_t global);
        void declare_variable(bool can_mut);
        void init_local();

        void add_local(Token name, bool can_mut);
        uint8_t parse_variable(const char *err_text, bool can_mut);
        static ObjString *copy_string(const char *chars, int length);
        static ObjString* copy_string_owned(char* chars, int length);
};

typedef void (*ParseFn)(Parser* parser, bool can_assign);
typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static void literal(Parser* parser, bool can_assign);
static void named_variable(Parser *parser, Token name, bool can_assign);
static void string(Parser *parser, bool can_assign);
static void number(Parser* parser, bool can_assign);
static void declaration(Parser *parser);
static void var_decl(Parser *parser);
static void fxn_decl(Parser *parser);
static void statement(Parser *parser);
static void expression(Parser *parser);
static void for_loop(Parser *parser);
static void grouping(Parser *parser, bool can_assign);
static void call(Parser *parser, bool can_assign);
static void return_stmt(Parser* parser);
static void unary(Parser *parser, bool can_assign);
static void while_loop(Parser *parser);
static void _or(Parser* parser, bool can_assign);
static void _and(Parser* parser, bool can_assign);
static void print_stmt(Parser *parser);
static void if_stmt(Parser *parser);
static void expr_stmt(Parser *parser);
static void binary(Parser *parser, bool can_assign);
static void ternary(Parser* parser, bool can_assign);
static void variable(Parser* parser, bool can_assign);
static void block(Parser *parser);
static void continue_stmt(Parser *parser);

static ParseRule* get_rule(TokenType type);


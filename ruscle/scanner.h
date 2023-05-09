#pragma once

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>

#include "common.h"

typedef enum {
  // Single-character tokens.
  TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
  TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
  TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
  TOKEN_QUESTION, TOKEN_COLON,
  // One or two character tokens.
  TOKEN_BANG, TOKEN_BANG_EQUAL,
  TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER, TOKEN_GREATER_EQUAL,
  TOKEN_LESS, TOKEN_LESS_EQUAL,
  // Literals.
  TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
  // Keywords.
  TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
  TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
  TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
  TOKEN_TRUE, TOKEN_LET, TOKEN_MUT, TOKEN_WHILE,
  TOKEN_CONTINUE,

  TOKEN_ERROR, TOKEN_EOF
} TokenType;

class Token {
    public:
        int line;
        TokenType type;
        int length;
        const char* start;
        Token();
        Token(int line, TokenType type, int length, const char* start);
        bool compare(Token* token);
};

class Scanner {
    public:
        const char* start;
        const char* current;
        int line;

        void init_source(const char* source);
        bool is_end();
        Token make_token(TokenType type);
        inline char advance();
        inline char peek();
        inline char peekNext();
        bool match(char match_char);
        Token error_token(const char* text);
        void skip_whitespace();
        TokenType suffix_match(const char* suffix, uint8_t start, uint8_t suffix_len, TokenType type);
        TokenType match_kw();
        Token scan_token();
};

extern Scanner scanner;
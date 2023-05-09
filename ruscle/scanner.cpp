
#include "scanner.h"

Token::Token() {
    this->line = 0;
    this->type = TOKEN_ERROR;
    this->length = 0;
    this->start = NULL;
}
Token::Token(int line, TokenType type, int length, const char* start) {
    this->line = line;
    this->type = type;
    this->length = length;
    this->start = start;
}
bool Token::compare(Token* token) {
    if (this->length != token->length || memcmp(this->start, token->start, this->length) != 0) {
        return false;
    }
    return true;
}


void Scanner::init_source(const char* source) {
    this->start = source;
    this->current = source;
    this->line = 1;
}

bool Scanner::is_end() {
    return *this->current == '\0';
}

Token Scanner::make_token(TokenType type) {
    return Token(
        this->line,
        type,
        this->current - this->start,
        this->start
    );
}

char Scanner::advance() {
    return *this->current++;
}

char Scanner::peek() {
    return *this->current;
}

char Scanner::peekNext() {
    if (this->is_end()) { return '\0'; }
    return this->current[1];
}

bool Scanner::match(char match_char) {
    if (this->peek() != match_char) { return false; }
    this->current++;
    return true;
}

Token Scanner::error_token(const char* text) {
    return Token(this->line, TOKEN_ERROR, strlen(text), text);
}

void Scanner::skip_whitespace() {
    for (;;) {
        switch (this->peek()) {
            case ' ': case '\r': case '\t':
                this->advance();
                break;
            case '\n':
                this->line++;
                this->current++;
                break;
            case '/':
                this->line++;
                this->current++;
                break;
            default: return;
        }
    }
}

TokenType Scanner::suffix_match(const char* suffix, uint8_t start, uint8_t suffix_len, TokenType type) {
    uint8_t len = this->current - this->start - start;
    if (len != suffix_len || memcmp(this->start+start, suffix, suffix_len) != 0) { return TOKEN_IDENTIFIER; }
    return type;
}

TokenType Scanner::match_kw() {
    switch (this->start[0]) {
        case 'a': { return this->suffix_match("nd", 1, 2, TOKEN_AND); }
        case 'c': { return this->suffix_match("lass", 1, 4, TOKEN_CLASS); }
        case 'e': { return this->suffix_match("lse", 1, 3, TOKEN_ELSE); }
        case 'f': {
            if (this->current - this->start > 1) {
                switch (this->start[1]) {
                    case 'a': return this->suffix_match("lse", 2, 3, TOKEN_FALSE);
                    case 'o': return this->suffix_match("r", 2, 1, TOKEN_FOR);
                    case 'x': return this->suffix_match("n", 2, 1, TOKEN_FUN);
                }
            }
            break;
        }
        case 'i': { return this->suffix_match("f", 1, 1, TOKEN_IF); }
        case 'l': { return this->suffix_match("et", 1, 2, TOKEN_LET); }
        case 'm': { return this->suffix_match("ut", 1, 2, TOKEN_MUT); }
        case 'n': { return this->suffix_match("il", 1, 2, TOKEN_NIL); }
        case 'o': { return this->suffix_match("r", 1, 1, TOKEN_OR); }
        case 'p': { return this->suffix_match("rint", 1, 4, TOKEN_PRINT); }
        case 'r': { return this->suffix_match("eturn", 1, 5, TOKEN_RETURN); }
        case 's': { return this->suffix_match("uper", 1, 4, TOKEN_SUPER); }
        case 't': {
            if (this->current - this->start > 1) {
                switch (this->start[1]) {
                    case 'h': return this->suffix_match("is", 2, 2, TOKEN_THIS);;
                    case 'r': return this->suffix_match("ue", 2, 2, TOKEN_TRUE);;
                }
            }
            break;
        }
        case 'w': { return this->suffix_match("hile", 1, 4, TOKEN_WHILE); }
    }
    return TOKEN_IDENTIFIER;
}

Token Scanner::scan_token() {
    this->skip_whitespace();
    this->start = this->current;
    if (this->is_end()) {
        return this->make_token(TOKEN_EOF);
    }
    char c = this->advance();
    if (isdigit(c)) {
        c = this->peek();
        while (isdigit(c)) { this->advance(); c = this->peek(); }
        if (this->match('.')) {
            if (!isdigit(c)) {
                return this->error_token("Number literal missing mantissa.");
            }
            while (isdigit(c)) { this->advance(); c = this->peek(); }
        }
        return this->make_token(TOKEN_NUMBER);
    }
    if (isalpha(c) || c == '_') {
        c = this->peek();
        while (isalnum(c) || c == '_') { this->advance(); c = this->peek(); }
        return this->make_token(this->match_kw());
    }
    switch (c) {
        // 1 char tokens
        case '(': { return this->make_token(TOKEN_LEFT_PAREN); }
        case ')': { return this->make_token(TOKEN_RIGHT_PAREN); }
        case '{': { return this->make_token(TOKEN_LEFT_BRACE); }
        case '}': { return this->make_token(TOKEN_RIGHT_BRACE); }
        case ',': { return this->make_token(TOKEN_COMMA); }
        case '.': { return this->make_token(TOKEN_DOT); }
        case '-': { return this->make_token(TOKEN_MINUS); }
        case '+': { return this->make_token(TOKEN_PLUS); }
        case ';': { return this->make_token(TOKEN_SEMICOLON); }
        case '/': { return this->make_token(TOKEN_SLASH); }
        case '*': { return this->make_token(TOKEN_STAR); }
        case '?': { return this->make_token(TOKEN_QUESTION); }
        case ':': { return this->make_token(TOKEN_COLON); }
        // 1-2 char tokens
        case '!': {
            TokenType type = this->match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG;
            return this->make_token(type);
        }
        case '=': {
            TokenType type = this->match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL;
            return this->make_token(type);
        }
        case '>': {
            TokenType type = this->match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER;
            return this->make_token(type);
        }
        case '<': {
            TokenType type = this->match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS;
            return this->make_token(type);
        }
        case '"': {
            c = this->peek();
            while (c != '"') {
                if (c == '\n') { this->line++; }
                this->advance();
                c = this->peek();
                if (this->is_end()) { return this->error_token("Unterminated string."); }
            }
            this->advance();
            return this->make_token(TOKEN_STRING);
        }
    }
    return this->error_token("Unexpected character.");
}

Scanner scanner;
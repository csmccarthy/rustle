#pragma once

#include "common.h"
#include "value.h"

typedef struct {
    ObjString* key;
    Value value;
} Entry;

#define TABLE_MAX_LOAD 0.75

class Table {
    public:
        int count;
        int capacity;
        Entry* entries;

        Table();
        static Entry* lookup(Entry* entries, int capacity, ObjString* key, bool is_insert);
        Entry* lookup(ObjString* key, bool is_insert);
        ObjString* lookup_by_chars(const char* chars, uint32_t hash, int length);
        bool get(ObjString* key, Value* value);
        bool pop(ObjString* key);
        bool insert(ObjString* key, Value value);
        void move_entries(Entry* entries);
        void adjust_capacity(int capacity);
        void free();
        void inheritFrom(Table* table);
};
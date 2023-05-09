
#include "table.h"

Table::Table() {
    this->count = 0;
    this->capacity = 0;
    this->entries = NULL;
}
Entry* Table::lookup(Entry* entries, int capacity, ObjString* key, bool is_insert) {
    uint32_t idx = key->hash % capacity;
    // printf("searching...\n");
    for (;;) {
        Entry* entry = entries + idx;
        // key->print();
        // printf("\n");
        // if (entry->key != NULL) { entry->key->print(); printf("%s", entry->key->as.chars); }
        // printf("\n");
        bool is_empty = entry->key == NULL && (is_insert || entry->value.is_nil());
        if (is_empty || *(entry->key) == key) {
            return entry;
        }
        idx = (idx + 1) % capacity;
    }
}
Entry* Table::lookup(ObjString* key, bool is_insert) {
    return lookup(this->entries, this->capacity, key, is_insert);
}
ObjString* Table::lookup_by_chars(const char* chars, uint32_t hash, int length) {
    if (this->capacity == 0) { return NULL; }
    uint32_t idx = hash % this->capacity;
    for (;;) {
        Entry* entry = entries + idx;
        bool is_empty = entry->key == NULL && entry->value.is_nil();
        if (is_empty || entry->key->compare(chars, length, hash)) {
            return entry->key;
        }
        idx = (idx + 1) % this->capacity;
    }
}
bool Table::get(ObjString* key, Value* value) {
    if (this->count == 0) { return false; }
    Entry* entry = this->lookup(key, false);
    if (entry->key == NULL) { return false; }
    *value = entry->value;
    return true;
}
bool Table::pop(ObjString* key) {
    if (this->count == 0) { return false; }
    Entry* entry = this->lookup(key, false);
    if (entry->key == NULL) { return false; }
    entry->key = NULL;
    entry->value = Value(true);
    return true;
}
bool Table::insert(ObjString* key, Value value) {
    if (this->count + 1 > this->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(this->capacity);
        this->adjust_capacity(capacity);
    }
    Entry* entry = this->lookup(key, true);
    bool new_key = entry->key == NULL;
    if (new_key && entry->value.is_nil()) { this->count++; }
    entry->key = key;
    entry->value = value;
    return new_key;
}
void Table::move_entries(Entry* entries) {
    this->count = 0;
    for (int i = 0; i < this->capacity; i++) {
        Entry* entry = this->entries + i;
        if (entry->key == NULL) { continue; }
        Entry* new_entry = lookup(entries, this->capacity, entry->key, false);
        new_entry->key = entry->key;
        new_entry->value = entry->value;
        this->count++;
    }
}
void Table::adjust_capacity(int capacity) {
    Entry* entries = (Entry*)(reallocate(NULL, 0, sizeof(Entry) * capacity));
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = Value();
    }
    this->move_entries(entries);
    FREE_ARRAY(Entry, this->entries, this->count);
    this->entries = entries;
    this->capacity = capacity;
}
void Table::free() {
    FREE_ARRAY(Entry, this->entries, this->capacity);
    this->count = 0;
    this->capacity = 0;
    this->entries = NULL;
}
void Table::inheritFrom(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = table->entries + i;
        if (entry->key == NULL) { continue; }
        this->insert(entry->key, entry->value);
    }
}
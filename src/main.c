#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE 700
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <unistd.h>
#include <limits.h>

/* ============================================================================
 * The Source Programming Language
 * ============================================================================ */

#define MAX_TOKENS 10000
#define MAX_IDENTIFIERS 1000
#define MAX_FUNCTIONS 200
#define MAX_VARIABLES 500
#define MAX_STACK_FRAMES 100
#define MAX_PARAMS 20
#define MAX_STATEMENTS 1000
#define MAX_STRING_LENGTH 1024
#define MAX_ARRAY_SIZE 10000
#define MAX_IMPORTS 50
#define MAX_PATH_LENGTH 512
#define MAX_TASKS 100
#define MAX_ACCESSED_VARS 1000

/* ============================================================================
 * Concurrency System - Task Tracking for Race Detection
 * ============================================================================ */

typedef struct {
    char name[256];
    char accessedVars[MAX_ACCESSED_VARS][256];
    int accessedCount;
    bool isWriting[MAX_ACCESSED_VARS];
} Task;

typedef struct {
    Task tasks[MAX_TASKS];
    int taskCount;
    bool raceDetected;
    char raceTask1[256];
    char raceTask2[256];
    char raceVar[256];
} TaskSystem;

static TaskSystem taskSystem;

/* ============================================================================
 * Compile-Time System
 * ============================================================================ */

typedef struct {
    char name[256];
    int intValue;
    double doubleValue;
    bool boolValue;
    char stringValue[MAX_STRING_LENGTH];
    int type; // 0=int, 1=double, 2=bool, 3=string
} ComptimeValue;

typedef struct {
    ComptimeValue values[MAX_VARIABLES];
    int count;
    char osName[64];
    char archName[64];
} ComptimeContext;

static ComptimeContext comptimeCtx;

/* ============================================================================
 * Import System
 * ============================================================================ */

typedef struct {
    char importedFiles[MAX_IMPORTS][MAX_PATH_LENGTH];
    int importCount;
    char currentDirectory[MAX_PATH_LENGTH];
} ImportTracker;

static ImportTracker importTracker;

/* ============================================================================
 * Real-Time Execution Tracking (v3.1)
 * ============================================================================ */

typedef struct {
    bool inRealTimeContext;
    char currentFunction[256];
} RealTimeContext;

static RealTimeContext rtContext;

/* ============================================================================
 * Token Types
 * ============================================================================ */

typedef enum {
    TOKEN_EOF,
    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_STRING,
    TOKEN_VOID,
    TOKEN_INT,
    TOKEN_DOUBLE,
    TOKEN_STRING_TYPE,
    TOKEN_BOOL,
    TOKEN_STATIC,
    TOKEN_DEF,
    TOKEN_RETURN,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_ELIF,
    TOKEN_WHILE,
    TOKEN_FOR,
    TOKEN_IN,
    TOKEN_RANGE,
    TOKEN_PRINT,
    TOKEN_TRUE,
    TOKEN_FALSE,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_NOT,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_COMMA,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_ASSIGN,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_MULTIPLY,
    TOKEN_DIVIDE,
    TOKEN_MODULO,
    TOKEN_EQUALS,
    TOKEN_NOT_EQUALS,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_PLUS_ASSIGN,
    TOKEN_MINUS_ASSIGN,
    TOKEN_DOT,
    TOKEN_LEN,
    TOKEN_APPEND,
    TOKEN_LIST,
    TOKEN_IMPORT,
    TOKEN_COMPTIME,
    TOKEN_SPAWN,
    TOKEN_AT,           // NEW: @ symbol
    TOKEN_REAL_TIME,    // NEW: real_time keyword
    TOKEN_GLOBAL,       // NEW: global keyword for global variables
} TokenType;

typedef struct {
    TokenType type;
    char lexeme[256];
    double numValue;
    int line;
} Token;

typedef struct {
    char *source;
    int current;
    int line;
    Token tokens[MAX_TOKENS];
    int tokenCount;
} Lexer;

/* ============================================================================
 * Value System with Automatic Memory Management
 * ============================================================================ */

typedef enum {
    VAL_NULL,
    VAL_INT,
    VAL_DOUBLE,
    VAL_BOOL,
    VAL_STRING,
    VAL_LIST,
} ValueType;

typedef struct Value Value;

typedef struct {
    Value *elements;
    int count;
    int capacity;
} List;

struct Value {
    ValueType type;
    union {
        int intVal;
        double doubleVal;
        bool boolVal;
        char stringVal[MAX_STRING_LENGTH];
        List listVal;
    } as;
};

/* ============================================================================
 * Abstract Syntax Tree (AST) Node Types
 * ============================================================================ */

typedef enum {
    EXPR_LITERAL,
    EXPR_VARIABLE,
    EXPR_BINARY,
    EXPR_UNARY,
    EXPR_CALL,
    EXPR_LIST,
    EXPR_INDEX,
    EXPR_RANGE,
    EXPR_COMPTIME,
} ExprType;

typedef struct Expr Expr;

typedef struct {
    Expr *left;
    Token op;
    Expr *right;
} BinaryExpr;

typedef struct {
    Token op;
    Expr *operand;
} UnaryExpr;

typedef struct {
    char name[256];
    Expr **args;
    int argCount;
} CallExpr;

typedef struct {
    Expr **elements;
    int count;
} ListExpr;

typedef struct {
    Expr *list;
    Expr *index;
} IndexExpr;

typedef struct {
    Expr *start;
    Expr *end;
} RangeExpr;

typedef struct {
    Expr *expression;
} ComptimeExpr;

struct Expr {
    ExprType type;
    union {
        Value literal;
        char varName[256];
        BinaryExpr binary;
        UnaryExpr unary;
        CallExpr call;
        ListExpr list;
        IndexExpr index;
        RangeExpr range;
        ComptimeExpr comptime;
    } as;
};

/* ============================================================================
 * Statement Types
 * ============================================================================ */

typedef enum {
    STMT_EXPR,
    STMT_VAR_DECL,
    STMT_ASSIGN,
    STMT_RETURN,
    STMT_IF,
    STMT_WHILE,
    STMT_FOR,
    STMT_BLOCK,
    STMT_PRINT,
    STMT_LIST_APPEND,
    STMT_COMPOUND_ASSIGN,
    STMT_IMPORT,
    STMT_SPAWN,
    STMT_INDEX_ASSIGN,  // NEW: list[index] = value
} StmtType;

typedef struct Stmt Stmt;

typedef struct {
    TokenType varType;
    char name[256];
    Expr *initializer;
    bool isComptime;
    bool isGlobal;  // NEW: Global variable flag
} VarDeclStmt;

typedef struct {
    char name[256];
    Expr *value;
} AssignStmt;

typedef struct {
    char name[256];
    Token op;
    Expr *value;
} CompoundAssignStmt;

typedef struct {
    Expr *condition;
    Stmt *thenBranch;
    Stmt *elseBranch;
    bool isComptime;
} IfStmt;

typedef struct {
    Expr *condition;
    Stmt *body;
} WhileStmt;

typedef struct {
    char varName[256];
    Expr *iterable;
    Stmt *body;
} ForStmt;

typedef struct {
    Stmt **statements;
    int count;
} BlockStmt;

typedef struct {
    char listName[256];
    Expr *value;
} ListAppendStmt;

typedef struct {
    char filename[MAX_PATH_LENGTH];
} ImportStmt;

typedef struct {
    CallExpr call;
} SpawnStmt;

typedef struct {
    char listName[256];
    Expr *index;
    Expr *value;
} IndexAssignStmt;  // NEW: for list[index] = value

struct Stmt {
    StmtType type;
    union {
        Expr *expression;
        VarDeclStmt varDecl;
        AssignStmt assign;
        CompoundAssignStmt compoundAssign;
        Expr *returnValue;
        IfStmt ifStmt;
        WhileStmt whileStmt;
        ForStmt forStmt;
        BlockStmt block;
        Expr *printExpr;
        ListAppendStmt listAppend;
        ImportStmt import;
        SpawnStmt spawn;
        IndexAssignStmt indexAssign;  // NEW
    } as;
};

/* ============================================================================
 * Function Definition
 * ============================================================================ */

typedef struct {
    char name[256];
    TokenType returnType;
    bool isStatic;
    bool isRealTime;  // NEW: @real_time annotation
    char params[MAX_PARAMS][256];
    TokenType paramTypes[MAX_PARAMS];
    int paramCount;
    Stmt *body;
} Function;

/* ============================================================================
 * Runtime Environment
 * ============================================================================ */

typedef struct {
    char name[256];
    Value value;
} Variable;

typedef struct {
    Variable vars[MAX_VARIABLES];
    int count;
} Environment;

typedef struct {
    Environment env;
    Function *function;
    int returnFlag;
    Value returnValue;
} StackFrame;

typedef struct {
    StackFrame frames[MAX_STACK_FRAMES];
    int frameCount;
    Function functions[MAX_FUNCTIONS];
    int functionCount;
    Environment globalEnv;  // NEW: Global variable environment
} Runtime;

static Runtime runtime;

/* Forward declarations */
/* Forward declarations */
void parseFile(const char *filename);
Value evaluateExpr(Expr *expr);
void executeStmt(Stmt *stmt);
void setGlobalVariable(const char *name, Value value);
Value *findVariable(const char *name);
void setVariable(const char *name, Value value);
Function *findFunction(const char *name);

/* ============================================================================
 * Real-Time System Implementation (v3.1)
 * ============================================================================ */

void initRealTimeContext() {
    rtContext.inRealTimeContext = false;
    rtContext.currentFunction[0] = '\0';
}

void enterRealTimeContext(const char *functionName) {
    rtContext.inRealTimeContext = true;
    strncpy(rtContext.currentFunction, functionName, 255);
    rtContext.currentFunction[255] = '\0';
}

void exitRealTimeContext() {
    rtContext.inRealTimeContext = false;
    rtContext.currentFunction[0] = '\0';
}

void checkRealTimeViolation(const char *operation) {
    if (rtContext.inRealTimeContext) {
        fprintf(stderr, "\n============================================\n");
        fprintf(stderr, "REAL-TIME VIOLATION DETECTED!\n");
        fprintf(stderr, "============================================\n");
        fprintf(stderr, "Function: @real_time %s\n", rtContext.currentFunction);
        fprintf(stderr, "Violation: %s\n", operation);
        fprintf(stderr, "\n@real_time functions cannot:\n");
        fprintf(stderr, "  - Allocate memory (no lists, no append)\n");
        fprintf(stderr, "  - Make system calls (no print)\n");
        fprintf(stderr, "  - Block or spawn tasks\n");
        fprintf(stderr, "  - Call non-@real_time functions\n");
        fprintf(stderr, "\nOnly pure computation allowed!\n");
        fprintf(stderr, "============================================\n");
        exit(1);
    }
}

/* ============================================================================
 * Compile-Time System Implementation
 * ============================================================================ */

void initComptime() {
    comptimeCtx.count = 0;
    
    // Detect OS at compile time
    #ifdef __linux__
    strcpy(comptimeCtx.osName, "linux");
    #elif defined(__APPLE__)
    strcpy(comptimeCtx.osName, "macos");
    #elif defined(_WIN32)
    strcpy(comptimeCtx.osName, "windows");
    #else
    strcpy(comptimeCtx.osName, "unknown");
    #endif
    
    // Detect architecture
    #ifdef __x86_64__
    strcpy(comptimeCtx.archName, "x64");
    #elif defined(__i386__)
    strcpy(comptimeCtx.archName, "x86");
    #elif defined(__aarch64__)
    strcpy(comptimeCtx.archName, "arm64");
    #else
    strcpy(comptimeCtx.archName, "unknown");
    #endif
}

void addComptimeInt(const char *name, int value) {
    if (comptimeCtx.count >= MAX_VARIABLES) return;
    ComptimeValue *cv = &comptimeCtx.values[comptimeCtx.count++];
    strcpy(cv->name, name);
    cv->type = 0;
    cv->intValue = value;
}

void addComptimeBool(const char *name, bool value) {
    if (comptimeCtx.count >= MAX_VARIABLES) return;
    ComptimeValue *cv = &comptimeCtx.values[comptimeCtx.count++];
    strcpy(cv->name, name);
    cv->type = 2;
    cv->boolValue = value;
}

void addComptimeString(const char *name, const char *value) {
    if (comptimeCtx.count >= MAX_VARIABLES) return;
    ComptimeValue *cv = &comptimeCtx.values[comptimeCtx.count++];
    strcpy(cv->name, name);
    cv->type = 3;
    strncpy(cv->stringValue, value, MAX_STRING_LENGTH - 1);
    cv->stringValue[MAX_STRING_LENGTH - 1] = '\0';
}

ComptimeValue* findComptimeValue(const char *name) {
    for (int i = 0; i < comptimeCtx.count; i++) {
        if (strcmp(comptimeCtx.values[i].name, name) == 0) {
            return &comptimeCtx.values[i];
        }
    }
    return NULL;
}

/* ============================================================================
 * Task System Implementation - Race Detection
 * ============================================================================ */

void initTaskSystem() {
    taskSystem.taskCount = 0;
    taskSystem.raceDetected = false;
}

void recordVarAccess(const char *taskName, const char *varName, bool isWrite) {
    // Find or create task
    Task *task = NULL;
    for (int i = 0; i < taskSystem.taskCount; i++) {
        if (strcmp(taskSystem.tasks[i].name, taskName) == 0) {
            task = &taskSystem.tasks[i];
            break;
        }
    }
    
    if (task == NULL) {
        if (taskSystem.taskCount >= MAX_TASKS) return;
        task = &taskSystem.tasks[taskSystem.taskCount++];
        strncpy(task->name, taskName, 255);
        task->name[255] = '\0';
        task->accessedCount = 0;
    }
    
    // Record access
    for (int i = 0; i < task->accessedCount; i++) {
        if (strcmp(task->accessedVars[i], varName) == 0) {
            if (isWrite) {
                task->isWriting[i] = true;
            }
            return;
        }
    }
    
    // New variable access
    if (task->accessedCount < MAX_ACCESSED_VARS) {
        strncpy(task->accessedVars[task->accessedCount], varName, 255);
        task->accessedVars[task->accessedCount][255] = '\0';
        task->isWriting[task->accessedCount] = isWrite;
        task->accessedCount++;
    }
}

bool detectRaceCondition() {
    // Check for conflicting accesses between tasks
    for (int i = 0; i < taskSystem.taskCount; i++) {
        for (int j = i + 1; j < taskSystem.taskCount; j++) {
            Task *task1 = &taskSystem.tasks[i];
            Task *task2 = &taskSystem.tasks[j];
            
            // Check for shared variable access
            for (int v1 = 0; v1 < task1->accessedCount; v1++) {
                for (int v2 = 0; v2 < task2->accessedCount; v2++) {
                    if (strcmp(task1->accessedVars[v1], task2->accessedVars[v2]) == 0) {
                        // Same variable accessed by both tasks
                        if (task1->isWriting[v1] || task2->isWriting[v2]) {
                            // At least one write - RACE CONDITION!
                            taskSystem.raceDetected = true;
                            strcpy(taskSystem.raceTask1, task1->name);
                            strcpy(taskSystem.raceTask2, task2->name);
                            strcpy(taskSystem.raceVar, task1->accessedVars[v1]);
                            return true;
                        }
                    }
                }
            }
        }
    }
    return false;
}

/* ============================================================================
 * Import System Implementation
 * ============================================================================ */

void initImportTracker() {
    importTracker.importCount = 0;
    if (getcwd(importTracker.currentDirectory, sizeof(importTracker.currentDirectory)) == NULL) {
        strcpy(importTracker.currentDirectory, ".");
    }
}

bool isAlreadyImported(const char *filename) {
    for (int i = 0; i < importTracker.importCount; i++) {
        if (strcmp(importTracker.importedFiles[i], filename) == 0) {
            return true;
        }
    }
    return false;
}

void addImportedFile(const char *filename) {
    if (importTracker.importCount < MAX_IMPORTS) {
        strncpy(importTracker.importedFiles[importTracker.importCount], filename, MAX_PATH_LENGTH - 1);
        importTracker.importedFiles[importTracker.importCount][MAX_PATH_LENGTH - 1] = '\0';
        importTracker.importCount++;
    }
}

void resolveImportPath(const char *importFile, char *resolvedPath, size_t maxLen) {
    if (importFile[0] == '/') {
        strncpy(resolvedPath, importFile, maxLen - 1);
        resolvedPath[maxLen - 1] = '\0';
        return;
    }
    
    snprintf(resolvedPath, maxLen, "%s/%s", importTracker.currentDirectory, importFile);
}

void processImport(const char *filename) {
    char resolvedPath[MAX_PATH_LENGTH];
    resolveImportPath(filename, resolvedPath, sizeof(resolvedPath));
    
    if (isAlreadyImported(resolvedPath)) {
        return;
    }
    
    FILE *file = fopen(resolvedPath, "r");
    if (!file) {
        fprintf(stderr, "Import Error: Cannot open file '%s'\n", resolvedPath);
        exit(1);
    }
    fclose(file);
    
    addImportedFile(resolvedPath);
    
    char savedDir[MAX_PATH_LENGTH];
    strcpy(savedDir, importTracker.currentDirectory);
    
    char newDir[MAX_PATH_LENGTH];
    strcpy(newDir, resolvedPath);
    
    char *lastSlash = strrchr(newDir, '/');
    if (lastSlash != NULL) {
        *lastSlash = '\0';
        strcpy(importTracker.currentDirectory, newDir);
    }
    
    parseFile(resolvedPath);
    
    strcpy(importTracker.currentDirectory, savedDir);
}

/* ============================================================================
 * Value Operations (Memory-Safe)
 * ============================================================================ */

Value createIntValue(int val) {
    Value v;
    v.type = VAL_INT;
    v.as.intVal = val;
    return v;
}

Value createDoubleValue(double val) {
    Value v;
    v.type = VAL_DOUBLE;
    v.as.doubleVal = val;
    return v;
}

Value createBoolValue(bool val) {
    Value v;
    v.type = VAL_BOOL;
    v.as.boolVal = val;
    return v;
}

Value createStringValue(const char *str) {
    Value v;
    v.type = VAL_STRING;
    strncpy(v.as.stringVal, str, MAX_STRING_LENGTH - 1);
    v.as.stringVal[MAX_STRING_LENGTH - 1] = '\0';
    return v;
}

Value createNullValue() {
    Value v;
    v.type = VAL_NULL;
    return v;
}

Value createListValue() {
    // Check for real-time violation
    checkRealTimeViolation("List creation (allocates memory)");
    
    Value v;
    v.type = VAL_LIST;
    v.as.listVal.capacity = 10;
    v.as.listVal.count = 0;
    v.as.listVal.elements = calloc(v.as.listVal.capacity, sizeof(Value));
    return v;
}

void appendToList(Value *list, Value item) {
    // Check for real-time violation
    checkRealTimeViolation("List append (allocates memory)");
    
    if (list->type != VAL_LIST) {
        fprintf(stderr, "Runtime Error: Cannot append to non-list\n");
        exit(1);
    }
    
    if (list->as.listVal.count >= list->as.listVal.capacity) {
        list->as.listVal.capacity *= 2;
        list->as.listVal.elements = realloc(list->as.listVal.elements,
                                           list->as.listVal.capacity * sizeof(Value));
    }
    
    list->as.listVal.elements[list->as.listVal.count++] = item;
}

Value getListElement(Value *list, int index) {
    if (list->type != VAL_LIST) {
        fprintf(stderr, "Runtime Error: Cannot index non-list\n");
        exit(1);
    }
    
    if (index < 0 || index >= list->as.listVal.count) {
        fprintf(stderr, "Runtime Error: List index out of bounds\n");
        exit(1);
    }
    
    return list->as.listVal.elements[index];
}

void printValue(Value val) {
    // Check for real-time violation
    checkRealTimeViolation("print() is a system call");
    
    switch (val.type) {
        case VAL_NULL:
            printf("null");
            break;
        case VAL_INT:
            printf("%d", val.as.intVal);
            break;
        case VAL_DOUBLE:
            printf("%g", val.as.doubleVal);
            break;
        case VAL_BOOL:
            printf("%s", val.as.boolVal ? "true" : "false");
            break;
        case VAL_STRING:
            printf("%s", val.as.stringVal);
            break;
        case VAL_LIST:
            printf("[");
            for (int i = 0; i < val.as.listVal.count; i++) {
                printValue(val.as.listVal.elements[i]);
                if (i < val.as.listVal.count - 1) printf(", ");
            }
            printf("]");
            break;
    }
}

/* ============================================================================
 * Lexer Implementation
 * ============================================================================ */

void initLexer(Lexer *lexer, char *source) {
    lexer->source = source;
    lexer->current = 0;
    lexer->line = 1;
    lexer->tokenCount = 0;
}

bool isAtEnd(Lexer *lexer) {
    return lexer->source[lexer->current] == '\0';
}

char advance(Lexer *lexer) {
    return lexer->source[lexer->current++];
}

char peek(Lexer *lexer) {
    return lexer->source[lexer->current];
}

char peekNext(Lexer *lexer) {
    if (isAtEnd(lexer)) return '\0';
    return lexer->source[lexer->current + 1];
}

bool match(Lexer *lexer, char expected) {
    if (isAtEnd(lexer)) return false;
    if (lexer->source[lexer->current] != expected) return false;
    lexer->current++;
    return true;
}

void addToken(Lexer *lexer, TokenType type, const char *lexeme, double numValue) {
    Token *token = &lexer->tokens[lexer->tokenCount++];
    token->type = type;
    strncpy(token->lexeme, lexeme, 255);
    token->lexeme[255] = '\0';
    token->numValue = numValue;
    token->line = lexer->line;
}

void scanString(Lexer *lexer) {
    int start = lexer->current;
    while (peek(lexer) != '"' && !isAtEnd(lexer)) {
        if (peek(lexer) == '\n') lexer->line++;
        advance(lexer);
    }
    
    if (isAtEnd(lexer)) {
        fprintf(stderr, "Lexer Error: Unterminated string at line %d\n", lexer->line);
        exit(1);
    }
    
    advance(lexer);
    
    char str[MAX_STRING_LENGTH];
    int len = lexer->current - start - 1;
    if (len >= MAX_STRING_LENGTH) len = MAX_STRING_LENGTH - 1;
    strncpy(str, &lexer->source[start], len);
    str[len] = '\0';
    
    addToken(lexer, TOKEN_STRING, str, 0);
}

void scanNumber(Lexer *lexer) {
    int start = lexer->current - 1;
    
    while (isdigit(peek(lexer))) advance(lexer);
    
    if (peek(lexer) == '.' && isdigit(peekNext(lexer))) {
        advance(lexer);
        while (isdigit(peek(lexer))) advance(lexer);
    }
    
    char numStr[256];
    int len = lexer->current - start;
    strncpy(numStr, &lexer->source[start], len);
    numStr[len] = '\0';
    
    double value = atof(numStr);
    addToken(lexer, TOKEN_NUMBER, numStr, value);
}

void scanIdentifier(Lexer *lexer) {
    int start = lexer->current - 1;
    
    while (isalnum(peek(lexer)) || peek(lexer) == '_') advance(lexer);
    
    char id[256];
    int len = lexer->current - start;
    strncpy(id, &lexer->source[start], len);
    id[len] = '\0';
    
    TokenType type = TOKEN_IDENTIFIER;
    
    if (strcmp(id, "void") == 0) type = TOKEN_VOID;
    else if (strcmp(id, "int") == 0) type = TOKEN_INT;
    else if (strcmp(id, "double") == 0) type = TOKEN_DOUBLE;
    else if (strcmp(id, "string") == 0) type = TOKEN_STRING_TYPE;
    else if (strcmp(id, "bool") == 0) type = TOKEN_BOOL;
    else if (strcmp(id, "static") == 0) type = TOKEN_STATIC;
    else if (strcmp(id, "def") == 0) type = TOKEN_DEF;
    else if (strcmp(id, "return") == 0) type = TOKEN_RETURN;
    else if (strcmp(id, "if") == 0) type = TOKEN_IF;
    else if (strcmp(id, "else") == 0) type = TOKEN_ELSE;
    else if (strcmp(id, "elif") == 0) type = TOKEN_ELIF;
    else if (strcmp(id, "while") == 0) type = TOKEN_WHILE;
    else if (strcmp(id, "for") == 0) type = TOKEN_FOR;
    else if (strcmp(id, "in") == 0) type = TOKEN_IN;
    else if (strcmp(id, "range") == 0) type = TOKEN_RANGE;
    else if (strcmp(id, "print") == 0) type = TOKEN_PRINT;
    else if (strcmp(id, "true") == 0) type = TOKEN_TRUE;
    else if (strcmp(id, "false") == 0) type = TOKEN_FALSE;
    else if (strcmp(id, "and") == 0) type = TOKEN_AND;
    else if (strcmp(id, "or") == 0) type = TOKEN_OR;
    else if (strcmp(id, "not") == 0) type = TOKEN_NOT;
    else if (strcmp(id, "len") == 0) type = TOKEN_LEN;
    else if (strcmp(id, "append") == 0) type = TOKEN_APPEND;
    else if (strcmp(id, "list") == 0) type = TOKEN_LIST;
    else if (strcmp(id, "import") == 0) type = TOKEN_IMPORT;
    else if (strcmp(id, "comptime") == 0) type = TOKEN_COMPTIME;
    else if (strcmp(id, "spawn") == 0) type = TOKEN_SPAWN;
    else if (strcmp(id, "real_time") == 0) type = TOKEN_REAL_TIME;
    else if (strcmp(id, "global") == 0) type = TOKEN_GLOBAL;
    
    addToken(lexer, type, id, 0);
}

void scanTokens(Lexer *lexer) {
    while (!isAtEnd(lexer)) {
        char c = advance(lexer);
        
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                break;
            case '\n':
                lexer->line++;
                break;
            case '#':
                // Single-line comment
                while (peek(lexer) != '\n' && !isAtEnd(lexer)) advance(lexer);
                break;
            case '@':
                addToken(lexer, TOKEN_AT, "@", 0);
                break;
            case '/':
                if (match(lexer, '/')) {
                    // C++ style comment
                    while (peek(lexer) != '\n' && !isAtEnd(lexer)) advance(lexer);
                } else if (match(lexer, '*')) {
                    // Multi-line C-style comment
                    while (!isAtEnd(lexer)) {
                        if (peek(lexer) == '*' && peekNext(lexer) == '/') {
                            advance(lexer); // consume *
                            advance(lexer); // consume /
                            break;
                        }
                        if (peek(lexer) == '\n') lexer->line++;
                        advance(lexer);
                    }
                } else {
                    addToken(lexer, TOKEN_DIVIDE, "/", 0);
                }
                break;
            case '(': addToken(lexer, TOKEN_LPAREN, "(", 0); break;
            case ')': addToken(lexer, TOKEN_RPAREN, ")", 0); break;
            case '{': addToken(lexer, TOKEN_LBRACE, "{", 0); break;
            case '}': addToken(lexer, TOKEN_RBRACE, "}", 0); break;
            case '[': addToken(lexer, TOKEN_LBRACKET, "[", 0); break;
            case ']': addToken(lexer, TOKEN_RBRACKET, "]", 0); break;
            case ',': addToken(lexer, TOKEN_COMMA, ",", 0); break;
            case ':': addToken(lexer, TOKEN_COLON, ":", 0); break;
            case ';': addToken(lexer, TOKEN_SEMICOLON, ";", 0); break;
            case '.': addToken(lexer, TOKEN_DOT, ".", 0); break;
            case '+':
                if (match(lexer, '=')) addToken(lexer, TOKEN_PLUS_ASSIGN, "+=", 0);
                else addToken(lexer, TOKEN_PLUS, "+", 0);
                break;
            case '-':
                if (match(lexer, '=')) addToken(lexer, TOKEN_MINUS_ASSIGN, "-=", 0);
                else addToken(lexer, TOKEN_MINUS, "-", 0);
                break;
            case '*': addToken(lexer, TOKEN_MULTIPLY, "*", 0); break;
            case '%': addToken(lexer, TOKEN_MODULO, "%", 0); break;
            case '=':
                if (match(lexer, '=')) addToken(lexer, TOKEN_EQUALS, "==", 0);
                else addToken(lexer, TOKEN_ASSIGN, "=", 0);
                break;
            case '!':
                if (match(lexer, '=')) addToken(lexer, TOKEN_NOT_EQUALS, "!=", 0);
                else {
                    fprintf(stderr, "Lexer Error: Unexpected character '!' at line %d\n", lexer->line);
                    exit(1);
                }
                break;
            case '<':
                if (match(lexer, '=')) addToken(lexer, TOKEN_LESS_EQUAL, "<=", 0);
                else addToken(lexer, TOKEN_LESS, "<", 0);
                break;
            case '>':
                if (match(lexer, '=')) addToken(lexer, TOKEN_GREATER_EQUAL, ">=", 0);
                else addToken(lexer, TOKEN_GREATER, ">", 0);
                break;
            case '"':
                scanString(lexer);
                break;
            default:
                if (isdigit(c)) {
                    scanNumber(lexer);
                } else if (isalpha(c) || c == '_') {
                    scanIdentifier(lexer);
                } else {
                    fprintf(stderr, "Lexer Error: Unexpected character '%c' at line %d\n", c, lexer->line);
                    exit(1);
                }
                break;
        }
    }
    
    addToken(lexer, TOKEN_EOF, "", 0);
}

/* ============================================================================
 * Parser Implementation
 * ============================================================================ */

typedef struct {
    Token *tokens;
    int current;
    int count;
} Parser;

void initParser(Parser *parser, Token *tokens, int count) {
    parser->tokens = tokens;
    parser->current = 0;
    parser->count = count;
}

Token *peekToken(Parser *parser) {
    return &parser->tokens[parser->current];
}

Token *previousToken(Parser *parser) {
    return &parser->tokens[parser->current - 1];
}

bool isAtEndParser(Parser *parser) {
    return peekToken(parser)->type == TOKEN_EOF;
}

Token *advanceToken(Parser *parser) {
    if (!isAtEndParser(parser)) parser->current++;
    return previousToken(parser);
}

bool check(Parser *parser, TokenType type) {
    if (isAtEndParser(parser)) return false;
    return peekToken(parser)->type == type;
}

bool matchToken(Parser *parser, TokenType type) {
    if (check(parser, type)) {
        advanceToken(parser);
        return true;
    }
    return false;
}

void consume(Parser *parser, TokenType type, const char *message) {
    if (check(parser, type)) {
        advanceToken(parser);
        return;
    }
    
    fprintf(stderr, "Parser Error: %s at line %d\n", message, peekToken(parser)->line);
    exit(1);
}

Expr *parseExpression(Parser *parser);
Stmt *parseStatement(Parser *parser);

Expr *createLiteralExpr(Value val) {
    Expr *expr = malloc(sizeof(Expr));
    expr->type = EXPR_LITERAL;
    expr->as.literal = val;
    return expr;
}

Expr *createVariableExpr(const char *name) {
    Expr *expr = malloc(sizeof(Expr));
    expr->type = EXPR_VARIABLE;
    strcpy(expr->as.varName, name);
    return expr;
}

Expr *parsePrimary(Parser *parser) {
    if (matchToken(parser, TOKEN_TRUE)) {
        return createLiteralExpr(createBoolValue(true));
    }
    
    if (matchToken(parser, TOKEN_FALSE)) {
        return createLiteralExpr(createBoolValue(false));
    }
    
    if (matchToken(parser, TOKEN_NUMBER)) {
        Token *token = previousToken(parser);
        if (strchr(token->lexeme, '.')) {
            return createLiteralExpr(createDoubleValue(token->numValue));
        } else {
            return createLiteralExpr(createIntValue((int)token->numValue));
        }
    }
    
    if (matchToken(parser, TOKEN_STRING)) {
        return createLiteralExpr(createStringValue(previousToken(parser)->lexeme));
    }
    
    if (matchToken(parser, TOKEN_COMPTIME)) {
        consume(parser, TOKEN_LPAREN, "Expected '(' after 'comptime'");
        Expr *expr = malloc(sizeof(Expr));
        expr->type = EXPR_COMPTIME;
        expr->as.comptime.expression = parseExpression(parser);
        consume(parser, TOKEN_RPAREN, "Expected ')' after comptime expression");
        
        // Evaluate at compile time
        Value result = evaluateExpr(expr->as.comptime.expression);
        free(expr);
        return createLiteralExpr(result);
    }
    
    if (matchToken(parser, TOKEN_IDENTIFIER)) {
        char name[256];
        strcpy(name, previousToken(parser)->lexeme);
        
        // Check for comptime variables
        if (strcmp(name, "os") == 0) {
            return createLiteralExpr(createStringValue(comptimeCtx.osName));
        }
        if (strcmp(name, "arch") == 0) {
            return createLiteralExpr(createStringValue(comptimeCtx.archName));
        }
        
        ComptimeValue *cv = findComptimeValue(name);
        if (cv != NULL) {
            if (cv->type == 0) {
                return createLiteralExpr(createIntValue(cv->intValue));
            } else if (cv->type == 2) {
                return createLiteralExpr(createBoolValue(cv->boolValue));
            } else if (cv->type == 3) {
                return createLiteralExpr(createStringValue(cv->stringValue));
            }
        }
        
        if (matchToken(parser, TOKEN_LPAREN)) {
            Expr *expr = malloc(sizeof(Expr));
            expr->type = EXPR_CALL;
            strcpy(expr->as.call.name, name);
            expr->as.call.argCount = 0;
            expr->as.call.args = malloc(MAX_PARAMS * sizeof(Expr*));
            
            if (!check(parser, TOKEN_RPAREN)) {
                do {
                    expr->as.call.args[expr->as.call.argCount++] = parseExpression(parser);
                } while (matchToken(parser, TOKEN_COMMA));
            }
            
            consume(parser, TOKEN_RPAREN, "Expected ')' after arguments");
            return expr;
        }
        
        return createVariableExpr(name);
    }
    
    if (matchToken(parser, TOKEN_LPAREN)) {
        Expr *expr = parseExpression(parser);
        consume(parser, TOKEN_RPAREN, "Expected ')' after expression");
        return expr;
    }
    
    if (matchToken(parser, TOKEN_LBRACKET)) {
        Expr *expr = malloc(sizeof(Expr));
        expr->type = EXPR_LIST;
        expr->as.list.count = 0;
        expr->as.list.elements = malloc(MAX_ARRAY_SIZE * sizeof(Expr*));
        
        if (!check(parser, TOKEN_RBRACKET)) {
            do {
                expr->as.list.elements[expr->as.list.count++] = parseExpression(parser);
            } while (matchToken(parser, TOKEN_COMMA));
        }
        
        consume(parser, TOKEN_RBRACKET, "Expected ']' after list elements");
        return expr;
    }
    
    if (matchToken(parser, TOKEN_RANGE)) {
        consume(parser, TOKEN_LPAREN, "Expected '(' after 'range'");
        Expr *expr = malloc(sizeof(Expr));
        expr->type = EXPR_RANGE;
        expr->as.range.start = parseExpression(parser);
        consume(parser, TOKEN_COMMA, "Expected ',' in range");
        expr->as.range.end = parseExpression(parser);
        consume(parser, TOKEN_RPAREN, "Expected ')' after range");
        return expr;
    }
    
    if (matchToken(parser, TOKEN_LEN)) {
        consume(parser, TOKEN_LPAREN, "Expected '(' after 'len'");
        Expr *expr = malloc(sizeof(Expr));
        expr->type = EXPR_CALL;
        strcpy(expr->as.call.name, "len");
        expr->as.call.argCount = 1;
        expr->as.call.args = malloc(sizeof(Expr*));
        expr->as.call.args[0] = parseExpression(parser);
        consume(parser, TOKEN_RPAREN, "Expected ')' after len argument");
        return expr;
    }
    
    fprintf(stderr, "Parser Error: Expected expression at line %d\n", peekToken(parser)->line);
    exit(1);
}

Expr *parsePostfix(Parser *parser) {
    Expr *expr = parsePrimary(parser);
    
    while (matchToken(parser, TOKEN_LBRACKET)) {
        Expr *index = malloc(sizeof(Expr));
        index->type = EXPR_INDEX;
        index->as.index.list = expr;
        index->as.index.index = parseExpression(parser);
        consume(parser, TOKEN_RBRACKET, "Expected ']' after index");
        expr = index;
    }
    
    return expr;
}

Expr *parseUnary(Parser *parser) {
    if (matchToken(parser, TOKEN_NOT) || matchToken(parser, TOKEN_MINUS)) {
        Expr *expr = malloc(sizeof(Expr));
        expr->type = EXPR_UNARY;
        expr->as.unary.op = *previousToken(parser);
        expr->as.unary.operand = parseUnary(parser);
        return expr;
    }
    
    return parsePostfix(parser);
}

Expr *parseMultiplicative(Parser *parser) {
    Expr *expr = parseUnary(parser);
    
    while (matchToken(parser, TOKEN_MULTIPLY) || matchToken(parser, TOKEN_DIVIDE) || matchToken(parser, TOKEN_MODULO)) {
        Expr *binary = malloc(sizeof(Expr));
        binary->type = EXPR_BINARY;
        binary->as.binary.left = expr;
        binary->as.binary.op = *previousToken(parser);
        binary->as.binary.right = parseUnary(parser);
        expr = binary;
    }
    
    return expr;
}

Expr *parseAdditive(Parser *parser) {
    Expr *expr = parseMultiplicative(parser);
    
    while (matchToken(parser, TOKEN_PLUS) || matchToken(parser, TOKEN_MINUS)) {
        Expr *binary = malloc(sizeof(Expr));
        binary->type = EXPR_BINARY;
        binary->as.binary.left = expr;
        binary->as.binary.op = *previousToken(parser);
        binary->as.binary.right = parseMultiplicative(parser);
        expr = binary;
    }
    
    return expr;
}

Expr *parseComparison(Parser *parser) {
    Expr *expr = parseAdditive(parser);
    
    while (matchToken(parser, TOKEN_GREATER) || matchToken(parser, TOKEN_GREATER_EQUAL) ||
           matchToken(parser, TOKEN_LESS) || matchToken(parser, TOKEN_LESS_EQUAL)) {
        Expr *binary = malloc(sizeof(Expr));
        binary->type = EXPR_BINARY;
        binary->as.binary.left = expr;
        binary->as.binary.op = *previousToken(parser);
        binary->as.binary.right = parseAdditive(parser);
        expr = binary;
    }
    
    return expr;
}

Expr *parseEquality(Parser *parser) {
    Expr *expr = parseComparison(parser);
    
    while (matchToken(parser, TOKEN_EQUALS) || matchToken(parser, TOKEN_NOT_EQUALS)) {
        Expr *binary = malloc(sizeof(Expr));
        binary->type = EXPR_BINARY;
        binary->as.binary.left = expr;
        binary->as.binary.op = *previousToken(parser);
        binary->as.binary.right = parseComparison(parser);
        expr = binary;
    }
    
    return expr;
}

Expr *parseLogicalAnd(Parser *parser) {
    Expr *expr = parseEquality(parser);
    
    while (matchToken(parser, TOKEN_AND)) {
        Expr *binary = malloc(sizeof(Expr));
        binary->type = EXPR_BINARY;
        binary->as.binary.left = expr;
        binary->as.binary.op = *previousToken(parser);
        binary->as.binary.right = parseEquality(parser);
        expr = binary;
    }
    
    return expr;
}

Expr *parseLogicalOr(Parser *parser) {
    Expr *expr = parseLogicalAnd(parser);
    
    while (matchToken(parser, TOKEN_OR)) {
        Expr *binary = malloc(sizeof(Expr));
        binary->type = EXPR_BINARY;
        binary->as.binary.left = expr;
        binary->as.binary.op = *previousToken(parser);
        binary->as.binary.right = parseLogicalAnd(parser);
        expr = binary;
    }
    
    return expr;
}

Expr *parseExpression(Parser *parser) {
    return parseLogicalOr(parser);
}

Stmt *parseBlock(Parser *parser) {
    Stmt *stmt = malloc(sizeof(Stmt));
    stmt->type = STMT_BLOCK;
    stmt->as.block.statements = malloc(MAX_STATEMENTS * sizeof(Stmt*));
    stmt->as.block.count = 0;
    
    while (!check(parser, TOKEN_RBRACE) && !isAtEndParser(parser)) {
        stmt->as.block.statements[stmt->as.block.count++] = parseStatement(parser);
    }
    
    consume(parser, TOKEN_RBRACE, "Expected '}' after block");
    return stmt;
}

Stmt *parseStatement(Parser *parser) {
    if (matchToken(parser, TOKEN_SPAWN)) {
        // Check if in real-time context
        checkRealTimeViolation("spawn (blocks/creates tasks)");
        
        Stmt *stmt = malloc(sizeof(Stmt));
        stmt->type = STMT_SPAWN;
        
        Token *funcName = advanceToken(parser);
        strcpy(stmt->as.spawn.call.name, funcName->lexeme);
        
        consume(parser, TOKEN_LPAREN, "Expected '(' after function name in spawn");
        
        stmt->as.spawn.call.argCount = 0;
        stmt->as.spawn.call.args = malloc(MAX_PARAMS * sizeof(Expr*));
        
        if (!check(parser, TOKEN_RPAREN)) {
            do {
                stmt->as.spawn.call.args[stmt->as.spawn.call.argCount++] = parseExpression(parser);
            } while (matchToken(parser, TOKEN_COMMA));
        }
        
        consume(parser, TOKEN_RPAREN, "Expected ')' after spawn arguments");
        return stmt;
    }
    
    if (matchToken(parser, TOKEN_IMPORT)) {
        consume(parser, TOKEN_STRING, "Expected filename string after 'import'");
        Stmt *stmt = malloc(sizeof(Stmt));
        stmt->type = STMT_IMPORT;
        strcpy(stmt->as.import.filename, previousToken(parser)->lexeme);
        return stmt;
    }
    
    if (matchToken(parser, TOKEN_PRINT)) {
        consume(parser, TOKEN_LPAREN, "Expected '(' after 'print'");
        Stmt *stmt = malloc(sizeof(Stmt));
        stmt->type = STMT_PRINT;
        stmt->as.printExpr = parseExpression(parser);
        consume(parser, TOKEN_RPAREN, "Expected ')' after print expression");
        return stmt;
    }
    
    if (matchToken(parser, TOKEN_RETURN)) {
        Stmt *stmt = malloc(sizeof(Stmt));
        stmt->type = STMT_RETURN;
        if (!check(parser, TOKEN_SEMICOLON) && !check(parser, TOKEN_RBRACE)) {
            stmt->as.returnValue = parseExpression(parser);
        } else {
            stmt->as.returnValue = NULL;
        }
        return stmt;
    }
    
    if (matchToken(parser, TOKEN_IF)) {
        bool isComptime = false;
        if (matchToken(parser, TOKEN_COMPTIME)) {
            isComptime = true;
        }
        
        consume(parser, TOKEN_LPAREN, "Expected '(' after 'if'");
        Stmt *stmt = malloc(sizeof(Stmt));
        stmt->type = STMT_IF;
        stmt->as.ifStmt.condition = parseExpression(parser);
        stmt->as.ifStmt.isComptime = isComptime;
        consume(parser, TOKEN_RPAREN, "Expected ')' after if condition");
        consume(parser, TOKEN_LBRACE, "Expected '{' after if condition");
        stmt->as.ifStmt.thenBranch = parseBlock(parser);
        
        if (matchToken(parser, TOKEN_ELSE)) {
            consume(parser, TOKEN_LBRACE, "Expected '{' after else");
            stmt->as.ifStmt.elseBranch = parseBlock(parser);
        } else {
            stmt->as.ifStmt.elseBranch = NULL;
        }
        
        // Handle comptime if - evaluate and prune branches
        if (isComptime) {
            Value condition = evaluateExpr(stmt->as.ifStmt.condition);
            if (condition.as.boolVal) {
                return stmt->as.ifStmt.thenBranch;
            } else if (stmt->as.ifStmt.elseBranch) {
                return stmt->as.ifStmt.elseBranch;
            } else {
                Stmt *empty = malloc(sizeof(Stmt));
                empty->type = STMT_BLOCK;
                empty->as.block.statements = malloc(sizeof(Stmt*));
                empty->as.block.count = 0;
                return empty;
            }
        }
        
        return stmt;
    }
    
    if (matchToken(parser, TOKEN_WHILE)) {
        consume(parser, TOKEN_LPAREN, "Expected '(' after 'while'");
        Stmt *stmt = malloc(sizeof(Stmt));
        stmt->type = STMT_WHILE;
        stmt->as.whileStmt.condition = parseExpression(parser);
        consume(parser, TOKEN_RPAREN, "Expected ')' after while condition");
        consume(parser, TOKEN_LBRACE, "Expected '{' after while condition");
        stmt->as.whileStmt.body = parseBlock(parser);
        return stmt;
    }
    
    if (matchToken(parser, TOKEN_FOR)) {
        Stmt *stmt = malloc(sizeof(Stmt));
        stmt->type = STMT_FOR;
        consume(parser, TOKEN_LPAREN, "Expected '(' after 'for'");
        Token *varToken = advanceToken(parser);
        strcpy(stmt->as.forStmt.varName, varToken->lexeme);
        consume(parser, TOKEN_IN, "Expected 'in' in for loop");
        stmt->as.forStmt.iterable = parseExpression(parser);
        consume(parser, TOKEN_RPAREN, "Expected ')' after for iterable");
        consume(parser, TOKEN_LBRACE, "Expected '{' after for header");
        stmt->as.forStmt.body = parseBlock(parser);
        return stmt;
    }
    
    // NEW: Check for global keyword
    bool isGlobal = false;
    if (matchToken(parser, TOKEN_GLOBAL)) {
        isGlobal = true;
    }
    
    if (matchToken(parser, TOKEN_INT) || matchToken(parser, TOKEN_DOUBLE) || 
        matchToken(parser, TOKEN_STRING_TYPE) || matchToken(parser, TOKEN_BOOL) ||
        matchToken(parser, TOKEN_LIST)) {
        Stmt *stmt = malloc(sizeof(Stmt));
        stmt->type = STMT_VAR_DECL;
        stmt->as.varDecl.varType = previousToken(parser)->type;
        stmt->as.varDecl.isComptime = false;
        stmt->as.varDecl.isGlobal = isGlobal;  // Set from global keyword
        Token *nameToken = advanceToken(parser);
        strcpy(stmt->as.varDecl.name, nameToken->lexeme);
        
        if (matchToken(parser, TOKEN_ASSIGN)) {
            if (matchToken(parser, TOKEN_COMPTIME)) {
                stmt->as.varDecl.isComptime = true;
            }
            stmt->as.varDecl.initializer = parseExpression(parser);
            
            // Handle comptime variables
            if (stmt->as.varDecl.isComptime) {
                Value val = evaluateExpr(stmt->as.varDecl.initializer);
                if (val.type == VAL_INT) {
                    addComptimeInt(stmt->as.varDecl.name, val.as.intVal);
                } else if (val.type == VAL_BOOL) {
                    addComptimeBool(stmt->as.varDecl.name, val.as.boolVal);
                } else if (val.type == VAL_STRING) {
                    addComptimeString(stmt->as.varDecl.name, val.as.stringVal);
                }
            }
        } else {
            stmt->as.varDecl.initializer = NULL;
        }
        
        return stmt;
    }
    
    if (check(parser, TOKEN_IDENTIFIER)) {
        Token *nameToken = advanceToken(parser);
        
        // Check for list[index] = value
        if (matchToken(parser, TOKEN_LBRACKET)) {
            Expr *index = parseExpression(parser);
            consume(parser, TOKEN_RBRACKET, "Expected ']' after index");
            consume(parser, TOKEN_ASSIGN, "Expected '=' after list[index]");
            
            Stmt *stmt = malloc(sizeof(Stmt));
            stmt->type = STMT_INDEX_ASSIGN;
            strcpy(stmt->as.indexAssign.listName, nameToken->lexeme);
            stmt->as.indexAssign.index = index;
            stmt->as.indexAssign.value = parseExpression(parser);
            return stmt;
        }
        
        if (matchToken(parser, TOKEN_ASSIGN)) {
            Stmt *stmt = malloc(sizeof(Stmt));
            stmt->type = STMT_ASSIGN;
            strcpy(stmt->as.assign.name, nameToken->lexeme);
            stmt->as.assign.value = parseExpression(parser);
            return stmt;
        } else if (matchToken(parser, TOKEN_PLUS_ASSIGN) || matchToken(parser, TOKEN_MINUS_ASSIGN)) {
            Stmt *stmt = malloc(sizeof(Stmt));
            stmt->type = STMT_COMPOUND_ASSIGN;
            strcpy(stmt->as.compoundAssign.name, nameToken->lexeme);
            stmt->as.compoundAssign.op = *previousToken(parser);
            stmt->as.compoundAssign.value = parseExpression(parser);
            return stmt;
        } else if (matchToken(parser, TOKEN_DOT)) {
            if (matchToken(parser, TOKEN_APPEND)) {
                consume(parser, TOKEN_LPAREN, "Expected '(' after 'append'");
                Stmt *stmt = malloc(sizeof(Stmt));
                stmt->type = STMT_LIST_APPEND;
                strcpy(stmt->as.listAppend.listName, nameToken->lexeme);
                stmt->as.listAppend.value = parseExpression(parser);
                consume(parser, TOKEN_RPAREN, "Expected ')' after append argument");
                return stmt;
            }
        } else {
            parser->current--;
        }
    }
    
    Stmt *stmt = malloc(sizeof(Stmt));
    stmt->type = STMT_EXPR;
    stmt->as.expression = parseExpression(parser);
    return stmt;
}

Function parseFunction(Parser *parser) {
    Function func;
    func.isStatic = false;
    func.isRealTime = false;  // Default: not real-time
    
    while (!isAtEndParser(parser) && 
           !check(parser, TOKEN_GLOBAL) &&
           !check(parser, TOKEN_AT) &&
           !check(parser, TOKEN_STATIC) &&
           !check(parser, TOKEN_VOID) &&
           !check(parser, TOKEN_INT) &&
           !check(parser, TOKEN_DOUBLE) &&
           !check(parser, TOKEN_STRING_TYPE) &&
           !check(parser, TOKEN_BOOL) &&
           !check(parser, TOKEN_LIST) &&
           !check(parser, TOKEN_IMPORT)) {
        advanceToken(parser);
    }
    
    if (isAtEndParser(parser)) {
        func.name[0] = '\0';
        return func;
    }
    
    // NEW: Handle global variable declarations
    if (matchToken(parser, TOKEN_GLOBAL)) {
        // Parse: global type name = value
        TokenType varType;
        if (matchToken(parser, TOKEN_INT)) varType = TOKEN_INT;
        else if (matchToken(parser, TOKEN_DOUBLE)) varType = TOKEN_DOUBLE;
        else if (matchToken(parser, TOKEN_STRING_TYPE)) varType = TOKEN_STRING_TYPE;
        else if (matchToken(parser, TOKEN_BOOL)) varType = TOKEN_BOOL;
        else if (matchToken(parser, TOKEN_LIST)) varType = TOKEN_LIST;
        else {
            fprintf(stderr, "Parser Error: Expected type after 'global'\n");
            exit(1);
        }
        
        Token *nameToken = advanceToken(parser);
        Value val;
        
        if (matchToken(parser, TOKEN_ASSIGN)) {
            // Evaluate initializer
            Expr *initExpr = parseExpression(parser);
            val = evaluateExpr(initExpr);
        } else {
            // Default value
            if (varType == TOKEN_LIST) {
                val = createListValue();
            } else {
                val = createIntValue(0);
            }
        }
        
        // Add to global environment
        setGlobalVariable(nameToken->lexeme, val);
        
        // Return empty function to signal we handled something
        func.name[0] = '\0';
        return func;
    }
    
    if (check(parser, TOKEN_IMPORT)) {
        Stmt *importStmt = parseStatement(parser);
        if (importStmt->type == STMT_IMPORT) {
            processImport(importStmt->as.import.filename);
        }
        func.name[0] = '\0';
        return func;
    }
    
    // NEW: Parse @real_time annotation
    if (matchToken(parser, TOKEN_AT)) {
        consume(parser, TOKEN_REAL_TIME, "Expected 'real_time' after '@'");
        func.isRealTime = true;
    }
    
    if (matchToken(parser, TOKEN_STATIC)) {
        func.isStatic = true;
    }
    
    if (matchToken(parser, TOKEN_VOID)) {
        func.returnType = TOKEN_VOID;
    } else if (matchToken(parser, TOKEN_INT)) {
        func.returnType = TOKEN_INT;
    } else if (matchToken(parser, TOKEN_DOUBLE)) {
        func.returnType = TOKEN_DOUBLE;
    } else if (matchToken(parser, TOKEN_STRING_TYPE)) {
        func.returnType = TOKEN_STRING_TYPE;
    } else if (matchToken(parser, TOKEN_BOOL)) {
        func.returnType = TOKEN_BOOL;
    } else if (matchToken(parser, TOKEN_LIST)) {
        func.returnType = TOKEN_LIST;
    } else {
        fprintf(stderr, "Parser Error: Expected return type at line %d\n", peekToken(parser)->line);
        exit(1);
    }
    
    Token *nameToken = advanceToken(parser);
    strcpy(func.name, nameToken->lexeme);
    
    consume(parser, TOKEN_LPAREN, "Expected '(' after function name");
    
    func.paramCount = 0;
    if (!check(parser, TOKEN_RPAREN)) {
        do {
            TokenType paramType;
            if (matchToken(parser, TOKEN_INT)) paramType = TOKEN_INT;
            else if (matchToken(parser, TOKEN_DOUBLE)) paramType = TOKEN_DOUBLE;
            else if (matchToken(parser, TOKEN_STRING_TYPE)) paramType = TOKEN_STRING_TYPE;
            else if (matchToken(parser, TOKEN_BOOL)) paramType = TOKEN_BOOL;
            else if (matchToken(parser, TOKEN_LIST)) paramType = TOKEN_LIST;
            else {
                fprintf(stderr, "Parser Error: Expected parameter type\n");
                exit(1);
            }
            
            Token *paramName = advanceToken(parser);
            func.paramTypes[func.paramCount] = paramType;
            strcpy(func.params[func.paramCount], paramName->lexeme);
            func.paramCount++;
        } while (matchToken(parser, TOKEN_COMMA));
    }
    
    consume(parser, TOKEN_RPAREN, "Expected ')' after parameters");
    consume(parser, TOKEN_LBRACE, "Expected '{' before function body");
    
    func.body = parseBlock(parser);
    
    return func;
}

/* ============================================================================
 * Runtime Interpreter
 * ============================================================================ */

Value *findVariable(const char *name) {
    // Check local scopes (frames) first
    for (int i = runtime.frameCount - 1; i >= 0; i--) {
        Environment *env = &runtime.frames[i].env;
        for (int j = 0; j < env->count; j++) {
            if (strcmp(env->vars[j].name, name) == 0) {
                return &env->vars[j].value;
            }
        }
    }
    
    // Check global scope
    for (int i = 0; i < runtime.globalEnv.count; i++) {
        if (strcmp(runtime.globalEnv.vars[i].name, name) == 0) {
            return &runtime.globalEnv.vars[i].value;
        }
    }
    
    return NULL;
}

void setVariable(const char *name, Value value) {
    Value *var = findVariable(name);
    if (var) {
        *var = value;
    } else {
        // Add to current frame (local)
        Environment *env = &runtime.frames[runtime.frameCount - 1].env;
        strcpy(env->vars[env->count].name, name);
        env->vars[env->count].value = value;
        env->count++;
    }
}

void setGlobalVariable(const char *name, Value value) {
    // Check if global already exists
    for (int i = 0; i < runtime.globalEnv.count; i++) {
        if (strcmp(runtime.globalEnv.vars[i].name, name) == 0) {
            runtime.globalEnv.vars[i].value = value;
            return;
        }
    }
    
    // Add new global
    strcpy(runtime.globalEnv.vars[runtime.globalEnv.count].name, name);
    runtime.globalEnv.vars[runtime.globalEnv.count].value = value;
    runtime.globalEnv.count++;
}

Function *findFunction(const char *name) {
    for (int i = 0; i < runtime.functionCount; i++) {
        if (strcmp(runtime.functions[i].name, name) == 0) {
            return &runtime.functions[i];
        }
    }
    return NULL;
}

Value evaluateBinaryExpr(BinaryExpr *expr) {
    Value left = evaluateExpr(expr->left);
    Value right = evaluateExpr(expr->right);
    
    switch (expr->op.type) {
        case TOKEN_PLUS:
            // String concatenation
            if (left.type == VAL_STRING && right.type == VAL_STRING) {
                Value result;
                result.type = VAL_STRING;
                snprintf(result.as.stringVal, MAX_STRING_LENGTH, "%s%s", 
                        left.as.stringVal, right.as.stringVal);
                return result;
            }
            // Integer addition
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createIntValue(left.as.intVal + right.as.intVal);
            // Mixed int/double
            if (left.type == VAL_DOUBLE || right.type == VAL_DOUBLE) {
                double lval = (left.type == VAL_DOUBLE) ? left.as.doubleVal : left.as.intVal;
                double rval = (right.type == VAL_DOUBLE) ? right.as.doubleVal : right.as.intVal;
                return createDoubleValue(lval + rval);
            }
            break;
            
        case TOKEN_MINUS:
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createIntValue(left.as.intVal - right.as.intVal);
            if (left.type == VAL_DOUBLE || right.type == VAL_DOUBLE) {
                double lval = (left.type == VAL_DOUBLE) ? left.as.doubleVal : left.as.intVal;
                double rval = (right.type == VAL_DOUBLE) ? right.as.doubleVal : right.as.intVal;
                return createDoubleValue(lval - rval);
            }
            break;
            
        case TOKEN_MULTIPLY:
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createIntValue(left.as.intVal * right.as.intVal);
            if (left.type == VAL_DOUBLE || right.type == VAL_DOUBLE) {
                double lval = (left.type == VAL_DOUBLE) ? left.as.doubleVal : left.as.intVal;
                double rval = (right.type == VAL_DOUBLE) ? right.as.doubleVal : right.as.intVal;
                return createDoubleValue(lval * rval);
            }
            break;
            
        case TOKEN_DIVIDE:
            if (left.type == VAL_DOUBLE || right.type == VAL_DOUBLE) {
                double lval = (left.type == VAL_DOUBLE) ? left.as.doubleVal : left.as.intVal;
                double rval = (right.type == VAL_DOUBLE) ? right.as.doubleVal : right.as.intVal;
                return createDoubleValue(lval / rval);
            } else {
                return createIntValue(left.as.intVal / right.as.intVal);
            }
            break;
            
        case TOKEN_MODULO:
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createIntValue(left.as.intVal % right.as.intVal);
            break;
            
        case TOKEN_LESS:
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createBoolValue(left.as.intVal < right.as.intVal);
            break;
            
        case TOKEN_LESS_EQUAL:
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createBoolValue(left.as.intVal <= right.as.intVal);
            break;
            
        case TOKEN_GREATER:
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createBoolValue(left.as.intVal > right.as.intVal);
            break;
            
        case TOKEN_GREATER_EQUAL:
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createBoolValue(left.as.intVal >= right.as.intVal);
            break;
            
        case TOKEN_EQUALS:
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createBoolValue(left.as.intVal == right.as.intVal);
            if (left.type == VAL_BOOL && right.type == VAL_BOOL)
                return createBoolValue(left.as.boolVal == right.as.boolVal);
            if (left.type == VAL_STRING && right.type == VAL_STRING)
                return createBoolValue(strcmp(left.as.stringVal, right.as.stringVal) == 0);
            break;
            
        case TOKEN_NOT_EQUALS:
            if (left.type == VAL_INT && right.type == VAL_INT)
                return createBoolValue(left.as.intVal != right.as.intVal);
            if (left.type == VAL_BOOL && right.type == VAL_BOOL)
                return createBoolValue(left.as.boolVal != right.as.boolVal);
            if (left.type == VAL_STRING && right.type == VAL_STRING)
                return createBoolValue(strcmp(left.as.stringVal, right.as.stringVal) != 0);
            break;
            
        case TOKEN_AND:
            if (left.type == VAL_BOOL && right.type == VAL_BOOL)
                return createBoolValue(left.as.boolVal && right.as.boolVal);
            break;
            
        case TOKEN_OR:
            if (left.type == VAL_BOOL && right.type == VAL_BOOL)
                return createBoolValue(left.as.boolVal || right.as.boolVal);
            break;
            
        default:
            break;
    }
    
    fprintf(stderr, "Runtime Error: Invalid binary operation\n");
    exit(1);
}

Value evaluateCallExpr(CallExpr *call) {
    if (strcmp(call->name, "len") == 0) {
        Value arg = evaluateExpr(call->args[0]);
        if (arg.type == VAL_LIST) {
            return createIntValue(arg.as.listVal.count);
        }
        if (arg.type == VAL_STRING) {
            return createIntValue(strlen(arg.as.stringVal));
        }
    }
    
    Function *func = findFunction(call->name);
    if (!func) {
        fprintf(stderr, "Runtime Error: Undefined function '%s'\n", call->name);
        exit(1);
    }
    
    // NEW: Check if calling non-@real_time from @real_time context
    if (rtContext.inRealTimeContext && !func->isRealTime) {
        char msg[512];
        snprintf(msg, sizeof(msg), 
                "Calling non-@real_time function '%s' from @real_time context", 
                call->name);
        checkRealTimeViolation(msg);
    }
    
    // NEW: Enter real-time context if function is @real_time
    bool wasInRealTime = rtContext.inRealTimeContext;
    if (func->isRealTime && !wasInRealTime) {
        enterRealTimeContext(func->name);
    }
    
    runtime.frameCount++;
    StackFrame *frame = &runtime.frames[runtime.frameCount - 1];
    frame->env.count = 0;
    frame->function = func;
    frame->returnFlag = 0;
    
    for (int i = 0; i < func->paramCount; i++) {
        Value argValue = evaluateExpr(call->args[i]);
        setVariable(func->params[i], argValue);
    }
    
    executeStmt(func->body);
    
    Value returnValue = frame->returnValue;
    runtime.frameCount--;
    
    // NEW: Exit real-time context if we entered it
    if (func->isRealTime && !wasInRealTime) {
        exitRealTimeContext();
    }
    
    return returnValue;
}

Value evaluateExpr(Expr *expr) {
    switch (expr->type) {
        case EXPR_LITERAL:
            return expr->as.literal;
            
        case EXPR_VARIABLE: {
            Value *var = findVariable(expr->as.varName);
            if (!var) {
                fprintf(stderr, "Runtime Error: Undefined variable '%s'\n", expr->as.varName);
                exit(1);
            }
            return *var;
        }
            
        case EXPR_BINARY:
            return evaluateBinaryExpr(&expr->as.binary);
            
        case EXPR_UNARY: {
            Value operand = evaluateExpr(expr->as.unary.operand);
            if (expr->as.unary.op.type == TOKEN_NOT) {
                return createBoolValue(!operand.as.boolVal);
            } else if (expr->as.unary.op.type == TOKEN_MINUS) {
                if (operand.type == VAL_INT) {
                    return createIntValue(-operand.as.intVal);
                } else if (operand.type == VAL_DOUBLE) {
                    return createDoubleValue(-operand.as.doubleVal);
                }
            }
            break;
        }
            
        case EXPR_CALL:
            return evaluateCallExpr(&expr->as.call);
            
        case EXPR_LIST: {
            Value list = createListValue();
            for (int i = 0; i < expr->as.list.count; i++) {
                Value elem = evaluateExpr(expr->as.list.elements[i]);
                appendToList(&list, elem);
            }
            return list;
        }
            
        case EXPR_INDEX: {
            Value list = evaluateExpr(expr->as.index.list);
            Value index = evaluateExpr(expr->as.index.index);
            return getListElement(&list, index.as.intVal);
        }
            
        case EXPR_RANGE: {
            Value start = evaluateExpr(expr->as.range.start);
            Value end = evaluateExpr(expr->as.range.end);
            Value list = createListValue();
            for (int i = start.as.intVal; i < end.as.intVal; i++) {
                appendToList(&list, createIntValue(i));
            }
            return list;
        }
            
        case EXPR_COMPTIME:
            return evaluateExpr(expr->as.comptime.expression);
    }
    
    return createNullValue();
}

void executeStmt(Stmt *stmt) {
    if (runtime.frames[runtime.frameCount - 1].returnFlag) return;
    
    switch (stmt->type) {
        case STMT_SPAWN: {
            // Track spawned function
            char taskName[300];
            snprintf(taskName, sizeof(taskName), "spawn_%s", stmt->as.spawn.call.name);
            
            // Execute spawned function (simplified - no actual threading)
            evaluateCallExpr(&stmt->as.spawn.call);
            
            // Check for race conditions
            if (detectRaceCondition()) {
                fprintf(stderr, "\n============================================\n");
                fprintf(stderr, "RACE CONDITION DETECTED!\n");
                fprintf(stderr, "============================================\n");
                fprintf(stderr, "Tasks '%s' and '%s'\n", taskSystem.raceTask1, taskSystem.raceTask2);
                fprintf(stderr, "Both access variable '%s'\n", taskSystem.raceVar);
                fprintf(stderr, "At least one task writes to it.\n");
                fprintf(stderr, "Program terminated for safety.\n");
                fprintf(stderr, "============================================\n");
                exit(1);
            }
            break;
        }
            
        case STMT_IMPORT:
            break;
            
        case STMT_EXPR:
            evaluateExpr(stmt->as.expression);
            break;
            
        case STMT_VAR_DECL: {
            Value val;
            if (stmt->as.varDecl.initializer) {
                val = evaluateExpr(stmt->as.varDecl.initializer);
            } else {
                if (stmt->as.varDecl.varType == TOKEN_LIST) {
                    val = createListValue();
                } else {
                    val = createIntValue(0);
                }
            }
            
            // Use global or local scope based on flag
            if (stmt->as.varDecl.isGlobal) {
                setGlobalVariable(stmt->as.varDecl.name, val);
            } else {
                setVariable(stmt->as.varDecl.name, val);
            }
            break;
        }
            
        case STMT_ASSIGN: {
            Value val = evaluateExpr(stmt->as.assign.value);
            setVariable(stmt->as.assign.name, val);
            break;
        }
            
        case STMT_COMPOUND_ASSIGN: {
            Value *var = findVariable(stmt->as.compoundAssign.name);
            Value right = evaluateExpr(stmt->as.compoundAssign.value);
            
            if (stmt->as.compoundAssign.op.type == TOKEN_PLUS_ASSIGN) {
                if (var->type == VAL_INT && right.type == VAL_INT) {
                    var->as.intVal += right.as.intVal;
                }
            } else if (stmt->as.compoundAssign.op.type == TOKEN_MINUS_ASSIGN) {
                if (var->type == VAL_INT && right.type == VAL_INT) {
                    var->as.intVal -= right.as.intVal;
                }
            }
            break;
        }
            
        case STMT_RETURN: {
            StackFrame *frame = &runtime.frames[runtime.frameCount - 1];
            frame->returnFlag = 1;
            if (stmt->as.returnValue) {
                frame->returnValue = evaluateExpr(stmt->as.returnValue);
            } else {
                frame->returnValue = createNullValue();
            }
            break;
        }
            
        case STMT_IF: {
            Value condition = evaluateExpr(stmt->as.ifStmt.condition);
            if (condition.as.boolVal) {
                executeStmt(stmt->as.ifStmt.thenBranch);
            } else if (stmt->as.ifStmt.elseBranch) {
                executeStmt(stmt->as.ifStmt.elseBranch);
            }
            break;
        }
            
        case STMT_WHILE: {
            while (true) {
                Value condition = evaluateExpr(stmt->as.whileStmt.condition);
                if (!condition.as.boolVal) break;
                executeStmt(stmt->as.whileStmt.body);
                if (runtime.frames[runtime.frameCount - 1].returnFlag) break;
            }
            break;
        }
            
        case STMT_FOR: {
            Value iterable = evaluateExpr(stmt->as.forStmt.iterable);
            if (iterable.type == VAL_LIST) {
                for (int i = 0; i < iterable.as.listVal.count; i++) {
                    setVariable(stmt->as.forStmt.varName, iterable.as.listVal.elements[i]);
                    executeStmt(stmt->as.forStmt.body);
                    if (runtime.frames[runtime.frameCount - 1].returnFlag) break;
                }
            }
            break;
        }
            
        case STMT_BLOCK:
            for (int i = 0; i < stmt->as.block.count; i++) {
                executeStmt(stmt->as.block.statements[i]);
                if (runtime.frames[runtime.frameCount - 1].returnFlag) break;
            }
            break;
            
        case STMT_PRINT: {
            Value val = evaluateExpr(stmt->as.printExpr);
            printValue(val);
            printf("\n");
            break;
        }
            
        case STMT_LIST_APPEND: {
            Value *list = findVariable(stmt->as.listAppend.listName);
            Value item = evaluateExpr(stmt->as.listAppend.value);
            appendToList(list, item);
            break;
        }
        
        case STMT_INDEX_ASSIGN: {
            // NEW: Handle list[index] = value
            Value *list = findVariable(stmt->as.indexAssign.listName);
            if (!list || list->type != VAL_LIST) {
                fprintf(stderr, "Runtime Error: '%s' is not a list\n", stmt->as.indexAssign.listName);
                exit(1);
            }
            
            Value indexVal = evaluateExpr(stmt->as.indexAssign.index);
            if (indexVal.type != VAL_INT) {
                fprintf(stderr, "Runtime Error: List index must be an integer\n");
                exit(1);
            }
            
            int index = indexVal.as.intVal;
            if (index < 0 || index >= list->as.listVal.count) {
                fprintf(stderr, "Runtime Error: List index out of bounds\n");
                exit(1);
            }
            
            Value newValue = evaluateExpr(stmt->as.indexAssign.value);
            list->as.listVal.elements[index] = newValue;
            break;
        }
    }
}

/* ============================================================================
 * File Parsing
 * ============================================================================ */

void parseFile(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        fprintf(stderr, "Error: Could not open file '%s'\n", filename);
        exit(1);
    }
    
    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char *source = malloc(fileSize + 1);
    size_t bytesRead = fread(source, 1, fileSize, file);
    source[bytesRead] = '\0';
    fclose(file);
    
    Lexer lexer;
    initLexer(&lexer, source);
    scanTokens(&lexer);
    
    Parser parser;
    initParser(&parser, lexer.tokens, lexer.tokenCount);
    
    while (!isAtEndParser(&parser)) {
        Function func = parseFunction(&parser);
        if (func.name[0] != '\0') {
            runtime.functions[runtime.functionCount++] = func;
        }
    }
    
    free(source);
}

/* ============================================================================
 * Main Entry Point
 * ============================================================================ */

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "The Source Programming Langage (src)\n");
        return 1;
    }
    
    // Initialize all systems
    initImportTracker();
    initComptime();
    initTaskSystem();
    initRealTimeContext();  // NEW
    
    // Initialize runtime
    runtime.functionCount = 0;
    runtime.frameCount = 1;
    runtime.frames[0].env.count = 0;
    runtime.frames[0].returnFlag = 0;
    runtime.globalEnv.count = 0;  // NEW: Initialize global environment
    
    // Parse and execute
    parseFile(argv[1]);
    
    Function *main = findFunction("main");
    if (!main) {
        fprintf(stderr, "Error: No main function found\n");
        return 1;
    }
    
    CallExpr mainCall;
    strcpy(mainCall.name, "main");
    mainCall.argCount = 0;
    mainCall.args = NULL;
    
    evaluateCallExpr(&mainCall);
    
    return 0;
}

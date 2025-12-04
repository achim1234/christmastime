// christmastime.c
// Minimal ChristmasTime interpreter (single-file)
// Features: say, gift (var), if/else, jingle N times { ... }, expressions, strings, comments
// Compile: gcc -std=c99 -Wall -Wextra christmastime.c -o christmastime

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* -------------------------
   Lexer
   ------------------------- */

typedef enum {
    TOK_EOF,
    TOK_NUMBER,
    TOK_STRING,
    TOK_IDENT,
    TOK_LBRACE, TOK_RBRACE,
    TOK_LPAREN, TOK_RPAREN,
    TOK_PLUS, TOK_MINUS, TOK_STAR, TOK_SLASH,
    TOK_BANG, TOK_BANG_EQUAL,
    TOK_EQ, TOK_EQUAL_EQUAL,
    TOK_LT, TOK_LTE,
    TOK_GT, TOK_GTE,
    TOK_SEMICOLON,
    TOK_KEY_SAY,
    TOK_KEY_GIFT,
    TOK_KEY_IF,
    TOK_KEY_ELSE,
    TOK_KEY_JINGLE,
    TOK_KEY_TIMES,
    TOK_TRUE,
    TOK_FALSE,
    TOK_UNKNOWN
} TokenType;

typedef struct {
    TokenType type;
    char *lexeme;
    double number;
} Token;

typedef struct {
    const char *src;
    size_t i;
    size_t len;
} Lexer;

int starts_with(const char *s, const char *p) {
    return strncmp(s, p, strlen(p)) == 0;
}

void lexer_init(Lexer *L, const char *src) {
    L->src = src;
    L->i = 0;
    L->len = strlen(src);
}

void push_tok(Token **arr, int *cap, int *len, Token t) {
    if (*len + 1 >= *cap) {
        *cap = (*cap == 0) ? 16 : *cap * 2;
        *arr = realloc(*arr, (*cap) * sizeof(Token));
    }
    (*arr)[(*len)++] = t;
}

void skip_ws(Lexer *L) {
    while (L->i < L->len) {
        char c = L->src[L->i];
        // comment
        if (c == '#' ) {
            while (L->i < L->len && L->src[L->i] != '\n') L->i++;
            continue;
        }
        if (isspace((unsigned char)c)) { L->i++; continue; }
        break;
    }
}

char peek(Lexer *L) {
    if (L->i >= L->len) return '\0';
    return L->src[L->i];
}
char peek2(Lexer *L) {
    if (L->i + 1 >= L->len) return '\0';
    return L->src[L->i + 1];
}
char advance(Lexer *L) {
    if (L->i >= L->len) return '\0';
    return L->src[L->i++];
}

Token make_tok(TokenType t, const char *lex, double number) {
    Token tok;
    tok.type = t;
    if (lex) tok.lexeme = strdup(lex);
    else tok.lexeme = NULL;
    tok.number = number;
    return tok;
}

int is_ident_start(char c) {
    return isalpha((unsigned char)c) || c == '_';
}
int is_ident_part(char c) {
    return isalnum((unsigned char)c) || c == '_';
}

Token *lex_all(const char *src, int *out_len) {
    Lexer L;
    lexer_init(&L, src);
    Token *arr = NULL;
    int cap = 0, len = 0;

    while (1) {
        skip_ws(&L);
        if (L.i >= L.len) break;
        char c = advance(&L);

        if (c == '{') { push_tok(&arr,&cap,&len, make_tok(TOK_LBRACE, "{", 0)); continue; }
        if (c == '}') { push_tok(&arr,&cap,&len, make_tok(TOK_RBRACE, "}", 0)); continue; }
        if (c == '(') { push_tok(&arr,&cap,&len, make_tok(TOK_LPAREN, "(", 0)); continue; }
        if (c == ')') { push_tok(&arr,&cap,&len, make_tok(TOK_RPAREN, ")", 0)); continue; }
        if (c == '+') { push_tok(&arr,&cap,&len, make_tok(TOK_PLUS, "+", 0)); continue; }
        if (c == '-') { push_tok(&arr,&cap,&len, make_tok(TOK_MINUS, "-", 0)); continue; }
        if (c == '*') { push_tok(&arr,&cap,&len, make_tok(TOK_STAR, "*", 0)); continue; }
        if (c == '/') { push_tok(&arr,&cap,&len, make_tok(TOK_SLASH, "/", 0)); continue; }
        if (c == ';') { push_tok(&arr,&cap,&len, make_tok(TOK_SEMICOLON, ";", 0)); continue; }

        if (c == '!') {
            if (peek(&L) == '=') { advance(&L); push_tok(&arr,&cap,&len, make_tok(TOK_BANG_EQUAL, "!=", 0)); }
            else push_tok(&arr,&cap,&len, make_tok(TOK_BANG, "!", 0));
            continue;
        }
        if (c == '=') {
            if (peek(&L) == '=') { advance(&L); push_tok(&arr,&cap,&len, make_tok(TOK_EQUAL_EQUAL, "==", 0)); }
            else push_tok(&arr,&cap,&len, make_tok(TOK_EQ, "=", 0));
            continue;
        }
        if (c == '<') {
            if (peek(&L) == '=') { advance(&L); push_tok(&arr,&cap,&len, make_tok(TOK_LTE, "<=", 0)); }
            else push_tok(&arr,&cap,&len, make_tok(TOK_LT, "<", 0));
            continue;
        }
        if (c == '>') {
            if (peek(&L) == '=') { advance(&L); push_tok(&arr,&cap,&len, make_tok(TOK_GTE, ">=", 0)); }
            else push_tok(&arr,&cap,&len, make_tok(TOK_GT, ">", 0));
            continue;
        }

        if (c == '"') {
            // string literal
            size_t start = L.i;
            while (L.i < L.len && L.src[L.i] != '"') {
                if (L.src[L.i] == '\\' && L.i + 1 < L.len) L.i += 2;
                else L.i++;
            }
            size_t end = L.i;
            if (peek(&L) == '"') advance(&L);
            size_t slen = end - start;
            char *buf = malloc(slen + 1);
            size_t bi = 0;
            for (size_t k = start; k < end; ++k) {
                if (L.src[k] == '\\' && k + 1 < end) {
                    k++;
                    char esc = L.src[k];
                    if (esc == 'n') buf[bi++] = '\n';
                    else if (esc == 't') buf[bi++] = '\t';
                    else buf[bi++] = esc;
                } else buf[bi++] = L.src[k];
            }
            buf[bi] = '\0';
            push_tok(&arr,&cap,&len, make_tok(TOK_STRING, buf, 0));
            free(buf);
            continue;
        }

        if (isdigit((unsigned char)c) || (c == '.' && isdigit((unsigned char)peek(&L)))) {
            size_t start = L.i - 1;
            while (isdigit((unsigned char)peek(&L))) advance(&L);
            if (peek(&L) == '.') { advance(&L); while (isdigit((unsigned char)peek(&L))) advance(&L); }
            size_t end = L.i;
            size_t slen = end - start;
            char *buf = malloc(slen + 1);
            memcpy(buf, L.src + start, slen);
            buf[slen] = '\0';
            double v = atof(buf);
            push_tok(&arr,&cap,&len, make_tok(TOK_NUMBER, buf, v));
            free(buf);
            continue;
        }

        if (is_ident_start(c)) {
            size_t start = L.i - 1;
            while (is_ident_part(peek(&L))) advance(&L);
            size_t end = L.i;
            size_t slen = end - start;
            char *buf = malloc(slen + 1);
            memcpy(buf, L.src + start, slen);
            buf[slen] = '\0';
            // keywords
            TokenType t = TOK_IDENT;
            if (strcmp(buf, "say") == 0) t = TOK_KEY_SAY;
            else if (strcmp(buf, "gift") == 0) t = TOK_KEY_GIFT;
            else if (strcmp(buf, "if") == 0) t = TOK_KEY_IF;
            else if (strcmp(buf, "else") == 0) t = TOK_KEY_ELSE;
            else if (strcmp(buf, "jingle") == 0) t = TOK_KEY_JINGLE;
            else if (strcmp(buf, "times") == 0) t = TOK_KEY_TIMES;
            else if (strcmp(buf, "true") == 0) t = TOK_TRUE;
            else if (strcmp(buf, "false") == 0) t = TOK_FALSE;

            push_tok(&arr,&cap,&len, make_tok(t, buf, 0));
            free(buf);
            continue;
        }

        // unknown char -> skip
    }

    push_tok(&arr,&cap,&len, make_tok(TOK_EOF, "EOF", 0));
    *out_len = len;
    return arr;
}

/* -------------------------
   Parser (recursive descent)
   ------------------------- */

/* Value types and interpreter structures */

typedef enum { VAL_NIL, VAL_NUMBER, VAL_BOOL, VAL_STRING } ValueType;
typedef struct {
    ValueType type;
    double number;
    int boolean;
    char *string;
} Value;

Value make_number(double n) { Value v; v.type = VAL_NUMBER; v.number = n; v.boolean = 0; v.string = NULL; return v; }
Value make_bool(int b) { Value v; v.type = VAL_BOOL; v.boolean = b; v.number = 0; v.string = NULL; return v; }
Value make_string(const char *s) { Value v; v.type = VAL_STRING; v.string = strdup(s); v.number = 0; v.boolean = 0; return v; }
Value make_nil() { Value v; v.type = VAL_NIL; v.number = 0; v.boolean = 0; v.string = NULL; return v; }
void free_value(Value *v) {
    if (v->type == VAL_STRING && v->string) free(v->string);
    v->string = NULL;
}

/* Environment - simple map */
typedef struct Var {
    char *name;
    Value val;
    struct Var *next;
} Var;

Var *env = NULL;

void env_set(const char *name, Value v) {
    Var *cur = env;
    while (cur) {
        if (strcmp(cur->name, name) == 0) {
            free_value(&cur->val);
            cur->val = v;
            return;
        }
        cur = cur->next;
    }
    Var *n = malloc(sizeof(Var));
    n->name = strdup(name);
    n->val = v;
    n->next = env;
    env = n;
}

int env_exists(const char *name) {
    Var *cur = env;
    while (cur) {
        if (strcmp(cur->name, name) == 0) return 1;
        cur = cur->next;
    }
    return 0;
}

Value env_get(const char *name) {
    Var *cur = env;
    while (cur) {
        if (strcmp(cur->name, name) == 0) return cur->val;
        cur = cur->next;
    }
    fprintf(stderr, "Runtime error: undefined variable '%s'\n", name);
    return make_nil();
}

/* AST nodes */
typedef enum {
    EXPR_LITERAL, EXPR_VAR, EXPR_BINARY, EXPR_UNARY, EXPR_GROUP
} ExprType;

typedef struct Expr {
    ExprType type;
    Value literal; // for literal
    char *name; // for var
    // binary
    TokenType op;
    struct Expr *left;
    struct Expr *right;
    // unary
    struct Expr *right_un;
} Expr;

typedef enum {
    STMT_PRINT, STMT_VARDECL, STMT_IF, STMT_BLOCK, STMT_LOOP, STMT_EXPR
} StmtType;

typedef struct Stmt {
    StmtType type;
    // print
    Expr *expr;
    // vardecl
    char *varname;
    Expr *init;
    // if
    Expr *cond;
    struct Stmt *thenBranch;
    struct Stmt *elseBranch;
    // block
    struct Stmt **stmts;
    int stmt_count;
    // loop
    Expr *countExpr;
} Stmt;

/* Parser state */
typedef struct {
    Token *tokens;
    int len;
    int idx;
} Parser;

Token *cur_tok(Parser *P) { if (P->idx < P->len) return &P->tokens[P->idx]; return NULL; }
Token *peek_tok(Parser *P, int n) { if (P->idx + n < P->len) return &P->tokens[P->idx + n]; return NULL; }
Token advance_tok(Parser *P) { if (P->idx < P->len) return P->tokens[P->idx++]; return P->tokens[P->len-1]; }
int match_tok(Parser *P, TokenType t) {
    if (P->idx < P->len && P->tokens[P->idx].type == t) { P->idx++; return 1; }
    return 0;
}
int check_tok(Parser *P, TokenType t) {
    if (P->idx < P->len && P->tokens[P->idx].type == t) return 1;
    return 0;
}

/* forward */
Expr *parse_expression(Parser *P);

Expr *expr_literal(Value v) {
    Expr *e = malloc(sizeof(Expr));
    e->type = EXPR_LITERAL;
    e->literal = v;
    e->left = e->right = NULL; e->name = NULL; e->right_un = NULL;
    return e;
}
Expr *expr_var(const char *name) {
    Expr *e = malloc(sizeof(Expr));
    e->type = EXPR_VAR;
    e->name = strdup(name);
    e->left = e->right = NULL; e->right_un = NULL;
    return e;
}
Expr *expr_unary(TokenType op, Expr *right) {
    Expr *e = malloc(sizeof(Expr));
    e->type = EXPR_UNARY;
    e->op = op;
    e->right_un = right;
    e->left = e->right = NULL; e->name = NULL;
    return e;
}
Expr *expr_binary(Expr *left, TokenType op, Expr *right) {
    Expr *e = malloc(sizeof(Expr));
    e->type = EXPR_BINARY;
    e->left = left; e->right = right; e->op = op; e->name = NULL;
    return e;
}
Expr *expr_group(Expr *inner) {
    Expr *e = malloc(sizeof(Expr));
    e->type = EXPR_GROUP;
    e->left = inner; e->right = NULL; e->name = NULL; e->right_un = NULL;
    return e;
}

/* Parser helpers for precedence */

Expr *parse_primary(Parser *P) {
    Token *t = cur_tok(P);
    if (!t) return NULL;
    if (match_tok(P, TOK_NUMBER)) {
        return expr_literal(make_number(t->number));
    }
    if (match_tok(P, TOK_STRING)) {
        return expr_literal(make_string(t->lexeme));
    }
    if (match_tok(P, TOK_TRUE)) {
        return expr_literal(make_bool(1));
    }
    if (match_tok(P, TOK_FALSE)) {
        return expr_literal(make_bool(0));
    }
    if (match_tok(P, TOK_LPAREN)) {
        Expr *e = parse_expression(P);
        match_tok(P, TOK_RPAREN);
        return expr_group(e);
    }
    if (match_tok(P, TOK_IDENT)) {
        return expr_var(t->lexeme);
    }
    if (match_tok(P, TOK_KEY_SAY)) {
        // 'say' as primary only used in statements; fallback
        return expr_literal(make_nil());
    }
    // fallback
    return expr_literal(make_nil());
}

Expr *parse_unary(Parser *P) {
    if (match_tok(P, TOK_BANG)) {
        Expr *right = parse_unary(P);
        return expr_unary(TOK_BANG, right);
    }
    if (match_tok(P, TOK_MINUS)) {
        Expr *right = parse_unary(P);
        return expr_unary(TOK_MINUS, right);
    }
    return parse_primary(P);
}

Expr *parse_factor(Parser *P) {
    Expr *e = parse_unary(P);
    while (check_tok(P, TOK_STAR) || check_tok(P, TOK_SLASH)) {
        Token t = advance_tok(P);
        Expr *right = parse_unary(P);
        e = expr_binary(e, t.type, right);
    }
    return e;
}

Expr *parse_term(Parser *P) {
    Expr *e = parse_factor(P);
    while (check_tok(P, TOK_PLUS) || check_tok(P, TOK_MINUS)) {
        Token t = advance_tok(P);
        Expr *right = parse_factor(P);
        e = expr_binary(e, t.type, right);
    }
    return e;
}

Expr *parse_comparison(Parser *P) {
    Expr *e = parse_term(P);
    while (check_tok(P, TOK_GT) || check_tok(P, TOK_GTE) || check_tok(P, TOK_LT) || check_tok(P, TOK_LTE)) {
        Token t = advance_tok(P);
        Expr *right = parse_term(P);
        e = expr_binary(e, t.type, right);
    }
    return e;
}

Expr *parse_equality(Parser *P) {
    Expr *e = parse_comparison(P);
    while (check_tok(P, TOK_EQUAL_EQUAL) || check_tok(P, TOK_BANG_EQUAL)) {
        Token t = advance_tok(P);
        Expr *right = parse_comparison(P);
        e = expr_binary(e, t.type, right);
    }
    return e;
}

Expr *parse_expression(Parser *P) {
    return parse_equality(P);
}

/* Statements */

Stmt *make_stmt(StmtType t) {
    Stmt *s = malloc(sizeof(Stmt));
    memset(s, 0, sizeof(Stmt));
    s->type = t;
    return s;
}

Stmt *parse_block(Parser *P);

Stmt *parse_statement(Parser *P) {
    Token *t = cur_tok(P);
    if (!t) return NULL;
    if (match_tok(P, TOK_KEY_SAY)) {
        Stmt *s = make_stmt(STMT_PRINT);
        s->expr = parse_expression(P);
        return s;
    }
    if (match_tok(P, TOK_KEY_GIFT)) {
        // var decl: gift name = expr
        Token *ident = cur_tok(P);
        if (match_tok(P, TOK_IDENT)) {
            char *name = strdup(ident->lexeme);
            match_tok(P, TOK_EQ);
            Expr *init = parse_expression(P);
            Stmt *s = make_stmt(STMT_VARDECL);
            s->varname = name;
            s->init = init;
            return s;
        } else {
            fprintf(stderr, "Parse error: expected identifier after 'gift'\n");
            return NULL;
        }
    }
    if (match_tok(P, TOK_KEY_IF)) {
        Expr *cond = parse_expression(P);
        Stmt *thenb = parse_block(P);
        Stmt *elseb = NULL;
        if (match_tok(P, TOK_KEY_ELSE)) {
            elseb = parse_block(P);
        }
        Stmt *s = make_stmt(STMT_IF);
        s->cond = cond; s->thenBranch = thenb; s->elseBranch = elseb;
        return s;
    }
    if (match_tok(P, TOK_KEY_JINGLE)) {
        Expr *count = parse_expression(P);
        if (!match_tok(P, TOK_KEY_TIMES)) {
            fprintf(stderr, "Parse error: expected 'times' after jingle count\n");
            return NULL;
        }
        Stmt *body = parse_block(P);
        Stmt *s = make_stmt(STMT_LOOP);
        s->countExpr = count; s->thenBranch = body;
        return s;
    }
    if (match_tok(P, TOK_LBRACE)) {
        // backtrack: we consumed LBRACE; parse block variant
        P->idx--; // step back so parse_block sees it
        return parse_block(P);
    }
    // default: expression statement (rare)
    Expr *e = parse_expression(P);
    Stmt *s = make_stmt(STMT_EXPR);
    s->expr = e;
    return s;
}

Stmt *parse_block(Parser *P) {
    if (!match_tok(P, TOK_LBRACE)) {
        fprintf(stderr, "Parse error: expected '{' for block\n");
        return NULL;
    }
    // collect statements until RBRACE
    Stmt **list = NULL;
    int cap = 0, cnt = 0;
    while (!check_tok(P, TOK_RBRACE) && !check_tok(P, TOK_EOF)) {
        Stmt *st = parse_statement(P);
        if (!st) break;
        if (cnt + 1 >= cap) {
            cap = (cap == 0) ? 8 : cap * 2;
            list = realloc(list, sizeof(Stmt*) * cap);
        }
        list[cnt++] = st;
    }
    match_tok(P, TOK_RBRACE);
    Stmt *s = make_stmt(STMT_BLOCK);
    s->stmts = list;
    s->stmt_count = cnt;
    return s;
}

Stmt **parse_program(Parser *P, int *out_count) {
    Stmt **list = NULL;
    int cap = 0, cnt = 0;
    while (!check_tok(P, TOK_EOF)) {
        Stmt *s = parse_statement(P);
        if (!s) break;
        if (cnt + 1 >= cap) {
            cap = (cap == 0) ? 16 : cap * 2;
            list = realloc(list, sizeof(Stmt*) * cap);
        }
        list[cnt++] = s;
    }
    *out_count = cnt;
    return list;
}

/* -------------------------
   Interpreter / Evaluator
   ------------------------- */

int is_truthy(Value v) {
    if (v.type == VAL_NIL) return 0;
    if (v.type == VAL_BOOL) return v.boolean;
    if (v.type == VAL_NUMBER) return v.number != 0;
    if (v.type == VAL_STRING) return strlen(v.string) > 0;
    return 0;
}

char *value_to_string(Value v) {
    char buf[256];
    if (v.type == VAL_NUMBER) {
        // remove trailing .0 if integer
        if (fabs(round(v.number) - v.number) < 1e-9) {
            snprintf(buf, sizeof(buf), "%.0f", v.number);
        } else {
            snprintf(buf, sizeof(buf), "%g", v.number);
        }
        return strdup(buf);
    } else if (v.type == VAL_BOOL) {
        return strdup(v.boolean ? "true" : "false");
    } else if (v.type == VAL_STRING) {
        return strdup(v.string ? v.string : "");
    } else {
        return strdup("nil");
    }
}

Value eval_expr(Expr *e);

Value eval_binary(Expr *e) {
    Value left = eval_expr(e->left);
    Value right = eval_expr(e->right);

    if (e->op == TOK_PLUS) {
        // If either is string -> concat
        if (left.type == VAL_STRING || right.type == VAL_STRING) {
            char *ls = value_to_string(left);
            char *rs = value_to_string(right);
            size_t n = strlen(ls) + strlen(rs) + 1;
            char *buf = malloc(n);
            strcpy(buf, ls);
            strcat(buf, rs);
            Value r = make_string(buf);
            free(ls); free(rs); free(buf);
            free_value(&left); free_value(&right);
            return r;
        } else {
            double ln = (left.type == VAL_NUMBER) ? left.number : 0;
            double rn = (right.type == VAL_NUMBER) ? right.number : 0;
            free_value(&left); free_value(&right);
            return make_number(ln + rn);
        }
    }

    if (e->op == TOK_MINUS || e->op == TOK_STAR || e->op == TOK_SLASH) {
        double ln = (left.type == VAL_NUMBER) ? left.number : 0;
        double rn = (right.type == VAL_NUMBER) ? right.number : 0;
        double res = 0;
        if (e->op == TOK_MINUS) res = ln - rn;
        if (e->op == TOK_STAR) res = ln * rn;
        if (e->op == TOK_SLASH) res = rn == 0 ? 0 : ln / rn;
        free_value(&left); free_value(&right);
        return make_number(res);
    }

    if (e->op == TOK_EQUAL_EQUAL || e->op == TOK_BANG_EQUAL) {
        int eq = 0;
        if (left.type == VAL_NUMBER && right.type == VAL_NUMBER) eq = (left.number == right.number);
        else {
            char *ls = value_to_string(left);
            char *rs = value_to_string(right);
            eq = strcmp(ls, rs) == 0;
            free(ls); free(rs);
        }
        free_value(&left); free_value(&right);
        if (e->op == TOK_BANG_EQUAL) eq = !eq;
        return make_bool(eq);
    }

    if (e->op == TOK_GT || e->op == TOK_GTE || e->op == TOK_LT || e->op == TOK_LTE) {
        double ln = (left.type == VAL_NUMBER) ? left.number : 0;
        double rn = (right.type == VAL_NUMBER) ? right.number : 0;
        int res = 0;
        if (e->op == TOK_GT) res = ln > rn;
        if (e->op == TOK_GTE) res = ln >= rn;
        if (e->op == TOK_LT) res = ln < rn;
        if (e->op == TOK_LTE) res = ln <= rn;
        free_value(&left); free_value(&right);
        return make_bool(res);
    }

    // fallback
    free_value(&left); free_value(&right);
    return make_nil();
}

Value eval_unary(Expr *e) {
    Value r = eval_expr(e->right_un);
    if (e->op == TOK_BANG) {
        int truth = is_truthy(r);
        free_value(&r);
        return make_bool(!truth);
    }
    if (e->op == TOK_MINUS) {
        double rn = (r.type == VAL_NUMBER) ? r.number : 0;
        free_value(&r);
        return make_number(-rn);
    }
    free_value(&r);
    return make_nil();
}

Value eval_expr(Expr *e) {
    if (!e) return make_nil();
    switch (e->type) {
        case EXPR_LITERAL: {
            // IMPORTANT: if the literal is a string, create a copy
            // so that runtime frees don't corrupt the AST-owned string.
            if (e->literal.type == VAL_STRING) {
                return make_string(e->literal.string ? e->literal.string : "");
            } else {
                // numbers/bools/nil are plain values — safe to return copy
                return e->literal;
            }
        }
        case EXPR_VAR: return env_get(e->name);
        case EXPR_GROUP: return eval_expr(e->left);
        case EXPR_UNARY: return eval_unary(e);
        case EXPR_BINARY: return eval_binary(e);
    }
    return make_nil();
}

/* execute statements */

void exec_stmt(Stmt *s);

void exec_block(Stmt *block) {
    if (!block) return;
    for (int i = 0; i < block->stmt_count; ++i) {
        exec_stmt(block->stmts[i]);
    }
}

void exec_stmt(Stmt *s) {
    if (!s) return;
    if (s->type == STMT_PRINT) {
        Value v = eval_expr(s->expr);
        char *out = value_to_string(v);
        printf("%s\n", out);
        free(out);
        free_value(&v);
        return;
    }
    if (s->type == STMT_VARDECL) {
        Value v = eval_expr(s->init);
        env_set(s->varname, v);
        return;
    }
    if (s->type == STMT_IF) {
        Value cond = eval_expr(s->cond);
        if (is_truthy(cond)) {
            exec_block(s->thenBranch);
        } else {
            exec_block(s->elseBranch);
        }
        free_value(&cond);
        return;
    }
    if (s->type == STMT_BLOCK) {
        exec_block(s);
        return;
    }
    if (s->type == STMT_LOOP) {
        Value cntv = eval_expr(s->countExpr);
        double n = (cntv.type == VAL_NUMBER) ? cntv.number : 0;
        int times = (int)floor(n);
        for (int i = 0; i < times; ++i) {
            exec_block(s->thenBranch);
        }
        free_value(&cntv);
        return;
    }
    if (s->type == STMT_EXPR) {
        Value v = eval_expr(s->expr);
        free_value(&v);
        return;
    }
}

/* -------------------------
   Utilities: free AST
   ------------------------- */

void free_expr(Expr *e) {
    if (!e) return;
    if (e->type == EXPR_LITERAL) {
        // literal might have string allocated; free when appropriate
        // The AST owns this copy of the literal, so free here.
        if (e->literal.type == VAL_STRING && e->literal.string) {
            free(e->literal.string);
            e->literal.string = NULL;
        }
    }
    if (e->name) free(e->name);
    if (e->left) free_expr(e->left);
    if (e->right) free_expr(e->right);
    if (e->right_un) free_expr(e->right_un);
    free(e);
}

void free_stmt(Stmt *s) {
    if (!s) return;
    if (s->type == STMT_PRINT || s->type == STMT_EXPR) {
        free_expr(s->expr);
    }
    if (s->type == STMT_VARDECL) {
        free(s->varname);
        free_expr(s->init);
    }
    if (s->type == STMT_IF) {
        free_expr(s->cond);
        free_stmt(s->thenBranch);
        free_stmt(s->elseBranch);
    }
    if (s->type == STMT_BLOCK) {
        for (int i = 0; i < s->stmt_count; ++i) free_stmt(s->stmts[i]);
        free(s->stmts);
    }
    if (s->type == STMT_LOOP) {
        free_expr(s->countExpr);
        free_stmt(s->thenBranch);
    }
    free(s);
}

/* -------------------------
   Runner & REPL
   ------------------------- */

void run_source(const char *src) {
    int toks_len = 0;
    Token *toks = lex_all(src, &toks_len);
    Parser P = { toks, toks_len, 0 };
    int stmt_count = 0;
    Stmt **program = parse_program(&P, &stmt_count);
    for (int i = 0; i < stmt_count; ++i) {
        exec_stmt(program[i]);
    }
    for (int i = 0; i < stmt_count; ++i) free_stmt(program[i]);
    free(program);
    // free tokens
    for (int i = 0; i < toks_len; ++i) if (toks[i].lexeme) free(toks[i].lexeme);
    free(toks);
}

char *read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(sz + 1);
    fread(buf, 1, sz, f);
    buf[sz] = '\0';
    fclose(f);
    return buf;
}

void repl() {
    printf("ChristmasTime REPL — type code. Use '{ }' for blocks. Ctrl+C to exit.\n");
    char line[4096];
    char *buffer = NULL;
    size_t buf_sz = 0;
    while (1) {
        printf("> ");
        if (!fgets(line, sizeof(line), stdin)) break;
        // accumulate lines until braces balanced or single-line statement
        size_t need = strlen(line);
        buffer = realloc(buffer, buf_sz + need + 1);
        memcpy(buffer + buf_sz, line, need);
        buf_sz += need;
        buffer[buf_sz] = '\0';
        // quick heuristic: if braces balanced and not in middle of string, run
        int balance = 0;
        int in_str = 0;
        for (size_t i = 0; i < buf_sz; ++i) {
            char c = buffer[i];
            if (c == '"' && (i == 0 || buffer[i-1] != '\\')) in_str = !in_str;
            if (!in_str) {
                if (c == '{') balance++;
                if (c == '}') balance--;
            }
        }
        if (balance <= 0) {
            // run
            run_source(buffer);
            free(buffer);
            buffer = NULL;
            buf_sz = 0;
        } else {
            // continue reading
            continue;
        }
    }
    if (buffer) free(buffer);
}

/* -------------------------
   Main
   ------------------------- */

int main(int argc, char **argv) {
    if (argc >= 2) {
        char *src = read_file(argv[1]);
        if (!src) {
            fprintf(stderr, "Could not open file: %s\n", argv[1]);
            return 1;
        }
        run_source(src);
        free(src);
    } else {
        repl();
    }
    return 0;
}

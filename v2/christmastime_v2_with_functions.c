// christmastime.c
// Minimal ChristmasTime interpreter (single-file)
// Features: say, gift (var), if/else, jingle N times { ... }, expressions, strings, comments
// Added: hoho (function), return, function calls, simple frame scoping
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
    TOK_COMMA,
    TOK_KEY_SAY,
    TOK_KEY_GIFT,
    TOK_KEY_IF,
    TOK_KEY_ELSE,
    TOK_KEY_JINGLE,
    TOK_KEY_TIMES,
    TOK_TRUE,
    TOK_FALSE,
    TOK_KEY_HOHO,
    TOK_KEY_RETURN,
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
        if (c == ',') { push_tok(&arr,&cap,&len, make_tok(TOK_COMMA, ",", 0)); continue; }

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
            else if (strcmp(buf, "hoho") == 0) t = TOK_KEY_HOHO;
            else if (strcmp(buf, "return") == 0) t = TOK_KEY_RETURN;

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

typedef enum { VAL_NIL, VAL_NUMBER, VAL_BOOL, VAL_STRING, VAL_FUNCTION } ValueType;
typedef struct Stmt Stmt; // forward

typedef struct {
    char **params;
    int param_count;
    Stmt *body; // a block statement
    char *name; // optional name (for debugging)
} Function;

typedef struct {
    ValueType type;
    double number;
    int boolean;
    char *string;
    Function *fn;
} Value;

Value make_number(double n) { Value v; v.type = VAL_NUMBER; v.number = n; v.boolean = 0; v.string = NULL; v.fn = NULL; return v; }
Value make_bool(int b) { Value v; v.type = VAL_BOOL; v.boolean = b; v.number = 0; v.string = NULL; v.fn = NULL; return v; }
Value make_string(const char *s) { Value v; v.type = VAL_STRING; v.string = strdup(s); v.number = 0; v.boolean = 0; v.fn = NULL; return v; }
Value make_nil() { Value v; v.type = VAL_NIL; v.number = 0; v.boolean = 0; v.string = NULL; v.fn = NULL; return v; }
Value make_function(Function *f) { Value v; v.type = VAL_FUNCTION; v.fn = f; v.number = 0; v.boolean = 0; v.string = NULL; return v; }

void free_value(Value *v) {
    if (!v) return;
    if (v->type == VAL_STRING && v->string) { free(v->string); v->string = NULL; }
    if (v->type == VAL_FUNCTION && v->fn) {
        // free function struct and its contents
        for (int i = 0; i < v->fn->param_count; ++i) {
            if (v->fn->params[i]) free(v->fn->params[i]);
        }
        free(v->fn->params);
        if (v->fn->name) free(v->fn->name);
        // function->body is owned by AST and freed elsewhere via free_stmt
        free(v->fn);
        v->fn = NULL;
    }
    v->type = VAL_NIL;
}

/* Environment - frame stack */

typedef struct Var {
    char *name;
    Value val;
    struct Var *next;
} Var;

typedef struct Frame {
    Var *vars; // linked list of vars in this frame
    struct Frame *next; // next frame down
} Frame;

Frame *frames = NULL; // top frame

void push_frame() {
    Frame *f = malloc(sizeof(Frame));
    f->vars = NULL;
    f->next = frames;
    frames = f;
}

void pop_frame() {
    if (!frames) return;
    Frame *top = frames;
    // free vars in this frame
    Var *v = top->vars;
    while (v) {
        Var *n = v->next;
        if (v->name) free(v->name);
        free_value(&v->val);
        free(v);
        v = n;
    }
    frames = top->next;
    free(top);
}

void env_set_local_in_frame(Frame *f, const char *name, Value v) {
    Var *cur = f->vars;
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
    n->next = f->vars;
    f->vars = n;
}

int env_exists_in_frames(const char *name, Frame **out_frame, Var **out_var) {
    Frame *f = frames;
    while (f) {
        Var *v = f->vars;
        while (v) {
            if (strcmp(v->name, name) == 0) {
                if (out_frame) *out_frame = f;
                if (out_var) *out_var = v;
                return 1;
            }
            v = v->next;
        }
        f = f->next;
    }
    return 0;
}

void env_set(const char *name, Value v) {
    // if exists in any frame, set there; else set in top frame (or create global frame)
    Frame *found_frame = NULL;
    Var *found_var = NULL;
    if (env_exists_in_frames(name, &found_frame, &found_var)) {
        free_value(&found_var->val);
        found_var->val = v;
        return;
    }
    if (!frames) {
        // create global frame
        push_frame();
    }
    env_set_local_in_frame(frames, name, v);
}

int env_exists(const char *name) {
    return env_exists_in_frames(name, NULL, NULL);
}

Value env_get(const char *name) {
    Frame *f = frames;
    while (f) {
        Var *v = f->vars;
        while (v) {
            if (strcmp(v->name, name) == 0) return v->val;
            v = v->next;
        }
        f = f->next;
    }
    fprintf(stderr, "Runtime error: undefined variable '%s'\n", name);
    return make_nil();
}

/* AST nodes */
typedef enum {
    EXPR_LITERAL, EXPR_VAR, EXPR_BINARY, EXPR_UNARY, EXPR_GROUP, EXPR_CALL
} ExprType;

typedef struct Expr {
    ExprType type;
    Value literal; // for literal
    char *name; // for var (and call name)
    // binary
    TokenType op;
    struct Expr *left;
    struct Expr *right;
    // unary
    struct Expr *right_un;
    // call
    struct Expr **args;
    int arg_count;
} Expr;

typedef enum {
    STMT_PRINT, STMT_VARDECL, STMT_IF, STMT_BLOCK, STMT_LOOP, STMT_EXPR,
    STMT_RETURN, STMT_FUNCDECL
} StmtType;

struct Stmt {
    StmtType type;
    // print / expr / return
    Expr *expr;
    // vardecl
    char *varname;
    Expr *init;
    // if
    Expr *cond;
    Stmt *thenBranch;
    Stmt *elseBranch;
    // block
    Stmt **stmts;
    int stmt_count;
    // loop
    Expr *countExpr;
    // func decl
    char *fname;
    char **params;
    int param_count;
    Stmt *body; // the block
};

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
    memset(e,0,sizeof(Expr));
    e->type = EXPR_LITERAL;
    e->literal = v;
    e->left = e->right = NULL; e->name = NULL; e->right_un = NULL;
    e->args = NULL; e->arg_count = 0;
    return e;
}
Expr *expr_var(const char *name) {
    Expr *e = malloc(sizeof(Expr));
    memset(e,0,sizeof(Expr));
    e->type = EXPR_VAR;
    e->name = strdup(name);
    e->left = e->right = NULL; e->right_un = NULL;
    e->args = NULL; e->arg_count = 0;
    return e;
}
Expr *expr_unary(TokenType op, Expr *right) {
    Expr *e = malloc(sizeof(Expr));
    memset(e,0,sizeof(Expr));
    e->type = EXPR_UNARY;
    e->op = op;
    e->right_un = right;
    e->left = e->right = NULL; e->name = NULL;
    e->args = NULL; e->arg_count = 0;
    return e;
}
Expr *expr_binary(Expr *left, TokenType op, Expr *right) {
    Expr *e = malloc(sizeof(Expr));
    memset(e,0,sizeof(Expr));
    e->type = EXPR_BINARY;
    e->left = left; e->right = right; e->op = op; e->name = NULL;
    e->args = NULL; e->arg_count = 0;
    return e;
}
Expr *expr_group(Expr *inner) {
    Expr *e = malloc(sizeof(Expr));
    memset(e,0,sizeof(Expr));
    e->type = EXPR_GROUP;
    e->left = inner; e->right = NULL; e->name = NULL; e->right_un = NULL;
    e->args = NULL; e->arg_count = 0;
    return e;
}
Expr *expr_call(const char *name, Expr **args, int arg_count) {
    Expr *e = malloc(sizeof(Expr));
    memset(e,0,sizeof(Expr));
    e->type = EXPR_CALL;
    e->name = strdup(name);
    e->args = args;
    e->arg_count = arg_count;
    e->left = e->right = NULL; e->right_un = NULL;
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
    // function call or var
    if (check_tok(P, TOK_IDENT)) {
        Token *ident = cur_tok(P);
        // lookahead to see if it's a call
        if (peek_tok(P,1) && peek_tok(P,1)->type == TOK_LPAREN) {
            // parse call
            match_tok(P, TOK_IDENT); // consume ident
            match_tok(P, TOK_LPAREN);
            // parse args
            Expr **args = NULL;
            int cap = 0, cnt = 0;
            if (!check_tok(P, TOK_RPAREN)) {
                while (1) {
                    Expr *a = parse_expression(P);
                    if (cnt + 1 >= cap) {
                        cap = (cap == 0) ? 4 : cap * 2;
                        args = realloc(args, sizeof(Expr*) * cap);
                    }
                    args[cnt++] = a;
                    if (check_tok(P, TOK_COMMA)) { match_tok(P, TOK_COMMA); continue; }
                    break;
                }
            }
            match_tok(P, TOK_RPAREN);
            return expr_call(ident->lexeme, args, cnt);
        } else {
            // simple var
            match_tok(P, TOK_IDENT);
            return expr_var(ident->lexeme);
        }
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
    if (match_tok(P, TOK_KEY_HOHO)) {
        // function declaration: hoho name (params...) { ... }
        Token *ident = cur_tok(P);
        if (!match_tok(P, TOK_IDENT)) {
            fprintf(stderr, "Parse error: expected identifier after 'hoho'\n");
            return NULL;
        }
        char *fname = strdup(ident->lexeme);
        // params
        if (!match_tok(P, TOK_LPAREN)) {
            fprintf(stderr, "Parse error: expected '(' after function name\n");
            free(fname);
            return NULL;
        }
        char **params = NULL;
        int cap = 0, cnt = 0;
        if (!check_tok(P, TOK_RPAREN)) {
            while (1) {
                Token *p = cur_tok(P);
                if (!match_tok(P, TOK_IDENT)) {
                    fprintf(stderr, "Parse error: expected parameter name\n");
                    // free partial
                    for (int i = 0; i < cnt; ++i) free(params[i]);
                    free(params);
                    free(fname);
                    return NULL;
                }
                if (cnt + 1 >= cap) {
                    cap = (cap == 0) ? 4 : cap * 2;
                    params = realloc(params, sizeof(char*) * cap);
                }
                params[cnt++] = strdup(p->lexeme);
                if (check_tok(P, TOK_COMMA)) { match_tok(P, TOK_COMMA); continue; }
                break;
            }
        }
        match_tok(P, TOK_RPAREN);
        // body
        Stmt *body = parse_block(P);
        Stmt *s = make_stmt(STMT_FUNCDECL);
        s->fname = fname;
        s->params = params;
        s->param_count = cnt;
        s->body = body;
        return s;
    }
    if (match_tok(P, TOK_KEY_RETURN)) {
        Stmt *s = make_stmt(STMT_RETURN);
        s->expr = parse_expression(P);
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
    if (v.type == VAL_STRING) return v.string && strlen(v.string) > 0;
    if (v.type == VAL_FUNCTION) return 1;
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
    } else if (v.type == VAL_FUNCTION) {
        char tmp[128];
        snprintf(tmp, sizeof(tmp), "<function %s/%d>", v.fn && v.fn->name ? v.fn->name : "anon", v.fn ? v.fn->param_count : 0);
        return strdup(tmp);
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
            if (e->literal.type == VAL_STRING) {
                return make_string(e->literal.string ? e->literal.string : "");
            } else if (e->literal.type == VAL_FUNCTION) {
                // shouldn't normally happen: functions as literals not created this way
                return e->literal;
            } else {
                return e->literal;
            }
        }
        case EXPR_VAR: return env_get(e->name);
        case EXPR_GROUP: return eval_expr(e->left);
        case EXPR_UNARY: return eval_unary(e);
        case EXPR_BINARY: return eval_binary(e);
        case EXPR_CALL: {
            // look up function by name
            Value cal = env_get(e->name);
            if (cal.type != VAL_FUNCTION) {
                fprintf(stderr, "Runtime error: '%s' is not a function\n", e->name);
                // still evaluate args to avoid leaks
                for (int i = 0; i < e->arg_count; ++i) {
                    Value tmp = eval_expr(e->args[i]);
                    free_value(&tmp);
                }
                return make_nil();
            }
            Function *f = cal.fn;
            // evaluate args
            Value *argvals = malloc(sizeof(Value) * e->arg_count);
            for (int i = 0; i < e->arg_count; ++i) {
                argvals[i] = eval_expr(e->args[i]);
            }
            // call: push frame, bind params, execute body, pop frame
            push_frame();
            // bind parameters
            for (int i = 0; i < f->param_count; ++i) {
                Value v = make_nil();
                if (i < e->arg_count) v = argvals[i];
                else v = make_nil();
                env_set_local_in_frame(frames, f->params[i], v);
            }
            // execute body and capture return
            Value ret = make_nil();
            int returned = 0;
            // execute block: reuse exec_block but with return propagation
            // we'll implement a small local exec function to call
            // forward declaration: exec_stmt returns int if returned and fills out_return
            extern int exec_stmt_with_return(Stmt *s, Value *out_return);
            // execute body (which is a block statement node)
            if (f->body) {
                // we'll use exec_stmt_with_return on the block node itself
                returned = exec_stmt_with_return(f->body, &ret);
            }
            // pop frame
            pop_frame();
            // free temporaries (argument values that were not moved into env)
            // Note: when we bound params we moved arg Value into frame (no copy), so don't free those
            // But we allocated argvals array; free any leftover arg values that were not bound
            // Actually we moved them into env via env_set_local_in_frame which duplicates pointer; so we should not free them here.
            free(argvals);
            if (returned) return ret;
            return make_nil();
        }
    }
    return make_nil();
}

/* execute statements */
/* exec_stmt_with_return returns 1 if a return occurred and places the return value in out_return.
   Otherwise returns 0.
*/

int exec_stmt_with_return(Stmt *s, Value *out_return);

void exec_block_no_return(Stmt *block) {
    if (!block) return;
    for (int i = 0; i < block->stmt_count; ++i) {
        // use exec_stmt_with_return but ignore returns
        Value tmp;
        if (exec_stmt_with_return(block->stmts[i], &tmp)) {
            // if return happens in a non-function context, just ignore but free tmp
            free_value(&tmp);
        }
    }
}

void exec_block(Stmt *block) {
    exec_block_no_return(block);
}

int exec_stmt_with_return(Stmt *s, Value *out_return) {
    if (!s) return 0;
    if (s->type == STMT_PRINT) {
        Value v = eval_expr(s->expr);
        char *out = value_to_string(v);
        printf("%s\n", out);
        free(out);
        free_value(&v);
        return 0;
    }
    if (s->type == STMT_VARDECL) {
        Value v = eval_expr(s->init);
        env_set(s->varname, v);
        return 0;
    }
    if (s->type == STMT_IF) {
        Value cond = eval_expr(s->cond);
        if (is_truthy(cond)) {
            int r = exec_stmt_with_return(s->thenBranch, out_return);
            free_value(&cond);
            return r;
        } else {
            int r = exec_stmt_with_return(s->elseBranch, out_return);
            free_value(&cond);
            return r;
        }
    }
    if (s->type == STMT_BLOCK) {
        for (int i = 0; i < s->stmt_count; ++i) {
            if (exec_stmt_with_return(s->stmts[i], out_return)) return 1;
        }
        return 0;
    }
    if (s->type == STMT_LOOP) {
        Value cntv = eval_expr(s->countExpr);
        double n = (cntv.type == VAL_NUMBER) ? cntv.number : 0;
        int times = (int)floor(n);
        for (int i = 0; i < times; ++i) {
            if (exec_stmt_with_return(s->thenBranch, out_return)) { free_value(&cntv); return 1; }
        }
        free_value(&cntv);
        return 0;
    }
    if (s->type == STMT_EXPR) {
        Value v = eval_expr(s->expr);
        free_value(&v);
        return 0;
    }
    if (s->type == STMT_RETURN) {
        Value v = eval_expr(s->expr);
        *out_return = v; // move result to caller
        return 1;
    }
    if (s->type == STMT_FUNCDECL) {
        // create Function object
        Function *f = malloc(sizeof(Function));
        f->param_count = s->param_count;
        f->params = malloc(sizeof(char*) * f->param_count);
        for (int i = 0; i < f->param_count; ++i) {
            f->params[i] = strdup(s->params[i]);
        }
        f->body = s->body;
        f->name = strdup(s->fname ? s->fname : "anon");
        Value fv = make_function(f);
        env_set(s->fname, fv);
        return 0;
    }
    return 0;
}

/* -------------------------
   Utilities: free AST
   ------------------------- */

void free_expr(Expr *e) {
    if (!e) return;
    if (e->type == EXPR_LITERAL) {
        if (e->literal.type == VAL_STRING && e->literal.string) {
            free(e->literal.string);
            e->literal.string = NULL;
        }
    }
    if (e->name) { free(e->name); e->name = NULL; }
    if (e->left) free_expr(e->left);
    if (e->right) free_expr(e->right);
    if (e->right_un) free_expr(e->right_un);
    if (e->args) {
        for (int i = 0; i < e->arg_count; ++i) free_expr(e->args[i]);
        free(e->args);
    }
    free(e);
}

void free_stmt(Stmt *s) {
    if (!s) return;
    if (s->type == STMT_PRINT || s->type == STMT_EXPR || s->type == STMT_RETURN) {
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
    if (s->type == STMT_FUNCDECL) {
        if (s->fname) free(s->fname);
        if (s->params) {
            for (int i = 0; i < s->param_count; ++i) free(s->params[i]);
            free(s->params);
        }
        // body is freed below via free_stmt when nested in block walk
        // To avoid double-free: body is part of AST and will be freed by the s->body free_stmt call if it's present in stmts
        // Here we assume body is owned by this node and free it:
        free_stmt(s->body);
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
    // ensure there's a global frame
    if (!frames) push_frame();
    for (int i = 0; i < stmt_count; ++i) {
        Value tmp;
        exec_stmt_with_return(program[i], &tmp);
        // if top-level return happened, free returned value (top-level return ignored)
        free_value(&tmp);
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
    // cleanup frames if any
    while (frames) pop_frame();
    return 0;
}

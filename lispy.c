#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// The Lisp object type
enum
{
    // Regular objects
    INTEGER = 1,
    CELL,
    SYMBOL,
    PRIMITIVE,
    FUNCTION,
    KEYWORD,
    ENV,
};

// Subtypes for KEYWORD
enum
{
    NIL = 1,
    DOT,
    PARENTHESIS,
    TTRUE,
};

struct Object;

// Typedef for the primitive function.
typedef struct Object *Primitive(struct Object *env, struct Object *args);

// The object type
typedef struct Object
{
    int type;

    // Objectect values.
    union {
        // Int
        int value;
        // Cons cell type
        struct
        {
            struct Object *car;
            struct Object *cdr;
        };
        // Symbol
        char name[1];
        // Primitive
        Primitive *fn;
        // Subtype for special type
        int subtype;
        // Environment frame
        struct
        {
            struct Object *vars;
            struct Object *up;
        };
        // Forwarding pointer
        void *moved;
    };
} Object;

// Constants
static Object *Nil;
static Object *Dot;
static Object *Paren;
static Object *True;

// The list containing all symbols.
static Object *Symbols;

static void error(char *fmt, ...) __attribute((noreturn));

//======================================================================
// Constructors
//======================================================================

static Object *allocate(int type, size_t size)
{
    // Add the size of the type tag.
    size += offsetof(Object, value);

    // Allocate the object.
    Object *obj = malloc(size);
    obj->type = type;
    return obj;
}

static Object *make_int(int value)
{
    Object *r = allocate(INTEGER, sizeof(int));
    r->value = value;
    return r;
}

static Object *make_symbol(char *name)
{
    Object *sym = allocate(SYMBOL, strlen(name) + 1);
    strcpy(sym->name, name);
    return sym;
}

static Object *make_primitive(Primitive *fn)
{
    Object *r = allocate(PRIMITIVE, sizeof(Primitive *));
    r->fn = fn;
    return r;
}

static Object *make_function(int type, Object *params, Object *body, Object *env)
{
    assert(type == FUNCTION);
    Object *r = allocate(type, sizeof(Object *) * 3);
    r->params = params;
    r->body = body;
    r->env = env;
    return r;
}

static Object *make_special(int subtype)
{
    Object *r = malloc(sizeof(void *) * 2);
    r->type = KEYWORD;
    r->subtype = subtype;
    return r;
}

struct Object *make_env(Object *vars, Object *up)
{
    Object *r = allocate(ENV, sizeof(Object *) * 2);
    r->vars = vars;
    r->up = up;
    return r;
}

static Object *cons(Object *car, Object *cdr)
{
    Object *cell = allocate(CELL, sizeof(Object *) * 2);
    cell->car = car;
    cell->cdr = cdr;
    return cell;
}

// Returns ((x . y) . a)
static Object *acons(Object *x, Object *y, Object *a)
{
    return cons(cons(x, y), a);
}

//======================================================================
// Parser
//======================================================================

static Object *read(void);

static void error(char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

static int peek(void)
{
    int c = getchar();
    ungetc(c, stdin);
    return c;
}

// Skips the input until newline is found.
static void skip_line(void)
{
    for (;;)
    {
        int c = getchar();
        if (c == EOF || c == '\n')
            return;
        if (c == '\r')
        {
            if (peek() == '\n')
                getchar();
            return;
        }
    }
}

// Reads a list
static Object *read_list(void)
{
    Object *obj = read();
    if (!obj)
        error("Unclosed parenthesis");
    if (obj == Dot)
        error("Stray dot");
    if (obj == Paren)
        return Nil;
    Object *head, *tail;
    head = tail = cons(obj, Nil);

    for (;;)
    {
        Object *obj = read();
        if (!obj)
            error("Unclosed parenthesis");
        if (obj == Paren)
            return head;
        if (obj == Dot)
        {
            tail->cdr = read();
            if (read() != Paren)
                error("Closed parenthesis expected after dot");
            return head;
        }
        tail->cdr = cons(obj, Nil);
        tail = tail->cdr;
    }
}

// If there's a symbol with the same name, it will not create a new symbol but return the existing one. Otherwise create a new one.
static Object *intern(char *name)
{
    for (Object *p = Symbols; p != Nil; p = p->cdr)
        if (strcmp(name, p->car->name) == 0)
            return p->car;
    Object *sym = make_symbol(name);
    Symbols = cons(sym, Symbols);
    return sym;
}

// Reads an expression and returns (quote <expr>).
static Object *read_quote(void)
{
    Object *sym = intern("quote");
    return cons(sym, cons(read(), Nil));
}

static int read_number(int val)
{
    while (isdigit(peek()))
        val = val * 10 + (getchar() - '0');
    return val;
}

#define SYMBOL_MAX_LEN 200

static Object *read_symbol(char c)
{
    char buf[SYMBOL_MAX_LEN + 1];
    int len = 1;
    buf[0] = c;
    while (isalnum(peek()) || peek() == '-')
    {
        if (SYMBOL_MAX_LEN <= len)
            error("Symbol name too long");
        buf[len++] = getchar();
    }
    buf[len] = '\0';
    return intern(buf);
}

static Object *read(void)
{
    for (;;)
    {
        int c = getchar();
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
            continue;
        if (c == EOF)
            return NULL;
        if (c == ';')
        {
            skip_line();
            continue;
        }
        if (c == '(')
            return read_list();
        if (c == ')')
            return Paren;
        if (c == '.')
            return Dot;
        if (c == '\'')
            return read_quote();
        if (isdigit(c))
            return make_int(read_number(c - '0'));
        if (c == '-')
            return make_int(-read_number(0));
        if (isalpha(c) || strchr("+=!@#$%^&*", c))
            return read_symbol(c);
        error("Unknown character: %c", c);
    }
}

// Prints the given object.
static void print(Object *obj)
{
    switch (obj->type)
    {
    case INTEGER:
        printf("%d", obj->value);
        return;
    case CELL:
        printf("(");
        for (;;)
        {
            print(obj->car);
            if (obj->cdr == Nil)
                break;
            if (obj->cdr->type != CELL)
            {
                printf(" . ");
                print(obj->cdr);
                break;
            }
            printf(" ");
            obj = obj->cdr;
        }
        printf(")");
        return;
    case SYMBOL:
        printf("%s", obj->name);
        return;
    case PRIMITIVE:
        printf("<primitive>");
        return;
    case FUNCTION:
        printf("<function>");
        return;
    case KEYWORD:
        if (obj == Nil)
            printf("()");
        else if (obj == True)
            printf("t");
        else
            error("Unknown subtype: %d", obj->subtype);
        return;
    default:
        error("Unknown tag type: %d", obj->type);
    }
}

static int list_length(Object *list)
{
    int len = 0;
    for (;;)
    {
        if (list == Nil)
            return len;
        if (list->type != CELL)
            error("Cannot handle dotted list");
        list = list->cdr;
        len++;
    }
}

//======================================================================
// Evaluator
//======================================================================

static Object *eval(Object *env, Object *obj);

static void add_variable(Object *env, Object *sym, Object *val)
{
    env->vars = acons(sym, val, env->vars);
}

// Returns a newly created environment frame.
static Object *push_env(Object *env, Object *vars, Object *values)
{
    if (list_length(vars) != list_length(values))
        error("Number of argument does not match");
    Object *map = Nil;
    for (Object *p = vars, *q = values; p != Nil; p = p->cdr, q = q->cdr)
    {
        Object *sym = p->car;
        Object *val = q->car;
        map = acons(sym, val, map);
    }
    return make_env(map, env);
}

// Evaluates the list elements from head and returns the last return value.
static Object *progn(Object *env, Object *list)
{
    Object *r = NULL;
    for (Object *lp = list; lp != Nil; lp = lp->cdr)
        r = eval(env, lp->car);
    return r;
}

// Evaluates all the list elements and returns their return values as a new list.
static Object *eval_list(Object *env, Object *list)
{
    Object *head = NULL;
    Object *tail = NULL;
    for (Object *lp = list; lp != Nil; lp = lp->cdr)
    {
        Object *tmp = eval(env, lp->car);
        if (head == NULL)
        {
            head = tail = cons(tmp, Nil);
        }
        else
        {
            tail->cdr = cons(tmp, Nil);
            tail = tail->cdr;
        }
    }
    if (head == NULL)
        return Nil;
    return head;
}

static bool is_list(Object *obj)
{
    return obj == Nil || obj->type == CELL;
}

// Apply fn with args.
static Object *apply(Object *env, Object *fn, Object *args)
{
    if (!is_list(args))
        error("Argument must be a list");
    if (fn->type == PRIMITIVE)
        return fn->fn(env, args);
    if (fn->type == FUNCTION)
    {
        Object *body = fn->body;
        Object *params = fn->params;
        Object *eargs = eval_list(env, args);
        Object *newenv = push_env(fn->env, params, eargs);
        return progn(newenv, body);
    }
    error("Not supported");
}

// Searches for a variable by symbol. Returns null if not found.
static Object *find(Object *env, Object *sym)
{
    for (Object *p = env; p; p = p->up)
    {
        for (Object *cell = p->vars; cell != Nil; cell = cell->cdr)
        {
            Object *bind = cell->car;
            if (sym == bind->car)
                return bind;
        }
    }
    return NULL;
}

// Evaluates the S expression.
static Object *eval(Object *env, Object *obj)
{
    switch (obj->type)
    {
    case INTEGER:
    case PRIMITIVE:
    case FUNCTION:
    case KEYWORD:
        // Self-evaluating objects
        return obj;
    case SYMBOL:
    {
        // Variable
        Object *bind = find(env, obj);
        if (!bind)
            error("Undefined symbol: %s", obj->name);
        return bind->cdr;
    }
    case CELL:
    {
        // Function application form
        Object *expanded = macroexpand(env, obj);
        if (expanded != obj)
            return eval(env, expanded);
        Object *fn = eval(env, obj->car);
        Object *args = obj->cdr;
        if (fn->type != PRIMITIVE && fn->type != FUNCTION)
            error("The head of a list must be a function");
        return apply(env, fn, args);
    }
    default:
        error("Unknown tag type: %d", obj->type);
    }
}

//======================================================================
// Functions and special forms
//======================================================================

// 'expr
static Object *primitive_QUOTE(Object *env, Object *list)
{
    if (list_length(list) != 1)
        error("Malformed quote");
    return list->car;
}

// (list expr ...)
static Object *primitive_LIST(Object *env, Object *list)
{
    return eval_list(env, list);
}

// (setvalue <symbol> expr)
static Object *primitive_SETVALUE(Object *env, Object *list)
{
    if (list_length(list) != 2 || list->car->type != SYMBOL)
        error("Unable to set new value");
    Object *bind = find(env, list->car);
    if (!bind)
        error("Unbound variable %s", list->car->name);
    Object *value = eval(env, list->cdr->car);
    bind->cdr = value;
    return value;
}

// (+ <integer> ...)
static Object *primitive_PLUS(Object *env, Object *list)
{
    int sum = 0;
    for (Object *args = eval_list(env, list); args != Nil; args = args->cdr)
    {
        if (args->car->type != INTEGER)
            error("+ takes only numbers");
        sum += args->car->value;
    }
    return make_int(sum);
}

static Object *handle_function(Object *env, Object *list, int type)
{
    if (list->type != CELL || !is_list(list->car) || list->cdr->type != CELL)
        error("Unable to create new lambda");
    for (Object *p = list->car; p != Nil; p = p->cdr)
    {
        if (p->car->type != SYMBOL)
            error("Parameter must be a symbol");
        if (!is_list(p->cdr))
            error("Parameter list is not a flat list");
    }
    Object *car = list->car;
    Object *cdr = list->cdr;
    return make_function(type, car, cdr, env);
}

// (lambda (<symbol> ...) expr ...)
static Object *primitive_LAMBDA(Object *env, Object *list)
{
    return handle_function(env, list, FUNCTION);
}

// (define <symbol> expr)
static Object *primitive_DEFINE(Object *env, Object *list)
{
    if (list_length(list) != 2 || list->car->type != SYMBOL)
        error("Malformed setq");
    Object *sym = list->car;
    Object *value = eval(env, list->cdr->car);
    add_variable(env, sym, value);
    return value;
}

// (println expr)
static Object *primitive_PRINTLN(Object *env, Object *list)
{
    print(eval(env, list->car));
    printf("\n");
    return Nil;
}

// (if expr expr expr ...)
static Object *primitive_IF(Object *env, Object *list)
{
    if (list_length(list) < 2)
        error("Malformed if");
    Object *cond = eval(env, list->car);
    if (cond != Nil)
    {
        Object *then = list->cdr->car;
        return eval(env, then);
    }
    Object *els = list->cdr->cdr;
    return els == Nil ? Nil : progn(env, els);
}

// (= <integer> <integer>)
static Object *primitive_EQUAL(Object *env, Object *list)
{
    if (list_length(list) != 2)
        error("Malformed =");
    Object *values = eval_list(env, list);
    Object *x = values->car;
    Object *y = values->cdr->car;
    if (x->type != INTEGER || y->type != INTEGER)
        error("= only takes numbers");
    return x->value == y->value ? True : Nil;
}

// (exit)
static Object *primitive_EXIT(Object *env, Object *list)
{
    exit(0);
}

static void add_primitive(Object *env, char *name, Primitive *fn)
{
    Object *sym = intern(name);
    Object *prim = make_primitive(fn);
    add_variable(env, sym, prim);
}

static void define_constants(Object *env)
{
    Object *sym = intern("t");
    add_variable(env, sym, True);
}

static void define_primitives(Object *env)
{
    add_primitive(env, "quote", primitive_QUOTE);
    add_primitive(env, "list", primitive_LIST);
    add_primitive(env, "setvalue", primitive_SETVALUE);
    add_primitive(env, "+", primitive_PLUS);
    add_primitive(env, "define", primitive_DEFINE);
    add_primitive(env, "lambda", primitive_LAMBDA);
    add_primitive(env, "if", primitive_IF);
    add_primitive(env, "=", primitive_EQUAL);
    add_primitive(env, "println", primitive_PRINTLN);
    add_primitive(env, "exit", primitive_EXIT);
}

int main(int argc, char **argv)
{
    // Constants and primitives
    Nil = make_special(NIL);
    Dot = make_special(DOT);
    Paren = make_special(PARENTHESIS);
    True = make_special(TTRUE);
    Symbols = Nil;

    Object *env = make_env(Nil, NULL);

    define_constants(env);
    define_primitives(env);

    // The main loop
    for (;;)
    {
        Object *expr = read();
        if (!expr)
            return 0;
        if (expr == Paren)
            error("Stray parenthesis");
        if (expr == Dot)
            error("Stray dot");
        print(eval(env, expr));
        printf("\n");
    }
}

/*Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan

 SPDX-License-Identifier: LGPL-3.0-or-later */

#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <alloca.h>

#define RV_GP_ARGS 8
#define WORD_SZ 8

void print_int(int64_t n)
{
    printf("%" PRId64, n);
    fflush(stdout);
}

typedef struct Closure
{
    int64_t arity;
    int64_t received;
    void *code;
    void *args[];
} Closure;

static void __attribute__((noreturn)) panic(const char *msg)
{
    fputs(msg, stderr);
    fputc('\n', stderr);
    abort();
}

static inline size_t clos_size_bytes(int64_t arity)
{
    return sizeof(Closure) + (size_t)arity * sizeof(void *);
}

static inline void *rv_call(void *fn, void **argv, int64_t n)
{
    int64_t spill = (n > RV_GP_ARGS) ? (n - RV_GP_ARGS) : 0;
    size_t spill_bytes = (size_t)spill * WORD_SZ;
    void **spill_ptr = (spill > 0) ? argv + RV_GP_ARGS : NULL;

    void *ret;
    asm(
        "mv   t0, %[sz]\n"
        "sub  sp, sp, t0\n"

        "beqz %[cnt], 2f\n"
        "mv   t1, sp\n"
        "mv   t2, %[spill]\n"
        "mv   t3, %[cnt]\n"
        "li   t4, 0\n"
        "1:\n"
        "beq  t4, t3, 2f\n"
        "slli t5, t4, 3\n"
        "add  t6, t2, t5\n"
        "ld   t0, 0(t6)\n"
        "sd   t0, 0(t1)\n"
        "addi t1, t1, 8\n"
        "addi t4, t4, 1\n"
        "j    1b\n"
        "2:\n"

        "mv   a0, %[a0]\n"
        "mv   a1, %[a1]\n"
        "mv   a2, %[a2]\n"
        "mv   a3, %[a3]\n"
        "mv   a4, %[a4]\n"
        "mv   a5, %[a5]\n"
        "mv   a6, %[a6]\n"
        "mv   a7, %[a7]\n"

        "mv   t6, %[fn]\n"
        "jalr ra, t6, 0\n"

        "mv   t0, %[sz]\n"
        "add  sp, sp, t0\n"

        "mv   %[ret], a0\n"
        : [ret] "=r"(ret)
        : [fn] "r"(fn),
          [a0] "r"((n > 0) ? argv[0] : 0),
          [a1] "r"((n > 1) ? argv[1] : 0),
          [a2] "r"((n > 2) ? argv[2] : 0),
          [a3] "r"((n > 3) ? argv[3] : 0),
          [a4] "r"((n > 4) ? argv[4] : 0),
          [a5] "r"((n > 5) ? argv[5] : 0),
          [a6] "r"((n > 6) ? argv[6] : 0),
          [a7] "r"((n > 7) ? argv[7] : 0),
          [spill] "r"(spill_ptr),
          [cnt] "r"(spill),
          [sz] "r"(spill_bytes)
        : "t0", "t1", "t2", "t3", "t4", "t5", "t6",
          "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
          "memory");
    return ret;
}

Closure *alloc_closure(void *code, int64_t arity)
{
    if (arity < 0)
        panic("alloc_closure: negative arity");
    Closure *c = (Closure *)malloc(clos_size_bytes(arity));
    if (!c)
        panic("alloc_closure: OOM");
    c->arity = arity;
    c->received = 0;
    c->code = code;
    if (arity)
        memset(c->args, 0, (size_t)arity * sizeof(void *));
    return c;
}

Closure *copy_closure(const Closure *src)
{
    Closure *dst = (Closure *)malloc(clos_size_bytes(src->arity));
    if (!dst)
        panic("copy_closure: OOM");
    memcpy(dst, src, clos_size_bytes(src->arity));
    return dst;
}

void *apply1(Closure *f, void *arg)
{
    assert(f != NULL);

    const int64_t r = f->received;
    const int64_t n = f->arity;

    // still collecting
    if (r + 1 < n)
    {
        Closure *g = copy_closure(f);
        g->args[g->received++] = arg;
        return g;
    }

    if (r + 1 == n)
    {
        void **argv = (void **)alloca((size_t)(n ? n : 1) * sizeof(void *));
        for (int64_t i = 0; i < r; ++i)
            argv[i] = f->args[i];
        argv[r] = arg;
        return rv_call(f->code, argv, n);
    }

    panic("apply1: too many arguments");
}

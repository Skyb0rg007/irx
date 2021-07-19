#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

typedef struct Record Record;
struct Record {
    uint64_t *base;
    Record *next;
    size_t size;
    const uint64_t *ret_ptr;
};

typedef enum {
    CALL,
    RET,
    ADD,
    PRINT,
    HALT,
    INT,
    GETLOCAL,
    SETLOCAL,
    COPY,
    TAILCALL
} instruction;

#define define_instr(name, args, r0, r1, r2) \
    static uint64_t i##name args { return (name << 24) | (r0 << 16) | (r1 << 8) | (r2); }

define_instr(ADD, (uint16_t dst, uint16_t a, uint16_t b), dst, a, b)
define_instr(PRINT, (uint16_t src), src, 0, 0)
define_instr(HALT, (), 0, 0, 0)
define_instr(RET, (), 0, 0, 0)
define_instr(INT, (uint16_t dst, uint16_t val), dst, val, 0)
define_instr(GETLOCAL, (uint16_t dst, uint16_t loc), dst, loc, 0)
define_instr(SETLOCAL, (uint16_t loc, uint16_t src), loc, src, 0)
define_instr(COPY, (uint16_t dst, uint16_t src), dst, src, 0)
static uint64_t iCALL(const uint64_t *addr) { return (CALL << 24) | (uint64_t)addr; }
static uint64_t iTAILCALL(const uint64_t *addr) { return (TAILCALL << 24) | (uint64_t)addr; }

static instruction instr_code(uint64_t i) { return i >> 24; }
static uint64_t *instr_addr(uint64_t i) { return (uint64_t *)(i & 0xffffff); }
static uint16_t instr_r0(uint64_t i) { return (i >> 16) & 0xff; }
static uint16_t instr_r1(uint64_t i) { return (i >> 8) & 0xff; }
static uint16_t instr_r2(uint64_t i) { return i & 0xff; }

static uint64_t *fp;
static Record *cur;
static uint64_t regs[0xff];

void run(const uint64_t *instrs)
{
    while (1) {
        uint64_t instr = *instrs++;
        switch (instr_code(instr))
        {
            case CALL: {
                const uint64_t *addr = instr_addr(instr);
                uint64_t frame_size = instrs[0];
                const uint64_t *ret_ptr = &instrs[1];
                fp += frame_size;
                fp[0] = (uint64_t)ret_ptr;
                instrs = addr;
                break;
            }
            case GETLOCAL: {
                regs[instr_r0(instr)] = fp[instr_r1(instr) + 1];
                break;
            }
            case SETLOCAL: {
                fp[instr_r0(instr) + 1] = regs[instr_r1(instr)];
                break;
            }
            case RET: {
                const uint64_t *addr = (const uint64_t *)fp[0];
                uint64_t frame_size = addr[-1];
                fp = fp - frame_size;
                instrs = addr;
                break;
            }
            case ADD: {
                regs[instr_r0(instr)] = regs[instr_r1(instr)] + regs[instr_r2(instr)];
                break;
            }
            case INT: {
                regs[instr_r0(instr)] = instr_r1(instr);
                break;
            }
            case PRINT: {
                printf("regs[%d] = %lu\n", instr_r0(instr), regs[instr_r0(instr)]);
                break;
            }
            case HALT: {
                return;
            }
        }
    }
}

#define STACK_SIZE 0xff

int main(void)
{
    static uint64_t stack[STACK_SIZE / 8];
    fp = stack;
    cur = malloc(sizeof *cur);
    cur->size = STACK_SIZE;
    cur->base = stack;
    cur->next = NULL;
    cur->ret_ptr = NULL;

    uint64_t instrs[] = {
        /* def print_int(n): print(n) */
        [0] = iPRINT(0),
        [1] = iRET(),
        /* def foreach(n):  */
        iINT(1, 12),
        iINT(2, 4),
        iADD(0, 1, 2),
        iSETLOCAL(0, 0),
        iGETLOCAL(1, 0),
        iPRINT(1),
        iHALT(),

    };
    run(instrs);

    for (Record *r = cur; r != NULL; r = cur) {
        cur = cur->next;
        free(r);
    }
    return 0;
}

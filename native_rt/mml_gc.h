/* mml_gc.h: MPS-backed moving/generational collector for the native backend.
 *
 * This is the alternative to the Boehm runtime (gc/gc.h). It is currently UNWIRED
 * — runtime.c still allocates via Boehm. A later increment routes allocation here
 * behind a flag. The collector is conservative-on-the-stack and precise-on-the-heap
 * (MPS AMC pool): it pins anything an ambiguous stack/root word points at and moves
 * the rest, reusing the MiniML object headers as the precise object format.
 *
 * Object model: an MPS object is the full [8-byte header | data] block; its base is
 * the header address. A mml_value is the CLIENT pointer = base + 8 (past the
 * header), exactly as mml_alloc returns today. The format converts client<->base at
 * the few fix sites; ambiguous interior (client) pointers pin correctly (validated).
 */
#ifndef MML_GC_H
#define MML_GC_H

#include <stdint.h>
#include <stddef.h>

/* Initialise the collector: create the arena, object format, AMC pool, allocation
   point, register this thread, and scan its C stack from `stack_cold` (a marker
   taken at the top of the entry frame) as ambiguous roots. Call once at startup. */
void mml_gc_init(void *stack_cold);

/* Allocate an object: `nbytes` data bytes (excluding the 8-byte header) and the
   prebuilt MiniML header word. Returns the CLIENT pointer (base + 8), matching
   mml_alloc. Data words are zeroed (so a collection during multi-step
   initialisation sees 0 = skip in not-yet-filled reference slots). */
void *mml_gc_alloc(int64_t nbytes, int64_t header);

/* Register / unregister an ambiguous root area [base, limit) (for fiber stacks and
   any C globals holding mml_values). */
void *mml_gc_add_area_root(void *base, void *limit);   /* returns an opaque handle */
void  mml_gc_remove_area_root(void *handle);

/* Force a full collection (for testing / deterministic stress). */
void mml_gc_collect(void);

#endif /* MML_GC_H */

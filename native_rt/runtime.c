#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>
#include <inttypes.h>
#include <unistd.h>
#include <sys/mman.h>
#include <gc/gc.h>
#include "runtime.h"

/* Forward declaration of the generated entry point */
extern mml_value mml_main(void);

/* Forward declarations */
mml_value mml_structural_eq(mml_value a, mml_value b);

/* Result type tag — set by codegen */
extern int32_t mml_result_type;

/* Track whether any print function was called */
static int mml_has_output = 0;

/* ---- UTF-8 helpers ---- */

/* Encode a Unicode codepoint as UTF-8 into buf (must be >= 4 bytes).
   Returns number of bytes written (1-4). Clamps invalid codepoints to U+FFFD. */
static int utf8_encode(int64_t cp, char *buf) {
    if (cp < 0 || cp > 0x10FFFF) cp = 0xFFFD; /* replacement character */
    if (cp < 0x80) {
        buf[0] = (char)cp;
        return 1;
    } else if (cp < 0x800) {
        buf[0] = (char)(0xC0 | (cp >> 6));
        buf[1] = (char)(0x80 | (cp & 0x3F));
        return 2;
    } else if (cp < 0x10000) {
        buf[0] = (char)(0xE0 | (cp >> 12));
        buf[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
        buf[2] = (char)(0x80 | (cp & 0x3F));
        return 3;
    } else {
        buf[0] = (char)(0xF0 | (cp >> 18));
        buf[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
        buf[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
        buf[3] = (char)(0x80 | (cp & 0x3F));
        return 4;
    }
}

/* Decode one UTF-8 codepoint starting at data[*i], advance *i past it.
   Validates continuation bytes; returns U+FFFD for malformed sequences. */
static int64_t utf8_decode(const char *data, int64_t len, int64_t *i) {
    unsigned char b = data[*i];
    int64_t cp;
    if (b < 0x80) {
        cp = b;
        (*i) += 1;
    } else if ((b & 0xE0) == 0xC0 && *i + 1 < len &&
               (data[*i+1] & 0xC0) == 0x80) {
        cp = ((b & 0x1F) << 6) | (data[*i+1] & 0x3F);
        (*i) += 2;
    } else if ((b & 0xF0) == 0xE0 && *i + 2 < len &&
               (data[*i+1] & 0xC0) == 0x80 && (data[*i+2] & 0xC0) == 0x80) {
        cp = ((b & 0x0F) << 12) | ((data[*i+1] & 0x3F) << 6) | (data[*i+2] & 0x3F);
        (*i) += 3;
    } else if ((b & 0xF8) == 0xF0 && *i + 3 < len &&
               (data[*i+1] & 0xC0) == 0x80 && (data[*i+2] & 0xC0) == 0x80 &&
               (data[*i+3] & 0xC0) == 0x80) {
        cp = ((b & 0x07) << 18) | ((data[*i+1] & 0x3F) << 12)
           | ((data[*i+2] & 0x3F) << 6) | (data[*i+3] & 0x3F);
        (*i) += 4;
    } else {
        cp = 0xFFFD; /* replacement character for malformed byte */
        (*i) += 1;
    }
    return cp;
}

/* Advance past one UTF-8 codepoint at data[*i], with bounds checking.
   Returns bytes advanced (1-4). Does not decode the codepoint. */
static int utf8_advance(const char *data, int64_t len, int64_t *i) {
    unsigned char b = data[*i];
    int n;
    if (b < 0x80) n = 1;
    else if ((b & 0xE0) == 0xC0) n = 2;
    else if ((b & 0xF0) == 0xE0) n = 3;
    else n = 4;
    /* Clamp to not overshoot the string end */
    if (*i + n > len) n = (int)(len - *i);
    (*i) += n;
    return n;
}

/* ---- Runtime functions ---- */

mml_value mml_print_int(mml_value v) {
    mml_has_output = 1;
    printf("%" PRId64 "\n", MML_INT_VAL(v));
    return MML_UNIT;
}

mml_value mml_print_bool(mml_value v) {
    mml_has_output = 1;
    printf("%s\n", v == MML_TRUE ? "true" : "false");
    return MML_UNIT;
}

mml_value mml_print_string(mml_value v) {
    mml_has_output = 1;
    printf("%s\n", MML_STR_DATA(v));
    return MML_UNIT;
}

mml_value mml_print_float(mml_value v) {
    mml_has_output = 1;
    double d = mml_unbox_float(v);
    /* Match OCaml/bytecode pp_value: use %g, append '.' if no dot/exponent */
    char buf[64];
    snprintf(buf, sizeof(buf), "%g", d);
    int has_dot = 0;
    for (int i = 0; buf[i]; i++) {
        if (buf[i] == '.' || buf[i] == 'e' || buf[i] == 'E' ||
            buf[i] == 'n' || buf[i] == 'i') {  /* nan, inf */
            has_dot = 1;
            break;
        }
    }
    if (!has_dot) {
        printf("%s.\n", buf);
    } else {
        printf("%s\n", buf);
    }
    return MML_UNIT;
}

mml_value mml_print_unit(mml_value v) {
    mml_has_output = 1;
    (void)v;
    printf("()\n");
    return MML_UNIT;
}

mml_value mml_print_value(mml_value v) {
    mml_has_output = 1;
    if (MML_IS_INT(v)) {
        if (v == MML_TRUE) {
            printf("true\n");
        } else {
            printf("%" PRId64 "\n", MML_INT_VAL(v));
        }
    } else if (MML_HDR_TAG(v) == MML_HDR_STRING) {
        printf("%s\n", MML_STR_DATA(v));
    } else {
        printf("<value>\n");
    }
    return MML_UNIT;
}

/* ---- String operations ---- */

mml_value mml_string_concat(mml_value a, mml_value b) {
    int64_t la = MML_STR_LEN(a);
    int64_t lb = MML_STR_LEN(b);
    mml_value result = mml_string_alloc(la + lb);
    char *dst = (char *)MML_STR_DATA(result);
    memcpy(dst, MML_STR_DATA(a), la);
    memcpy(dst + la, MML_STR_DATA(b), lb);
    return result;
}

mml_value mml_string_eq(mml_value a, mml_value b) {
    int64_t la = MML_STR_LEN(a);
    int64_t lb = MML_STR_LEN(b);
    if (la != lb) return MML_FALSE;
    return memcmp(MML_STR_DATA(a), MML_STR_DATA(b), la) == 0 ? MML_TRUE : MML_FALSE;
}

mml_value mml_string_compare(mml_value a, mml_value b) {
    int64_t la = MML_STR_LEN(a);
    int64_t lb = MML_STR_LEN(b);
    int64_t min_len = la < lb ? la : lb;
    int cmp = memcmp(MML_STR_DATA(a), MML_STR_DATA(b), min_len);
    if (cmp == 0) cmp = (la > lb) - (la < lb);
    return MML_TAG_INT(cmp < 0 ? -1 : (cmp > 0 ? 1 : 0));
}

mml_value mml_string_length(mml_value s) {
    return MML_TAG_INT(MML_STR_LEN(s));
}

mml_value mml_string_of_int(mml_value v) {
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%" PRId64, MML_INT_VAL(v));
    return mml_string_from_buf(buf, len);
}

/* ---- Float operations ---- */

mml_value mml_box_float(double d) {
    int64_t *p = (int64_t *)GC_malloc(16);  /* 8 header + 8 double */
    if (!p) mml_panic("out of memory in box_float");
    p[0] = MML_MAKE_HDR(MML_HDR_FLOAT, 0);
    *(double *)(p + 1) = d;
    return (mml_value)(intptr_t)(p + 1);
}

double mml_unbox_float(mml_value v) {
    double *p = (double *)(intptr_t)v;
    return *p;
}

mml_value mml_float_of_int(mml_value v) {
    double d = (double)MML_INT_VAL(v);
    return mml_box_float(d);
}

mml_value mml_int_of_float(mml_value v) {
    double d = mml_unbox_float(v);
    return MML_TAG_INT((int64_t)d);
}

/* ---- Math builtins ---- */

mml_value mml_math_pow(mml_value a, mml_value b) {
    double da = mml_unbox_float(a);
    double db = mml_unbox_float(b);
    return mml_box_float(pow(da, db));
}

mml_value mml_math_sqrt(mml_value a) {
    double da = mml_unbox_float(a);
    return mml_box_float(sqrt(da));
}

mml_value mml_math_floor(mml_value a) {
    double da = mml_unbox_float(a);
    return MML_TAG_INT((int64_t)floor(da));
}

mml_value mml_math_ceil(mml_value a) {
    double da = mml_unbox_float(a);
    return MML_TAG_INT((int64_t)ceil(da));
}

mml_value mml_math_round(mml_value a) {
    double da = mml_unbox_float(a);
    return MML_TAG_INT((int64_t)round(da));
}

/* ---- Byte builtins ---- */

mml_value mml_byte_of_int(mml_value v) {
    return MML_TAG_INT(MML_INT_VAL(v) & 0xFF);
}

mml_value mml_byte_to_int(mml_value v) {
    return v; /* bytes are already tagged ints */
}

mml_value mml_byte_to_string(mml_value v) {
    char buf[4];
    int n = snprintf(buf, sizeof(buf), "%lld", (long long)MML_INT_VAL(v));
    return mml_string_from_buf(buf, n);
}

/* ---- Format builtins ---- */

mml_value mml_fmt_float_str(mml_value prec, mml_value val) {
    int p = (int)MML_INT_VAL(prec);
    double d = mml_unbox_float(val);
    char buf[64];
    int n = snprintf(buf, sizeof(buf), "%.*f", p, d);
    return mml_string_from_buf(buf, n);
}

mml_value mml_fmt_hex(mml_value val) {
    char buf[32];
    int n = snprintf(buf, sizeof(buf), "%llx", (unsigned long long)MML_INT_VAL(val));
    return mml_string_from_buf(buf, n);
}

mml_value mml_fmt_hex_upper(mml_value val) {
    char buf[32];
    int n = snprintf(buf, sizeof(buf), "%llX", (unsigned long long)MML_INT_VAL(val));
    return mml_string_from_buf(buf, n);
}

mml_value mml_fmt_oct(mml_value val) {
    char buf[32];
    int n = snprintf(buf, sizeof(buf), "%llo", (unsigned long long)MML_INT_VAL(val));
    return mml_string_from_buf(buf, n);
}

mml_value mml_fmt_bin(mml_value val) {
    int64_t v = MML_INT_VAL(val);
    if (v == 0) return mml_string_from_cstr("0");
    char buf[65];
    int pos = 64;
    buf[pos] = '\0';
    uint64_t uv = (uint64_t)v;
    while (uv > 0 && pos > 0) {
        buf[--pos] = (uv & 1) ? '1' : '0';
        uv >>= 1;
    }
    return mml_string_from_cstr(buf + pos);
}

mml_value mml_fmt_zero_pad(mml_value width, mml_value str) {
    int64_t w = MML_INT_VAL(width);
    int64_t len = MML_STR_LEN(str);
    if (len >= w) return str;
    int64_t pad = w - len;
    int64_t new_len = w;
    mml_value result = mml_string_alloc(new_len);
    char *dst = (char *)((int64_t *)(intptr_t)result + 1);
    for (int64_t i = 0; i < pad; i++) dst[i] = '0';
    memcpy(dst + pad, MML_STR_DATA(str), len);
    dst[new_len] = '\0';
    return result;
}

mml_value mml_fmt_pad_left(mml_value width, mml_value str) {
    int64_t w = MML_INT_VAL(width);
    int64_t len = MML_STR_LEN(str);
    if (len >= w) return str;
    int64_t pad = w - len;
    int64_t new_len = w;
    mml_value result = mml_string_alloc(new_len);
    char *dst = (char *)((int64_t *)(intptr_t)result + 1);
    for (int64_t i = 0; i < pad; i++) dst[i] = ' ';
    memcpy(dst + pad, MML_STR_DATA(str), len);
    dst[new_len] = '\0';
    return result;
}

mml_value mml_fmt_pad_right(mml_value width, mml_value str) {
    int64_t w = MML_INT_VAL(width);
    int64_t len = MML_STR_LEN(str);
    if (len >= w) return str;
    int64_t pad = w - len;
    int64_t new_len = w;
    mml_value result = mml_string_alloc(new_len);
    char *dst = (char *)((int64_t *)(intptr_t)result + 1);
    memcpy(dst, MML_STR_DATA(str), len);
    for (int64_t i = 0; i < pad; i++) dst[len + i] = ' ';
    dst[new_len] = '\0';
    return result;
}

/* ---- String extra builtins ---- */

mml_value mml_string_split(mml_value str, mml_value delim) {
    const char *s = MML_STR_DATA(str);
    int64_t slen = MML_STR_LEN(str);
    const char *d = MML_STR_DATA(delim);
    int64_t dlen = MML_STR_LEN(delim);

    /* Build result list in reverse, then reverse */
    mml_value result = MML_UNIT; /* nil */
    if (dlen == 0) {
        /* Empty delimiter: return each character */
        for (int64_t i = slen - 1; i >= 0; i--) {
            mml_value ch = mml_string_from_buf(s + i, 1);
            mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
            cell[0] = ch;
            cell[1] = result;
            result = (mml_value)(intptr_t)cell;
        }
        return result;
    }

    int64_t start = 0;
    mml_value parts = MML_UNIT;
    for (int64_t i = 0; i <= slen - dlen; i++) {
        if (memcmp(s + i, d, dlen) == 0) {
            mml_value part = mml_string_from_buf(s + start, i - start);
            mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
            cell[0] = part;
            cell[1] = parts;
            parts = (mml_value)(intptr_t)cell;
            i += dlen - 1;
            start = i + 1;
        }
    }
    /* Last segment */
    mml_value last = mml_string_from_buf(s + start, slen - start);
    mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
    cell[0] = last;
    cell[1] = parts;
    parts = (mml_value)(intptr_t)cell;

    return mml_list_rev(parts);
}

mml_value mml_string_trim(mml_value str) {
    const char *s = MML_STR_DATA(str);
    int64_t len = MML_STR_LEN(str);
    int64_t start = 0, end = len;
    while (start < end && (s[start] == ' ' || s[start] == '\t' || s[start] == '\n' || s[start] == '\r'))
        start++;
    while (end > start && (s[end-1] == ' ' || s[end-1] == '\t' || s[end-1] == '\n' || s[end-1] == '\r'))
        end--;
    return mml_string_from_buf(s + start, end - start);
}

mml_value mml_string_starts_with(mml_value str, mml_value prefix) {
    int64_t slen = MML_STR_LEN(str);
    int64_t plen = MML_STR_LEN(prefix);
    if (plen > slen) return MML_FALSE;
    return memcmp(MML_STR_DATA(str), MML_STR_DATA(prefix), plen) == 0 ? MML_TRUE : MML_FALSE;
}

mml_value mml_string_replace(mml_value str, mml_value old_s, mml_value new_s) {
    const char *s = MML_STR_DATA(str);
    int64_t slen = MML_STR_LEN(str);
    const char *o = MML_STR_DATA(old_s);
    int64_t olen = MML_STR_LEN(old_s);
    const char *n = MML_STR_DATA(new_s);
    int64_t nlen = MML_STR_LEN(new_s);

    if (olen == 0) return str;

    /* Count occurrences */
    int64_t count = 0;
    for (int64_t i = 0; i <= slen - olen; i++) {
        if (memcmp(s + i, o, olen) == 0) { count++; i += olen - 1; }
    }
    if (count == 0) return str;

    int64_t new_len = slen + count * (nlen - olen);
    mml_value result = mml_string_alloc(new_len);
    char *dst = (char *)((int64_t *)(intptr_t)result + 1);
    int64_t di = 0;
    for (int64_t i = 0; i < slen; ) {
        if (i <= slen - olen && memcmp(s + i, o, olen) == 0) {
            memcpy(dst + di, n, nlen);
            di += nlen;
            i += olen;
        } else {
            dst[di++] = s[i++];
        }
    }
    dst[new_len] = '\0';
    return result;
}

mml_value mml_string_to_int(mml_value str) {
    const char *s = MML_STR_DATA(str);
    char *end;
    int64_t val = strtoll(s, &end, 10);
    if (end == s || *end != '\0') {
        return MML_TAG_INT(0); /* None */
    }
    mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_VARIANT, 0));
    cell[0] = MML_TAG_INT(1); /* Some tag */
    cell[1] = MML_TAG_INT(val);
    return (mml_value)(intptr_t)cell;
}

mml_value mml_string_to_float(mml_value str) {
    const char *s = MML_STR_DATA(str);
    char *end;
    double val = strtod(s, &end);
    if (end == s || *end != '\0') {
        return MML_TAG_INT(0); /* None */
    }
    mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_VARIANT, 0));
    cell[0] = MML_TAG_INT(1); /* Some tag */
    cell[1] = mml_box_float(val);
    return (mml_value)(intptr_t)cell;
}

mml_value mml_string_uppercase(mml_value str) {
    int64_t len = MML_STR_LEN(str);
    mml_value result = mml_string_alloc(len);
    const char *src = MML_STR_DATA(str);
    char *dst = (char *)((int64_t *)(intptr_t)result + 1);
    for (int64_t i = 0; i < len; i++) {
        dst[i] = (src[i] >= 'a' && src[i] <= 'z') ? src[i] - 32 : src[i];
    }
    dst[len] = '\0';
    return result;
}

mml_value mml_string_lowercase(mml_value str) {
    int64_t len = MML_STR_LEN(str);
    mml_value result = mml_string_alloc(len);
    const char *src = MML_STR_DATA(str);
    char *dst = (char *)((int64_t *)(intptr_t)result + 1);
    for (int64_t i = 0; i < len; i++) {
        dst[i] = (src[i] >= 'A' && src[i] <= 'Z') ? src[i] + 32 : src[i];
    }
    dst[len] = '\0';
    return result;
}

mml_value mml_string_to_byte_array(mml_value str) {
    int64_t len = MML_STR_LEN(str);
    if (len == 0) return MML_UNIT; /* empty array */
    const char *s = MML_STR_DATA(str);
    mml_value *arr = (mml_value *)mml_alloc((1 + len) * sizeof(mml_value), MML_MAKE_HDR(MML_HDR_ARRAY, 0));
    arr[0] = len; /* raw length */
    for (int64_t i = 0; i < len; i++) {
        arr[1 + i] = MML_TAG_INT((unsigned char)s[i]);
    }
    return (mml_value)(intptr_t)arr;
}

mml_value mml_string_of_byte_array(mml_value arr) {
    if (arr == MML_UNIT) return mml_string_from_cstr("");
    int64_t len = MML_ARR_LEN(arr);
    mml_value result = mml_string_alloc(len);
    char *dst = (char *)((int64_t *)(intptr_t)result + 1);
    mml_value *data = MML_ARR_DATA(arr);
    for (int64_t i = 0; i < len; i++) {
        dst[i] = (char)MML_INT_VAL(data[i]);
    }
    dst[len] = '\0';
    return result;
}

mml_value mml_string_make(mml_value n, mml_value ch) {
    int64_t count = MML_INT_VAL(n);
    int64_t c = MML_INT_VAL(ch);
    if (count <= 0) return mml_string_from_cstr("");
    mml_value result = mml_string_alloc(count);
    char *dst = (char *)((int64_t *)(intptr_t)result + 1);
    memset(dst, (char)c, count);
    dst[count] = '\0';
    return result;
}

mml_value mml_string_index_opt(mml_value str, mml_value byte_val) {
    const char *s = MML_STR_DATA(str);
    int64_t slen = MML_STR_LEN(str);
    char c = (char)MML_INT_VAL(byte_val);
    for (int64_t i = 0; i < slen; i++) {
        if (s[i] == c) {
            mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_VARIANT, 0));
            cell[0] = MML_TAG_INT(1); /* Some tag */
            cell[1] = MML_TAG_INT(i);
            return (mml_value)(intptr_t)cell;
        }
    }
    return MML_TAG_INT(0); /* None */
}

mml_value mml_string_rindex_opt(mml_value str, mml_value byte_val) {
    const char *s = MML_STR_DATA(str);
    int64_t slen = MML_STR_LEN(str);
    char c = (char)MML_INT_VAL(byte_val);
    for (int64_t i = slen - 1; i >= 0; i--) {
        if (s[i] == c) {
            mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_VARIANT, 0));
            cell[0] = MML_TAG_INT(1); /* Some tag */
            cell[1] = MML_TAG_INT(i);
            return (mml_value)(intptr_t)cell;
        }
    }
    return MML_TAG_INT(0); /* None */
}

mml_value mml_string_concat_list(mml_value sep, mml_value lst) {
    /* First pass: compute total length */
    const char *sep_data = MML_STR_DATA(sep);
    int64_t sep_len = MML_STR_LEN(sep);
    int64_t total = 0;
    int64_t count = 0;
    mml_value cur = lst;
    while (cur != MML_UNIT) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        total += MML_STR_LEN(cell[0]);
        count++;
        cur = cell[1];
    }
    if (count == 0) return mml_string_from_cstr("");
    total += sep_len * (count - 1);

    mml_value result = mml_string_alloc(total);
    char *dst = (char *)((int64_t *)(intptr_t)result + 1);
    int64_t pos = 0;
    int64_t idx = 0;
    cur = lst;
    while (cur != MML_UNIT) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        if (idx > 0) {
            memcpy(dst + pos, sep_data, sep_len);
            pos += sep_len;
        }
        int64_t slen = MML_STR_LEN(cell[0]);
        memcpy(dst + pos, MML_STR_DATA(cell[0]), slen);
        pos += slen;
        idx++;
        cur = cell[1];
    }
    dst[total] = '\0';
    return result;
}

mml_value mml_string_compare_str(mml_value a, mml_value b) {
    int64_t alen = MML_STR_LEN(a);
    int64_t blen = MML_STR_LEN(b);
    int64_t minlen = alen < blen ? alen : blen;
    int cmp = memcmp(MML_STR_DATA(a), MML_STR_DATA(b), minlen);
    if (cmp != 0) return MML_TAG_INT(cmp < 0 ? -1 : 1);
    if (alen < blen) return MML_TAG_INT(-1);
    if (alen > blen) return MML_TAG_INT(1);
    return MML_TAG_INT(0);
}

/* Array.sub */
mml_value mml_array_sub(mml_value arr, mml_value start, mml_value len) {
    int64_t s = MML_INT_VAL(start);
    int64_t l = MML_INT_VAL(len);
    if (l <= 0) return MML_UNIT;
    mml_value *src = MML_ARR_DATA(arr);
    mml_value *dst = (mml_value *)mml_alloc((1 + l) * sizeof(mml_value), MML_MAKE_HDR(MML_HDR_ARRAY, 0));
    dst[0] = l;
    memcpy(dst + 1, src + s, l * sizeof(mml_value));
    return (mml_value)(intptr_t)dst;
}

/* ---- Error handling ---- */

void __attribute__((noreturn)) mml_panic(const char *msg) {
    fprintf(stderr, "MiniML panic: %s\n", msg);
    exit(1);
}

void mml_check_div_zero(int64_t divisor) {
    if (divisor == 0) {
        mml_panic("division by zero");
    }
}

void mml_panic_mml(mml_value s) {
    fprintf(stderr, "MiniML panic: %s\n", MML_STR_DATA(s));
    exit(1);
}

/* ---- String allocation helpers ---- */

mml_value mml_string_alloc(int64_t len) {
    int64_t *p = (int64_t *)GC_malloc_atomic(8 + 8 + len + 1);  /* +8 for header */
    if (!p) mml_panic("out of memory in string_alloc");
    p[0] = MML_MAKE_HDR(MML_HDR_STRING, 0);
    p[1] = len << 1;  /* stored shifted so bit 0 is always clear (distinguishes from tagged ints) */
    ((char *)(p + 2))[len] = '\0';
    return (mml_value)(intptr_t)(p + 1);  /* return past header */
}

mml_value mml_string_from_cstr(const char *s) {
    int64_t len = (int64_t)strlen(s);
    mml_value v = mml_string_alloc(len);
    memcpy((char *)MML_STR_DATA(v), s, len);
    return v;
}

mml_value mml_string_from_buf(const char *buf, int64_t len) {
    mml_value v = mml_string_alloc(len);
    memcpy((char *)MML_STR_DATA(v), buf, len);
    return v;
}

/* ---- Heap allocation ---- */

void *mml_alloc(int64_t nbytes, int64_t header) {
    int64_t *p = (int64_t *)GC_malloc((size_t)(nbytes + 8));
    if (!p) mml_panic("out of memory");
    p[0] = header;
    return (void *)(p + 1);
}

/* ---- Format functions (no newline) ---- */

void mml_fmt_int(mml_value v) {
    printf("%" PRId64, MML_INT_VAL(v));
}

void mml_fmt_bool(mml_value v) {
    printf("%s", v == MML_TRUE ? "true" : "false");
}

void mml_fmt_string(mml_value v) {
    printf("%s", MML_STR_DATA(v));
}

void mml_fmt_float(mml_value v) {
    double d = mml_unbox_float(v);
    char buf[64];
    snprintf(buf, sizeof(buf), "%g", d);
    int has_dot = 0;
    for (int i = 0; buf[i]; i++) {
        if (buf[i] == '.' || buf[i] == 'e' || buf[i] == 'E' ||
            buf[i] == 'n' || buf[i] == 'i') {
            has_dot = 1;
            break;
        }
    }
    if (!has_dot)
        printf("%s.", buf);
    else
        printf("%s", buf);
}

void mml_fmt_str(const char *s) {
    printf("%s", s);
}

void mml_fmt_str_raw(mml_value v) {
    printf("%s", MML_STR_DATA(v));
}

void mml_fmt_byte(mml_value v) {
    printf("#%x", (int)MML_INT_VAL(v));
}

void mml_fmt_rune(mml_value v) {
    char buf[5];
    int len = utf8_encode(MML_INT_VAL(v), buf);
    buf[len] = '\0';
    printf("'%s'", buf);
}

void mml_fmt_newline(void) {
    mml_has_output = 1;
    printf("\n");
}

/* ---- Physical equality ---- */

mml_value mml_phys_equal(mml_value a, mml_value b) {
    return a == b ? MML_TRUE : MML_FALSE;
}

/* ---- String.of_byte ---- */

mml_value mml_string_of_byte(mml_value v) {
    mml_value s = mml_string_alloc(1);
    ((char *)MML_STR_DATA(s))[0] = (char)MML_INT_VAL(v);
    return s;
}

/* ---- Show support (string_of_*) ---- */

mml_value mml_string_of_float(mml_value v) {
    double d = mml_unbox_float(v);
    char buf[64];
    int len = snprintf(buf, sizeof(buf), "%g", d);
    int has_dot = 0;
    for (int i = 0; buf[i]; i++) {
        if (buf[i] == '.' || buf[i] == 'e' || buf[i] == 'E' ||
            buf[i] == 'n' || buf[i] == 'i') { has_dot = 1; break; }
    }
    if (!has_dot) {
        buf[len++] = '.';
        buf[len] = '\0';
    }
    return mml_string_from_buf(buf, len);
}

mml_value mml_string_of_bool(mml_value v) {
    return mml_string_from_cstr(v == MML_TRUE ? "true" : "false");
}

mml_value mml_show_unit(mml_value v) {
    (void)v;
    return mml_string_from_cstr("()");
}

mml_value mml_identity(mml_value v) { return v; }

/* ---- Generic show for any value (runtime heuristic) ---- */

mml_value mml_show_value(mml_value v) {
    if (MML_IS_INT(v)) {
        /* Tagged integer */
        int64_t n = MML_INT_VAL(v);
        char buf[32];
        int len = snprintf(buf, sizeof(buf), "%" PRId64, n);
        return mml_string_from_buf(buf, len);
    }
    /* Pointer value — check if it's a string */
    if (MML_HDR_TAG(v) == MML_HDR_STRING) {
        return v;  /* strings are already their own show representation */
    }
    /* Default: show as int (raw value) */
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%" PRId64, MML_INT_VAL(v));
    return mml_string_from_buf(buf, len);
}

/* ---- Show for byte and rune ---- */

mml_value mml_show_byte(mml_value v) {
    char buf[8];
    int len = snprintf(buf, sizeof(buf), "#%02x", (int)MML_INT_VAL(v));
    return mml_string_from_buf(buf, len);
}

mml_value mml_show_rune(mml_value v) {
    char buf[8];
    buf[0] = '\'';
    int n = utf8_encode(MML_INT_VAL(v), buf + 1);
    buf[1 + n] = '\'';
    return mml_string_from_buf(buf, 2 + n);
}

/* ---- Show for poly variants ---- */

/* names_list: MML list of (hash_tagged_int, name_string) pairs (2-tuples)
   show_payload_fn: closure to show payload values
   value: the poly variant value to show */
mml_value mml_show_polyvariant(mml_value names_list, mml_value show_payload_fn, mml_value value) {
    mml_value tag_val;
    mml_value payload = 0;
    int has_payload = 0;

    if (MML_IS_INT(value)) {
        /* No-arg: bare tagged int IS the tag */
        tag_val = value;
    } else {
        /* With-arg: [tag, payload] */
        mml_value *ptr = (mml_value *)(intptr_t)value;
        tag_val = ptr[0];
        payload = ptr[1];
        has_payload = 1;
    }

    /* Search the names list for a matching tag */
    mml_value cur = names_list;
    while (!MML_IS_INT(cur)) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        mml_value pair_val = cell[0];
        mml_value *pair = (mml_value *)(intptr_t)pair_val;
        mml_value entry_hash = pair[0];
        mml_value entry_name = pair[1];  /* MML string */

        if (entry_hash == tag_val) {
            /* Found matching tag */
            int64_t name_len = MML_STR_LEN(entry_name);
            const char *name_data = (const char *)MML_STR_DATA(entry_name);

            if (has_payload) {
                /* Show payload */
                mml_value payload_str = mml_apply1(show_payload_fn, payload);
                int64_t pl_len = MML_STR_LEN(payload_str);
                const char *pl_data = (const char *)MML_STR_DATA(payload_str);
                /* Build "`Name payload_str" */
                int64_t total = 1 + name_len + 1 + pl_len;
                mml_value result = mml_string_alloc(total);
                char *dst = (char *)MML_STR_DATA(result);
                dst[0] = '`';
                memcpy(dst + 1, name_data, name_len);
                dst[1 + name_len] = ' ';
                memcpy(dst + 1 + name_len + 1, pl_data, pl_len);
                return result;
            } else {
                /* Build "`Name" */
                int64_t total = 1 + name_len;
                mml_value result = mml_string_alloc(total);
                char *dst = (char *)MML_STR_DATA(result);
                dst[0] = '`';
                memcpy(dst + 1, name_data, name_len);
                return result;
            }
        }
        cur = cell[1];
    }

    /* Fallback: unknown tag */
    return mml_show_value(value);
}

/* ---- Show for records ---- */

/* fields_list: MML list of (name_string, show_fn) pairs
   record: pointer to record fields array (sorted alphabetically)
   Fields are accessed by index (0, 1, 2...) matching the sorted order in the list */
mml_value mml_show_record(mml_value fields_list, mml_value record) {
    int64_t cap = 64;
    char *buf = (char *)GC_malloc_atomic(cap);
    if (!buf) mml_panic("out of memory in show_record");
    int64_t len = 0;
    buf[len++] = '{'; buf[len++] = ' ';

    mml_value *rec_ptr = (mml_value *)(intptr_t)record;
    mml_value cur = fields_list;
    int idx = 0;
    int first = 1;
    while (!MML_IS_INT(cur)) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        mml_value pair_val = cell[0];
        mml_value *pair = (mml_value *)(intptr_t)pair_val;
        mml_value field_name = pair[0];  /* MML string */
        mml_value show_fn = pair[1];     /* closure */

        if (!first) {
            if (len + 2 >= cap) { cap *= 2; buf = GC_realloc(buf, cap); }
            buf[len++] = ';'; buf[len++] = ' ';
        }
        first = 0;

        /* Append "field_name = " */
        int64_t fn_len = MML_STR_LEN(field_name);
        const char *fn_data = (const char *)MML_STR_DATA(field_name);
        while (len + fn_len + 3 >= cap) { cap *= 2; buf = GC_realloc(buf, cap); }
        memcpy(buf + len, fn_data, fn_len);
        len += fn_len;
        buf[len++] = ' '; buf[len++] = '='; buf[len++] = ' ';

        /* Show the field value */
        mml_value field_val = rec_ptr[idx];
        mml_value shown = mml_apply1(show_fn, field_val);
        int64_t sv_len = MML_STR_LEN(shown);
        const char *sv_data = (const char *)MML_STR_DATA(shown);
        while (len + sv_len + 3 >= cap) { cap *= 2; buf = GC_realloc(buf, cap); }
        memcpy(buf + len, sv_data, sv_len);
        len += sv_len;

        idx++;
        cur = cell[1];
    }
    if (len + 2 >= cap) { cap *= 2; buf = GC_realloc(buf, cap); }
    buf[len++] = ' '; buf[len++] = '}';

    mml_value result = mml_string_from_buf(buf, len);
    return result;
}

/* show_fns_list is a list of show closures, one per element */
mml_value mml_show_tuple(mml_value show_fns_list, mml_value tuple) {
    int64_t cap = 64;
    char *buf = (char *)GC_malloc_atomic(cap);
    if (!buf) mml_panic("out of memory in show_tuple");
    int64_t len = 0;
    buf[len++] = '(';

    mml_value *tup_ptr = (mml_value *)(intptr_t)tuple;
    mml_value cur = show_fns_list;
    int idx = 0;
    int first = 1;
    while (!MML_IS_INT(cur)) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        mml_value show_fn = cell[0];

        if (!first) {
            if (len + 2 >= cap) { cap *= 2; buf = GC_realloc(buf, cap); }
            buf[len++] = ','; buf[len++] = ' ';
        }
        first = 0;

        mml_value elem_val = tup_ptr[idx];
        mml_value shown = mml_apply1(show_fn, elem_val);
        int64_t sv_len = MML_STR_LEN(shown);
        const char *sv_data = (const char *)MML_STR_DATA(shown);
        while (len + sv_len + 2 >= cap) { cap *= 2; buf = GC_realloc(buf, cap); }
        memcpy(buf + len, sv_data, sv_len);
        len += sv_len;

        idx++;
        cur = cell[1];
    }
    if (len + 1 >= cap) { cap = len + 2; buf = GC_realloc(buf, cap); }
    buf[len++] = ')';
    return mml_string_from_buf(buf, len);
}

/* ctors_list is a list of (tag_int, name_str, show_fn_or_unit) triples.
   value is the variant value. */
mml_value mml_show_variant(mml_value ctors_list, mml_value value) {
    int64_t tag;
    int is_tagged = MML_IS_INT(value);
    if (is_tagged) {
        tag = MML_INT_VAL(value);
    } else {
        mml_value *ptr = (mml_value *)(intptr_t)value;
        tag = MML_INT_VAL(ptr[0]);
    }

    /* Search ctors_list for matching tag */
    mml_value cur = ctors_list;
    while (!MML_IS_INT(cur)) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        mml_value *triple = (mml_value *)(intptr_t)cell[0];
        int64_t ctor_tag = MML_INT_VAL(triple[0]);
        if (ctor_tag == tag) {
            mml_value name_str = triple[1];
            mml_value show_fn = triple[2]; /* MML_UNIT if no payload */
            if (is_tagged || show_fn == MML_UNIT) {
                return name_str;
            }
            /* Has payload: "Name payload_shown" */
            mml_value *vptr = (mml_value *)(intptr_t)value;
            mml_value payload = vptr[1];
            mml_value payload_shown = mml_apply1(show_fn, payload);
            /* Check if payload needs parens (contains space and not quoted) */
            const char *ps_data = (const char *)MML_STR_DATA(payload_shown);
            int64_t ps_len = MML_STR_LEN(payload_shown);
            int needs_parens = 0;
            if (ps_len > 0 && ps_data[0] == '(') needs_parens = 0; /* already parenthesized */
            else {
                for (int64_t i = 0; i < ps_len; i++) {
                    if (ps_data[i] == ' ') { needs_parens = 1; break; }
                }
            }
            int64_t name_len = MML_STR_LEN(name_str);
            const char *name_data = (const char *)MML_STR_DATA(name_str);
            int64_t total = name_len + 1 + (needs_parens ? 2 : 0) + ps_len;
            char *buf = (char *)GC_malloc_atomic(total + 1);
            if (!buf) mml_panic("out of memory in show_variant");
            int64_t pos = 0;
            memcpy(buf + pos, name_data, name_len); pos += name_len;
            buf[pos++] = ' ';
            if (needs_parens) buf[pos++] = '(';
            memcpy(buf + pos, ps_data, ps_len); pos += ps_len;
            if (needs_parens) buf[pos++] = ')';
            return mml_string_from_buf(buf, pos);
        }
        cur = cell[1];
    }
    /* Fallback: tag not found */
    return mml_string_from_cstr("<variant>");
}

/* ---- Show for compound types ---- */

mml_value mml_show_list(mml_value show_elem_fn, mml_value list) {
    int64_t cap = 64;
    char *buf = (char *)GC_malloc_atomic(cap);
    if (!buf) mml_panic("out of memory in show_list");
    int64_t len = 0;
    buf[len++] = '[';

    mml_value cur = list;
    int first = 1;
    while (!MML_IS_INT(cur)) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        mml_value elem_str = mml_apply1(show_elem_fn, cell[0]);
        const char *es = MML_STR_DATA(elem_str);
        int64_t es_len = MML_STR_LEN(elem_str);
        int64_t need = len + (first ? 0 : 2) + es_len + 2;
        if (need >= cap) {
            while (need >= cap) cap *= 2;
            buf = (char *)GC_realloc(buf, cap);
            if (!buf) mml_panic("out of memory in show_list");
        }
        if (!first) { buf[len++] = ';'; buf[len++] = ' '; }
        memcpy(buf + len, es, es_len); len += es_len;
        first = 0;
        cur = cell[1];
    }
    if (len + 2 >= cap) {
        cap = len + 4;
        buf = (char *)GC_realloc(buf, cap);
    }
    buf[len++] = ']';
    buf[len] = '\0';
    mml_value result = mml_string_from_buf(buf, len);

    return result;
}

mml_value mml_show_array(mml_value show_elem_fn, mml_value array) {
    int64_t cap = 64;
    char *buf = (char *)GC_malloc_atomic(cap);
    if (!buf) mml_panic("out of memory in show_array");
    int64_t len = 0;
    buf[len++] = '#'; buf[len++] = '[';

    int64_t arr_len = MML_IS_INT(array) ? 0 : MML_ARR_LEN(array);
    mml_value *arr_data = MML_IS_INT(array) ? NULL : MML_ARR_DATA(array);
    for (int64_t i = 0; i < arr_len; i++) {
        mml_value elem_str = mml_apply1(show_elem_fn, arr_data[i]);
        const char *es = MML_STR_DATA(elem_str);
        int64_t es_len = MML_STR_LEN(elem_str);
        int64_t need = len + (i > 0 ? 2 : 0) + es_len + 2;
        if (need >= cap) {
            while (need >= cap) cap *= 2;
            buf = (char *)GC_realloc(buf, cap);
            if (!buf) mml_panic("out of memory in show_array");
        }
        if (i > 0) { buf[len++] = ';'; buf[len++] = ' '; }
        memcpy(buf + len, es, es_len); len += es_len;
    }
    if (len + 2 >= cap) {
        cap = len + 4;
        buf = (char *)GC_realloc(buf, cap);
    }
    buf[len++] = ']';
    buf[len] = '\0';
    mml_value result = mml_string_from_buf(buf, len);

    return result;
}

/* ---- Result printing ---- */

static void mml_print_result(mml_value v, int type_tag) {
    switch (type_tag) {
    case MML_RESULT_INT:
        printf("%" PRId64, MML_INT_VAL(v));
        break;
    case MML_RESULT_BOOL:
        printf("%s", v == MML_TRUE ? "true" : "false");
        break;
    case MML_RESULT_STRING:
        printf("%s", MML_STR_DATA(v));
        break;
    case MML_RESULT_FLOAT: {
        double d = mml_unbox_float(v);
        char buf[64];
        snprintf(buf, sizeof(buf), "%g", d);
        int has_dot = 0;
        for (int i = 0; buf[i]; i++) {
            if (buf[i] == '.' || buf[i] == 'e' || buf[i] == 'E' ||
                buf[i] == 'n' || buf[i] == 'i') {
                has_dot = 1;
                break;
            }
        }
        if (!has_dot)
            printf("%s.", buf);
        else
            printf("%s", buf);
        break;
    }
    case MML_RESULT_UNIT:
        printf("()");
        break;
    case MML_RESULT_BYTE:
        printf("#%x", (int)MML_INT_VAL(v));
        break;
    case MML_RESULT_RUNE: {
        char buf[5];
        int len = utf8_encode(MML_INT_VAL(v), buf);
        buf[len] = '\0';
        printf("'%s'", buf);
        break;
    }
    default:
        printf("<unknown type tag %d>", type_tag);
        break;
    }
}

/* ---- List operations ---- */

mml_value mml_list_rev(mml_value lst) {
    mml_value acc = MML_UNIT;  /* nil = tagged 1 */
    while (!(lst & 1)) {       /* while lst is a pointer (cons cell) */
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_value *new_cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        new_cell[0] = cell[0]; /* head */
        new_cell[1] = acc;     /* reversed tail */
        acc = (mml_value)(intptr_t)new_cell;
        lst = cell[1];         /* next */
    }
    return acc;
}

mml_value mml_list_length(mml_value lst) {
    int64_t len = 0;
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        lst = cell[1];
        len++;
    }
    return MML_TAG_INT(len);
}

mml_value mml_list_map(mml_value fn, mml_value lst) {
    if (lst & 1) return MML_UNIT;  /* nil -> nil */
    mml_value result = MML_UNIT;
    mml_value *tail = NULL;
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_value mapped = mml_apply1(fn, cell[0]);
        mml_value *new_cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        new_cell[0] = mapped;
        new_cell[1] = MML_UNIT;
        if (tail) tail[1] = (mml_value)(intptr_t)new_cell;
        else result = (mml_value)(intptr_t)new_cell;
        tail = new_cell;
        lst = cell[1];
    }
    return result;
}

mml_value mml_list_fold(mml_value fn, mml_value acc, mml_value lst) {
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        acc = mml_apply2(fn, acc, cell[0]);
        lst = cell[1];
    }
    return acc;
}

mml_value mml_list_filter(mml_value fn, mml_value lst) {
    if (lst & 1) return MML_UNIT;
    /* Build in reverse, then reverse */
    mml_value result = MML_UNIT;
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_value keep = mml_apply1(fn, cell[0]);
        if (keep == MML_TRUE) {
            mml_value *new_cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
            new_cell[0] = cell[0];
            new_cell[1] = result;
            result = (mml_value)(intptr_t)new_cell;
        }
        lst = cell[1];
    }
    return mml_list_rev(result);
}

mml_value mml_list_find(mml_value fn, mml_value lst) {
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_value match = mml_apply1(fn, cell[0]);
        if (match == MML_TRUE) {
            /* Some(elem) */
            mml_value *opt = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_VARIANT, 0));
            opt[0] = MML_TAG_INT(1); /* Some tag */
            opt[1] = cell[0];
            return (mml_value)(intptr_t)opt;
        }
        lst = cell[1];
    }
    return MML_TAG_INT(0); /* None */
}

mml_value mml_list_find_map(mml_value fn, mml_value lst) {
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_value result = mml_apply1(fn, cell[0]);
        /* Some(x) is a heap pointer (bit 0 clear), None is MML_TAG_INT(0) */
        if (!(result & 1)) return result;
        lst = cell[1];
    }
    return MML_TAG_INT(0); /* None */
}

mml_value mml_list_exists(mml_value fn, mml_value lst) {
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_value result = mml_apply1(fn, cell[0]);
        if (result == MML_TRUE) return MML_TRUE;
        lst = cell[1];
    }
    return MML_FALSE;
}

mml_value mml_list_forall(mml_value fn, mml_value lst) {
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_value result = mml_apply1(fn, cell[0]);
        if (result == MML_FALSE) return MML_FALSE;
        lst = cell[1];
    }
    return MML_TRUE;
}

mml_value mml_list_iter(mml_value fn, mml_value lst) {
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_apply1(fn, cell[0]);
        lst = cell[1];
    }
    return MML_UNIT;
}

mml_value mml_list_mapi(mml_value fn, mml_value lst) {
    mml_value result = MML_UNIT;
    int64_t i = 0;
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_value mapped = mml_apply2(fn, MML_TAG_INT(i), cell[0]);
        mml_value *new_cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        new_cell[0] = mapped;
        new_cell[1] = result;
        result = (mml_value)(intptr_t)new_cell;
        i++;
        lst = cell[1];
    }
    return mml_list_rev(result);
}

mml_value mml_list_concat(mml_value a, mml_value b) {
    /* Append two lists: concat [1;2] [3;4] = [1;2;3;4] */
    if (a & 1) return b; /* nil ++ b = b */
    /* Reverse a, then prepend to b */
    mml_value rev_a = mml_list_rev(a);
    mml_value result = b;
    while (!(rev_a & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)rev_a;
        mml_value *new_cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        new_cell[0] = cell[0];
        new_cell[1] = result;
        result = (mml_value)(intptr_t)new_cell;
        rev_a = cell[1];
    }
    return result;
}

mml_value mml_list_flatten(mml_value lst) {
    /* lst is a list of lists; concatenate them all */
    mml_value result = MML_UNIT;
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        result = mml_list_concat(result, cell[0]);
        lst = cell[1];
    }
    return result;
}

mml_value mml_list_sort(mml_value fn, mml_value lst) {
    /* Count elements */
    int64_t len = 0;
    mml_value cur = lst;
    while (!(cur & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        len++;
        cur = cell[1];
    }
    if (len <= 1) return lst;

    /* Copy to array */
    mml_value *arr = (mml_value *)GC_malloc(len * sizeof(mml_value));
    cur = lst;
    for (int64_t i = 0; i < len; i++) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        arr[i] = cell[0];
        cur = cell[1];
    }

    /* Bottom-up iterative merge sort — O(n log n), stable */
    mml_value *tmp = (mml_value *)GC_malloc(len * sizeof(mml_value));
    if (!tmp) mml_panic("out of memory in list_sort");

    for (int64_t width = 1; width < len; width *= 2) {
        for (int64_t i = 0; i < len; i += 2 * width) {
            int64_t left = i;
            int64_t mid = i + width;
            if (mid > len) mid = len;
            int64_t right = i + 2 * width;
            if (right > len) right = len;
            /* Merge arr[left..mid) and arr[mid..right) into tmp[left..right) */
            int64_t l = left, r = mid, t = left;
            while (l < mid && r < right) {
                mml_value cmp = mml_apply2(fn, arr[l], arr[r]);
                if (MML_INT_VAL(cmp) <= 0) {
                    tmp[t++] = arr[l++];
                } else {
                    tmp[t++] = arr[r++];
                }
            }
            while (l < mid) tmp[t++] = arr[l++];
            while (r < right) tmp[t++] = arr[r++];
        }
        /* Swap arr and tmp */
        mml_value *swap = arr;
        arr = tmp;
        tmp = swap;
    }

    /* Build list from sorted array (reverse order) */
    mml_value result = MML_UNIT;
    for (int64_t i = len - 1; i >= 0; i--) {
        mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        cell[0] = arr[i];
        cell[1] = result;
        result = (mml_value)(intptr_t)cell;
    }
    return result;
}

mml_value mml_list_assoc_opt(mml_value key, mml_value lst) {
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        mml_value *pair = (mml_value *)(intptr_t)cell[0];
        mml_value k = pair[0];
        /* Structural equality for key comparison */
        if (mml_structural_eq(k, key) == MML_TRUE) {
            /* Some(pair[1]) */
            mml_value *opt = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_VARIANT, 0));
            opt[0] = MML_TAG_INT(1); /* Some tag */
            opt[1] = pair[1];
            return (mml_value)(intptr_t)opt;
        }
        lst = cell[1];
    }
    return MML_TAG_INT(0); /* None */
}

mml_value mml_list_fold_right(mml_value fn, mml_value lst, mml_value acc) {
    /* Count elements and copy to array for right-to-left processing */
    int64_t len = 0;
    mml_value cur = lst;
    while (!(cur & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        len++;
        cur = cell[1];
    }
    if (len == 0) return acc;
    mml_value *arr = (mml_value *)GC_malloc(len * sizeof(mml_value));
    cur = lst;
    for (int64_t i = 0; i < len; i++) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        arr[i] = cell[0];
        cur = cell[1];
    }
    mml_value result = acc;
    for (int64_t i = len - 1; i >= 0; i--) {
        result = mml_apply2(fn, arr[i], result);
    }
    return result;
}

mml_value mml_list_init(mml_value n_val, mml_value fn) {
    int64_t n = MML_INT_VAL(n_val);
    /* Build in reverse then reverse */
    mml_value result = MML_UNIT; /* nil */
    for (int64_t i = n - 1; i >= 0; i--) {
        mml_value elem = mml_apply1(fn, MML_TAG_INT(i));
        mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        cell[0] = elem;
        cell[1] = result;
        result = (mml_value)(intptr_t)cell;
    }
    return result;
}

mml_value mml_list_map2(mml_value fn, mml_value lst1, mml_value lst2) {
    mml_value result = MML_UNIT;
    mml_value *tail = NULL;
    while (!(lst1 & 1) && !(lst2 & 1)) {
        mml_value *c1 = (mml_value *)(intptr_t)lst1;
        mml_value *c2 = (mml_value *)(intptr_t)lst2;
        mml_value elem = mml_apply2(fn, c1[0], c2[0]);
        mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        cell[0] = elem;
        cell[1] = MML_UNIT;
        if (tail) tail[1] = (mml_value)(intptr_t)cell;
        else result = (mml_value)(intptr_t)cell;
        tail = cell;
        lst1 = c1[1];
        lst2 = c2[1];
    }
    return result;
}

mml_value mml_list_iter2(mml_value fn, mml_value lst1, mml_value lst2) {
    while (!(lst1 & 1) && !(lst2 & 1)) {
        mml_value *c1 = (mml_value *)(intptr_t)lst1;
        mml_value *c2 = (mml_value *)(intptr_t)lst2;
        mml_apply2(fn, c1[0], c2[0]);
        lst1 = c1[1];
        lst2 = c2[1];
    }
    return MML_UNIT;
}

mml_value mml_array_copy(mml_value arr) {
    if (arr & 1) return MML_UNIT; /* empty */
    int64_t len = MML_ARR_LEN(arr);
    mml_value *new_arr = (mml_value *)mml_alloc((len + 1) * 8, MML_MAKE_HDR(MML_HDR_ARRAY, 0));
    new_arr[0] = len;
    mml_value *src = MML_ARR_DATA(arr);
    for (int64_t i = 0; i < len; i++) {
        new_arr[i + 1] = src[i];
    }
    return (mml_value)(intptr_t)new_arr;
}

mml_value mml_array_init(mml_value n_val, mml_value fn) {
    int64_t n = MML_INT_VAL(n_val);
    if (n <= 0) return MML_UNIT;
    mml_value *arr = (mml_value *)mml_alloc((n + 1) * 8, MML_MAKE_HDR(MML_HDR_ARRAY, 0));
    arr[0] = n;
    for (int64_t i = 0; i < n; i++) {
        arr[i + 1] = mml_apply1(fn, MML_TAG_INT(i));
    }
    return (mml_value)(intptr_t)arr;
}

mml_value mml_array_map(mml_value fn, mml_value arr) {
    if (arr & 1) return MML_UNIT; /* empty */
    int64_t len = MML_ARR_LEN(arr);
    mml_value *new_arr = (mml_value *)mml_alloc((len + 1) * 8, MML_MAKE_HDR(MML_HDR_ARRAY, 0));
    new_arr[0] = len;
    mml_value *src = MML_ARR_DATA(arr);
    for (int64_t i = 0; i < len; i++) {
        new_arr[i + 1] = mml_apply1(fn, src[i]);
    }
    return (mml_value)(intptr_t)new_arr;
}

mml_value mml_array_mapi(mml_value fn, mml_value arr) {
    if (arr & 1) return MML_UNIT;
    int64_t len = MML_ARR_LEN(arr);
    mml_value *new_arr = (mml_value *)mml_alloc((len + 1) * 8, MML_MAKE_HDR(MML_HDR_ARRAY, 0));
    new_arr[0] = len;
    mml_value *src = MML_ARR_DATA(arr);
    for (int64_t i = 0; i < len; i++) {
        new_arr[i + 1] = mml_apply2(fn, MML_TAG_INT(i), src[i]);
    }
    return (mml_value)(intptr_t)new_arr;
}

mml_value mml_array_iter(mml_value fn, mml_value arr) {
    if (arr & 1) return MML_UNIT;
    int64_t len = MML_ARR_LEN(arr);
    mml_value *src = MML_ARR_DATA(arr);
    for (int64_t i = 0; i < len; i++) {
        mml_apply1(fn, src[i]);
    }
    return MML_UNIT;
}

/* ---- Breakable fold (for break-in-fold support) ---- */

static int mml_fold_broken = 0;
static mml_value mml_fold_break_val = 0;

mml_value mml_list_fold_breakable(mml_value fn, mml_value acc, mml_value lst) {
    /* Save outer fold state for reentrancy */
    int saved_broken = mml_fold_broken;
    mml_value saved_break_val = mml_fold_break_val;
    mml_fold_broken = 0;
    while (!(lst & 1)) {
        mml_value *cell = (mml_value *)(intptr_t)lst;
        acc = mml_apply2(fn, acc, cell[0]);
        if (mml_fold_broken) {
            acc = mml_fold_break_val;
            /* Restore outer state */
            mml_fold_broken = saved_broken;
            mml_fold_break_val = saved_break_val;
            return acc;
        }
        lst = cell[1];
    }
    /* Restore outer state */
    mml_fold_broken = saved_broken;
    mml_fold_break_val = saved_break_val;
    return acc;
}

void mml_fold_break(mml_value val) {
    mml_fold_broken = 1;
    mml_fold_break_val = val;
}

int64_t mml_check_fold_broken(void) {
    return mml_fold_broken;
}

mml_value mml_consume_fold_break(void) {
    mml_fold_broken = 0;
    return mml_fold_break_val;
}

/* ---- Early return support (for return inside for-in loops) ---- */

static int mml_early_return_flag = 0;
static mml_value mml_early_return_val = 0;

void mml_set_early_return(mml_value val) {
    mml_early_return_flag = 1;
    mml_early_return_val = val;
    /* Also break the fold so iteration stops immediately */
    mml_fold_broken = 1;
    mml_fold_break_val = val;
}

int64_t mml_check_early_return(void) {
    return mml_early_return_flag;
}

mml_value mml_get_early_return(void) {
    mml_early_return_flag = 0;
    return mml_early_return_val;
}

/* ---- Index operations ---- */

mml_value mml_list_nth(mml_value idx_tagged, mml_value lst) {
    int64_t idx = MML_INT_VAL(idx_tagged);
    int64_t i = 0;
    while (!(lst & 1)) {
        if (i == idx) {
            mml_value *cell = (mml_value *)(intptr_t)lst;
            return cell[0];
        }
        mml_value *cell = (mml_value *)(intptr_t)lst;
        lst = cell[1];
        i++;
    }
    mml_panic("index out of bounds");
    return MML_UNIT;
}

mml_value mml_string_get_byte(mml_value idx_tagged, mml_value str) {
    int64_t idx = MML_INT_VAL(idx_tagged);
    int64_t len = MML_STR_LEN(str);
    if (idx < 0 || idx >= len) mml_panic("string index out of bounds");
    return MML_TAG_INT((int64_t)(unsigned char)MML_STR_DATA(str)[idx]);
}

/* ---- Array operations (flat mutable buffers) ---- */

mml_value mml_array_length(mml_value arr) {
    if (MML_IS_INT(arr)) return MML_TAG_INT(0); /* empty array */
    return MML_TAG_INT(MML_ARR_LEN(arr));
}

mml_value mml_array_get(mml_value idx_tagged, mml_value arr) {
    int64_t idx = MML_INT_VAL(idx_tagged);
    if (MML_IS_INT(arr)) mml_panic("array index out of bounds");
    int64_t len = MML_ARR_LEN(arr);
    if (idx < 0 || idx >= len) mml_panic("array index out of bounds");
    return MML_ARR_DATA(arr)[idx];
}

mml_value mml_array_set(mml_value arr, mml_value idx_tagged, mml_value val) {
    int64_t idx = MML_INT_VAL(idx_tagged);
    if (MML_IS_INT(arr)) mml_panic("array index out of bounds");
    int64_t len = MML_ARR_LEN(arr);
    if (idx < 0 || idx >= len) mml_panic("array index out of bounds");
    MML_ARR_DATA(arr)[idx] = val;
    return MML_UNIT;
}

mml_value mml_array_make(mml_value n_tagged, mml_value val) {
    int64_t n = MML_INT_VAL(n_tagged);
    if (n <= 0) return MML_UNIT;
    int64_t *p = (int64_t *)mml_alloc((n + 1) * 8, MML_MAKE_HDR(MML_HDR_ARRAY, 0));
    p[0] = n;
    mml_value *data = (mml_value *)(p + 1);
    for (int64_t i = 0; i < n; i++) data[i] = val;
    return (mml_value)(intptr_t)p;
}

mml_value mml_array_of_list(mml_value lst) {
    /* Count list length */
    int64_t len = 0;
    mml_value cur = lst;
    while (!(cur & 1)) { len++; cur = ((mml_value*)(intptr_t)cur)[1]; }
    if (len == 0) return MML_UNIT;
    int64_t *p = (int64_t *)mml_alloc((len + 1) * 8, MML_MAKE_HDR(MML_HDR_ARRAY, 0));
    p[0] = len;
    mml_value *data = (mml_value *)(p + 1);
    cur = lst;
    for (int64_t i = 0; i < len; i++) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        data[i] = cell[0];
        cur = cell[1];
    }
    return (mml_value)(intptr_t)p;
}

mml_value mml_array_to_list(mml_value arr) {
    if (MML_IS_INT(arr)) return MML_UNIT; /* empty → nil */
    int64_t len = MML_ARR_LEN(arr);
    mml_value *data = MML_ARR_DATA(arr);
    mml_value acc = MML_UNIT; /* nil */
    for (int64_t i = len - 1; i >= 0; i--) {
        mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        cell[0] = data[i];
        cell[1] = acc;
        acc = (mml_value)(intptr_t)cell;
    }
    return acc;
}

mml_value mml_array_fold(mml_value fn, mml_value acc, mml_value arr) {
    if (MML_IS_INT(arr)) return acc; /* empty array */
    int64_t len = MML_ARR_LEN(arr);
    mml_value *data = MML_ARR_DATA(arr);
    for (int64_t i = 0; i < len; i++) {
        acc = mml_apply2(fn, acc, data[i]);
    }
    return acc;
}

mml_value mml_array_fold_breakable(mml_value fn, mml_value acc, mml_value arr) {
    /* Save outer fold state for reentrancy */
    int saved_broken = mml_fold_broken;
    mml_value saved_break_val = mml_fold_break_val;
    mml_fold_broken = 0;
    if (MML_IS_INT(arr)) {
        mml_fold_broken = saved_broken;
        mml_fold_break_val = saved_break_val;
        return acc; /* empty array */
    }
    int64_t len = MML_ARR_LEN(arr);
    mml_value *data = MML_ARR_DATA(arr);
    for (int64_t i = 0; i < len; i++) {
        acc = mml_apply2(fn, acc, data[i]);
        if (mml_fold_broken) {
            acc = mml_fold_break_val;
            mml_fold_broken = saved_broken;
            mml_fold_break_val = saved_break_val;
            return acc;
        }
    }
    mml_fold_broken = saved_broken;
    mml_fold_break_val = saved_break_val;
    return acc;
}

mml_value mml_array_eq(mml_value a, mml_value b) {
    /* Both empty */
    if (MML_IS_INT(a) && MML_IS_INT(b)) return a == b ? MML_TRUE : MML_FALSE;
    /* One empty, one not */
    if (MML_IS_INT(a) || MML_IS_INT(b)) return MML_FALSE;
    int64_t la = MML_ARR_LEN(a);
    int64_t lb = MML_ARR_LEN(b);
    if (la != lb) return MML_FALSE;
    mml_value *da = MML_ARR_DATA(a);
    mml_value *db = MML_ARR_DATA(b);
    for (int64_t i = 0; i < la; i++) {
        if (mml_structural_eq(da[i], db[i]) != MML_TRUE) return MML_FALSE;
    }
    return MML_TRUE;
}

mml_value mml_list_eq(mml_value a, mml_value b) {
    while (1) {
        if (MML_IS_INT(a) && MML_IS_INT(b)) return MML_TRUE;   /* both nil */
        if (MML_IS_INT(a) || MML_IS_INT(b)) return MML_FALSE;  /* different lengths */
        mml_value *ca = (mml_value *)(intptr_t)a;
        mml_value *cb = (mml_value *)(intptr_t)b;
        if (mml_structural_eq(ca[0], cb[0]) != MML_TRUE) return MML_FALSE;
        a = ca[1];
        b = cb[1];
    }
}

mml_value mml_tuple_eq(mml_value a, mml_value b, mml_value n_val) {
    int64_t n = MML_INT_VAL(n_val);
    mml_value *pa = (mml_value *)(intptr_t)a;
    mml_value *pb = (mml_value *)(intptr_t)b;
    for (int64_t i = 0; i < n; i++) {
        if (mml_structural_eq(pa[i], pb[i]) != MML_TRUE) return MML_FALSE;
    }
    return MML_TRUE;
}

mml_value mml_record_eq(mml_value a, mml_value b) {
    /* Records: header at ptr[-1] encodes size in bits 63-16 */
    mml_value *pa = (mml_value *)(intptr_t)a;
    mml_value *pb = (mml_value *)(intptr_t)b;
    int64_t n = MML_HDR_SIZE(a);
    int64_t nb = MML_HDR_SIZE(b);
    if (n != nb) return MML_FALSE;
    for (int64_t i = 0; i < n; i++) {
        if (mml_structural_eq(pa[i], pb[i]) != MML_TRUE) return MML_FALSE;
    }
    return MML_TRUE;
}


/* ---- Rune/String extra operations ---- */

mml_value mml_rune_to_string(mml_value cp_tagged) {
    char buf[4];
    int len = utf8_encode(MML_INT_VAL(cp_tagged), buf);
    return mml_string_from_buf(buf, len);
}

mml_value mml_string_rune_length(mml_value s) {
    int64_t byte_len = MML_STR_LEN(s);
    const char *data = MML_STR_DATA(s);
    int64_t count = 0;
    for (int64_t i = 0; i < byte_len; ) {
        utf8_advance(data, byte_len, &i);
        count++;
    }
    return MML_TAG_INT(count);
}

mml_value mml_string_of_bytes(mml_value lst) {
    /* First pass: count length */
    int64_t len = 0;
    mml_value cur = lst;
    while (!(cur & 1)) { len++; cur = ((mml_value*)(intptr_t)cur)[1]; }
    /* Second pass: fill buffer */
    mml_value s = mml_string_alloc(len);
    char *dst = (char *)MML_STR_DATA(s);
    cur = lst;
    for (int64_t i = 0; i < len; i++) {
        mml_value *cell = (mml_value *)(intptr_t)cur;
        dst[i] = (char)MML_INT_VAL(cell[0]);
        cur = cell[1];
    }
    return s;
}

mml_value mml_string_of_runes(mml_value lst) {
    /* Two-pass: measure then fill */
    int64_t total = 0;
    mml_value cur = lst;
    while (!(cur & 1)) {
        int64_t cp = MML_INT_VAL(((mml_value*)(intptr_t)cur)[0]);
        if (cp < 0x80) total += 1;
        else if (cp < 0x800) total += 2;
        else if (cp < 0x10000) total += 3;
        else total += 4;
        cur = ((mml_value*)(intptr_t)cur)[1];
    }
    mml_value s = mml_string_alloc(total);
    char *dst = (char *)MML_STR_DATA(s);
    int64_t pos = 0;
    cur = lst;
    while (!(cur & 1)) {
        int64_t cp = MML_INT_VAL(((mml_value*)(intptr_t)cur)[0]);
        if (cp < 0x80) { dst[pos++] = cp; }
        else if (cp < 0x800) {
            dst[pos++] = 0xC0 | (cp >> 6);
            dst[pos++] = 0x80 | (cp & 0x3F);
        } else if (cp < 0x10000) {
            dst[pos++] = 0xE0 | (cp >> 12);
            dst[pos++] = 0x80 | ((cp >> 6) & 0x3F);
            dst[pos++] = 0x80 | (cp & 0x3F);
        } else {
            dst[pos++] = 0xF0 | (cp >> 18);
            dst[pos++] = 0x80 | ((cp >> 12) & 0x3F);
            dst[pos++] = 0x80 | ((cp >> 6) & 0x3F);
            dst[pos++] = 0x80 | (cp & 0x3F);
        }
        cur = ((mml_value*)(intptr_t)cur)[1];
    }
    return s;
}

mml_value mml_string_sub(mml_value s, mml_value start_tagged, mml_value len_tagged) {
    int64_t start = MML_INT_VAL(start_tagged);
    int64_t len = MML_INT_VAL(len_tagged);
    int64_t slen = MML_STR_LEN(s);
    if (start < 0) start = 0;
    if (start + len > slen) len = slen - start;
    if (len <= 0) return mml_string_alloc(0);
    return mml_string_from_buf(MML_STR_DATA(s) + start, len);
}

mml_value mml_string_contains(mml_value s, mml_value sub) {
    int64_t slen = MML_STR_LEN(s);
    int64_t sublen = MML_STR_LEN(sub);
    if (sublen == 0) return MML_TRUE;
    if (sublen > slen) return MML_FALSE;
    const char *sd = MML_STR_DATA(s);
    const char *subd = MML_STR_DATA(sub);
    for (int64_t i = 0; i <= slen - sublen; i++) {
        if (memcmp(sd + i, subd, sublen) == 0) return MML_TRUE;
    }
    return MML_FALSE;
}

mml_value mml_string_to_list(mml_value s) {
    int64_t len = MML_STR_LEN(s);
    const char *data = MML_STR_DATA(s);
    mml_value acc = MML_UNIT; /* nil */
    for (int64_t i = len - 1; i >= 0; i--) {
        mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        cell[0] = MML_TAG_INT((int64_t)(unsigned char)data[i]);
        cell[1] = acc;
        acc = (mml_value)(intptr_t)cell;
    }
    return acc;
}

/* ---- String.get_rune / String.to_bytes / String.to_runes ---- */

/* decode_utf8 is now the top-level utf8_decode helper (see above) */

mml_value mml_string_get_rune(mml_value idx_tagged, mml_value s) {
    int64_t idx = MML_INT_VAL(idx_tagged);
    int64_t len = MML_STR_LEN(s);
    const char *data = MML_STR_DATA(s);
    int64_t pos = 0;
    int64_t rune_idx = 0;
    while (pos < len) {
        if (rune_idx == idx) {
            int64_t cp = utf8_decode(data, len, &pos);
            return MML_TAG_INT(cp);
        }
        utf8_advance(data, len, &pos);
        rune_idx++;
    }
    mml_panic("string rune index out of bounds");
    return MML_UNIT;
}

mml_value mml_string_to_bytes(mml_value s) {
    int64_t len = MML_STR_LEN(s);
    const char *data = MML_STR_DATA(s);
    mml_value acc = MML_UNIT; /* nil */
    for (int64_t i = len - 1; i >= 0; i--) {
        mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        cell[0] = MML_TAG_INT((int64_t)(unsigned char)data[i]);
        cell[1] = acc;
        acc = (mml_value)(intptr_t)cell;
    }
    return acc;
}

mml_value mml_string_to_runes(mml_value s) {
    int64_t len = MML_STR_LEN(s);
    const char *data = MML_STR_DATA(s);
    /* First pass: count runes and decode into temp array */
    int64_t cap = len; /* at most len runes */
    int64_t *runes = (int64_t *)GC_malloc_atomic(cap * sizeof(int64_t));
    if (!runes) mml_panic("out of memory in string_to_runes");
    int64_t count = 0;
    int64_t pos = 0;
    while (pos < len) {
        runes[count++] = utf8_decode(data, len, &pos);
    }
    /* Build list right-to-left */
    mml_value acc = MML_UNIT; /* nil */
    for (int64_t i = count - 1; i >= 0; i--) {
        mml_value *cell = (mml_value *)mml_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));
        cell[0] = MML_TAG_INT(runes[i]);
        cell[1] = acc;
        acc = (mml_value)(intptr_t)cell;
    }
    return acc;
}

/* ---- Association list helpers for map pattern matching ---- */

mml_value mml_assoc_has(mml_value key, mml_value alist) {
    while (!MML_IS_INT(alist)) {
        mml_value *cell = (mml_value *)(intptr_t)alist;
        mml_value pair = cell[0];
        mml_value *pp = (mml_value *)(intptr_t)pair;
        if (mml_structural_eq(pp[0], key) == MML_TRUE) return MML_TRUE;
        alist = cell[1];
    }
    return MML_FALSE;
}

mml_value mml_assoc_get(mml_value key, mml_value alist) {
    while (!MML_IS_INT(alist)) {
        mml_value *cell = (mml_value *)(intptr_t)alist;
        mml_value pair = cell[0];
        mml_value *pp = (mml_value *)(intptr_t)pair;
        if (mml_structural_eq(pp[0], key) == MML_TRUE) return pp[1];
        alist = cell[1];
    }
    mml_panic("key not found in map pattern");
    return MML_UNIT;
}

/* ---- Structural equality ---- */

/*
 * Structural equality for MiniML values.
 * Uses header word tag at ptr[-1] to dispatch by type.
 */
mml_value mml_structural_eq(mml_value a, mml_value b) {
restart:
    if (a == b) return MML_TRUE;
    if (MML_IS_INT(a) || MML_IS_INT(b)) return MML_FALSE;

    int tag_a = MML_HDR_TAG(a), tag_b = MML_HDR_TAG(b);
    if (tag_a != tag_b) return MML_FALSE;

    mml_value *pa = (mml_value *)(intptr_t)a;
    mml_value *pb = (mml_value *)(intptr_t)b;

    switch (tag_a) {
    case MML_HDR_STRING:
        return mml_string_eq(a, b);
    case MML_HDR_FLOAT:
        return *(double *)pa == *(double *)pb ? MML_TRUE : MML_FALSE;
    case MML_HDR_CONS:
        if (mml_structural_eq(pa[0], pb[0]) != MML_TRUE) return MML_FALSE;
        a = pa[1]; b = pb[1]; goto restart;
    case MML_HDR_TUPLE:
    case MML_HDR_RECORD: {
        int64_t n = MML_HDR_SIZE(a);
        if (n != MML_HDR_SIZE(b)) return MML_FALSE;
        for (int64_t i = 0; i < n; i++)
            if (mml_structural_eq(pa[i], pb[i]) != MML_TRUE) return MML_FALSE;
        return MML_TRUE;
    }
    case MML_HDR_VARIANT:
    case MML_HDR_POLYVAR:
        if (pa[0] != pb[0]) return MML_FALSE;
        return mml_structural_eq(pa[1], pb[1]);
    case MML_HDR_PAIR:
        if (mml_structural_eq(pa[0], pb[0]) != MML_TRUE) return MML_FALSE;
        return mml_structural_eq(pa[1], pb[1]);
    case MML_HDR_ARRAY:
        return mml_array_eq(a, b);
    default:
        return MML_FALSE; /* closures, refs: not structurally comparable */
    }
}

/* ---- Closure application ---- */

typedef mml_value (*fn0)(void *);
typedef mml_value (*fn1)(void *, mml_value);
typedef mml_value (*fn2)(void *, mml_value, mml_value);
typedef mml_value (*fn3)(void *, mml_value, mml_value, mml_value);
typedef mml_value (*fn4)(void *, mml_value, mml_value, mml_value, mml_value);
typedef mml_value (*fn5)(void *, mml_value, mml_value, mml_value, mml_value, mml_value);
typedef mml_value (*fn6)(void *, mml_value, mml_value, mml_value, mml_value, mml_value, mml_value);
typedef mml_value (*fn7)(void *, mml_value, mml_value, mml_value, mml_value, mml_value, mml_value, mml_value);
typedef mml_value (*fn8)(void *, mml_value, mml_value, mml_value, mml_value, mml_value, mml_value, mml_value, mml_value);
/* Arity > 8: array-based convention fn(env, args_ptr) */
typedef mml_value (*fn_arr)(void *, mml_value *);

mml_value mml_call_fn(void *fn, void *env, mml_value *args, int64_t n) {
    switch (n) {
    case 0: return ((fn0)fn)(env);
    case 1: return ((fn1)fn)(env, args[0]);
    case 2: return ((fn2)fn)(env, args[0], args[1]);
    case 3: return ((fn3)fn)(env, args[0], args[1], args[2]);
    case 4: return ((fn4)fn)(env, args[0], args[1], args[2], args[3]);
    case 5: return ((fn5)fn)(env, args[0], args[1], args[2], args[3], args[4]);
    case 6: return ((fn6)fn)(env, args[0], args[1], args[2], args[3], args[4], args[5]);
    case 7: return ((fn7)fn)(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
    case 8: return ((fn8)fn)(env, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
    default:
        /* Arity > 8: closure function uses (env, args_ptr) convention */
        return ((fn_arr)fn)(env, args);
    }
}

static mml_value mml_apply_call(mml_value *root, mml_value *saved, int64_t n_saved,
                                mml_value *new_args, int64_t n_new, int64_t arity) {
    /* Assemble all args into a contiguous buffer and call */
    mml_value all_args[arity];  /* VLA — arity is always small and bounded by source code */
    for (int64_t i = 0; i < n_saved; i++) all_args[i] = saved[i];
    for (int64_t i = 0; i < n_new; i++) all_args[n_saved + i] = new_args[i];
    void *fn = (void *)(intptr_t)root[0];
    return mml_call_fn(fn, root, all_args, arity);
}

mml_value mml_apply(mml_value closure_val, mml_value *new_args, int64_t n_new) {
    mml_value *cls = (mml_value *)(intptr_t)closure_val;
    int64_t arity = cls[1];
    int64_t num_applied = cls[2];

    /* Determine root closure and previously-saved args */
    mml_value *root;
    mml_value *saved;
    int64_t n_saved;
    if (num_applied > 0) {
        root = (mml_value *)(intptr_t)cls[3]; /* original closure */
        saved = &cls[4];
        n_saved = num_applied;
    } else {
        root = cls;
        saved = NULL;
        n_saved = 0;
    }

    int64_t total = n_saved + n_new;

    if (total == arity) {
        /* Exact application */
        return mml_apply_call(root, saved, n_saved, new_args, n_new, arity);
    } else if (total < arity) {
        /* Under-application: create PAP closure */
        int64_t pap_size = (4 + total) * 8;
        mml_value *pap = (mml_value *)mml_alloc(pap_size, MML_MAKE_HDR(MML_HDR_CLOSURE, 0));
        pap[0] = 0;  /* unused fn_ptr slot for PAP */
        pap[1] = arity;
        pap[2] = total;
        pap[3] = (mml_value)(intptr_t)root; /* original closure */
        for (int64_t i = 0; i < n_saved; i++) pap[4 + i] = saved[i];
        for (int64_t i = 0; i < n_new; i++) pap[4 + n_saved + i] = new_args[i];
        return (mml_value)(intptr_t)pap;
    } else {
        /* Over-application: exact-call with first (arity - n_saved) new args,
           then apply result to remaining */
        int64_t needed = arity - n_saved;
        mml_value result = mml_apply_call(root, saved, n_saved, new_args, needed, arity);
        return mml_apply(result, new_args + needed, n_new - needed);
    }
}

mml_value mml_apply1(mml_value cls, mml_value a0) {
    mml_value args[] = {a0};
    return mml_apply(cls, args, 1);
}

mml_value mml_apply2(mml_value cls, mml_value a0, mml_value a1) {
    mml_value args[] = {a0, a1};
    return mml_apply(cls, args, 2);
}

mml_value mml_apply3(mml_value cls, mml_value a0, mml_value a1, mml_value a2) {
    mml_value args[] = {a0, a1, a2};
    return mml_apply(cls, args, 3);
}

mml_value mml_apply4(mml_value cls, mml_value a0, mml_value a1, mml_value a2, mml_value a3) {
    mml_value args[] = {a0, a1, a2, a3};
    return mml_apply(cls, args, 4);
}

mml_value mml_apply5(mml_value cls, mml_value a0, mml_value a1, mml_value a2, mml_value a3, mml_value a4) {
    mml_value args[] = {a0, a1, a2, a3, a4};
    return mml_apply(cls, args, 5);
}

mml_value mml_apply6(mml_value cls, mml_value a0, mml_value a1, mml_value a2, mml_value a3, mml_value a4, mml_value a5) {
    mml_value args[] = {a0, a1, a2, a3, a4, a5};
    return mml_apply(cls, args, 6);
}

/* ---- Entry point ---- */

int main(int argc, char **argv) {
    (void)argc;
    (void)argv;
    GC_INIT();
    mml_value result = mml_main();
    /*
     * Match bytecode VM output behavior:
     * - If result is unit AND print was called: just show print output (no "()")
     * - If result is unit AND no print output: show "()"
     * - If result is not unit: show print output + result
     */
    if (mml_result_type == MML_RESULT_UNIT && mml_has_output) {
        /* print functions already output to stdout; nothing more needed */
    } else if (mml_result_type == MML_RESULT_COMPOUND) {
        mml_format_result(result);
        printf("\n");
    } else {
        mml_print_result(result, mml_result_type);
        printf("\n");
    }
    return 0;
}

/* ---- Algebraic effect handler stack ---- */

mml_handler* mml_current_handler = NULL;

mml_handler* mml_alloc_handler(int64_t num_ops) {
    size_t size = sizeof(mml_handler) + (size_t)num_ops * sizeof(mml_op_entry);
    mml_handler* h = (mml_handler*)mml_alloc((int64_t)size, 0);
    h->parent = NULL;
    h->num_ops = (int)num_ops;
    h->return_fn = 0;
    h->return_env = 0;
    h->try_result = MML_UNIT;
    h->has_try_arms = 0;
    return h;
}

void mml_push_handler(mml_handler* h) {
    h->parent = mml_current_handler;
    mml_current_handler = h;
}

void mml_pop_handler(void) {
    if (mml_current_handler) {
        mml_current_handler = mml_current_handler->parent;
    }
}

void mml_handler_set_op(mml_handler* h, int64_t index, const char* name,
                        int64_t kind, int64_t fn_ptr, int64_t env_ptr) {
    h->ops[(int)index].name = name;
    h->ops[(int)index].kind = (int)kind;
    h->ops[(int)index].fn_ptr = fn_ptr;
    h->ops[(int)index].env_ptr = env_ptr;
}

void mml_handler_set_return(mml_handler* h, int64_t fn_ptr, int64_t env_ptr) {
    h->return_fn = fn_ptr;
    h->return_env = env_ptr;
}

int64_t mml_handler_setjmp(mml_handler* h) {
    return (int64_t)setjmp(h->try_jmp);
}

int64_t mml_handler_get_try_result(mml_handler* h) {
    return h->try_result;
}

/* Run a handler body with setjmp protection for try handlers.
 * setjmp must be called directly in the function that will be active
 * when longjmp fires, hence we cannot use a wrapper.
 * body_fn: (env*) -> i64    — the body thunk
 * return_fn: (env*, i64) -> i64  — the return arm (0 = identity)
 * All pointer params are passed as i64. */
int64_t mml_run_try_handler(int64_t handler_i64,
                            int64_t body_fn_i64, int64_t body_env_i64,
                            int64_t return_fn_i64, int64_t return_env_i64) {
    typedef int64_t (*body_fn_t)(int64_t*);
    typedef int64_t (*ret_fn_t)(int64_t*, int64_t);
    mml_handler* h = (mml_handler*)(intptr_t)handler_i64;
    body_fn_t body_fn = (body_fn_t)(intptr_t)body_fn_i64;
    int64_t* body_env = (int64_t*)(intptr_t)body_env_i64;
    ret_fn_t return_fn = return_fn_i64 ? (ret_fn_t)(intptr_t)return_fn_i64 : NULL;
    int64_t* return_env = (int64_t*)(intptr_t)return_env_i64;

    if (setjmp(h->try_jmp) == 0) {
        /* Normal path: run body */
        int64_t body_result = body_fn(body_env);
        mml_pop_handler();
        if (return_fn) {
            return return_fn(return_env, body_result);
        }
        return body_result;
    } else {
        /* Caught path: a try arm fired longjmp */
        return h->try_result;
    }
}

int64_t mml_perform_op(const char* op_name, int64_t arg) {
    typedef int64_t (*arm_fn_t)(int64_t*, int64_t, int64_t);
    /* Search handler stack from innermost to outermost */
    mml_handler* h = mml_current_handler;
    while (h) {
        for (int i = 0; i < h->num_ops; i++) {
            if (strcmp(h->ops[i].name, op_name) == 0) {
                mml_op_entry* entry = &h->ops[i];
                int64_t* env = (int64_t*)(intptr_t)entry->env_ptr;
                arm_fn_t fn = (arm_fn_t)(intptr_t)entry->fn_ptr;
                switch (entry->kind) {
                case MML_HANDLER_PROVIDE:
                    /* Tail-resumptive: call fn(env, arg, 0), return result directly */
                    return fn(env, arg, 0);
                case MML_HANDLER_TRY: {
                    /* Never-resume: call fn(env, arg, 0), longjmp back to handler */
                    int64_t result = fn(env, arg, 0);
                    /* Pop all handlers down to and including this one */
                    mml_current_handler = h->parent;
                    h->try_result = result;
                    longjmp(h->try_jmp, 1);
                    /* unreachable */
                }
                case MML_HANDLER_FULL:
                    /* Yield to the handler: suspend this fiber */
                    if (!mml_current_fiber) {
                        fprintf(stderr, "native: full handler perform outside fiber\n");
                        exit(1);
                    }
                    mml_current_fiber->op_name = op_name;
                    mml_current_fiber->op_arg = arg;
                    mml_current_fiber->state = MML_FIBER_YIELDED;
                    /* Save which handler matched (stash as result temporarily) */
                    mml_current_fiber->result = (int64_t)(intptr_t)h;
                    mml_swap_context(&mml_current_fiber->ctx, mml_current_fiber->parent_ctx);
                    /* When resumed, the resumed value is in fiber->result */
                    return mml_current_fiber->result;
                }
            }
        }
        h = h->parent;
    }
    fprintf(stderr, "Unhandled effect: %s\n", op_name);
    exit(1);
    return MML_UNIT; /* unreachable */
}

/* ---- Fiber infrastructure ---- */

/* Guarded stack allocation: [guard page | usable stack]
 * Returns pointer to the usable region (one page above the base).
 * The guard page at the bottom catches stack overflow with SIGSEGV. */
static size_t page_size_cached = 0;

static void *alloc_guarded_stack(size_t usable_size) {
    if (!page_size_cached) page_size_cached = (size_t)sysconf(_SC_PAGESIZE);
    size_t total = page_size_cached + usable_size;
    void *base = mmap(NULL, total, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base == MAP_FAILED) return NULL;
    /* Guard page at the low end (stack grows down toward it) */
    mprotect(base, page_size_cached, PROT_NONE);
    return (char*)base + page_size_cached;
}

static void free_guarded_stack(void *usable_ptr, size_t usable_size) {
    if (!page_size_cached) page_size_cached = (size_t)sysconf(_SC_PAGESIZE);
    void *base = (char*)usable_ptr - page_size_cached;
    munmap(base, page_size_cached + usable_size);
}

mml_fiber *mml_current_fiber = NULL;

/* Fiber entry point trampoline — called via mml_make_context */
static void fiber_entry_trampoline(void *arg) {
    mml_fiber *f = (mml_fiber *)arg;
    typedef int64_t (*body_fn_t)(int64_t*);
    /* The body fn pointer and env are stashed in op_arg and result temporarily */
    body_fn_t body_fn = (body_fn_t)(intptr_t)f->op_arg;
    int64_t *body_env = (int64_t*)(intptr_t)f->result;
    f->op_arg = 0;
    int64_t result = body_fn(body_env);
    /* Use mml_current_fiber instead of f for completion, because for copied
     * fibers f still points to the original (baked into context by make_context).
     * mml_current_fiber is always set correctly before swap_context to us. */
    mml_fiber *self = mml_current_fiber;
    self->result = result;
    self->state = MML_FIBER_COMPLETED;
    /* Return to parent */
    mml_swap_context(&self->ctx, self->parent_ctx);
}

static mml_fiber *create_fiber(int64_t body_fn_i64, int64_t body_env_i64) {
    mml_fiber *f = (mml_fiber *)GC_malloc(sizeof(mml_fiber));
    if (!f) { fprintf(stderr, "OOM: fiber\n"); exit(1); }
    f->stack = alloc_guarded_stack(MML_FIBER_STACK_SIZE);
    if (!f->stack) { fprintf(stderr, "OOM: fiber stack\n"); exit(1); }
    f->state = MML_FIBER_RUNNING;
    f->result = body_env_i64;  /* stash env temporarily */
    f->op_arg = body_fn_i64;   /* stash fn temporarily */
    f->op_name = NULL;
    f->parent_ctx = NULL;

    mml_make_context(&f->ctx, f->stack, MML_FIBER_STACK_SIZE,
                     fiber_entry_trampoline, f);
    return f;
}

/* Shared dispatch loop for fiber effect handling.
 * Dispatches yielded fiber ops to handler arms until the fiber completes
 * or a TRY/FULL arm returns early. Returns the final result. */
static int64_t fiber_dispatch_loop(mml_fiber *fiber, mml_context *caller_ctx,
                                   mml_fiber *saved_fiber,
                                   int64_t return_fn_i64, int64_t return_env_i64) {
    typedef int64_t (*arm_fn_t)(int64_t*, int64_t, int64_t);
    typedef int64_t (*ret_fn_t)(int64_t*, int64_t);

    while (fiber->state == MML_FIBER_YIELDED) {
        const char *op_name = fiber->op_name;
        int64_t op_arg = fiber->op_arg;
        mml_handler *matched_h = (mml_handler*)(intptr_t)fiber->result;

        /* Find the matching op entry */
        mml_op_entry *entry = NULL;
        for (int i = 0; i < matched_h->num_ops; i++) {
            if (strcmp(matched_h->ops[i].name, op_name) == 0) {
                entry = &matched_h->ops[i];
                break;
            }
        }
        if (!entry) {
            fprintf(stderr, "fiber dispatch: op '%s' not found\n", op_name);
            exit(1);
        }

        switch (entry->kind) {
        case MML_HANDLER_PROVIDE: {
            arm_fn_t fn = (arm_fn_t)(intptr_t)entry->fn_ptr;
            int64_t *env = (int64_t*)(intptr_t)entry->env_ptr;
            int64_t result = fn(env, op_arg, 0);
            fiber->result = result;
            fiber->state = MML_FIBER_RUNNING;
            mml_current_fiber = fiber;
            mml_swap_context(caller_ctx, &fiber->ctx);
            break;
        }
        case MML_HANDLER_TRY: {
            arm_fn_t fn = (arm_fn_t)(intptr_t)entry->fn_ptr;
            int64_t *env = (int64_t*)(intptr_t)entry->env_ptr;
            int64_t result = fn(env, op_arg, 0);
            mml_current_handler = matched_h->parent;
            mml_current_fiber = saved_fiber;
            free_guarded_stack(fiber->stack, MML_FIBER_STACK_SIZE);
            return result;
        }
        case MML_HANDLER_FULL: {
            mml_handler *saved_handler = mml_current_handler;
            mml_current_handler = matched_h->parent;
            mml_current_fiber = saved_fiber;

            mml_continuation *k = (mml_continuation *)GC_malloc(sizeof(mml_continuation));
            if (!k) { fprintf(stderr, "OOM: continuation\n"); exit(1); }
            k->fiber = fiber;
            k->handler = saved_handler;
            k->return_fn = return_fn_i64;
            k->return_env = return_env_i64;
            k->used = 0;

            arm_fn_t fn = (arm_fn_t)(intptr_t)entry->fn_ptr;
            int64_t *env = (int64_t*)(intptr_t)entry->env_ptr;
            int64_t arm_result = fn(env, op_arg, (int64_t)(intptr_t)k);

            if (!k->used) {
                free_guarded_stack(fiber->stack, MML_FIBER_STACK_SIZE);
            }
            return arm_result;
        }
        }
    }

    /* Fiber completed */
    mml_current_fiber = saved_fiber;
    int64_t body_result = fiber->result;
    free_guarded_stack(fiber->stack, MML_FIBER_STACK_SIZE);
    mml_pop_handler();

    ret_fn_t return_fn = return_fn_i64 ? (ret_fn_t)(intptr_t)return_fn_i64 : NULL;
    int64_t *return_env = (int64_t*)(intptr_t)return_env_i64;
    if (return_fn) {
        return return_fn(return_env, body_result);
    }
    return body_result;
}

int64_t mml_run_full_handler(int64_t handler_i64,
                             int64_t body_fn_i64, int64_t body_env_i64,
                             int64_t return_fn_i64, int64_t return_env_i64) {
    mml_handler *h = (mml_handler*)(intptr_t)handler_i64;
    (void)h; /* handler already installed by codegen */

    /* Create fiber for body */
    mml_fiber *fiber = create_fiber(body_fn_i64, body_env_i64);
    mml_context caller_ctx;
    fiber->parent_ctx = &caller_ctx;

    mml_fiber *saved_fiber = mml_current_fiber;
    mml_current_fiber = fiber;

    /* Start the fiber */
    mml_swap_context(&caller_ctx, &fiber->ctx);

    return fiber_dispatch_loop(fiber, &caller_ctx, saved_fiber,
                               return_fn_i64, return_env_i64);
}

int64_t mml_resume_continuation(int64_t k_i64, int64_t val) {
    mml_continuation *k = (mml_continuation*)(intptr_t)k_i64;
    if (k->used) {
        fprintf(stderr, "native: continuation already resumed (one-shot violation)\n");
        exit(1);
    }
    k->used = 1;

    mml_fiber *fiber = k->fiber;

    /* Reinstall the handler */
    mml_current_handler = k->handler;

    /* Set the resumed value */
    fiber->result = val;
    fiber->state = MML_FIBER_RUNNING;

    mml_context caller_ctx;
    fiber->parent_ctx = &caller_ctx;

    mml_fiber *saved_fiber = mml_current_fiber;
    mml_current_fiber = fiber;

    /* Resume the fiber */
    mml_swap_context(&caller_ctx, &fiber->ctx);

    return fiber_dispatch_loop(fiber, &caller_ctx, saved_fiber,
                               k->return_fn, k->return_env);
}

int64_t mml_copy_continuation(int64_t k_i64) {
    mml_continuation *orig = (mml_continuation*)(intptr_t)k_i64;
    mml_fiber *orig_fiber = orig->fiber;

    /* Deep copy the fiber struct */
    mml_fiber *new_fiber = (mml_fiber *)GC_malloc(sizeof(mml_fiber));
    if (!new_fiber) { fprintf(stderr, "OOM: fiber copy\n"); exit(1); }
    memcpy(new_fiber, orig_fiber, sizeof(mml_fiber));

    /* Copy the stack */
    new_fiber->stack = alloc_guarded_stack(MML_FIBER_STACK_SIZE);
    if (!new_fiber->stack) { fprintf(stderr, "OOM: fiber stack copy\n"); exit(1); }
    memcpy(new_fiber->stack, orig_fiber->stack, MML_FIBER_STACK_SIZE);

    /* Compute stack relocation offset */
    ptrdiff_t stack_offset = (char*)new_fiber->stack - (char*)orig_fiber->stack;
    char *old_lo = (char*)orig_fiber->stack;
    char *old_hi = old_lo + MML_FIBER_STACK_SIZE;

    /* Relocate SP — portable via MML_CTX_SP_OFFSET */
    uint64_t *sp_ptr = (uint64_t*)((char*)&new_fiber->ctx + MML_CTX_SP_OFFSET);
    *sp_ptr += stack_offset;

    /* Relocate FP/BP and walk the frame pointer chain */
    uint64_t *bp_ptr = (uint64_t*)((char*)&new_fiber->ctx + MML_CTX_BP_OFFSET);
    uint64_t bp = *bp_ptr;
    if ((char*)(uintptr_t)bp >= old_lo && (char*)(uintptr_t)bp < old_hi) {
        bp += stack_offset;
        *bp_ptr = bp;
    }
    /* Walk frame pointer chain in the copied stack */
    {
        uint64_t *fp = (uint64_t*)(uintptr_t)bp;
        for (int depth = 0; depth < 1024 && fp; depth++) {
            uint64_t saved_fp = fp[0];
            if (saved_fp == 0) break;
            if ((char*)(uintptr_t)saved_fp >= old_lo &&
                (char*)(uintptr_t)saved_fp < old_hi) {
                fp[0] = saved_fp + stack_offset;
                fp = (uint64_t*)(uintptr_t)fp[0];
            } else {
                break;
            }
        }
    }

    /* Create new continuation */
    mml_continuation *new_k = (mml_continuation *)GC_malloc(sizeof(mml_continuation));
    if (!new_k) { fprintf(stderr, "OOM: continuation copy\n"); exit(1); }
    new_k->fiber = new_fiber;
    new_k->handler = orig->handler;
    new_k->return_fn = orig->return_fn;
    new_k->return_env = orig->return_env;
    new_k->used = 0;

    return (int64_t)(intptr_t)new_k;
}

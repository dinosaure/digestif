/********************************************************************\
 *
 *      FILE:     rmd160.c
 *
 *      CONTENTS: A sample C-implementation of the RIPEMD-160
 *                hash-function.
 *      TARGET:   any computer with an ANSI C compiler
 *
 *      AUTHOR:   Antoon Bosselaers, ESAT-COSIC
 *      DATE:     1 March 1996
 *      VERSION:  1.0
 *
 *      Copyright (c) Katholieke Universiteit Leuven
 *      1996, All Rights Reserved
 *
\********************************************************************/

/* XXX(dinosaure): most inspired from the Antoon Bosselaers's
   implementation but modified in the same way for digestif. */

#include <string.h>
#include "ripemd160.h"
#include "bitfn.h"

void digestif_rmd160_init(struct rmd160_ctx *ctx)
{
  memset(ctx, 0, sizeof(*ctx));

  ctx->h[0] = 0x67452301UL;
  ctx->h[1] = 0xefcdab89UL;
  ctx->h[2] = 0x98badcfeUL;
  ctx->h[3] = 0x10325476UL;
  ctx->h[4] = 0xc3d2e1f0UL;

  ctx->sz[0] = 0;
  ctx->sz[1] = 0;

  ctx->n = 0;
}

/* the five basic functions F(), G() and H() */
#define F(x, y, z)        ((x) ^ (y) ^ (z))
#define G(x, y, z)        (((x) & (y)) | (~(x) & (z)))
#define H(x, y, z)        (((x) | ~(y)) ^ (z))
#define I(x, y, z)        (((x) & (z)) | ((y) & ~(z)))
#define J(x, y, z)        ((x) ^ ((y) | ~(z)))

/* the ten basic operations FF() through III() */
#define FF(a, b, c, d, e, x, s)        {\
      (a) += F((b), (c), (d)) + (x);\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }
#define GG(a, b, c, d, e, x, s)        {\
      (a) += G((b), (c), (d)) + (x) + 0x5a827999UL;\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }
#define HH(a, b, c, d, e, x, s)        {\
      (a) += H((b), (c), (d)) + (x) + 0x6ed9eba1UL;\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }
#define II(a, b, c, d, e, x, s)        {\
      (a) += I((b), (c), (d)) + (x) + 0x8f1bbcdcUL;\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }
#define JJ(a, b, c, d, e, x, s)        {\
      (a) += J((b), (c), (d)) + (x) + 0xa953fd4eUL;\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }
#define FFF(a, b, c, d, e, x, s)        {\
      (a) += F((b), (c), (d)) + (x);\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }
#define GGG(a, b, c, d, e, x, s)        {\
      (a) += G((b), (c), (d)) + (x) + 0x7a6d76e9UL;\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }
#define HHH(a, b, c, d, e, x, s)        {\
      (a) += H((b), (c), (d)) + (x) + 0x6d703ef3UL;\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }
#define III(a, b, c, d, e, x, s)        {\
      (a) += I((b), (c), (d)) + (x) + 0x5c4dd124UL;\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }
#define JJJ(a, b, c, d, e, x, s)        {\
      (a) += J((b), (c), (d)) + (x) + 0x50a28be6UL;\
      (a) = rol32((a), (s)) + (e);\
      (c) = rol32((c), 10);\
   }

void rmd160_compress(struct rmd160_ctx *ctx, uint32_t *buf)
{
   uint32_t aa = ctx->h[0],
            bb = ctx->h[1],
            cc = ctx->h[2],
            dd = ctx->h[3],
            ee = ctx->h[4];
   uint32_t aaa = ctx->h[0],
            bbb = ctx->h[1],
            ccc = ctx->h[2],
            ddd = ctx->h[3],
            eee = ctx->h[4];

   /* round 1 */
   FF(aa, bb, cc, dd, ee, buf[ 0], 11);
   FF(ee, aa, bb, cc, dd, buf[ 1], 14);
   FF(dd, ee, aa, bb, cc, buf[ 2], 15);
   FF(cc, dd, ee, aa, bb, buf[ 3], 12);
   FF(bb, cc, dd, ee, aa, buf[ 4],  5);
   FF(aa, bb, cc, dd, ee, buf[ 5],  8);
   FF(ee, aa, bb, cc, dd, buf[ 6],  7);
   FF(dd, ee, aa, bb, cc, buf[ 7],  9);
   FF(cc, dd, ee, aa, bb, buf[ 8], 11);
   FF(bb, cc, dd, ee, aa, buf[ 9], 13);
   FF(aa, bb, cc, dd, ee, buf[10], 14);
   FF(ee, aa, bb, cc, dd, buf[11], 15);
   FF(dd, ee, aa, bb, cc, buf[12],  6);
   FF(cc, dd, ee, aa, bb, buf[13],  7);
   FF(bb, cc, dd, ee, aa, buf[14],  9);
   FF(aa, bb, cc, dd, ee, buf[15],  8);

   /* round 2 */
   GG(ee, aa, bb, cc, dd, buf[ 7],  7);
   GG(dd, ee, aa, bb, cc, buf[ 4],  6);
   GG(cc, dd, ee, aa, bb, buf[13],  8);
   GG(bb, cc, dd, ee, aa, buf[ 1], 13);
   GG(aa, bb, cc, dd, ee, buf[10], 11);
   GG(ee, aa, bb, cc, dd, buf[ 6],  9);
   GG(dd, ee, aa, bb, cc, buf[15],  7);
   GG(cc, dd, ee, aa, bb, buf[ 3], 15);
   GG(bb, cc, dd, ee, aa, buf[12],  7);
   GG(aa, bb, cc, dd, ee, buf[ 0], 12);
   GG(ee, aa, bb, cc, dd, buf[ 9], 15);
   GG(dd, ee, aa, bb, cc, buf[ 5],  9);
   GG(cc, dd, ee, aa, bb, buf[ 2], 11);
   GG(bb, cc, dd, ee, aa, buf[14],  7);
   GG(aa, bb, cc, dd, ee, buf[11], 13);
   GG(ee, aa, bb, cc, dd, buf[ 8], 12);

   /* round 3 */
   HH(dd, ee, aa, bb, cc, buf[ 3], 11);
   HH(cc, dd, ee, aa, bb, buf[10], 13);
   HH(bb, cc, dd, ee, aa, buf[14],  6);
   HH(aa, bb, cc, dd, ee, buf[ 4],  7);
   HH(ee, aa, bb, cc, dd, buf[ 9], 14);
   HH(dd, ee, aa, bb, cc, buf[15],  9);
   HH(cc, dd, ee, aa, bb, buf[ 8], 13);
   HH(bb, cc, dd, ee, aa, buf[ 1], 15);
   HH(aa, bb, cc, dd, ee, buf[ 2], 14);
   HH(ee, aa, bb, cc, dd, buf[ 7],  8);
   HH(dd, ee, aa, bb, cc, buf[ 0], 13);
   HH(cc, dd, ee, aa, bb, buf[ 6],  6);
   HH(bb, cc, dd, ee, aa, buf[13],  5);
   HH(aa, bb, cc, dd, ee, buf[11], 12);
   HH(ee, aa, bb, cc, dd, buf[ 5],  7);
   HH(dd, ee, aa, bb, cc, buf[12],  5);

   /* round 4 */
   II(cc, dd, ee, aa, bb, buf[ 1], 11);
   II(bb, cc, dd, ee, aa, buf[ 9], 12);
   II(aa, bb, cc, dd, ee, buf[11], 14);
   II(ee, aa, bb, cc, dd, buf[10], 15);
   II(dd, ee, aa, bb, cc, buf[ 0], 14);
   II(cc, dd, ee, aa, bb, buf[ 8], 15);
   II(bb, cc, dd, ee, aa, buf[12],  9);
   II(aa, bb, cc, dd, ee, buf[ 4],  8);
   II(ee, aa, bb, cc, dd, buf[13],  9);
   II(dd, ee, aa, bb, cc, buf[ 3], 14);
   II(cc, dd, ee, aa, bb, buf[ 7],  5);
   II(bb, cc, dd, ee, aa, buf[15],  6);
   II(aa, bb, cc, dd, ee, buf[14],  8);
   II(ee, aa, bb, cc, dd, buf[ 5],  6);
   II(dd, ee, aa, bb, cc, buf[ 6],  5);
   II(cc, dd, ee, aa, bb, buf[ 2], 12);

   /* round 5 */
   JJ(bb, cc, dd, ee, aa, buf[ 4],  9);
   JJ(aa, bb, cc, dd, ee, buf[ 0], 15);
   JJ(ee, aa, bb, cc, dd, buf[ 5],  5);
   JJ(dd, ee, aa, bb, cc, buf[ 9], 11);
   JJ(cc, dd, ee, aa, bb, buf[ 7],  6);
   JJ(bb, cc, dd, ee, aa, buf[12],  8);
   JJ(aa, bb, cc, dd, ee, buf[ 2], 13);
   JJ(ee, aa, bb, cc, dd, buf[10], 12);
   JJ(dd, ee, aa, bb, cc, buf[14],  5);
   JJ(cc, dd, ee, aa, bb, buf[ 1], 12);
   JJ(bb, cc, dd, ee, aa, buf[ 3], 13);
   JJ(aa, bb, cc, dd, ee, buf[ 8], 14);
   JJ(ee, aa, bb, cc, dd, buf[11], 11);
   JJ(dd, ee, aa, bb, cc, buf[ 6],  8);
   JJ(cc, dd, ee, aa, bb, buf[15],  5);
   JJ(bb, cc, dd, ee, aa, buf[13],  6);

   /* parallel round 1 */
   JJJ(aaa, bbb, ccc, ddd, eee, buf[ 5],  8);
   JJJ(eee, aaa, bbb, ccc, ddd, buf[14],  9);
   JJJ(ddd, eee, aaa, bbb, ccc, buf[ 7],  9);
   JJJ(ccc, ddd, eee, aaa, bbb, buf[ 0], 11);
   JJJ(bbb, ccc, ddd, eee, aaa, buf[ 9], 13);
   JJJ(aaa, bbb, ccc, ddd, eee, buf[ 2], 15);
   JJJ(eee, aaa, bbb, ccc, ddd, buf[11], 15);
   JJJ(ddd, eee, aaa, bbb, ccc, buf[ 4],  5);
   JJJ(ccc, ddd, eee, aaa, bbb, buf[13],  7);
   JJJ(bbb, ccc, ddd, eee, aaa, buf[ 6],  7);
   JJJ(aaa, bbb, ccc, ddd, eee, buf[15],  8);
   JJJ(eee, aaa, bbb, ccc, ddd, buf[ 8], 11);
   JJJ(ddd, eee, aaa, bbb, ccc, buf[ 1], 14);
   JJJ(ccc, ddd, eee, aaa, bbb, buf[10], 14);
   JJJ(bbb, ccc, ddd, eee, aaa, buf[ 3], 12);
   JJJ(aaa, bbb, ccc, ddd, eee, buf[12],  6);

   /* parallel round 2 */
   III(eee, aaa, bbb, ccc, ddd, buf[ 6],  9);
   III(ddd, eee, aaa, bbb, ccc, buf[11], 13);
   III(ccc, ddd, eee, aaa, bbb, buf[ 3], 15);
   III(bbb, ccc, ddd, eee, aaa, buf[ 7],  7);
   III(aaa, bbb, ccc, ddd, eee, buf[ 0], 12);
   III(eee, aaa, bbb, ccc, ddd, buf[13],  8);
   III(ddd, eee, aaa, bbb, ccc, buf[ 5],  9);
   III(ccc, ddd, eee, aaa, bbb, buf[10], 11);
   III(bbb, ccc, ddd, eee, aaa, buf[14],  7);
   III(aaa, bbb, ccc, ddd, eee, buf[15],  7);
   III(eee, aaa, bbb, ccc, ddd, buf[ 8], 12);
   III(ddd, eee, aaa, bbb, ccc, buf[12],  7);
   III(ccc, ddd, eee, aaa, bbb, buf[ 4],  6);
   III(bbb, ccc, ddd, eee, aaa, buf[ 9], 15);
   III(aaa, bbb, ccc, ddd, eee, buf[ 1], 13);
   III(eee, aaa, bbb, ccc, ddd, buf[ 2], 11);

   /* parallel round 3 */
   HHH(ddd, eee, aaa, bbb, ccc, buf[15],  9);
   HHH(ccc, ddd, eee, aaa, bbb, buf[ 5],  7);
   HHH(bbb, ccc, ddd, eee, aaa, buf[ 1], 15);
   HHH(aaa, bbb, ccc, ddd, eee, buf[ 3], 11);
   HHH(eee, aaa, bbb, ccc, ddd, buf[ 7],  8);
   HHH(ddd, eee, aaa, bbb, ccc, buf[14],  6);
   HHH(ccc, ddd, eee, aaa, bbb, buf[ 6],  6);
   HHH(bbb, ccc, ddd, eee, aaa, buf[ 9], 14);
   HHH(aaa, bbb, ccc, ddd, eee, buf[11], 12);
   HHH(eee, aaa, bbb, ccc, ddd, buf[ 8], 13);
   HHH(ddd, eee, aaa, bbb, ccc, buf[12],  5);
   HHH(ccc, ddd, eee, aaa, bbb, buf[ 2], 14);
   HHH(bbb, ccc, ddd, eee, aaa, buf[10], 13);
   HHH(aaa, bbb, ccc, ddd, eee, buf[ 0], 13);
   HHH(eee, aaa, bbb, ccc, ddd, buf[ 4],  7);
   HHH(ddd, eee, aaa, bbb, ccc, buf[13],  5);

   /* parallel round 4 */
   GGG(ccc, ddd, eee, aaa, bbb, buf[ 8], 15);
   GGG(bbb, ccc, ddd, eee, aaa, buf[ 6],  5);
   GGG(aaa, bbb, ccc, ddd, eee, buf[ 4],  8);
   GGG(eee, aaa, bbb, ccc, ddd, buf[ 1], 11);
   GGG(ddd, eee, aaa, bbb, ccc, buf[ 3], 14);
   GGG(ccc, ddd, eee, aaa, bbb, buf[11], 14);
   GGG(bbb, ccc, ddd, eee, aaa, buf[15],  6);
   GGG(aaa, bbb, ccc, ddd, eee, buf[ 0], 14);
   GGG(eee, aaa, bbb, ccc, ddd, buf[ 5],  6);
   GGG(ddd, eee, aaa, bbb, ccc, buf[12],  9);
   GGG(ccc, ddd, eee, aaa, bbb, buf[ 2], 12);
   GGG(bbb, ccc, ddd, eee, aaa, buf[13],  9);
   GGG(aaa, bbb, ccc, ddd, eee, buf[ 9], 12);
   GGG(eee, aaa, bbb, ccc, ddd, buf[ 7],  5);
   GGG(ddd, eee, aaa, bbb, ccc, buf[10], 15);
   GGG(ccc, ddd, eee, aaa, bbb, buf[14],  8);

   /* parallel round 5 */
   FFF(bbb, ccc, ddd, eee, aaa, buf[12] ,  8);
   FFF(aaa, bbb, ccc, ddd, eee, buf[15] ,  5);
   FFF(eee, aaa, bbb, ccc, ddd, buf[10] , 12);
   FFF(ddd, eee, aaa, bbb, ccc, buf[ 4] ,  9);
   FFF(ccc, ddd, eee, aaa, bbb, buf[ 1] , 12);
   FFF(bbb, ccc, ddd, eee, aaa, buf[ 5] ,  5);
   FFF(aaa, bbb, ccc, ddd, eee, buf[ 8] , 14);
   FFF(eee, aaa, bbb, ccc, ddd, buf[ 7] ,  6);
   FFF(ddd, eee, aaa, bbb, ccc, buf[ 6] ,  8);
   FFF(ccc, ddd, eee, aaa, bbb, buf[ 2] , 13);
   FFF(bbb, ccc, ddd, eee, aaa, buf[13] ,  6);
   FFF(aaa, bbb, ccc, ddd, eee, buf[14] ,  5);
   FFF(eee, aaa, bbb, ccc, ddd, buf[ 0] , 15);
   FFF(ddd, eee, aaa, bbb, ccc, buf[ 3] , 13);
   FFF(ccc, ddd, eee, aaa, bbb, buf[ 9] , 11);
   FFF(bbb, ccc, ddd, eee, aaa, buf[11] , 11);

   /* combine results */
   ddd += cc + ctx->h[1]; /* final result for MDbuf[0] */
   ctx->h[1] = ctx->h[2] + dd + eee;
   ctx->h[2] = ctx->h[3] + ee + aaa;
   ctx->h[3] = ctx->h[4] + aa + bbb;
   ctx->h[4] = ctx->h[0] + bb + ccc;
   ctx->h[0] = ddd;
}

void digestif_rmd160_update(struct rmd160_ctx *ctx, uint8_t *data, uint32_t len)
{
  uint32_t t;

  /* update length */
  t = ctx->sz[0];

  if ((ctx->sz[0] = t + (len << 3)) < t)
    ctx->sz[1]++; /* carry from low 32 bits to high 32 bits. */

  ctx->sz[1] += (len >> 29);

  /* if data was left in buffer, pad it with fresh data and munge/eat block. */
  if (ctx->n != 0)
    {
      t = 64 - ctx->n;

      if (len < t) /* not enough to munge. */
        {
          memcpy(ctx->buf + ctx->n, data, len);
          ctx->n += len;
          return;
        }

      memcpy(ctx->buf + ctx->n, data, t);
      rmd160_compress(ctx, (uint32_t *) ctx->buf);
      data += t;
      len -= t;
    }

  /* munge/eat data in 64 bytes chunks. */
  while (len >= 64)
    {
      /* memcpy(ctx->buf, data, 64); XXX(dinosaure): from X.L. but
         avoid to be fast. */
      rmd160_compress(ctx, (uint32_t *) data);
      data += 64;
      len -= 64;
    }

  /* save remaining data. */
  memcpy(ctx->buf, data, len);
  ctx->n = len;
}

void digestif_rmd160_finalize(struct rmd160_ctx *ctx, uint8_t *out)
{
  int i = ctx->n;

  ctx->buf[i++] = 0x80;

  if (i > 55)
    {
      memset(ctx->buf + i, 0, 64 - i);
      rmd160_compress(ctx, (uint32_t *) ctx->buf);
      i = 0;
    }

  memset(ctx->buf + i, 0, 56 - i);
  cpu_to_le32_array((uint32_t *) (ctx->buf + 56), ctx->sz, 2);
  rmd160_compress(ctx, (uint32_t *) ctx->buf);
 
cpu_to_le32_array((uint32_t *) out, ctx->h, 5);
}



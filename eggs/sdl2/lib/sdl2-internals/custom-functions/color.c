/*
 * chicken-sdl2: CHICKEN Scheme bindings to Simple DirectMedia Layer 2
 *
 * Copyright © 2013, 2015-2016  John Croisant.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */


/* This file contains custom C functions related to colors. */


/* Linear interpolation of Uint8s. Useful for color blending.
 * n1 and n2 are the values to interpolate between.
 * t is the interpolation factor, ranging from 0 to 255.
 * t=0 means n1, t=255 means n2, t=127 means halfway between, etc.
*/
Uint8 chickenSDL2_Uint8Lerp(Uint8 n1, Uint8 n2, Uint8 t)
{
  return n1 + (n2 - n1) * t / 255;
}


/* Multiply components of c by integer scale, storing results in dest.
 * dest might be the same object as c.
 * The results are clamped to the range [0, 255] (no wrapping).
 */
void chickenSDL2_ColorScaleInt(SDL_Color* c, Uint8 scale, SDL_Color* dest)
{
  Uint16 r = scale * c->r;
  Uint16 g = scale * c->g;
  Uint16 b = scale * c->b;
  Uint16 a = scale * c->a;

  dest->r = SDL_min(r, 255);
  dest->g = SDL_min(g, 255);
  dest->b = SDL_min(b, 255);
  dest->a = SDL_min(a, 255);
}


/* Multiply components of c by float scale, storing results in dest.
 * dest might be the same object as c.
 * The results are clamped to the range [0, 255] (no wrapping).
 */
void chickenSDL2_ColorScaleFloat(SDL_Color* c, float scale, SDL_Color* dest)
{
  /* Scale outside of range [0, 255] wouldn't change the results, but
     it could cause an overflow or underflow, so clamp it. */
  if (0.0 > scale) {
    scale = 0.0;
  } else if (255.0 < scale) {
    scale = 255.0;
  }

  Uint16 r = scale * c->r;
  Uint16 g = scale * c->g;
  Uint16 b = scale * c->b;
  Uint16 a = scale * c->a;

  dest->r = SDL_min(r, 255);
  dest->g = SDL_min(g, 255);
  dest->b = SDL_min(b, 255);
  dest->a = SDL_min(a, 255);
}


/* Color multiplication of c1 and c2, storing results in dest.
 * The alpha of c2 affects the weight of the multiplication.
 * dest will have the same alpha as c1.
 * dest might be the same object as c1 or c2.
 */
void chickenSDL2_ColorMult(SDL_Color* c1, SDL_Color* c2, SDL_Color* dest)
{
  if (0 == c2->a) {
    dest->r = c1->r;
    dest->g = c1->g;
    dest->b = c1->b;
  } else if (255 == c2->a) {
    dest->r = (c1->r * c2->r) / 255;
    dest->g = (c1->g * c2->g) / 255;
    dest->b = (c1->b * c2->b) / 255;
  } else {
    Uint8 r = (c1->r * c2->r) / 255;
    Uint8 g = (c1->g * c2->g) / 255;
    Uint8 b = (c1->b * c2->b) / 255;
    dest->r = chickenSDL2_Uint8Lerp(c1->r, r, c2->a);
    dest->g = chickenSDL2_Uint8Lerp(c1->g, g, c2->a);
    dest->b = chickenSDL2_Uint8Lerp(c1->b, b, c2->a);
  }
  dest->a = c1->a;
}


/* Color addition of c1 and c2, storing results in dest.
 * The alpha of c2 affects the weight of the addition.
 * dest will have the same alpha as c1.
 * dest might be the same object as c1 or c2.
 * The results are clamped to the range [0, 255].
 */
void chickenSDL2_ColorAdd(SDL_Color* c1, SDL_Color* c2, SDL_Color* dest)
{
  if (0 == c2->a) {
    dest->r = c1->r;
    dest->g = c1->g;
    dest->b = c1->b;
  } else if (255 == c2->a) {
    Uint16 r = c1->r + c2->r;
    Uint16 g = c1->g + c2->g;
    Uint16 b = c1->b + c2->b;
    dest->r = SDL_min(r, 255);
    dest->g = SDL_min(g, 255);
    dest->b = SDL_min(b, 255);
  } else {
    Uint16 r = c1->r + (c2->r * c2->a / 255);
    Uint16 g = c1->g + (c2->g * c2->a / 255);
    Uint16 b = c1->b + (c2->b * c2->a / 255);
    dest->r = SDL_min(r, 255);
    dest->g = SDL_min(g, 255);
    dest->b = SDL_min(b, 255);
  }
  dest->a = c1->a;
}


/* Color subtraction of c1 and c2, storing results in dest.
 * The alpha of c2 affects the weight of the subtraction.
 * dest will have the same alpha as c1.
 * dest might be the same object as c1 or c2.
 * The results are clamped to the range [0, 255].
 */
void chickenSDL2_ColorSub(SDL_Color* c1, SDL_Color* c2, SDL_Color* dest)
{
  if (0 == c2->a) {
    dest->r = c1->r;
    dest->g = c1->g;
    dest->b = c1->b;
  } else if (255 == c2->a) {
    Sint16 r = c1->r - c2->r;
    Sint16 g = c1->g - c2->g;
    Sint16 b = c1->b - c2->b;
    dest->r = SDL_max(0, r);
    dest->g = SDL_max(0, g);
    dest->b = SDL_max(0, b);
  } else {
    Sint16 r = c1->r - (c2->r * c2->a / 255);
    Sint16 g = c1->g - (c2->g * c2->a / 255);
    Sint16 b = c1->b - (c2->b * c2->a / 255);
    dest->r = SDL_max(0, r);
    dest->g = SDL_max(0, g);
    dest->b = SDL_max(0, b);
  }
  dest->a = c1->a;
}


void chickenSDL2_ColorLerp(SDL_Color* c1, SDL_Color* c2, double t, SDL_Color* dest)
{
  Sint32 r = chickenSDL2_Lerp_iid(c1->r, c2->r, t);
  Sint32 g = chickenSDL2_Lerp_iid(c1->g, c2->g, t);
  Sint32 b = chickenSDL2_Lerp_iid(c1->b, c2->b, t);
  Sint32 a = chickenSDL2_Lerp_iid(c1->a, c2->a, t);
  dest->r = CLAMP(r, 0, 255);
  dest->g = CLAMP(g, 0, 255);
  dest->b = CLAMP(b, 0, 255);
  dest->a = CLAMP(a, 0, 255);
}

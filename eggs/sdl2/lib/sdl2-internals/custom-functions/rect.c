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


void chickenSDL2_RectScale_i(SDL_Rect* r, Sint32 scale, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Mul_ii(r->x, scale);
  dest->y = chickenSDL2_Mul_ii(r->y, scale);
  dest->w = chickenSDL2_Mul_ii(r->w, scale);
  dest->h = chickenSDL2_Mul_ii(r->h, scale);
}


void chickenSDL2_RectScale_d(SDL_Rect* r, double scale, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Mul_id(r->x, scale);
  dest->y = chickenSDL2_Mul_id(r->y, scale);
  dest->w = chickenSDL2_Mul_id(r->w, scale);
  dest->h = chickenSDL2_Mul_id(r->h, scale);
}


void chickenSDL2_RectUnscale_i(SDL_Rect* r, Sint32 scale, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Div_ii(r->x, scale);
  dest->y = chickenSDL2_Div_ii(r->y, scale);
  dest->w = chickenSDL2_Div_ii(r->w, scale);
  dest->h = chickenSDL2_Div_ii(r->h, scale);
}


void chickenSDL2_RectUnscale_d(SDL_Rect* r, double scale, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Div_id(r->x, scale);
  dest->y = chickenSDL2_Div_id(r->y, scale);
  dest->w = chickenSDL2_Div_id(r->w, scale);
  dest->h = chickenSDL2_Div_id(r->h, scale);
}


void chickenSDL2_RectMove(SDL_Rect* r, Sint32 dx, Sint32 dy, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Add_ii(r->x, dx);
  dest->y = chickenSDL2_Add_ii(r->y, dy);
  dest->w = r->w;
  dest->h = r->h;
}


void chickenSDL2_RectAddPoint(SDL_Rect* r, SDL_Point* p, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Add_ii(r->x, p->x);
  dest->y = chickenSDL2_Add_ii(r->y, p->y);
  dest->w = r->w;
  dest->h = r->h;
}


void chickenSDL2_RectSubPoint(SDL_Rect* r, SDL_Point* p, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Sub_ii(r->x, p->x);
  dest->y = chickenSDL2_Sub_ii(r->y, p->y);
  dest->w = r->w;
  dest->h = r->h;
}


void chickenSDL2_RectGrow(SDL_Rect* r, Sint32 dw, Sint32 dh, SDL_Rect* dest)
{
  dest->x = r->x;
  dest->y = r->y;
  dest->w = chickenSDL2_Add_ii(r->w, dw);
  dest->h = chickenSDL2_Add_ii(r->h, dh);
}


/* Grow the rect outwards from its center point. */
void chickenSDL2_RectGrowCenter(SDL_Rect* r, Sint32 dw, Sint32 dh, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Sub_ii(r->x, dw / 2);
  dest->y = chickenSDL2_Sub_ii(r->y, dh / 2);
  dest->w = chickenSDL2_Add_ii(r->w, dw);
  dest->h = chickenSDL2_Add_ii(r->h, dh);
}


void chickenSDL2_RectLerp(SDL_Rect* r1, SDL_Rect* r2, double t, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Lerp_iid(r1->x, r2->x, t);
  dest->y = chickenSDL2_Lerp_iid(r1->y, r2->y, t);
  dest->w = chickenSDL2_Lerp_iid(r1->w, r2->w, t);
  dest->h = chickenSDL2_Lerp_iid(r1->h, r2->h, t);
}


void chickenSDL2_RectLerpXY(SDL_Rect* r1, SDL_Rect* r2, double t, SDL_Rect* dest)
{
  dest->x = chickenSDL2_Lerp_iid(r1->x, r2->x, t);
  dest->y = chickenSDL2_Lerp_iid(r1->y, r2->y, t);
  dest->w = r1->w;
  dest->h = r1->h;
}

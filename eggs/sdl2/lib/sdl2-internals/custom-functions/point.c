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


void chickenSDL2_PointScale_i(SDL_Point* p, Sint32 scale, SDL_Point* dest)
{
  dest->x = chickenSDL2_Mul_ii(p->x, scale);
  dest->y = chickenSDL2_Mul_ii(p->y, scale);
}


void chickenSDL2_PointScale_d(SDL_Point* p, double scale, SDL_Point* dest)
{
  dest->x = chickenSDL2_Mul_id(p->x, scale);
  dest->y = chickenSDL2_Mul_id(p->y, scale);
}


void chickenSDL2_PointUnscale_i(SDL_Point* p, Sint32 scale, SDL_Point* dest)
{
  dest->x = chickenSDL2_Div_ii(p->x, scale);
  dest->y = chickenSDL2_Div_ii(p->y, scale);
}


void chickenSDL2_PointUnscale_d(SDL_Point* p, double scale, SDL_Point* dest)
{
  dest->x = chickenSDL2_Div_id(p->x, scale);
  dest->y = chickenSDL2_Div_id(p->y, scale);
}


void chickenSDL2_PointMove(SDL_Point* p, Sint32 dx, Sint32 dy, SDL_Point* dest)
{
  dest->x = chickenSDL2_Add_ii(p->x, dx);
  dest->y = chickenSDL2_Add_ii(p->y, dy);
}


void chickenSDL2_PointAdd(SDL_Point* p1, SDL_Point* p2, SDL_Point* dest)
{
  dest->x = chickenSDL2_Add_ii(p1->x, p2->x);
  dest->y = chickenSDL2_Add_ii(p1->y, p2->y);
}


void chickenSDL2_PointSub(SDL_Point* p1, SDL_Point* p2, SDL_Point* dest)
{
  dest->x = chickenSDL2_Sub_ii(p1->x, p2->x);
  dest->y = chickenSDL2_Sub_ii(p1->y, p2->y);
}


void chickenSDL2_PointLerp(SDL_Point* p1, SDL_Point* p2, double t, SDL_Point* dest)
{
  dest->x = chickenSDL2_Lerp_iid(p1->x, p2->x, t);
  dest->y = chickenSDL2_Lerp_iid(p1->y, p2->y, t);
}

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


/* This file contains custom C functions related to surfaces. */


/* Returns a Uint32 value for the pixel at the given x and y. Assumes
 * that x and y are in bounds and that the surface is already locked.
 */
Uint32 chickenSDL2_SurfaceGetPixel( SDL_Surface* surface, int x, int y )
{
  int bpp = surface->format->BytesPerPixel;
  Uint8* p = (Uint8*)surface->pixels + (y * surface->pitch) + (x * bpp);
  switch(bpp) {
  case 1:
    C_return( *p );
    break;
  case 2:
    C_return( *(Uint16*)p );
    break;
  case 3:
    if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
      C_return( p[0] << 16 | p[1] << 8 | p[2] );
    } else {
      C_return( p[0] | p[1] << 8 | p[2] << 16 );
    }
    break;
  case 4:
    C_return( *(Uint32*)p );
    break;
  default:
    C_return( 0 );
  }
}



/* Sets the pixel at the given x and y to a Uint32 value. Assumes
 * that x and y are in bounds and that the surface is already locked.
 */
void chickenSDL2_SurfaceSetPixel( SDL_Surface* surface, int x, int y, Uint32 pixel )
{
  int bpp = surface->format->BytesPerPixel;
  Uint8* p = (Uint8*)surface->pixels + (y * surface->pitch) + (x * bpp);
  switch(bpp) {
  case 1:
    *p = pixel;
    break;
  case 2:
    *(Uint16*)p = pixel;
    break;
  case 3:
    if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
      p[0] = (pixel >> 16) & 0xff;
      p[1] = (pixel >> 8) & 0xff;
      p[2] = pixel & 0xff;
    } else {
      p[0] = pixel & 0xff;
      p[1] = (pixel >> 8) & 0xff;
      p[2] = (pixel >> 16) & 0xff;
    }
    break;
  case 4:
    *(Uint32*)p = pixel;
    break;
  }
}



/* Create a new surface similar to the given surface, but with (maybe)
 * different dimensions. If the given surface has a palette, the new
 * surface will share the same palette. The new surface will have the
 * same color key, alpha mod, color mod, and blend mode as the given
 * surface. The new surface will have no clip rect.
 */
SDL_Surface* chickenSDL2_MakeSimilarSurface( SDL_Surface* src, int w, int h )
{
  SDL_Surface* dst = SDL_CreateRGBSurface(
    0, w, h, src->format->BitsPerPixel,
    src->format->Rmask, src->format->Gmask, src->format->Bmask, src->format->Amask
  );

  if (!dst) { return NULL; }

  /* Share palette if needed. */
  if (dst->format->palette && src->format->palette) {
    SDL_SetPixelFormatPalette( dst->format, src->format->palette );
  }

  /* Copy color key. */
  int success = 0;
  Uint32 colorKey = 0;
  success = SDL_GetColorKey(src, &colorKey);
  if (0 == success) {
    success = SDL_SetColorKey(dst, SDL_TRUE, colorKey);
    if (0 != success) {
      SDL_FreeSurface(dst);
      return NULL;
    }
  } else {
    /* No color key, so do nothing. */
  }

  /* Copy alpha mod. */
  Uint8 alphaMod = 0;
  success = SDL_GetSurfaceAlphaMod(src, &alphaMod);
  if (0 == success) {
    success = SDL_SetSurfaceAlphaMod(dst, alphaMod);
    if (0 != success) {
      SDL_FreeSurface(dst);
      return NULL;
    }
  } else {
    SDL_FreeSurface(dst);
    return NULL;
  }

  /* Copy blend mode. */
  SDL_BlendMode blendMode = SDL_BLENDMODE_NONE;
  success = SDL_GetSurfaceBlendMode(src, &blendMode);
  if (0 == success) {
    success = SDL_SetSurfaceBlendMode(dst, blendMode);
    if (0 != success) {
      SDL_FreeSurface(dst);
      return NULL;
    }
  } else {
    SDL_FreeSurface(dst);
    return NULL;
  }

  /* Copy color mod. */
  Uint8 colorModR = 0;
  Uint8 colorModG = 0;
  Uint8 colorModB = 0;
  success = SDL_GetSurfaceColorMod(src, &colorModR, &colorModG, &colorModB);
  if (0 == success) {
    success = SDL_SetSurfaceColorMod(dst, colorModR, colorModG, colorModB);
    if (0 != success) {
      SDL_FreeSurface(dst);
      return NULL;
    }
  } else {
    SDL_FreeSurface(dst);
    return NULL;
  }

  return dst;
}



/* Return a copy of the surface, rotated by a number of 90 degree
 * clockwise turns. Automatically locks original surface if needed.
 * If the given format has a palette, the new surface will share the
 * same palette.
 */
SDL_Surface* chickenSDL2_RotateSurface90( SDL_Surface* src, int turns )
{
  /* Make sure turns is 0, 1, 2, or 3. */
  turns = turns % 4;
  if (turns < 0) { turns += 4; }

  int dst_w = src->w;
  int dst_h = src->h;
  if (turns % 2) { /* 90 or 270 degrees. Width and height switch. */
    dst_w = src->h;
    dst_h = src->w;
  }

  /* Create a new surface with same format. */
  SDL_Surface* dst = chickenSDL2_MakeSimilarSurface( src, dst_w, dst_h );

  if (!dst) { return NULL; }

  if (SDL_MUSTLOCK(src)) { SDL_LockSurface(src); }
  if (SDL_MUSTLOCK(dst)) { SDL_LockSurface(dst); }

  /* Copy each pixel */
  int src_x, src_y, dst_x, dst_y;
  for( src_x = 0; src_x < src->w; ++src_x ) {
    for( src_y = 0; src_y < src->h; ++src_y ) {
      switch (turns) {
        case 3: { /* 270 degrees */
          dst_x = src_y;
          dst_y = src->w - 1 - src_x;
          break;
        }
        case 2: { /* 180 degrees */
          dst_x = src->w - 1 - src_x;
          dst_y = src->h - 1 - src_y;
          break;
        }
        case 1: { /* 90 degrees */
          dst_x = src->h - 1 - src_y;
          dst_y = src_x;
          break;
        }
        default: { /* 0 degrees */
          dst_x = src_x;
          dst_y = src_y;
          break;
        }
      }

      chickenSDL2_SurfaceSetPixel(
        dst, dst_x, dst_y,
        chickenSDL2_SurfaceGetPixel( src, src_x, src_y )
      );
    }
  }

  if (SDL_MUSTLOCK(src)) { SDL_UnlockSurface(src); }
  if (SDL_MUSTLOCK(dst)) { SDL_UnlockSurface(dst); }

  return dst;
}



/* Return a copy of the surface, flipped on the X and/or Y axes.
 * Automatically locks original surface if needed. If the given format
 * has a palette, the new surface will share the same palette.
 */
SDL_Surface* chickenSDL2_FlipSurface( SDL_Surface* src, int flip_x, int flip_y )
{
  /* Create a new surface with same format. */
  SDL_Surface* dst = chickenSDL2_MakeSimilarSurface( src, src->w, src->h );

  if (!dst) { return NULL; }

  if (SDL_MUSTLOCK(src)) { SDL_LockSurface(src); }
  if (SDL_MUSTLOCK(dst)) { SDL_LockSurface(dst); }

  /* Copy each pixel */
  int src_x, src_y, dst_x, dst_y;
  for( src_x = 0; src_x < src->w; ++src_x ) {
    for( src_y = 0; src_y < src->h; ++src_y ) {
      dst_x = (flip_x ? (src->w - 1 - src_x) : src_x);
      dst_y = (flip_y ? (src->h - 1 - src_y) : src_y);
      chickenSDL2_SurfaceSetPixel(
        dst, dst_x, dst_y,
        chickenSDL2_SurfaceGetPixel( src, src_x, src_y )
      );
    }
  }

  if (SDL_MUSTLOCK(src)) { SDL_UnlockSurface(src); }
  if (SDL_MUSTLOCK(dst)) { SDL_UnlockSurface(dst); }

  return dst;
}

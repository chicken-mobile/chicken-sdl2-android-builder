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


#define CLAMP(x, low, high) (x < low ? low : x > high ? high : x)


/* Since C doesn't support overloaded functions, these generic math
 * functions have suffixes to indicate their argument types.
 */

Sint32 chickenSDL2_Add_ii(Sint32 a, Sint32 b)
{
  Sint64 result = a + b;
  return CLAMP(result, INT32_MIN, INT32_MAX);
}

Sint32 chickenSDL2_Sub_ii(Sint32 a, Sint32 b)
{
  Sint64 result = a - b;
  return CLAMP(result, INT32_MIN, INT32_MAX);
}

Sint32 chickenSDL2_Mul_ii(Sint32 a, Sint32 b)
{
  Sint64 result = a * b;
  return CLAMP(result, INT32_MIN, INT32_MAX);
}

Sint32 chickenSDL2_Mul_id(Sint32 a, double b)
{
  double result = a * b;
  return CLAMP(result, INT32_MIN, INT32_MAX);
}

Sint32 chickenSDL2_Div_ii(Sint32 a, Sint32 b)
{
  Sint64 result = a / b;
  return CLAMP(result, INT32_MIN, INT32_MAX);
}

Sint32 chickenSDL2_Div_id(Sint32 a, double b)
{
  double result = a / b;
  return CLAMP(result, INT32_MIN, INT32_MAX);
}

/* Linear inter/extrapolation of Sint32s, with overflow checks.
 * t between 0.0 and 1.0 performs interpolation between a and b.
 * t less than 0.0 or greater than 1.0 performs extrapolation.
 */
Sint32 chickenSDL2_Lerp_iid(Sint32 a, Sint32 b, double t)
{
  double result = a + (b - a) * t;
  return CLAMP(result, INT32_MIN, INT32_MAX);
}

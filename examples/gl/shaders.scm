;; requires canvas.scm

(define (p/fill out r g b a)
  (with-output-to-canvas
   out
   (gl:clear-color r g b a)
   (gl:clear gl:+color-buffer-bit+)))

(define (p/pset out   x y   r g b a)
  (with-texture
   (canvas-tex out)
   (gl:tex-sub-image-2d gl:+texture-2d+ ;; target
                        0               ;; level
                        x y 2 2         ;; x y w h
                        gl:+rgb+
                        gl:+unsigned-byte+
                        (gl-utils:->pointer (f32vector r g b   r g b   r g b   r g b))))
  (gl-utils:check-error))

(define (p/rain canvas r g b a #!optional (drops 100))
  (repeat drops
          (p/pset canvas
                  (random (canvas-w canvas))
                  (random (canvas-h canvas))
                  r g b a)))

;; (p/pset den  100 100  1 0 0 0)

;; (map canvas-tex (list den den2 vel vel2 divergence prs prs2))


(define p/splat
  (let ()
    (define prg
      (create-program
       #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform vec2 Point;
uniform float Radius;
uniform vec3 FillColor;

void main() {
    float d = distance(Point, gl_FragCoord.xy);
    if (d < Radius) {
        float a = (Radius - d) * 0.5;
        a = (d/Radius);//min(a, 1.0);
        FragColor = vec4(FillColor, 1.0);
    } else {
        FragColor = vec4(0);
    }
}
"))
    (let-program-locations
     prg (Point Radius FillColor)

     (lambda (out  x y radius  r g b)
       (with-output-to-canvas
        out
        (with-program
         prg

         (gl:uniform2f Point     (* x (canvas-w out)) (* (canvas-h out) y))
         (gl:uniform1f Radius    (* radius (canvas-h out)))
         (gl:uniform3f FillColor r g b)

         (gl:enable gl:+blend+)
         (gl:blend-func gl:+one+ gl:+one-minus-src-alpha+)
         ;; gl:+one-minus-src-alpha+ wont work on my fairphone2! why!?
         (render-square)
         (gl:disable gl:+blend+)))))))


(define p/splat+
  (let ()
    (define prg
      (create-program
       #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform vec2 Point;
uniform float Radius;
uniform vec3 FillColor;

void main() {
    float d = distance(Point, gl_FragCoord.xy);
    if (d < Radius) {
        float a = (Radius - d) * 0.5;
        a = (d/Radius);//min(a, 1.0);
        FragColor = vec4(FillColor, 1.0);
    } else {
        FragColor = vec4(0);
    }
}
"))
    (let-program-locations
     prg (Point Radius FillColor)

     (lambda (out  x y radius  r g b)
       (with-output-to-canvas
        out
        (with-program
         prg

         (gl:uniform2f Point     (* x (canvas-w out)) (* (canvas-h out) y))
         (gl:uniform1f Radius    (* radius (canvas-h out)))
         (gl:uniform3f FillColor r g b)

         (gl:enable gl:+blend+)
         (gl:blend-func gl:+one+ gl:+one+)
         ;;(gl:blend-func gl:+one+ gl:+one-minus-src-alpha+)
         ;; gl:+one-minus-src-alpha+ wont work on my fairphone2! why!?
         (render-square)
         (gl:disable gl:+blend+)))))))

(define p/+

  (let ((prg
         (create-program
          #f
          "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D VelocityTexture;
uniform vec2 InverseSize;
uniform float Amount;

void main() {
 vec2 fragCoord = gl_FragCoord.xy;
 FragColor = texture(VelocityTexture, InverseSize * fragCoord) + Amount;
}
")))
    (let-program-locations
     prg (InverseSize Amount)
     (lambda (out source amount)
       (with-output-to-canvas
        out
        (with-program
         prg
         (gl:uniform2f InverseSize (/ 1 (canvas-w out)) (/ 1 (canvas-h out)))
         (gl:uniform1f Amount amount)

         (gl:bind-texture gl:+texture-2d+ (canvas-tex source))
         (render-square)))))))

(define p/*

  (let ((prg
         (create-program
          #f
          "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D A;
uniform sampler2D B;
uniform vec2 InverseSize;

void main() {
 //vec2 tc = gl_FragCoord.xy * InverseSize;
 ivec2 T = ivec2(gl_FragCoord.xy);
 FragColor = texelFetch(A, T, 0) * clamp(texelFetch(B, T, 0), 0.0, 1.0);
}
")))
    (let-program-locations
     prg (InverseSize A B)
     (lambda (out a b)
       (with-output-to-canvas
        out
        (with-program
         prg

         (gl:uniform1i B 1)
         (gl:active-texture gl:+texture1+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex b))

         
         (gl:uniform1i A 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex a))

         (render-square)))))))

(define p/advect
  (let ()
    (define prg
      (create-program
       #f
       "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D VelocityTexture;
uniform sampler2D SourceTexture;
uniform sampler2D Obstacles;

uniform vec2 InverseSize;
uniform float TimeStep;
uniform float Dissipation;

void main() {
    vec2 fragCoord = gl_FragCoord.xy;
    float solid = texture(Obstacles, InverseSize * fragCoord).x;
    if (solid > 0.0) {
        FragColor = vec4(0);
        return;
    }

    vec2 u = texture(VelocityTexture, InverseSize * fragCoord).xy;
    vec2 coord = InverseSize * (fragCoord - TimeStep * u);
    FragColor = Dissipation * texture(SourceTexture, coord);
}
"))
    (let-program-locations
     prg (VelocityTexture SourceTexture Obstacles InverseSize TimeStep Dissipation)

     (lambda (out  velocity source obstacles timestep dissipation)
       (assert (= 2 (canvas-d velocity)))
       (with-output-to-canvas
        out
        (with-program
         prg

         (gl:uniform1i Obstacles 2)
         (gl:active-texture gl:+texture2+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex obstacles))

         (gl:uniform1i SourceTexture   1)
         (gl:active-texture gl:+texture1+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex source))

         
         (gl:uniform1i VelocityTexture 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

         (gl:uniform2f InverseSize     (/ 1 (canvas-w out)) (/ 1 (canvas-h out)))
         (gl:uniform1f TimeStep timestep)
         (gl:uniform1f Dissipation dissipation)
         (render-square)))))))

;; this one was really hard to write. the idea is that we always
;; subtract the same amount from one place as we add to another. this
;; has a big limitation: velocities will be capped at 1 (transfer only
;; between neightbours).
(define p/advect-conserving
  (let ()
    (define prg
      (create-program
       #f
       "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D VelocityTexture;
uniform sampler2D SourceTexture;
uniform sampler2D Obstacles;

uniform vec2 InverseSize;
uniform float TimeStep;
uniform float Dissipation;
uniform bool Positive; // set to true to avoid negative values (ie for density)

// scale [x,y] such that x+y = 1.0 at most, and x and y become their
// respective fractions.

vec2 ratio(vec2 v) {
  float s = abs(v.x) + abs(v.y);
  if(s == 0.0) return vec2(0);
  //if(s < 1.0) s = 1.0;
  return vec2(v.x / s, v.y / s);
}

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // calculate how much I will remove from my neighbours and add that to my value
    // calculate how much my neighbouard will take from me and subtract that.

    // where are my neighbouard moving to
    vec2 velN = texelFetchOffset(VelocityTexture, T,0,ivec2( 0, +1)).xy;
    vec2 velS = texelFetchOffset(VelocityTexture, T,0,ivec2( 0, -1)).xy;
    vec2 velE = texelFetchOffset(VelocityTexture, T,0,ivec2(+1,  0)).xy;
    vec2 velW = texelFetchOffset(VelocityTexture, T,0,ivec2(-1,  0)).xy;
    vec2 vel  = texelFetch(VelocityTexture, T, 0).xy;

    // surrounding obstructions
    vec4 oN = texelFetchOffset(Obstacles, T, 0, ivec2( 0, +1));
    vec4 oS = texelFetchOffset(Obstacles, T, 0, ivec2( 0, -1));
    vec4 oE = texelFetchOffset(Obstacles, T, 0, ivec2(+1,  0));
    vec4 oW = texelFetchOffset(Obstacles, T, 0, ivec2(-1,  0));
    vec4 o  = texelFetch(Obstacles, T, 0);


    // what do they carry with them
    vec4 pN = texelFetchOffset(SourceTexture, T, 0, ivec2( 0, +1));
    vec4 pS = texelFetchOffset(SourceTexture, T, 0, ivec2( 0, -1));
    vec4 pE = texelFetchOffset(SourceTexture, T, 0, ivec2(+1,  0));
    vec4 pW = texelFetchOffset(SourceTexture, T, 0, ivec2(-1,  0));
    vec4 p  = texelFetch(SourceTexture, T, 0);

//   if(Positive) {
//     pN = max(pN, 0.0);
//     pS = max(pS, 0.0);
//     pE = max(pE, 0.0);
//     pW = max(pW, 0.0);
//     p  = max( p, 0.0);
//   }

    if(o.x > 0.0) {
      vel = velN = velS = velE = velW = vec2(0.0);
    }


    velN = ratio(velN);
    velS = ratio(velS);
    velE = ratio(velE);
    velW = ratio(velW);
    vel  = ratio(vel);

    //                    ,--- dont take from             ,-- don't give to surrounding obstacles
    if(oN.x > 0.0) { velN = vec2(0.0); if(vel.y > 0.0) vel.y = 0.0; }
    if(oS.x > 0.0) { velS = vec2(0.0); if(vel.y < 0.0) vel.y = 0.0; }
    if(oE.x > 0.0) { velE = vec2(0.0); if(vel.x > 0.0) vel.x = 0.0; }
    if(oW.x > 0.0) { velW = vec2(0.0); if(vel.x < 0.0) vel.x = 0.0; }

    // find out what our neighbours give to us
    vec4 taking = vec4(0.0, 0.0, 0.0, 0.0);
    if(velN.y < 0.0) taking += pN * -velN.y;
    if(velS.y > 0.0) taking += pS *  velS.y;
    if(velE.x < 0.0) taking += pE * -velE.x;
    if(velW.x > 0.0) taking += pW *  velW.x;


    vec4 giving = vec4(0.0, 0.0, 0.0, 0.0);
    if(vel.y > 0.0) giving += p *  vel.y;
    if(vel.y < 0.0) giving += p * -vel.y;
    if(vel.x > 0.0) giving += p *  vel.x;
    if(vel.x < 0.0) giving += p * -vel.x;

    FragColor = p + (taking - giving) * TimeStep;
}
"))
    (let-program-locations
     prg (VelocityTexture SourceTexture Obstacles InverseSize TimeStep Positive)

     (lambda (out velocity source obstacles timestep #!optional (positive (if (= 1 (canvas-d source)) #t #f)))
       (assert (= 2 (canvas-d velocity)))
       (with-output-to-canvas
        out
        (with-program
         prg

         (gl:uniform1i Obstacles 2)
         (gl:active-texture gl:+texture2+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex obstacles))

         (gl:uniform1i SourceTexture   1)
         (gl:active-texture gl:+texture1+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex source))

         
         (gl:uniform1i VelocityTexture 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

         (gl:uniform2f InverseSize     (/ 1 (canvas-w out)) (/ 1 (canvas-h out)))
         (gl:uniform1f TimeStep timestep)
         (gl:uniform1i TimeStep (if positive 1 0))
         (render-square)))))))


(define p/divergence
  (let ()
    (define prg
      (create-program #f "
#version 300 es
precision mediump float;

out float FragColor;

uniform sampler2D Velocity;
uniform sampler2D Obstacles;
uniform float HalfInverseCellSize;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring velocities:
    vec2 vN = texelFetchOffset(Velocity, T, 0, ivec2(0, 1)).xy;
    vec2 vS = texelFetchOffset(Velocity, T, 0, ivec2(0, -1)).xy;
    vec2 vE = texelFetchOffset(Velocity, T, 0, ivec2(1, 0)).xy;
    vec2 vW = texelFetchOffset(Velocity, T, 0, ivec2(-1, 0)).xy;

   // Find neighboring obstacles:
    vec3 oN = texelFetchOffset(Obstacles, T, 0, ivec2(0, 1)).xyz;
    vec3 oS = texelFetchOffset(Obstacles, T, 0, ivec2(0, -1)).xyz;
    vec3 oE = texelFetchOffset(Obstacles, T, 0, ivec2(1, 0)).xyz;
    vec3 oW = texelFetchOffset(Obstacles, T, 0, ivec2(-1, 0)).xyz;

    // Use obstacle velocities for solid cells:
    if (oN.x > 0.0) vN = vec2(0.0, 0.0); //oN.yz;
    if (oS.x > 0.0) vS = vec2(0.0, 0.0); //oS.yz;
    if (oE.x > 0.0) vE = vec2(0.0, 0.0); //oE.yz;
    if (oW.x > 0.0) vW = vec2(0.0, 0.0); //oW.yz;

    FragColor = HalfInverseCellSize * (vE.x - vW.x + vN.y - vS.y);
}
"))
    (let-program-locations
     prg (Velocity Obstacles HalfInverseCellSize)

     (lambda (out velocity obstacles #!optional (half-inverse-size (/ 0.5 1.25)))

       (with-output-to-canvas
        out
        (with-program
         prg

         (gl:uniform1i Obstacles 1)
         (gl:active-texture gl:+texture1+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex obstacles))

         (gl:uniform1i Velocity 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

         (gl:uniform1f HalfInverseCellSize half-inverse-size)
         (render-square)))))))

(define p/jacobi
  (let ()

    (define prg (create-program #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D Pressure;
uniform sampler2D Divergence;
uniform sampler2D Obstacles;

uniform float Alpha;
uniform float InverseBeta;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring pressure:
    vec4 pN = texelFetchOffset(Pressure, T, 0, ivec2(0, 1));
    vec4 pS = texelFetchOffset(Pressure, T, 0, ivec2(0, -1));
    vec4 pE = texelFetchOffset(Pressure, T, 0, ivec2(1, 0));
    vec4 pW = texelFetchOffset(Pressure, T, 0, ivec2(-1, 0));
    vec4 pC = texelFetch(Pressure, T, 0);

    // Find neighboring obstacles:
    vec3 oN = texelFetchOffset(Obstacles, T, 0, ivec2(0, 1)).xyz;
    vec3 oS = texelFetchOffset(Obstacles, T, 0, ivec2(0, -1)).xyz;
    vec3 oE = texelFetchOffset(Obstacles, T, 0, ivec2(1, 0)).xyz;
    vec3 oW = texelFetchOffset(Obstacles, T, 0, ivec2(-1, 0)).xyz;

    // Use center pressure for solid cells:
    if (oN.x > 0.0) pN = pC;
    if (oS.x > 0.0) pS = pC;
    if (oE.x > 0.0) pE = pC;
    if (oW.x > 0.0) pW = pC;

    vec4 bC = texelFetch(Divergence, T, 0);
    FragColor = (pW + pE + pS + pN + Alpha * bC) * InverseBeta;
}
"))

    (let-program-locations
     prg (Pressure Divergence Obstacles Alpha InverseBeta)

     (lambda (out pressure divergence obstacles)
       (with-output-to-canvas
        out
        (with-program
         prg


         (define CellSize 1.25)

         (gl:uniform1i Obstacles  2)
         (gl:active-texture gl:+texture2+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex obstacles))

         (gl:uniform1i Divergence 1)
         (gl:active-texture gl:+texture1+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex divergence))

         (gl:uniform1i Pressure 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex pressure))

         (gl:uniform1f Alpha (* (- CellSize) CellSize))
         (gl:uniform1f InverseBeta 0.25)

         (render-square)))))))

(define p/subtract-gradient
  (let ()
    (define prg (create-program #f "
#version 300 es
precision mediump float;

out vec2 FragColor;

uniform sampler2D Velocity;
uniform sampler2D Pressure;
uniform sampler2D Obstacles;
uniform float GradientScale;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring pressure:
    float pN = texelFetchOffset(Pressure, T, 0, ivec2(0, 1)).r;
    float pS = texelFetchOffset(Pressure, T, 0, ivec2(0, -1)).r;
    float pE = texelFetchOffset(Pressure, T, 0, ivec2(1, 0)).r;
    float pW = texelFetchOffset(Pressure, T, 0, ivec2(-1, 0)).r;
    float pC = texelFetch(Pressure, T, 0).r;


    // Find neighboring obstacles:
    vec3 oN = texelFetchOffset(Obstacles, T, 0, ivec2(0, 1)).xyz;
    vec3 oS = texelFetchOffset(Obstacles, T, 0, ivec2(0, -1)).xyz;
    vec3 oE = texelFetchOffset(Obstacles, T, 0, ivec2(1, 0)).xyz;
    vec3 oW = texelFetchOffset(Obstacles, T, 0, ivec2(-1, 0)).xyz;


    // Use center pressure for solid cells:
    vec2 vMask = vec2(1);

    if (oN.x > 0.0) { pN = pC; if(pC < pS) vMask.y = 0.0; }
    if (oS.x > 0.0) { pS = pC; if(pC < pN) vMask.y = 0.0; }
    if (oE.x > 0.0) { pE = pC; if(pC < pW) vMask.x = 0.0; }
    if (oW.x > 0.0) { pW = pC; if(pC < pE) vMask.x = 0.0; }

    // Enforce the free-slip boundary condition:
    vec2 oldV = texelFetch(Velocity, T, 0).xy;
    vec2 grad = vec2(pE - pW, pN - pS) * GradientScale;
    FragColor = vMask * (oldV - grad);
}
"))

    (define CellSize 1.25)

    (let-program-locations
     prg (Velocity Pressure Obstacles GradientScale)

     (lambda (out velocity pressure obstacles #!optional (gradient-scale (/ 1.125 CellSize)))
       (with-output-to-canvas
        out
        (with-program
         prg

         (gl:uniform1i Obstacles 2)
         (gl:active-texture gl:+texture2+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex obstacles))

         (gl:uniform1i Pressure 1)
         (gl:active-texture gl:+texture1+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex pressure))

         (gl:uniform1i Velocity 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

         (gl:uniform1f GradientScale gradient-scale)

         (render-square)))))))






;; ==================== lin_solve from Jos Stam's famout paper ====================

(define p/lin_solve
  (let ()

    (define prg (create-program #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D Out;
uniform sampler2D Source0;
uniform sampler2D Obstacles;

uniform float Alpha;
uniform float InverseBeta;
uniform ivec2 Edge;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring pressure:
    vec4 pN = texelFetchOffset(Out, T, 0, ivec2(0, 1));
    vec4 pS = texelFetchOffset(Out, T, 0, ivec2(0, -1));
    vec4 pE = texelFetchOffset(Out, T, 0, ivec2(1, 0));
    vec4 pW = texelFetchOffset(Out, T, 0, ivec2(-1, 0));
    vec4 pC = texelFetch(Out, T, 0);


    // Find neighboring obstacles:
    vec4 oN = texelFetchOffset(Obstacles, T, 0, ivec2(0, 1));
    vec4 oS = texelFetchOffset(Obstacles, T, 0, ivec2(0, -1));
    vec4 oE = texelFetchOffset(Obstacles, T, 0, ivec2(1, 0));
    vec4 oW = texelFetchOffset(Obstacles, T, 0, ivec2(-1, 0));

    vec4 s0;

    if(oN.x > 0.0) pN = pC;
    if(oS.x > 0.0) pS = pC;
    if(oE.x > 0.0) pE = pC;
    if(oW.x > 0.0) pW = pC;

    // TODO: handle corner-cases (literally)
    if(T.x == 0)           { s0 = texelFetchOffset(Source0, T, 0, ivec2(1,0)); }
    else if(T.y == 0)      { s0 = texelFetchOffset(Source0, T, 0, ivec2(0,1)); }
    else if(T.x >= Edge.x) { s0 = texelFetchOffset(Source0, T, 0, ivec2(-1,0)); }
    else if(T.y >= Edge.y) { s0 = texelFetchOffset(Source0, T, 0, ivec2(0,-1)); }
    else { s0 = texelFetch(Source0, T, 0); }

    FragColor = (s0 + Alpha * (pN + pS + pE + pW)) * InverseBeta;
}
"))

    (define CellSize 1.25)
    (define wrk #f) ;; temporary worker surface since we can't write to our uniform texture

    (let-program-locations
     prg (Out Source0 Obstacles Alpha Edge InverseBeta)

     (lambda (out source0 obstacles alpha beta iterations)

       (define out-fb (canvas-fb out))

       (unless (and (canvas? wrk)
                    (=  (canvas-h wrk) (canvas-h out))
                    (=  (canvas-w wrk) (canvas-w out))
                    (>= (canvas-d wrk) (canvas-d out)))
         (print "obs: p/lin_solve creating new temporary surface")
         (set! wrk (create-canvas (canvas-w out)
                                  (canvas-h out)
                                  (canvas-d out))))
       ;;(print "start wrk " wrk " " out)
       (p/fill wrk 0 0 0 0)

       (with-program
        prg
        (repeat
         iterations ;; must be even since we're swapping. must leak our wrk!
         (with-output-to-canvas
          out

          (gl:uniform1i Obstacles 2)
          (gl:active-texture gl:+texture2+)
          (gl:bind-texture   gl:+texture-2d+ (canvas-tex obstacles))

          (gl:uniform1i Out 1)
          (gl:active-texture gl:+texture1+)
          (gl:bind-texture   gl:+texture-2d+ (canvas-tex wrk))

          (gl:uniform1i Source0 0)
          (gl:active-texture gl:+texture0+)
          (gl:bind-texture   gl:+texture-2d+ (canvas-tex source0))

          (gl:uniform1f Alpha alpha)
          (gl:uniform1f InverseBeta (/ beta))
          (gl:uniform2i Edge (- (canvas-w source0) 1) (- (canvas-h source0) 1))

          (render-square))
         (canvas-swap! out wrk)))

       (if (not (= out-fb (canvas-fb out)))
           (error "out has been swapped, use even number of iterations"))))))


(define (p/diffuse out source obstacles diff dt #!optional (iterations 30))
  (let ((alpha (* dt diff (canvas-w out) (canvas-h out))))
    (p/lin_solve out source obstacles alpha (+ 1 (* 4 alpha)) iterations)))

;; out is new velocity
(define (p/project out tmp_prs tmp_divergence vel obstacles iterations)

  (p/divergence tmp_divergence vel obstacles (/ -0.5 (canvas-h tmp_divergence)))
  (p/fill tmp_prs 0 0 0 0)
  (p/lin_solve tmp_prs tmp_divergence obstacles 1 4 iterations)
  (p/subtract-gradient out vel tmp_prs obstacles (* (canvas-h tmp_divergence) 0.5)))



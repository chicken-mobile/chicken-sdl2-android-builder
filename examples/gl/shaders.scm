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

(define p/add

  (let ((prg
         (create-program
          #f
          "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D VelocityTexture;
uniform vec2 InverseSize;
uniform float amount;

void main() {
 vec2 fragCoord = gl_FragCoord.xy;
 FragColor = clamp(texture(VelocityTexture, InverseSize * fragCoord) + amount, 0.0, 1.0);
}
")))
    (let-program-locations
     prg (InverseSize amount)
     (lambda (out source amount)
       (with-output-to-canvas
        out
        (with-program
         program-blue
         (gl:uniform2f $InverseSize (/ 1 (canvas-w out)) (/ 1 (canvas-h out)))
         (gl:uniform1f $amount amount)

         (gl:bind-texture gl:+texture-2d+ (canvas-tex source))
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
//uniform sampler2D Obstacles;

uniform vec2 InverseSize;
uniform float TimeStep;
uniform float Dissipation;

void main() {
    vec2 fragCoord = gl_FragCoord.xy;
//    float solid = texture(Obstacles, InverseSize * fragCoord).x;
//    if (solid > 0) {
//        FragColor = vec4(0);
//        return;
//    }
//
    vec2 u = texture(VelocityTexture, InverseSize * fragCoord).xy;
    vec2 coord = InverseSize * (fragCoord - TimeStep * u);
    FragColor = Dissipation * texture(SourceTexture, coord);
}
"))
    (let-program-locations
     prg (VelocityTexture SourceTexture InverseSize TimeStep Dissipation)

     (lambda (out  velocity source timestep dissipation)
       (assert (= 2 (canvas-d velocity)))
       (with-output-to-canvas
        out
        (with-program
         prg

         (gl:uniform1i VelocityTexture 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

         (gl:uniform1i SourceTexture   1)
         (gl:active-texture gl:+texture1+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex source))

         (gl:active-texture gl:+texture0+)

         (gl:uniform2f InverseSize     (/ 1 (canvas-w out)) (/ 1 (canvas-h out)))
         (gl:uniform1f TimeStep timestep)
         (gl:uniform1f Dissipation dissipation)
         (render-square)))))))

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
        FragColor = vec4(FillColor, a);
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

         (gl:uniform2f Point     x y)
         (gl:uniform1f Radius    radius)
         (gl:uniform3f FillColor r g b)

         (gl:enable gl:+blend+)
         ;; gl:+one-minus-src-alpha+ wont work on my fairphone2! why!?
         (gl:blend-func gl:+one+ gl:+one+)
         (render-square)
         (gl:disable gl:+blend+)))))))

(define p/divergence
  (let ()
    (define prg
      (create-program #f "
#version 300 es
precision mediump float;

out float FragColor;

uniform sampler2D Velocity;
uniform float HalfInverseCellSize;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring velocities:
    vec2 vN = texelFetchOffset(Velocity, T, 0, ivec2(0, 1)).xy;
    vec2 vS = texelFetchOffset(Velocity, T, 0, ivec2(0, -1)).xy;
    vec2 vE = texelFetchOffset(Velocity, T, 0, ivec2(1, 0)).xy;
    vec2 vW = texelFetchOffset(Velocity, T, 0, ivec2(-1, 0)).xy;

    FragColor = HalfInverseCellSize * (vE.x - vW.x + vN.y - vS.y);
}
"))
    (let-program-locations
     prg (Velocity HalfInverseCellSize)

     (lambda (out velocity)

       (with-output-to-canvas
        out
        (with-program
         prg

         (gl:uniform1i Velocity 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

         (gl:uniform1f HalfInverseCellSize (/ 0.5 1.25))
         (render-square)))))))

(define p/jacobi
  (let ()

    (define prg (create-program #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D Pressure;
uniform sampler2D Divergence;

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

    vec4 bC = texelFetch(Divergence, T, 0);
    FragColor = (pW + pE + pS + pN + Alpha * bC) * InverseBeta;
}
"))

    (let-program-locations
     prg (Pressure Divergence Alpha InverseBeta)

     (lambda (out pressure divergence)
       (with-output-to-canvas
        out
        (with-program
         prg


         (define CellSize 1.25)

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
uniform float GradientScale;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring pressure:
    float pN = texelFetchOffset(Pressure, T, 0, ivec2(0, 1)).r;
    float pS = texelFetchOffset(Pressure, T, 0, ivec2(0, -1)).r;
    float pE = texelFetchOffset(Pressure, T, 0, ivec2(1, 0)).r;
    float pW = texelFetchOffset(Pressure, T, 0, ivec2(-1, 0)).r;
    float pC = texelFetch(Pressure, T, 0).r;

    // Use center pressure for solid cells:
    vec2 obstV = vec2(0);
    vec2 vMask = vec2(1);

    // Enforce the free-slip boundary condition:
    vec2 oldV = texelFetch(Velocity, T, 0).xy;
    vec2 grad = vec2(pE - pW, pN - pS) * GradientScale;
    vec2 newV = oldV - grad;
    FragColor = (vMask * newV) + obstV;  
}
"))

    (let-program-locations
     prg (Velocity Pressure GradientScale)

     (lambda (out velocity pressure)
       (with-output-to-canvas
        out
        (with-program
         prg

         (define CellSize 1.25)

         (gl:uniform1i Pressure 1)
         (gl:active-texture gl:+texture1+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex pressure))

         (gl:uniform1i Velocity 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

         (gl:uniform1f GradientScale (/ 1.125 CellSize))

         (render-square)))))))


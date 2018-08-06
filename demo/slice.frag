precision mediump float;
uniform vec2 resolution;
uniform float time;

#define PI 3.14159
#define TAU 6.283

float random (vec2 st) { return fract(sin(dot(st.xy, vec2(12.9898,78.233)))* 43758.5453123); }
float polar(inout vec2 p, float repetitions) {
	float angle = 2.*PI/repetitions;
	float a = atan(p.y, p.x) + angle/2.;
	float r = length(p);
	float c = floor(a/angle);
	a = mod(a,angle) - angle/2.;
	p = vec2(cos(a), sin(a))*r;
	if (abs(c) >= (repetitions/2.)) c = abs(c);
	return c;
}

vec3 cucumber (vec2 uv)
{
  vec2 p = uv;
  float dither = random(uv);
  float dist = length(uv);
  float crop = step(.5, dist);
  float shade = smoothstep(.3, .5, dist)*.5+smoothstep(.475,.5, dist);
  shade += dither * .1;
  polar(p, 3.);
  p.x -= .3;
  float crop2 = smoothstep(PI*.5,PI*.8,abs(atan(p.y,p.x)));
  polar(p, 12.);
  p.x -= .15;
  p.x *= .5;
  dist = length(p);
  shade += .2*sin(smoothstep(.1,.01,dist)*PI)*crop2;
  // shade = mod(abs(shade), 1.);
  // shade = sin(shade*PI)*.5+.5;
  shade = clamp(shade, 0., 1.);
  return mix(mix(vec3(1), vec3(0,.5,0), shade), vec3(0), crop);
}

vec3 orange (vec2 uv)
{
  vec2 p = uv;
  float dither = random(uv);
  float dist = length(uv);
  float crop = step(.5, dist);
  float shade = smoothstep(.3, .5, dist)*.5+smoothstep(.475,.5, dist)*.5;
  float angle = atan(p.y,p.x);
  float wave = angle*5.;
  float freq = 60.;
  dist = length(p);
  dist -= smoothstep(.0,1.,abs(sin(wave))+.4)*.04 * dist;
  float pulp = abs(sin(angle*freq)*sin(dist*freq+sin(angle*freq)))*.25+.5;
  shade += smoothstep(1.,.0,.1/abs(sin(wave))) * smoothstep(.45,.44, dist) * smoothstep(.01,.02, dist) * pulp;
  shade += (dither*2.-1.) * .1;
  shade = clamp(shade, 0., 1.);
  return mix(mix(vec3(1), vec3(1.0, 0.66, 0.0), shade), vec3(0), crop);
}


vec3 carot (vec2 uv)
{
  vec2 p = uv;
  float dither = random(uv);
  float dist = length(uv);
  float crop = step(dist,.5);
  polar(p, 6.);
  dist = mix(p.x, dist, dist);
  float shade = smoothstep(.3, .5, dist)*.5+smoothstep(.475,.5, dist)*.5;
  shade += ((abs(sin(dist*100.))+abs(sin(dist*400.))+abs(sin(dist*20.)))*.5+.5)*.2;
  shade += .4;
  shade += (dither*2.-1.) * .1;
  shade = clamp(shade, 0., 1.);
  return mix(vec3(1), vec3(1.0, 0.42, 0.0), shade)*crop;
}

vec3 banana (vec2 uv)
{
  vec2 p = uv;
  float dither = random(uv);
  float dist = length(uv);
  float crop = step(dist,.5);
  float shade = smoothstep(.5, .3, dist)*.5;//+smoothstep(.3,.1, dist)*.5;
	polar(p, 3.);
	p.x -= .15;
	dist = length(p);
	shade += smoothstep(.2,.0,dist);
	polar(p, 3.);
	p.x -= .05;
	p.y *= 1.+abs(p.x)*20.;
	dist = length(abs(p)-.02);
	float dark = 1.-clamp(.01/abs(dist), 0., 1.);
	dark = dark * .5 + .5;
  shade += (dither*2.-1.) * .1;
  shade = clamp(shade, 0., 1.);
  return mix(vec3(1), vec3(1.0, 0.99, 0.69), shade)*crop*dark;
}

void main ()
{
  vec2 uv = (gl_FragCoord.xy-.5*resolution.xy)/resolution.y;
  gl_FragColor = vec4(banana(uv),1);
}

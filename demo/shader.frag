precision mediump float;
#define synth_Resolution resolution

uniform float time;
uniform vec2 synth_Resolution;

const float PI = 3.14159;
const float TAU = 6.283;
const float PIHALF = 1.7079;
const float PIQUART = 0.785397;
const float epsilon = .0001;
const float steps = 30.;
const float far = 100.;

// begin

// ponk (Leon Denise) 19/07/2018
// most lines below are from the community
// licensed under hippie love conspiracy
// happy tweaking

#define repeat(p,r) (mod(p,r)-r/2.)
#define sdist(p,r) (length(p)-r)
#define ss(a,b,v) smoothstep(a,b,v)
#define add(a,b) a = mix(b,a,step(a.z,b.z))
#define beat1(v) smoothstep(1., 0., .1/abs(sin(time*PI+v)))

// Raymarching
float random (vec2 st) { return fract(sin(dot(st.xy, vec2(12.9898,78.233)))* 43758.5453123); }
float box (vec3 p, vec3 b) { vec3 d = abs(p) - b; return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0)); }
float cone( vec3 p, vec2 c ) { float q = length(p.xy); return dot(c,vec2(q,p.z)); }
float torus (vec3 p, vec2 t) { vec2 q = vec2(length(p.xz)-t.x,p.y); return length(q)-t.y; }
float smoothmin (float a, float b, float r) { float h = clamp(.5+.5*(b-a)/r, 0., 1.); return mix(b, a, h)-r*h*(1.-h); }
mat2 rot (float a) { float c=cos(a),s=sin(a); return mat2(c,-s,s,c); }
vec3 look (vec3 eye, vec3 target, vec2 anchor) {
	vec3 forward = normalize(target-eye);
	vec3 right = normalize(cross(forward, vec3(0,1,0)));
	vec3 up = normalize(cross(right, forward));
	return normalize(forward + right * anchor.x + up * anchor.y);
}
vec2 toroidal (vec2 p, float r) { return vec2(length(p)-r, atan(p.y,p.x)*r); }
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

vec3 colorGround (vec2 uv, float dither) {
	return vec3(1.-.5*clamp(.02/abs(sin(length(uv)*10.)*sin(atan(uv.y,uv.x)*20.)), 0., 1.));
}

vec3 colorCucumber (vec2 uv, float dither)
{
  vec2 p = uv;
  float dist = length(uv);
  float shade = smoothstep(.3, .5, dist)*.5+smoothstep(.475,.5, dist);
  shade += dither * .1;
  polar(p, 3.);
  p.x -= .3;
  float crop2 = smoothstep(PI*.5,PI*.8,abs(atan(p.y,p.x)));
  polar(p, 12.);
  p.x -= .15;
  p.x *= .5;
  dist = length(p);
  shade += .3*sin(smoothstep(.1,.01,dist)*PI)*crop2;
  shade = clamp(shade, 0., 1.);
  return mix(vec3(1), vec3(0.05, 0.67, 0.05), shade);
}

vec3 colorAgrum (vec2 uv, float dither, vec3 color)
{
  vec2 p = uv;
  float dist = length(uv);
  float crop = step(.5, dist);
  float shade = smoothstep(.45, .5, dist)*.5+smoothstep(.475,.5, dist)*.5;
  float angle = atan(p.y,p.x);
  float wave = angle*5.;
  float freq = 60.;
  dist = length(p);
  dist -= smoothstep(.0,1.,abs(sin(wave))+.4)*.1 * dist;
  float pulp = abs(sin(angle*freq)*sin(dist*freq+sin(angle*freq)))*.25+.5;
  shade += smoothstep(1.,.0,.1/abs(sin(wave))) * smoothstep(.41,.4, dist) * smoothstep(.01,.02, dist) * pulp;
  shade += dither;
  shade = clamp(shade, 0., 1.);
  return mix(vec3(1), color, shade);
}

vec3 colorCarot (vec2 uv, float dither)
{
  vec2 p = uv;
  float dist = length(uv);
  polar(p, 6.);
  dist = mix(p.x, dist, dist);
  float shade = smoothstep(.3, .5, dist)*.5+smoothstep(.475,.5, dist)*.5;
  shade += ((abs(sin(dist*100.))+abs(sin(dist*200.))+abs(sin(dist*20.)))*.5+.5)*.2;
  shade += .4;
  shade += dither;
  shade = clamp(shade, 0., 1.);
  return mix(vec3(1), vec3(1.0, 0.42, 0.0), shade);
}

vec3 colorBanana (vec2 uv, float dither) {
  vec2 p = uv;
  float dist = length(uv);
  float shade = smoothstep(.5, .3, dist)*.5;
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
  shade += dither;
  shade = clamp(shade, 0., 1.);
  return mix(vec3(1), vec3(1.0, 0.99, 0.69), shade)*dark;
}

vec4 orange (vec3 p) {
	return vec4(p.xz,
		max(sdist(p, 1.), abs(p.y)-.04),
		1.);
}

vec4 lemon (vec3 p) {
	return vec4(p.xz,
		max(sdist(p, 1.), abs(p.y)-.04),
		2.);
}

vec4 carot (vec3 p) {
	vec3 pp = p;
	polar(pp.xz,5.);
	p.xz -= normalize(p.xz) * sin(-abs(pp.z)*2.)*.05;
	p.xz += normalize(p.xz) * abs(sin(p.y*100.)+sin(atan(p.z,p.x)*3.)) * .002;
	return vec4(p.xz,
		max(sdist(p.xz, 1.), abs(p.y)-.04),
		3.);
}

vec4 cucumber (vec3 p) {
	float angle = atan(p.z,p.x);
	p.xz -= normalize(p.xz) * abs((1.-abs(sin(angle*20.)+.5)) * .01 + cos(angle*1.5) * .1);
	return vec4(p.xz,
		max(sdist(p.xz, 1.), abs(p.y)-.04),
		4.);
}

vec4 banana (vec3 p) {
	float angle = atan(p.z,p.x);
	p.xz -= normalize(p.xz) * abs((abs(sin(angle*20.)+.5)) * .01 + sin(angle*3.) * .05);
	return vec4(p.xz,
		max(sdist(p.xz, 1.), abs(p.y)-.04),
		5.);
}

void select (vec3 pos, float number, inout vec4 scene) {
	if (number == 0.) add(scene, orange(pos));
	else if (number == 1.) add(scene, lemon(pos));
	else if (number == 2.) add(scene, cucumber(pos));
	else if (number == 3.) add(scene, carot(pos));
	else if (number == 4.) add(scene, banana(pos));
}

void field (vec3 pos, float space, inout vec4 scene) {
	vec3 p = pos;
	float number = floor(p.z / space);
	number = mod(number, 5.);
	pos.xz = repeat(pos.xz, space);
	pos.y += sin(length(p.xz)-time)*.3;
	pos.yz *= rot(sin(p.z*4.+time)*.1);
	select(pos, number, scene);
}

vec4 geometry (vec3 pos)
{
	vec4 scene, shape;
	vec3 p;
	float x, cell, space, number;
	scene.z = 10.;
	scene.xy = pos.xz;

	// pos.xz *= rot(time*.955);
	// pos.yz *= rot(time*.65);
	// pos.yz *= rot(time*.265);

	space = 2.;
	field(pos, space, scene);
	// field(pos + vec3(space/2.,0.,space/2.), space, scene);

	/*
	p = pos;
	x = p.x + time;
	cell = PI;
	p.zx = toroidal(p.zx, 7.);
	p.yz *= rot(PI/2. + floor(x/cell) + time);
	p.x = repeat(x, cell);
	*/

	return scene;
}

float mapGlass (vec3 pos) {
	// pos.y -= sin(time);
	vec3 p = pos;
	float h = .5;
	float radius = 1.;
	float thin = .05;
	p.y -= h;
	return smoothmin(max(max(sdist(pos, radius), -sdist(pos, radius+thin)), p.y), torus(p, vec2(sqrt(radius*radius-h*h)-thin/2., thin/2.)), .05);
}

vec4 raymarchGlass (vec3 pos, vec3 ray, inout vec4 hit) {
	float total = 0.;
	vec2 e = vec2(epsilon,0);
	float dither = random(gl_FragCoord.xy/synth_Resolution.xy);
	for (float i = steps; i >= 0.; --i) {
		float dist = mapGlass(pos);
		hit.w = total;
		if (dist < epsilon * total) {
			return vec4(normalize(vec3(mapGlass(pos+e.xyy)-mapGlass(pos-e.xyy),
				mapGlass(pos+e.yxy)-mapGlass(pos-e.yxy),
				mapGlass(pos+e.yyx)-mapGlass(pos-e.yyx))),
				i/steps
			);
		} else if (total > far) break;
		// dist *= .9 + .1 * dither;
		total += dist;
		pos += ray * dist;
	}
	return vec4(0);
}

vec3 getNormal (vec3 p) {
	vec2 e = vec2(epsilon,0);
	return normalize(vec3(geometry(p+e.xyy).z-geometry(p-e.xyy).z,
	geometry(p+e.yxy).z-geometry(p-e.yxy).z,
	geometry(p+e.yyx).z-geometry(p-e.yyx).z));
}

vec4 raymarching (vec3 pos, vec3 ray, inout vec4 hit)
{
	vec4 scene;
	float total = 0.;
	float dither = random(gl_FragCoord.xy/synth_Resolution.xy);
	for (float i = steps; i >= 0.; --i) {
		scene = geometry(pos);
		float dist = scene.z;
		if (scene.z < epsilon * total || total > far) {
			hit.xyz = pos;
			hit.w = i/steps;
			break;
		}
		dist *= .9 + .1 * dither;
		total += dist;
		pos += ray * dist;
	}
	return scene;
}

void main()
{
	vec4 hit, hit2, scene, glass;
	vec3 eye, target, ray, pos, color;
	vec2 uv = (gl_FragCoord.xy-.5*synth_Resolution.xy)/synth_Resolution.y;
	eye = vec3(2,-10,7);
	target = vec3(0);
	ray = look(eye, target, uv);
	// glass = raymarchGlass(eye, ray, hit2);
	// float isGlass = step(0.01, length(glass.xyz));
	// glass.w = mix(1., glass.w, isGlass);
	// * (dot(ray, glass.xyz)*.5+.5)
	// isGlass *= step(hit2.w, length(hit.xyz));
	// ray = mix(ray, reflect(ray, glass.xyz), isGlass);
	// ray = normalize(ray);

	scene = raymarching(eye, ray, hit);
	pos = hit.xyz;
	uv = scene.xy * .5;
  float dither = (random(uv)*2.-1.)*.1;
	float ao = hit.w ;
	// float ao = mix(glass.w, 1., step(length)) * hit.w;

	color = vec3(1);
	// if (scene.w == 0.) color = colorGround(uv, dither);
	if (scene.w == 1.) color = colorAgrum(uv, dither, vec3(1.0, 0.66, 0.0));
	else if (scene.w == 2.) color = colorAgrum(uv, dither, vec3(1.0, 0.93, 0.0));
	else if (scene.w == 3.) color = colorCarot(uv, dither);
	else if (scene.w == 4.) color = colorCucumber(uv, dither);
	else if (scene.w == 5.) color = colorBanana(uv, dither);
	// color = mix(color, vec3((color.r+color.g+color.b)/3.), beat1(0.));
	ao = pow(ao, 1./2.2);
	gl_FragColor = vec4(color * ao * step(length(eye-pos), far), 1);
}

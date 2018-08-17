precision mediump float;
#define synth_Resolution resolution

uniform float time;
uniform vec2 synth_Resolution;
uniform vec2 mouse;

const float PI = 3.14159;
const float TAU = 6.283;
const float PIHALF = 1.7079;
const float PIQUART = 0.785397;
const float epsilon = .001;
const float steps = 50.;
const float far = 40.;
const float s0 = 0.;
const float s1 = 20.5;
const float s2 = 42.;
const float s3 = 61.5;
const float s4 = 90.;
const float s5 = 120.;
const float s6 = 150.;
const float s7 = 180.;
const float speed = 1.5;

// begin

// ponk (Leon Denise) 19/07/2018
// most lines below are from the community
// licensed under hippie love conspiracy
// happy tweaking

#define repeat(p,r) (mod(p,r)-r/2.)
#define sdist(p,r) (length(p)-r)
#define add(a,b) a = mix(b,a,step(a.z,b.z))
#define slice(p,r,h) max(sdist(p, r), abs(p.z)-h)
#define timeline (mod(time, 180.))
// #define timeline (65.+mod(time,10.))
// #define timeline 156.
#define cut(at) clamp(abs(timeline-at), 0., 1.)
#define cuts cut(0.)
#define beat1 (.5+.5*sin(time*20.))*(smoothstep(2., 1., abs(timeline-40.))+smoothstep(1.,.5,abs(timeline-51.))+smoothstep(1.,.5,abs(timeline-61.)))

// Raymarching
// https://www.shadertoy.com/view/4djSRW
float random (vec2 p) {
		vec3 p3  = fract(vec3(p.xyx) * 100.1031);
	    p3 += dot(p3, p3.yzx + 19.19);
	    return fract((p3.x + p3.y) * p3.z);
 }
float box (vec3 p, vec3 b) { vec3 d = abs(p) - b; return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0)); }
float cone( vec3 p, vec2 c ) { float q = length(p.xy); return dot(c,vec2(q,p.z)); }
float torus (vec3 p, vec2 t) { vec2 q = vec2(length(p.xz)-t.x,p.y); return length(q)-t.y; }
float smoothmin (float a, float b, float r) { float h = clamp(.5+.5*(b-a)/r, 0., 1.); return mix(b, a, h)-r*h*(1.-h); }
mat2 rot (float a) { float c=cos(a),s=sin(a); return mat2(c,-s,s,c); }
vec3 look (vec3 eye, vec3 target, vec2 anchor) {
	anchor /= 1.+length(anchor)*2.*beat1;
	vec3 forward = normalize(target-eye);
	vec3 right = normalize(cross(forward, vec3(0,1,0)));
	vec3 up = normalize(cross(right, forward));
	return normalize(forward / mix(1., 3., beat1) + right * anchor.x + up * anchor.y);
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

vec3 colorCucumber (vec2 p, float dither) {
	float dist = length(p);
	float shade = smoothstep(.3, .5, dist)*.5+smoothstep(.475,.5, dist);
	shade += dither * .1;
	polar(p, 3.);
	p.x -= .3;
	float crop2 = smoothstep(PI*.5,PI*.8,abs(atan(p.y,p.x)));
	polar(p, 12.);
	p.x -= .15;
	p.x *= .5;
	shade += .3*sin(smoothstep(.1,.01,length(p))*PI)*crop2;
	shade = clamp(shade, 0., 1.);
	return mix(vec3(1), vec3(0.05, 0.67, 0.05), shade);
}

vec3 colorAgrum (vec2 p, float dither, vec3 color) {
	float dist = length(p);
	float shade = smoothstep(.45, .5, dist)*.5+smoothstep(.475,.5, dist)*.5;
	float angle = atan(p.y,p.x);
	dist = length(p);
	dist -= smoothstep(.0,1.,abs(sin(angle*5.))+.4)*.1 * dist;
	float pulp = abs(sin(angle*60.)*sin(dist*60.+sin(angle*60.)))*.25+.5;
	shade += smoothstep(1.,.0,.1/abs(sin(angle*5.))) * smoothstep(.41,.4, dist) * smoothstep(.01,.02, dist) * pulp;
	shade += dither;
	shade = clamp(shade, 0., 1.);
	return mix(vec3(1), color, shade);
}

vec3 colorCarot (vec2 p, float dither) {
	float dist = length(p);
	polar(p, 6.);
	dist = mix(p.x, dist, dist);
	float shade = smoothstep(.3, .5, dist)*.5+smoothstep(.475,.5, dist)*.5;
	shade += ((abs(sin(dist*100.))+abs(sin(dist*200.))+abs(sin(dist*20.)))*.5+.5)*.2;
	shade += .4;
	shade += dither;
	shade = clamp(shade, 0., 1.);
	return mix(vec3(1), vec3(1.0, 0.42, 0.0), shade);
}

vec3 colorBanana (vec2 p, float dither) {
	float dist = length(p);
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

// cucumber p.xz -= normalize(p.xz) * abs((1.-abs(sin(angle*20.)+.5)) * .01 + cos(angle*1.5) * .1);
// banana p.xz -= normalize(p.xz) * abs((abs(sin(angle*20.)+.5)) * .01 + sin(angle*3.) * .05);

void sketch (inout vec4 scene, vec3 pos, float orbiting, float bouncing, float dropping, float splashing, float gnacing, float swimming) {
	vec3 p, pp;
	float r, salt, size, thin, angle, offset, ratio, number, anim, fall, splash, bouncy, jump, falloff, gnac, cell, shape;
	const float count = 8.;
	for (float index = count; index > 0.; --index) {
		r = index / count;
		p = pos;

		// ratio = mod(time*speed, 1.);
		ratio = mod(time*speed+r*.2*splashing, 1.);
		number = 1.+mod(mix(floor(time*speed),index,clamp(swimming,0.,1.)), 3.);
		anim = smoothstep(.8,1.,ratio)+smoothstep(.2,.0,ratio);
		fall = smoothstep(.0,.2,ratio);
		splash = smoothstep(.2,.4,ratio);
		bouncy = smoothstep(.3,.2,ratio) * fall;
		jump = smoothstep(.0,.7,ratio);
		falloff = smoothstep(.3,1.,ratio);
		offset = floor(time*speed);
		gnac = floor(ratio*8.);
		salt = random(vec2(r+offset*.14598));
		size = 1.-.5*r*splashing;
		thin = .04;

		// orbit
		p.xz *= rot((ratio*PI+PI/2.)*orbiting);

		// bounce
		angle = time*50.*speed;
		p.xy *= 1.+.4*anim*bouncing*vec2(sin(angle), cos(angle));

		// drop
		p.y += ((fall*8.-8.+r)-(bouncy * abs(sin(time+r))*2.))*dropping;
		angle = r*TAU+offset;
		p.xz += vec2(cos(angle), sin(angle)) * splash * (1.+4.*salt)*dropping;
		p.xz *= rot(salt*TAU*dropping);
		p.zy *= rot((PI/2.-r*.1)*(dropping+swimming));

		// splash
		p += vec3(sin(r*TAU*5.654+offset)*5.*ratio, 5.-14.*jump+14.*falloff, index*.5) * splashing;
		p.xz *= rot(ratio*5.*splashing);
		p.yz *= rot(r*5.456*splashing);

		// swimming
		p += vec3(sin(r*TAU*3.51)*4., sin(time+r*TAU), (r*2.-1.) * (2.+sin(time+r)))*swimming;
		p.z += sin(length(p.xy)*4.-time*5.)*.1*swimming;
		p.zx *= rot(sin(time+r*TAU+p.y)*.5*swimming);
		p.yz *= rot(sin(time+r+p.y)*.2*swimming);

		// gnac
		pp = p;
		cell = polar(pp.xy, 4.);
		pp.x -= 1. + 1. * step(gnac, cell+2.);
		shape = sdist(pp.xy, .5);
		polar(pp.xy, 16.);
		pp.x -= .5;
		shape = min(shape, sdist(pp.xy, .15));
		shape = max(slice(p, size, thin), -mix(10., shape, gnacing));
		shape = mix(10., shape, mix(1., step(gnac, 5.), gnacing));

		add(scene, vec4(p.xy/size, shape, number));
	}
}

vec4 geometry (vec3 pos)
{
	vec4 scene;
	vec3 p, pp;
	float dropping, bouncing, splashing, gnacing, orbiting, swimming;
	const float count = 4.;
	scene.z = 10.;
	scene.xy = pos.xz;
	scene.w = 0.;

	orbiting = step(timeline, s1);
	bouncing = step(s1, timeline) * step(timeline, s2);
	dropping = step(s2, timeline) * step(timeline, s3);
	swimming = step(s3, timeline) * step(timeline, s4);
	splashing = step(s4, timeline) * step(timeline, s5);
	gnacing = step(s5, timeline) * step(timeline, s6);

	sketch(scene, pos, orbiting, bouncing, dropping, splashing, gnacing, swimming);

	// orbiting = 0.;
	// bouncing = 0.;
	// dropping = 0.;
	// swimming = 0.;
	// splashing = 1.;
	// gnacing = 0.;
	// sketch(scene, pos, orbiting, bouncing, dropping, splashing, gnacing, swimming);

	return scene;
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
	// uv = mix(uv, -uv, step(sin(length(uv)*8.-time*4.), .0));
	// uv *= rot(floor(length(uv)*8.)*time);
	// eye = vec3(0,-1,3);
	eye = mix(vec3(0,4,25), vec3(0,0,3), smoothstep(s0, s1/2., timeline));
	eye = mix(eye, vec3(vec2(1)*rot(floor(time*speed)*1.5156),1), step(s1, timeline));
	eye = mix(eye, vec3(0,5,8), step(s2, timeline));
	eye = mix(eye, vec3(0,-3,8), step(s3, timeline));
	// eye = mix(eye, vec3(vec2(1)*rot(floor(time*speed)*1.5156),1), step(s4, timeline));
	// eye = vec3(1,2,5);
	// eye.xz *= rot(mouse.x*2.-1.);
	// eye.yz *= rot(mouse.y*2.-1.);
	target = vec3(0);
	ray = look(eye, target, uv);
	color = mix(vec3(.9,.5,.5), vec3(1,1,.7), uv.y);

	vec2 p = uv;
	p *= 1.-length(p)*.5;
	float cell = .05;
	vec2 id = floor(p/cell);
	p = repeat(p, cell);
	p *= rot(time+random(id/10.)*TAU);
	p.x -= cell/4.;
	color += .0002/length(p);

	scene = raymarching(eye, ray, hit);
	pos = hit.xyz;
	uv = scene.xy * .5;
	float dither = (random(uv)*2.-1.)*.1;

	// hit.w = pow(hit.w, 1./2.2);
	float far = step(length(eye-pos), far);

	if (scene.w == 1.) color = colorAgrum(uv, dither, vec3(1.0, 0.66, 0.0));
	else if (scene.w == 2.) color = colorAgrum(uv, dither, vec3(1, 0.913, 0.341));
	else if (scene.w == 3.) color = colorAgrum(uv, dither, vec3(0.376, 0.819, 0.278));
	// else if (scene.w == 3.) color = colorCucumber(uv, dither);
	// else if (scene.w == 4.) color = colorCarot(uv, dither);
	// else if (scene.w == 5.) color = colorBanana(uv, dither);
	// color *= 1.-clamp(pos.z, 0., 1.);

 // * pow(smoothstep(.5, 1., hit.w), 1./2.2) * step(length(eye-pos), far);

	gl_FragColor = vec4(cuts * color, 1);
}

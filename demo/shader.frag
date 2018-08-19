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
const float steps = 100.;
const float far = 40.;
const float s0 = 0.;
const float s1 = 20.5;
const float s2 = 41.5;
const float s3 = 51.;
const float s4 = 61.5;
const float s5 = 82.;
const float s6 = 122.;
const float speed = 1.5;
const float count = 8.;
const float size = 1.;
const float thin = .04;

// begin

// ponk (Leon Denise) 18/08/2018
// most lines below are from the community
// licensed under hippie love conspiracy
// happy tweaking

#define repeat(p,r) (mod(p,r)-r/2.)
void add (inout vec4 a, vec4 b) { a = mix(b,a,step(a.z,b.z)); }
float random (vec2 p) { vec3 p3  = fract(vec3(p.xyx) * 100.1031); p3 += dot(p3, p3.yzx + 19.19); return fract((p3.x + p3.y) * p3.z); }
mat2 rot (float a) { float c=cos(a),s=sin(a); return mat2(c,-s,s,c); }
void toroidal (inout vec2 p, float r) { p = vec2(length(p)-r, atan(p.y,p.x)*r); }
void polar (inout vec2 p, float c) {
	float an = TAU/c;
	float a = mod(atan(p.y,p.x)+an/2., an)-an/2.;
	p = vec2(cos(a), sin(a))*length(p);
}

vec4 geometry (vec3 pos)
{
	vec4 scene;
	vec3 p, pp;
	float r, salt, z, angle, offset, ratio, number, anim, fall, splash, bouncy, gnac, cell, shape;
	scene.z = 10.;
	scene.xy = pos.xz;
	scene.w = 0.;

	ratio = mod(time*speed, 1.);
	number = 1.+mod(floor(time*speed), 4.);
	offset = floor(time*speed);

	// bouncing
	if ((time < s2 && time > s1) || time > s5) {
		p = pos;
		z = time*speed;
		if (time < s5) {
			p.z += 3.;
			p.yx *= rot(sin(floor(z)*1.5156));
			p.yz *= rot(sin(floor(z)*2.5156));
		}
		angle = z*25.;
		ratio = mod(z, 1.);
		number = 1.+mod(floor(z), 4.);
		anim = smoothstep(.8,1.,ratio)+smoothstep(.2,.0,ratio);
		p.xy *= 1.+.4*anim*vec2(sin(angle), cos(angle));

		// gnacinc
		if (time > s1+10.7 && time < s2) {
			pp = p;
			// cell = 2.+sign(pp.x)+2.*sign(pp.y);
			// pp.xy = abs(pp.xy)-.75 - 1. * step(gnac, cell+1.);
			cell = floor((atan(p.y,p.x)/PI*.5+.5)*5.);
			polar(pp.xy, 5.);
			pp.x -= 1.+step(floor(ratio*8.), cell);
			shape = length(pp.xy)-.4;
			polar(pp.xy, 16.);
			pp.x -= .4;
			shape = min(shape, length(pp.xy)-.1);
			add(scene, vec4(p.xy/size, max(max(length(p)-size, abs(p.z)-thin), -shape), number));

		} else add(scene, vec4(p.xy/size, max(length(p)-size, abs(p.z)-thin), number));
	}

	// orbiting
	if (time < s1) {
		p = pos;
		p.y -= mix(10., 0., smoothstep(s0, s1/2., time));
		p.z -= mix(8., -2., smoothstep(s1/3., s1/1.5, time));
		p.xz *= rot(ratio*PI+PI/2.);
		add(scene, vec4(p.xy/size, max(length(p)- size, abs(p.z)-thin), number));

	// swimming
	} else if (time < s5 && time > s4) {
		pos.z += mix(4., -5., smoothstep(s4, s4 + 10., time));
		pos.xz *= rot(-PI/2.);
		cell = 3.;
		z = pos.z + time;
		number = 1.+mod(floor(abs(z)/cell), 4.);
		r = number / 4.;
		pos.y += sin(pos.z * .2+time);
		p = pos;
		p.xy *= rot(pos.z*.1);
		p.x -= 4.;
		p.z = repeat(z, cell);
		p.y += sin(length(p.xz)*4.-time*5.+number)*.1;
		p.zx *= rot(sin(time+r*TAU+p.y)*.5);
		p.yz *= rot(sin(time*2.+r+p.x*2.)*.2);
		p.y = abs(p.y)-2.*mix(0., .5+.5*sin(number+time), smoothstep(65., 70., time));
		p.y = abs(p.y)-1.*mix(0., .5+.5*sin(number+time), smoothstep(70., 75., time));
		add(scene, vec4(p.xz/size, max(length(p.xzy)-size, abs(p.y)-thin), number));

	// dropping
	} else if (time < s3 && time > s2) {
		pos.z -= 4.;
		pos.yz *= rot(PI/8.);
		for (float index = count; index > 0.; --index) {
			r = index / count;
			p = pos;
			salt = random(vec2(r+offset*.14598));
			fall = smoothstep(.0,.2,ratio);
			splash = smoothstep(.2,.4,ratio);
			bouncy = smoothstep(.3,.2,ratio) * fall;
			p.y += ((fall*8.-8.+r)-(bouncy * abs(sin(time+r))*2.));
			angle = r*TAU+offset;
			p.xz += vec2(cos(angle), sin(angle)) * splash * (1.+4.*salt);
			p.xz *= rot(salt*TAU);
			p.zy *= rot(PI/2.-r*.1);
			add(scene, vec4(p.xy/size, max(length(p)- size, abs(p.z)-thin), number));
		}

	// dancing
	} else if (time < s6 && time > s5) {
		p = pos;
		p.yz *= rot(PI/2.);
		p.xz *= rot(5.*smoothstep(s5+20., s6, time));
		toroidal(p.xz, 6.);
		p.z += p.y * .5;
		number = 1.+mod(abs(floor(p.z / PI)+floor((p.y-time*5.)/4.)), 4.);
		p.y = repeat(p.y-time*5., 4.);
		p.z = repeat(p.z, PI);
		p.xz *= rot(time+number);
		p.yz *= rot(time+number);
		add(scene, vec4(p.xy/size, max(length(p)- size, abs(p.z)-thin), number));
	}


	// splashing
	if ((time < s4 && time > s3)){// || time > s5+10. ) {
		for (float index = count; index > 0.; --index) {
			r = index / count;
			cell = time*speed/2.+r;
			ratio = mod(cell, 1.);
			p = pos-vec3(0,0,5);
			p += vec3(sin(r*TAU*5.654+floor(cell))*7.*ratio, 5.-12.*smoothstep(.0,.7,ratio)+12.*smoothstep(.3,1.,ratio), index*.5);
			p.xz *= rot(ratio*8.);
			p.yz *= rot(r*5.456);
			add(scene, vec4(p.xy/size, max(length(p)- size, abs(p.z)-thin), 1.+mod(floor(cell)+index, 4.)));
		}
	}

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
		dist *= .5 + .1 * dither;
		total += dist;
		pos += ray * dist;
	}
	return scene;
}

void main()
{
	vec4 hit, hit2, scene, glass;
	vec3 eye, ray, pos, color;
	vec2 uv = (gl_FragCoord.xy-.5*synth_Resolution.xy)/synth_Resolution.y;
	float cell;
	eye = vec3(0,0,-5);
	if (time > s5) eye.z = -.5-5. * smoothstep(s5, s5 + 10., time);
	ray = normalize(vec3(uv,1.+((sin(time*25.))*(smoothstep(2., 1., abs(time-40.))+smoothstep(1.,.5,abs(time-51.))+smoothstep(1.,.5,abs(time-61.))))*.3));
	scene = raymarching(eye, ray, hit);
	pos = hit.xyz;

	if (scene.w == 1.) color = vec3(1.0, 0.66, 0.0);
	else if (scene.w == 2.) color = vec3(0.376, 0.819, 0.278);
	else if (scene.w == 3.) color = vec3(1, 0.913, 0.341);
	else if (scene.w == 4.) color = vec3(1.0, 0.11, 0.05);

	// background
	if (length(color) == 0. || pos.z > far/1.5) {
		vec2 p = (gl_FragCoord.xy-.5*synth_Resolution.xy)/synth_Resolution.y;
		vec2 id = floor(p/.05);
		p = repeat(p, .05);
		p *= rot(time+random(id/20.)*TAU);
		p.x -= .05/4.;
		color = mix(vec3(.9,.5,.5), vec3(1,1,.7), uv.y);
		float n = .01+.99*random(id/10.);
		color += smoothstep(.005*n,.004*n,length(p));

	}
	// slice texture
	else {
		uv = scene.xy*.5;
		float dist = length(uv);
		float shade = smoothstep(.45, .5, dist)*.5+smoothstep(.475,.5, dist)*.5;
		float angle = atan(uv.y,uv.x);
		dist -= smoothstep(.0,1.,abs(sin(angle*5.))+.4)*.1 * dist;
		float pulp = abs(sin(angle*60.)*sin(dist*60.+sin(angle*60.)))*.25+.5;
		color = mix(vec3(1), color, clamp(shade+smoothstep(1.,.0,.1/abs(sin(angle*5.))) * smoothstep(.41,.4, dist) * smoothstep(.01,.02, dist) * pulp + (random(scene.xy * .5)*2.-1.)*.1, 0., 1.));
	}


	if (time > s4) {
		uv = (gl_FragCoord.xy-.5*synth_Resolution.xy)/synth_Resolution.y;
		if (time < s5) {
			uv.y = abs(uv.y)-.1;
			cell = floor((uv.x+.25)/.25)/1.5;
			uv.x = repeat(uv.x+.25, .25);
		} else {
			cell = 0.;//floor(atan(uv.x,uv.y)/TAU*8.+PI/2.);
			polar(uv.yx, 8.);
			uv.y -= .3;
		}
		for (float i = 16.; i > 0.; --i) {
			float r = i / 16.;
			float salt = random(vec2(r*10.16));
			vec2 p = uv;
			float ratio = mod(time*speed+r*.1+cell, 1.);
			float number = 1.+mod(floor(r*.1+time*speed+cell), 4.);
			vec3 c;
			if (number == 1.) c = vec3(1.0, 0.66, 0.0);
			else if (number == 2.) c = vec3(0.376, 0.819, 0.278);
			else if (number == 3.) c = vec3(1, 0.913, 0.341);
			else if (number == 4.) c = vec3(1.0, 0.11, 0.05);
			float a = r*TAU*2.15498+floor(time*speed+r*.1+cell);
			// p.x += sin(offset)*.25;
			p += vec2(cos(a),sin(a)) * ratio * (.5+.5*salt) * .25;
			p.y -= sin(ratio*PI) * .2;
			float size = .01;
			if (time < s5) size -= .005*salt*smoothstep(1.,.8,ratio);
			else size += .02*salt*smoothstep(1.,.8,ratio);
			color = mix(color, c, smoothstep(size,size-.002,length(p)));
		}
	}

	gl_FragColor = vec4(clamp(abs(time), 0., 1.) * clamp(-(time-s6), 0., 1.) * color, 1);
	vec2 p = (gl_FragCoord.xy-.5*synth_Resolution.xy)/synth_Resolution.y;
	p *= 1.5*smoothstep(s6-15., s6-5., time);
	p.y -= sin(sqrt(abs(p.x))) * .4;
	p.y *= 1.25;
	p.y += .1;
	gl_FragColor *= smoothstep(.01, 0., length(p)-.5);
}

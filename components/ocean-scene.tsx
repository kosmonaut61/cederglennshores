"use client"

import { useRef, useEffect, useState, useCallback } from "react"
import * as THREE from "three"
import { gsap } from "gsap"
import { SplitText } from "gsap/SplitText"

// Register GSAP plugins
if (typeof window !== "undefined") {
  gsap.registerPlugin(SplitText)
}

// ============ FIXED PRESETS ============
const PRESETS = {
  Sunset: {
    sunPosX: 0.0,
    sunPosY: 0.05,
    sunSize: 2.1,
    sunIntensity: 4.0,
    horizonColor: "#ff2200",
    enableClouds: true,
    cloudDensity: 0.6,
    cloudColor: "#ffaa00",
    waveHeight: 0.22,
    speed: 0.35,
    sssBaseColor: "#000000",
    sssTipColor: "#ff3300",
    reflectionStrength: 1.4,
    reflectionWidth: 0.05,
    haloStrength: 0.5,
    haloRadius: 0.3,
    haloSize: 0.02,
    vignetteStrength: 0.5,
    enableGrid: false,
    flareIntensity: 1.0,
    flareGhosting: 1.0,
    flareStreak: 2.0,
    flareAngle: 140,
  },
  Sunny: {
    sunPosX: 0.0,
    sunPosY: 0.6,
    sunSize: 1.0,
    sunIntensity: 6.0,
    horizonColor: "#00bbff",
    enableClouds: true,
    cloudDensity: 0.25,
    cloudColor: "#ffffff",
    waveHeight: 0.25,
    speed: 0.4,
    sssBaseColor: "#001a33",
    sssTipColor: "#0099ff",
    reflectionStrength: 3.0,
    reflectionWidth: 0.1,
    haloStrength: 0.2,
    haloRadius: 0.3,
    haloSize: 0.02,
    vignetteStrength: 0.2,
    enableGrid: false,
    flareIntensity: 0.8,
    flareGhosting: 0.5,
    flareStreak: 3.0,
    flareAngle: 140,
  },
  Cloudy: {
    sunPosX: 0.0,
    sunPosY: 0.3,
    sunSize: 3.0,
    sunIntensity: 1.5,
    horizonColor: "#667788",
    enableClouds: true,
    cloudDensity: 1.5,
    cloudColor: "#556677",
    waveHeight: 0.45,
    speed: 0.5,
    sssBaseColor: "#111520",
    sssTipColor: "#4a5a6a",
    reflectionStrength: 0.8,
    reflectionWidth: 0.3,
    haloStrength: 0.1,
    haloRadius: 0.3,
    haloSize: 0.02,
    vignetteStrength: 0.4,
    enableGrid: false,
    flareIntensity: 0.2,
    flareGhosting: 0.3,
    flareStreak: 0.5,
    flareAngle: 140,
  },
  Night: {
    sunPosX: 0.0,
    sunPosY: 0.3,
    sunSize: 0.9,
    sunIntensity: 3.0,
    horizonColor: "#0a0a15",
    enableClouds: true,
    cloudDensity: 0.3,
    cloudColor: "#101018",
    waveHeight: 0.2,
    speed: 0.2,
    sssBaseColor: "#000005",
    sssTipColor: "#8888aa",
    reflectionStrength: 2.5,
    reflectionWidth: 0.015,
    haloStrength: 1.5,
    haloRadius: 0.3,
    haloSize: 0.02,
    vignetteStrength: 0.65,
    enableGrid: false,
    flareIntensity: 1.5,
    flareGhosting: 0.8,
    flareStreak: 1.0,
    flareAngle: 140,
  },
  Twilight: {
    sunPosX: 0.0,
    sunPosY: -0.04,
    sunSize: 2.5,
    sunIntensity: 2.0,
    horizonColor: "#1a0a20",
    enableClouds: true,
    cloudDensity: 0.4,
    cloudColor: "#2a1a30",
    waveHeight: 0.3,
    speed: 0.25,
    sssBaseColor: "#050008",
    sssTipColor: "#6644aa",
    reflectionStrength: 1.8,
    reflectionWidth: 0.08,
    haloStrength: 0.8,
    haloRadius: 0.4,
    haloSize: 0.025,
    vignetteStrength: 0.55,
    enableGrid: false,
    flareIntensity: 1.2,
    flareGhosting: 1.0,
    flareStreak: 1.5,
    flareAngle: 140,
  },
  Dark: {
    sunPosX: 0.0,
    sunPosY: 0.15,
    sunSize: 0.5,
    sunIntensity: 4.8,
    horizonColor: "#4476ff",
    enableClouds: true,
    cloudDensity: 0.15,
    cloudColor: "#080810",
    waveHeight: 0.35,
    speed: 0.15,
    sssBaseColor: "#000002",
    sssTipColor: "#222233",
    reflectionStrength: 8.2,
    reflectionWidth: 0.5,
    haloStrength: 0.6,
    haloRadius: 0.54,
    haloSize: 0.1,
    vignetteStrength: 0.75,
    enableGrid: false,
    flareIntensity: 0.8,
    flareGhosting: 0.4,
    flareStreak: 0.5,
    flareAngle: 140,
  },
}

type PresetKey = keyof typeof PRESETS

// ============ SHADER CODE ============
const fragmentShader = `
  precision highp float;
  uniform float uTime;
  uniform vec2 uResolution;
  uniform vec2 uMousePos;

  uniform float uStyle;      
  uniform float uEnableGrid; 
  uniform float uEnableClouds;
  uniform float uEnableReflections;
  uniform float uEnableFX;

  uniform float uFlareIntensity;
  uniform float uFlareGhosting;
  uniform float uFlareStreak;
  uniform float uFlareAngle;
  uniform float uCameraHeight;
  uniform float uCameraTilt;
  uniform float uFocalLength;

  uniform float uWaveHeight;
  uniform float uWaveChoppiness;
  uniform float uSpeed;
  uniform float uFlySpeed;
  
  uniform float uSssStrength;
  uniform vec3 uSssBaseColor;
  uniform vec3 uSssTipColor;
  
  uniform float uSunSize;
  uniform float uSunIntensity;
  uniform float uSunPosX;
  uniform float uSunPosY;
  
  uniform float uReflectionStrength;
  uniform float uReflectionWidth;
  
  uniform float uCloudDensity;
  uniform float uCloudSpeed;
  uniform vec3 uCloudColor;
  uniform vec3 uHorizonColor;
  
  uniform float uHaloStrength;
  uniform float uHaloRadius;
  uniform float uHaloSize;
  
  uniform float uDustStrength;
  uniform float uHorizonFade;
  uniform float uVignetteStrength;
  
  uniform float uGrainAmount;
  uniform float uGrainScale;

  #define PI 3.14159265359

  float hash(vec2 p) { return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453); }
  float noise(vec2 p) {
      vec2 i = floor(p); vec2 f = fract(p); f = f*f*(3.0-2.0*f);
      return mix(mix(hash(i+vec2(0,0)), hash(i+vec2(1,0)), f.x),
                 mix(hash(i+vec2(0,1)), hash(i+vec2(1,1)), f.x), f.y);
  }
  
  float gaussian(float z, float u, float o) {
      return (1.0 / (o * sqrt(2.0 * PI))) * exp(-(((z - u) * (z - u)) / (2.0 * (o * o))));
  }
  
  vec3 grainOverlay(vec3 a, vec3 b, float w) {
      vec3 mixed = mix(
          2.0 * a * b,
          vec3(1.0) - 2.0 * (vec3(1.0) - a) * (vec3(1.0) - b),
          step(vec3(0.5), a)
      );
      return mix(a, mixed, w);
  }
  
  float fbm(vec2 p) {
      float v = 0.0; float a = 0.5; 
      mat2 rot = mat2(0.8, 0.6, -0.6, 0.8);
      for(int i=0; i<3; i++) { v += a * noise(p); p = rot * p * 2.0; a *= 0.5; }
      return v;
  }
  
  float noise3D(vec3 p) {
      vec3 i = floor(p); vec3 f = fract(p); f = f*f*(3.0-2.0*f);
      float n = dot(i, vec3(1.0, 57.0, 113.0));
      return mix(mix(mix(hash(vec2(n+0.0)), hash(vec2(n+1.0)), f.x),
                     mix(hash(vec2(n+57.0)), hash(vec2(n+58.0)), f.x), f.y),
                 mix(mix(hash(vec2(n+113.0)), hash(vec2(n+114.0)), f.x),
                     mix(hash(vec2(n+170.0)), hash(vec2(n+171.0)), f.x), f.y), f.z);
  }
  
  float cloudNoise(vec2 p) {
      float f = 0.0;
      f += 0.50000 * noise(p); p = p * 2.02;
      f += 0.25000 * noise(p); p = p * 2.03;
      f += 0.12500 * noise(p);
      return f;
  }

  float map(vec3 p) {
      vec2 q = p.xz * 0.35; 
      float h = 0.0;
      float a = 0.6 * uWaveHeight;
      if(uWaveChoppiness > 0.1) q += vec2(fbm(q + uTime * 0.05), fbm(q)) * uWaveChoppiness;
      for(int i=0; i<4; i++) {
          float ang = float(i) * 0.6;
          vec2 dir = vec2(sin(ang), cos(ang) * 1.5); dir = normalize(dir);
          float wave = 1.0 - abs(sin(dot(q, dir) - uTime * uSpeed + float(i)));
          wave = pow(wave, 3.0); h += a * wave;
          a *= 0.5; q *= 1.8; q.x += 1.0; 
      }
      return p.y - h;
  }

  vec3 getNormal(vec3 p) {
      float eps = 0.01 + uWaveHeight * 0.02;
      vec2 e = vec2(eps, 0.0);
      return normalize(vec3(map(p+e.xyy) - map(p-e.xyy), e.x * 2.0, map(p+e.yyx) - map(p-e.yyx)));
  }

  vec3 getSky(vec3 rd, vec3 sunDir, bool renderSun) {
      float sunDot = max(0.0, dot(rd, sunDir));
      vec3 zenithCol = vec3(0.0, 0.0, 0.02); 
      vec3 skyCol = mix(uHorizonColor, zenithCol, pow(max(0.0,rd.y + 0.05), 0.5));

      float occlusion = 0.0;
      if (uEnableClouds > 0.5) {
          if (uCloudDensity > 0.0 && rd.y > 0.0 && rd.y < 0.45) {
             vec2 skyUV = rd.xz / max(0.05, rd.y); 
             skyUV.x += uTime * uCloudSpeed;
             float cl = cloudNoise(skyUV * 0.15); 
             float heightMask = smoothstep(0.0, 0.1, rd.y) * smoothstep(0.45, 0.1, rd.y);
             float cloudIntensity = smoothstep(0.3, 0.7, cl) * heightMask * uCloudDensity;
             skyCol = mix(skyCol, uCloudColor, cloudIntensity);
             occlusion = cloudIntensity;
          }
      }
      
      float sunRadiusThreshold = 0.99 - (uSunSize * 0.03); 
      float sun = (uSunSize < 0.1) ? 0.0 : smoothstep(sunRadiusThreshold, sunRadiusThreshold + 0.002, sunDot);
      float glow = (uSunSize < 0.1) ? 0.0 : pow(sunDot, 12.0 / uSunSize);
      float sunVis = 1.0 - clamp(occlusion * 1.5, 0.0, 0.9);

      vec3 sunCol = uSssTipColor * uSunIntensity * sunVis;
      skyCol += glow * sunCol * 1.5; 

      if (renderSun) { skyCol += sun * sunCol * 8.0; }
      
      if (uEnableFX > 0.5 && uHaloStrength > 0.0) {
          float baseR = 1.0 - uHaloRadius * 0.2; 
          float sizeR = uHaloSize; 
          float sizeG = uHaloSize + 0.005;
          float sizeB = uHaloSize + 0.010;

          float ringR = smoothstep(sizeR, 0.0, abs(sunDot - baseR));
          float ringG = smoothstep(sizeG, 0.0, abs(sunDot - (baseR + 0.005)));
          float ringB = smoothstep(sizeB, 0.0, abs(sunDot - (baseR + 0.010)));

          vec3 haloCol = vec3(ringR, ringG, ringB);
          skyCol += haloCol * uHaloStrength * 0.5 * (1.0 - occlusion * 0.5);
      }
      
      return skyCol;
  }

  vec4 lensflares(vec2 uv, vec2 pos, float ghostingScale, vec2 parallaxShift) {
      vec2 main = uv - pos;
      vec2 uvd = uv * (length(uv));
      
      float ang = atan(main.y, main.x);
      float dist = length(main); 
      dist = pow(dist, 0.1);
      
      float f0 = 1.0 / (length(uv - pos) * 25.0 + 1.0); 
      f0 = pow(f0, 2.0); 
      float star = sin(noise(vec2(sin(ang*2.0+pos.x)*4.0, cos(ang*3.0+pos.y)))*16.0);
      f0 = f0 + f0 * (star * 0.1 + dist * 0.1 + 0.8);
      
      vec2 scaledPos = (pos * ghostingScale) + parallaxShift;
      
      float centerDist = length(scaledPos);
      float distanceFactor = 1.0 + centerDist * 0.5;

      float f2  = max(1.0 / (1.0 + 32.0 * pow(length(uvd + 0.8 * scaledPos), 2.0)), 0.0) * 0.25;
      float f22 = max(1.0 / (1.0 + 32.0 * pow(length(uvd + 0.85 * scaledPos), 2.0)), 0.0) * 0.23;
      float f23 = max(1.0 / (1.0 + 32.0 * pow(length(uvd + 0.9 * scaledPos), 2.0)), 0.0) * 0.21;
      
      vec2 uvx = mix(uv, uvd, -0.5);
      
      float f4  = max(0.01 - pow(length(uvx + 0.4 * scaledPos), 2.4), 0.0) * 6.0;
      float f42 = max(0.01 - pow(length(uvx + 0.45 * scaledPos), 2.4), 0.0) * 5.0;
      float f43 = max(0.01 - pow(length(uvx + 0.5 * scaledPos), 2.4), 0.0) * 3.0;
      
      uvx = mix(uv, uvd, -0.4);
      
      float f5  = max(0.01 - pow(length(uvx + 0.2 * scaledPos), 5.5), 0.0) * 2.0;
      float f52 = max(0.01 - pow(length(uvx + 0.4 * scaledPos), 5.5), 0.0) * 2.0;
      float f53 = max(0.01 - pow(length(uvx + 0.6 * scaledPos), 5.5), 0.0) * 2.0;
      
      uvx = mix(uv, uvd, -0.5);
      
      float f6  = max(0.01 - pow(length(uvx - 0.3 * scaledPos), 1.6), 0.0) * 6.0;
      float f62 = max(0.01 - pow(length(uvx - 0.325 * scaledPos), 1.6), 0.0) * 3.0;
      float f63 = max(0.01 - pow(length(uvx - 0.35 * scaledPos), 1.6), 0.0) * 5.0;
      
      vec3 c = vec3(0.0);
      
      c.r += (f2 + f4 + f5 + f6) * distanceFactor; 
      c.g += (f22 + f42 + f52 + f62) * distanceFactor; 
      c.b += (f23 + f43 + f53 + f63) * distanceFactor;
      c = max(vec3(0.0), c * 1.3 - vec3(length(uvd) * 0.05));
      
      return vec4(c, f0);
  }
  
  vec3 anflares_optimized(vec2 uv, vec2 pos, float streakIntensity) {
      vec2 main = uv - pos;
      float v = smoothstep(0.02, 0.0, abs(main.y));
      float h = smoothstep(1.0, 0.0, abs(main.x) / 1.5); 
      return vec3(v * h) * streakIntensity * 0.8;
  }

  vec3 filmic(vec3 x) {
    vec3 a = max(vec3(0.0), x - vec3(0.004));
    return (a * (6.2 * a + 0.5)) / (a * (6.2 * a + 1.7) + 0.06);
  }
  
  float dither4x4(vec2 position, float brightness) {
    int x = int(mod(position.x, 4.0)); int y = int(mod(position.y, 4.0));
    int index = x + y * 4; float limit = 0.0;
    if (x < 8) {
      if (index == 0) limit = 0.0625; if (index == 1) limit = 0.5625; if (index == 2) limit = 0.1875; if (index == 3) limit = 0.6875;
      if (index == 4) limit = 0.8125; if (index == 5) limit = 0.3125; if (index == 6) limit = 0.9375; if (index == 7) limit = 0.4375;
      if (index == 8) limit = 0.25;   if (index == 9) limit = 0.75;   if (index == 10) limit = 0.125; if (index == 11) limit = 0.625;
      if (index == 12) limit = 1.0;   if (index == 13) limit = 0.5;   if (index == 14) limit = 0.875; if (index == 15) limit = 0.375;
    }
    return brightness < limit ? 0.0 : 1.0;
  }

  vec3 renderScene(vec3 ro, vec3 rd, vec3 sunDir) {
      float t = 0.0; float d = 0.0; float maxDist = 150.0;
      for(int i=0; i<100; i++) { d = map(ro + rd*t); t += d * 0.6; if(d<0.01 || t>maxDist) break; }
      vec3 col = vec3(0.0);
      
      if(t < maxDist) {
          vec3 p = ro + rd*t; 
          vec3 n = getNormal(p); 
          vec3 ref = reflect(rd, n);
          float fresnel = 0.02 + 0.98 * pow(1.0 - max(0.0, dot(n, -rd)), 5.0); 
          
          col = uSssBaseColor * (0.002 + 0.1*max(0.0, dot(n, sunDir)));
          col = mix(col, getSky(ref, sunDir, false), fresnel * 0.95); 
          
          float sss = pow(max(0.0, dot(n, -sunDir)), 2.0) * smoothstep(-0.2, uWaveHeight, p.y);
          col += uSssTipColor * sss * uSssStrength * 3.0; 
          
          if (uEnableReflections > 0.5) {
              float refDot = dot(ref, sunDir);
              float specPower = 1.0 / max(0.0001, uReflectionWidth * uReflectionWidth);
              float specular = pow(max(0.0, refDot), specPower);
              col += uSssTipColor * specular * uReflectionStrength;
          }
          if(uEnableGrid > 0.5) {
              vec2 gridUV = p.xz * 0.5;
              float grid = step(0.97, fract(gridUV.x)) + step(0.97, fract(gridUV.y));
              float fade = smoothstep(50.0, 0.0, t);
              col += uSssTipColor * grid * fade * 2.0;
          }
          float hBlend = smoothstep(maxDist * (1.0 - max(0.001, uHorizonFade)), maxDist, t);
          col = mix(col, getSky(rd, sunDir, true), hBlend);
      } else {
          col = getSky(rd, sunDir, true);
      }
      return col;
  }

  void main() {
    vec2 coord = gl_FragCoord.xy;
    vec2 uv = (coord * 2.0 - uResolution.xy) / uResolution.y;
    
    vec3 ro = vec3(0.0, uCameraHeight, uTime * (uFlySpeed * 2.0 + 1.0));
    
    vec3 ta = ro + vec3(0.0, uCameraTilt, 10.0); 
    vec3 ww = normalize(ta - ro);
    vec3 uu = normalize(cross(ww, vec3(0.0, 1.0, 0.0)));
    vec3 vv = normalize(cross(uu, ww));
    vec3 sunDir = normalize(vec3(uSunPosX, uSunPosY, 1.0)); 
    vec3 rd = normalize(uv.x * uu + uv.y * vv + uFocalLength * ww);
    
    vec3 col = renderScene(ro, rd, sunDir);
    
    if(uEnableFX > 0.5 && uDustStrength > 0.0) {
        vec3 pDust = rd * 8.0; pDust.y -= uTime * 0.3; 
        float specks = smoothstep(0.90, 1.0, noise3D(pDust));
        col += uSssTipColor * specks * uDustStrength;
    }

    if (uEnableFX > 0.5 && uFlareIntensity > 0.0) {
        vec3 sunView = vec3(dot(sunDir, uu), dot(sunDir, vv), dot(sunDir, ww));
        
        if (sunView.z > 0.0) { 
            float focalLength = uFocalLength;
            vec2 sunScreenPos = sunView.xy * focalLength; 
            
            float sunThreshold = 0.99 - uSunSize * 0.03;
            float angularRadius = acos(clamp(sunThreshold, 0.0, 1.0));
            float sunRadiusScreen = tan(angularRadius) * focalLength;
            
            float angleRad = uFlareAngle * PI / 180.0;
            
            vec2 edgeDirection = vec2(cos(angleRad), sin(angleRad));
            vec2 edgeOffset = edgeDirection * sunRadiusScreen;
            
            vec2 flareSource = sunScreenPos + edgeOffset;

            vec2 parallaxShift = uMousePos * 0.15;
            
            vec4 flareData = lensflares(uv, flareSource, uFlareGhosting, parallaxShift);
            vec3 ghosts = flareData.rgb;
            float core = flareData.a;
            
            vec3 streak = anflares_optimized(uv, flareSource, uFlareStreak);
            
            vec3 flareColorBase = vec3(0.643, 0.494, 0.867); 
            vec3 finalFlareColor = mix(flareColorBase, uSssTipColor, 0.7); 

            vec3 finalFlare = max(vec3(0.0), ghosts) * uFlareGhosting + streak;
            finalFlare += vec3(core) * 0.5;
            
            float flareBoost = 1.0;
            if (uStyle < 0.5 || uStyle > 2.5) {
                flareBoost = 1.8;
            }
            if (uStyle > 3.5) {
                flareBoost = 2.5;
            }
            
            col += max(vec3(0.0), finalFlare * uFlareIntensity * flareBoost * finalFlareColor);
        }
    }

    if (uStyle > 0.5 && uStyle < 1.5) {
         float lum = dot(col, vec3(0.299, 0.587, 0.114));
         vec3 mono = vec3(lum);
         mono = smoothstep(0.1, 0.9, mono);
         vec3 tint = mix(vec3(1.0), uHorizonColor, 0.2);
         col = mono * tint;
         
         float grainSpeed = 2.0;
         float grainIntensity = 0.08;
         float grainMean = 0.0;
         float grainVariance = 0.5;
         
         vec2 grainUV = coord / uResolution.xy;
         float t = uTime * grainSpeed;
         float seed = dot(grainUV, vec2(12.9898, 78.233));
         float filmNoise = fract(sin(seed) * 43758.5453 + t);
         filmNoise = gaussian(filmNoise, grainMean, grainVariance * grainVariance);
         
         vec3 grain = vec3(filmNoise) * (1.0 - col);
         col = grainOverlay(col, grain, grainIntensity);
    }
    
    if (uStyle > 1.5 && uStyle < 2.5) {
        float brightness = dot(col, vec3(0.299, 0.587, 0.114));
        float d = dither4x4(gl_FragCoord.xy, brightness * 1.5);
        col = uSssTipColor * d;
    }
    
    if (uStyle > 2.5 && uStyle < 3.5) {
        col = pow(col, vec3(1.2));
        
        float lum = dot(col, vec3(0.299, 0.587, 0.114));
        vec3 pink = vec3(1.0, 0.44, 0.81);
        vec3 cyan = vec3(0.0, 0.8, 1.0);
        vec3 purple = vec3(0.69, 0.49, 0.97);
        
        vec3 synthColor = mix(purple, pink, smoothstep(0.0, 0.5, lum));
        synthColor = mix(synthColor, cyan, smoothstep(0.5, 1.0, lum));
        col = mix(col, synthColor * (lum + 0.2), 0.6);
        
        float scanline = sin(coord.y * 2.0) * 0.5 + 0.5;
        scanline = pow(scanline, 1.5) * 0.15;
        col -= scanline;
        
        vec2 uvNorm = coord / uResolution.xy;
        float aberration = 0.002;
        vec2 dir = uvNorm - 0.5;
        float dist = length(dir);
        col.r = col.r + dist * aberration * 10.0;
        col.b = col.b - dist * aberration * 10.0;
        
        col += lum * pink * 0.1;
    }
    
    if (uStyle > 3.5 && uStyle < 4.5) {
        vec2 uvNorm = coord / uResolution.xy;
        
        float lum = dot(col, vec3(0.299, 0.587, 0.114));
        float bloom = smoothstep(0.3, 1.0, lum);
        col += col * bloom * 0.6;
        
        vec3 warmTint = vec3(1.05, 1.0, 0.95);
        vec3 coolShadows = vec3(0.95, 0.97, 1.05);
        col *= mix(coolShadows, warmTint, lum);
        
        vec3 glowColor = vec3(1.0, 0.98, 0.95);
        col = mix(col, glowColor * lum, bloom * 0.3);
        
        vec2 vigUV = uvNorm - 0.5;
        float softVig = 1.0 - dot(vigUV, vigUV) * 0.5;
        softVig = smoothstep(0.0, 1.0, softVig);
        col *= softVig;
        
        float leak1 = smoothstep(0.7, 0.0, length(uvNorm - vec2(0.1, 0.9)));
        float leak2 = smoothstep(0.6, 0.0, length(uvNorm - vec2(0.9, 0.1)));
        col += vec3(1.0, 0.9, 0.7) * leak1 * 0.15;
        col += vec3(0.9, 0.8, 1.0) * leak2 * 0.1;
        
        float haze = smoothstep(0.0, 0.6, lum);
        col = mix(col, col + vec3(0.1, 0.08, 0.12), haze * 0.2);
        
        float pulse = sin(uTime * 0.5) * 0.5 + 0.5;
        col += col * pulse * 0.05;
        
        col = mix(vec3(0.5), col, 0.9);
    }
    
    if (uStyle > 4.5 && uStyle < 5.5) {
        // PIXELATED / MOSAIC
        // Scale pixel size based on resolution to maintain consistent pixelation
        float basePixelSize = 8.0;
        float resolutionScale = min(uResolution.x, uResolution.y) / 800.0; // Normalize to ~800px baseline
        float pixelSize = basePixelSize * max(1.0, resolutionScale);
        vec2 pixelCoord = floor(coord / pixelSize) * pixelSize;
        
        // Sample from center of each pixel block for consistency
        vec2 pixelUV = (pixelCoord + pixelSize * 0.5) * 2.0 - uResolution.xy;
        pixelUV /= uResolution.y;
        
        vec3 roPixel = vec3(0.0, uCameraHeight, uTime * (uFlySpeed * 2.0 + 1.0));
        vec3 taPixel = roPixel + vec3(0.0, uCameraTilt, 10.0);
        vec3 wwPixel = normalize(taPixel - roPixel);
        vec3 uuPixel = normalize(cross(wwPixel, vec3(0.0, 1.0, 0.0)));
        vec3 vvPixel = normalize(cross(uuPixel, wwPixel));
        vec3 sunDir = normalize(vec3(uSunPosX, uSunPosY, 1.0));
        vec3 rdPixel = normalize(pixelUV.x * uuPixel + pixelUV.y * vvPixel + uFocalLength * wwPixel);
        
        col = renderScene(roPixel, rdPixel, sunDir);
        
        // Add slight color quantization for retro feel
        col = floor(col * 16.0) / 16.0;
        
        // Add pixel border/grid
        vec2 pixelFract = fract(coord / pixelSize);
        float border = step(0.92, max(pixelFract.x, pixelFract.y));
        col *= 1.0 - border * 0.2;
    }
    
    if (uStyle > 5.5 && uStyle < 6.5) {
        // CRT / TV SCREEN
        vec2 uvNorm = coord / uResolution.xy;
        
        // Screen curvature
        vec2 crtUV = uvNorm * 2.0 - 1.0;
        vec2 offset = crtUV.yx / 8.0;
        crtUV = crtUV + crtUV * offset * offset;
        crtUV = crtUV * 0.5 + 0.5;
        
        // RGB phosphor separation
        float separation = 0.003;
        vec3 crtCol;
        crtCol.r = col.r;
        crtCol.g = col.g; 
        crtCol.b = col.b;
        
        // Scanlines
        float scanline = sin(coord.y * 3.0) * 0.5 + 0.5;
        scanline = mix(0.5, 1.0, scanline);
        crtCol *= scanline;
        
        // RGB grid pattern
        float gridX = sin(coord.x * 3.0) * 0.5 + 0.5;
        float gridY = sin(coord.y * 3.0) * 0.5 + 0.5;
        vec3 grid = vec3(
          sin(coord.x * 3.0 + 0.0) * 0.5 + 0.5,
          sin(coord.x * 3.0 + 2.094) * 0.5 + 0.5,
          sin(coord.x * 3.0 + 4.189) * 0.5 + 0.5
        );
        grid = mix(vec3(0.8), vec3(1.0), grid);
        crtCol *= grid;
        
        // Vignette for curved screen
        vec2 vigPos = crtUV * 2.0 - 1.0;
        float vig = 1.0 - dot(vigPos, vigPos) * 0.5;
        vig = smoothstep(0.0, 1.0, vig);
        crtCol *= vig;
        
        // Screen flicker
        float flicker = sin(uTime * 24.0) * 0.01 + 0.99;
        crtCol *= flicker;
        
        col = crtCol;
    }
    
    if (uStyle > 6.5 && uStyle < 7.5) {
        // POSTERIZATION / OIL PAINTING
        // Reduce color depth to create distinct bands
        float levels = 6.0;
        col = floor(col * levels) / levels;
        
        // Add edge detection for cartoon effect
        vec2 uvNorm = coord / uResolution.xy;
        float edgeThreshold = 0.15;
        
        // Sample neighboring pixels for edge detection
        float dx = 2.0 / uResolution.x;
        float dy = 2.0 / uResolution.y;
        
        // Simple edge detection using luminance
        float lum = dot(col, vec3(0.299, 0.587, 0.114));
        
        // Add subtle outline
        vec2 outline = vec2(
          step(0.95, fract(coord.x * 0.5)),
          step(0.95, fract(coord.y * 0.5))
        );
        float edge = max(outline.x, outline.y);
        col = mix(col, col * 0.5, edge * 0.3);
        
        // Boost saturation for vibrant poster look
        vec3 lumCoeff = vec3(0.299, 0.587, 0.114);
        float gray = dot(col, lumCoeff);
        col = mix(vec3(gray), col, 1.3);
    }
    
    if (uStyle > 7.5 && uStyle < 8.5) {
        // ASCII ART
        float charSize = 10.0;
        vec2 charCoord = floor(coord / charSize);
        vec2 charPos = fract(coord / charSize);
        
        // Sample the average brightness of this character cell
        float lum = dot(col, vec3(0.299, 0.587, 0.114));
        
        // Create ASCII-like patterns based on brightness
        float pattern = 0.0;
        
        if (lum > 0.8) {
            // Bright: '#' pattern - filled
            pattern = 1.0;
        } else if (lum > 0.6) {
            // '@' pattern - dense cross-hatch
            float cross1 = step(0.45, charPos.x) * step(charPos.x, 0.55);
            float cross2 = step(0.45, charPos.y) * step(charPos.y, 0.55);
            float diag1 = step(abs(charPos.x - charPos.y), 0.15);
            float diag2 = step(abs(charPos.x - (1.0 - charPos.y)), 0.15);
            pattern = max(max(cross1, cross2), max(diag1, diag2));
        } else if (lum > 0.45) {
            // '%' pattern - diagonal lines
            float diag1 = step(abs(charPos.x - charPos.y), 0.12);
            float diag2 = step(abs(charPos.x - (1.0 - charPos.y)), 0.12);
            pattern = max(diag1, diag2);
        } else if (lum > 0.3) {
            // '+' pattern - cross
            float cross1 = step(0.4, charPos.x) * step(charPos.x, 0.6);
            float cross2 = step(0.4, charPos.y) * step(charPos.y, 0.6);
            pattern = max(cross1, cross2);
        } else if (lum > 0.15) {
            // '.' pattern - dot
            float dot = step(length(charPos - 0.5), 0.2);
            pattern = dot;
        } else {
            // Dark: ' ' - empty
            pattern = 0.0;
        }
        
        // Apply the pattern with green terminal color
        vec3 terminalGreen = vec3(0.0, 1.0, 0.3);
        vec3 terminalBg = vec3(0.0, 0.05, 0.0);
        col = mix(terminalBg, terminalGreen, pattern);
        
        // Add scanline effect
        float scanline = sin(coord.y * 2.0) * 0.1 + 0.9;
        col *= scanline;
        
        // Add slight chromatic aberration for screen effect
        float flicker = sin(uTime * 60.0) * 0.02 + 0.98;
        col *= flicker;
    }

    col = filmic(col);
    col *= 1.0 - length(uv * uVignetteStrength); 

    gl_FragColor = vec4(col, 1.0);
  }
`

const vertexShader = `varying vec2 vUv;void main(){vUv=uv;gl_Position=vec4(position,1.0);}`

// ============ SUBTITLES ============
// Empty array - will be populated with in-game text at key moments
// Each subtitle has: { text: string, delay: number (ms to show before next) }
const subtitles: Array<{ text: string; delay: number }> = []

export function OceanScene() {
  // Detect iOS/mobile for performance optimizations (must be early)
  const isIOS = typeof window !== "undefined" && /iPad|iPhone|iPod/.test(navigator.userAgent)
  const isMobile = typeof window !== "undefined" && /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent)
  
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const rendererRef = useRef<THREE.WebGLRenderer | null>(null)
  const uniformsRef = useRef<Record<string, { value: unknown }> | null>(null)
  const animationIdRef = useRef<number>(0)
  const mousePosRef = useRef(new THREE.Vector2(0, 0))

  const [isPlaying, setIsPlaying] = useState(false)
  const [showDebugModal, setShowDebugModal] = useState(false)
  
  // Debug state for real-time parameter adjustment
  const [debugParams, setDebugParams] = useState({
    cameraHeight: 4.0,
    cameraTilt: -0.1,
    focalLength: 1.5,
    sunPosX: 0.0,
    sunPosY: 0.3,
    sunSize: 0.9,
    sunIntensity: 3.0,
    waveHeight: 0.2,
    waveChoppiness: 2.5,
    speed: 0.2,
    flySpeed: 0.5,
  })
  
  // Update uniforms in real-time when debug params change
  useEffect(() => {
    if (!uniformsRef.current) return
    
    uniformsRef.current.uCameraHeight.value = debugParams.cameraHeight
    uniformsRef.current.uCameraTilt.value = debugParams.cameraTilt
    uniformsRef.current.uFocalLength.value = debugParams.focalLength
    uniformsRef.current.uSunPosX.value = debugParams.sunPosX
    uniformsRef.current.uSunPosY.value = debugParams.sunPosY
    uniformsRef.current.uSunSize.value = debugParams.sunSize
    uniformsRef.current.uSunIntensity.value = debugParams.sunIntensity
    uniformsRef.current.uWaveHeight.value = debugParams.waveHeight
    uniformsRef.current.uWaveChoppiness.value = debugParams.waveChoppiness
    uniformsRef.current.uSpeed.value = debugParams.speed
    uniformsRef.current.uFlySpeed.value = debugParams.flySpeed
    paramsRef.current.flySpeed = debugParams.flySpeed
  }, [debugParams])
  
  const [storyVisible, setStoryVisible] = useState(false)
  const [creditsVisible, setCreditsVisible] = useState(false)
  const [scrollProgress, setScrollProgress] = useState(0)
  const [currentStyle, setCurrentStyle] = useState(5) // Pixelated
  const [currentPreset, setCurrentPreset] = useState<PresetKey>("Twilight")
  
  // Splash screen and audio state
  const [showSplash, setShowSplash] = useState(true)
  const [musicEnabled, setMusicEnabled] = useState(true)
  const audioRef = useRef<HTMLAudioElement | null>(null)
  
  // Initialize audio element
  useEffect(() => {
    if (typeof window !== "undefined" && !audioRef.current) {
      const audio = new Audio("/music/Theme A.mp3")
      audio.loop = true
      audio.volume = 0.3
      audio.preload = "auto"
      // iOS/Safari compatibility
      audio.setAttribute("playsinline", "true")
      audioRef.current = audio
    }
    
    return () => {
      if (audioRef.current) {
        audioRef.current.pause()
        audioRef.current = null
      }
    }
  }, [])
  
  // Handle music enabled/disabled
  useEffect(() => {
    if (!audioRef.current) return
    
    if (musicEnabled && isPlaying) {
      audioRef.current.play().catch((e) => {
        console.log("Audio play failed:", e)
      })
    } else {
      audioRef.current.pause()
    }
  }, [musicEnabled, isPlaying])

  // Fishing game state
  const [fishCaught, setFishCaught] = useState(0)
  const [goldPieces, setGoldPieces] = useState(0)
  const [isFishing, setIsFishing] = useState(false)
  const [markAngle, setMarkAngle] = useState(0)
  const [feedbackState, setFeedbackState] = useState<{ type: "success"; rarity: FishRarity } | { type: "fail" } | null>(null)
  const [showFishPopup, setShowFishPopup] = useState(false)
  // Fish data table
  type FishRarity = "common" | "uncommon" | "rare" | "epic" | "legendary"
  
  // Get color for rarity-based feedback
  const getRarityColor = (rarity: FishRarity): string => {
    switch (rarity) {
      case "common":
        return "rgba(156, 163, 175, 0.6)" // Gray
      case "uncommon":
        return "rgba(34, 197, 94, 0.6)" // Green
      case "rare":
        return "rgba(59, 130, 246, 0.6)" // Blue
      case "epic":
        return "rgba(163, 53, 238, 0.6)" // Purple
      case "legendary":
        return "rgba(255, 215, 0, 0.6)" // Gold
      default:
        return "rgba(255, 255, 255, 0.5)" // White fallback
    }
  }
  
  interface Fish {
    id: string
    name: string
    rarity: FishRarity
    image: string
  }
  
  const fishTable: Fish[] = [
    { id: "1", name: "Brinewhisk Silverfin", rarity: "common", image: "/images/fish-catch.png" },
    { id: "2", name: "Cobalt Mudsnapper", rarity: "common", image: "/images/fish-catch-2.png" },
    { id: "3", name: "Lanternscale Darter", rarity: "common", image: "/images/fish-catch-3.png" },
    { id: "4", name: "Saffron Reefskipper", rarity: "uncommon", image: "/images/fish-catch-4.png" },
    { id: "5", name: "Tanglejaw Spratling", rarity: "uncommon", image: "/images/fish-catch-5.png" },
    { id: "6", name: "Velvet Currentgill", rarity: "uncommon", image: "/images/fish-catch-6.png" },
    { id: "7", name: "Wickerback Driftfish", rarity: "rare", image: "/images/fish-catch-7.png" },
    { id: "8", name: "Copperline Siltpicker", rarity: "rare", image: "/images/fish-catch-8.png" },
    { id: "9", name: "Mistwater Spindleperch", rarity: "epic", image: "/images/fish-catch-9.png" },
    { id: "10", name: "Cragfin Moonrunner", rarity: "epic", image: "/images/fish-catch-10.png" },
    { id: "11", name: "Thornbelly Shallowskulk", rarity: "legendary", image: "/images/fish-catch-11.png" },
    { id: "12", name: "Glimmerhook Tidecarp", rarity: "legendary", image: "/images/fish-catch-12.png" },
  ]
  
  // Weighted probability system for fish rarity
  // Weights determine relative probability of each rarity tier
  const rarityWeights: Record<FishRarity, number> = {
    common: 50,      // 50% chance total (16.67% per common fish)
    uncommon: 25,    // 25% chance total (8.33% per uncommon fish)
    rare: 15,        // 15% chance total (7.5% per rare fish)
    epic: 7,         // 7% chance total (3.5% per epic fish)
    legendary: 3,    // 3% chance total (1.5% per legendary fish)
  }
  
  // Select a random fish based on weighted probabilities
  const selectRandomFish = (): Fish => {
    // Calculate total weight
    const totalWeight = Object.values(rarityWeights).reduce((sum, weight) => sum + weight, 0)
    
    // Generate random number between 0 and totalWeight
    let random = Math.random() * totalWeight
    
    // Select rarity based on weights
    let selectedRarity: FishRarity = "common"
    let cumulativeWeight = 0
    
    for (const [rarity, weight] of Object.entries(rarityWeights) as [FishRarity, number][]) {
      cumulativeWeight += weight
      if (random <= cumulativeWeight) {
        selectedRarity = rarity
        break
      }
    }
    
    // Filter fish by selected rarity and randomly pick one
    const fishOfRarity = fishTable.filter(fish => fish.rarity === selectedRarity)
    return fishOfRarity[Math.floor(Math.random() * fishOfRarity.length)]
  }
  
  const [caughtFish, setCaughtFish] = useState<Fish | null>(null)
  const [currentFish, setCurrentFish] = useState<Fish | null>(null) // Fish selected before minigame
  const [fishTransform, setFishTransform] = useState({ flip: 1, rotation: 0 })
  const fishingTimerRef = useRef<number | null>(null)
  const successWindowsRef = useRef<Array<{ start: number; end: number }>>([])

  
  // Net counts (how many of each type owned)
  const [nets, setNets] = useState({
    basic: 0,
    standard: 0,
    large: 0,
    expert: 0,
    legendary: 0,
  })
  
  // Shop modal state
  const [shopOpen, setShopOpen] = useState(false)
  const [shopNotifications, setShopNotifications] = useState(1) // Set to 0 to hide badge
  
  // Tacklebox modal state
  const [tackleboxOpen, setTackleboxOpen] = useState(false)
  
  // Progress tracking for each net type (0-100)
  const [netProgress, setNetProgress] = useState({
    basic: 0,
    standard: 0,
    large: 0,
    expert: 0,
    legendary: 0,
  })

  const subtitleRef = useRef<HTMLDivElement>(null)
  const subtitleIndexRef = useRef(0)
  const subtitleTimeoutRef = useRef<NodeJS.Timeout | null>(null)
  const currentSplitRef = useRef<SplitText | null>(null)
  const subtitlesPausedRef = useRef(false)

  const paramsRef = useRef({
    activePreset: "Twilight" as PresetKey,
    style: 5,
    enableGrid: true,
    sunPosX: 0.0,
    sunPosY: 0.3,
    sunSize: 0.9,
    sunIntensity: 3.0,
    horizonColor: "#0a0a15",
    enableClouds: true,
    cloudDensity: 0.3,
    cloudSpeed: 0.05,
    cloudColor: "#101018",
    horizonFade: 0.05,
    waveHeight: 0.2,
    waveChoppiness: 2.5,
    speed: 0.2,
    sssBaseColor: "#000005",
    sssTipColor: "#8888aa",
    sssStrength: 4.0,
    enableReflections: true,
    reflectionStrength: 2.5,
    reflectionWidth: 0.015,
    flySpeed: 0.5,
    enableFX: true,
    dustStrength: 1.0,
    flareIntensity: 1.5,
    flareGhosting: 0.8,
    flareStreak: 1.0,
    flareAngle: 140,
    haloStrength: 1.5,
    haloRadius: 0.3,
    haloSize: 0.02,
    grainAmount: 0.0,
    grainScale: 50.0,
    vignetteStrength: 0.65,
  })

  const cleanupSplit = useCallback(() => {
    if (currentSplitRef.current) {
      currentSplitRef.current.revert()
      currentSplitRef.current = null
    }
    if (subtitleRef.current) {
      subtitleRef.current.textContent = ""
    }
  }, [])

  const animateSubtitleIn = useCallback(
    (text: string) => {
      const el = subtitleRef.current
      if (!el) return

      cleanupSplit()

      if (!text) {
        el.textContent = ""
        return
      }

      el.textContent = text

      currentSplitRef.current = new SplitText(el, { type: "chars" })
      const chars = currentSplitRef.current.chars

      gsap.fromTo(
        chars,
        { opacity: 0, filter: "blur(10px)" },
        {
          opacity: 1,
          filter: "blur(0px)",
          duration: 0.6,
          ease: "power2.out",
          stagger: { each: 0.05, from: "start" },
        }
      )
    },
    [cleanupSplit]
  )

  const animateSubtitleOut = useCallback(() => {
    return new Promise<void>((resolve) => {
      if (!currentSplitRef.current || !currentSplitRef.current.chars || currentSplitRef.current.chars.length === 0) {
        resolve()
        return
      }

      const chars = currentSplitRef.current.chars

      gsap.to(chars, {
        opacity: 0,
        filter: "blur(10px)",
        duration: 0.4,
        ease: "power2.in",
        stagger: { each: 0.03, from: "start" },
        onComplete: () => {
          cleanupSplit()
          resolve()
        },
      })
    })
  }, [cleanupSplit])

  const showNextSubtitle = useCallback(async () => {
    if (subtitleTimeoutRef.current) {
      clearTimeout(subtitleTimeoutRef.current)
      subtitleTimeoutRef.current = null
    }

    while (subtitlesPausedRef.current) {
      await new Promise((r) => setTimeout(r, 500))
    }

    // If no subtitles, don't try to show anything
    if (subtitles.length === 0) {
      return
    }

    if (subtitleIndexRef.current >= subtitles.length) {
      subtitleIndexRef.current = 0
    }

    const current = subtitles[subtitleIndexRef.current]

    await animateSubtitleOut()
    await new Promise((r) => setTimeout(r, 200))

    animateSubtitleIn(current.text)

    subtitleIndexRef.current++
    subtitleTimeoutRef.current = setTimeout(showNextSubtitle, current.delay)
  }, [animateSubtitleIn, animateSubtitleOut])

  const updateAllUniforms = useCallback(() => {
    if (!uniformsRef.current) return
    const params = paramsRef.current
    const uniforms = uniformsRef.current

    Object.entries(params).forEach(([k, v]) => {
      if (k === "activePreset") return
      const uName = `u${k.charAt(0).toUpperCase() + k.slice(1)}`
      if (!uniforms[uName]) return
      if (k.includes("Color")) (uniforms[uName].value as THREE.Color).set(v as string)
      else if (k.startsWith("enable")) uniforms[uName].value = v ? 1.0 : 0.0
      else uniforms[uName].value = v
    })
  }, [])

  const applyPreset = useCallback(
    (presetName: PresetKey) => {
      const p = PRESETS[presetName]
      const currentStyle = paramsRef.current.style
      Object.assign(paramsRef.current, p)
      paramsRef.current.style = currentStyle
      paramsRef.current.activePreset = presetName
      updateAllUniforms()
    },
    [updateAllUniforms]
  )

  const updateScrollAnimations = useCallback(() => {
    const scrollHeight = document.documentElement.scrollHeight - window.innerHeight
    const progress = Math.min(1, window.scrollY / scrollHeight)
    setScrollProgress(progress)

    if (!uniformsRef.current) return
    
    // Don't override debug values when debug modal is open
    if (showDebugModal) return

    // Use same camera animation logic for all devices
    const cameraStartHeight = 4.0
    const cameraEndHeight = 1.5
    const cameraTiltStart = -0.1
    const cameraTiltEnd = 2.5

    const easedProgress = progress * progress * (3 - 2 * progress)
    const cameraHeight = cameraStartHeight - (cameraStartHeight - cameraEndHeight) * easedProgress
    uniformsRef.current.uCameraHeight.value = cameraHeight

    const cameraTilt = cameraTiltStart + (cameraTiltEnd - cameraTiltStart) * easedProgress
    uniformsRef.current.uCameraTilt.value = cameraTilt
  }, [showDebugModal])
  
  // Initialize Three.js
  useEffect(() => {
    if (!canvasRef.current) return

    const canvas = canvasRef.current
    const scene = new THREE.Scene()
    const camera = new THREE.OrthographicCamera(-1, 1, 1, -1, 0, 1)
    const renderer = new THREE.WebGLRenderer({ canvas, antialias: false })
    renderer.setSize(window.innerWidth, window.innerHeight)
    
    // Reduce pixel ratio for performance on all platforms
    // This significantly reduces memory usage and GPU load
    const maxPixelRatio = 0.85 // Same for all platforms
    renderer.setPixelRatio(Math.min(window.devicePixelRatio, maxPixelRatio))
    rendererRef.current = renderer

    const params = paramsRef.current
    
    // Adjust field of view for mobile to show more of the scene
    // Larger focal length = wider field of view = shows more of the scene
    // Store focal length multiplier as a uniform so shader can use it
    const focalLengthMultiplier = 1.5 // Use same as desktop
    
    // Get actual drawing buffer size (accounts for pixel ratio reduction)
    // This ensures uResolution matches gl_FragCoord coordinates
    const renderWidth = renderer.domElement.width
    const renderHeight = renderer.domElement.height
    
    const uniforms: Record<string, { value: unknown }> = {
      uTime: { value: 0 },
      uResolution: { value: new THREE.Vector2(renderWidth, renderHeight) },
      uMousePos: { value: mousePosRef.current },
      uStyle: { value: params.style },
      uEnableGrid: { value: params.enableGrid ? 1.0 : 0.0 },
      uEnableClouds: { value: params.enableClouds ? 1.0 : 0.0 },
      uEnableReflections: { value: params.enableReflections ? 1.0 : 0.0 },
      uEnableFX: { value: params.enableFX ? 1.0 : 0.0 },
      uFocalLength: { value: focalLengthMultiplier }, // Add focal length as uniform
      uWaveHeight: { value: params.waveHeight },
      uWaveChoppiness: { value: params.waveChoppiness },
      uSpeed: { value: params.speed },
      uFlySpeed: { value: params.flySpeed },
      uSssStrength: { value: params.sssStrength },
      uSssBaseColor: { value: new THREE.Color(params.sssBaseColor) },
      uSssTipColor: { value: new THREE.Color(params.sssTipColor) },
      uSunSize: { value: params.sunSize },
      uSunIntensity: { value: params.sunIntensity },
      uSunPosX: { value: params.sunPosX },
      uSunPosY: { value: params.sunPosY },
      uReflectionStrength: { value: params.reflectionStrength },
      uReflectionWidth: { value: params.reflectionWidth },
      uCloudDensity: { value: params.cloudDensity },
      uCloudSpeed: { value: params.cloudSpeed },
      uCloudColor: { value: new THREE.Color(params.cloudColor) },
      uHorizonColor: { value: new THREE.Color(params.horizonColor) },
      uHaloStrength: { value: params.haloStrength },
      uHaloRadius: { value: params.haloRadius },
      uHaloSize: { value: params.haloSize },
      uDustStrength: { value: params.dustStrength },
      uHorizonFade: { value: params.horizonFade },
      uVignetteStrength: { value: params.vignetteStrength },
      uGrainAmount: { value: params.grainAmount },
      uGrainScale: { value: params.grainScale },
      uFlareIntensity: { value: params.flareIntensity },
      uFlareGhosting: { value: params.flareGhosting },
      uFlareStreak: { value: params.flareStreak },
      uFlareAngle: { value: params.flareAngle },
      uCameraHeight: { value: 4.0 },
      uCameraTilt: { value: -0.1 },
    }

    uniformsRef.current = uniforms

    const material = new THREE.ShaderMaterial({
      vertexShader,
      fragmentShader,
      uniforms,
    })

    scene.add(new THREE.Mesh(new THREE.PlaneGeometry(2, 2), material))

    const handleResize = () => {
      renderer.setSize(window.innerWidth, window.innerHeight)
      // Re-apply pixel ratio after resize (same for all platforms)
      const maxPixelRatio = 0.85
      renderer.setPixelRatio(Math.min(window.devicePixelRatio, maxPixelRatio))
      // Use actual drawing buffer size to match gl_FragCoord coordinates
      uniforms.uResolution.value = new THREE.Vector2(renderer.domElement.width, renderer.domElement.height)
    }

    const handleMouseMove = (event: MouseEvent) => {
      mousePosRef.current.x = (event.clientX / window.innerWidth) * 2 - 1
      mousePosRef.current.y = (event.clientY / window.innerHeight) * 2 - 1
    }

    window.addEventListener("resize", handleResize)
    document.addEventListener("mousemove", handleMouseMove)

    // Throttle FPS for performance on all platforms
    const targetFPS = 24 // Same for all platforms
    const frameInterval = 1000 / targetFPS
    let lastFrameTime = 0
    let isTabVisible = !document.hidden

    const animate = (t: number) => {
      // Pause animation if tab is not visible
      if (!isTabVisible) {
        animationIdRef.current = requestAnimationFrame(animate)
        return
      }

      const elapsed = t - lastFrameTime
      
      if (elapsed >= frameInterval) {
        const time = t * 0.001
        uniforms.uTime.value = time
        uniforms.uMousePos.value = mousePosRef.current
        uniforms.uFlySpeed.value = paramsRef.current.flySpeed

        renderer.render(scene, camera)
        lastFrameTime = t - (elapsed % frameInterval) // Maintain frame timing accuracy
      }
      
      animationIdRef.current = requestAnimationFrame(animate)
    }

    // Handle tab visibility changes
    const handleVisibilityChange = () => {
      isTabVisible = !document.hidden
      if (isTabVisible && !animationIdRef.current) {
        // Resume animation if tab becomes visible and animation was stopped
        animationIdRef.current = requestAnimationFrame(animate)
      }
    }

    document.addEventListener("visibilitychange", handleVisibilityChange)
    animationIdRef.current = requestAnimationFrame(animate)

    // Start subtitles
    subtitleTimeoutRef.current = setTimeout(showNextSubtitle, 2000)

    return () => {
      cancelAnimationFrame(animationIdRef.current)
      window.removeEventListener("resize", handleResize)
      document.removeEventListener("mousemove", handleMouseMove)
      document.removeEventListener("visibilitychange", handleVisibilityChange)
      if (subtitleTimeoutRef.current) {
        clearTimeout(subtitleTimeoutRef.current)
      }
      renderer.dispose()
    }
  }, [showNextSubtitle])

  // Scroll handler
  useEffect(() => {
    window.addEventListener("scroll", updateScrollAnimations, { passive: true })
    updateScrollAnimations()
    return () => window.removeEventListener("scroll", updateScrollAnimations)
  }, [updateScrollAnimations])

  // Keyboard shortcuts
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.target instanceof HTMLInputElement || e.target instanceof HTMLTextAreaElement) return

      if (e.key === "Escape") {
        setStoryVisible(false)
        setCreditsVisible(false)
        subtitlesPausedRef.current = false
      }
    }

    document.addEventListener("keydown", handleKeyDown)
    return () => document.removeEventListener("keydown", handleKeyDown)
  }, [])

  // Handle style change
  useEffect(() => {
    if (!uniformsRef.current) return
    uniformsRef.current.uStyle.value = currentStyle
    paramsRef.current.style = currentStyle
  }, [currentStyle])

  // Initialize with Twilight preset on mount
  useEffect(() => {
    if (!uniformsRef.current) return
    applyPreset("Twilight")
  }, [])

  // Handle story visibility
  useEffect(() => {
    subtitlesPausedRef.current = storyVisible
  }, [storyVisible])

  // Handle splash screen play button
  const handlePlay = () => {
    setShowSplash(false)
    
    // Play audio on user interaction if music is enabled
    if (musicEnabled && audioRef.current) {
      setIsPlaying(true)
      audioRef.current.play().catch((e) => {
        console.log("Audio play failed:", e)
        setIsPlaying(false)
      })
    } else {
      setIsPlaying(false)
    }
  }

  // Fishing game handler - skill-based timing
  const handleCast = () => {
    if (!isFishing) {
      // Randomly select a fish before the minigame starts using weighted probabilities
      const randomFish = selectRandomFish()
      setCurrentFish(randomFish)
      
      // Start fishing minigame
      setIsFishing(true)
      setMarkAngle(0)
      
      // Generate 1-3 random success windows
      // TODO: Difficulty will be affected by fish rarity in the future
      const numWindows = Math.floor(Math.random() * 3) + 1
      const windows: Array<{ start: number; end: number }> = []
      
      for (let i = 0; i < numWindows; i++) {
        const start = Math.random() * 360
        const width = 20 + Math.random() * 30 // 20-50 degree windows
        windows.push({ start, end: (start + width) % 360 })
      }
      
      successWindowsRef.current = windows
      console.log("[v0] Selected fish:", randomFish.name, "Rarity:", randomFish.rarity)
      console.log("[v0] Generated success windows:", windows)
    }
  }

  const handleReel = () => {
    if (!isFishing) return
    
    // Check if mark is within any success window with tolerance
    const currentAngle = markAngle % 360
    const tolerance = 5 // 5 degree tolerance for better feel
    
    console.log("[v0] Checking angle:", currentAngle.toFixed(1), "against windows:", successWindowsRef.current)
    
    const success = successWindowsRef.current.some(window => {
      // Normalize angles
      const start = window.start % 360
      const end = window.end % 360
      
      // Add tolerance to the window
      const expandedStart = (start - tolerance + 360) % 360
      const expandedEnd = (end + tolerance) % 360
      
      if (expandedEnd > expandedStart) {
        // Normal case: window doesn't cross 0
        return currentAngle >= expandedStart && currentAngle <= expandedEnd
      } else {
        // Wrap-around case: window crosses 0 degrees
        return currentAngle >= expandedStart || currentAngle <= expandedEnd
      }
    })
    
    console.log("[v0] Reel attempt - angle:", currentAngle.toFixed(1), "success:", success)
    
    if (success && currentFish) {
      // Catch the pre-selected fish
      setFishCaught((prev) => prev + 1)
      setCaughtFish(currentFish)
      setFeedbackState({ type: "success", rarity: currentFish.rarity })
      
      // Randomly flip and rotate the fish
      const randomFlip = Math.random() > 0.5 ? 1 : -1
      const randomRotation = (Math.random() * 5 + 5) * (Math.random() > 0.5 ? 1 : -1) // 5-10 degrees, either direction
      setFishTransform({ flip: randomFlip, rotation: randomRotation })
      
      setShowFishPopup(true)
      
      // Hide fish popup after animation
      setTimeout(() => {
        setShowFishPopup(false)
        setCaughtFish(null)
      }, 1500)
    } else {
      setFeedbackState({ type: "fail" })
      // Clear current fish on failure
      setCurrentFish(null)
    }
    
    // Reset fishing state
    setIsFishing(false)
    setMarkAngle(0)
    
    // Clear feedback after animation
    setTimeout(() => {
      setFeedbackState(null)
    }, 600)
  }

  // Animate the fishing timer mark
  useEffect(() => {
    if (!isFishing) return
    
    let animationFrameId: number
    const animate = () => {
      setMarkAngle((prev) => (prev + 2) % 360) // 2 degrees per frame
      animationFrameId = requestAnimationFrame(animate)
    }
    
    animationFrameId = requestAnimationFrame(animate)
    
    return () => cancelAnimationFrame(animationFrameId)
  }, [isFishing])

  // Sell all fish for GP
  const handleSellFish = () => {
    setGoldPieces((prev) => prev + fishCaught)
    setFishCaught(0)
  }

  // Net data
  const netTypes = {
    basic: { name: "Basic Net", cost: 25, fishPerSecond: 0.1 },
    standard: { name: "Standard Net", cost: 250, fishPerSecond: 0.5 },
    large: { name: "Large Net", cost: 1000, fishPerSecond: 2 },
    expert: { name: "Expert Net", cost: 5000, fishPerSecond: 5 },
    legendary: { name: "Legendary Net", cost: 25000, fishPerSecond: 10 },
  }

  // Buy net
  const buyNet = (type: keyof typeof nets) => {
    const cost = netTypes[type].cost
    if (goldPieces >= cost) {
      setGoldPieces((prev) => prev - cost)
      setNets((prev) => ({ ...prev, [type]: prev[type] + 1 }))
    }
  }

  // Calculate total fish per second
  const calculateFishPerSecond = () => {
    let total = 0
    Object.keys(nets).forEach((key) => {
      const type = key as keyof typeof nets
      total += nets[type] * netTypes[type].fishPerSecond
    })
    return total
  }

  // Auto-generate fish from nets
  useEffect(() => {
    const fishPerSecond = calculateFishPerSecond()
    if (fishPerSecond === 0) return

    let accumulatedFish = 0

    const interval = setInterval(() => {
      accumulatedFish += fishPerSecond
      const wholeFish = Math.floor(accumulatedFish)
      if (wholeFish > 0) {
        setFishCaught((prev) => prev + wholeFish)
        accumulatedFish -= wholeFish // Keep the fractional remainder
      }
    }, 1000)

    return () => clearInterval(interval)
  }, [nets])

  // Animate progress bars for each net type
  useEffect(() => {
    const interval = setInterval(() => {
      setNetProgress((prev) => {
        const updated = { ...prev }
        Object.keys(netTypes).forEach((key) => {
          const type = key as keyof typeof nets
          if (nets[type] > 0) {
            const info = netTypes[type]
            const timeToOneFish = 1 / info.fishPerSecond // seconds per fish
            const incrementPerTick = (100 / timeToOneFish) / 10 // 10 ticks per second for smooth animation

            updated[type] = (prev[type] + incrementPerTick) % 100
          }
        })
        return updated
      })
    }, 100) // Update every 100ms for smooth animation

    return () => clearInterval(interval)
  }, [nets])

  const getStyleClass = () => {
    switch (currentStyle) {
      case 1:
        return "style-noir"
      case 2:
        return "style-retro"
      case 3:
        return "style-synthwave"
      case 4:
        return "style-dream"
      case 5:
        return "style-pixelated"
      case 6:
        return "style-crt"
      case 7:
        return "style-posterized"
      case 8:
        return "style-ascii"
      default:
        return "style-standard"
    }
  }

  const getScrollMarkerClass = (type: "past" | "present" | "future") => {
    if (type === "past" && scrollProgress < 0.33) return "active"
    if (type === "present" && scrollProgress >= 0.33 && scrollProgress < 0.66) return "active"
    if (type === "future" && scrollProgress >= 0.66) return "active"
    return ""
  }

  return (
    <div 
      className={getStyleClass()}
    >
      {/* Fish Catch Popup */}
      {showFishPopup && (
        <div
          style={{
            position: "fixed",
            bottom: "120px",
            left: "50%",
            transform: "translateX(-50%)",
            zIndex: 10000,
            pointerEvents: "none",
            animation: "fishPopup 1.5s ease-out forwards",
          }}
        >
          {caughtFish && (
            <>
              <img
                src={caughtFish.image || "/placeholder.svg"}
                alt={caughtFish.name}
                style={{
                  width: "200px",
                  height: "auto",
                  imageRendering: "pixelated",
                  transform: `scaleX(${fishTransform.flip}) rotate(${fishTransform.rotation}deg)`,
                }}
              />
              <div
                style={{
                  marginTop: "8px",
                  fontFamily: "PPNeueBit, monospace",
                  fontSize: "clamp(16px, 1.5vw, 20px)",
                  color: "rgba(255, 255, 255, 0.9)",
                  textAlign: "center",
                  textTransform: "lowercase",
                  letterSpacing: "0.05em",
                }}
              >
                {caughtFish.name}
              </div>
              <div
                style={{
                  marginTop: "4px",
                  fontFamily: "PPNeueBit, monospace",
                  fontSize: "clamp(12px, 1.2vw, 16px)",
                  color: 
                    caughtFish.rarity === "legendary" ? "rgba(255, 215, 0, 0.95)" :
                    caughtFish.rarity === "epic" ? "rgba(163, 53, 238, 0.95)" :
                    caughtFish.rarity === "rare" ? "rgba(59, 130, 246, 0.95)" :
                    caughtFish.rarity === "uncommon" ? "rgba(34, 197, 94, 0.95)" :
                    "rgba(156, 163, 175, 0.95)",
                  textAlign: "center",
                  textTransform: "uppercase",
                  letterSpacing: "0.1em",
                  fontWeight: "bold",
                }}
              >
                {caughtFish.rarity}
              </div>
            </>
          )}
        </div>
      )}

      {/* Feedback Border Overlay */}
      {feedbackState && (
        <div
          style={{
            position: "fixed",
            top: 0,
            left: 0,
            right: 0,
            bottom: 0,
            pointerEvents: "none",
            zIndex: 9999,
            background: feedbackState.type === "success"
              ? `radial-gradient(circle at center, transparent 40%, ${getRarityColor(feedbackState.rarity)} 70%, ${getRarityColor(feedbackState.rarity).replace("0.6", "0.8")} 100%)`
              : "radial-gradient(circle at center, transparent 40%, rgba(255, 50, 50, 0.4) 70%, rgba(255, 50, 50, 0.6) 100%)",
            animation: feedbackState.type === "success"
              ? "flashSuccess 0.6s ease-out"
              : "flashRed 0.6s ease-out, shake 0.5s ease-in-out",
          }}
        />
      )}
      
      <style>{`
        @keyframes fishPopup {
          0% {
            opacity: 0;
            transform: translateX(-50%) translateY(20px);
          }
          20% {
            opacity: 1;
            transform: translateX(-50%) translateY(0);
          }
          80% {
            opacity: 1;
            transform: translateX(-50%) translateY(-20px);
          }
          100% {
            opacity: 0;
            transform: translateX(-50%) translateY(-40px);
          }
        }
        
        @keyframes shake {
          0%, 100% { transform: translateX(0); }
          10%, 30%, 50%, 70%, 90% { transform: translateX(-8px); }
          20%, 40%, 60%, 80% { transform: translateX(8px); }
        }
        
        @keyframes flashSuccess {
          0% { opacity: 1; }
          100% { opacity: 0; }
        }
        
        @keyframes flashRed {
          0% { opacity: 1; }
          100% { opacity: 0; }
        }
      `}</style>
      

      {/* Splash Screen */}
      {showSplash && (
        <div
          style={{
            position: "fixed",
            top: 0,
            left: 0,
            right: 0,
            bottom: 0,
            background: "rgba(0, 0, 0, 0.95)",
            display: "flex",
            flexDirection: "column",
            alignItems: "center",
            justifyContent: "center",
            gap: "48px",
            zIndex: 10000,
          }}
        >
          <div
            style={{
              fontFamily: "PPNeueBit, monospace",
              fontSize: "clamp(32px, 5vw, 64px)",
              color: "rgba(255, 255, 255, 0.9)",
              letterSpacing: "0.1em",
              textTransform: "lowercase",
            }}
          >
            Cederglenn Shores
          </div>

          {/* Music Toggle */}
          <label
            style={{
              display: "flex",
              alignItems: "center",
              gap: "12px",
              fontFamily: "PPNeueBit, monospace",
              fontSize: "clamp(14px, 1.5vw, 18px)",
              color: "rgba(255, 255, 255, 0.7)",
              cursor: "pointer",
              letterSpacing: "0.05em",
              textTransform: "lowercase",
            }}
          >
            <input
              type="checkbox"
              checked={musicEnabled}
              onChange={(e) => setMusicEnabled(e.target.checked)}
              style={{
                width: "20px",
                height: "20px",
                cursor: "pointer",
              }}
            />
            background music
          </label>

          {/* Play Button */}
          <button
            onClick={handlePlay}
            className="pixel-button"
            style={{
              fontSize: "clamp(16px, 1.8vw, 22px)",
            }}
          >
            play
          </button>
        </div>
      )}

      {/* Scroll Container */}
      <div className="scroll-container" />

      {/* Canvas */}
      <canvas ref={canvasRef} className="ocean-canvas" />

      {/* Fish Counter */}
      <div
        style={{
          position: "fixed",
          top: "24px",
          left: "50%",
          transform: "translateX(-50%)",
          fontFamily: "PPNeueBit, monospace",
          fontSize: "clamp(14px, 1.5vw, 18px)",
          color: "rgba(255, 255, 255, 0.7)",
          letterSpacing: "0.05em",
          zIndex: 100,
          pointerEvents: "none",
        }}
      >
        {fishCaught} fish
      </div>

      {/* Shop Button */}
      <button
        onClick={() => setShopOpen(true)}
        className="pixel-button"
        style={{
          position: "fixed",
          top: "24px",
          right: "24px",
          fontSize: "clamp(12px, 1.2vw, 14px)",
          height: "2.5rem",
          lineHeight: "2.5rem",
          padding: "0 1rem",
          zIndex: 100,
        }}
      >
        shop
        {shopNotifications > 0 && (
          <span
            style={{
              position: "absolute",
              top: "-6px",
              right: "-6px",
              background: "rgba(255, 255, 255, 0.95)",
              color: "black",
              fontSize: "10px",
              fontWeight: "bold",
              borderRadius: "50%",
              width: "16px",
              height: "16px",
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
            }}
          >
            {shopNotifications}
          </span>
        )}
      </button>

      {/* Tacklebox Button */}
      <button
        onClick={() => setTackleboxOpen(true)}
        className="pixel-button"
        style={{
          position: "fixed",
          top: "24px",
          left: "24px",
          fontSize: "clamp(12px, 1.2vw, 14px)",
          height: "2.5rem",
          lineHeight: "2.5rem",
          padding: "0 1rem",
          zIndex: 100,
        }}
      >
        tacklebox
      </button>

      {/* Shop Sidepanel - Slides in from right */}
      <div
        onClick={() => setShopOpen(false)}
        style={{
          position: "fixed",
          top: 0,
          left: 0,
          right: 0,
          bottom: 0,
          background: "rgba(0, 0, 0, 0.4)",
          backdropFilter: "blur(8px)",
          zIndex: 500,
          pointerEvents: shopOpen ? "auto" : "none",
          opacity: shopOpen ? 1 : 0,
          transition: "opacity 0.3s ease",
        }}
      >
        <div
          onClick={(e) => e.stopPropagation()}
          style={{
            position: "fixed",
            top: 0,
            right: shopOpen ? 0 : "-400px",
            height: "100vh",
            width: "min(400px, 90vw)",
            fontFamily: "PPNeueBit, monospace",
            fontSize: "clamp(14px, 1.5vw, 18px)",
            color: "rgba(255, 255, 255, 0.8)",
            letterSpacing: "0.05em",
            textTransform: "lowercase",
            padding: "40px 32px",
            borderLeft: "1px solid rgba(255, 255, 255, 0.15)",
            background: "rgba(0, 0, 0, 0.6)",
            overflowY: "auto",
            transition: "right 0.3s ease",
          }}
        >
          <div style={{ marginBottom: "24px", fontSize: "clamp(16px, 1.8vw, 20px)" }}>shop</div>

          {/* GP Display */}
          <div
            style={{
              marginBottom: "16px",
              padding: "16px",
              background: "rgba(255, 255, 255, 0.05)",
              border: "1px solid rgba(255, 255, 255, 0.1)",
              borderRadius: "6px",
            }}
          >
            <div style={{ fontSize: "clamp(12px, 1.3vw, 14px)", color: "rgba(255, 255, 255, 0.5)" }}>
              gold pieces
            </div>
            <div style={{ fontSize: "clamp(18px, 2vw, 22px)", marginTop: "4px" }}>{goldPieces} GP</div>
          </div>

          {/* Sell Fish Button */}
          <button
            onClick={handleSellFish}
            disabled={fishCaught === 0}
            className="pixel-button"
            style={{
              width: "100%",
              marginBottom: "24px",
              fontSize: "clamp(12px, 1.3vw, 14px)",
            }}
          >
            sell all fish ({fishCaught})
          </button>

          {/* Buy Gear Section */}
          <div style={{ marginBottom: "16px", fontSize: "clamp(14px, 1.5vw, 16px)", color: "rgba(255, 255, 255, 0.6)" }}>
            buy gear
          </div>

          {/* Net Cards */}
          <div style={{ display: "flex", flexDirection: "column", gap: "12px" }}>
            {Object.keys(netTypes).map((key) => {
              const type = key as keyof typeof nets
              const info = netTypes[type]
              const canAfford = goldPieces >= info.cost
              const owned = nets[type]

              return (
                <div
                  key={key}
                  style={{
                    background: "rgba(255, 255, 255, 0.05)",
                    border: `1px solid ${canAfford ? "rgba(255, 255, 255, 0.2)" : "rgba(255, 255, 255, 0.1)"}`,
                    borderRadius: "6px",
                    padding: "16px",
                    display: "flex",
                    justifyContent: "space-between",
                    alignItems: "center",
                    transition: "all 0.3s ease",
                  }}
                >
                  <div style={{ flex: 1 }}>
                    <div style={{ fontSize: "clamp(13px, 1.4vw, 15px)", marginBottom: "4px" }}>{info.name}</div>
                    <div
                      style={{
                        fontSize: "clamp(11px, 1.2vw, 13px)",
                        color: "rgba(255, 255, 255, 0.5)",
                      }}
                    >
                      {info.fishPerSecond} fish/sec
                    </div>
                    {owned > 0 && (
                      <div
                        style={{
                          fontSize: "clamp(11px, 1.2vw, 13px)",
                          color: "rgba(100, 200, 100, 0.8)",
                          marginTop: "4px",
                        }}
                      >
                        owned: {owned}
                      </div>
                    )}
                  </div>
                  <button
                    onClick={() => buyNet(type)}
                    disabled={!canAfford}
                    className="pixel-button"
                    style={{
                      fontSize: "clamp(11px, 1.2vw, 13px)",
                      height: "2rem",
                      lineHeight: "2rem",
                      padding: "0 1rem",
                    }}
                  >
                    {info.cost} GP
                  </button>
                </div>
              )
            })}
          </div>
        </div>
      </div>

      {/* Tacklebox Sidepanel - Slides in from left */}
      <div
        onClick={() => setTackleboxOpen(false)}
        style={{
          position: "fixed",
          top: 0,
          left: 0,
          right: 0,
          bottom: 0,
          background: "rgba(0, 0, 0, 0.4)",
          backdropFilter: "blur(8px)",
          zIndex: 500,
          pointerEvents: tackleboxOpen ? "auto" : "none",
          opacity: tackleboxOpen ? 1 : 0,
          transition: "opacity 0.3s ease",
        }}
      >
        <div
          onClick={(e) => e.stopPropagation()}
          style={{
            position: "fixed",
            top: 0,
            left: tackleboxOpen ? 0 : "-400px",
            height: "100vh",
            width: "min(400px, 90vw)",
            fontFamily: "PPNeueBit, monospace",
            fontSize: "clamp(14px, 1.5vw, 18px)",
            color: "rgba(255, 255, 255, 0.8)",
            letterSpacing: "0.05em",
            textTransform: "lowercase",
            padding: "40px 32px",
            borderRight: "1px solid rgba(255, 255, 255, 0.15)",
            background: "rgba(0, 0, 0, 0.6)",
            overflowY: "auto",
            transition: "left 0.3s ease",
          }}
        >
          <div
            style={{
              marginBottom: "24px",
              display: "flex",
              justifyContent: "space-between",
              alignItems: "center",
            }}
          >
            <div style={{ fontSize: "clamp(16px, 1.8vw, 20px)" }}>tacklebox</div>
            <div
              style={{
                fontSize: "clamp(12px, 1.3vw, 14px)",
                color: "rgba(255, 255, 255, 0.6)",
                letterSpacing: "0.05em",
              }}
            >
              {calculateFishPerSecond().toFixed(1)} fish/s
            </div>
          </div>
          
          {/* Net Cards Container */}
          <div
            style={{
              display: "flex",
              flexDirection: "column",
              gap: "16px",
            }}
          >
            {/* Show "No Nets" message if user has no nets */}
            {Object.values(nets).every(count => count === 0) ? (
              <div
                style={{
                  background: "rgba(255, 255, 255, 0.05)",
                  border: "1px solid rgba(255, 255, 255, 0.1)",
                  borderRadius: "6px",
                  padding: "20px",
                  textAlign: "center",
                  color: "rgba(255, 255, 255, 0.5)",
                  fontSize: "clamp(13px, 1.4vw, 15px)",
                }}
              >
                no nets
              </div>
            ) : (
              /* Owned Nets */
              Object.keys(nets).map((key) => {
                const type = key as keyof typeof nets
                const count = nets[type]
                const info = netTypes[type]
                const progress = netProgress[type]

                if (count === 0) return null

                return (
                  <div
                    key={key}
                    style={{
                      position: "relative",
                      overflow: "hidden",
                      border: "1px solid rgba(255, 255, 255, 0.1)",
                      borderRadius: "6px",
                      padding: "20px",
                      transition: "all 0.3s ease",
                    }}
                  >
                    {/* Progress Bar Background */}
                    <div
                      style={{
                        position: "absolute",
                        top: 0,
                        left: 0,
                        height: "100%",
                        width: `${progress}%`,
                        background: "rgba(255, 255, 255, 0.08)",
                        transition: "width 0.1s linear",
                        zIndex: 0,
                      }}
                    />
                    
                    {/* Content */}
                    <div style={{ position: "relative", zIndex: 1 }}>
                      <div
                        style={{
                          fontSize: "clamp(12px, 1.3vw, 14px)",
                          color: "rgba(255, 255, 255, 0.5)",
                          marginBottom: "4px",
                        }}
                      >
                        {info.name} {count > 1 ? `(x${count})` : ""}
                      </div>
                      <div
                        style={{
                          fontSize: "clamp(13px, 1.4vw, 15px)",
                          color: "rgba(255, 255, 255, 0.6)",
                          lineHeight: "1.5",
                        }}
                      >
                        {info.fishPerSecond} fish/sec {count > 1 ? `• total: ${info.fishPerSecond * count} fish/sec` : ""}
                      </div>
                    </div>
                  </div>
                )
              })
            )}
          </div>
        </div>
      </div>

      {/* Fishing Timer - circular indicator */}
      {isFishing && (
        <div
          style={{
            position: "fixed",
            bottom: "120px",
            left: "50%",
            transform: "translateX(-50%)",
            zIndex: 200,
          }}
        >
          <svg width="200" height="200" viewBox="0 0 160 160" style={{ filter: "drop-shadow(0 4px 12px rgba(0, 0, 0, 0.5))" }}>
            <defs>
              <mask id="ring-mask">
                <circle cx="80" cy="80" r="80" fill="white" />
                <circle cx="80" cy="80" r="60" fill="black" />
              </mask>
            </defs>

            {/* Success windows (green segments) */}
            {successWindowsRef.current.map((window, i) => {
              // Convert degrees to radians, adjusting for SVG coordinate system (0° = top, clockwise)
              const startRad = ((window.start - 90) * Math.PI) / 180
              const endRad = ((window.end - 90) * Math.PI) / 180
              const outerRadius = 80
              const innerRadius = 60

              // Calculate outer arc points
              const x1Outer = 80 + Math.cos(startRad) * outerRadius
              const y1Outer = 80 + Math.sin(startRad) * outerRadius
              const x2Outer = 80 + Math.cos(endRad) * outerRadius
              const y2Outer = 80 + Math.sin(endRad) * outerRadius

              // Calculate inner arc points (in reverse order for proper path)
              const x1Inner = 80 + Math.cos(endRad) * innerRadius
              const y1Inner = 80 + Math.sin(endRad) * innerRadius
              const x2Inner = 80 + Math.cos(startRad) * innerRadius
              const y2Inner = 80 + Math.sin(startRad) * innerRadius

              // Determine if arc is > 180 degrees
              const largeArcFlag = (window.end - window.start + 360) % 360 > 180 ? 1 : 0

              return (
                <g key={i}>
                  <path
                    d={`
                      M ${x1Outer} ${y1Outer}
                      A ${outerRadius} ${outerRadius} 0 ${largeArcFlag} 1 ${x2Outer} ${y2Outer}
                      L ${x1Inner} ${y1Inner}
                      A ${innerRadius} ${innerRadius} 0 ${largeArcFlag} 0 ${x2Inner} ${y2Inner}
                      Z
                    `}
                    fill="rgba(255, 255, 255, 0.6)"
                  />
                </g>
              )
            })}

            {/* Rotating mark - calculate endpoint based on angle */}
            <line
              x1="80"
              y1="80"
              x2={80 + Math.cos(((markAngle - 90) * Math.PI) / 180) * 80}
              y2={80 + Math.sin(((markAngle - 90) * Math.PI) / 180) * 80}
              stroke="rgba(255, 255, 255, 0.9)"
              strokeWidth="4"
              strokeLinecap="round"
            />
            <circle
              cx={80 + Math.cos(((markAngle - 90) * Math.PI) / 180) * 70}
              cy={80 + Math.sin(((markAngle - 90) * Math.PI) / 180) * 70}
              r="6"
              fill="rgba(255, 255, 255, 0.95)"
            />

            {/* Ring outline */}
            <circle cx="80" cy="80" r="80" fill="none" stroke="rgba(255, 255, 255, 0.3)" strokeWidth="2" />
            <circle cx="80" cy="80" r="60" fill="none" stroke="rgba(255, 255, 255, 0.3)" strokeWidth="2" />
          </svg>
        </div>
      )}

      {/* Cast/Reel Button */}
      <button
        onClick={isFishing ? handleReel : handleCast}
        className="pixel-button"
        style={{
          position: "fixed",
          bottom: "48px",
          left: "50%",
          transform: "translateX(-50%)",
          fontSize: "clamp(12px, 1.2vw, 14px)",
          height: "2.5rem",
          lineHeight: "2.5rem",
          padding: "0 1.5rem",
          zIndex: 100,
        }}
      >
        {isFishing ? "reel" : "cast"}
      </button>

      {/* Subtitle System */}
      <div className="subtitle-container" style={{ opacity: storyVisible ? 0 : 1, visibility: storyVisible ? "hidden" : "visible" }}>
        <div className="subtitle" ref={subtitleRef} />
      </div>

      {/* Controls Panel - Hidden but functional */}
      <div className="controls-panel" style={{ display: "none" }}>
        <div className="control-group">
          <label>Preset</label>
          <select
            value={currentPreset}
            onChange={(e) => {
              const preset = e.target.value as PresetKey
              setCurrentPreset(preset)
              applyPreset(preset)
            }}
          >
            {Object.keys(PRESETS).map((preset) => (
              <option key={preset} value={preset}>
                {preset}
              </option>
            ))}
          </select>
        </div>
        <div className="control-group">
          <label>Style</label>
          <select value={currentStyle} onChange={(e) => setCurrentStyle(Number(e.target.value))}>
            <option value={0}>Standard (Real)</option>
            <option value={1}>Noir Film</option>
            <option value={2}>Retro Grid</option>
            <option value={3}>Synthwave</option>
            <option value={4}>Dream</option>
            <option value={5}>Pixelated</option>
            <option value={6}>CRT Screen</option>
            <option value={7}>Posterized</option>
            <option value={8}>ASCII Art</option>
          </select>
        </div>
      </div>

      {/* Debug Button */}
      <button
        onClick={() => setShowDebugModal(!showDebugModal)}
        style={{
          position: "fixed",
          bottom: "20px",
          left: "20px",
          width: "40px",
          height: "40px",
          borderRadius: "50%",
          backgroundColor: "rgba(0, 0, 0, 0.6)",
          border: "2px solid rgba(255, 255, 255, 0.3)",
          color: "white",
          fontSize: "20px",
          cursor: "pointer",
          zIndex: 10001,
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          fontFamily: "monospace",
        }}
        title="Debug Panel"
      >
        ⚙
      </button>

      {/* Debug Modal */}
      {showDebugModal && (
        <div
          style={{
            position: "fixed",
            top: 0,
            left: 0,
            right: 0,
            bottom: 0,
            backgroundColor: "rgba(0, 0, 0, 0.5)",
            zIndex: 10000,
            overflowY: "auto",
            padding: "20px",
            color: "white",
            fontFamily: "monospace",
          }}
        >
          <div style={{ maxWidth: "600px", margin: "0 auto" }}>
            <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: "20px" }}>
              <h2 style={{ margin: 0 }}>Debug Panel</h2>
              <button
                onClick={() => setShowDebugModal(false)}
                style={{
                  backgroundColor: "rgba(255, 255, 255, 0.2)",
                  border: "1px solid rgba(255, 255, 255, 0.3)",
                  color: "white",
                  padding: "8px 16px",
                  cursor: "pointer",
                  borderRadius: "4px",
                }}
              >
                Close
              </button>
            </div>

            {/* Camera Controls */}
            <div style={{ marginBottom: "30px" }}>
              <h3 style={{ borderBottom: "1px solid rgba(255, 255, 255, 0.3)", paddingBottom: "10px" }}>Camera</h3>
              
              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Camera Height: {debugParams.cameraHeight.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="0"
                  max="15"
                  step="0.1"
                  value={debugParams.cameraHeight}
                  onChange={(e) => setDebugParams({ ...debugParams, cameraHeight: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>

              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Camera Tilt: {debugParams.cameraTilt.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="-2"
                  max="3"
                  step="0.1"
                  value={debugParams.cameraTilt}
                  onChange={(e) => setDebugParams({ ...debugParams, cameraTilt: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>

              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Focal Length: {debugParams.focalLength.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="0.5"
                  max="5"
                  step="0.1"
                  value={debugParams.focalLength}
                  onChange={(e) => setDebugParams({ ...debugParams, focalLength: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>
            </div>

            {/* Sun Controls */}
            <div style={{ marginBottom: "30px" }}>
              <h3 style={{ borderBottom: "1px solid rgba(255, 255, 255, 0.3)", paddingBottom: "10px" }}>Sun</h3>
              
              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Sun Position X: {debugParams.sunPosX.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="-1"
                  max="1"
                  step="0.01"
                  value={debugParams.sunPosX}
                  onChange={(e) => setDebugParams({ ...debugParams, sunPosX: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>

              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Sun Position Y: {debugParams.sunPosY.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="-0.5"
                  max="1"
                  step="0.01"
                  value={debugParams.sunPosY}
                  onChange={(e) => setDebugParams({ ...debugParams, sunPosY: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>

              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Sun Size: {debugParams.sunSize.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="0"
                  max="5"
                  step="0.1"
                  value={debugParams.sunSize}
                  onChange={(e) => setDebugParams({ ...debugParams, sunSize: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>

              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Sun Intensity: {debugParams.sunIntensity.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="0"
                  max="10"
                  step="0.1"
                  value={debugParams.sunIntensity}
                  onChange={(e) => setDebugParams({ ...debugParams, sunIntensity: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>
            </div>

            {/* Wave Controls */}
            <div style={{ marginBottom: "30px" }}>
              <h3 style={{ borderBottom: "1px solid rgba(255, 255, 255, 0.3)", paddingBottom: "10px" }}>Waves</h3>
              
              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Wave Height: {debugParams.waveHeight.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="0"
                  max="1"
                  step="0.01"
                  value={debugParams.waveHeight}
                  onChange={(e) => setDebugParams({ ...debugParams, waveHeight: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>

              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Wave Choppiness: {debugParams.waveChoppiness.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="0"
                  max="5"
                  step="0.1"
                  value={debugParams.waveChoppiness}
                  onChange={(e) => setDebugParams({ ...debugParams, waveChoppiness: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>

              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Wave Speed: {debugParams.speed.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="0"
                  max="1"
                  step="0.01"
                  value={debugParams.speed}
                  onChange={(e) => setDebugParams({ ...debugParams, speed: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>

              <div style={{ marginBottom: "15px" }}>
                <label style={{ display: "block", marginBottom: "5px" }}>
                  Fly Speed: {debugParams.flySpeed.toFixed(2)}
                </label>
                <input
                  type="range"
                  min="0"
                  max="2"
                  step="0.1"
                  value={debugParams.flySpeed}
                  onChange={(e) => setDebugParams({ ...debugParams, flySpeed: parseFloat(e.target.value) })}
                  style={{ width: "100%" }}
                />
              </div>
            </div>
          </div>
        </div>
      )}

    </div>
  )
}

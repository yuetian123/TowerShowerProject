// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "METAVERSE IMAGINATION/ASE_URPLitLeaves"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_Cutout("Cutout", Range( 0 , 1)) = 0.4
		_AlbedoColor("Albedo Color", Color) = (1,1,1,0)
		_AlbedoLightness("Albedo Lightness", Range( 0 , 5)) = 0
		[Toggle]_ColorVariation("Color Variation", Float) = 1
		_LeafColorvariatoion("Leaf Color variatoion", Range( 0 , 5)) = 0.6
		[NoScaleOffset]_MainTex("Albedo Map", 2D) = "white" {}
		[NoScaleOffset]_NormalMap("Normal Map", 2D) = "bump" {}
		_NormalIntensity("Normal Intensity", Range( -3 , 3)) = 0
		[Toggle]_NormalBackFaceFixBranch("Normal BackFace Fix (Branch)", Float) = 0
		[NoScaleOffset]_MaskMap("Mask Map", 2D) = "white" {}
		_SmoothnessIntensity("Smoothness Intensity", Float) = 1
		_blackColor("blackColor", Color) = (0,0,0,0)
		[Toggle]_UseTranslucent("UseTranslucent", Float) = 0
		_TranslucencyColor("Translucency Color", Color) = (0.5,0.5,0.5,0)
		_TranslucencyPower("Translucency Power", Range( 0 , 40)) = 8
		_TranslucencyRange("Translucency Range", Float) = 1
		_AmbientOcclusion("Ambient Occlusion", Range( 0 , 1)) = 1
		[Toggle]_VertexAo("Vertex Ao", Float) = 0
		_VertexAointensity("Vertex Ao intensity", Range( 0 , 1)) = 0
		[Toggle]_UseWind("UseWind", Float) = 1
		_GlobalWindPower("Global Wind Power", Range( 0 , 5)) = 1
		_WindDirection("Wind Direction", Range( 1.54 , 1.6)) = 1
		_WindPower("Wind Power", Range( 0 , 0.89)) = 1
		_WorldFrequency("World Frequency", Range( 0 , 1)) = 1
		_WindScale("Wind Scale", Range( 0 , 2)) = 1
		_WindAngley("Wind Angle (y)", Range( -100 , 100)) = 20
		_BendAmount("Bend Amount", Range( 0 , 1)) = 1
		[ASEEnd]_BranchBending("Branch Bending", Range( 0 , 10)) = 1
		[HideInInspector] _texcoord( "", 2D ) = "white" {}

		//_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 0.5
		//_TransStrength( "Trans Strength", Range( 0, 50 ) ) = 1
		//_TransNormal( "Trans Normal Distortion", Range( 0, 1 ) ) = 0.5
		//_TransScattering( "Trans Scattering", Range( 1, 50 ) ) = 2
		//_TransDirect( "Trans Direct", Range( 0, 1 ) ) = 0.9
		//_TransAmbient( "Trans Ambient", Range( 0, 1 ) ) = 0.1
		//_TransShadow( "Trans Shadow", Range( 0, 1 ) ) = 0.5
		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
		Cull Off
		AlphaToMask Off
		HLSLINCLUDE
		#pragma target 3.0

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 70401

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_SHADOWCOORDS
			#define ASE_NEEDS_FRAG_WORLD_VIEW_DIR
			#define ASE_NEEDS_FRAG_WORLD_NORMAL
			#define ASE_NEEDS_FRAG_COLOR


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord : TEXCOORD0;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_color : COLOR;
				float4 ase_texcoord8 : TEXCOORD8;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _TranslucencyColor;
			half4 _blackColor;
			float4 _AlbedoColor;
			float _UseWind;
			float _AmbientOcclusion;
			float _VertexAo;
			float _SmoothnessIntensity;
			float _TranslucencyRange;
			float _TranslucencyPower;
			float _UseTranslucent;
			float _NormalIntensity;
			float _NormalBackFaceFixBranch;
			float _AlbedoLightness;
			half _LeafColorvariatoion;
			float _ColorVariation;
			float _BendAmount;
			float _WorldFrequency;
			float _WindScale;
			float _WindPower;
			float _WindAngley;
			float _GlobalWindPower;
			float _WindDirection;
			float _BranchBending;
			float _VertexAointensity;
			float _Cutout;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _MainTex;
			sampler2D _NormalMap;
			sampler2D _MaskMap;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			//https://www.shadertoy.com/view/XdXGW8
			float2 GradientNoiseDir( float2 x )
			{
				const float2 k = float2( 0.3183099, 0.3678794 );
				x = x * k + k.yx;
				return -1.0 + 2.0 * frac( 16.0 * k * frac( x.x * x.y * ( x.x + x.y ) ) );
			}
			
			float GradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 i = floor( p );
				float2 f = frac( p );
				float2 u = f * f * ( 3.0 - 2.0 * f );
				return lerp( lerp( dot( GradientNoiseDir( i + float2( 0.0, 0.0 ) ), f - float2( 0.0, 0.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 0.0 ) ), f - float2( 1.0, 0.0 ) ), u.x ),
						lerp( dot( GradientNoiseDir( i + float2( 0.0, 1.0 ) ), f - float2( 0.0, 1.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 1.0 ) ), f - float2( 1.0, 1.0 ) ), u.x ), u.y );
			}
			
			float4 CalculateContrast( float contrastValue, float4 colorTarget )
			{
				float t = 0.5 * ( 1.0 - contrastValue );
				return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
			}
			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float clampResult255 = clamp( v.vertex.xyz.y , -0.9 , 1.2 );
				float clampResult249 = clamp( v.vertex.xyz.x , -0.3 , 0.3 );
				float clampResult247 = clamp( v.vertex.xyz.z , -1.5 , 1.5 );
				float clampResult257 = clamp( ( clampResult255 * ( ( clampResult249 * ( clampResult247 * ( 0.25 * (v.vertex.xyz).x ) ) ) * 0.002 ) ) , -1.0 , 1.0 );
				float3 temp_cast_0 = (( ( 1.0 * clampResult257 ) * _BranchBending )).xxx;
				float3 ase_worldPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float mulTime10 = _TimeParameters.x * 0.1;
				float simplePerlin2D36 = snoise( (ase_worldPos*1.0 + float3( ( mulTime10 * float2( -0.5,-0.5 ) ) ,  0.0 )).xy*2.0 );
				simplePerlin2D36 = simplePerlin2D36*0.5 + 0.5;
				float gradientNoise19 = GradientNoise((ase_worldPos*1.0 + float3( ( _TimeParameters.x * float2( 0,-0.1 ) ) ,  0.0 )).xy,20.0);
				gradientNoise19 = gradientNoise19*0.5 + 0.5;
				float4 temp_cast_7 = (gradientNoise19).xxxx;
				float4 appendResult61 = (float4(( v.vertex.xyz.y * simplePerlin2D36 ) , ( ( CalculateContrast(_WindAngley,temp_cast_7) * 0.25 ) + ( v.vertex.xyz.y * 1.2 ) ).r , v.vertex.xyz.z , 0.0));
				float4 lerpResult74 = lerp( float4( v.vertex.xyz , 0.0 ) , appendResult61 , v.ase_color.g);
				float2 texCoord52 = v.texcoord.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult54 = (float2(ase_worldPos.x , ase_worldPos.z));
				float2 appendResult47 = (float2(_TimeParameters.x , _TimeParameters.x));
				float simplePerlin2D77 = snoise( ( appendResult54 + appendResult47 )*_WindScale );
				simplePerlin2D77 = simplePerlin2D77*0.5 + 0.5;
				float4 FinalWind182 = ( 100.0 * ( ( _GlobalWindPower * ( float4( v.vertex.xyz , 0.0 ) - lerpResult74 ) ) * ( v.ase_color.g * ( pow( ( texCoord52.y * 0.5 ) , ( 1.0 - _WindPower ) ) * simplePerlin2D77 ) ) ) );
				float3 _Vector1 = float3(0,0,0);
				float4 temp_cast_9 = (_Vector1.y).xxxx;
				float4 transform114 = mul(GetWorldToObjectMatrix(),float4( ase_worldPos , 0.0 ));
				float4 appendResult128 = (float4(( ( v.vertex.xyz.y * cos( ( ( ( transform114.x + transform114.z ) * _WorldFrequency ) + ( _TimeParameters.x ) ) ) ) * _BendAmount ) , _Vector1.y , 0.0 , 0.0));
				float normalizeResult236 = normalize( transform114.y );
				float clampResult238 = clamp( ( normalizeResult236 * 10.0 ) , 0.0 , 10.0 );
				float4 lerpResult130 = lerp( temp_cast_9 , appendResult128 , clampResult238);
				float3 rotatedValue131 = RotateAroundAxis( float3( 0,0,0 ), ( FinalWind182 + lerpResult130 ).xyz, temp_cast_0, _WindDirection );
				float3 TrunkPivotBend169 = (( _UseWind )?( rotatedValue131 ):( float3(0,0,0) ));
				
				o.ase_texcoord7.xy = v.texcoord.xy;
				o.ase_color = v.ase_color;
				o.ase_texcoord8 = v.vertex;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = TrunkPivotBend169;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag ( VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						, half ase_vface : VFACE ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float2 uv_MainTex57 = IN.ase_texcoord7.xy;
				float4 tex2DNode57 = tex2D( _MainTex, uv_MainTex57 );
				float4 break150 = tex2DNode57;
				float4 transform147 = mul(GetObjectToWorldMatrix(),float4( 1,1,1,1 ));
				float dotResult4_g17 = dot( transform147.xy , float2( 12.9898,78.233 ) );
				float lerpResult10_g17 = lerp( 0.9 , 1.15 , frac( ( sin( dotResult4_g17 ) * 43758.55 ) ));
				float4 appendResult152 = (float4(( break150.r * lerpResult10_g17 ) , break150.g , break150.b , 0.0));
				float4 lerpResult155 = lerp( tex2DNode57 , appendResult152 , _LeafColorvariatoion);
				float4 AlbedoOutput204 = ( _AlbedoColor * ( (( _ColorVariation )?( lerpResult155 ):( tex2DNode57 )) * _AlbedoLightness ) );
				
				float2 uv_NormalMap81 = IN.ase_texcoord7.xy;
				float3 unpack81 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap81 ), _NormalIntensity );
				unpack81.z = lerp( 1, unpack81.z, saturate(_NormalIntensity) );
				float3 tex2DNode81 = unpack81;
				float3 appendResult232 = (float3(tex2DNode81.r , ( tex2DNode81.g * ase_vface ) , tex2DNode81.b));
				float3 appendResult162 = (float3(tex2DNode81.r , tex2DNode81.g , ( tex2DNode81.b * ase_vface )));
				float3 NormalOutput207 = (( _NormalBackFaceFixBranch )?( appendResult162 ):( appendResult232 ));
				
				float ase_lightAtten = 0;
				Light ase_lightAtten_mainLight = GetMainLight( ShadowCoords );
				ase_lightAtten = ase_lightAtten_mainLight.distanceAttenuation * ase_lightAtten_mainLight.shadowAttenuation;
				float2 uv_MaskMap44 = IN.ase_texcoord7.xy;
				float4 tex2DNode44 = tex2D( _MaskMap, uv_MaskMap44 );
				float fresnelNdotV35 = dot( WorldNormal, WorldViewDirection );
				float fresnelNode35 = ( 0.0 + 1.0 * pow( 1.0 - fresnelNdotV35, 5.0 ) );
				float4 FresnelBase185 = ( tex2DNode44.b * ( (( _ColorVariation )?( lerpResult155 ):( tex2DNode57 )) * saturate( fresnelNode35 ) ) );
				float3 objToWorldDir7 = normalize( mul( GetObjectToWorldMatrix(), float4( IN.ase_texcoord8.xyz, 0 ) ).xyz );
				float dotResult38 = dot( WorldViewDirection , -( SafeNormalize(_MainLightPosition.xyz) + ( objToWorldDir7 * _TranslucencyRange ) ) );
				float4 TranslucencyBase194 = saturate( ( dotResult38 * _TranslucencyColor ) );
				float4 TranslucencyOutput200 = (( _UseTranslucent )?( ( ase_lightAtten * ( ( IN.ase_color.r * ( _TranslucencyPower * ( FresnelBase185 * ( tex2DNode44.b * TranslucencyBase194 ) ) ) ) * float4( _MainLightColor.rgb , 0.0 ) * _MainLightColor.a ) ) ):( ( _blackColor * tex2DNode57 ) ));
				
				float SmoothnessOutput192 = ( tex2DNode44.a * _SmoothnessIntensity );
				
				float AoBase175 = pow( tex2DNode44.g , _AmbientOcclusion );
				float blendOpSrc95 = IN.ase_color.r;
				float blendOpDest95 = AoBase175;
				float lerpBlendMode95 = lerp(blendOpDest95,( blendOpSrc95 * blendOpDest95 ),_VertexAointensity);
				float AoOutput202 = (( _VertexAo )?( ( saturate( lerpBlendMode95 )) ):( AoBase175 ));
				
				float OpacityMaskOutput213 = tex2DNode57.a;
				
				float3 Albedo = AlbedoOutput204.rgb;
				float3 Normal = NormalOutput207;
				float3 Emission = TranslucencyOutput200.xyz;
				float3 Specular = 0.5;
				float Metallic = 0;
				float Smoothness = SmoothnessOutput192;
				float Occlusion = AoOutput202;
				float Alpha = OpacityMaskOutput213;
				float AlphaClipThreshold = _Cutout;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#endif
				half4 color = UniversalFragmentPBR(
					inputData, 
					Albedo, 
					Metallic, 
					Specular, 
					Smoothness, 
					Occlusion, 
					Emission, 
					Alpha);

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif

				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );

					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif

				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, WorldNormal ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif

				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif

				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif

				return color;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual
			AlphaToMask Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 70401

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_SHADOWCASTER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _TranslucencyColor;
			half4 _blackColor;
			float4 _AlbedoColor;
			float _UseWind;
			float _AmbientOcclusion;
			float _VertexAo;
			float _SmoothnessIntensity;
			float _TranslucencyRange;
			float _TranslucencyPower;
			float _UseTranslucent;
			float _NormalIntensity;
			float _NormalBackFaceFixBranch;
			float _AlbedoLightness;
			half _LeafColorvariatoion;
			float _ColorVariation;
			float _BendAmount;
			float _WorldFrequency;
			float _WindScale;
			float _WindPower;
			float _WindAngley;
			float _GlobalWindPower;
			float _WindDirection;
			float _BranchBending;
			float _VertexAointensity;
			float _Cutout;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _MainTex;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			//https://www.shadertoy.com/view/XdXGW8
			float2 GradientNoiseDir( float2 x )
			{
				const float2 k = float2( 0.3183099, 0.3678794 );
				x = x * k + k.yx;
				return -1.0 + 2.0 * frac( 16.0 * k * frac( x.x * x.y * ( x.x + x.y ) ) );
			}
			
			float GradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 i = floor( p );
				float2 f = frac( p );
				float2 u = f * f * ( 3.0 - 2.0 * f );
				return lerp( lerp( dot( GradientNoiseDir( i + float2( 0.0, 0.0 ) ), f - float2( 0.0, 0.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 0.0 ) ), f - float2( 1.0, 0.0 ) ), u.x ),
						lerp( dot( GradientNoiseDir( i + float2( 0.0, 1.0 ) ), f - float2( 0.0, 1.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 1.0 ) ), f - float2( 1.0, 1.0 ) ), u.x ), u.y );
			}
			
			float4 CalculateContrast( float contrastValue, float4 colorTarget )
			{
				float t = 0.5 * ( 1.0 - contrastValue );
				return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
			}
			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			

			float3 _LightDirection;

			VertexOutput VertexFunction( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float clampResult255 = clamp( v.vertex.xyz.y , -0.9 , 1.2 );
				float clampResult249 = clamp( v.vertex.xyz.x , -0.3 , 0.3 );
				float clampResult247 = clamp( v.vertex.xyz.z , -1.5 , 1.5 );
				float clampResult257 = clamp( ( clampResult255 * ( ( clampResult249 * ( clampResult247 * ( 0.25 * (v.vertex.xyz).x ) ) ) * 0.002 ) ) , -1.0 , 1.0 );
				float3 temp_cast_0 = (( ( 1.0 * clampResult257 ) * _BranchBending )).xxx;
				float3 ase_worldPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float mulTime10 = _TimeParameters.x * 0.1;
				float simplePerlin2D36 = snoise( (ase_worldPos*1.0 + float3( ( mulTime10 * float2( -0.5,-0.5 ) ) ,  0.0 )).xy*2.0 );
				simplePerlin2D36 = simplePerlin2D36*0.5 + 0.5;
				float gradientNoise19 = GradientNoise((ase_worldPos*1.0 + float3( ( _TimeParameters.x * float2( 0,-0.1 ) ) ,  0.0 )).xy,20.0);
				gradientNoise19 = gradientNoise19*0.5 + 0.5;
				float4 temp_cast_7 = (gradientNoise19).xxxx;
				float4 appendResult61 = (float4(( v.vertex.xyz.y * simplePerlin2D36 ) , ( ( CalculateContrast(_WindAngley,temp_cast_7) * 0.25 ) + ( v.vertex.xyz.y * 1.2 ) ).r , v.vertex.xyz.z , 0.0));
				float4 lerpResult74 = lerp( float4( v.vertex.xyz , 0.0 ) , appendResult61 , v.ase_color.g);
				float2 texCoord52 = v.ase_texcoord.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult54 = (float2(ase_worldPos.x , ase_worldPos.z));
				float2 appendResult47 = (float2(_TimeParameters.x , _TimeParameters.x));
				float simplePerlin2D77 = snoise( ( appendResult54 + appendResult47 )*_WindScale );
				simplePerlin2D77 = simplePerlin2D77*0.5 + 0.5;
				float4 FinalWind182 = ( 100.0 * ( ( _GlobalWindPower * ( float4( v.vertex.xyz , 0.0 ) - lerpResult74 ) ) * ( v.ase_color.g * ( pow( ( texCoord52.y * 0.5 ) , ( 1.0 - _WindPower ) ) * simplePerlin2D77 ) ) ) );
				float3 _Vector1 = float3(0,0,0);
				float4 temp_cast_9 = (_Vector1.y).xxxx;
				float4 transform114 = mul(GetWorldToObjectMatrix(),float4( ase_worldPos , 0.0 ));
				float4 appendResult128 = (float4(( ( v.vertex.xyz.y * cos( ( ( ( transform114.x + transform114.z ) * _WorldFrequency ) + ( _TimeParameters.x ) ) ) ) * _BendAmount ) , _Vector1.y , 0.0 , 0.0));
				float normalizeResult236 = normalize( transform114.y );
				float clampResult238 = clamp( ( normalizeResult236 * 10.0 ) , 0.0 , 10.0 );
				float4 lerpResult130 = lerp( temp_cast_9 , appendResult128 , clampResult238);
				float3 rotatedValue131 = RotateAroundAxis( float3( 0,0,0 ), ( FinalWind182 + lerpResult130 ).xyz, temp_cast_0, _WindDirection );
				float3 TrunkPivotBend169 = (( _UseWind )?( rotatedValue131 ):( float3(0,0,0) ));
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = TrunkPivotBend169;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif
				float3 normalWS = TransformObjectToWorldDir(v.ase_normal);

				float4 clipPos = TransformWorldToHClip( ApplyShadowBias( positionWS, normalWS, _LightDirection ) );

				#if UNITY_REVERSED_Z
					clipPos.z = min(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
				#else
					clipPos.z = max(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = clipPos;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );
				
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_MainTex57 = IN.ase_texcoord2.xy;
				float4 tex2DNode57 = tex2D( _MainTex, uv_MainTex57 );
				float OpacityMaskOutput213 = tex2DNode57.a;
				
				float Alpha = OpacityMaskOutput213;
				float AlphaClipThreshold = _Cutout;
				float AlphaClipThresholdShadow = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					#ifdef _ALPHATEST_SHADOW_ON
						clip(Alpha - AlphaClipThresholdShadow);
					#else
						clip(Alpha - AlphaClipThreshold);
					#endif
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif
				return 0;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 70401

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _TranslucencyColor;
			half4 _blackColor;
			float4 _AlbedoColor;
			float _UseWind;
			float _AmbientOcclusion;
			float _VertexAo;
			float _SmoothnessIntensity;
			float _TranslucencyRange;
			float _TranslucencyPower;
			float _UseTranslucent;
			float _NormalIntensity;
			float _NormalBackFaceFixBranch;
			float _AlbedoLightness;
			half _LeafColorvariatoion;
			float _ColorVariation;
			float _BendAmount;
			float _WorldFrequency;
			float _WindScale;
			float _WindPower;
			float _WindAngley;
			float _GlobalWindPower;
			float _WindDirection;
			float _BranchBending;
			float _VertexAointensity;
			float _Cutout;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _MainTex;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			//https://www.shadertoy.com/view/XdXGW8
			float2 GradientNoiseDir( float2 x )
			{
				const float2 k = float2( 0.3183099, 0.3678794 );
				x = x * k + k.yx;
				return -1.0 + 2.0 * frac( 16.0 * k * frac( x.x * x.y * ( x.x + x.y ) ) );
			}
			
			float GradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 i = floor( p );
				float2 f = frac( p );
				float2 u = f * f * ( 3.0 - 2.0 * f );
				return lerp( lerp( dot( GradientNoiseDir( i + float2( 0.0, 0.0 ) ), f - float2( 0.0, 0.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 0.0 ) ), f - float2( 1.0, 0.0 ) ), u.x ),
						lerp( dot( GradientNoiseDir( i + float2( 0.0, 1.0 ) ), f - float2( 0.0, 1.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 1.0 ) ), f - float2( 1.0, 1.0 ) ), u.x ), u.y );
			}
			
			float4 CalculateContrast( float contrastValue, float4 colorTarget )
			{
				float t = 0.5 * ( 1.0 - contrastValue );
				return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
			}
			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float clampResult255 = clamp( v.vertex.xyz.y , -0.9 , 1.2 );
				float clampResult249 = clamp( v.vertex.xyz.x , -0.3 , 0.3 );
				float clampResult247 = clamp( v.vertex.xyz.z , -1.5 , 1.5 );
				float clampResult257 = clamp( ( clampResult255 * ( ( clampResult249 * ( clampResult247 * ( 0.25 * (v.vertex.xyz).x ) ) ) * 0.002 ) ) , -1.0 , 1.0 );
				float3 temp_cast_0 = (( ( 1.0 * clampResult257 ) * _BranchBending )).xxx;
				float3 ase_worldPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float mulTime10 = _TimeParameters.x * 0.1;
				float simplePerlin2D36 = snoise( (ase_worldPos*1.0 + float3( ( mulTime10 * float2( -0.5,-0.5 ) ) ,  0.0 )).xy*2.0 );
				simplePerlin2D36 = simplePerlin2D36*0.5 + 0.5;
				float gradientNoise19 = GradientNoise((ase_worldPos*1.0 + float3( ( _TimeParameters.x * float2( 0,-0.1 ) ) ,  0.0 )).xy,20.0);
				gradientNoise19 = gradientNoise19*0.5 + 0.5;
				float4 temp_cast_7 = (gradientNoise19).xxxx;
				float4 appendResult61 = (float4(( v.vertex.xyz.y * simplePerlin2D36 ) , ( ( CalculateContrast(_WindAngley,temp_cast_7) * 0.25 ) + ( v.vertex.xyz.y * 1.2 ) ).r , v.vertex.xyz.z , 0.0));
				float4 lerpResult74 = lerp( float4( v.vertex.xyz , 0.0 ) , appendResult61 , v.ase_color.g);
				float2 texCoord52 = v.ase_texcoord.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult54 = (float2(ase_worldPos.x , ase_worldPos.z));
				float2 appendResult47 = (float2(_TimeParameters.x , _TimeParameters.x));
				float simplePerlin2D77 = snoise( ( appendResult54 + appendResult47 )*_WindScale );
				simplePerlin2D77 = simplePerlin2D77*0.5 + 0.5;
				float4 FinalWind182 = ( 100.0 * ( ( _GlobalWindPower * ( float4( v.vertex.xyz , 0.0 ) - lerpResult74 ) ) * ( v.ase_color.g * ( pow( ( texCoord52.y * 0.5 ) , ( 1.0 - _WindPower ) ) * simplePerlin2D77 ) ) ) );
				float3 _Vector1 = float3(0,0,0);
				float4 temp_cast_9 = (_Vector1.y).xxxx;
				float4 transform114 = mul(GetWorldToObjectMatrix(),float4( ase_worldPos , 0.0 ));
				float4 appendResult128 = (float4(( ( v.vertex.xyz.y * cos( ( ( ( transform114.x + transform114.z ) * _WorldFrequency ) + ( _TimeParameters.x ) ) ) ) * _BendAmount ) , _Vector1.y , 0.0 , 0.0));
				float normalizeResult236 = normalize( transform114.y );
				float clampResult238 = clamp( ( normalizeResult236 * 10.0 ) , 0.0 , 10.0 );
				float4 lerpResult130 = lerp( temp_cast_9 , appendResult128 , clampResult238);
				float3 rotatedValue131 = RotateAroundAxis( float3( 0,0,0 ), ( FinalWind182 + lerpResult130 ).xyz, temp_cast_0, _WindDirection );
				float3 TrunkPivotBend169 = (( _UseWind )?( rotatedValue131 ):( float3(0,0,0) ));
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = TrunkPivotBend169;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_MainTex57 = IN.ase_texcoord2.xy;
				float4 tex2DNode57 = tex2D( _MainTex, uv_MainTex57 );
				float OpacityMaskOutput213 = tex2DNode57.a;
				
				float Alpha = OpacityMaskOutput213;
				float AlphaClipThreshold = _Cutout;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif
				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "Meta"
			Tags { "LightMode"="Meta" }

			Cull Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 70401

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_SHADOWCOORDS
			#define ASE_NEEDS_VERT_NORMAL
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _SHADOWS_SOFT


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _TranslucencyColor;
			half4 _blackColor;
			float4 _AlbedoColor;
			float _UseWind;
			float _AmbientOcclusion;
			float _VertexAo;
			float _SmoothnessIntensity;
			float _TranslucencyRange;
			float _TranslucencyPower;
			float _UseTranslucent;
			float _NormalIntensity;
			float _NormalBackFaceFixBranch;
			float _AlbedoLightness;
			half _LeafColorvariatoion;
			float _ColorVariation;
			float _BendAmount;
			float _WorldFrequency;
			float _WindScale;
			float _WindPower;
			float _WindAngley;
			float _GlobalWindPower;
			float _WindDirection;
			float _BranchBending;
			float _VertexAointensity;
			float _Cutout;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _MainTex;
			sampler2D _MaskMap;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			//https://www.shadertoy.com/view/XdXGW8
			float2 GradientNoiseDir( float2 x )
			{
				const float2 k = float2( 0.3183099, 0.3678794 );
				x = x * k + k.yx;
				return -1.0 + 2.0 * frac( 16.0 * k * frac( x.x * x.y * ( x.x + x.y ) ) );
			}
			
			float GradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 i = floor( p );
				float2 f = frac( p );
				float2 u = f * f * ( 3.0 - 2.0 * f );
				return lerp( lerp( dot( GradientNoiseDir( i + float2( 0.0, 0.0 ) ), f - float2( 0.0, 0.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 0.0 ) ), f - float2( 1.0, 0.0 ) ), u.x ),
						lerp( dot( GradientNoiseDir( i + float2( 0.0, 1.0 ) ), f - float2( 0.0, 1.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 1.0 ) ), f - float2( 1.0, 1.0 ) ), u.x ), u.y );
			}
			
			float4 CalculateContrast( float contrastValue, float4 colorTarget )
			{
				float t = 0.5 * ( 1.0 - contrastValue );
				return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
			}
			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float clampResult255 = clamp( v.vertex.xyz.y , -0.9 , 1.2 );
				float clampResult249 = clamp( v.vertex.xyz.x , -0.3 , 0.3 );
				float clampResult247 = clamp( v.vertex.xyz.z , -1.5 , 1.5 );
				float clampResult257 = clamp( ( clampResult255 * ( ( clampResult249 * ( clampResult247 * ( 0.25 * (v.vertex.xyz).x ) ) ) * 0.002 ) ) , -1.0 , 1.0 );
				float3 temp_cast_0 = (( ( 1.0 * clampResult257 ) * _BranchBending )).xxx;
				float3 ase_worldPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float mulTime10 = _TimeParameters.x * 0.1;
				float simplePerlin2D36 = snoise( (ase_worldPos*1.0 + float3( ( mulTime10 * float2( -0.5,-0.5 ) ) ,  0.0 )).xy*2.0 );
				simplePerlin2D36 = simplePerlin2D36*0.5 + 0.5;
				float gradientNoise19 = GradientNoise((ase_worldPos*1.0 + float3( ( _TimeParameters.x * float2( 0,-0.1 ) ) ,  0.0 )).xy,20.0);
				gradientNoise19 = gradientNoise19*0.5 + 0.5;
				float4 temp_cast_7 = (gradientNoise19).xxxx;
				float4 appendResult61 = (float4(( v.vertex.xyz.y * simplePerlin2D36 ) , ( ( CalculateContrast(_WindAngley,temp_cast_7) * 0.25 ) + ( v.vertex.xyz.y * 1.2 ) ).r , v.vertex.xyz.z , 0.0));
				float4 lerpResult74 = lerp( float4( v.vertex.xyz , 0.0 ) , appendResult61 , v.ase_color.g);
				float2 texCoord52 = v.ase_texcoord.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult54 = (float2(ase_worldPos.x , ase_worldPos.z));
				float2 appendResult47 = (float2(_TimeParameters.x , _TimeParameters.x));
				float simplePerlin2D77 = snoise( ( appendResult54 + appendResult47 )*_WindScale );
				simplePerlin2D77 = simplePerlin2D77*0.5 + 0.5;
				float4 FinalWind182 = ( 100.0 * ( ( _GlobalWindPower * ( float4( v.vertex.xyz , 0.0 ) - lerpResult74 ) ) * ( v.ase_color.g * ( pow( ( texCoord52.y * 0.5 ) , ( 1.0 - _WindPower ) ) * simplePerlin2D77 ) ) ) );
				float3 _Vector1 = float3(0,0,0);
				float4 temp_cast_9 = (_Vector1.y).xxxx;
				float4 transform114 = mul(GetWorldToObjectMatrix(),float4( ase_worldPos , 0.0 ));
				float4 appendResult128 = (float4(( ( v.vertex.xyz.y * cos( ( ( ( transform114.x + transform114.z ) * _WorldFrequency ) + ( _TimeParameters.x ) ) ) ) * _BendAmount ) , _Vector1.y , 0.0 , 0.0));
				float normalizeResult236 = normalize( transform114.y );
				float clampResult238 = clamp( ( normalizeResult236 * 10.0 ) , 0.0 , 10.0 );
				float4 lerpResult130 = lerp( temp_cast_9 , appendResult128 , clampResult238);
				float3 rotatedValue131 = RotateAroundAxis( float3( 0,0,0 ), ( FinalWind182 + lerpResult130 ).xyz, temp_cast_0, _WindDirection );
				float3 TrunkPivotBend169 = (( _UseWind )?( rotatedValue131 ):( float3(0,0,0) ));
				
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord3.xyz = ase_worldNormal;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_color = v.ase_color;
				o.ase_texcoord4 = v.vertex;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				o.ase_texcoord3.w = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = TrunkPivotBend169;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = MetaVertexPosition( v.vertex, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST, unity_DynamicLightmapST );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.texcoord1 = v.texcoord1;
				o.texcoord2 = v.texcoord2;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.texcoord2 = patch[0].texcoord2 * bary.x + patch[1].texcoord2 * bary.y + patch[2].texcoord2 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_MainTex57 = IN.ase_texcoord2.xy;
				float4 tex2DNode57 = tex2D( _MainTex, uv_MainTex57 );
				float4 break150 = tex2DNode57;
				float4 transform147 = mul(GetObjectToWorldMatrix(),float4( 1,1,1,1 ));
				float dotResult4_g17 = dot( transform147.xy , float2( 12.9898,78.233 ) );
				float lerpResult10_g17 = lerp( 0.9 , 1.15 , frac( ( sin( dotResult4_g17 ) * 43758.55 ) ));
				float4 appendResult152 = (float4(( break150.r * lerpResult10_g17 ) , break150.g , break150.b , 0.0));
				float4 lerpResult155 = lerp( tex2DNode57 , appendResult152 , _LeafColorvariatoion);
				float4 AlbedoOutput204 = ( _AlbedoColor * ( (( _ColorVariation )?( lerpResult155 ):( tex2DNode57 )) * _AlbedoLightness ) );
				
				float ase_lightAtten = 0;
				Light ase_lightAtten_mainLight = GetMainLight( ShadowCoords );
				ase_lightAtten = ase_lightAtten_mainLight.distanceAttenuation * ase_lightAtten_mainLight.shadowAttenuation;
				float2 uv_MaskMap44 = IN.ase_texcoord2.xy;
				float4 tex2DNode44 = tex2D( _MaskMap, uv_MaskMap44 );
				float3 ase_worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				ase_worldViewDir = normalize(ase_worldViewDir);
				float3 ase_worldNormal = IN.ase_texcoord3.xyz;
				float fresnelNdotV35 = dot( ase_worldNormal, ase_worldViewDir );
				float fresnelNode35 = ( 0.0 + 1.0 * pow( 1.0 - fresnelNdotV35, 5.0 ) );
				float4 FresnelBase185 = ( tex2DNode44.b * ( (( _ColorVariation )?( lerpResult155 ):( tex2DNode57 )) * saturate( fresnelNode35 ) ) );
				ase_worldViewDir = SafeNormalize( ase_worldViewDir );
				float3 objToWorldDir7 = normalize( mul( GetObjectToWorldMatrix(), float4( IN.ase_texcoord4.xyz, 0 ) ).xyz );
				float dotResult38 = dot( ase_worldViewDir , -( SafeNormalize(_MainLightPosition.xyz) + ( objToWorldDir7 * _TranslucencyRange ) ) );
				float4 TranslucencyBase194 = saturate( ( dotResult38 * _TranslucencyColor ) );
				float4 TranslucencyOutput200 = (( _UseTranslucent )?( ( ase_lightAtten * ( ( IN.ase_color.r * ( _TranslucencyPower * ( FresnelBase185 * ( tex2DNode44.b * TranslucencyBase194 ) ) ) ) * float4( _MainLightColor.rgb , 0.0 ) * _MainLightColor.a ) ) ):( ( _blackColor * tex2DNode57 ) ));
				
				float OpacityMaskOutput213 = tex2DNode57.a;
				
				
				float3 Albedo = AlbedoOutput204.rgb;
				float3 Emission = TranslucencyOutput200.xyz;
				float Alpha = OpacityMaskOutput213;
				float AlphaClipThreshold = _Cutout;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = Albedo;
				metaInput.Emission = Emission;
				
				return MetaFragment(metaInput);
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "Universal2D"
			Tags { "LightMode"="Universal2D" }

			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 70401

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_2D

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#define ASE_NEEDS_VERT_POSITION


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _TranslucencyColor;
			half4 _blackColor;
			float4 _AlbedoColor;
			float _UseWind;
			float _AmbientOcclusion;
			float _VertexAo;
			float _SmoothnessIntensity;
			float _TranslucencyRange;
			float _TranslucencyPower;
			float _UseTranslucent;
			float _NormalIntensity;
			float _NormalBackFaceFixBranch;
			float _AlbedoLightness;
			half _LeafColorvariatoion;
			float _ColorVariation;
			float _BendAmount;
			float _WorldFrequency;
			float _WindScale;
			float _WindPower;
			float _WindAngley;
			float _GlobalWindPower;
			float _WindDirection;
			float _BranchBending;
			float _VertexAointensity;
			float _Cutout;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _MainTex;


			float3 mod2D289( float3 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float2 mod2D289( float2 x ) { return x - floor( x * ( 1.0 / 289.0 ) ) * 289.0; }
			float3 permute( float3 x ) { return mod2D289( ( ( x * 34.0 ) + 1.0 ) * x ); }
			float snoise( float2 v )
			{
				const float4 C = float4( 0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439 );
				float2 i = floor( v + dot( v, C.yy ) );
				float2 x0 = v - i + dot( i, C.xx );
				float2 i1;
				i1 = ( x0.x > x0.y ) ? float2( 1.0, 0.0 ) : float2( 0.0, 1.0 );
				float4 x12 = x0.xyxy + C.xxzz;
				x12.xy -= i1;
				i = mod2D289( i );
				float3 p = permute( permute( i.y + float3( 0.0, i1.y, 1.0 ) ) + i.x + float3( 0.0, i1.x, 1.0 ) );
				float3 m = max( 0.5 - float3( dot( x0, x0 ), dot( x12.xy, x12.xy ), dot( x12.zw, x12.zw ) ), 0.0 );
				m = m * m;
				m = m * m;
				float3 x = 2.0 * frac( p * C.www ) - 1.0;
				float3 h = abs( x ) - 0.5;
				float3 ox = floor( x + 0.5 );
				float3 a0 = x - ox;
				m *= 1.79284291400159 - 0.85373472095314 * ( a0 * a0 + h * h );
				float3 g;
				g.x = a0.x * x0.x + h.x * x0.y;
				g.yz = a0.yz * x12.xz + h.yz * x12.yw;
				return 130.0 * dot( m, g );
			}
			
			//https://www.shadertoy.com/view/XdXGW8
			float2 GradientNoiseDir( float2 x )
			{
				const float2 k = float2( 0.3183099, 0.3678794 );
				x = x * k + k.yx;
				return -1.0 + 2.0 * frac( 16.0 * k * frac( x.x * x.y * ( x.x + x.y ) ) );
			}
			
			float GradientNoise( float2 UV, float Scale )
			{
				float2 p = UV * Scale;
				float2 i = floor( p );
				float2 f = frac( p );
				float2 u = f * f * ( 3.0 - 2.0 * f );
				return lerp( lerp( dot( GradientNoiseDir( i + float2( 0.0, 0.0 ) ), f - float2( 0.0, 0.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 0.0 ) ), f - float2( 1.0, 0.0 ) ), u.x ),
						lerp( dot( GradientNoiseDir( i + float2( 0.0, 1.0 ) ), f - float2( 0.0, 1.0 ) ),
						dot( GradientNoiseDir( i + float2( 1.0, 1.0 ) ), f - float2( 1.0, 1.0 ) ), u.x ), u.y );
			}
			
			float4 CalculateContrast( float contrastValue, float4 colorTarget )
			{
				float t = 0.5 * ( 1.0 - contrastValue );
				return mul( float4x4( contrastValue,0,0,t, 0,contrastValue,0,t, 0,0,contrastValue,t, 0,0,0,1 ), colorTarget );
			}
			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float clampResult255 = clamp( v.vertex.xyz.y , -0.9 , 1.2 );
				float clampResult249 = clamp( v.vertex.xyz.x , -0.3 , 0.3 );
				float clampResult247 = clamp( v.vertex.xyz.z , -1.5 , 1.5 );
				float clampResult257 = clamp( ( clampResult255 * ( ( clampResult249 * ( clampResult247 * ( 0.25 * (v.vertex.xyz).x ) ) ) * 0.002 ) ) , -1.0 , 1.0 );
				float3 temp_cast_0 = (( ( 1.0 * clampResult257 ) * _BranchBending )).xxx;
				float3 ase_worldPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float mulTime10 = _TimeParameters.x * 0.1;
				float simplePerlin2D36 = snoise( (ase_worldPos*1.0 + float3( ( mulTime10 * float2( -0.5,-0.5 ) ) ,  0.0 )).xy*2.0 );
				simplePerlin2D36 = simplePerlin2D36*0.5 + 0.5;
				float gradientNoise19 = GradientNoise((ase_worldPos*1.0 + float3( ( _TimeParameters.x * float2( 0,-0.1 ) ) ,  0.0 )).xy,20.0);
				gradientNoise19 = gradientNoise19*0.5 + 0.5;
				float4 temp_cast_7 = (gradientNoise19).xxxx;
				float4 appendResult61 = (float4(( v.vertex.xyz.y * simplePerlin2D36 ) , ( ( CalculateContrast(_WindAngley,temp_cast_7) * 0.25 ) + ( v.vertex.xyz.y * 1.2 ) ).r , v.vertex.xyz.z , 0.0));
				float4 lerpResult74 = lerp( float4( v.vertex.xyz , 0.0 ) , appendResult61 , v.ase_color.g);
				float2 texCoord52 = v.ase_texcoord.xy * float2( 1,1 ) + float2( 0,0 );
				float2 appendResult54 = (float2(ase_worldPos.x , ase_worldPos.z));
				float2 appendResult47 = (float2(_TimeParameters.x , _TimeParameters.x));
				float simplePerlin2D77 = snoise( ( appendResult54 + appendResult47 )*_WindScale );
				simplePerlin2D77 = simplePerlin2D77*0.5 + 0.5;
				float4 FinalWind182 = ( 100.0 * ( ( _GlobalWindPower * ( float4( v.vertex.xyz , 0.0 ) - lerpResult74 ) ) * ( v.ase_color.g * ( pow( ( texCoord52.y * 0.5 ) , ( 1.0 - _WindPower ) ) * simplePerlin2D77 ) ) ) );
				float3 _Vector1 = float3(0,0,0);
				float4 temp_cast_9 = (_Vector1.y).xxxx;
				float4 transform114 = mul(GetWorldToObjectMatrix(),float4( ase_worldPos , 0.0 ));
				float4 appendResult128 = (float4(( ( v.vertex.xyz.y * cos( ( ( ( transform114.x + transform114.z ) * _WorldFrequency ) + ( _TimeParameters.x ) ) ) ) * _BendAmount ) , _Vector1.y , 0.0 , 0.0));
				float normalizeResult236 = normalize( transform114.y );
				float clampResult238 = clamp( ( normalizeResult236 * 10.0 ) , 0.0 , 10.0 );
				float4 lerpResult130 = lerp( temp_cast_9 , appendResult128 , clampResult238);
				float3 rotatedValue131 = RotateAroundAxis( float3( 0,0,0 ), ( FinalWind182 + lerpResult130 ).xyz, temp_cast_0, _WindDirection );
				float3 TrunkPivotBend169 = (( _UseWind )?( rotatedValue131 ):( float3(0,0,0) ));
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = TrunkPivotBend169;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_MainTex57 = IN.ase_texcoord2.xy;
				float4 tex2DNode57 = tex2D( _MainTex, uv_MainTex57 );
				float4 break150 = tex2DNode57;
				float4 transform147 = mul(GetObjectToWorldMatrix(),float4( 1,1,1,1 ));
				float dotResult4_g17 = dot( transform147.xy , float2( 12.9898,78.233 ) );
				float lerpResult10_g17 = lerp( 0.9 , 1.15 , frac( ( sin( dotResult4_g17 ) * 43758.55 ) ));
				float4 appendResult152 = (float4(( break150.r * lerpResult10_g17 ) , break150.g , break150.b , 0.0));
				float4 lerpResult155 = lerp( tex2DNode57 , appendResult152 , _LeafColorvariatoion);
				float4 AlbedoOutput204 = ( _AlbedoColor * ( (( _ColorVariation )?( lerpResult155 ):( tex2DNode57 )) * _AlbedoLightness ) );
				
				float OpacityMaskOutput213 = tex2DNode57.a;
				
				
				float3 Albedo = AlbedoOutput204.rgb;
				float Alpha = OpacityMaskOutput213;
				float AlphaClipThreshold = _Cutout;

				half4 color = half4( Albedo, Alpha );

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				return color;
			}
			ENDHLSL
		}
		
	}
	/*ase_lod*/
	CustomEditor "UnityEditor.ShaderGraph.PBRMasterGUI"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=18909
2560;0;2560;1379;4782.605;1031.143;1.702304;True;True
Node;AmplifyShaderEditor.CommentaryNode;1;-4694.708,790.7449;Inherit;False;3074.063;810.8815;;32;12;197;198;97;82;86;174;74;173;172;65;61;171;55;217;34;37;36;29;28;24;2;21;19;18;14;16;9;8;10;4;3;Vertex Wind_Layer A;0,0.7931032,1,1;0;0
Node;AmplifyShaderEditor.SimpleTimeNode;3;-4283.109,887.0696;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;4;-4347.783,984.1605;Inherit;False;Constant;_EdgeFlutterFrequency;Edge Flutter Frequency;14;0;Create;True;0;0;0;True;0;False;0,-0.1;0,-0.1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.WorldPosInputsNode;12;-4601.499,1163.395;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.Vector2Node;9;-4293.065,1402.975;Inherit;False;Constant;_Vector0;Vector 0;12;0;Create;True;0;0;0;True;0;False;-0.5,-0.5;0.5,0.5;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;8;-4102.368,943.0771;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleTimeNode;10;-4284.505,1302.604;Inherit;False;1;0;FLOAT;0.1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;14;-4102.831,1362.288;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;16;-3948.064,1010.924;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;19;-3721.512,897.2493;Inherit;True;Gradient;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;20;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;18;-3747.404,1143.68;Inherit;False;Property;_WindAngley;Wind Angle (y);25;0;Create;True;0;0;0;True;0;False;20;100;-100;100;0;1;FLOAT;0
Node;AmplifyShaderEditor.ScaleAndOffsetNode;21;-3957.734,1146.931;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT;1;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;29;-3708.842,1317.859;Inherit;False;Constant;_FlutterFrequency;Flutter Frequency;21;0;Create;True;0;0;0;True;0;False;2;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleContrastOpNode;24;-3461.12,975.6795;Inherit;True;2;1;COLOR;0,0,0,0;False;0;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.PosVertexDataNode;2;-3217.568,1312.574;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WireNode;28;-3761.226,1236.964;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;31;-3043.105,1680.361;Inherit;False;1426.626;711.0196;;14;101;84;76;77;63;66;67;62;54;52;47;50;53;39;Vertex Wind_Layer B;1,1,1,1;0;0
Node;AmplifyShaderEditor.WireNode;284;-4177.026,1802.67;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ScaleNode;34;-2986.154,1403.495;Inherit;False;1.2;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;239;-4465.501,4083.125;Inherit;False;1970.539;923.8995;Comment;16;261;260;259;258;257;256;253;252;251;250;248;246;245;243;242;240;Branch Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;112;-4355.556,3102.126;Inherit;False;3032.439;810.6194;Comment;24;128;134;129;184;130;126;124;123;122;120;119;118;117;116;115;114;169;131;235;236;237;238;285;286;Trunk Pivot Bend;1,1,1,1;0;0
Node;AmplifyShaderEditor.ScaleNode;37;-3189.014,978.3224;Inherit;False;0.25;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;36;-3461.647,1238.095;Inherit;True;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;39;-2985.151,2253.197;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;283;-4217.854,1850.306;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;55;-2816.313,1190.833;Inherit;True;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;217;-2827.434,967.0376;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;52;-2954.288,1730.361;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WorldToObjectTransfNode;114;-4289.592,3389.658;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;54;-2708.154,2170.196;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;240;-4357.15,4541.654;Inherit;False;318;280;Comment;1;241;;1,1,1,1;0;0
Node;AmplifyShaderEditor.PosVertexDataNode;262;-4763.665,4128.854;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WireNode;171;-2959.86,932.6373;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;50;-2783.561,1988.822;Inherit;False;Property;_WindPower;Wind Power;22;0;Create;True;0;0;0;False;0;False;1;0.89;0;0.89;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;47;-2737.154,2267.197;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;53;-3001.25,1872.442;Inherit;False;Constant;_WindGradient;Wind Gradient;9;0;Create;True;0;0;0;False;0;False;0.5;0.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;25;-3311.314,-469.8424;Inherit;False;2724.468;1204.099;;47;200;156;78;210;73;80;199;99;232;140;81;111;48;42;192;44;165;209;92;103;160;175;204;93;166;105;96;72;162;161;158;196;231;186;207;213;220;221;219;57;275;276;277;281;274;282;287;Base Inputs;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;116;-4049.4,3486.583;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;61;-2502.863,1044.745;Inherit;True;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.CommentaryNode;242;-4284.777,4293.3;Inherit;False;221;209;(z);1;247;Remove this (Z);1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;63;-2532.44,2215.482;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.VertexColorNode;65;-2796.81,1410.345;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;243;-3972.025,4549.577;Inherit;False;Constant;_Float2;Float 2;14;0;Create;True;0;0;0;False;0;False;0.25;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;57;-3237.169,-347.7953;Inherit;True;Property;_MainTex;Albedo Map;5;1;[NoScaleOffset];Create;False;0;0;0;True;0;False;-1;None;502ac29bf3b77e54893f853cf44b8905;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.OneMinusNode;66;-2508.013,1981.075;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;115;-4209.709,3647.343;Inherit;False;Property;_WorldFrequency;World Frequency;23;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;172;-2274.574,1028.193;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.WireNode;173;-2969.006,894.9108;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;67;-2658.602,1758.019;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;241;-4309.37,4589.434;Inherit;True;True;False;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;62;-2653.358,2080.872;Inherit;False;Property;_WindScale;Wind Scale;24;0;Create;True;0;0;0;False;0;False;1;1;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;246;-3790.466,4599.13;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NoiseGeneratorNode;77;-2341.986,2006.852;Inherit;True;Simplex2D;True;False;2;0;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;245;-3814.426,4208.81;Inherit;False;221;209;(X);1;249;Set this to control the bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;74;-2215.351,1154.654;Inherit;False;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;117;-3851.411,3429.268;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;174;-2254.507,996.0104;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PowerNode;76;-2374.253,1749.125;Inherit;True;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;146;-4751.398,-664.6533;Inherit;False;1047.931;504.8553;Comment;9;155;152;159;153;151;168;149;150;147;Grass Color Variation;0.7504205,1,0,1;0;0
Node;AmplifyShaderEditor.TimeNode;118;-3838.238,3655.224;Inherit;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ClampOpNode;247;-4234.777,4343.3;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;-1.5;False;2;FLOAT;1.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;156;-2973.32,-121.4953;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;119;-3630.573,3527.268;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;249;-3764.426,4258.81;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;-0.3;False;2;FLOAT;0.3;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;157;-4490.42,-166.4277;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;84;-2096.712,1906.326;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;86;-2136.426,992.507;Inherit;False;Property;_GlobalWindPower;Global Wind Power;20;0;Create;True;0;0;0;False;0;False;1;0.002;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;158;-2974.734,-436.0768;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;82;-2032.653,1079.469;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;248;-3625.14,4535.671;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;147;-4715.579,-614.32;Inherit;False;1;0;FLOAT4;1,1,1,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WireNode;167;-3575.836,-380.5073;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.CommentaryNode;253;-3417.832,4546.427;Inherit;False;212;185;Comment;1;254;Final Rotation;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;252;-3550.532,4137.114;Inherit;False;221;209;Comment;1;255;(Y);1,1,1,1;0;0
Node;AmplifyShaderEditor.CosOpNode;120;-3492.139,3528.809;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;101;-1846.1,1745.073;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.BreakToComponentsNode;150;-4501.493,-488.6604;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;251;-3514.932,4374.246;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;149;-4500.35,-611.1265;Inherit;False;Random Range;-1;;17;7b754edb8aebbfb4a9ace907af661cfc;0;3;1;FLOAT2;0,0;False;2;FLOAT;0.9;False;3;FLOAT;1.15;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;183;-1579.193,1244.932;Inherit;False;705.807;416.543;Comment;4;110;135;136;182;Final Wind;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;250;-3965.5,4698.274;Inherit;False;Constant;_Float7;Float 7;14;0;Create;True;0;0;0;False;0;False;0.002;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;97;-1847.548,992.9334;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;122;-3306.02,3433.194;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;235;-3268.59,3238.54;Inherit;False;Constant;_Float1;Float 1;14;0;Create;True;0;0;0;False;0;False;10;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;255;-3500.532,4187.111;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;-0.9;False;2;FLOAT;1.2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;254;-3367.832,4598.794;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;123;-3490.984,3756.855;Inherit;False;Property;_BendAmount;Bend Amount;26;0;Create;True;0;0;0;False;0;False;1;0.05;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;135;-1472.068,1515.821;Inherit;False;Constant;_Float0;Float 0;22;0;Create;True;0;0;0;False;0;False;100;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;151;-4311.489,-545.2778;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;110;-1529.193,1294.932;Inherit;True;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.WireNode;168;-3861.417,-605.7603;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.NormalizeNode;236;-3279.362,3152.637;Inherit;False;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;136;-1304.117,1407.475;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;152;-4171.573,-482.5418;Inherit;False;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.WireNode;159;-4005.852,-581.1594;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;124;-3081.205,3560.761;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;126;-3022.62,3373.5;Inherit;False;Constant;_Vector1;Vector 1;14;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;237;-3082.513,3144.484;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;153;-4300.695,-334.4302;Half;False;Property;_LeafColorvariatoion;Leaf Color variatoion;4;0;Create;True;0;0;0;False;0;False;0.6;0.6;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;256;-3294.435,4309.276;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;257;-3143.093,4663.983;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;-1;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;128;-2840.943,3555.408;Inherit;True;FLOAT4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ClampOpNode;238;-2820.917,3204.868;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;10;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;182;-1081.217,1398.339;Inherit;False;FinalWind;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;258;-3200.668,4811.137;Inherit;False;Constant;_Float3;Float 3;14;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;155;-3977.159,-495.3538;Inherit;True;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;259;-2966.971,4764.323;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;184;-2514.374,3675.352;Inherit;False;182;FinalWind;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;260;-2993.851,4874.415;Inherit;False;Property;_BranchBending;Branch Bending;27;0;Create;True;0;0;0;False;0;False;1;1;0;10;0;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;278;-3331.685,-51.81213;Inherit;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LerpOp;130;-2578.145,3444.786;Inherit;True;3;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;129;-2620.129,3360.504;Inherit;False;Property;_WindDirection;Wind Direction;21;0;Create;True;0;0;0;False;0;False;1;1.6;1.54;1.6;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;261;-2656.964,4705.227;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;134;-2310.212,3555.022;Inherit;True;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;92;-2620.596,-25.87578;Inherit;False;Property;_AlbedoLightness;Albedo Lightness;2;0;Create;True;0;0;0;True;0;False;0;1;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;219;-2682.774,28.63831;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ToggleSwitchNode;277;-2612.36,-234.1316;Inherit;False;Property;_ColorVariation;Color Variation;3;0;Create;True;0;0;0;False;0;False;1;True;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.Vector3Node;286;-2024.345,3363.4;Float;False;Constant;_NoWindMove;NoWindMove;30;0;Create;True;0;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.WireNode;221;-2281.823,19.51141;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;99;-2586.312,-411.4217;Inherit;False;Property;_AlbedoColor;Albedo Color;1;0;Create;True;0;0;0;False;0;False;1,1,1,0;1,1,1,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RotateAboutAxisNode;131;-2035.004,3516.675;Inherit;False;False;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;103;-2350.715,-133.809;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.ToggleSwitchNode;285;-1570.426,3602.304;Inherit;False;Property;_UseWind;UseWind;19;0;Create;True;0;0;0;False;0;False;1;True;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.WireNode;220;-1212.969,152.8139;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;105;-2206.481,-271.991;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;204;-2053.676,-260.9829;Inherit;False;AlbedoOutput;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.CommentaryNode;71;-1576.009,1970.198;Inherit;False;1008.786;448.881;Comment;5;95;176;87;202;280;Vertex AO;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;178;-2024.659,-951.2426;Inherit;False;1162.169;418.4254;Comment;5;185;75;59;49;35;Fresnel Base;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;169;-1663.281,3318.997;Inherit;False;TrunkPivotBend;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;213;-883.3602,288.7041;Inherit;False;OpacityMaskOutput;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;5;-2784.75,2451.73;Inherit;False;1363.509;608.5165;Comment;12;194;60;51;33;38;27;26;20;17;15;11;7;Translucency Base;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;273;-404.6442,468.0545;Inherit;False;Property;_Cutout;Cutout;0;0;Create;True;0;0;0;False;0;False;0.4;0.35;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;214;-385.2633,368.992;Inherit;False;213;OpacityMaskOutput;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;96;-2512.534,596.5235;Inherit;False;Property;_TranslucencyPower;Translucency Power;14;0;Create;True;0;0;0;False;0;False;8;8;0;40;0;1;FLOAT;0
Node;AmplifyShaderEditor.ToggleSwitchNode;280;-943.541,2043.136;Inherit;False;Property;_VertexAo;Vertex Ao;17;0;Create;True;0;0;0;False;0;False;0;True;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FresnelNode;35;-1975.66,-747.6569;Inherit;False;Standard;WorldNormal;ViewDir;False;False;5;0;FLOAT3;0,0,1;False;4;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;72;-1897.966,375.1519;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SaturateNode;49;-1740.268,-734.038;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;44;-3015.557,338.3598;Inherit;True;Property;_MaskMap;Mask Map;9;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WireNode;181;-2424.314,-1066.41;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;203;-348.0876,283.5541;Inherit;False;202;AoOutput;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;194;-1629.807,2754.183;Inherit;False;TranslucencyBase;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;20;-2328.043,2709.66;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DotProductOpNode;38;-2039.298,2633.846;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;193;-397.5868,183.3905;Inherit;False;192;SmoothnessOutput;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;6;-3106.329,2531.85;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ToggleSwitchNode;281;-1111.161,465.4055;Inherit;False;Property;_UseTranslucent;UseTranslucent;12;0;Create;True;0;0;0;False;0;False;0;True;2;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;185;-1077.736,-832.5429;Inherit;False;FresnelBase;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;205;-369.2605,-171.6957;Inherit;False;204;AlbedoOutput;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;59;-1554.479,-857.443;Inherit;True;2;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;200;-869.6831,439.3702;Inherit;False;TranslucencyOutput;-1;True;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;201;-389.1656,13.56282;Inherit;False;200;TranslucencyOutput;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RangedFloatNode;42;-2502.358,487.3014;Inherit;False;Property;_SmoothnessIntensity;Smoothness Intensity;10;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;140;-1518.3,275.0743;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.WireNode;197;-2606.203,1242.058;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;93;-1769.171,286.7313;Inherit;False;2;2;0;FLOAT4;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;175;-2530.849,355.1566;Inherit;False;AoBase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;51;-1913.432,2730.562;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;73;-3301.139,402.6602;Inherit;False;Property;_NormalIntensity;Normal Intensity;7;0;Create;True;0;0;0;False;0;False;0;0;-3;3;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;87;-1554.674,2233.298;Inherit;False;Property;_VertexAointensity;Vertex Ao intensity;18;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;170;-338.5742,558.5815;Inherit;False;169;TrunkPivotBend;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;27;-2357.127,2550.37;Inherit;False;World;True;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SamplerNode;81;-3171.387,44.89965;Inherit;True;Property;_NormalMap;Normal Map;6;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;-1;None;None;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WireNode;209;-2149.49,475.3153;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;180;-1336.728,-1079.349;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;287;-1210.786,612.1148;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.WireNode;199;-1886.413,602.6767;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;210;-1831.297,-282.085;Inherit;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;111;-1641.707,426.1729;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;207;-874.7732,173.8667;Inherit;False;NormalOutput;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;78;-2977.489,592.6654;Inherit;False;Property;_AmbientOcclusion;Ambient Occlusion;16;0;Create;True;0;0;0;False;0;False;1;0.918;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;192;-875.4271,546.8015;Inherit;False;SmoothnessOutput;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WireNode;279;-2129.652,-742.3696;Inherit;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.WireNode;198;-2581.516,862.1125;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TransformDirectionNode;7;-2700.254,2707.115;Inherit;False;Object;World;True;Fast;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;186;-2241.012,-388.9149;Inherit;False;185;FresnelBase;1;0;OBJECT;;False;1;FLOAT4;0
Node;AmplifyShaderEditor.LightColorNode;165;-1487.704,397.6751;Inherit;False;0;3;COLOR;0;FLOAT3;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;166;-1312.138,364.998;Inherit;False;3;3;0;FLOAT4;0,0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.FaceVariableNode;160;-3015.151,242.7705;Inherit;False;0;1;FLOAT;0
Node;AmplifyShaderEditor.LightAttenuation;275;-1370.947,268.3849;Inherit;False;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;60;-1771.272,2734.502;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.DynamicAppendNode;162;-2549.198,218.2076;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;231;-2703.601,147.7472;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;80;-2692.831,523.6613;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;33;-2319.868,2831.874;Inherit;False;Property;_TranslucencyColor;Translucency Color;13;0;Create;True;0;0;0;False;0;False;0.5,0.5,0.5,0;0.5,0.5,0.5,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WorldSpaceLightDirHlpNode;15;-2709.985,2506.73;Inherit;False;True;1;0;FLOAT;0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.DynamicAppendNode;232;-2562.204,89.58797;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;206;-397.2667,-79.89816;Inherit;False;207;NormalOutput;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.BlendOpsNode;95;-1250.831,2195.403;Inherit;True;Multiply;True;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;11;-2702.115,2878.087;Inherit;False;Property;_TranslucencyRange;Translucency Range;15;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;161;-2702.872,254.3043;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;176;-1422.903,2051.479;Inherit;False;175;AoBase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;274;-1161.88,312.289;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.NegateNode;26;-2185.024,2701.666;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;75;-1288.933,-826.9722;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT4;0,0,0,0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.GetLocalVarNode;196;-2136.743,533.511;Inherit;False;194;TranslucencyBase;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;202;-770.7432,2259.105;Inherit;False;AoOutput;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ToggleSwitchNode;276;-2288.381,85.00214;Inherit;False;Property;_NormalBackFaceFixBranch;Normal BackFace Fix (Branch);8;0;Create;True;0;0;0;False;0;False;0;True;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;48;-2275.537,382.5274;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;282;-1459.559,536.4862;Half;False;Property;_blackColor;blackColor;11;0;Create;True;0;0;0;False;0;False;0,0,0,0;0.2735848,0.2735848,0.2735848,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;17;-2460.616,2799.517;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;268;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;0;wiiu;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;267;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;wiiu;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;265;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;wiiu;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;266;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;wiiu;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;264;0,0;Half;False;True;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;METAVERSE IMAGINATION/ASE_URPLitLeaves;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;18;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;True;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;Hidden/InternalErrorShader;0;0;Standard;38;Workflow;1;Surface;0;  Refraction Model;0;  Blend;0;Two Sided;0;Fragment Normal Space,InvertActionOnDeselection;0;Transmission;0;  Transmission Shadow;0.5,False,-1;Translucency;0;  Translucency Strength;1,False,-1;  Normal Distortion;0.5,False,-1;  Scattering;2,False,-1;  Direct;0.9,False,-1;  Ambient;0.1,False,-1;  Shadow;0.5,False,-1;Cast Shadows;1;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;1;Built-in Fog;1;_FinalColorxAlpha;0;Meta Pass;1;Override Baked GI;0;Extra Pre Pass;0;DOTS Instancing;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Write Depth;0;  Early Z;0;Vertex Position,InvertActionOnDeselection;1;0;6;False;True;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;263;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;wiiu;0;0;Standard;0;False;0
WireConnection;8;0;3;0
WireConnection;8;1;4;0
WireConnection;14;0;10;0
WireConnection;14;1;9;0
WireConnection;16;0;12;0
WireConnection;16;2;8;0
WireConnection;19;0;16;0
WireConnection;21;0;12;0
WireConnection;21;2;14;0
WireConnection;24;1;19;0
WireConnection;24;0;18;0
WireConnection;28;0;21;0
WireConnection;284;0;12;1
WireConnection;34;0;2;2
WireConnection;37;0;24;0
WireConnection;36;0;28;0
WireConnection;36;1;29;0
WireConnection;283;0;12;3
WireConnection;55;0;37;0
WireConnection;55;1;34;0
WireConnection;217;0;2;2
WireConnection;217;1;36;0
WireConnection;114;0;12;0
WireConnection;54;0;284;0
WireConnection;54;1;283;0
WireConnection;171;0;2;0
WireConnection;47;0;39;0
WireConnection;47;1;39;0
WireConnection;116;0;114;1
WireConnection;116;1;114;3
WireConnection;61;0;217;0
WireConnection;61;1;55;0
WireConnection;61;2;2;3
WireConnection;63;0;54;0
WireConnection;63;1;47;0
WireConnection;66;0;50;0
WireConnection;172;0;171;0
WireConnection;173;0;2;0
WireConnection;67;0;52;2
WireConnection;67;1;53;0
WireConnection;241;0;262;0
WireConnection;246;0;243;0
WireConnection;246;1;241;0
WireConnection;77;0;63;0
WireConnection;77;1;62;0
WireConnection;74;0;172;0
WireConnection;74;1;61;0
WireConnection;74;2;65;2
WireConnection;117;0;116;0
WireConnection;117;1;115;0
WireConnection;174;0;173;0
WireConnection;76;0;67;0
WireConnection;76;1;66;0
WireConnection;247;0;262;3
WireConnection;156;0;57;0
WireConnection;119;0;117;0
WireConnection;119;1;118;2
WireConnection;249;0;262;1
WireConnection;157;0;156;0
WireConnection;84;0;76;0
WireConnection;84;1;77;0
WireConnection;158;0;57;0
WireConnection;82;0;174;0
WireConnection;82;1;74;0
WireConnection;248;0;247;0
WireConnection;248;1;246;0
WireConnection;167;0;158;0
WireConnection;120;0;119;0
WireConnection;101;0;65;2
WireConnection;101;1;84;0
WireConnection;150;0;157;0
WireConnection;251;0;249;0
WireConnection;251;1;248;0
WireConnection;149;1;147;0
WireConnection;97;0;86;0
WireConnection;97;1;82;0
WireConnection;122;0;2;2
WireConnection;122;1;120;0
WireConnection;255;0;262;2
WireConnection;254;0;251;0
WireConnection;254;1;250;0
WireConnection;151;0;150;0
WireConnection;151;1;149;0
WireConnection;110;0;97;0
WireConnection;110;1;101;0
WireConnection;168;0;167;0
WireConnection;236;0;114;2
WireConnection;136;0;135;0
WireConnection;136;1;110;0
WireConnection;152;0;151;0
WireConnection;152;1;150;1
WireConnection;152;2;150;2
WireConnection;159;0;168;0
WireConnection;124;0;122;0
WireConnection;124;1;123;0
WireConnection;237;0;236;0
WireConnection;237;1;235;0
WireConnection;256;0;255;0
WireConnection;256;1;254;0
WireConnection;257;0;256;0
WireConnection;128;0;124;0
WireConnection;128;1;126;2
WireConnection;238;0;237;0
WireConnection;182;0;136;0
WireConnection;155;0;159;0
WireConnection;155;1;152;0
WireConnection;155;2;153;0
WireConnection;259;0;258;0
WireConnection;259;1;257;0
WireConnection;278;0;155;0
WireConnection;130;0;126;2
WireConnection;130;1;128;0
WireConnection;130;2;238;0
WireConnection;261;0;259;0
WireConnection;261;1;260;0
WireConnection;134;0;184;0
WireConnection;134;1;130;0
WireConnection;219;0;57;4
WireConnection;277;0;57;0
WireConnection;277;1;278;0
WireConnection;221;0;219;0
WireConnection;131;0;261;0
WireConnection;131;1;129;0
WireConnection;131;3;134;0
WireConnection;103;0;277;0
WireConnection;103;1;92;0
WireConnection;285;0;286;0
WireConnection;285;1;131;0
WireConnection;220;0;221;0
WireConnection;105;0;99;0
WireConnection;105;1;103;0
WireConnection;204;0;105;0
WireConnection;169;0;285;0
WireConnection;213;0;220;0
WireConnection;280;0;176;0
WireConnection;280;1;95;0
WireConnection;72;0;44;3
WireConnection;72;1;196;0
WireConnection;49;0;35;0
WireConnection;181;0;44;3
WireConnection;194;0;60;0
WireConnection;20;0;15;0
WireConnection;20;1;17;0
WireConnection;38;0;27;0
WireConnection;38;1;26;0
WireConnection;6;0;2;0
WireConnection;281;0;287;0
WireConnection;281;1;274;0
WireConnection;185;0;75;0
WireConnection;59;0;279;0
WireConnection;59;1;49;0
WireConnection;200;0;281;0
WireConnection;140;0;199;0
WireConnection;140;1;111;0
WireConnection;197;0;65;1
WireConnection;93;0;210;0
WireConnection;93;1;72;0
WireConnection;175;0;80;0
WireConnection;51;0;38;0
WireConnection;51;1;33;0
WireConnection;81;5;73;0
WireConnection;209;0;48;0
WireConnection;180;0;181;0
WireConnection;287;0;282;0
WireConnection;287;1;57;0
WireConnection;199;0;198;0
WireConnection;210;0;186;0
WireConnection;111;0;96;0
WireConnection;111;1;93;0
WireConnection;207;0;276;0
WireConnection;192;0;209;0
WireConnection;279;0;277;0
WireConnection;198;0;197;0
WireConnection;7;0;6;0
WireConnection;166;0;140;0
WireConnection;166;1;165;1
WireConnection;166;2;165;2
WireConnection;60;0;51;0
WireConnection;162;0;81;1
WireConnection;162;1;81;2
WireConnection;162;2;161;0
WireConnection;231;0;81;2
WireConnection;231;1;160;0
WireConnection;80;0;44;2
WireConnection;80;1;78;0
WireConnection;232;0;81;1
WireConnection;232;1;231;0
WireConnection;232;2;81;3
WireConnection;95;0;65;1
WireConnection;95;1;176;0
WireConnection;95;2;87;0
WireConnection;161;0;81;3
WireConnection;161;1;160;0
WireConnection;274;0;275;0
WireConnection;274;1;166;0
WireConnection;26;0;20;0
WireConnection;75;0;180;0
WireConnection;75;1;59;0
WireConnection;202;0;280;0
WireConnection;276;0;232;0
WireConnection;276;1;162;0
WireConnection;48;0;44;4
WireConnection;48;1;42;0
WireConnection;17;0;7;0
WireConnection;17;1;11;0
WireConnection;264;0;205;0
WireConnection;264;1;206;0
WireConnection;264;2;201;0
WireConnection;264;4;193;0
WireConnection;264;5;203;0
WireConnection;264;6;214;0
WireConnection;264;7;273;0
WireConnection;264;8;170;0
ASEEND*/
//CHKSM=EEFDB2539EF33150E06A22260CD4EA4570079EB4
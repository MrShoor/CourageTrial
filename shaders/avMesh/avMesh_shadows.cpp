#include "hlsl.h"
#include "matrices.h"
#include "avModelMaterials.h"

struct VS_Input {
    float3 vsCoord   : vsCoord;
    float3 vsNormal  : vsNormal;
    float2 vsTex     : vsTex;
    float  vsMatIndex: vsMatIndex;
    float4 vsWIndex  : vsWIndex;
    float4 vsWeight  : vsWeight;
    float2 aiBoneMatOffset: aiBoneMatOffset;
};

struct VS_Output {
    float3 wCoord : wCoord;
};

Texture2D BoneTransform; SamplerState BoneTransformSampler;

float4x4 GetBoneTransform(in float BoneCoord) {
    float2 TexSize;
    BoneTransform.GetDimensions(TexSize.x, TexSize.y);
    float2 PixSize = 1.0 / TexSize;
    
    float2 TexCoord;
    TexCoord.x = frac(BoneCoord / TexSize.x);
    TexCoord.y = trunc(BoneCoord / TexSize.x) / TexSize.y;
    TexCoord += 0.5 * PixSize;
    
    float4x4 m;
    m[0] = BoneTransform.SampleLevel(BoneTransformSampler, float2(TexCoord.x,                 TexCoord.y), 0);
    m[1] = BoneTransform.SampleLevel(BoneTransformSampler, float2(TexCoord.x +     PixSize.x, TexCoord.y), 0);
    m[2] = BoneTransform.SampleLevel(BoneTransformSampler, float2(TexCoord.x + 2.0*PixSize.x, TexCoord.y), 0);
    m[3] = BoneTransform.SampleLevel(BoneTransformSampler, float2(TexCoord.x + 3.0*PixSize.x, TexCoord.y), 0);
    return m;
}

float4x4 GetBoneTransform(in float4 Indices, in float4 Weights) {
    float4x4 m = {
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    };
    float4 ind = Indices*4.0;
    if (Indices.x>=0.0) m  = GetBoneTransform(ind.x)*Weights.x;
    if (Indices.y>=0.0) m += GetBoneTransform(ind.y)*Weights.y;
    if (Indices.z>=0.0) m += GetBoneTransform(ind.z)*Weights.z;
    if (Indices.w>=0.0) m += GetBoneTransform(ind.w)*Weights.w;
    return m;
}

VS_Output VS(VS_Input In) {
    VS_Output Out;
    float4x4 mBone = GetBoneTransform(In.vsWIndex+In.aiBoneMatOffset.x, In.vsWeight);
    Out.wCoord = mul(float4(In.vsCoord, 1.0), mBone).xyz;
    return Out;
}

///////////////////////////////////////////////////////////////////////////////

struct GS_Output {
    float4 Pos : SV_Position;
    uint Idx : SV_RenderTargetArrayIndex;
};

float4x4 light_matrices[6];

[maxvertexcount(3*6)]
void GS(triangle VS_Output In[3], inout TriangleStream<GS_Output> OutStream) {
    GS_Output Out;
    for (uint i = 0; i < 6; i++) {
        Out.Idx = i;
        for (uint j = 0; j < 3; j++) {
            Out.Pos = mul(float4(In[j].wCoord, 1.0), light_matrices[i]);
            OutStream.Append(Out);
        }
        OutStream.RestartStrip();
    }
}

///////////////////////////////////////////////////////////////////////////////
struct PS_Output {
    float4 Color : SV_Target0;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    Out.Color = float4(In.wCoord.z, 0, 0, 0);
    return Out;
}
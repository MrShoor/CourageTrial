#include "hlsl.h"
#include "matrices.h"
#include "lighting_types.h"

float3 headSize;
RWTexture3D<uint> headBuffer : register(u0);
globallycoherent RWStructuredBuffer<ListNode> lightLinkedList : register(u1);

struct Frustum {
    float4 plane[6];
    float3 pts[8];
};

float3 Unproject(float3 ppt) {
    float4 Out;
    Out = mul(float4(ppt, 1.0), VP_InverseMatrix);
    return Out.xyz/Out.w;
}

float3 ProjectFromView(float3 pt) {
    float4 Out;
    Out = mul(float4(pt, 1.0), P_Matrix);
    return Out.xyz/Out.w;
}

Frustum BuildFrustum(float3 boundmin, float3 boundmax) {
    Frustum Out;
    float3 pt0 = Unproject(boundmin);
    float3 pt1 = Unproject(float3(boundmin.x, boundmax.y, boundmin.z));
    float3 pt2 = Unproject(float3(boundmax.x, boundmin.y, boundmin.z));
    float3 pt3 = Unproject(float3(boundmin.x, boundmax.y, boundmax.z));
    float3 pt4 = Unproject(float3(boundmax.x, boundmin.y, boundmax.z));
    float3 pt5 = Unproject(boundmax);
    Out.plane[0].xyz = cross(pt2-pt0, pt1-pt0); //near
    Out.plane[1].xyz = cross(pt4-pt5, pt3-pt5); //far
    Out.plane[2].xyz = cross(pt5-pt4, pt2-pt4); //right
    Out.plane[3].xyz = cross(pt3-pt1, pt0-pt1); //left
    Out.plane[4].xyz = cross(pt1-pt3, pt5-pt3); //top
    Out.plane[5].xyz = cross(pt0-pt2, pt4-pt2); //bottom

    Out.plane[0].w = -dot(Out.plane[0].xyz, pt0); //near
    Out.plane[1].w = -dot(Out.plane[1].xyz, pt5); //far
    Out.plane[2].w = -dot(Out.plane[2].xyz, pt4); //right
    Out.plane[3].w = -dot(Out.plane[3].xyz, pt1); //left
    Out.plane[4].w = -dot(Out.plane[4].xyz, pt3); //top
    Out.plane[5].w = -dot(Out.plane[5].xyz, pt2); //bottom
    
    Out.pts[0] = pt0;
    Out.pts[1] = pt1;
    Out.pts[2] = pt2;
    Out.pts[3] = pt3;
    Out.pts[4] = pt4;
    Out.pts[5] = pt5;
    Out.pts[6] = Unproject(float3(boundmax.x, boundmax.y, boundmin.z));
    Out.pts[7] = Unproject(float3(boundmin.x, boundmin.y, boundmax.z));
    
    [unroll]
    for (uint i=0; i<6; i++)
        Out.plane[i] /= length(Out.plane[i].xyz);
    return Out;
}

bool LightInFrustum(Light light, Frustum f) {
    uint i;
    uint j;
    for (i = 0; i < 6; i++) {
        if (dot(f.plane[i].xyz, light.PosRange.xyz) - f.plane[i].w > light.PosRange.w) return false;
    }
    for (i = 0; i < 8; i++) {
        float4 pl;
        pl.xyz = normalize(f.pts[i] - light.PosRange.xyz);
        pl.w = -dot(pl.xyz, light.PosRange.xyz + pl.xyz*light.PosRange.w);
        for (j = 0; j < 8; j++) {
            if (dot(f.pts[j], pl.xyz)+pl.w < 0) break;
        }
        if (j == 8) return false;
    }
    return true;
}

[numthreads(8, 8, 8)]
void CS(uint3 id: SV_DispatchThreadID)
{
    float3 boundMin = (float3)id / headSize;
    float3 boundMax = ((float3)id + float3(1.0,1.0,1.0)) / headSize;
    
    boundMin.xy = lerp(float2(-1.0, -1.0), float2(1.0, 1.0), boundMin.xy);
    boundMax.xy = lerp(float2(-1.0, -1.0), float2(1.0, 1.0), boundMax.xy);
    boundMin.z = ProjectFromView(float3(0,0,lerp(planesNearFar.x, planesNearFar.y, boundMin.z))).z;
    boundMax.z = ProjectFromView(float3(0,0,lerp(planesNearFar.x, planesNearFar.y, boundMax.z))).z;
    
    Frustum f = BuildFrustum(boundMin, boundMax);
    headBuffer[id] = 0xffffffff;
    for (uint i = 0; i < (uint)lightCount; i++) {
        if (LightInFrustum(light_list[i], f)) {
            uint n = lightLinkedList.IncrementCounter();
            if (n == 0xffffffff) return;
            uint prev_n = headBuffer[id];
            ListNode newNode;
            newNode.LightIdx = i;
            newNode.NextNode = prev_n;
            lightLinkedList[n] = newNode;
            headBuffer[id] = n;
//            headBuffer[id] = 0;
        }
    }
}
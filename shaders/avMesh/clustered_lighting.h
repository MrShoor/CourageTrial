/* 
 * File:   clustered_lighting.h
 * Author: alexander.busarov
 *
 * Created on February 5, 2018, 2:31 PM
 */

#ifndef CLUSTERED_LIGHTING_H
#define	CLUSTERED_LIGHTING_H

#include "../Lighting/lighting_types.h"

float3 light_headBufferSize;
Texture3D<uint> light_headBuffer;
StructuredBuffer<ListNode> light_linkedList;

float3 PhongColor(float3 Normal, float3 ViewDir, float3 LightDir, float3 LightColor, float4 Diffuse, float4 Specular, float SpecPower)
{
   float3 RefLightDir = -reflect(LightDir, Normal);
   float diffK = (saturate(dot(Normal, LightDir)));
   if (diffK <= 0.0) return 0.0;
   float3 DiffuseK = LightColor * diffK;
   float3 DiffuseColor = Diffuse.rgb * DiffuseK;
   float3 SpecularColor = (1.0-diffK) * Specular.rgb * (pow(saturate(dot(ViewDir, RefLightDir)), SpecPower));
   return (DiffuseColor + SpecularColor);
}

float4 Clustered_Phong(float3 ProjPos, float3 ViewPos, float3 Normal, float3 ViewDir, float4 Diffuse, float4 Specular, float4 Ambient, float SpecPower) {
    float4 Out = float4(Ambient.xyz, 1.0);
    
    float z = (ViewPos.z - planesNearFar.x) / (planesNearFar.y - planesNearFar.x);
    ProjPos.xy *= 0.5;
    ProjPos.xy += 0.5;
    ProjPos.z = lerp(depthRange.x, depthRange.y, z);
    uint3 crd = trunc(ProjPos.xyz*(light_headBufferSize+0.0));
    uint nodeIdx = light_headBuffer[crd];
    int i = 0;
    while ((nodeIdx != 0xffffffff)&&(i<10)) {
        ListNode node = light_linkedList[nodeIdx];
        nodeIdx = node.NextNode;
        i++;
        
        Light l = light_list[node.LightIdx];
        l.PosRange.xyz = mul(float4(l.PosRange.xyz, 1.0), V_Matrix).xyz;
        float3 LightDir = l.PosRange.xyz - ViewPos;
        float dist = length(LightDir);
        LightDir /= dist;
        float atten = saturate(1.0 - ((dist * dist) / (l.PosRange.w * l.PosRange.w)));
                
        Out.xyz += PhongColor(Normal, ViewDir, LightDir, l.Color, Diffuse, Specular, SpecPower)*atten;

        //Out.xyz = atten;
        
    }
    
    return Out;
}

#endif	/* CLUSTERED_LIGHTING_H */


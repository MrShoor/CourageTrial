/* 
 * File:   lighting_types.h
 * Author: alexander.busarov
 *
 * Created on February 5, 2018, 2:47 PM
 */

#ifndef LIGHTING_TYPES_H
#define	LIGHTING_TYPES_H

struct Light {
    float4 PosRange;
    float3 Color;
};

struct ListNode {
    uint LightIdx;
    uint NextNode;
};

float2 depthRange;
float2 planesNearFar;
float lightCount;
StructuredBuffer<Light> light_list;

#endif	/* LIGHTING_TYPES_H */


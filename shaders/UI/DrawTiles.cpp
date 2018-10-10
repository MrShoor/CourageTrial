#include <hlsl.h>
#include "matrices.h"

struct VS_In {
    uint VID: SV_VertexID;
    float2 aiPos  : aiPos;
    uint   aiColor: aiColor;
};

struct VS_Out {
    float4 Pos  : SV_Position;
    float4 Color: Color;
};

#define pySmall 0.57735026918962576450914878050258
#define pyBig 1.1547005383792515290182975610052

float2 HexVerts[18] = {
    {0, 0}, {-1,   pySmall}, {-1,  -pySmall},
    {0, 0}, {-1,  -pySmall}, { 0,  -pyBig},
    {0, 0}, { 0,  -pyBig},   { 1,  -pySmall},
    {0, 0}, { 1,  -pySmall}, { 1,   pySmall},
    {0, 0}, { 1,   pySmall}, { 0,   pyBig},
    {0, 0}, { 0,   pyBig},   {-1,   pySmall},
};

float4 TileColors[5];
float YPos;

VS_Out VS(VS_In In) {
    VS_Out Out;
    float2 v = HexVerts[In.VID] * 0.5;    
    Out.Pos = float4(In.aiPos.x + v.x, YPos, In.aiPos.y + v.y, 1.0);
    Out.Pos = mul(Out.Pos, VP_Matrix);
    Out.Color = In.VID % 3 ? TileColors[ In.aiColor ] : 0.0;
    return Out;
}

//////////////////////////////////////////////


struct PS_Out {
    float4 Color : SV_Target0;
};

PS_Out PS(VS_Out In) {
    PS_Out Out;
    Out.Color = In.Color;    
    Out.Color.a = pow(Out.Color.a, 4);
    return Out;
}
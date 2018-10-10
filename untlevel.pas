unit untLevel;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Math,
  Classes, SysUtils, avBase, avRes, bWorld, mutils, bLights, avMesh, avTypes, avTess, avContnrs;

type
  TRoomMap = class;

  { TRoomObject }

  TRoomObject = class (TbGameObject)
  private
    FRoomDir: Integer;
    FRoomPos: TVec2i;
    procedure SetRoomDir(const AValue: Integer);
    procedure SetRoomPos(const AValue: TVec2i);
  protected
    FRoom: TRoomMap;
    function CanRegister(target: TavObject): boolean; override;
  public
    function BlockedCellsCount: Integer; virtual;
    function GetBlockedCell(AIndex: Integer): TVec2i; virtual;
    function GetAbsoluteBlockedCell(AIndex: Integer): TVec2i; overload;
    function GetAbsoluteBlockedCell(AIndex: Integer; APos: TVec2i; ADir: Integer): TVec2i; overload;

    property Room: TRoomMap read FRoom;
    property RoomPos: TVec2i read FRoomPos write SetRoomPos;
    property RoomDir: Integer read FRoomDir write SetRoomDir;
    procedure SetRoomPosDir(const APos: TVec2i; const ADir: Integer);

    destructor Destroy; override;
  end;

  {$ScopedEnums On}
  {$Z4}
  TTileColorID = (None, Normal, Selected, Highlighted, Hovered);
  {$Z1}
  {$ScopedEnums Off}

  { TRoomUI }

  TRoomUI = class (TavMainRenderChild)
  private type
    THexTile = packed record
      vsPos  : TVec2;
      vsColor: TTileColorID;
      class function Layout: IDataLayout; static;
    end;
    PHexTile = ^THexTile;
    IHexTiles = {$IfDef FPC}specialize{$EndIf} IArray<THexTile>;
    THexTiles = {$IfDef FPC}specialize{$EndIf} TVerticesRec<THexTile>;
  private
    FPlaneY    : Single;

    FRadius    : Integer;
    FTilesProg : TavProgram;
    FTilesVB   : TavVB;
    FTilesData : IHexTiles;
    FAffinePack: TMat2;
    FAffinePackInv: TMat2;

    FColors: array [TTileColorID] of TVec4;
    function  GetColors(ID: TTileColorID): TVec4;
    procedure SetColors(ID: TTileColorID; const AValue: TVec4);
  protected
    function  PosToIndex(const APos: TVec2i): Integer;
    procedure SetTileColor(const APos: TVec2i; const AColorID: TTileColorID);
    function  GetTileColor(const APos: TVec2i): TTileColorID;

    procedure SetMapRadius(const ARadius: Integer);
  protected
    procedure AfterRegister; override;
  public
    property Radius: Integer read FRadius write SetMapRadius;
    property Colors[ID: TTileColorID]: TVec4 read GetColors write SetColors;
    property TileColor[const APos: TVec2i]: TTileColorID read GetTileColor write SetTileColor;

    function GetTileAtCoords(const ARay: TLine): TVec2i; overload;
    function GetTileAtCoords(const ACoord: TVec2): TVec2i; overload;

    function TilePosToWorldPos(const ATilePos: TVec2i): TVec3;

    function AutoTileColor(const APos: TVec2i): TTileColorID;

    procedure DrawUI();
  end;

  { TRoomMap }

  TRoomMap = class (TavMainRenderChild)
  private type
    TObjectMap = {$IFDef FPC}specialize{$EndIf} THashMap<TVec2i, TRoomObject>;
    IObjectMap = {$IFDef FPC}specialize{$EndIf} IHashMap<TVec2i, TRoomObject>;
  private
    FRadius: Integer;

    FRoomUI: TRoomUI;
    FMovedTile: TVec2i;

    FObjects: IObjectMap;

    procedure SetRadius(const AValue: Integer);
  protected
    procedure AfterRegister; override;
  public
    property UI: TRoomUI read FRoomUI;

    property Radius: Integer read FRadius write SetRadius;

    procedure Draw();

    function NeighbourTile(const ACurrent: TVec2i; const AIndex: Integer): TVec2i;
    function Distance(const APt1, APt2: TVec2i): Integer;
    function IsCellExists(const APos: TVec2i): Boolean;

    procedure PutObject(const AObject: TRoomObject);
    procedure RemoveObject(const AObject: TRoomObject);
    function  ObjectAt(const APos: TVec2i): TRoomObject;
  end;

  { TLantern }

  TLantern = class (TRoomObject)
  private
    FLight: IavPointLight;
  public
    function BlockedCellsCount: Integer; override;
    function GetBlockedCell(AIndex: Integer): TVec2i; override;
    procedure LoadModels();
  end;

  { TPlayer }

  TPlayer = class (TbGameObject)
  private
    FAnim: array of IavAnimationController;
  protected
    procedure UpdateStep; override;
  public
    procedure LoadModels();
  end;

  { TBattleRoom }

  TBattleRoom = class (TavMainRenderChild)
  private
    FWorld: TbWorld;

    FFloor: TbGameObject;
    FLanterns: IbGameObjArr;

    FPlayer: TPlayer;

    FMap: TRoomMap;
  protected
    procedure EMUps(var msg: TavMessage); message EM_UPS;
  	procedure AfterRegister; override;

    procedure OnAfterWorldDraw(Sender: TObject);
  public
    procedure Draw();
    procedure Generate();
  end;

implementation

{ TRoomObject }

procedure TRoomObject.SetRoomPos(const AValue: TVec2i);
begin
  if FRoomPos = AValue then Exit;
  FRoomPos := AValue;
  FRoom.RemoveObject(Self);
  FRoom.PutObject(Self);
  Pos := FRoom.UI.TilePosToWorldPos(AValue);
end;

procedure TRoomObject.SetRoomDir(const AValue: Integer);
begin
  if FRoomDir = AValue then Exit;
  FRoomDir := AValue;
  FRoom.RemoveObject(Self);
  FRoom.PutObject(Self);
  Rot := Quat(Vec(0, 1, 0), 2 * Pi * (AValue / 6));
end;

function TRoomObject.CanRegister(target: TavObject): boolean;
begin
  Result := inherited CanRegister(target);
  if not Result then Exit;
  FRoom := TRoomMap(target.FindAtParents(TRoomMap));
  Result := Assigned(FRoom);
end;

function TRoomObject.BlockedCellsCount: Integer;
begin
  Result := 1;
end;

function TRoomObject.GetBlockedCell(AIndex: Integer): TVec2i;
begin
  Result := Vec(0,0);
end;

function TRoomObject.GetAbsoluteBlockedCell(AIndex: Integer): TVec2i;
begin
  Result := GetAbsoluteBlockedCell(AIndex, FRoomPos, FRoomDir);
end;

function TRoomObject.GetAbsoluteBlockedCell(AIndex: Integer; APos: TVec2i; ADir: Integer): TVec2i;
begin
  Result := GetBlockedCell(AIndex) + APos;
end;

procedure TRoomObject.SetRoomPosDir(const APos: TVec2i; const ADir: Integer);
begin
  FRoomPos := APos;
  FRoomDir := ADir;
  FRoom.RemoveObject(Self);
  FRoom.PutObject(Self);
  Pos := FRoom.UI.TilePosToWorldPos(APos);
  Rot := Quat(Vec(0, 1, 0), 2 * Pi * (ADir / 6));
end;

destructor TRoomObject.Destroy;
begin
  inherited Destroy;
  FRoom.RemoveObject(Self);
end;

{ TRoomUI.THexTile }

class function TRoomUI.THexTile.Layout: IDataLayout;
begin
  Result := LB.Add('aiPos', ctFloat, 2).Add('aiColor', ctUInt, 1).Finish();
end;

{ TRoomUI }

procedure TRoomUI.AfterRegister;
var epmty_tile: THexTile;
begin
  inherited AfterRegister;
  FPlaneY := 0.01;

  epmty_tile.vsPos := Vec(0,0);
  epmty_tile.vsColor := TTileColorID.None;

  FTilesData := THexTiles.Create();
  FTilesData.Add(epmty_tile);

  FTilesProg := TavProgram.Create(Self);
  FTilesProg.Load('UI_DrawTiles', False, 'D:\Projects\CourageTrial\shaders\!Out');
  FTilesVB   := TavVB.Create(Self);
  FTilesVB.Vertices := FTilesData as IVerticesData;

  FAffinePack.Row[0] := Vec(1,0);
  FAffinePack.Row[1] := Vec(0.5,0.86602540378443864676372317075294);
  FAffinePackInv := Inv(FAffinePack);

  FColors[TTileColorID.Normal] := Vec(0.0,0.0,0.0,1);
  FColors[TTileColorID.Hovered] := Vec(0,1,0,1);
end;

function TRoomUI.GetTileAtCoords(const ARay: TLine): TVec2i;
var IntPt: TVec3;
begin
  if Intersect(Plane(0,1,0,FPlaneY), ARay, IntPt) then
    Result := GetTileAtCoords(Vec(IntPt.x, IntPt.z))
  else
    Result := Vec(0,0);
end;

function TRoomUI.GetTileAtCoords(const ACoord: TVec2): TVec2i;
var v: TVec2;
    vi: array [0..3] of TVec2i;
    i: Integer;
    dist, mindist: Single;
begin
  Result := Vec(0,0);

  v := ACoord * FAffinePackInv;
  vi[0] := Floor(v);
  vi[3] := Ceil(v);
  vi[1] := Vec(vi[0].x, vi[3].y);
  vi[2] := Vec(vi[3].x, vi[0].y);

  mindist := HUGE;
  for i := 0 to 3 do
  begin
    v := vi[i] * FAffinePack;
    dist := LenSqr(ACoord - v);
    if dist < mindist then
    begin
      mindist := dist;
      Result := vi[i];
    end;
  end;
end;

function TRoomUI.TilePosToWorldPos(const ATilePos: TVec2i): TVec3;
var v: TVec2;
begin
  v := ATilePos * FAffinePackInv;
  Result := Vec(v.x, 0, v.y);
end;

function TRoomUI.AutoTileColor(const APos: TVec2i): TTileColorID;
var rm: TRoomMap;
    obj: TRoomObject;
begin
  rm := Parent as TRoomMap;
  if not rm.IsCellExists(APos) then Exit(TTileColorID.None);
  obj := rm.ObjectAt(APos);
  if obj = nil then Exit(TTileColorID.Normal);
  Result := TTileColorID.None;
end;

procedure TRoomUI.DrawUI;
var tmpColors: TVec4Arr;
  i: Integer;
begin
  if FTilesData.Count = 0 then Exit;
  SetLength(tmpColors, Length(FColors));
  for i := 0 to Length(FColors) - 1 do
    tmpColors[i] := FColors[TTileColorID(i)];

  FTilesProg.Select();
  FTilesProg.SetAttributes(nil, nil, FTilesVB);
  FTilesProg.SetUniform('TileColors', tmpColors);
  FTilesProg.SetUniform('YPos', FPlaneY);
  FTilesProg.Draw(ptTriangles, cmNone, False, FTilesData.Count, 0, 18);
end;

function TRoomUI.GetColors(ID: TTileColorID): TVec4;
begin
  Result := FColors[ID];
end;

procedure TRoomUI.SetColors(ID: TTileColorID; const AValue: TVec4);
begin
  FColors[ID] := AValue;
end;

function TRoomUI.PosToIndex(const APos: TVec2i): Integer;
var w: Integer;
begin
  w := FRadius*2 + 1;
  Result := (APos.y + FRadius) * w + (APos.x + FRadius);
end;

procedure TRoomUI.SetTileColor(const APos: TVec2i; const AColorID: TTileColorID);
var pTile: PHexTile;
begin
  if not (Parent as TRoomMap).IsCellExists(APos) then Exit;

  pTile := PHexTile(FTilesData.PItem[PosToIndex(APos)]);
  if pTile^.vsColor = AColorID then Exit;
  pTile^.vsColor := AColorID;
  FTilesVB.Invalidate;
end;

function TRoomUI.GetTileColor(const APos: TVec2i): TTileColorID;
begin
  Result := PHexTile(FTilesData.PItem[PosToIndex(APos)])^.vsColor;
end;

procedure TRoomUI.SetMapRadius(const ARadius: Integer);
var
  j, i: Integer;
  new_tile: THexTile;
  w: Integer;
begin
  if ARadius = FRadius then Exit;
  FRadius := ARadius;
  w := FRadius * 2 + 1;
  FTilesData.Clear();
  FTilesData.Capacity := w*w;
  for j := -FRadius to FRadius do
    for i := -FRadius to FRadius do
    begin
      new_tile.vsPos := Vec(i, j) * FAffinePack;
      if (Parent as TRoomMap).IsCellExists(Vec(i,j)) then
        new_tile.vsColor := TTileColorID.Normal
      else
        new_tile.vsColor := TTileColorID.None;
      FTilesData.Add(new_tile);
    end;
  FTilesVB.Invalidate;
end;

{ TRoomMap }

procedure TRoomMap.SetRadius(const AValue: Integer);
begin
  if FRadius = AValue then Exit;
  FRadius := AValue;
  FRoomUI.Radius := AValue;
end;

function TRoomMap.NeighbourTile(const ACurrent: TVec2i; const AIndex: Integer): TVec2i;
begin
  case AIndex of
    0:
      begin
        Result.x := ACurrent.x-1;
        Result.y := ACurrent.y;
      end;
    1:
      begin
        Result.x := ACurrent.x+1;
        Result.y := ACurrent.y;
      end;
    2:
      begin
        Result.x := ACurrent.x-1;
        Result.y := ACurrent.y+1;
      end;
    3:
      begin
        Result.x := ACurrent.x;
        Result.y := ACurrent.y+1;
      end;
    4:
      begin
        Result.x := ACurrent.x;
        Result.y := ACurrent.y-1;
      end;
    5:
      begin
        Result.x := ACurrent.x+1;
        Result.y := ACurrent.y-1;
      end;
  else
    Assert(False);
  end;
end;

procedure TRoomMap.AfterRegister;
begin
  inherited AfterRegister;
  FRoomUI := TRoomUI.Create(Self);

  FObjects := TObjectMap.Create();
end;

procedure TRoomMap.Draw;
var tile: TVec2i;
begin
  tile := FRoomUI.GetTileAtCoords(Main.Cursor.Ray);
  if tile <> FMovedTile then
  begin
    FRoomUI.TileColor[FMovedTile] := FRoomUI.AutoTileColor(FMovedTile);
    FRoomUI.TileColor[tile] := TTileColorID.Hovered;
    FMovedTile := tile;
  end;
  FRoomUI.DrawUI();
end;

function TRoomMap.Distance(const APt1, APt2: TVec2i): Integer;
var dv: TVec2i;
begin
  dv := APt2 - APt1;
  if dv.x * dv.y < 0 then
    Result := Max(abs(dv.x), abs(dv.y))
  else
    Result := abs(dv.x) + abs(dv.y);
end;

function TRoomMap.IsCellExists(const APos: TVec2i): Boolean;
begin
  Result := Distance(Vec(0,0), APos) <= FRadius;
end;

procedure TRoomMap.PutObject(const AObject: TRoomObject);
var
  i: Integer;
  v: TVec2i;
begin
  Assert(AObject <> nil);
  for i := 0 to AObject.BlockedCellsCount() - 1 do
  begin
    v := AObject.GetAbsoluteBlockedCell(i);
    FObjects.Add(v, AObject);
    FRoomUI.TileColor[v] := TTileColorID.None;
  end;
end;

procedure TRoomMap.RemoveObject(const AObject: TRoomObject);
var
  i: Integer;
  v: TVec2i;
  curr: TRoomObject;
begin
  Assert(AObject <> nil);
  for i := 0 to AObject.BlockedCellsCount() - 1 do
  begin
    v := AObject.GetAbsoluteBlockedCell(i);
    if FObjects.TryGetValue(v, curr) then
    begin
      FObjects.Delete(v);
      FRoomUI.TileColor[v] := TTileColorID.Normal;
    end;
  end;
end;

function TRoomMap.ObjectAt(const APos: TVec2i): TRoomObject;
begin
  if not FObjects.TryGetValue(APos, Result) then
    Exit(nil);
end;

{ TPlayer }

procedure TPlayer.UpdateStep;
var
  i: Integer;
begin
  inherited UpdateStep;

  for i := 0 to Length(FAnim) - 1 do
	  FAnim[i].SetTime(World.GameTime);
end;

procedure TPlayer.LoadModels();
var
  i: Integer;
begin
  AddModel('Gop_Body', mtDefault);
  AddModel('Gop_Bottoms', mtDefault);
  AddModel('Gop_Hair', mtDefault);
  AddModel('Gop_Hats', mtDefault);
  AddModel('Gop_Shoes', mtDefault);
  AddModel('Gop_Tops', mtDefault);

  SetLength(FAnim, FModels.Count);
  for i := 0 to FModels.Count - 1 do
  begin
	  FAnim[i] := Create_IavAnimationController(FModels[i].Mesh.Pose, World.GameTime);
	  FAnim[i].AnimationStart('Idle0');
  end;
  SubscribeForUpdateStep;
end;

{ TLantern }

function TLantern.BlockedCellsCount: Integer;
begin
  Result := 2;
end;

function TLantern.GetBlockedCell(AIndex: Integer): TVec2i;
begin
  case AIndex mod 2 of
    0: Result := Vec(0,0);
    1: Result := Vec(1,0);
  end;
end;

procedure TLantern.LoadModels();
begin
  AddModel('Lantern', mtDefault);
  FLight := World.Renderer.CreatePointLight();
  FLight.Pos := Vec(0.89635, 2.51368, 0.03713);
  FLight.Radius := 130;
  FLight.Color := Vec(1,1,1);
  FLight.CastShadows := True;
end;

{ TBattleRoom }

procedure TBattleRoom.EMUps(var msg: TavMessage);
begin
  FWorld.UpdateStep();
end;

procedure TBattleRoom.AfterRegister;
begin
  inherited AfterRegister;
  FWorld := TbWorld.Create(Self);
  FWorld.Renderer.OnAfterDraw := {$IfDef FPC}@{$EndIf}OnAfterWorldDraw;

  Main.Camera.At := Vec(0,0,0);
  Main.Camera.Up := Vec(0,1,0);
  Main.Camera.Eye := Main.Camera.At + Vec(10, 10, 5);

  FMap := TRoomMap.Create(FWorld);
  FMap.Radius := 20;
end;

procedure TBattleRoom.OnAfterWorldDraw(Sender: TObject);
begin
  //  Main.States.DepthTest := False;
    Main.States.Blending[0] := True;
    Main.States.SetBlendFunctions(bfSrcAlpha, bfInvSrcAlpha);
    FMap.Draw();
end;

procedure TBattleRoom.Draw();
begin
  FWorld.Renderer.PrepareToDraw;
  Main.Clear(Black, True, Main.Projection.DepthRange.y, True);
  FWorld.Renderer.DrawWorld;

  Main.ActiveFrameBuffer.BlitToWindow();
end;

procedure TBattleRoom.Generate();
var lantern: TLantern;
begin
  FLanterns := TbGameObjArr.Create();

  FWorld.Renderer.PreloadModels(['models\scene1.avm']);
  FWorld.Renderer.PreloadModels(['chars\gop.avm']);

  FFloor := TbGameObject.Create(FWorld);
  FFloor.AddModel('Floor', mtDefault);

  lantern := TLantern.Create(FMap);
  lantern.LoadModels();
  lantern.SetRoomPosDir(Vec(0,0), 0);
  FLanterns.Add(lantern);

  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(6,0), 0);
  //FLanterns.Add(lantern);


  FPlayer := TPlayer.Create(FWorld);
  FPlayer.LoadModels();
  FPlayer.Pos := Vec(5,0,5);
end;

end.


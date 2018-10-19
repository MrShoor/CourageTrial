unit untLevel;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Math,
  Classes, SysUtils, avBase, avRes, bWorld, mutils, bLights, avMesh, avTypes, avTess, avContnrs, avContnrsDefaults,
  avPathFinder;

type
  IRoomMapGraph = {$IfDef FPC}specialize{$EndIf} IMap<TVec2i>;
  IRoomMapPF  = {$IfDef FPC}specialize{$EndIf} IAStar<TVec2i>;
  TRoomMapPF  = {$IfDef FPC}specialize{$EndIf} TAStar<TVec2i>;
  IRoomPath = {$IfDef FPC}specialize{$EndIf} IArray<TVec2i>;

  IBRA_Action = interface
    function ProcessAction: Boolean;
  end;
  IBRA_ActionArr = {$IfDef FPC}specialize{$EndIf} IArray<IBRA_Action>;
  TBRA_ActionArr = {$IfDef FPC}specialize{$EndIf} TArray<IBRA_Action>;

  TRoomMap = class;
  TBattleRoom = class;

  { TRoomObject }

  TRoomObject = class (TbGameObject)
  private
    FRegistered: Boolean;
    FRoomDir: Integer;
    FRoomPos: TVec2i;
  protected
    FRoom: TRoomMap;
    function CanRegister(target: TavObject): boolean; override;
    procedure SetRoomDir(const AValue: Integer); virtual;
    procedure SetRoomPos(const AValue: TVec2i); virtual;
  public
    function BlockedCellsCount: Integer; virtual;
    function GetBlockedCell(AIndex: Integer): TVec2i; virtual;
    function GetAbsoluteBlockedCell(AIndex: Integer): TVec2i; overload;
    function GetAbsoluteBlockedCell(AIndex: Integer; APos: TVec2i; ADir: Integer): TVec2i; overload;

    property Room: TRoomMap read FRoom;
    property RoomPos: TVec2i read FRoomPos write SetRoomPos;
    property RoomDir: Integer read FRoomDir write SetRoomDir;
    procedure SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean = True); virtual;
    procedure RegisterAtRoom();

    destructor Destroy; override;
  end;
  TRoomObjectArr = {$IfDef FPC}specialize{$EndIf} TArray<TRoomObject>;
  IRoomObjectArr = {$IfDef FPC}specialize{$EndIf} IArray<TRoomObject>;
  TRoomObjectSet = {$IfDef FPC}specialize{$EndIf} THashSet<TRoomObject>;
  IRoomObjectSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TRoomObject>;

  { TRoomUnit }

  TRoomUnit = class (TRoomObject)
  private
    FAP: Integer;
    FMaxAP: Integer;

    FHP: Integer;
    FMaxHP: Integer;

    procedure SetAP(const AValue: Integer);
    procedure SetHP(const AValue: Integer);
    procedure SetMaxAP(const AValue: Integer);
    procedure SetMaxHP(const AValue: Integer);
  protected
  public
    procedure LoadModels(); virtual;

    procedure SetAnimation(const AName: string); virtual;
    function  GetUnitMoveSpeed: Single; virtual;

    function FindPath(const ATarget: TVec2i): IRoomPath;
    function FindPath(const ATarget: TVec2i; ATargetUnit: TRoomUnit): IRoomPath;

    property MaxAP: Integer read FMaxAP write SetMaxAP;
    property AP: Integer read FAP write SetAP;

    property MaxHP: Integer read FMaxHP write SetMaxHP;
    property HP: Integer read FHP write SetHP;

    procedure DealDamage(ADmg: Integer);
  end;
  TRoomUnitArr = {$IfDef FPC}specialize{$EndIf} TArray<TRoomObject>;
  IRoomUnitArr = {$IfDef FPC}specialize{$EndIf} IArray<TRoomObject>;
  TRoomUnitSet = {$IfDef FPC}specialize{$EndIf} THashSet<TRoomObject>;
  IRoomUnitSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TRoomObject>;

  {$ScopedEnums On}
  {$Z4}
  TTileColorID = (None, Normal, Selected, HighlightedGreen, HighlightedRed, Hovered);
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
    procedure ClearTileColors();

    function GetTileAtCoords(const ARay: TLine): TVec2i; overload;
    function GetTileAtCoords(const ACoord: TVec2): TVec2i; overload;

    function TilePosToWorldPos(const ATilePos: TVec2i): TVec3;

    function AutoTileColor(const APos: TVec2i): TTileColorID;

    procedure DrawUI();
  end;

  { TRoomMapGraph }

  TRoomMapGraph = class(TInterfacedObjectEx, IRoomMapGraph, IEqualityComparer)
  private
    FRoomMap: TRoomMap;

    function Hash(const Value): Cardinal;
    function IsEqual(const Left, Right): Boolean;

    function MaxNeighbourCount(const ANode: TVec2i): Integer;
    function NodeComparer: IEqualityComparer;
    function GetNeighbour(Index: Integer; const AFrom, ACurrent, ATarget: TVec2i; out ANeighbour: TVec2i; out MoveWeight, DistWeight: Single): Boolean; overload;
  protected
    function IsNonBlockingObject(const AObj: TRoomObject): Boolean; virtual;
  public
    constructor Create(const ARoomMap: TRoomMap); overload;
  end;

  { TRoomMapGraphExcludeSelfAndTarget }

  TRoomMapGraphExcludeSelfAndTarget = class(TRoomMapGraph)
  protected
    FSelf: TRoomObject;
    FTarget: TRoomObject;
    function IsNonBlockingObject(const AObj: TRoomObject): Boolean; override;
  public
    constructor Create(const ARoomMap: TRoomMap; const ASelf, ATarget: TRoomObject); overload;
  end;

  TRoomMap = class (TavMainRenderChild)
  private type
    TObjectMap = {$IFDef FPC}specialize{$EndIf} THashMap<TVec2i, TRoomObject>;
    IObjectMap = {$IFDef FPC}specialize{$EndIf} IHashMap<TVec2i, TRoomObject>;
  private
    FRadius: Integer;

    FRoomUI: TRoomUI;
    FBattleRoom: TBattleRoom;

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
    function Direction(const APt1, APt2: TVec2i): Integer;
    function IsCellExists(const APos: TVec2i): Boolean;
    function IsCellBlocked(const APos: TVec2i): Boolean;

    procedure PutObject(const AObject: TRoomObject);
    procedure RemoveObject(const AObject: TRoomObject);
    function  ObjectAt(const APos: TVec2i): TRoomObject;

    procedure AddAction(AAction: IBRA_Action);
  end;

  { TLantern }

  TLantern = class (TRoomObject)
  private const
    cLightSrcPos: TVec3 = (x: 0.89635; y: 2.51368; z: 0.03713);
  private
    FLight: IavPointLight;
  protected
    procedure SetRoomDir(const AValue: Integer); override;
    procedure SetRoomPos(const AValue: TVec2i); override;
  public
    function BlockedCellsCount: Integer; override;
    function GetBlockedCell(AIndex: Integer): TVec2i; override;
    procedure LoadModels();
    procedure SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean = True); override;
  end;

  { TPlayer }

  TPlayer = class (TRoomUnit)
  private
    FAnim: array of IavAnimationController;
  protected
    procedure UpdateStep; override;
  public
    procedure SetAnimation(const AName: string); override;
    function  GetUnitMoveSpeed: Single; override;

    procedure LoadModels(); override;
  end;

  { TBRA_Action }

  TBRA_Action = class(TInterfacedObject, IBRA_Action)
  public
    function ProcessAction: Boolean; virtual; abstract;
  end;

  { TBRA_UnitMovementAction }

  TBRA_UnitMovementAction = class(TBRA_Action)
  private
    RoomUnit: TRoomUnit;

    MovePath: IRoomPath;
    MovePathIdx: Integer;
    MovePathWeight: Single;

    MoveSpeed: Single;

    function MoveToNextCell: Boolean;
  public
    function ProcessAction: Boolean; override;
    constructor Create(const AUnit: TRoomUnit; const APath: IRoomPath);
  end;

  { TBRA_UnitDefaultAttack }

  TBRA_UnitDefaultAttack = class(TBRA_Action)
  private const
    cAPCost = 3;
  private
    FValidAction: Boolean;
    FRoomUnit   : TRoomUnit;
    FTarget     : TRoomUnit;
    FActionTime : Integer;
    FDamageStartTime: Integer;
  public
    function ProcessAction: Boolean; override;
    constructor Create(const AUnit, ATarget: TRoomUnit; const ADurationTime, ADamageStartTime: Integer);
  end;

  { TBRA_MakeDamage }

  TBRA_MakeDamage = class(TBRA_Action)
  private
    FRoomUnit  : TRoomUnit;
    FDamage    : Integer;
    FActionTime: Integer;
  public
    function ProcessAction: Boolean; override;
    constructor Create(const AUnit: TRoomUnit; const ADamage: Integer);
  end;

  { TBattleRoom }

  TBattleRoom = class (TavMainRenderChild)
  private
    FWorld: TbWorld;

    FFloor: TbGameObject;
    FLanterns: IbGameObjArr;

    FPlayer: TPlayer;

    FUnits: IRoomUnitArr;
    FActiveUnit: Integer;

    FMap: TRoomMap;
  private
    FActions: IBRA_ActionArr;
  protected
    FMovedTile: TVec2i;
    FMovePath : IRoomPath;
    function IsPlayerTurn: Boolean;
    function IsBotTurn: Boolean;
  protected
    procedure EMUps(var msg: TavMessage); message EM_UPS;
  	procedure AfterRegister; override;

    procedure OnAfterWorldDraw(Sender: TObject);
  public
    property Player: TPlayer read FPlayer;

    procedure KeyPress(KeyCode: Integer);
    procedure MouseMove(xpos, ypos: Integer);
    procedure MouseClick(button: Integer; xpos, ypos: Integer);
    procedure EndTurn();

    procedure AddAction(AAction: IBRA_Action);

    procedure Draw();
    procedure Generate();
  end;

implementation

uses
  untEnemies, untGraphics;

{ TBRA_MakeDamage }

function TBRA_MakeDamage.ProcessAction: Boolean;
begin
  Result := FRoomUnit.World.GameTime < FActionTime;
  if not Result then
    FRoomUnit.SetAnimation('Idle0');
end;

constructor TBRA_MakeDamage.Create(const AUnit: TRoomUnit; const ADamage: Integer);
begin
  Assert(AUnit <> nil);
  FRoomUnit := AUnit;
  FActionTime := FRoomUnit.World.GameTime + 1000;
  FDamage := ADamage;

  FRoomUnit.SetAnimation('React0');
  FRoomUnit.DealDamage(FDamage);
end;

{ TBRA_UnitDefaultAttack }

function TBRA_UnitDefaultAttack.ProcessAction: Boolean;
begin
  if not FValidAction then Exit(False);

  if FRoomUnit.World.GameTime > FDamageStartTime then
  begin
    FDamageStartTime := HUGE;
    FTarget.Room.AddAction(TBRA_MakeDamage.Create(FTarget, 5));
  end;

  Result := FRoomUnit.World.GameTime < FActionTime;
  if not Result then
  begin
    FRoomUnit.SetAnimation('Idle0');
  end;
end;

constructor TBRA_UnitDefaultAttack.Create(const AUnit, ATarget: TRoomUnit; const ADurationTime, ADamageStartTime: Integer);
begin
  Assert(AUnit <> nil);
  Assert(ATarget <> nil);
  FValidAction := (AUnit.AP >= cAPCost) and (AUnit.Room.Distance(AUnit.RoomPos, ATarget.RoomPos) = 1);

  FRoomUnit := AUnit;
  FTarget := ATarget;
  FActionTime := FRoomUnit.World.GameTime + ADurationTime;
  FDamageStartTime := FRoomUnit.World.GameTime + ADamageStartTime;

  if FValidAction then
  begin
    FRoomUnit.AP := FRoomUnit.AP - cAPCost;
    FRoomUnit.SetAnimation('Attack0');
    FRoomUnit.RoomDir := FRoomUnit.Room.Direction(FRoomUnit.RoomPos, FTarget.RoomPos);
  end;
end;

{ TBRA_UnitMovementAction }

function TBRA_UnitMovementAction.MoveToNextCell: Boolean;
begin
  RoomUnit.RoomPos := MovePath[MovePathIdx];
  RoomUnit.AP := RoomUnit.AP - 1;
  if RoomUnit.AP <= 0 then
  begin
    RoomUnit.SetAnimation('Idle0');
    Exit(False);
  end;

  if MovePath = nil then Exit(False);

  Inc(MovePathIdx);
  if MovePathIdx > MovePath.Count - 1 then
  begin
    RoomUnit.SetAnimation('Idle0');
    Exit(False);
  end;

  RoomUnit.RoomDir := RoomUnit.Room.Direction(RoomUnit.RoomPos, MovePath[MovePathIdx]);

  MovePathWeight := 0;
end;

function TBRA_UnitMovementAction.ProcessAction: Boolean;
var fromPt, toPt: TVec3;
begin
  if MovePath = nil then Exit(False);
  if MovePath.Count = 0 then Exit(False);
  if RoomUnit.AP <= 0 then Exit(False);

  Result := True;
  MovePathWeight := MovePathWeight + RoomUnit.Main.UpdateStatesInterval / 1000 * MoveSpeed;
  if MovePathWeight >= 1 then
    if not MoveToNextCell then Exit(False);

  fromPt := RoomUnit.Room.UI.TilePosToWorldPos(RoomUnit.RoomPos);
  toPt := RoomUnit.Room.UI.TilePosToWorldPos(MovePath[MovePathIdx]);
  RoomUnit.Pos := Lerp(fromPt, toPt, MovePathWeight);
end;

constructor TBRA_UnitMovementAction.Create(const AUnit: TRoomUnit; const APath: IRoomPath);
begin
  RoomUnit := AUnit;
  MovePathIdx := 0;
  MovePathWeight := 0;
  MovePath := APath;
  MoveSpeed := RoomUnit.GetUnitMoveSpeed();
  if (MovePath <> nil) and (RoomUnit <> nil) and (RoomUnit.AP > 0) then
  begin
    RoomUnit.RoomDir := RoomUnit.Room.Direction(RoomUnit.RoomPos, MovePath[0]);
    RoomUnit.SetAnimation('Walk');
  end;
end;

{ TRoomMapGraphExcludeSelfAndTarget }

function TRoomMapGraphExcludeSelfAndTarget.IsNonBlockingObject(const AObj: TRoomObject): Boolean;
begin
  Result := (AObj = FSelf) or (AObj = nil) or (AObj = FTarget);
end;

constructor TRoomMapGraphExcludeSelfAndTarget.Create(const ARoomMap: TRoomMap;
  const ASelf, ATarget: TRoomObject);
begin
  Create(ARoomMap);
  FSelf := ASelf;
  FTarget := ATarget;
end;

{ TRoomMapGraph }

function TRoomMapGraph.Hash(const Value): Cardinal;
begin
  Result := Murmur2DefSeed(Value, SizeOf(TVec2i));
end;

function TRoomMapGraph.IsEqual(const Left, Right): Boolean;
var nl: TVec2i absolute Left;
    nr: TVec2i absolute Right;
begin
  Result := (nl.x = nr.x) and (nl.y = nr.y);
end;

function TRoomMapGraph.MaxNeighbourCount(const ANode: TVec2i): Integer;
begin
  Result := 6;
end;

function TRoomMapGraph.NodeComparer: IEqualityComparer;
begin
  Result := Self;
end;

function TRoomMapGraph.GetNeighbour(Index: Integer; const AFrom, ACurrent, ATarget: TVec2i; out ANeighbour: TVec2i; out MoveWeight, DistWeight: Single): Boolean;
begin
  ANeighbour := FRoomMap.NeighbourTile(ACurrent, Index);
  Result := FRoomMap.IsCellExists(ANeighbour);
  if not Result then Exit;
  Result := IsNonBlockingObject(FRoomMap.ObjectAt(ANeighbour));
  if not Result then Exit;
  MoveWeight := 1;
  MoveWeight := MoveWeight - dot( (ACurrent - AFrom), (ANeighbour - ACurrent) )*0.01 + 0.01;
  DistWeight := FRoomMap.Distance(ANeighbour, ATarget);
end;

function TRoomMapGraph.IsNonBlockingObject(const AObj: TRoomObject): Boolean;
begin
  Result := AObj = nil;
end;

constructor TRoomMapGraph.Create(const ARoomMap: TRoomMap);
begin
  FRoomMap := ARoomMap;
end;

{ TRoomUnit }

procedure TRoomUnit.SetMaxAP(const AValue: Integer);
begin
  if FMaxAP = AValue then Exit;
  FMaxAP := AValue;
end;

procedure TRoomUnit.SetMaxHP(const AValue: Integer);
begin
  if FMaxHP = AValue then Exit;
  FMaxHP := AValue;
end;

procedure TRoomUnit.LoadModels;
begin

end;

procedure TRoomUnit.SetAnimation(const AName: string);
begin

end;

function TRoomUnit.GetUnitMoveSpeed: Single;
begin
  Result := 1;
end;

function TRoomUnit.FindPath(const ATarget: TVec2i): IRoomPath;
var graph: IRoomMapGraph;
    pf: IRoomMapPF;
begin
  graph := TRoomMapGraphExcludeSelfAndTarget.Create(Room, Self, nil);
  pf := TRoomMapPF.Create(graph);
  Result := pf.FindPath(RoomPos, ATarget, Infinity, False);
end;

function TRoomUnit.FindPath(const ATarget: TVec2i; ATargetUnit: TRoomUnit): IRoomPath;
var graph: IRoomMapGraph;
    pf: IRoomMapPF;
begin
  graph := TRoomMapGraphExcludeSelfAndTarget.Create(Room, Self, ATargetUnit);
  pf := TRoomMapPF.Create(graph);
  Result := pf.FindPath(RoomPos, ATarget, Infinity, False);
end;

procedure TRoomUnit.DealDamage(ADmg: Integer);
var msg: TbFlyOutMessage;
begin
  FHP := FHP - ADmg;

  msg := TbFlyOutMessage.Create(World);
  msg.SetState(Pos+Vec(0,1.5,0), IntToStr(ADmg), Vec(1,0,0,1));
end;

procedure TRoomUnit.SetAP(const AValue: Integer);
begin
  if FAP = AValue then Exit;
  FAP := AValue;
end;

procedure TRoomUnit.SetHP(const AValue: Integer);
begin
  if FHP = AValue then Exit;
  FHP := AValue;
end;

{ TRoomObject }

procedure TRoomObject.SetRoomPos(const AValue: TVec2i);
begin
  if FRoomPos = AValue then Exit;
  if FRegistered then FRoom.RemoveObject(Self);
  FRoomPos := AValue;
  if FRegistered then FRoom.PutObject(Self);
  Pos := FRoom.UI.TilePosToWorldPos(AValue);
end;

procedure TRoomObject.SetRoomDir(const AValue: Integer);
begin
  if FRoomDir = AValue then Exit;
  if FRegistered then FRoom.RemoveObject(Self);
  FRoomDir := AValue;
  if FRegistered then FRoom.PutObject(Self);
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
var dirmod: Integer;
    v: TVec2i;
begin
  dirmod := ADir mod 6;
  if dirmod < 0 then dirmod := dirmod + 6;
  v := GetBlockedCell(AIndex);
  case dirmod of
    0: Result := v;
    1:
      begin
        Result.x := v.x + v.y;
        Result.y := -v.x;
      end;
    2:
      begin
        Result.x := v.y;
        Result.y := -v.x - v.y;
      end;
    3:
      begin
        Result.x := -v.x;
        Result.y := -v.y;
      end;
    4:
      begin
        Result.x := -v.x - v.y;
        Result.y := v.x;
      end;
    5:
      begin
        Result.x := -v.y;
        Result.y := v.x + v.y;
      end;
    else
      Assert(False);
      Result := v;
  end;
  Result := Result + APos;
end;

procedure TRoomObject.SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean);
begin
  if FRegistered then FRoom.RemoveObject(Self);
  FRoomPos := APos;
  FRoomDir := ADir;
  if FRegistered then FRoom.PutObject(Self);
  if AAutoRegister then RegisterAtRoom();
  Pos := FRoom.UI.TilePosToWorldPos(APos);
  Rot := Quat(Vec(0, 1, 0), 2 * Pi * (ADir / 6));
end;

procedure TRoomObject.RegisterAtRoom;
begin
  if FRegistered then Exit;
  FRegistered := True;
  FRoom.PutObject(Self);
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
  //FAffinePack.Row[1] := Vec(0,1);
  FAffinePackInv := Inv(FAffinePack);

  FColors[TTileColorID.Normal] := Vec(0,0,0,1);
  FColors[TTileColorID.HighlightedGreen] := Vec(0,0.5,0,1);
  FColors[TTileColorID.HighlightedRed] := Vec(0.5,0,0,1);
  FColors[TTileColorID.Hovered] := Vec(1,1,1,1);
end;

procedure TRoomUI.ClearTileColors;
var
  j, i: Integer;
  n: Integer;
  w: Integer;
begin
  w := FRadius * 2 + 1;
  n := 0;
  for j := -FRadius to FRadius do
    for i := -FRadius to FRadius do
    begin
      PHexTile(FTilesData.PItem[n])^.vsColor := AutoTileColor(Vec(i, j));
      Inc(n);
    end;
  FTilesVB.Invalidate;
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
  v := ATilePos * FAffinePack;
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
begin
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

function TRoomMap.Direction(const APt1, APt2: TVec2i): Integer;
var p1, p2: TVec3;
    dir: TVec2;
    angle: Single;
begin
  p1 := UI.TilePosToWorldPos(APt1);
  p2 := UI.TilePosToWorldPos(APt2);
  dir := Vec(p2.x, p2.z) - Vec(p1.x, p1.z);
  angle := -(arctan2(dir.y, dir.x)) / (2 * Pi) * 6;
  if angle < 0 then angle := angle + 6;
  Result := Round(angle);
end;

function TRoomMap.IsCellExists(const APos: TVec2i): Boolean;
begin
  Result := Distance(Vec(0,0), APos) <= FRadius;
end;

function TRoomMap.IsCellBlocked(const APos: TVec2i): Boolean;
begin
  if not IsCellExists(APos) then Exit(False);
  Result := ObjectAt(APos) <> nil;
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

procedure TRoomMap.AddAction(AAction: IBRA_Action);
begin
  FBattleRoom.AddAction(AAction);
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

procedure TPlayer.SetAnimation(const AName: string);
var
  i: Integer;
begin
  inherited SetAnimation(AName);
  for i := 0 to Length(FAnim) - 1 do
	  FAnim[i].AnimationStartAndStopOther([AName]);
end;

function TPlayer.GetUnitMoveSpeed: Single;
begin
  Result := 2;
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
	  FAnim[i].AnimationStartAndStopOther(['Idle0']);
  end;
  SubscribeForUpdateStep;

  Scale := 0.5;
  MaxAP := 10;
  MaxHP := 100;
  HP := MaxHP;
end;

{ TLantern }

procedure TLantern.SetRoomDir(const AValue: Integer);
begin
  inherited SetRoomDir(AValue);
  FLight.Pos := cLightSrcPos * Transform();
end;

procedure TLantern.SetRoomPos(const AValue: TVec2i);
begin
  inherited SetRoomPos(AValue);
  FLight.Pos := cLightSrcPos * Transform();
end;

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
  FLight.Pos := cLightSrcPos;
  FLight.Radius := 30;
  FLight.Color := Vec(1,1,1);
  //FLight.CastShadows := True;
end;

procedure TLantern.SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean);
begin
  inherited SetRoomPosDir(APos, ADir, AAutoRegister);
  FLight.Pos := cLightSrcPos * Transform();
end;

{ TBattleRoom }

function TBattleRoom.IsPlayerTurn: Boolean;
begin
  Result := FUnits[FActiveUnit] = FPlayer;
end;

function TBattleRoom.IsBotTurn: Boolean;
begin
  Result := FUnits[FActiveUnit] is TBot;
end;

procedure TBattleRoom.EMUps(var msg: TavMessage);
var bot: TBot;
    new_action: IBRA_Action;
    i: LongInt;
begin
  FWorld.UpdateStep();

  if IsPlayerTurn then
  begin
    FMovedTile := FMap.UI.GetTileAtCoords(Main.Cursor.Ray);
    FMovePath := nil;
    if FPlayer <> nil then
    begin
      FMovePath := FPlayer.FindPath(FMovedTile);
      if FMovePath <> nil then FMovePath.Add(FMovedTile);
    end;
  end;

  if (FActions.Count = 0) and IsBotTurn() then
  begin
    bot := FUnits[FActiveUnit] as TBot;
    new_action := bot.DoAction();
    if new_action = nil then
      EndTurn()
    else
      FActions.Add(new_action);
  end;

  for i := FActions.Count - 1 downto 0 do
  begin
    if not FActions[i].ProcessAction() then
      FActions.DeleteWithSwap(i);
  end;
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
  FMap.FBattleRoom := Self;
end;

procedure TBattleRoom.OnAfterWorldDraw(Sender: TObject);
begin
  //  Main.States.DepthTest := False;
    Main.States.Blending[0] := True;
    Main.States.SetBlendFunctions(bfSrcAlpha, bfInvSrcAlpha);
    FMap.Draw();
end;

procedure TBattleRoom.KeyPress(KeyCode: Integer);
begin
  if not IsPlayerTurn() then Exit;
  if (KeyCode = Ord('E')) then
    EndTurn();
end;

procedure TBattleRoom.MouseMove(xpos, ypos: Integer);
begin
  if not IsPlayerTurn() then Exit;
end;

procedure TBattleRoom.MouseClick(button: Integer; xpos, ypos: Integer);
var obj: TRoomObject;
    new_action: IBRA_Action;
begin
  if not IsPlayerTurn() then Exit;

  if (button = 0) and (FActions.Count = 0) then
  begin
    obj := FMap.ObjectAt(FMovedTile);
    if obj = nil then
    begin
      new_action := TBRA_UnitMovementAction.Create(FPlayer, FMovePath);
    end;
    if obj is TRoomUnit then
    begin
      new_action := TBRA_UnitDefaultAttack.Create(FPlayer, obj as TRoomUnit, 1000, 300);
    end;
    if new_action <> nil then
      FActions.Add(new_action);
  end;
end;

procedure TBattleRoom.EndTurn;
var unt: TRoomUnit;
begin
  FActiveUnit := (FActiveUnit + 1) mod FUnits.Count;
  unt := FUnits[FActiveUnit] as TRoomUnit;
  unt.AP := unt.MaxAP;
end;

procedure TBattleRoom.AddAction(AAction: IBRA_Action);
begin
  FActions.Add(AAction);
end;

procedure TBattleRoom.Draw();

  procedure DrawTileMap;
  var
    i: Integer;
  begin
    FMap.UI.ClearTileColors();
    if IsPlayerTurn then
    begin
      if FMovePath <> nil then
        for i := 0 to FMovePath.Count - 1 do
        begin
          if i >= FPlayer.AP then
            FMap.UI.TileColor[FMovePath[i]] := TTileColorID.HighlightedRed
          else
            FMap.UI.TileColor[FMovePath[i]] := TTileColorID.HighlightedGreen;
        end;
      FMap.UI.TileColor[FMovedTile] := TTileColorID.Hovered;
    end;
  end;

begin
  DrawTileMap;

  FWorld.Renderer.PrepareToDraw;
  Main.Clear(Black, True, Main.Projection.DepthRange.y, True);
  FWorld.Renderer.DrawWorld;

  Main.ActiveFrameBuffer.BlitToWindow();
end;

procedure TBattleRoom.Generate();
var lantern: TLantern;
    bot: TBot;
begin
  FLanterns := TbGameObjArr.Create();
  FUnits := TRoomUnitArr.Create();
  FActions := TBRA_ActionArr.Create();

  FWorld.Renderer.PreloadModels(['models\scene1.avm']);
  FWorld.Renderer.PreloadModels(['chars\gop.avm']);
  FWorld.Renderer.PreloadModels(['enemies\creature1.avm']);

  FFloor := TbGameObject.Create(FWorld);
  FFloor.AddModel('Floor', mtDefault);

  lantern := TLantern.Create(FMap);
  lantern.LoadModels();
  lantern.SetRoomPosDir(Vec(0,0), 0);
  FLanterns.Add(lantern);

  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(3,0), 1);
  //FLanterns.Add(lantern);
  //
  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(6,0), 2);
  //FLanterns.Add(lantern);
  //
  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(9,0), 3);
  //FLanterns.Add(lantern);
  //
  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(12,0), 4);
  //FLanterns.Add(lantern);

  lantern := TLantern.Create(FMap);
  lantern.LoadModels();
  lantern.SetRoomPosDir(Vec(15,0), 5);
  FLanterns.Add(lantern);

  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(0,3), 1);
  //FLanterns.Add(lantern);
  //
  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(0,6), 2);
  //FLanterns.Add(lantern);
  //
  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(0,9), 3);
  //FLanterns.Add(lantern);
  //
  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(0,12), 4);
  //FLanterns.Add(lantern);
  //
  //lantern := TLantern.Create(FMap);
  //lantern.LoadModels();
  //lantern.SetRoomPosDir(Vec(0,15), 5);
  //FLanterns.Add(lantern);

  FPlayer := TPlayer.Create(FMap);
  //FPlayer.SetRoomPosDir(Vec(5, 5), 0);
  FPlayer.SetRoomPosDir(Vec(2, 1), 0);
  FPlayer.LoadModels();
  FUnits.Add(FPlayer);

  bot := TBotMutant1.Create(FMap);
  bot.SetRoomPosDir(Vec(-4, 4), 0);
  bot.LoadModels();
  FUnits.Add(bot);

  FActiveUnit := FUnits.Count - 1;
  EndTurn();
end;

end.


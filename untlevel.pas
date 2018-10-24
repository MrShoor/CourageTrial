unit untLevel;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Graphics,
  Math,
  Classes, SysUtils, avBase, avRes, bWorld, mutils, bLights, avMesh, avTypes, avTess, avContnrs, avContnrsDefaults,
  avPathFinder, avMiniControls, avModel, avTexLoader,
  untObstacles;

type
  IRoomMapGraph = {$IfDef FPC}specialize{$EndIf} IMap<TVec2i>;
  IRoomMapPF  = {$IfDef FPC}specialize{$EndIf} IAStar<TVec2i>;
  TRoomMapPF  = {$IfDef FPC}specialize{$EndIf} TAStar<TVec2i>;
  IRoomPath = {$IfDef FPC}specialize{$EndIf} IArray<TVec2i>;
  TRoomPath = {$IfDef FPC}specialize{$EndIf} TArray<TVec2i>;

  IBRA_Action = interface
    procedure TryCancel;
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
    function BlockedViewCell(AIndex: Integer): Boolean; virtual;
    function GetAbsoluteBlockedCell(AIndex: Integer): TVec2i; overload;
    function GetAbsoluteBlockedCell(AIndex: Integer; APos: TVec2i; ADir: Integer): TVec2i; overload;
    class function RotateTileCoord(const APos: TVec2i; ADir: Integer): TVec2i;

    property Room: TRoomMap read FRoom;
    property RoomPos: TVec2i read FRoomPos write SetRoomPos;
    property RoomDir: Integer read FRoomDir write SetRoomDir;
    procedure SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean = True); virtual;
    property Registred: Boolean read FRegistered;
    procedure RegisterAtRoom();
    procedure UnregisterAtRoom();

    destructor Destroy; override;
  end;
  TRoomObjectArr = {$IfDef FPC}specialize{$EndIf} TArray<TRoomObject>;
  IRoomObjectArr = {$IfDef FPC}specialize{$EndIf} IArray<TRoomObject>;
  TRoomObjectSet = {$IfDef FPC}specialize{$EndIf} THashSet<TRoomObject>;
  IRoomObjectSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TRoomObject>;

  TRoomObjectClass = class of TRoomObject;

  { TRoomUnit }

  TRoomUnit = class (TRoomObject)
  private
    FAP: Integer;
    FMaxAP: Integer;

    FHP: Integer;
    FMaxHP: Integer;
    FPreview96_128: string;

    FViewAngle: Single;
    FViewRange: Single;
    FViewWholeRange: Single;

    procedure SetAP(const AValue: Integer);
    procedure SetHP(const AValue: Integer);
    procedure SetMaxAP(const AValue: Integer);
    procedure SetMaxHP(const AValue: Integer);
    procedure SetPreview96_128(const AValue: string);
  protected
    procedure OnRegisterRoomObject(const ANewObject: TRoomObject); virtual;
  public
    property Preview96_128: string read FPreview96_128 write SetPreview96_128;

    procedure LoadModels(); virtual;

    procedure SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean); virtual;
    function  GetUnitMoveSpeed: Single; virtual;

    function FindPath(const ATarget: TVec2i): IRoomPath;
    function FindPath(const ATarget: TVec2i; ATargetUnit: TRoomUnit): IRoomPath;

    function IsDead(): Boolean;

    property MaxAP: Integer read FMaxAP write SetMaxAP;
    property AP: Integer read FAP write SetAP;

    property MaxHP: Integer read FMaxHP write SetMaxHP;
    property HP: Integer read FHP write SetHP;

    property ViewAngle: Single read FViewAngle write FViewAngle;
    property ViewRange: Single read FViewRange write FViewRange;
    property ViewWholeRange: Single read FViewWholeRange write FViewWholeRange;
    function InViewField(const APt: TVec2i): Boolean;
    function CanSee(const APos: TVec2i): Boolean;
    function CanSee(const AOtherUnit: TRoomUnit): Boolean;
    function GetVisible(): Boolean; override;

    procedure DealDamage(ADmg: Integer);
  end;
  TRoomUnitArr = {$IfDef FPC}specialize{$EndIf} TArray<TRoomObject>;
  IRoomUnitArr = {$IfDef FPC}specialize{$EndIf} IArray<TRoomObject>;
  TRoomUnitSet = {$IfDef FPC}specialize{$EndIf} THashSet<TRoomObject>;
  IRoomUnitSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TRoomObject>;

  {$ScopedEnums On}
  {$Z4} //this enum must be 4 bytes for directly passing to shaders
  TTileColorID = (None, Normal, Selected, HighlightedGreen, HighlightedRed, HighlightedYellow, Hovered);
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
    FTilesVBFogOfWar  : TavVB;
    FTilesDataFogOfWar: IHexTiles;
    FAffinePack: TMat2;
    FAffinePackInv: TMat2;

    FFogOfWarValid: Boolean;

    FColors: array [TTileColorID] of TVec4;
    FColorsFogOfWar: array [TTileColorID] of TVec4;
    function  GetColors(ID: TTileColorID): TVec4;
    procedure SetColors(ID: TTileColorID; const AValue: TVec4);
    procedure ValidateFogOfWar;
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

    procedure InvalidateFogOfWar;

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
    function IsNonBlockingObject(AObj: TRoomObject): Boolean; virtual;
  public
    constructor Create(const ARoomMap: TRoomMap); overload;
  end;

  { TRoomMapGraphExcludeSelfAndTarget }

  TRoomMapGraphExcludeSelfAndTarget = class(TRoomMapGraph)
  protected
    FSelf: TRoomObject;
    FTarget: TRoomObject;
    function IsNonBlockingObject(AObj: TRoomObject): Boolean; override;
  public
    constructor Create(const ARoomMap: TRoomMap; const ASelf, ATarget: TRoomObject); overload;
  end;

  TRoomMap = class (TavMainRenderChild)
  private type
    TObjectMap = {$IFDef FPC}specialize{$EndIf} THashMap<TVec2i, TRoomObject>;
    IObjectMap = {$IFDef FPC}specialize{$EndIf} IHashMap<TVec2i, TRoomObject>;
  private
    FCurrentPlayer: TRoomUnit;

    FRadius: Integer;

    FRoomUI: TRoomUI;
    FBattleRoom: TBattleRoom;

    FObjects: IObjectMap;

    procedure SetRadius(const AValue: Integer);
    function NextPointOnRay(const APt: TVec2i; const ADir, ADirStep: TVec2i): TVec2i;
  protected
    procedure AfterRegister; override;
  public
    property UI: TRoomUI read FRoomUI;

    property Radius: Integer read FRadius write SetRadius;

    procedure Draw();

    function NeighbourTile(const ACurrent: TVec2i; const AIndex: Integer): TVec2i;
    function Distance(const APt1, APt2: TVec2i): Integer;
    function Direction(const APt1, APt2: TVec2i): Integer;
    function RayCast(const APt1, APt2: TVec2i; const AllowHitBlockers: Boolean): IRoomPath;
    function RayCastBoolean(const APt1, APt2: TVec2i): Boolean;
    function IsCellExists(const APos: TVec2i): Boolean;
    function IsCellBlocked(const APos: TVec2i): Boolean;
    function IsCellBlockView(const APos: TVec2i): Boolean;

    procedure PutObject(const AObject: TRoomObject);
    procedure RemoveObject(const AObject: TRoomObject);
    function  ObjectAt(const APos: TVec2i): TRoomObject;

    procedure AddAction(AAction: IBRA_Action);

    property CurrentPlayer: TRoomUnit read FCurrentPlayer write FCurrentPlayer;
  end;

  { TObstacle }

  TObstacle = class (TRoomObject)
  private
    FObstacle: TObstacleDesc;
  public
    property Obstacle: TObstacleDesc read FObstacle;

    function BlockedCellsCount: Integer; override;
    function GetBlockedCell(AIndex: Integer): TVec2i; override;
    function BlockedViewCell(AIndex: Integer): Boolean; override;
    procedure LoadModels(const AObstacle: TObstacleDesc); virtual;

    procedure WriteStream(const AStream: TStream); virtual;
    procedure ReadStream(const AStream: TStream); virtual;
  end;

  { TLantern }

  TLantern = class (TObstacle)
  private const
    cLightSrcPos: TVec3 = (x: 0.89635; y: 2.51368; z: 0.03713);
  private
    FLight: IavPointLight;
  protected
    procedure SetRoomDir(const AValue: Integer); override;
    procedure SetRoomPos(const AValue: TVec2i); override;
  public
    procedure LoadModels(const AObstacle: TObstacleDesc); override;
    procedure SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean = True); override;
  end;

  { TPlayer }

  TPlayer = class (TRoomUnit)
  private
    FAnim: array of IavAnimationController;
  protected
    procedure UpdateStep; override;
  public
    procedure SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean); override;
    function  GetUnitMoveSpeed: Single; override;

    procedure LoadModels(); override;
  end;

  { TBRA_Action }

  TBRA_Action = class(TInterfacedObject, IBRA_Action)
  public
    procedure TryCancel; virtual;
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

    Cancelled: Boolean;

    function MoveToNextCell: Boolean;
  public
    procedure TryCancel; override;
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
    class function CanUse(const AUnit, ATarget: TRoomUnit): Boolean;

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
    FObstacles: IObstacleArr;
    FLanterns: IbGameObjArr;

    FPlayer: TPlayer;

    FUnits: IRoomUnitArr;
    FActiveUnit: Integer;

    FMap: TRoomMap;

    FUnitMenu: TavmCustomControl;
    procedure OnEndTurnBtnClick(ASender: TObject);
  private
    FActions: IBRA_ActionArr;
  protected
    FMovedTile: TVec2i;
    FMovePath : IRoomPath;
    FRayPath : IRoomPath;

    FEmptyLight: IavPointLight;

    function IsPlayerTurn: Boolean;
    function IsBotTurn: Boolean;
    function IsMouseOnUI: Boolean;
  protected
    procedure EMUps(var msg: TavMessage); message EM_UPS;
    procedure AfterRegister; override;

    procedure OnAfterWorldDraw(Sender: TObject);

    procedure PreloadModels;
  public
    property Player: TPlayer read FPlayer;
    property Obstacles: IObstacleArr read FObstacles;
    property Map: TRoomMap read FMap;

    procedure KeyPress(KeyCode: Integer);
    procedure MouseMove(xpos, ypos: Integer);
    procedure MouseClick(button: Integer; xpos, ypos: Integer);
    procedure EndTurn();

    procedure AddAction(AAction: IBRA_Action);

    procedure Draw();
    procedure Generate();
    procedure GenerateWithLoad(const AFileName: string);
    procedure GenerateEmpty();
    procedure DrawObstaclePreview(const AName: String; const bmp: TBitmap);

    procedure SaveRoomMap(const AStream: TStream);
    procedure LoadRoomMap(const AStream: TStream);

    function CreateRoomObject(const AClass: TRoomObjectClass): TRoomObject;
  end;

function CanPlaceObstacle(const ARoom: TRoomMap; const AObstacle: TObstacleDesc; const APos: TVec2i; const ADir: Integer): Boolean;

procedure RegRoomClass(const ARoomClass: TRoomObjectClass);
function  FindRoomClass(const AName: string): TRoomObjectClass;

implementation

uses
  untEnemies, untGraphics, ui_unit;

type
  IRoomClassesMap = {$IfDef FPC}specialize{$EndIf}IHashMap<string, TRoomObjectClass>;
  TRoomClassesMap = {$IfDef FPC}specialize{$EndIf}THashMap<string, TRoomObjectClass>;

var
  gvRegRommClasses: IRoomClassesMap;

function CanPlaceObstacle(const ARoom: TRoomMap; const AObstacle: TObstacleDesc; const APos: TVec2i; const ADir: Integer): Boolean;
var cell: TVec2i;
    i: Integer;
begin
  for i := 0 to AObstacle.cells.Count - 1 do
  begin
    cell := AObstacle.cells[i].xy;
    cell := TRoomObject.RotateTileCoord(cell, ADir) + APos;
    if not ARoom.IsCellExists(cell) then Exit(False);
    if ARoom.ObjectAt(cell) <> nil then Exit(False);
  end;
  Result := True;
end;

procedure RegRoomClass(const ARoomClass: TRoomObjectClass);
begin
  if gvRegRommClasses = nil then gvRegRommClasses := TRoomClassesMap.Create();
  gvRegRommClasses.AddOrSet(ARoomClass.ClassName, ARoomClass);
end;

function FindRoomClass(const AName: string): TRoomObjectClass;
begin
  if gvRegRommClasses = nil then Exit(nil);
  if not gvRegRommClasses.TryGetValue(AName, Result) then Result := nil;
end;

{ TObstacle }

function TObstacle.BlockedCellsCount: Integer;
begin
  Result := FObstacle.cells.Count;
end;

function TObstacle.GetBlockedCell(AIndex: Integer): TVec2i;
begin
  Result := FObstacle.cells[AIndex].xy;
end;

function TObstacle.BlockedViewCell(AIndex: Integer): Boolean;
begin
  Result := FObstacle.cells[AIndex].z <> 0;
end;

procedure TObstacle.LoadModels(const AObstacle: TObstacleDesc);
begin
  FObstacle := AObstacle;
  AddModel(FObstacle.name, mtDefault);
end;

procedure TObstacle.WriteStream(const AStream: TStream);
begin
  FObstacle.WriteStream(AStream);
  AStream.WriteBuffer(FRoomPos, SizeOf(FRoomPos));
  AStream.WriteBuffer(FRoomDir, SizeOf(FRoomDir));
end;

procedure TObstacle.ReadStream(const AStream: TStream);
var rPos: TVec2i;
    rDir: Integer;
begin
  rPos := Vec(0,0);
  rDir := 0;
  UnregisterAtRoom();
  FModels.Clear();

  FObstacle.ReadStream(AStream);
  AStream.ReadBuffer(rPos, SizeOf(rPos));
  AStream.ReadBuffer(rDir, SizeOf(rDir));

  LoadModels(FObstacle);
  SetRoomPosDir(rPos, rDir);
end;

{ TBRA_Action }

procedure TBRA_Action.TryCancel;
begin

end;

{ TBRA_MakeDamage }

function TBRA_MakeDamage.ProcessAction: Boolean;
begin
  Result := FRoomUnit.World.GameTime < FActionTime;
end;

constructor TBRA_MakeDamage.Create(const AUnit: TRoomUnit; const ADamage: Integer);
begin
  Assert(AUnit <> nil);
  FRoomUnit := AUnit;
  FActionTime := FRoomUnit.World.GameTime + 1000;
  FDamage := ADamage;

  FRoomUnit.DealDamage(FDamage);
  if FRoomUnit.HP <= 0 then
    FRoomUnit.SetAnimation(['React0', 'Death0'], False)
  else
    FRoomUnit.SetAnimation(['React0', 'Idle0'], True);
end;

{ TBRA_UnitDefaultAttack }

class function TBRA_UnitDefaultAttack.CanUse(const AUnit, ATarget: TRoomUnit): Boolean;
begin
  if ATarget = nil then Exit(False);
  Result := (AUnit.AP >= cAPCost) and (AUnit.Room.Distance(AUnit.RoomPos, ATarget.RoomPos) = 1) and (not ATarget.IsDead());
end;

function TBRA_UnitDefaultAttack.ProcessAction: Boolean;
begin
  if not FValidAction then Exit(False);

  if FRoomUnit.World.GameTime > FDamageStartTime then
  begin
    FDamageStartTime := HUGE;
    FTarget.Room.AddAction(TBRA_MakeDamage.Create(FTarget, 50));
  end;

  Result := FRoomUnit.World.GameTime < FActionTime;
end;

constructor TBRA_UnitDefaultAttack.Create(const AUnit, ATarget: TRoomUnit; const ADurationTime, ADamageStartTime: Integer);
begin
  Assert(AUnit <> nil);
  Assert(ATarget <> nil);
  FValidAction := (AUnit.AP >= cAPCost) and (AUnit.Room.Distance(AUnit.RoomPos, ATarget.RoomPos) = 1) and (not ATarget.IsDead());

  FRoomUnit := AUnit;
  FTarget := ATarget;
  FActionTime := FRoomUnit.World.GameTime + ADurationTime;
  FDamageStartTime := FRoomUnit.World.GameTime + ADamageStartTime;

  if FValidAction then
  begin
    FRoomUnit.AP := FRoomUnit.AP - cAPCost;
    FRoomUnit.SetAnimation(['Attack0', 'Idle0'], True);
    FRoomUnit.RoomDir := FRoomUnit.Room.Direction(FRoomUnit.RoomPos, FTarget.RoomPos);
  end;
end;

{ TBRA_UnitMovementAction }

function TBRA_UnitMovementAction.MoveToNextCell: Boolean;
begin
  RoomUnit.RoomPos := MovePath[MovePathIdx];
  RoomUnit.AP := RoomUnit.AP - 1;
  if RoomUnit.AP <= 0 then Exit(False);
  if MovePath = nil then Exit(False);
  if Cancelled then Exit(False);

  Inc(MovePathIdx);
  if MovePathIdx > MovePath.Count - 1 then Exit(False);

  if RoomUnit.Room.ObjectAt(MovePath[MovePathIdx]) <> nil then Exit(False);

  RoomUnit.RoomDir := RoomUnit.Room.Direction(RoomUnit.RoomPos, MovePath[MovePathIdx]);

  MovePathWeight := 0;
end;

procedure TBRA_UnitMovementAction.TryCancel;
begin
  Cancelled := True;
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
    if not MoveToNextCell then
    begin
      RoomUnit.SetAnimation(['Idle0'], True);
      Exit(False);
    end;

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
    RoomUnit.SetAnimation(['Walk'], True);
  end;
end;

{ TRoomMapGraphExcludeSelfAndTarget }

function TRoomMapGraphExcludeSelfAndTarget.IsNonBlockingObject(AObj: TRoomObject): Boolean;
begin
  if (FSelf <> nil) and
     (AObj is TRoomUnit) and
     (FSelf.Room.CurrentPlayer = FSelf) and (not TRoomUnit(FSelf).CanSee(TRoomUnit(AObj))) then
    AObj := nil;
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

function TRoomMapGraph.IsNonBlockingObject(AObj: TRoomObject): Boolean;
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

procedure TRoomUnit.SetPreview96_128(const AValue: string);
begin
  if FPreview96_128 = AValue then Exit;
  FPreview96_128 := AValue;
end;

procedure TRoomUnit.OnRegisterRoomObject(const ANewObject: TRoomObject);
begin

end;

procedure TRoomUnit.LoadModels();
begin
  FViewAngle := Pi / 3;
  FViewRange := 10;
end;

procedure TRoomUnit.SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean);
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

function TRoomUnit.IsDead(): Boolean;
begin
  Result := HP <= 0;
end;

function TRoomUnit.InViewField(const APt: TVec2i): Boolean;
var v0, vView, vDir: TVec3;
begin
  if APt = RoomPos then Exit(True);
  v0 := Room.UI.TilePosToWorldPos(RoomPos);
  vDir := Room.UI.TilePosToWorldPos(APt) - v0;
  if LenSqr(vDir) <= ViewWholeRange*ViewWholeRange then Exit(True);
  if LenSqr(vDir) > ViewRange*ViewRange then Exit(False);
  vView := Room.UI.TilePosToWorldPos(RotateTileCoord(Vec(1,0), RoomDir) + RoomPos) - v0;
  vDir := normalize(vDir);
  vView := normalize(vView);
  Result := dot(vDir, vView) >= Cos(ViewAngle);
end;

function TRoomUnit.CanSee(const APos: TVec2i): Boolean;
begin
  if not InViewField(APos) then Exit(False);
  Result := Room.RayCastBoolean(RoomPos, APos);
end;

function TRoomUnit.CanSee(const AOtherUnit: TRoomUnit): Boolean;
begin
  if not InViewField(AOtherUnit.RoomPos) then Exit(False);
  Result := Room.RayCastBoolean(RoomPos, AOtherUnit.RoomPos);
end;

function TRoomUnit.GetVisible(): Boolean;
begin
  if Room.CurrentPlayer = nil then
    Result := inherited GetVisible()
  else
    Result := Room.CurrentPlayer.CanSee(Self);
end;

procedure TRoomUnit.DealDamage(ADmg: Integer);
var msg: TbFlyOutMessage;
begin
  HP := HP - ADmg;

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
  if FRegistered then
  begin
    if (FHP > 0) and (AValue <= 0) then
      FRoom.RemoveObject(Self);
    if (FHP <= 0) and (AValue > 0) then
      FRoom.PutObject(Self);
  end;
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

function TRoomObject.BlockedViewCell(AIndex: Integer): Boolean;
begin
  Result := True;
end;

function TRoomObject.GetAbsoluteBlockedCell(AIndex: Integer): TVec2i;
begin
  Result := GetAbsoluteBlockedCell(AIndex, FRoomPos, FRoomDir);
end;

function TRoomObject.GetAbsoluteBlockedCell(AIndex: Integer; APos: TVec2i; ADir: Integer): TVec2i;
begin
  Result := RotateTileCoord(GetBlockedCell(AIndex), ADir) + APos;
end;

class function TRoomObject.RotateTileCoord(const APos: TVec2i; ADir: Integer): TVec2i;
var dirmod: Integer;
begin
  dirmod := ADir mod 6;
  if dirmod < 0 then dirmod := dirmod + 6;
  case dirmod of
    0: Result := APos;
    1:
      begin
        Result.x := APos.x + APos.y;
        Result.y := -APos.x;
      end;
    2:
      begin
        Result.x := APos.y;
        Result.y := -APos.x - APos.y;
      end;
    3:
      begin
        Result.x := -APos.x;
        Result.y := -APos.y;
      end;
    4:
      begin
        Result.x := -APos.x - APos.y;
        Result.y := APos.x;
      end;
    5:
      begin
        Result.x := -APos.y;
        Result.y := APos.x + APos.y;
      end;
    else
      Assert(False);
      Result := APos;
  end;
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

procedure TRoomObject.RegisterAtRoom();
begin
  if FRegistered then Exit;
  FRegistered := True;
  FRoom.PutObject(Self);
end;

procedure TRoomObject.UnregisterAtRoom();
begin
  if not FRegistered then Exit;
  FRegistered := False;
  FRoom.RemoveObject(Self);
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
  FTilesProg.Load('UI_DrawTiles', False, '..\shaders\!Out');
  FTilesVB   := TavVB.Create(Self);
  FTilesVB.Vertices := FTilesData as IVerticesData;

  FTilesDataFogOfWar := THexTiles.Create();
  FTilesDataFogOfWar.Add(epmty_tile);
  FTilesVBFogOfWar := TavVB.Create(Self);
  FTilesVBFogOfWar.Vertices := FTilesDataFogOfWar as IVerticesData;

  FAffinePack.Row[0] := Vec(1,0);
  FAffinePack.Row[1] := Vec(0.5,0.86602540378443864676372317075294);
  //FAffinePack.Row[1] := Vec(0,1);
  FAffinePackInv := Inv(FAffinePack);

  FColors[TTileColorID.Normal] := Vec(0,0,0,1);
  FColors[TTileColorID.HighlightedGreen] := Vec(0,0.5,0,1);
  FColors[TTileColorID.HighlightedRed] := Vec(0.5,0,0,1);
  FColors[TTileColorID.HighlightedYellow] := Vec(0.5,0.5,0,1);
  FColors[TTileColorID.Hovered] := Vec(1,1,1,1);

  FColorsFogOfWar[TTileColorID.Normal] := Vec(0,0,0,0.7);
  FColorsFogOfWar[TTileColorID.HighlightedGreen] := Vec(0,0.5,0,1);
  FColorsFogOfWar[TTileColorID.HighlightedRed] := Vec(0.5,0,0,1);
  FColorsFogOfWar[TTileColorID.HighlightedYellow] := Vec(0.5,0.5,0,1);
  FColorsFogOfWar[TTileColorID.Hovered] := Vec(1,1,1,1);
end;

procedure TRoomUI.ClearTileColors();
var
  j, i: Integer;
  n: Integer;
begin
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
  if obj is TRoomUnit then
    if (rm.CurrentPlayer <> nil) then
      if not rm.CurrentPlayer.CanSee(TRoomUnit(obj)) then Exit(TTileColorID.Normal);
  Result := TTileColorID.None;
end;

procedure TRoomUI.InvalidateFogOfWar;
begin
  FFogOfWarValid := False;
end;

procedure TRoomUI.DrawUI();
var tmpColors: TVec4Arr;
  i: Integer;
begin
  if FTilesData.Count = 0 then Exit;
  SetLength(tmpColors, Length(FColors));

  ValidateFogOfWar;

  FTilesProg.Select();
  FTilesProg.SetUniform('YPos', FPlaneY);

  for i := 0 to Length(FColors) - 1 do
    tmpColors[i] := FColorsFogOfWar[TTileColorID(i)];
  FTilesProg.SetAttributes(nil, nil, FTilesVBFogOfWar);
  FTilesProg.SetUniform('TileColors', tmpColors);
  FTilesProg.SetUniform('gradPow', 1.0);
  FTilesProg.SetUniform('minAlpha', 0.7);
  FTilesProg.Draw(ptTriangles, cmNone, False, FTilesData.Count, 0, 18);

  for i := 0 to Length(FColors) - 1 do
    tmpColors[i] := FColors[TTileColorID(i)];
  FTilesProg.SetAttributes(nil, nil, FTilesVB);
  FTilesProg.SetUniform('TileColors', tmpColors);
  FTilesProg.SetUniform('gradPow', 4.0);
  FTilesProg.SetUniform('minAlpha', 0.0);
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

procedure TRoomUI.ValidateFogOfWar;
var
  w, j, i: Integer;
  new_tile: THexTile;
  rm: TRoomMap;
begin
  if FFogOfWarValid then Exit;
  rm := Parent as TRoomMap;

  w := FRadius * 2 + 1;
  FTilesDataFogOfWar.Clear();
  FTilesDataFogOfWar.Capacity := w*w;
  for j := -FRadius to FRadius do
    for i := -FRadius to FRadius do
    begin
      new_tile.vsPos := Vec(i, j) * FAffinePack;
      if rm.IsCellExists(Vec(i, j)) then
      begin
        new_tile.vsColor := TTileColorID.None;
        if rm.CurrentPlayer <> nil then
          if not rm.CurrentPlayer.CanSee(Vec(i, j)) then
            new_tile.vsColor := TTileColorID.Normal;
      end
      else
        new_tile.vsColor := TTileColorID.None;
      FTilesDataFogOfWar.Add(new_tile);
    end;
  FTilesVBFogOfWar.Invalidate;

  FFogOfWarValid := True;
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

function TRoomMap.NextPointOnRay(const APt: TVec2i; const ADir, ADirStep: TVec2i): TVec2i;
var p: array [0..2] of TVec2i;
    wp: TVec3;
    wpStart, wpEnd: TVec3;
    wpLine2D: TVec3;
    mindist, dist: Single;
    i: Integer;
    n: Integer;
begin
  if ADirStep.x * ADirStep.y = 0 then Exit(APt + ADirStep);
  p[0] := APt + Vec(ADirStep.x, 0);
  p[1] := APt + Vec(0, ADirStep.y);
  p[2] := APt + ADirStep;
  if ADir.x * ADir.y >= 0 then n := 1 else n := 2;
  wpStart := FRoomUI.TilePosToWorldPos(Vec(0, 0));
  wpEnd := FRoomUI.TilePosToWorldPos(ADir);
  wpLine2D := Cross(Vec(wpStart.x, wpStart.z, 1), Vec(wpEnd.x, wpEnd.z, 1));
  mindist := HUGE;
  for i := 0 to n do
  begin
    wp := FRoomUI.TilePosToWorldPos(p[i]);
    dist := abs( wpLine2D.z + dot(wpLine2D.xy, Vec(wp.x, wp.z)) );
    if dist < mindist then
    begin
      Result := p[i];
      mindist := dist;
    end;
  end;
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

procedure TRoomMap.Draw();
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

function TRoomMap.RayCast(const APt1, APt2: TVec2i; const AllowHitBlockers: Boolean): IRoomPath;
var dir: TVec2i;
    dirStep: TVec2i;
    pt: TVec2i;
begin
  Result := TRoomPath.Create();
  if APt1 = APt2 then
  begin
    Result.Add(APt2);
    Exit;
  end;

  dir := APt2 - APt1;
  dirStep := Sign(dir);
  pt := NextPointOnRay(Vec(0,0), dir, dirStep);
  while pt <> dir do
  begin
    Result.Add(APt1 + pt);
    if AllowHitBlockers then
      if IsCellBlockView(APt1 + pt) then
        Exit;
    pt := NextPointOnRay(pt, dir, dirStep);
  end;
  Result.Add(APt2);
end;

function TRoomMap.RayCastBoolean(const APt1, APt2: TVec2i): Boolean;
var dir: TVec2i;
    dirStep: TVec2i;
    pt: TVec2i;
begin
  if APt1 = APt2 then Exit(True);

  dir := APt2 - APt1;
  dirStep := Sign(dir);
  pt := NextPointOnRay(Vec(0,0), dir, dirStep);
  while pt <> dir do
  begin
    if IsCellBlockView(APt1 + pt) then Exit(False);
    pt := NextPointOnRay(pt, dir, dirStep);
  end;
  Result := True;
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

function TRoomMap.IsCellBlockView(const APos: TVec2i): Boolean;
var obj: TRoomObject;
    i: Integer;
begin
  obj := ObjectAt(APos);
  if obj = nil then Exit(False);
  for i := 0 to obj.BlockedCellsCount - 1 do
    if obj.BlockedViewCell(i) then
      if obj.GetAbsoluteBlockedCell(i) = APos then
        Exit(True);
  Result := False;
end;

procedure TRoomMap.PutObject(const AObject: TRoomObject);
var
  i: Integer;
  v: TVec2i;
  roomobj: TRoomObject;
begin
  Assert(AObject <> nil);
  for i := 0 to AObject.BlockedCellsCount() - 1 do
  begin
    v := AObject.GetAbsoluteBlockedCell(i);
    FObjects.Add(v, AObject);
    FRoomUI.TileColor[v] := TTileColorID.None;
  end;

  FObjects.Reset;
  while FObjects.NextValue(roomobj) do
    if (roomobj is TRoomUnit) and (roomobj <> AObject) then
      TRoomUnit(roomobj).OnRegisterRoomObject(AObject);

  FRoomUI.InvalidateFogOfWar;
  Main.InvalidateWindow;
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
  FRoomUI.InvalidateFogOfWar;
  Main.InvalidateWindow;
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

procedure TPlayer.SetAnimation(const ANameSequence: array of string; const ALoopedLast: Boolean);
var
  i: Integer;
begin
  inherited SetAnimation(ANameSequence, ALoopedLast);
  for i := 0 to Length(FAnim) - 1 do
    FAnim[i].AnimationSequence_StartAndStopOther(ANameSequence, ALoopedLast);
end;

function TPlayer.GetUnitMoveSpeed: Single;
begin
  Result := 2;
end;

procedure TPlayer.LoadModels();
var
  i: Integer;
begin
  ViewAngle := 0.5 * Pi + EPS;
  ViewRange := 20.5;
  ViewWholeRange := 2.5;

  AddModel('Gop_Body', mtDefault);
  AddModel('Gop_Bottoms', mtDefault);
  AddModel('Gop_Hair', mtDefault);
  AddModel('Gop_Hats', mtDefault);
  AddModel('Gop_Shoes', mtDefault);
  AddModel('Gop_Tops', mtDefault);

  SetLength(FAnim, FModels.Count);
  for i := 0 to FModels.Count - 1 do
    FAnim[i] := Create_IavAnimationController(FModels[i].Mesh.Pose, World.GameTime);
  SetAnimation(['Idle0'], True);
  SubscribeForUpdateStep;

  Scale := 0.5;
  MaxAP := 10;
  MaxHP := 100;
  HP := MaxHP;

  Preview96_128 := 'ui\units\player.png';
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

procedure TLantern.LoadModels(const AObstacle: TObstacleDesc);
begin
  inherited LoadModels(AObstacle);
  FLight := World.Renderer.CreatePointLight();
  FLight.Pos := cLightSrcPos;
  FLight.Radius := 30;
  FLight.Color := Vec(1,1,1);
  FLight.CastShadows := False;
end;

procedure TLantern.SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean);
begin
  inherited SetRoomPosDir(APos, ADir, AAutoRegister);
  FLight.Pos := cLightSrcPos * Transform();
end;

{ TBattleRoom }

procedure TBattleRoom.OnEndTurnBtnClick(ASender: TObject);
begin
  if not IsPlayerTurn then Exit;
  EndTurn();
end;

function TBattleRoom.IsPlayerTurn: Boolean;
begin
  if FUnits.Count = 0 then Exit(False);
  Result := FUnits[FActiveUnit] = FPlayer;
end;

function TBattleRoom.IsBotTurn: Boolean;
begin
  Result := FUnits[FActiveUnit] is TBot;
end;

function TBattleRoom.IsMouseOnUI: Boolean;
var curpt: TVec2;
begin
  curpt := (Main.Cursor.WindowCur*Vec(0.5, -0.5) + Vec(0.5, 0.5) )*Main.WindowSize;
  Result := FUnitMenu.HitTest(curpt, True) <> nil;
end;

procedure TBattleRoom.EMUps(var msg: TavMessage);
var bot: TBot;
    new_action: IBRA_Action;
    i: LongInt;
begin
  FWorld.UpdateStep();
  if FUnits.Count = 0 then Exit;

  if IsPlayerTurn then
  begin
    FMovedTile := FMap.UI.GetTileAtCoords(Main.Cursor.Ray);
    FMovePath := nil;
    if (not IsMouseOnUI) and (FPlayer <> nil) then
    begin
      FMovePath := FPlayer.FindPath(FMovedTile);
      if FMovePath <> nil then FMovePath.Add(FMovedTile);
    end;

    //FRayPath := FMap.RayCast(FPlayer.RoomPos, FMovedTile, True);
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
var oldDepthWrite: Boolean;
begin
  //  Main.States.DepthTest := False;
    oldDepthWrite := Main.States.DepthWrite;
    Main.States.DepthWrite := False;
    Main.States.Blending[0] := True;
    Main.States.SetBlendFunctions(bfSrcAlpha, bfInvSrcAlpha);
    FMap.Draw();
    Main.States.DepthWrite := oldDepthWrite;
end;

procedure TBattleRoom.PreloadModels;
begin
  FLanterns := TbGameObjArr.Create();
  FUnits := TRoomUnitArr.Create();
  FActions := TBRA_ActionArr.Create();
  FObstacles := LoadObstacles(ExeRelativeFileName('models\scene1_obstacles.txt'));
  FWorld.Renderer.PreloadModels([ExeRelativeFileName('models\scene1.avm')]);
  //FWorld.Renderer.PreloadModels([ExeRelativeFileName('chars\gop.avm')]);
  //FWorld.Renderer.PreloadModels([ExeRelativeFileName('enemies\creature1.avm')]);

  FFloor := TbGameObject.Create(FWorld);
  FFloor.AddModel('Floor', mtDefault);
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
  if IsMouseOnUI() then Exit;
end;

procedure TBattleRoom.MouseClick(button: Integer; xpos, ypos: Integer);
var obj: TRoomObject;
    new_action: IBRA_Action;
begin
  if not IsPlayerTurn() then Exit;
  if IsMouseOnUI() then Exit;

  if (button = 0) and (FActions.Count > 0) then
  begin
    FActions[0].TryCancel;
    Exit;
  end;

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
    Exit;
  end;
end;

procedure TBattleRoom.EndTurn;
var unt: TRoomUnit;
begin
  FActiveUnit := (FActiveUnit + 1) mod FUnits.Count;
  unt := FUnits[FActiveUnit] as TRoomUnit;
  unt.AP := unt.MaxAP;

  (FUnitMenu as TavmUnitMenu).RoomUnit := unt;
  if unt.HP <= 0 then EndTurn();
end;

procedure TBattleRoom.AddAction(AAction: IBRA_Action);
begin
  FActions.Add(AAction);
end;

procedure TBattleRoom.Draw();

  procedure DrawTileMap;
  var
    i: Integer;
    unt: TRoomUnit;
    bounds_min: TVec2i;
    bounds_max: TVec2i;
    x, y: Integer;
    movedObj: TRoomObject;
  begin
    FMap.UI.ClearTileColors();
    if IsPlayerTurn then
    begin
      if (FMovePath <> nil) and (FActions.Count = 0) then
        for i := 0 to FMovePath.Count - 1 do
        begin
          if i >= FPlayer.AP then
            FMap.UI.TileColor[FMovePath[i]] := TTileColorID.HighlightedRed
          else
            FMap.UI.TileColor[FMovePath[i]] := TTileColorID.HighlightedGreen;
        end;
      if (not IsMouseOnUI) and (FMovePath = nil) then
        FMap.UI.TileColor[FMovedTile] := TTileColorID.Hovered;

      movedObj := FMap.ObjectAt(FMovedTile);
      if (movedObj is TRoomUnit) and (movedObj <> FMap.CurrentPlayer) then
      begin
        unt := movedObj as TRoomUnit;
        if FPlayer.CanSee(unt) then
        begin
          bounds_min := unt.RoomPos - Floor(Vec(unt.ViewRange, unt.ViewRange));
          bounds_max := unt.RoomPos + Ceil(Vec(unt.ViewRange, unt.ViewRange));
          for y := bounds_min.y to bounds_max.y do
            for x := bounds_min.x to bounds_max.x do
              if unt.CanSee(Vec(x, y)) then
                FMap.UI.TileColor[Vec(x, y)] := TTileColorID.HighlightedYellow;
        end;
      end;

      if FRayPath <> nil then
        for i := 0 to FRayPath.Count - 1 do
        begin
          FMap.UI.TileColor[FRayPath[i]] := TTileColorID.Hovered;
        end;
    end;
  end;

begin
  DrawTileMap;

  FWorld.Renderer.PrepareToDraw;
  Main.Clear(Black, True, Main.Projection.DepthRange.y, True);
  FWorld.Renderer.DrawWorld;

  if FUnitMenu <> nil then
    FUnitMenu.Draw();

  Main.ActiveFrameBuffer.BlitToWindow();
end;

procedure TBattleRoom.Generate();

  procedure CreateObstacles();
  var obs: TObstacle;
      i, j: Integer;
      obsDesc: PObstacleDesc;
      newPos: TVec2i;
      newDir: Integer;
  begin
    if FObstacles = nil then Exit;
    if FObstacles.Count = 0 then Exit;
    for i := 0 to 40 do
    begin
      obsDesc := FObstacles.PItem[Random(FObstacles.Count)];
      for j := 0 to 10 do
      begin
        newPos.x := Random(FMap.Radius * 2 + 1) - FMap.Radius;
        newPos.y := Random(FMap.Radius * 2 + 1) - FMap.Radius;
        newDir := Random(6);
        if not FMap.IsCellExists(newPos) then Continue;
        if not CanPlaceObstacle(FMap, obsDesc^, newPos, newDir) then Continue;
        obs := TObstacle.Create(FMap);
        obs.LoadModels(obsDesc^);
        obs.SetRoomPosDir(newPos, newDir);
        Break;
      end;
    end;
  end;

var lantern: TLantern;
    bot: TBot;

    menu: TavmUnitMenu;
    obsDescIdx, i: Integer;
begin
  FLanterns := TbGameObjArr.Create();
  FUnits := TRoomUnitArr.Create();
  FActions := TBRA_ActionArr.Create();
  FObstacles := LoadObstacles(ExeRelativeFileName('models\scene1_obstacles.txt'));
  FWorld.Renderer.PreloadModels([ExeRelativeFileName('models\scene1.avm')]);
  FWorld.Renderer.PreloadModels([ExeRelativeFileName('chars\gop.avm')]);
  FWorld.Renderer.PreloadModels([ExeRelativeFileName('enemies\creature1.avm')]);

  menu := TavmUnitMenu.Create(Self);
  menu.OnEndTurnClick := {$IfDef FPC}@{$EndIf}OnEndTurnBtnClick;
  FUnitMenu := menu;

  FFloor := TbGameObject.Create(FWorld);
  FFloor.AddModel('Floor', mtDefault);

  obsDescIdx := -1;
  for i := 0 to FObstacles.Count - 1 do
    if FObstacles[i].name = 'Lantern' then
    begin
      obsDescIdx := i;
      Break;
    end;

  if obsDescIdx > 0 then
  begin
    lantern := TLantern.Create(FMap);
    lantern.LoadModels(FObstacles[obsDescIdx]);
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
    lantern.LoadModels(FObstacles[obsDescIdx]);
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
  end;

  FPlayer := TPlayer.Create(FMap);
  //FPlayer.SetRoomPosDir(Vec(5, 5), 0);
  FPlayer.LoadModels();
  FPlayer.SetRoomPosDir(Vec(2, 1), 0);
  FUnits.Add(FPlayer);

  bot := TBotMutant1.Create(FMap);
  bot.LoadModels();
  bot.SetRoomPosDir(Vec(-4, 4), 0);
  FUnits.Add(bot);

  FActiveUnit := FUnits.Count - 1;
  EndTurn();

  CreateObstacles();

  FMap.CurrentPlayer := FPlayer;
end;

procedure TBattleRoom.GenerateWithLoad(const AFileName: string);

  procedure LoadObstacles;
  var fs: TFileStream;
  begin
    fs := TFileStream.Create(ExeRelativeFileName(AFileName), fmOpenRead);
    try
      LoadRoomMap(fs);
    finally
      FreeAndNil(fs);
    end;
  end;

  function GetSpawnPlace(): TVec2i;
  begin
    repeat
      Result.x := Random(FMap.Radius*2+1) - FMap.Radius;
      Result.y := Random(FMap.Radius*2+1) - FMap.Radius;
      if FMap.IsCellExists(Result) and not FMap.IsCellBlocked(Result) then
        Exit(Result);
    until False;
  end;

  procedure SpawnBot();
  var bot: TBot;
  begin
    bot := TBotArcher1.Create(FMap);
    bot.LoadModels();
    bot.SetRoomPosDir(GetSpawnPlace(), Random(6));
    FUnits.Add(bot);
  end;

var
  i: Integer;
  menu: TavmUnitMenu;
begin
  PreloadModels;
  FWorld.Renderer.PreloadModels([ExeRelativeFileName('chars\gop.avm')]);
  FWorld.Renderer.PreloadModels([ExeRelativeFileName('enemies\creature1.avm')]);
  FWorld.Renderer.PreloadModels([ExeRelativeFileName('enemies\archer\archer.avm')]);

  menu := TavmUnitMenu.Create(Self);
  menu.OnEndTurnClick := {$IfDef FPC}@{$EndIf}OnEndTurnBtnClick;
  FUnitMenu := menu;

  LoadObstacles();

  FPlayer := TPlayer.Create(FMap);
  //FPlayer.SetRoomPosDir(Vec(5, 5), 0);
  FPlayer.LoadModels();
  FPlayer.SetRoomPosDir(GetSpawnPlace(), Random(6));
  FUnits.Add(FPlayer);

  for i := 0 to 6 do
    SpawnBot();

  FActiveUnit := FUnits.Count - 1;
  EndTurn();

  FMap.CurrentPlayer := FPlayer;
end;

procedure TBattleRoom.GenerateEmpty;
begin
  PreloadModels;

  FEmptyLight := FWorld.Renderer.CreatePointLight();
  FEmptyLight.Pos := Vec(0, 10, 0);
  FEmptyLight.Radius := 50;
  FEmptyLight.Color := Vec(1,1,1);
  FEmptyLight.CastShadows := True;
end;

procedure TBattleRoom.DrawObstaclePreview(const AName: String; const bmp: TBitmap);
var inst: IavModelInstanceArr;
    bbox: TAABB;

    oldAt: TVec3;
    oldEye: TVec3;

    tmpfbo: TavFrameBuffer;
    texdata: ITextureData;
    mipdata: ITextureMip;

    pDst: PVec3b;
    pSrc: PVec4b;
    j, i: Integer;
begin
  Assert(bmp.Width * bmp.Height > 0);

  inst := FWorld.Renderer.CreateModelInstances([AName]);
  inst[0].Mesh.Transform := IdentityMat4;
  bbox := inst[0].Mesh.Mesh.BBox;

  oldAt := Main.Camera.At;
  oldEye := Main.Camera.Eye;

  Main.Camera.At := bbox.Center;
  Main.Camera.Eye := Main.Camera.At + normalize(Vec(-1, 1, -1)) * (Len(bbox.Size) / tan(Main.Projection.Fov*0.5))*0.65;

  Main.States.CullMode := cmNone;
  Main.States.DepthWrite := True;
  Main.States.DepthTest := True;
  Main.States.Blending[0] := False;

  tmpfbo := Create_FrameBuffer(Self, [TTextureFormat.RGBA, TTextureFormat.D32f], [true, false]);
  try
    tmpfbo.FrameRect := RectI(0, 0, bmp.Width, bmp.Height);
    tmpfbo.Select();
    tmpfbo.Clear(0, Vec(0,0,0,0));
    tmpfbo.ClearDS(Main.Projection.DepthRange.y);

    FWorld.Renderer.ModelsProgram_NoLight.Select();
    FWorld.Renderer.ModelsCollection.Select();
    FWorld.Renderer.ModelsCollection.Draw(inst);

    texdata := EmptyTexData(bmp.Width, bmp.Height, TTextureFormat.RGBA, False, True);
    tmpfbo.GetColor(0).ReadBack(texdata, 0, 0);
    mipdata := texdata.MipData(0, 0);
    bmp.PixelFormat := pf24bit;

    pSrc := PVec4b(mipdata.Data);
    for j := 0 to bmp.Height - 1 do
    begin
      pDst := bmp.ScanLine[j];
      for i := 0 to bmp.Width - 1 do
      begin
        pDst^ := Vec(pSrc^.z, pSrc^.y, pSrc^.x);
        Inc(pDst);
        Inc(pSrc);
      end;
    end;
  finally
    FreeAndNil(tmpfbo);
  end;

  Main.Camera.At := oldAt;
  Main.Camera.Eye := oldEye;
end;

procedure TBattleRoom.SaveRoomMap(const AStream: TStream);
var obs: TObstacle;
    obsCount: Integer;
    i: Integer;
begin
  obsCount := Map.ChildCount(TObstacle);
  AStream.WriteBuffer(obsCount, SizeOf(obsCount));

  for i := 0 to Map.ChildCount - 1 do
  begin
    if (Map.Child[i] is TObstacle) then
    begin
      obs := TObstacle(Map.Child[i]);
      StreamWriteString(AStream, obs.ClassName);
      obs.WriteStream(AStream);
    end;
  end;
end;

procedure TBattleRoom.LoadRoomMap(const AStream: TStream);
var obs: TObstacle;
    obsCount: Integer;
    i: Integer;
    clsName : string;
    roomCls : TRoomObjectClass;
begin
  obsCount := 0;
  AStream.ReadBuffer(obsCount, SizeOf(obsCount));

  for i := 0 to obsCount - 1 do
  begin
    StreamReadString(AStream, clsName);
    roomCls := FindRoomClass(clsName);
    Assert(roomCls <> nil);
    Assert(roomCls.InheritsFrom(TObstacle));
    obs := CreateRoomObject(roomCls) as TObstacle;
    obs.ReadStream(AStream);
  end;
end;

function TBattleRoom.CreateRoomObject(const AClass: TRoomObjectClass): TRoomObject;
begin
  Result := AClass.Create(FMap);
end;

initialization
  RegRoomClass(TObstacle);
  RegRoomClass(TLantern);

end.


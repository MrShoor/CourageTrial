unit untLevel;

//{$Define DEBUGBOTS}

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Graphics,
  Math,
  Classes, SysUtils, avBase, avRes, bWorld, mutils, bLights, avMesh, avTypes, avTess, avContnrs, avContnrsDefaults,
  avPathFinder, avModel, avTexLoader, avRTTIUtils,
  bBassLight,
  untObstacles;

const
  cRoomRadius = 20;
  //cRoomRadius = 16;

type
  {$IfnDef FPC}
  TStringArray = array of string;
  {$EndIf}

  IVisitedRooms = {$IfDef FPC}specialize{$EndIf}IHashSet<string>;
  TVisitedRooms = {$IfDef FPC}specialize{$EndIf}THashSet<string>;

  IRoomMapGraph = {$IfDef FPC}specialize{$EndIf} IMap<TVec2i>;
  IRoomMapNonWeightedGraph = {$IfDef FPC}specialize{$EndIf}INonWeightedGraph<TVec2i>;
  IRoomMapPF  = {$IfDef FPC}specialize{$EndIf} IAStar<TVec2i>;
  TRoomMapPF  = {$IfDef FPC}specialize{$EndIf} TAStar<TVec2i>;
  IRoomMapBFS = {$IfDef FPC}specialize{$EndIf} IBFS_Iterator<TVec2i>;
  TRoomMapBFS = {$IfDef FPC}specialize{$EndIf} TBFS_Iterator<TVec2i>;
  IRoomMapFloodFill = {$IfDef FPC}specialize{$EndIf} IFloodFill_Iterator<TVec2i>;
  TRoomMapFloodFill = {$IfDef FPC}specialize{$EndIf} TFloodFill_Iterator<TVec2i>;
  IRoomPath = {$IfDef FPC}specialize{$EndIf} IArray<TVec2i>;
  TRoomPath = {$IfDef FPC}specialize{$EndIf} TArray<TVec2i>;
  IRoomPathArr = {$IfDef FPC}specialize{$EndIf} IArray<IRoomPath>;
  TRoomPathArr = {$IfDef FPC}specialize{$EndIf} TArray<IRoomPath>;

  IRoomCellFilter = interface
    function IsValid(const ACell: TVec2i): Boolean;
  end;

  TDoors = array [0..5] of Boolean;

  IVec2iArr = {$IfDef FPC}specialize{$EndIf}IArray<TVec2i>;
  TVec2iArr = {$IfDef FPC}specialize{$EndIf}TArray<TVec2i>;
  IVec3iArr = {$IfDef FPC}specialize{$EndIf}IArray<TVec3i>;
  TVec3iArr = {$IfDef FPC}specialize{$EndIf}TArray<TVec3i>;
  IVec2iSet = {$IfDef FPC}specialize{$EndIf}IHashSet<TVec2i>;
  TVec2iSet = {$IfDef FPC}specialize{$EndIf}THashSet<TVec2i>;

  IVec2iWeightedSet = {$IfDef FPC}specialize{$EndIf}IHashMap<TVec2i, Integer>;
  TVec2iWeightedSet = {$IfDef FPC}specialize{$EndIf}THashMap<TVec2i, Integer>;

  { IBRA_Action }

  IBRA_Action = interface
    function GetDone: Boolean;
    procedure SetDone(const AValue: Boolean);

    function Name: string;
    procedure TryCancel;
    function ProcessAction: Boolean;

    property Done: Boolean read GetDone write SetDone;
  end;
  IBRA_ActionArr = {$IfDef FPC}specialize{$EndIf} IArray<IBRA_Action>;
  TBRA_ActionArr = {$IfDef FPC}specialize{$EndIf} TArray<IBRA_Action>;

  TRoomMap = class;
  TBattleRoom = class;
  TRoomObject = class;
  TRoomUnit = class;

  TRoomObjectArr = {$IfDef FPC}specialize{$EndIf} TArray<TRoomObject>;
  IRoomObjectArr = {$IfDef FPC}specialize{$EndIf} IArray<TRoomObject>;
  TRoomObjectSet = {$IfDef FPC}specialize{$EndIf} THashSet<TRoomObject>;
  IRoomObjectSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TRoomObject>;
  TRoomUnitArr = {$IfDef FPC}specialize{$EndIf} TArray<TRoomUnit>;
  IRoomUnitArr = {$IfDef FPC}specialize{$EndIf} IArray<TRoomUnit>;
  TRoomUnitSet = {$IfDef FPC}specialize{$EndIf} THashSet<TRoomUnit>;
  IRoomUnitSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TRoomUnit>;

  TOnLeaveBattleRoom = procedure (const ABattleRoom: TBattleRoom; const ADoorIdx: Integer) of object;

  TRoomUnitEqSlot = (esNone, esLeftHand, esRightHand, esBothHands);
  TUnitItemKind = (ikUnknown, ikConsumable, ikBow, ikAxe);
const
  cUnitItemKindNames : array [TUnitItemKind] of string = ('???', 'Съедобное', 'Лук', 'Топор');
  cUnitItemKind_Weapons : set of TUnitItemKind = [ikBow, ikAxe];

type
  IUnitItem = interface;
  IUnitBuff = interface;

  TUnitSkillID = (sidUnknown, sidBowMastery, sidAxeMastery);
  IUnitSkill = interface
    function  GetSkillLevel: Integer;
    procedure SetSkillLevel(ALevel: Integer);
    property SkillLevel: Integer read GetSkillLevel write SetSkillLevel;

    function Item: IUnitItem;
    function Idx : Integer;
    function WearedOnly: Boolean;
    function UseReady(AUnit: TRoomUnit): Boolean;

    function ID   : TUnitSkillID;
    function Name : string;
    function Desc : string;
    function Ico  : string;
    function Sound: string;

    function Cost       : Integer;
    function Range      : Single;
    function Damage     : TVec2i;
    function DamageScale: Single;
    function Accuracy   : TVec2;

    function Req_WeaponType: TUnitItemKind;

    function Animation: string;
    function IsAttackSkill: Boolean;
    function IsBuffSkill: Boolean;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean;
    function SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff;

    function DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action;
    function CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean;
  end;
  IUnitSkillArr = {$IfDef FPC}specialize{$EndIf}IArray<IUnitSkill>;
  TUnitSkillArr = {$IfDef FPC}specialize{$EndIf}TArray<IUnitSkill>;

  TRoomUnitStats = packed record
    Lucky: Integer;
  end;

  TUnitBuffKind = (bkPowerUp, bkStun, bkBleed, bkPoison, bkDebuff);
  TUnitBuffID = (bidUnknown, bidAbsoluteSight);
  IUnitBuff = interface
    function ID  : TUnitBuffID;
    function Name: string;
    function Desc: string;
    function Ico : string;

    function Duration: Integer;
    function Kind: TUnitBuffKind;

    function Owner : TRoomUnit;
    function AtUnit: TRoomUnit;

    procedure ProcessDamage(var ADmg: Integer; AFromUnit: TRoomUnit);

    procedure SetUnit(const AUnit: TRoomUnit);
    function DoStep: Boolean;
  end;
  IUnitBuffsArr = {$IfDef FPC}specialize{$EndIf}IArray<IUnitBuff>;
  TUnitBuffsArr = {$IfDef FPC}specialize{$EndIf}TArray<IUnitBuff>;

  {$ScopedEnums On}
  TUnitItemID = (Unknown, LuckySocks);
  {$ScopedEnums Off}
  IUnitItem = interface
  ['{E6D665A9-3E67-43A8-891E-32B835887FB8}']
    function  ID: TUnitItemID;
    function  GetEquipped: Boolean;
    procedure SetEquipped(const AValue: Boolean);

    function Name  : string;
    function Kind  : TUnitItemKind;
    function Slot  : TRoomUnitEqSlot;
    function Model : string;
    function Ico48 : string;

    function Weapon_Damage: TVec2i;
    function ExtraDesc: string;

    function SkillsCount: Integer;
    function Skill(ASkillIndex: Integer): IUnitSkill;

    function Consume(AUnit: TRoomUnit): IBRA_Action;

    function StatsUp: TRoomUnitStats;

    procedure ProcessDamage(ADmg: Integer; AFromUnit: TRoomUnit);

    property Equipped: Boolean read GetEquipped write SetEquipped;
  end;
  IUnitItemArr = {$IfDef FPC}specialize{$EndIf}IArray<IUnitItem>;
  TUnitItemArr = {$IfDef FPC}specialize{$EndIf}TArray<IUnitItem>;

  IInventory = interface
    function Owner  : TRoomObject;
    function StateID: Integer;
    function Items  : IUnitItemArr;
    function Pop(const AIndex: Integer): IUnitItem;
    function Pop(const AItem: IUnitItem): Boolean;
    function Push(const AItem: IUnitItem; const AIndex: Integer): Integer;
    procedure BumpStateID;
  end;

  IGameUI = interface
    function IsMouseOnUI: Boolean;

    procedure SetActiveUnit(const ARoomUnit: TRoomUnit);
    procedure SetPlayerActiveSkill(const ASkill: IUnitSkill);
    procedure SetReservedAP(const ANewReservedAP: Integer);
    procedure SetOtherInventory(const AInventory: IInventory; ACloseCallback: TNotifyEvent);
    procedure AdjustCameraToPlayer();
    procedure AdjustCameraToPlayerKeepDist();
    procedure SetCameraBounds(const ABounds: TRectF);

    procedure InvalidateEnemiesBar;
    procedure SetEnemiesList(const AEnemies: IRoomUnitArr);

    procedure AddMessage(const AMsg: string);

    procedure UpdateStep(const AIsPlayerTurn: Boolean);
  end;

  { TTileUtils }

  TTileUtils = class
  public
    class function CircleLen(const ARadius: Integer): Integer;
    class function PolarToTileCoord(const ARadius, AIndex: Integer): TVec2i;
    class function RotateTileCoord(const APos: TVec2i; ADir: Integer): TVec2i;
    class function NeighbourTile(const ACurrent: TVec2i; const AIndex: Integer): TVec2i;
    class function DirToQuat(const ADir: Integer): TQuat;
  end;

  { TRoomObject }

  TRoomObject = class (TbGameObject)
  private
    FRegistered: Boolean;
    FRoomDir: Integer;
    FRoomPos: TVec2i;
  protected
    FNonRegistrable: Boolean;
    FRoom: TRoomMap;
    function CanRegister(target: TavObject): boolean; override;
    procedure SetRoomDir(const AValue: Integer); virtual;
    procedure SetRoomPos(const AValue: TVec2i); virtual;
  protected
    function  GetStatic: Boolean; override;
  protected
    procedure Notify_PlayerLeave; virtual;
    procedure Notify_PlayerEnter; virtual;

    procedure AfterRegister; override;
  public
    function BlockedCellsCount: Integer; virtual;
    function GetBlockedCell(AIndex: Integer): TVec2i; virtual;
    function BlockedViewCell(AIndex: Integer): Boolean; virtual;
    function GetAbsoluteBlockedCell(AIndex: Integer): TVec2i; overload;
    function GetAbsoluteBlockedCell(AIndex: Integer; APos: TVec2i; ADir: Integer): TVec2i; overload;
    class function RotateTileCoord(const APos: TVec2i; ADir: Integer): TVec2i;

    function Inventory(): IInventory; virtual;

    property Room: TRoomMap read FRoom;
    property RoomPos: TVec2i read FRoomPos write SetRoomPos;
    property RoomDir: Integer read FRoomDir write SetRoomDir;
    procedure SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean = True); virtual;
    property Registred: Boolean read FRegistered;
    procedure RegisterAtRoom();
    procedure UnregisterAtRoom();

    destructor Destroy; override;
  end;
  TRoomObjectClass = class of TRoomObject;

  { TRoomDoor }

  TRoomDoor = class (TRoomObject)
  private
    FOpened: Boolean;
    FAnim  : IavAnimationController;
    procedure SetOpened(AValue: Boolean);
    procedure KillBlockingUnits;
  protected
    procedure UpdateStep; override;
  public
    procedure LoadModels;
    function BlockedCellsCount: Integer; override;
    function GetBlockedCell(AIndex: Integer): TVec2i; override;
    function BlockedViewCell(AIndex: Integer): Boolean; override;

    property Opened: Boolean read FOpened write SetOpened;
  end;

  { TRoomFloor }

  TRoomFloor = class(TRoomObject)
  private
    FInited: Boolean;
    FDoors: array[0..5] of TRoomDoor;
    FDoorsOpened: Boolean;
    FHoles: IVec2iSet;
    function GetIsHole(const ATileCoord: TVec2i): Boolean;
    procedure SetDoorsOpened(AValue: Boolean);
    procedure SetIsHole(const ATileCoord: TVec2i; AValue: Boolean);

    procedure ValidateFloor;
    procedure InvalidateFloor;
  protected
    FFloorModels: IavModelInstanceArr;
    FBackPackModel: IavModelInstance;
    procedure AfterRegister; override;
    procedure AddWalls();
  public
    procedure InitModels();
    procedure WriteModels(const ACollection: IavModelInstanceArr; AType: TModelType); override;

    function AllHoles: IVec2iSet;
    property IsHole[const ATileCoord: TVec2i]: Boolean read GetIsHole write SetIsHole;

    property DoorsOpened: Boolean read FDoorsOpened write SetDoorsOpened;
    function DoorCellIndex(const ATileCoord: TVec2i): Integer;

    procedure UpdateInventoryObjects;
  end;

  { TRoomBullet }

  TRoomBullet = class (TbGameObject)
  private
    FOwner: TRoomUnit;
    procedure SetVisible(const AValue: Boolean);
  protected
    FDmg: Integer;
    FStartPt: TVec2i;
    FTarget: TVec2i;
    FMaxRange: Single;
    FVelocity: Single;
    FVisible: Boolean;
  public
    function GetVisible: Boolean; override;
  public
    function Accuracy(const ADist: Single): Single; virtual;

    property Owner: TRoomUnit read FOwner write FOwner;
    property Dmg: Integer read FDmg write FDmg;
    property StartPt: TVec2i read FStartPt write FStartPt;
    property Target: TVec2i read FTarget write FTarget;
    property Velocity: Single read FVelocity write FVelocity;
    property MaxRange: Single read FMaxRange write FMaxRange;

    property Visible: Boolean read GetVisible write SetVisible;

    procedure LoadModels(const AModelName: string);
  end;

  { TInventory }

  TInventory = class(TInterfacedObject, IInventory)
  private
    FOwner : TRoomObject;
  protected
    FStateID  : Integer;
    FInventory: IUnitItemArr;
    function Owner  : TRoomObject;
    function StateID: Integer; virtual;
    function Items  : IUnitItemArr; virtual;
    function Pop(const AIndex: Integer): IUnitItem; virtual; overload;
    function Pop(const AItem: IUnitItem): Boolean; overload;
    function Push(const AItem: IUnitItem; const AIndex: Integer): Integer; virtual;
    procedure BumpStateID;
  public
    constructor Create(const AOwner: TRoomObject);
  end;

  TRoomUnitAnimateState = (asStand, asWalk, asDeath);
  TRoomUnitMaterial = (matNone, matFlesh, matEnergy);

  { TRoomUnit }

  TRoomUnit = class (TRoomObject)
  private type
    TUnitInventory = class (TInventory)
    protected
      function Pop(const AIndex: Integer): IUnitItem; override;
    end;
  private
    FAP: Integer;
    FMaxAP: Integer;
    FStats: TRoomUnitStats;

    FHP: Integer;
    FMaxHP: Integer;
    FPreview96_128: string;

    FViewAngle: Single;
    FViewRange: Single;
    FViewWholeRange: Single;

    FAnimateState: TRoomUnitAnimateState;

    procedure SetAP(const AValue: Integer);
    procedure SetHP(const AValue: Integer);
    procedure SetMaxAP(const AValue: Integer);
    procedure SetMaxHP(const AValue: Integer);
    procedure SetPreview96_128(const AValue: string);

    function GetCurrentStats: TRoomUnitStats;
  protected
    FBuffs: IUnitBuffsArr;

    FAnimationPrefix: string;
    FUnitSkills   : IUnitSkillArr;
    FEquippedItem : array [TRoomUnitEqSlot] of IUnitItem;
    FEquippedModel: array [TRoomUnitEqSlot] of IavModelInstance;
    FTemporaryEquippedModel: array [TRoomUnitEqSlot] of IavModelInstance;
    FInventory: IInventory;

    FAllSkillsCacheInventoryLastStateID: Integer;
    FAllSkillsCache: IUnitSkillArr;

    FSlots10: IUnitSkillArr;
    procedure OnRegisterRoomObject(const ANewObject: TRoomObject); virtual;
    procedure OnDead(); virtual;
    procedure OnRessurect(); virtual;

    function AddAnimationPrefix(const ANameSequence: array of string): TStringArray;
  protected
    procedure Notify_PlayerLeave; override;
    procedure Notify_PlayerEnter; override;

    procedure UpdateStep; override;

    procedure AfterRegister; override;
    function InAbsoluteSight: Boolean;
  public
    procedure WriteModels(const ACollection: IavModelInstanceArr; AType: TModelType); override;
    procedure WriteDepthOverrideModels(const ACollection: IavModelInstanceArr; const ADepthOverride: IOverrideColorArr); override;
  public
    property BasicStats: TRoomUnitStats read FStats write FStats;
    property Stats: TRoomUnitStats read GetCurrentStats;

    property Preview96_128: string read FPreview96_128 write SetPreview96_128;

    property SkillSlots: IUnitSkillArr read FSlots10 write FSlots10;
    function UnitSkills(): IUnitSkillArr;
    function AllSkills(): IUnitSkillArr;
    function AllBuffs(): IUnitBuffsArr;
    procedure ProcessBuffs;

    function  Inventory(): IInventory; override;
    procedure Unequip(const ASlot: TRoomUnitEqSlot);
    function  Equip(const AItem: IUnitItem): Boolean;
    function  GetEquip(const ASlot: TRoomUnitEqSlot): IUnitItem;

    procedure TemporaryEquip(ASlot: TRoomUnitEqSlot; const AModel: string);
    procedure TemporaryUnEquip(ASlot: TRoomUnitEqSlot);

    procedure LoadModels(); virtual;

    procedure SetAnimation(const ANameSequence: array of string); virtual;
    function  GetUnitMoveSpeed: Single; virtual;

    function FindPath(const ATarget: TVec2i): IRoomPath; overload;
    function FindPath(const ATarget: TVec2i; ATargetUnit: TRoomUnit): IRoomPath; overload;
    function FindPath(const ATarget: TVec2i; const AFilter: IRoomCellFilter): IRoomPath; overload;

    function IsDead(): Boolean;

    property MaxAP: Integer read FMaxAP write SetMaxAP;
    property AP: Integer read FAP write SetAP;

    property MaxHP: Integer read FMaxHP write SetMaxHP;
    property HP: Integer read FHP write SetHP;

    property ViewAngle: Single read FViewAngle write FViewAngle;
    property ViewRange: Single read FViewRange write FViewRange;
    property ViewWholeRange: Single read FViewWholeRange write FViewWholeRange;
    function InViewField(const AUnitPos: TVec2i; const AUnitDir: Integer; const APt: TVec2i): Boolean; overload;
    function InViewField(const APt: TVec2i): Boolean; overload;
    function InViewRange(const APt: TVec2i): Boolean;
    function CanSeeFromPos(const AFromPos: TVec2i; const APos: TVec2i): Boolean; overload;
    function CanSee(const APos: TVec2i): Boolean; overload;
    function CanSee(const AOtherUnit: TRoomUnit): Boolean; overload;
    function GetShootPoints(): IVec2iArr;
    function GetShootPointsSet(): IVec2iSet;
    function GetObservablePointSet(const AInSet, AExSet: IVec2iSet): IVec2iSet;
    function GetMovePointsWeighted(AMaxDepth: Integer): IVec2iWeightedSet; overload;
    function GetMovePointsWeighted(AMaxDepth: Integer; const AFilter: IRoomCellFilter): IVec2iWeightedSet; overload;
    function GetMovePoints(AMaxDepth: Integer): IVec2iSet;
    function GetVisible(): Boolean; override;

    function Sound_Footstep(const AStepIndex: Integer): string; virtual;
    function Material_Body(): TRoomUnitMaterial; virtual;

    procedure ApplyBuff(ABuff: IUnitBuff); virtual;
    procedure DealPureDamage(ADmg: Integer; AFromUnit: TRoomUnit; const AMsg: string = ''); virtual;
    procedure DealDamage(ADmg: Integer; AFromUnit: TRoomUnit; const AMsg: string = ''); virtual;
    procedure InstantKill(const AMessage: string); virtual;

    property AnimateState: TRoomUnitAnimateState read FAnimateState write FAnimateState;
  end;

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

    procedure ValidateTiles;
    procedure SetMapRadius(const ARadius: Integer);
  protected
    procedure AfterRegister; override;
    function RoomMap: TRoomMap;
  public
    property Radius: Integer read FRadius write SetMapRadius;
    property Colors[ID: TTileColorID]: TVec4 read GetColors write SetColors;
    property TileColor[const APos: TVec2i]: TTileColorID read GetTileColor write SetTileColor;
    procedure ClearTileColors();

    function GetTileAtCoords(const ARay: TLine): TVec2i; overload;
    function GetTileAtCoords(const ACoord: TVec2): TVec2i; overload;

    function TilePosToRoom3DPos(const ATilePos: TVec2i): TVec3;
    function TilePosToWorldPos(const ATilePos: TVec2i): TVec3;
    function TileToWorldTransform(const ATilePos: TVec2i; const ATileRot: Integer): TMat4;

    function AutoTileColor(const APos: TVec2i): TTileColorID;

    procedure InvalidateFogOfWar;

    procedure DrawUI(const ATransform: TMat4);
  end;

  { TRoomMapGraph }

  TRoomMapGraph = class(TInterfacedObjectEx, IRoomMapGraph, IRoomMapNonWeightedGraph, IEqualityComparer)
  private
    FRoomMap: TRoomMap;

    function Hash(const Value): Cardinal;
    function IsEqual(const Left, Right): Boolean;

    function IsCellExists(const ACell: TVec2i): Boolean;

    function MaxNeighbourCount(const ANode: TVec2i): Integer;
    function NodeComparer: IEqualityComparer;
    function GetNeighbour(Index: Integer; const AFrom, ACurrent, ATarget: TVec2i; out ANeighbour: TVec2i; out MoveWeight, DistWeight: Single): Boolean; overload;
    function GetNeighbour(Index: Integer; const ACurrent: TVec2i; out ANeighbour: TVec2i): Boolean; overload;
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

  { TRoomMapGraph_CustomFilter }

  TRoomMapGraph_CustomFilter = class(TRoomMapGraph)
  protected
    FFilter: IRoomCellFilter;
    function IsNonBlockingObject(AObj: TRoomObject): Boolean; override;
  public
    constructor Create(const ARoomMap: TRoomMap; AFilter: IRoomCellFilter); overload;
  end;

  { TRoomCellFilter_ExcludeUnits }

  TRoomCellFilter_ExcludeUnits = class(TInterfacedObject, IRoomCellFilter)
  private
    FRoom: TRoomMap;
  public
    function IsValid(const ACell: TVec2i): Boolean;
    constructor Create(const ARoom: TRoomMap);
  end;

  { TRoomCellFilter_ViewableExcludeUnits }

  TRoomCellFilter_ViewableExcludeUnits = class(TInterfacedObject, IRoomCellFilter)
  private
    FRoom: TRoomMap;
  public
    function IsValid(const ACell: TVec2i): Boolean;
    constructor Create(const ARoom: TRoomMap);
  end;


  { TRoomCellFilter_ViewableExclude_ObjectsExclude }

  TRoomCellFilter_ViewableExclude_ObjectsExclude = class(TInterfacedObject, IRoomCellFilter)
  private
    FRoom: TRoomMap;
    FExclude: IRoomObjectSet;
  public
    function IsValid(const ACell: TVec2i): Boolean;
    constructor Create(const ARoom: TRoomMap; const AObjectsForExclude: array of TRoomObject);
  end;

  { TRoomCellComparer_MaxDistance }

  TRoomCellComparer_MaxDistance = class (TInterfacedObject, IComparer)
  private
    FRoom: TRoomMap;
    FStartPt: TVec2i;
  public
    function Compare(const Left, Right): Integer;
    constructor Create(const AUnit: TRoomUnit); overload;
    constructor Create(const ARoom: TRoomMap; const AStartPt: TVec2i); overload;
  end;

  TRoomMap = class (TavMainRenderChild)
  private type
    TObjectMap = {$IFDef FPC}specialize{$EndIf} THashMap<TVec2i, TRoomObject>;
    IObjectMap = {$IFDef FPC}specialize{$EndIf} IHashMap<TVec2i, TRoomObject>;
  private
    FRoomFloor: TRoomFloor;
    FCurrentPlayer: TRoomUnit;
    FInEditMode: Boolean;

    FRadius: Integer;
    FDoors : TDoors;

    FRoomUI: TRoomUI;

    FObjects: IObjectMap;

    FInventoryObjects: IRoomObjectSet;

    procedure SetRadius(const AValue: Integer);
    function NextPointOnRay(const APt: TVec2i; const ADir, ADirStep: TVec2i): TVec2i;
  protected
    procedure AfterRegister; override;
    function GetBattleRoom: TBattleRoom;

    procedure Notify_PlayerLeave;
    procedure Notify_PlayerEnter;
  public
    property InEditMode: Boolean read FInEditMode write FInEditMode;
    property UI: TRoomUI read FRoomUI;
    property BattleRoom: TBattleRoom read GetBattleRoom;

    property Radius: Integer read FRadius write SetRadius;
    procedure SetDoorsRadius(const ADoors: TDoors; const ARadius: Integer);

    procedure Draw();
    procedure AddMessage(const AStr: string);
    procedure AddFlyOutMessage(const AStr: string; const ATilePos: TVec2i; const AColor: TVec3; AHeight: Single = 1.5; const ATime: Integer = 1700);

    function NeighbourTile(const ACurrent: TVec2i; const AIndex: Integer): TVec2i;
    function Distance(const APt1, APt2: TVec2i): Integer;
    function Direction(const APt1, APt2: TVec2i): Integer;
    function RayCast(const APt1, APt2: TVec2i; const AllowHitBlockers: Boolean): IRoomPath; overload;
    function RayCast(const APt1, APt2: TVec2i; const AFilter: IRoomCellFilter): IRoomPath; overload;
    function RayCastDist(const APt1, APt2: TVec2i; ADist: Single; const AllowHitBlockers: Boolean): IRoomPath;
    function RayCastBoolean(const APt1, APt2: TVec2i): Boolean;
    function RayCastBoolean(const APt1, APt2: TVec2i; AFilter: IRoomCellFilter): Boolean;
    function GetShootPointsSet(const ASourcePt: TVec2i; const AViewRange: Single; const AInFilter: IVec2iSet): IVec2iSet;
    function GetShootPoints(const ASourcePt: TVec2i; const AViewRange: Single; const AInFilter: IVec2iSet): IVec2iArr;

    function IsCellExists(const APos: TVec2i): Boolean;
    function IsCellBlocked(const APos: TVec2i): Boolean;
    function IsCellBlockView(const APos: TVec2i): Boolean;

    procedure PutObject(const AObject: TRoomObject);
    procedure RemoveObject(const AObject: TRoomObject);
    function  ObjectAt(const APos: TVec2i): TRoomObject;

    procedure RegInventoryObject(const AObject: TRoomObject);
    procedure UnRegInventoryObject(const AObject: TRoomObject);
    function  InventoryObjectsAt(const APos: TVec2i; const ANonEmpty: Boolean): IRoomObjectArr;
    function  AllInventoryObjects(const ANonEmpty: Boolean): IRoomObjectArr;
    function  AllInventoryObjectsPos(const ANonEmpty: Boolean): IVec2iSet;
    procedure InvalidateInventoryBagsOnGround;

    function  InAction: Boolean;
    procedure AddAction(AAction: IBRA_Action);

    property CurrentPlayer: TRoomUnit read FCurrentPlayer write FCurrentPlayer;
    property RoomFloor: TRoomFloor read FRoomFloor;
  end;

  { TObstacle }

  TObstacle = class (TRoomObject)
  private
    FObstacle: TObstacleDesc;
  public
    procedure LoadComplete; virtual;
  public
    function GetProps: TPersistent; virtual;
    property Obstacle: TObstacleDesc read FObstacle;

    function BlockedCellsCount: Integer; override;
    function GetBlockedCell(AIndex: Integer): TVec2i; override;
    function BlockedViewCell(AIndex: Integer): Boolean; override;
    procedure LoadModels(const AObstacle: TObstacleDesc); virtual;

    procedure WriteStream(const AStream: TStream); virtual;
    procedure ReadStream(const AStream: TStream; AAutoRegister: Boolean = True); virtual;
  end;

  { TRoomInteractiveObject }

  TRoomInteractiveObject = class (TObstacle)
  private
  protected
    function GetInteractiveIdx(AUnit: TRoomUnit): Integer;
  public
    function Interactive_CellsCount: Integer; virtual;
    function Interactive_GetCell(AIndex: Integer): TVec2i; virtual;
    function Interactive_GetAbsoluteCell(AIndex: Integer): TVec2i;

    function Interactive_Cost(AIndex: Integer): Integer; virtual;
    function Interactive_Try(AUnit: TRoomUnit): IBRA_Action; virtual;
  end;

  { TPlayer }

  TPlayer = class (TRoomUnit)
  private
    FAnim: array of IavAnimationController;
    FActiveSkill: IUnitSkill;
    function  GetActiveSkill: IUnitSkill;
    procedure SetActiveSkill(const AValue: IUnitSkill);
  protected
    procedure OnDead(); override;
    procedure Notify_PlayerLeave; override;
    procedure Notify_PlayerEnter; override;
    procedure UpdateStep; override;
  public
    property ActiveSkill: IUnitSkill read GetActiveSkill write SetActiveSkill;

    procedure SetAnimation(const ANameSequence: array of string); override;
    function  GetUnitMoveSpeed: Single; override;

    procedure LoadModels(); override;

    function Sound_Footstep(const AStepIndex: Integer): string; override;
    function Material_Body(): TRoomUnitMaterial; override;
    procedure DealPureDamage(ADmg: Integer; AFromUnit: TRoomUnit; const AMsg: string = ''); override;
  end;

  { TBRA_Action }

  TBRA_Action = class(TInterfacedObject, IBRA_Action)
  private
    FDone: Boolean;
    function GetDone: Boolean;
    procedure SetDone(const AValue: Boolean);
  public
    function Name: string; virtual;
    procedure TryCancel; virtual;
    function ProcessAction: Boolean; virtual; abstract;
  end;

  { TBRA_ComingIn }

  TBRA_ComingIn = class(TBRA_Action)
  private
    FRoomUnit: TRoomUnit;
    FMovePath: IRoomPath;
    FMovePathIdx: Integer;
    FMovePathWeight: Single;
    function MoveToNextCell: Boolean;
  public
    function Name: string; override;
    function ProcessAction: Boolean; override;

    constructor Create(const AUnit: TRoomUnit; const ADoorIdx: Integer);
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

    TimeStart: Int64;
    StepIdx  : Integer;

    function MoveToNextCell: Boolean;
  public
    function Name: string; override;
    procedure TryCancel; override;
    function ProcessAction: Boolean; override;
    constructor Create(const AUnit: TRoomUnit; const APath: IRoomPath);
  end;

  { TBRA_UnitTurnAction }

  TBRA_UnitTurnAction = class (TBRA_Action)
  private
    RoomUnit: TRoomUnit;
    NewDir: Integer;
  public
    function ProcessAction: Boolean; override;
    constructor Create(const AUnit: TRoomUnit; const ATargetPt: TVec2i);
    constructor Create(const AUnit: TRoomUnit; const ANewDir: Integer);
  end;

  { TBRA_Shoot }

  TBRA_Shoot = class (TBRA_Action)
  private type
      TBulletInfo = record
        bullet: TRoomBullet;
        start : TVec3;
        stop  : TVec3;
        hit   : TRoomUnit;
      end;
      PBulletInfo = ^TBulletInfo;
      IBulletInfoArr = {$IfDef FPC}specialize{$EndIf} IArray<TBulletInfo>;
      TBulletInfoArr = {$IfDef FPC}specialize{$EndIf} TArray<TBulletInfo>;
  private
    FRoomUint: TRoomUnit;
    FSkill   : IUnitSkill;
    FBullets : IBulletInfoArr;
    FShootDelay: Integer;
    FSoundPlayed: Boolean;
  public
    function ProcessAction: Boolean; override;
    constructor Create(const AUnit: TRoomUnit;
                       const ABullets: array of TRoomBullet;
                       const ASkill: IUnitSkill;
                       const AShootDelay: Integer;
                       const ABulletY: Single);
  end;

  { TBRA_UnitDefaultAttack }

  TBRA_UnitDefaultAttack = class(TBRA_Action)
  private
    FRoomUnit   : TRoomUnit;
    FTarget     : TRoomUnit;
    FSkill      : IUnitSkill;
    FActionTime : Integer;
    FDamageStartTime: Integer;
    FSoundTime : Integer;
  public
    function ProcessAction: Boolean; override;
    constructor Create(const AUnit, ATarget: TRoomUnit;
                       const ASkill: IUnitSkill;
                       const ADurationTime, ADamageStartTime: Integer;
                       const ASoundDelay: Integer);
  end;

  { TBRA_UnitKickAttack }

  TBRA_UnitKickAttack = class(TBRA_Action)
  private
    FRoomUnit   : TRoomUnit;
    FTarget     : TRoomUnit;
    FSkill      : IUnitSkill;
    FActionTime : Integer;
    FDamageStartTime: Integer;
    FSoundTime  : Integer;
    FInKnockBack: Boolean;
    FKnockBack  : Single;
    FKnockBackFrom: TVec3;
    FKnockBackTo  : TVec3;
  public
    function ProcessAction: Boolean; override;
    constructor Create(const AUnit, ATarget: TRoomUnit;
                       const ASkill: IUnitSkill;
                       const ADurationTime, ADamageStartTime, ASoundTime: Integer);
  end;

  { TBRA_MakeDamage }

  TBRA_MakeDamage = class(TBRA_Action)
  private
    FRoomUnit  : TRoomUnit;
    FDamage    : Integer;
    FActionTime: Integer;
  public
    function ProcessAction: Boolean; override;
    constructor Create(const AUnit, AFromUnit: TRoomUnit; const ADamage: Integer; const AMsg: string = '');
  end;

  { TBRA_LootGround }

  TBRA_LootGround = class(TBRA_Action)
  private
    FInAction: Boolean;
    FUnit : TRoomUnit;
    FInventoryUnit: TRoomObject;
    function GetBattleRoom: TBattleRoom;
    procedure ShowOtherInventory;
    procedure HideOtherInventory;
    procedure OnCloseOtherInventory(ASender: TObject);
  public
    procedure TryCancel; override;
    function ProcessAction: Boolean; override;
    constructor Create(AUnit: TRoomUnit);
  end;

  { TBattleRoom }

  TBattleRoom = class (TavMainRenderChild)
  private
    FAffinePack: TMat2;
    FAffinePackInv: TMat2;

    FRoomPos: TVec2i;
    FRoomDir: Integer;
    FRoomTransform: TMat4;
    FRoomTransformInv: TMat4;
    procedure UpdateRoomTransform;
    procedure SetRoomPos(AValue: TVec2i);
    procedure SetRoomDir(AValue: Integer);
  private
    FObstacles: IObstacleArr;

    FPlayer: TPlayer;

    FUnits: IRoomUnitArr;
    FActiveUnit: Integer;

    FMap: TRoomMap;
    FUI : IGameUI;
    FOnLeaveBattleRoom: TOnLeaveBattleRoom;
  private
    FActions: IBRA_ActionArr;
  protected
    FMovedTile: TVec2i;
    FMovePath : IRoomPath;
    FRayPath : IRoomPath;

    FTryLeaveIndex: Integer;

    FEmptyLight: IavPointLight;

    FRoomClear: Boolean;

    function IsBotTurn: Boolean;
    function IsMouseOnUI: Boolean;
    procedure AutoOpenDoors;
    procedure AutoCloseDoors;

    procedure UpdateUnitsAtUI;
  protected
    procedure AfterRegister; override;
    function World: TbWorld;
  public
    procedure SetEditMode();

    property RoomPos: TVec2i read FRoomPos write SetRoomPos;
    property RoomDir: Integer read FRoomDir write SetRoomDir;
    property RoomTransform: TMat4 read FRoomTransform;
    property RoomTransformInv: TMat4 read FRoomTransformInv;
    function WorldPos: TVec3;
    function WorldRot: TQuat;

    function IsPlayerTurn: Boolean;
    function IsClear: Boolean;
    property MovedTile: TVec2i read FMovedTile;
    property Player: TPlayer read FPlayer;
    property Obstacles: IObstacleArr read FObstacles;
    property Map: TRoomMap read FMap;
    property UI : IGameUI read FUI write FUI;

    procedure KeyPress(KeyCode: Integer);
    procedure MouseMove(xpos, ypos: Integer);
    procedure MouseClick(button: Integer; xpos, ypos: Integer);
    procedure EndTurn();

    property OnLeaveBattleRoom: TOnLeaveBattleRoom read FOnLeaveBattleRoom write FOnLeaveBattleRoom;

    function  InAction(): Boolean;
    procedure AddAction(AAction: IBRA_Action);

    procedure UpdateStep();
    procedure PrepareToDraw();
    procedure Draw3DUI();
    procedure GenerateWithLoad(const AFileName: string; const ADoors: TDoors; const AForPlayer: TPlayer = nil; const AVisitedRooms: IVisitedRooms = nil);
    procedure GenerateEmpty();
    procedure DrawObstaclePreview(const AName: String; const bmp: TBitmap);

    procedure DetachPlayer();
    procedure AttachPlayer(const APlayer: TPlayer; const ADoorIdx: Integer);
    procedure AttachPlayer(const APlayer: TPlayer; const ARoomPos: TVec2i; ARoomDir: Integer);
    procedure TryLeaveRoom(const AUnit: TRoomUnit; const ADoorIdx: Integer);
    function Units: IRoomUnitArr;

    procedure SaveRoomMap(const AStream: TStream);
    procedure LoadRoomMap(const AStream: TStream);

    function CreateRoomObject(const AClass: TRoomObjectClass): TRoomObject;
  end;

function CanPlaceObstacle(const ARoom: TRoomMap; const AObstacle: TObstacleDesc; const APos: TVec2i; const ADir: Integer): Boolean;

procedure RegRoomClass(const ARoomClass: TRoomObjectClass);
function  FindRoomClass(const AName: string): TRoomObjectClass;

function TryPlaySound3D(const AName: string; ARoomObject: TRoomObject; const ALooped: Boolean = False): ISoundStream3D;

implementation

{$R 'shaders\CT_shaders.rc'}

uses
  untEnemies, untGraphics, untItems, untSkills, untRoomObstacles, generator;

type
  IRoomClassesMap = {$IfDef FPC}specialize{$EndIf}IHashMap<string, TRoomObjectClass>;
  TRoomClassesMap = {$IfDef FPC}specialize{$EndIf}THashMap<string, TRoomObjectClass>;

var
  gvRegRommClasses: IRoomClassesMap;


function TryPlaySound3D(const AName: string; ARoomObject: TRoomObject; const ALooped: Boolean = False): ISoundStream3D;
var sndPos: TSoundPos;
    attr  : TSoundAttr3D;
begin
  Result := nil;
  if AName = '' then Exit;
  if ARoomObject = nil then Exit;
  Result := GetLightPlayer.GetStream3D(AName);
  sndPos := Result.Pos3D;
  sndPos.Pos := ARoomObject.Pos + Vec(0, 0.5, 0);
  attr := Result.Attr3D;
  attr.DistRange := Vec(4, 2000);
  Result.Attr3D := attr;
  Result.Pos3D := sndPos;
  Result.Play(ALooped);
end;

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

{ TBRA_UnitKickAttack }

function TBRA_UnitKickAttack.ProcessAction: Boolean;
var buff: IUnitBuff;
    wallPos: TVec2i;
    wallObj: TRoomObject;
    dir: TVec2i;
    exDamage: Integer;

    knockBackDone: Boolean;
begin
  if (FRoomUnit.World.GameTime > FSoundTime) and (FSkill <> nil) then
  begin
    FSoundTime := HUGE;
    TryPlaySound3D(FSkill.Sound, FRoomUnit);
  end;

  if FRoomUnit.World.GameTime > FDamageStartTime then
  begin
    FDamageStartTime := HUGE;
    if FSkill.IsAttackSkill then
      if FSkill.SampleHitChance(FRoomUnit, FTarget) then
      begin
        dir := FTarget.RoomPos - FRoomUnit.RoomPos;
        wallPos := FTarget.RoomPos + dir;
        wallObj := FRoomUnit.Room.ObjectAt(wallPos);
        exDamage := 0;
        if (wallObj <> nil) or not FTarget.Room.IsCellExists(wallPos) then
        begin
          FInKnockBack := False;
          exDamage := 10;
          if (wallObj is TRoomUnit) then
          begin
            FTarget.Room.AddAction(TBRA_MakeDamage.Create(TRoomUnit(wallObj), FTarget, 10, TRoomUnit(wallObj).Name + ' получает 10 урона от столкновения'));
          end
          else
          begin
            buff := FSkill.SampleBuffChance(FRoomUnit, FTarget);
            if buff <> nil then
            begin
              FTarget.Room.AddMessage(FTarget.Name + ' оглушена на ' + IntToStr(buff.Duration) + ' хода');
              FTarget.Room.AddFlyOutMessage('Оглушение', FTarget.RoomPos, Vec(1,0,0));
              FTarget.ApplyBuff(buff);
            end;
          end;
        end
        else
        begin
          FKnockBackFrom := FTarget.Room.UI.TilePosToWorldPos(FTarget.RoomPos);
          FInKnockBack := True;
          FKnockBack := 1;
          FTarget.RoomPos := wallPos;
          FKnockBackTo := FTarget.Room.UI.TilePosToWorldPos(FTarget.RoomPos);
        end;

        if FTarget.Room.RoomFloor.IsHole[FTarget.RoomPos] then
          FTarget.InstantKill(FTarget.Name + ' свалился в пропасть')
        else
          FTarget.Room.AddAction(TBRA_MakeDamage.Create(FTarget, FRoomUnit, FSkill.SampleDamage(FRoomUnit, FTarget) + exDamage ));
      end;
  end;

  knockBackDone := True;
  if FInKnockBack then
  begin
    FKnockBack := FKnockBack * 0.9;
    if FKnockBack < 0.05 then
      FKnockBack := 0
    else
      knockBackDone := False;
    FTarget.Pos := Lerp(FKnockBackTo, FKnockBackFrom, FKnockBack);
  end;

  Result := (FRoomUnit.World.GameTime < FActionTime) or not knockBackDone;
end;

constructor TBRA_UnitKickAttack.Create(const AUnit, ATarget: TRoomUnit; const ASkill: IUnitSkill; const ADurationTime, ADamageStartTime, ASoundTime: Integer);
begin
  Assert(AUnit <> nil);
  Assert(ATarget <> nil);
  Assert(ASkill <> nil);
  FSkill := ASkill;

  FRoomUnit := AUnit;
  FTarget := ATarget;
  FActionTime := FRoomUnit.World.GameTime + ADurationTime;
  FDamageStartTime := FRoomUnit.World.GameTime + ADamageStartTime;
  FSoundTime := FRoomUnit.World.GameTime + ASoundTime;

  FRoomUnit.SetAnimation([FSkill.Animation]);
  FRoomUnit.RoomDir := FRoomUnit.Room.Direction(FRoomUnit.RoomPos, FTarget.RoomPos);
end;

{ TBRA_LootGround }

function TBRA_LootGround.GetBattleRoom: TBattleRoom;
begin
  Result := TBattleRoom(FUnit.FindAtParents(TBattleRoom));
end;

procedure TBRA_LootGround.ShowOtherInventory;
begin
  GetBattleRoom.UI.SetOtherInventory(FInventoryUnit.Inventory(), {$IfDef FPC}@{$EndIf}OnCloseOtherInventory);
end;

procedure TBRA_LootGround.HideOtherInventory;
begin
  GetBattleRoom.UI.SetOtherInventory(nil, nil);
end;

procedure TBRA_LootGround.OnCloseOtherInventory(ASender: TObject);
begin
  TryCancel;
end;

procedure TBRA_LootGround.TryCancel;
begin
  if not FInAction then Exit;
  FInAction := False;
  HideOtherInventory;
  TryPlaySound3D('sounds\BagClose.mp3', FUnit);
  FUnit.Room.InvalidateInventoryBagsOnGround;
end;

function TBRA_LootGround.ProcessAction: Boolean;
begin
  Result := FInAction;
end;

constructor TBRA_LootGround.Create(AUnit: TRoomUnit);
var inv: IRoomObjectArr;
begin
  FUnit := AUnit;
  FInventoryUnit := nil;
  inv := FUnit.Room.InventoryObjectsAt(FUnit.RoomPos, True);
  if inv.Count = 0 then
  begin
    FInAction := False;
    FUnit.Room.AddFlyOutMessage('На земле ничего нет', FUnit.RoomPos, Vec(1,0,0));
    Exit;
  end;
  if FUnit.AP <= 0 then
  begin
    FInAction := False;
    FUnit.Room.AddFlyOutMessage('Не хватает 1 ОД', FUnit.RoomPos, Vec(1,0,0));
    Exit;
  end;
  FInAction := True;
  FUnit.AP := FUnit.AP - 1;
  FInventoryUnit := inv[0];
  ShowOtherInventory;
  TryPlaySound3D('sounds\BagOpen.mp3', FUnit);
end;

{ TRoomCellFilter_ViewableExclude_ObjectsExclude }

function TRoomCellFilter_ViewableExclude_ObjectsExclude.IsValid(const ACell: TVec2i): Boolean;
var
  obj: TRoomObject;
begin
  obj := FRoom.ObjectAt(ACell);
  if obj = nil then Exit(True);
  if FExclude.Contains(obj) then Exit(True);
  Result := not FRoom.IsCellBlockView(ACell);
end;

constructor TRoomCellFilter_ViewableExclude_ObjectsExclude.Create(const ARoom: TRoomMap; const AObjectsForExclude: array of TRoomObject);
var
  i: Integer;
begin
  FRoom := ARoom;
  FExclude := TRoomObjectSet.Create();
  for i := 0 to Length(AObjectsForExclude) - 1 do
    FExclude.Add(AObjectsForExclude[i]);
end;

{ TBRA_ComingIn }

function TBRA_ComingIn.MoveToNextCell: Boolean;
begin
  if FMovePath = nil then Exit(False);
  FRoomUnit.RoomPos := FMovePath[FMovePathIdx];

  Inc(FMovePathIdx);
  if FMovePathIdx > FMovePath.Count - 1 then Exit(False);

  FRoomUnit.RoomDir := FRoomUnit.Room.Direction(FRoomUnit.RoomPos, FMovePath[FMovePathIdx]);

  FMovePathWeight := 0;
end;

function TBRA_ComingIn.Name: string;
begin
  Result := ClassName;
end;

function TBRA_ComingIn.ProcessAction: Boolean;
var fromPt, toPt: TVec3;
begin
  if FMovePath = nil then Exit(False);
  if FMovePath.Count = 0 then Exit(False);

  Result := True;
  FMovePathWeight := FMovePathWeight + FRoomUnit.Main.UpdateStatesInterval / 1000 * FRoomUnit.GetUnitMoveSpeed;
  if FMovePathWeight >= 1 then
    if not MoveToNextCell then
    begin
      FRoomUnit.AnimateState := asStand;
      FRoomUnit.SetAnimation([]);
      FRoomUnit.RegisterAtRoom();
      Exit(False);
    end;

  fromPt := FRoomUnit.Room.UI.TilePosToWorldPos(FRoomUnit.RoomPos);
  toPt := FRoomUnit.Room.UI.TilePosToWorldPos(FMovePath[FMovePathIdx]);
  FRoomUnit.Pos := Lerp(fromPt, toPt, FMovePathWeight);
end;

constructor TBRA_ComingIn.Create(const AUnit: TRoomUnit; const ADoorIdx: Integer);
var door: TRoomDoor;
begin
  FRoomUnit := AUnit;
  FMovePathIdx := 0;
  FMovePathWeight := 0;

  door := AUnit.Room.RoomFloor.FDoors[ADoorIdx];
  Assert(door <> nil);

  door.Opened := True;
  FMovePath := TRoomPath.Create();
  FMovePath.Add(TTileUtils.RotateTileCoord(Vec(-1,1), door.RoomDir) + door.RoomPos);
  FMovePath.Add(door.RoomPos);

  if (FMovePath <> nil) and (FRoomUnit <> nil) then
  begin
    FRoomUnit.RoomDir := FRoomUnit.Room.Direction(FRoomUnit.RoomPos, FMovePath[0]);
    FRoomUnit.AnimateState := asWalk;
    FRoomUnit.SetAnimation([]);
  end;
end;

{ TRoomDoor }

procedure TRoomDoor.SetOpened(AValue: Boolean);
begin
  if FOpened = AValue then Exit;
  if not AValue then
    UnregisterAtRoom();
  FOpened := AValue;
  KillBlockingUnits;
  if AValue then
    RegisterAtRoom();

  if FOpened then
  begin
    SubscribeForUpdateStep(2000);
    FAnim.SetTime(World.GameTime);
    FAnim.AnimationSequence_StartAndStopOther(['Door_Opening'], False);
    TryPlaySound3D('sounds\DoorSmallOpen.mp3', Self);
  end
  else
  begin
    SubscribeForUpdateStep(2000);
    FAnim.SetTime(World.GameTime);
    FAnim.AnimationSequence_StartAndStopOther(['Door_Closing'], False);
    TryPlaySound3D('sounds\DoorSmallClose.mp3', Self);
  end;
end;

procedure TRoomDoor.KillBlockingUnits;
var tile: TVec2i;
    obj : TRoomObject;
    i: Integer;
begin
  for i := 0 to BlockedCellsCount - 1 do
  begin
    tile := GetAbsoluteBlockedCell(i);
    obj := FRoom.ObjectAt(tile);
    if (obj <> nil) and (obj is TRoomUnit) then
      TRoomUnit(obj).InstantKill(TRoomUnit(obj).Name + ' прихлопнут дверью');
  end;
end;

procedure TRoomDoor.UpdateStep;
begin
  inherited UpdateStep;
  if FAnim <> nil then
    FAnim.SetTime(World.GameTime);
end;

procedure TRoomDoor.LoadModels;
var model: IavModelInstance;
begin
  model := World.Renderer.CreateModelInstances(['Door'])[0];
  FModels.Add(model);
  FAnim := Create_IavAnimationController(model.Mesh.Pose, World.GameTime);
end;

function TRoomDoor.BlockedCellsCount: Integer;
begin
  if FOpened then
    Result := 2
  else
    Result := 0;
end;

function TRoomDoor.GetBlockedCell(AIndex: Integer): TVec2i;
begin
  if AIndex = 0 then
    Result := Vec(0, -1)
  else
    Result := Vec(-1, -1);
end;

function TRoomDoor.BlockedViewCell(AIndex: Integer): Boolean;
begin
  Result := True;
end;

{ TRoomUnit.TUnitInventory }

function TRoomUnit.TUnitInventory.Pop(const AIndex: Integer): IUnitItem;
var i: Integer;
    newskills: IUnitSkillArr;
    slots: IUnitSkillArr;
begin
  Result := inherited Pop(AIndex);
  if Result <> nil then
  begin
    if Result.Equipped then
      TRoomUnit(FOwner).Unequip(Result.Slot);

    newskills := TRoomUnit(FOwner).AllSkills();
    slots := TRoomUnit(FOwner).SkillSlots;
    for i := 0 to slots.Count - 1 do
    begin
      if newskills.IndexOf(slots[i]) < 0 then
        slots[i] := nil;
    end;
  end;
end;

{ TInventory }

function TInventory.Owner: TRoomObject;
begin
  Result := FOwner;
end;

function TInventory.StateID: Integer;
begin
  Result := FStateID;
end;

function TInventory.Items: IUnitItemArr;
begin
  Result := FInventory;
end;

function TInventory.Pop(const AIndex: Integer): IUnitItem;
begin
  Result := FInventory[AIndex];
  FInventory.Delete(AIndex);
  Inc(FStateID);
end;

function TInventory.Pop(const AItem: IUnitItem): Boolean;
var n: Integer;
begin
  n := FInventory.IndexOf(AItem);
  Result := n >= 0;
  if Result then Pop(n);
end;

function TInventory.Push(const AItem: IUnitItem; const AIndex: Integer): Integer;
begin
  Assert(FInventory.IndexOf(AItem) = -1);
  if AIndex = -1 then
    Result := FInventory.Add(AItem)
  else
  begin
    FInventory.Insert(AIndex, AItem);
    Result := AIndex;
  end;
  Inc(FStateID);
end;

procedure TInventory.BumpStateID;
begin
  Inc(FStateID);
end;

constructor TInventory.Create(const AOwner: TRoomObject);
begin
  FOwner := AOwner;
  FInventory := TUnitItemArr.Create();
end;

{ TRoomInteractiveObject }

function TRoomInteractiveObject.GetInteractiveIdx(AUnit: TRoomUnit): Integer;
var i: Integer;
begin
  for i := 0 to Interactive_CellsCount - 1 do
    if AUnit.RoomPos = Interactive_GetAbsoluteCell(i) then
      Exit(i);
  Result := -1;
end;

function TRoomInteractiveObject.Interactive_CellsCount: Integer;
begin
  Result := 0;
end;

function TRoomInteractiveObject.Interactive_GetCell(AIndex: Integer): TVec2i;
begin
  Result := Vec(0,0);
end;

function TRoomInteractiveObject.Interactive_GetAbsoluteCell(AIndex: Integer): TVec2i;
begin
  Result := TTileUtils.RotateTileCoord(Interactive_GetCell(AIndex), RoomDir) + RoomPos;
end;

function TRoomInteractiveObject.Interactive_Cost(AIndex: Integer): Integer;
begin
  Result := 0;
end;

function TRoomInteractiveObject.Interactive_Try(AUnit: TRoomUnit): IBRA_Action;
begin
  Result := nil;
end;

{ TRoomFloor }

procedure TRoomFloor.SetDoorsOpened(AValue: Boolean);
var
  i: Integer;
begin
  if FDoorsOpened = AValue then Exit;
  FDoorsOpened := AValue;
  for i := 0 to 5 do
    if FDoors[i] <> nil then
      FDoors[i].Opened := FDoorsOpened;
end;

function TRoomFloor.GetIsHole(const ATileCoord: TVec2i): Boolean;
begin
  Result := FHoles.Contains(ATileCoord);
end;

procedure TRoomFloor.SetIsHole(const ATileCoord: TVec2i; AValue: Boolean);
begin
  if AValue then
  begin
    if FHoles.Add(ATileCoord) then
      InvalidateFloor;
  end
  else
  begin
    if FHoles.Delete(ATileCoord) then
      InvalidateFloor;
  end;
end;

procedure TRoomFloor.ValidateFloor;

  function GetPitIndex(const ACrd: TVec2i; out AIdx: Integer; out ADir: Integer): Boolean;

    function MatchPattern(ACurrDir: Integer; Pattern: array of Integer): Boolean;
    var i: Integer;
    begin
      Assert(Length(Pattern) = 6);
      for i := 0 to 5 do
      begin
        if Pattern[i] = 2 then Continue;
        if (Pattern[i] = 1) <> (FHoles.Contains( TTileUtils.RotateTileCoord(Vec(1, 0), ACurrDir + i) + ACrd ) ) then
          Exit(False);
      end;
      Result := True;
    end;

  var
    i: Integer;
  begin
    Result := FHoles.Contains(ACrd);
    if not Result then Exit;
    for i := 0 to 5 do
    begin
      ADir := i;
      if MatchPattern(ADir, [1, 1, 0, 2, 2, 0]) then
      begin
        AIdx := 0;
        Exit;
      end;

      if MatchPattern(ADir, [1, 1, 1, 0, 2, 0]) then
      begin
        AIdx := 1;
        Exit;
      end;

      if MatchPattern(ADir, [1, 1, 1, 1, 0, 0]) then
      begin
        AIdx := 2;
        Exit;
      end;

      if MatchPattern(ADir, [1, 0, 1, 1, 1, 1]) then
      begin
        AIdx := 4;
        Exit;
      end;
    end;
    AIdx := -1;
    ADir := 0;
  end;

var idx: Integer;
    i, j, n: Integer;
    hex: array [0..4] of IVec2iArr;
    pit: array [0..5] of IVec3iArr;
    meshInst: IavMeshInstance;
    pitIdx: Integer;
    pitDir: Integer;
    pitModelName: string;
    connectorsCount: Integer;
    connectorDir: Integer;
begin
  if FFloorModels <> nil then Exit;
  FFloorModels := TavModelInstanceArr.Create();

  for i := 0 to Length(hex) - 1 do
    hex[i] := TVec2iArr.Create();
  for i := 0 to Length(pit) - 1 do
    pit[i] := TVec3iArr.Create();

  idx := 2;
  for i := -Room.Radius to Room.Radius do
    for j := -Room.Radius to Room.Radius do
    begin
      if Room.Distance(Vec(0,0), Vec(i, j)) > Room.FRadius then Continue;
      if GetPitIndex(Vec(i, j), pitIdx, pitDir) then
      begin
        if pitIdx >= 0 then
          pit[pitIdx].Add(Vec(i, j, pitDir));
      end
      else
      begin
        //idx := Random(5) + 1;
        if Room.InEditMode then
          idx := 1
        else
          //idx := WeightedRandom([5, 50, 2, 1, 1]);
          idx := WeightedRandom([0, 50, 0, 0, 0]);
        hex[idx].Add(Vec(i,j));
      end;
    end;

  for j := 0 to Length(hex) - 1 do
  begin
    if hex[j].Count = 0 then Continue;

    meshInst := World.Renderer.FindPrefabInstances('Hex'+IntToStr(j+1));
    n := FFloorModels.Add(World.Renderer.ModelsCollection.AddFromMesh(meshInst.Mesh, hex[j].Count));
    for i := 0 to hex[j].Count - 1 do
      FFloorModels[n].MultiMesh[i].Transform := Room.UI.TileToWorldTransform(hex[j][i], 0);
  end;

  for j := 0 to Length(pit) - 1 do
  begin
    if pit[j].Count = 0 then Continue;

    case j of
      0: pitModelName := 'Pit_corner0';
      1: pitModelName := 'Pit_corner1';
      2: pitModelName := 'Pit_line';
      3: pitModelName := 'Pit_corner0';
      4: pitModelName := 'Pit_corner1';
    else
      Assert(False);
      Continue;
    end;

    meshInst := World.Renderer.FindPrefabInstances(pitModelName);
    n := FFloorModels.Add(World.Renderer.ModelsCollection.AddFromMesh(meshInst.Mesh, pit[j].Count));
    for i := 0 to pit[j].Count - 1 do
      FFloorModels[n].MultiMesh[i].Transform := Room.UI.TileToWorldTransform(pit[j][i].xy, pit[j][i].z);

    case j of
      0: pitModelName := 'Pit_corner0_hex';
      1: pitModelName := 'Pit_corner1_hex';
      2: pitModelName := 'Pit_line_hex';
      3: pitModelName := '';
      4: pitModelName := 'Pit_corner1_hex2';
    else
      Assert(False);
      Continue;
    end;

    if pitModelName <> '' then
    begin
      meshInst := World.Renderer.FindPrefabInstances(pitModelName);
      n := FFloorModels.Add(World.Renderer.ModelsCollection.AddFromMesh(meshInst.Mesh, pit[j].Count));
      for i := 0 to pit[j].Count - 1 do
        FFloorModels[n].MultiMesh[i].Transform := Room.UI.TileToWorldTransform(pit[j][i].xy, pit[j][i].z);
    end;
  end;

  connectorsCount := 0;
  for j := 0 to Length(pit) - 1 do
    Inc(connectorsCount, pit[j].Count);
  if connectorsCount > 0 then
  begin
    meshInst := World.Renderer.FindPrefabInstances('Pit_connector');
    n := FFloorModels.Add(World.Renderer.ModelsCollection.AddFromMesh(meshInst.Mesh, connectorsCount));
    connectorsCount := 0;
    for j := 0 to Length(pit) - 1 do
    begin
      if pit[j].Count = 0 then Continue;
      for i := 0 to pit[j].Count - 1 do
      begin
        if j = 4 then
          connectorDir := (pit[j][i].z + 2) mod 6
        else
          connectorDir := pit[j][i].z;
        FFloorModels[n].MultiMesh[connectorsCount + i].Transform := Room.UI.TileToWorldTransform(pit[j][i].xy, connectorDir);
      end;
      Inc(connectorsCount, pit[j].Count);
    end;
  end;
end;

procedure TRoomFloor.InvalidateFloor;
begin
  FFloorModels := nil;
end;

procedure TRoomFloor.InitModels();
begin
  if FInited then Exit;
  InvalidateFloor;
  AddWalls();
  FInited := True;
end;

procedure TRoomFloor.AfterRegister;
var NewBox: TAABB;
begin
  inherited AfterRegister;
  FNonRegistrable := True;
  FHoles := TVec2iSet.Create();

  NewBox.max :=  Vec(cRoomRadius+1, 4, cRoomRadius+1);
  NewBox.min := -Vec(cRoomRadius+1, 2, cRoomRadius+1);
  BBox := NewBox;
end;

procedure TRoomFloor.AddWalls();
var OWall: array[0..1] of IVec3iArr;
    OWallConn: IVec3iArr;
    v, vCenter: TVec2i;
    i, j, n: Integer;
    meshInst: IavMeshInstance;
    tileDir: Integer;
    tilePos: TVec2i;
begin
  Assert(Room.Radius mod 2 = 0, 'Odd map radius is not valid');

  OWall[0] := TVec3iArr.Create();
  OWall[1] := TVec3iArr.Create();
  OWallConn := TVec3iArr.Create();

  vCenter := Vec(-Room.Radius div 2, Room.Radius);
  for j := 0 to 5 do
  begin
    tileDir := j;
    tilePos := TTileUtils.RotateTileCoord(vCenter, tileDir);

    if Room.FDoors[j] then
    begin
      FDoors[j] := TRoomDoor.Create(Self);
      FDoors[j].LoadModels;
      FDoors[j].RoomPos := tilePos;
      FDoors[j].RoomDir := tileDir;
    end
    else
    begin
      FDoors[j] := nil;
      OWall[Random(2)].Add(Vec(vCenter, j));
    end;

    for i := 1 to (Room.Radius div 3) div 2 do
    begin
      v := vCenter + Vec(i * 3, 0);
      OWall[Random(2)].Add(Vec(v, j));
      OWallConn.Add(Vec(v, j));

      v := vCenter - Vec(i * 3, 0);
      OWall[Random(2)].Add(Vec(v, j));
      OWallConn.Add(Vec(v + Vec(3, 0), j));
    end;
  end;

  for j := 0 to Length(OWall) - 1 do
  begin
    meshInst := World.Renderer.FindPrefabInstances('OWall'+IntToStr(j));
    n := FModels.Add(World.Renderer.ModelsCollection.AddFromMesh(meshInst.Mesh, OWall[j].Count));
    for i := 0 to OWall[j].Count - 1 do
    begin
      tileDir := OWall[j][i].z;
      tilePos := TTileUtils.RotateTileCoord(OWall[j][i].xy, tileDir);
      FModels[n].MultiMesh[i].Transform := Room.UI.TileToWorldTransform(tilePos, tileDir);
    end;
  end;

  meshInst := World.Renderer.FindPrefabInstances('OWall2');
  n := FModels.Add(World.Renderer.ModelsCollection.AddFromMesh(meshInst.Mesh, OWallConn.Count));
  for i := 0 to OWallConn.Count - 1 do
  begin
    tileDir := OWallConn[i].z;
    tilePos := TTileUtils.RotateTileCoord(OWallConn[i].xy, tileDir);
    FModels[n].MultiMesh[i].Transform := Room.UI.TileToWorldTransform(tilePos, tileDir);
  end;

  meshInst := World.Renderer.FindPrefabInstances('OWall3');
  n := FModels.Add(World.Renderer.ModelsCollection.AddFromMesh(meshInst.Mesh, 6));
  for i := 0 to 5 do
  begin
    tileDir := i;
    tilePos := TTileUtils.RotateTileCoord(Vec(Room.Radius, 0), tileDir);
    FModels[n].MultiMesh[i].Transform := Room.UI.TileToWorldTransform(tilePos, tileDir);
  end;
end;

procedure TRoomFloor.WriteModels(const ACollection: IavModelInstanceArr; AType: TModelType);
var
  i: Integer;
begin
  ValidateFloor;
  inherited WriteModels(ACollection, AType);
  if AType = mtDefault then
    for i := 0 to FFloorModels.Count - 1 do
      ACollection.Add(FFloorModels[i]);
  if FBackPackModel <> nil then
    ACollection.Add(FBackPackModel);
end;

function TRoomFloor.AllHoles: IVec2iSet;
begin
  Result := FHoles;
end;

function TRoomFloor.DoorCellIndex(const ATileCoord: TVec2i): Integer;
var i: Integer;
begin
  if not FDoorsOpened then Exit(-1);
  for i := 0 to 5 do
    if FDoors[i] <> nil then
      if FDoors[i].RoomPos = ATileCoord then Exit(i);
  Result := -1;
end;

procedure TRoomFloor.UpdateInventoryObjects;
var pts: IVec2iSet;
    meshInst: IavMeshInstance;
    pt: TVec2i;
    n : Integer;
begin
  pts := Room.AllInventoryObjectsPos(True);
  if pts.Count = 0 then
  begin
    FBackPackModel := nil;
    Exit;
  end;

  meshInst := World.Renderer.FindPrefabInstances('BackPack');
  FBackPackModel := World.Renderer.ModelsCollection.AddFromMesh(meshInst.Mesh, pts.Count);
  pts.Reset;
  n := 0;
  while pts.Next(pt) do
  begin
    FBackPackModel.MultiMesh[n].Transform := Room.UI.TileToWorldTransform(pt, 0);
    Inc(n);
  end;
end;

{ TRoomMapGraph_CustomFilter }

function TRoomMapGraph_CustomFilter.IsNonBlockingObject(AObj: TRoomObject): Boolean;
begin
  if AObj = nil then Exit(True);
  Result := FFilter.IsValid(AObj.RoomPos);
end;

constructor TRoomMapGraph_CustomFilter.Create(const ARoomMap: TRoomMap; AFilter: IRoomCellFilter);
begin
  FRoomMap := ARoomMap;
  FFilter := AFilter;
end;

{ TRoomCellComparer_MaxDistance }

function TRoomCellComparer_MaxDistance.Compare(const Left, Right): Integer;
var L: TVec2i absolute Left;
    R: TVec2i absolute Right;
    dL, dR: Integer;
begin
  dL := FRoom.Distance(FStartPt, L);
  dR := FRoom.Distance(FStartPt, R);
  Result := dR - dL;
end;

constructor TRoomCellComparer_MaxDistance.Create(const AUnit: TRoomUnit);
begin
  Create(AUnit.Room, AUnit.RoomPos);
end;

constructor TRoomCellComparer_MaxDistance.Create(const ARoom: TRoomMap; const AStartPt: TVec2i);
begin
  FRoom := ARoom;
  FStartPt := AStartPt;
end;

{ TRoomCellFilter_ViewableExcludeUnits }

function TRoomCellFilter_ViewableExcludeUnits.IsValid(const ACell: TVec2i): Boolean;
var
  obj: TRoomObject;
begin
  obj := FRoom.ObjectAt(ACell);
  if obj = nil then Exit(True);
  if obj is TRoomUnit then Exit(True);
  Result := not FRoom.IsCellBlockView(ACell);
end;

constructor TRoomCellFilter_ViewableExcludeUnits.Create(const ARoom: TRoomMap);
begin
  FRoom := ARoom;
end;

{ TRoomCellFilter_ExcludeUnits }

function TRoomCellFilter_ExcludeUnits.IsValid(const ACell: TVec2i): Boolean;
var
  obj: TRoomObject;
begin
  obj := FRoom.ObjectAt(ACell);
  if obj = nil then Exit(True);
  if obj is TRoomUnit then Exit(True);
  Result := False;
end;

constructor TRoomCellFilter_ExcludeUnits.Create(const ARoom: TRoomMap);
begin
  FRoom := ARoom;
end;

{ TBRA_Shoot }

function TBRA_Shoot.ProcessAction: Boolean;

  procedure DealDamage(ABltInfo: PBulletInfo);
  begin
    if ABltInfo^.hit <> nil then
    begin
      ABltInfo^.hit.DealDamage(FSkill.SampleDamage(FRoomUint, ABltInfo^.hit), ABltInfo^.bullet.Owner);
      TryPlaySound3D('sounds\ArrowImpact1.mp3', FRoomUint);
    end;
    FreeAndNil(ABltInfo^.bullet);
  end;

var
  i: Integer;
  blt: TRoomBullet;
  bltDir: TVec3;
begin
  if FBullets.Count = 0 then Exit(False);
  Result := True;
  if FRoomUint.World.GameTime < FShootDelay then Exit;

  if not FSoundPlayed then
  begin
    FSoundPlayed := True;
    TryPlaySound3D('sounds\BowShot1.mp3', FRoomUint);
  end;

  for i := 0 to FBullets.Count - 1 do
  begin
    blt := FBullets[i].bullet;
    blt.Visible := True;
    bltDir := FBullets[i].stop - FBullets[i].start;
    if LenSqr(bltDir) > 0 then
    begin
      bltDir := normalize(bltDir);
      blt.Pos := blt.Pos + bltDir * (FRoomUint.Main.UpdateStatesInterval/1000*blt.Velocity);
      if LenSqr(blt.Pos - FBullets[i].start) > LenSqr(FBullets[i].stop - FBullets[i].start) then
        DealDamage(PBulletInfo(FBullets.PItem[i]));
    end
    else
      DealDamage(PBulletInfo(FBullets.PItem[i]));
  end;

  for i := FBullets.Count - 1 downto 0 do
    if FBullets[i].bullet = nil then FBullets.DeleteWithSwap(i);
end;

constructor TBRA_Shoot.Create(const AUnit: TRoomUnit;
  const ABullets: array of TRoomBullet; const ASkill: IUnitSkill;
  const AShootDelay: Integer; const ABulletY: Single);
var
  i, j: Integer;
  path: IRoomPath;
  obj: TRoomObject;
  bInfo: TBulletInfo;
  pt: TVec3;
  dir: TVec3;
begin
  Assert(AUnit <> nil);
  Assert(Length(ABullets) > 0);
  Assert(ASkill <> nil);

  AUnit.SetAnimation([ASkill.Animation]);

  FSoundPlayed := False;
  FRoomUint := AUnit;
  FSkill := ASkill;
  FShootDelay := FRoomUint.World.GameTime + AShootDelay;
  FBullets := TBulletInfoArr.Create();
  FBullets.Capacity := Length(ABullets);
  for i := 0 to Length(ABullets) - 1 do
    if ABullets[i] <> nil then
    begin
      bInfo.bullet := ABullets[i];
      bInfo.bullet.Visible := False;
      bInfo.start := FRoomUint.Room.UI.TilePosToWorldPos(ABullets[i].StartPt);
      bInfo.start.y := ABulletY;
      bInfo.bullet.Pos := bInfo.start;
      bInfo.hit := Nil;

      FRoomUint.RoomDir := FRoomUint.Room.Direction(FRoomUint.RoomPos, ABullets[i].Target);

      path := FRoomUint.Room.RayCastDist(ABullets[i].StartPt, ABullets[i].Target, ABullets[i].MaxRange+1, False);
      path.Insert(0, ABullets[i].StartPt);

      bInfo.stop := FRoomUint.Room.UI.TilePosToWorldPos(ABullets[i].Target);
      bInfo.stop.y := ABulletY;

      if ABullets[i].Target = ABullets[i].StartPt then
      begin
        obj := FRoomUint.Room.ObjectAt(ABullets[i].Target);
        if (obj <> nil) and (obj is TRoomUnit) and FSkill.SampleHitChance(FRoomUint, TRoomUnit(obj)) then
          bInfo.hit := TRoomUnit(obj);
        FBullets.Add(bInfo);
        Continue;
      end;

      dir := normalize(bInfo.stop - bInfo.start);
      bInfo.stop := dir * ABullets[i].MaxRange + bInfo.start;
      bInfo.bullet.Rot := Quat(Vec(0,-1,0), arctan2(dir.z, dir.x));

      for j := 0 to path.Count - 1 do
      begin
        obj := FRoomUint.Room.ObjectAt(path[j]);
        if obj = FRoomUint then Continue;

        if (obj <> nil) and (obj is TRoomUnit) then
        begin
          pt := FRoomUint.Room.UI.TilePosToWorldPos(path[j]);
          pt.y := ABulletY;
          pt := Projection(pt, Line(bInfo.start, bInfo.stop - bInfo.start));
          if FSkill.SampleHitChance(FRoomUint, TRoomUnit(obj)) then
          begin
            bInfo.stop := pt;
            bInfo.hit := TRoomUnit(obj);
            Break;
          end;
        end;

        if FRoomUint.Room.IsCellBlockView(path[j]) then
        begin
          pt := FRoomUint.Room.UI.TilePosToWorldPos(path[j]);
          pt.y := ABulletY;
          bInfo.stop := Projection(pt, Line(bInfo.start, bInfo.stop - bInfo.start));
          Break;
        end;
      end;

      FBullets.Add(bInfo);
    end;

  if FBullets.Count = 1 then
    if FBullets[0].hit = nil then
      FRoomUint.Room.AddMessage('Промах!');
end;

{ TRoomBullet }

procedure TRoomBullet.SetVisible(const AValue: Boolean);
begin
  FVisible := AValue;
end;

function TRoomBullet.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TRoomBullet.Accuracy(const ADist: Single): Single;
begin
  Result := 1;
end;

procedure TRoomBullet.LoadModels(const AModelName: string);
begin
  AddModel(AModelName, mtDefault);
  BBox := AABB(Vec(-0.5, -0.5, -0.5), Vec(0.5, 0.5, 0.5));
  FModels[0].Static := False;
  FMaxRange := 100;
  Static := False;
end;

{ TBRA_UnitTurnAction }

function TBRA_UnitTurnAction.ProcessAction: Boolean;
begin
  if RoomUnit.AP > 0 then
  begin
    if NewDir <> RoomUnit.RoomDir then
      RoomUnit.AP := RoomUnit.AP - 1;
    RoomUnit.RoomDir := NewDir;
  end;
  Result := False;
end;

constructor TBRA_UnitTurnAction.Create(const AUnit: TRoomUnit; const ATargetPt: TVec2i);
begin
  RoomUnit := AUnit;
  NewDir := RoomUnit.Room.Direction(RoomUnit.RoomPos, ATargetPt);
end;

constructor TBRA_UnitTurnAction.Create(const AUnit: TRoomUnit; const ANewDir: Integer);
begin
  RoomUnit := AUnit;
  NewDir := ANewDir;
end;

{ TTileUtils }

class function TTileUtils.CircleLen(const ARadius: Integer): Integer;
begin
  Result := ARadius * 6;
end;

class function TTileUtils.PolarToTileCoord(const ARadius, AIndex: Integer): TVec2i;
var dir: Integer;
    idx: Integer;
begin
  dir := AIndex div ARadius;
  idx := AIndex mod ARadius;
  Result := RotateTileCoord(Vec(idx, ARadius - idx), dir);
end;

class function TTileUtils.RotateTileCoord(const APos: TVec2i; ADir: Integer): TVec2i;
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

class function TTileUtils.NeighbourTile(const ACurrent: TVec2i; const AIndex: Integer): TVec2i;
begin
  Result := RotateTileCoord(Vec(0, 1), AIndex) + ACurrent;
end;

class function TTileUtils.DirToQuat(const ADir: Integer): TQuat;
begin
  Result := Quat(Vec(0, 1, 0), 2 * Pi * (ADir / 6));
end;

{ TObstacle }

procedure TObstacle.LoadComplete;
begin

end;

function TObstacle.GetProps: TPersistent;
begin
  Result := nil;
end;

function TObstacle.BlockedCellsCount: Integer;
begin
  if FObstacle.cells = nil then Exit(0);
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
var wholeBox: TAABB;
    cellBox : TAABB;
    cellpos: TVec3;
    i: Integer;
begin
  FObstacle := AObstacle;
  AddModel(FObstacle.name, mtDefault);
  wholeBox := EmptyAABB;
  for i := 0 to AObstacle.cells.Count - 1 do
  begin
    cellpos := Room.UI.TilePosToRoom3DPos(AObstacle.cells[i].xy);
    cellBox.min := cellpos - Vec(0.5, 0.1, 0.5);
    cellBox.max := cellpos + Vec(0.5, 2.0, 0.5);
    wholeBox := wholeBox + cellBox;
  end;
  BBox := wholeBox;
end;

procedure TObstacle.WriteStream(const AStream: TStream);
begin
  FObstacle.WriteStream(AStream);
  AStream.WriteBuffer(FRoomPos, SizeOf(FRoomPos));
  AStream.WriteBuffer(FRoomDir, SizeOf(FRoomDir));
  WritePersistent(AStream, GetProps());
end;

procedure TObstacle.ReadStream(const AStream: TStream; AAutoRegister: Boolean);
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
  SetRoomPosDir(rPos, rDir, AAutoRegister);

  ReadPersistent(AStream, GetProps());
  LoadComplete;
end;

{ TBRA_Action }

function TBRA_Action.GetDone: Boolean;
begin
  Result := FDone;
end;

procedure TBRA_Action.SetDone(const AValue: Boolean);
begin
  FDone := AValue;
end;

function TBRA_Action.Name: string;
begin
  Result := ClassName;
end;

procedure TBRA_Action.TryCancel;
begin

end;

{ TBRA_MakeDamage }

function TBRA_MakeDamage.ProcessAction: Boolean;
begin
  Result := FRoomUnit.World.GameTime < FActionTime;
end;

constructor TBRA_MakeDamage.Create(const AUnit, AFromUnit: TRoomUnit; const ADamage: Integer; const AMsg: string);
begin
  Assert(AUnit <> nil);
  FRoomUnit := AUnit;
  FActionTime := FRoomUnit.World.GameTime + 1000;
  FDamage := ADamage;

  FRoomUnit.DealDamage(FDamage, AFromUnit, AMsg);
end;

{ TBRA_UnitDefaultAttack }

function TBRA_UnitDefaultAttack.ProcessAction: Boolean;
var buff: IUnitBuff;
begin
  if (FRoomUnit.World.GameTime > FSoundTime) and (FSkill <> nil) then
  begin
    FSoundTime := HUGE;
    TryPlaySound3D(FSkill.Sound, FRoomUnit);
  end;

  if FRoomUnit.World.GameTime > FDamageStartTime then
  begin
    FDamageStartTime := HUGE;
    if FSkill.IsAttackSkill then
      if FSkill.SampleHitChance(FRoomUnit, FTarget) then
      begin
        FTarget.Room.AddAction(TBRA_MakeDamage.Create(FTarget, FRoomUnit, FSkill.SampleDamage(FRoomUnit, FTarget) ));
        case FTarget.Material_Body() of
          matFlesh : TryPlaySound3D('sounds\Impact1.mp3', FTarget);
        end;
      end;
    if FSkill.IsBuffSkill then
    begin
      buff := FSkill.SampleBuffChance(FRoomUnit, FTarget);
      if buff <> nil then
        FTarget.ApplyBuff(buff);
    end;
  end;

  Result := FRoomUnit.World.GameTime < FActionTime;
end;

constructor TBRA_UnitDefaultAttack.Create(const AUnit, ATarget: TRoomUnit;
  const ASkill: IUnitSkill; const ADurationTime, ADamageStartTime: Integer;
  const ASoundDelay: Integer);
begin
  Assert(AUnit <> nil);
  Assert(ATarget <> nil);
  Assert(ASkill <> nil);
  FSkill := ASkill;

  FRoomUnit := AUnit;
  FTarget := ATarget;
  FActionTime := FRoomUnit.World.GameTime + ADurationTime;
  FDamageStartTime := FRoomUnit.World.GameTime + ADamageStartTime;
  FSoundTime := FRoomUnit.World.GameTime + ASoundDelay;

  FRoomUnit.SetAnimation([FSkill.Animation]);
  FRoomUnit.RoomDir := FRoomUnit.Room.Direction(FRoomUnit.RoomPos, FTarget.RoomPos);
end;

{ TBRA_UnitMovementAction }

function TBRA_UnitMovementAction.MoveToNextCell: Boolean;
begin
  if RoomUnit.Room.ObjectAt(MovePath[MovePathIdx]) <> nil then Exit(False);
  RoomUnit.RoomPos := MovePath[MovePathIdx];
  RoomUnit.AP := RoomUnit.AP - 1;

  if not RoomUnit.Room.BattleRoom.IsClear then
    if RoomUnit.AP <= 0 then Exit(False);
  if MovePath = nil then Exit(False);
  if Cancelled then Exit(False);

  Inc(MovePathIdx);
  if MovePathIdx > MovePath.Count - 1 then Exit(False);

  if RoomUnit.Room.ObjectAt(MovePath[MovePathIdx]) <> nil then Exit(False);

  RoomUnit.RoomDir := RoomUnit.Room.Direction(RoomUnit.RoomPos, MovePath[MovePathIdx]);

  MovePathWeight := 0;
end;

function TBRA_UnitMovementAction.Name: string;
begin
  Result := inherited Name;
  Result := Result + '('+IntToStr(MovePath.Count)+')';
end;

procedure TBRA_UnitMovementAction.TryCancel;
begin
  Cancelled := True;
end;

function TBRA_UnitMovementAction.ProcessAction: Boolean;
var fromPt, toPt: TVec3;
    n: Integer;
    newFootStepIdx: Int64;
begin
  if MovePath = nil then Exit(False);
  if MovePath.Count = 0 then Exit(False);
  if RoomUnit.AP <= 0 then
  begin
    if RoomUnit.Room.BattleRoom.IsClear then
      Exit(True)
    else
      Exit(False);
  end;

  newFootStepIdx := (RoomUnit.World.GameTime - TimeStart) div 300;
  if newFootStepIdx > StepIdx then
  begin
    StepIdx := newFootStepIdx;
    TryPlaySound3D(RoomUnit.Sound_Footstep(StepIdx), RoomUnit)
  end;

  Result := True;
  MovePathWeight := MovePathWeight + RoomUnit.Main.UpdateStatesInterval / 1000 * MoveSpeed;
  if MovePathWeight >= 1 then
    if not MoveToNextCell then
    begin
      //if RoomUnit is TPlayer then
      //  RoomUnit.Room.BattleRoom.UI.AdjustCameraToPlayerKeepDist();
      //
      RoomUnit.AnimateState := asStand;
      RoomUnit.SetAnimation([]);

      if RoomUnit is TPlayer then
        if RoomUnit.Room.RoomFloor.DoorsOpened then
        begin
          n := RoomUnit.Room.RoomFloor.DoorCellIndex(RoomUnit.RoomPos);
          if n >= 0 then
            RoomUnit.Room.BattleRoom.TryLeaveRoom(RoomUnit, n);
        end;

      Exit(False);
    end;

  fromPt := RoomUnit.Room.UI.TilePosToWorldPos(RoomUnit.RoomPos);
  toPt := RoomUnit.Room.UI.TilePosToWorldPos(MovePath[MovePathIdx]);
  RoomUnit.Pos := Lerp(fromPt, toPt, MovePathWeight);
end;

constructor TBRA_UnitMovementAction.Create(const AUnit: TRoomUnit; const APath: IRoomPath);
begin
  TimeStart := AUnit.World.GameTime;
  StepIdx   := 0;

  RoomUnit := AUnit;
  MovePathIdx := 0;
  MovePathWeight := 0;
  if APath = nil then
    MovePath := nil
  else
    MovePath := APath.Clone();
  MoveSpeed := RoomUnit.GetUnitMoveSpeed();
  if (MovePath <> nil) and (RoomUnit <> nil) and (RoomUnit.AP > 0) then
  begin
    RoomUnit.RoomDir := RoomUnit.Room.Direction(RoomUnit.RoomPos, MovePath[0]);
    RoomUnit.AnimateState := asWalk;
    RoomUnit.SetAnimation([]);
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

function TRoomMapGraph.IsCellExists(const ACell: TVec2i): Boolean;
begin
  Result := FRoomMap.IsCellExists(ACell);
  if Result then
    if FRoomMap.RoomFloor.IsHole[ACell] then Exit(False);
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
  Result := IsCellExists(ANeighbour);
  if not Result then Exit;
  Result := IsNonBlockingObject(FRoomMap.ObjectAt(ANeighbour));
  if not Result then Exit;
  MoveWeight := 1;
  MoveWeight := MoveWeight - dot( (ACurrent - AFrom), (ANeighbour - ACurrent) )*0.01 + 0.01;
  DistWeight := FRoomMap.Distance(ANeighbour, ATarget);
end;

function TRoomMapGraph.GetNeighbour(Index: Integer; const ACurrent: TVec2i; out ANeighbour: TVec2i): Boolean;
begin
  ANeighbour := FRoomMap.NeighbourTile(ACurrent, Index);
  Result := IsCellExists(ANeighbour);
  if not Result then Exit;
  Result := IsNonBlockingObject(FRoomMap.ObjectAt(ANeighbour));
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

procedure TRoomUnit.Unequip(const ASlot: TRoomUnitEqSlot);
begin
  if FEquippedItem[ASlot] <> nil then
  begin
    FEquippedItem[ASlot].Equipped := False;
    FInventory.BumpStateID;
  end;

  FEquippedModel[ASlot] := nil;
  FEquippedItem[ASlot] := nil;
end;

function TRoomUnit.Equip(const AItem: IUnitItem): Boolean;
var
  m: TMat4;
  inst: IavModelInstanceArr;
begin
  if AItem = nil then Exit(False);
  if AItem.Slot = esNone then Exit(False);
  if AP <= 0 then
  begin
    Room.AddMessage(string('Нет очков хода чтобы экипировать ') + AItem.Name);
    Room.AddFlyOutMessage('Нужно 1 ОД', RoomPos, Vec(1,0,0));
    Exit(False);
  end;
  AP := AP - 1;

  Unequip(AItem.Slot);
  if (AItem.Slot = esBothHands) then
  begin
    Unequip(esLeftHand);
    Unequip(esRightHand);
  end
  else
    if AItem.Slot in [esLeftHand, esRightHand] then
      Unequip(esBothHands);

  m := FModels[0].Mesh.BindPoseTransform;
  {$Warning 'temporary workaroud'}
  m.f[0,0] := 100;
  m.f[1,1] := 100;
  m.f[2,2] := 100;
  inst := World.Renderer.CreateModelInstances([AItem.Model]);
  inst[0].Mesh.Transform := m;
  inst[0].Mesh.Pose := FModels[0].Mesh.Pose;
  inst[0].Mesh.Transform := Transform();
  inst[0].Static := False;

  FEquippedModel[AItem.Slot] := inst[0];
  FEquippedItem[AItem.Slot] := AItem;
  AItem.Equipped := True;

  FInventory.BumpStateID;
  Result := True;
end;

function TRoomUnit.GetEquip(const ASlot: TRoomUnitEqSlot): IUnitItem;
begin
  Result := FEquippedItem[ASlot];
end;

procedure TRoomUnit.TemporaryEquip(ASlot: TRoomUnitEqSlot; const AModel: string);
var
  m: TMat4;
  inst: IavModelInstanceArr;
begin
  TemporaryUnEquip(ASlot);
  if (ASlot = esBothHands) then
  begin
    TemporaryUnEquip(esLeftHand);
    TemporaryUnEquip(esRightHand);
  end
  else
    if ASlot in [esLeftHand, esRightHand] then
      TemporaryUnEquip(esBothHands);

  m := FModels[0].Mesh.BindPoseTransform;
  inst := World.Renderer.CreateModelInstances([AModel]);
  inst[0].Mesh.Transform := m;
  inst[0].Mesh.Pose := FModels[0].Mesh.Pose;
  inst[0].Mesh.Transform := Transform();
  inst[0].Static := False;
  FTemporaryEquippedModel[ASlot] := inst[0];
end;

procedure TRoomUnit.TemporaryUnEquip(ASlot: TRoomUnitEqSlot);
begin
  FTemporaryEquippedModel[ASlot] := nil;
end;

function TRoomUnit.AllSkills(): IUnitSkillArr;
var i, j: Integer;
    items: IUnitItemArr;
begin
  if FAllSkillsCacheInventoryLastStateID <> FInventory.StateID then
  begin
    FAllSkillsCache := nil;
    FAllSkillsCacheInventoryLastStateID := FInventory.StateID;
  end;

  if FAllSkillsCache = nil then
  begin
    FAllSkillsCache := TUnitSkillArr.Create();
    FAllSkillsCache.AddArray(FUnitSkills);
    items := Inventory().Items;
    for i := 0 to items.Count - 1 do
    begin
      if items[i] = nil then Continue;
      for j := 0 to items[i].SkillsCount - 1 do
        FAllSkillsCache.Add(items[i].Skill(j));
    end;
  end;

  Result := FAllSkillsCache;
end;

function TRoomUnit.AllBuffs(): IUnitBuffsArr;
begin
  Result := FBuffs;
end;

procedure TRoomUnit.ProcessBuffs;
var i: Integer;
begin
  for i := 0 to FBuffs.Count - 1 do
    if not FBuffs[i].DoStep then
      FBuffs[i] := nil;

  for i := FBuffs.Count - 1 downto 0 do
    if FBuffs[i] = nil then
      FBuffs.Delete(i);
end;

function TRoomUnit.Inventory(): IInventory;
begin
  Result := FInventory;
end;

procedure TRoomUnit.OnRegisterRoomObject(const ANewObject: TRoomObject);
begin

end;

procedure TRoomUnit.OnDead();
begin
  FAnimateState := asDeath;
  Room.BattleRoom.UI.InvalidateEnemiesBar;
end;

procedure TRoomUnit.OnRessurect();
begin
  FAnimateState := asStand;
  Room.BattleRoom.UI.InvalidateEnemiesBar;
end;

function TRoomUnit.AddAnimationPrefix(const ANameSequence: array of string): TStringArray;
var i: Integer;
begin
  if FAnimateState = asDeath then
  begin
    SetLength(Result, 1);
    Result[0] := FAnimationPrefix + 'Death0';
    Exit;
  end;

  SetLength(Result, Length(ANameSequence) + 1);
  for i := 0 to Length(ANameSequence) - 1 do
    Result[i] := FAnimationPrefix + ANameSequence[i];
  case FAnimateState of
    asStand: Result[High(Result)] := FAnimationPrefix + 'Idle0';
    asWalk : Result[High(Result)] := FAnimationPrefix + 'Walk';
  else
    Result[High(Result)] := FAnimationPrefix + 'Idle0';
  end;
end;

procedure TRoomUnit.Notify_PlayerLeave;
begin
  UnSubscribeFromUpdateStep;
end;

procedure TRoomUnit.Notify_PlayerEnter;
begin
  SubscribeForUpdateStep;
end;

procedure TRoomUnit.UpdateStep;
begin
  inherited UpdateStep;
  if FRoom <> nil then
    if FRoom.RoomFloor <> nil then
      if FRoom.RoomFloor.IsHole[RoomPos] then
        Pos := Vec(Pos.x, Max(Pos.y-0.05, -10), Pos.z);
end;

procedure TRoomUnit.AfterRegister;
begin
  inherited AfterRegister;
  Name := 'Игрок';

  if FUnitSkills = nil then
    FUnitSkills := TUnitSkillArr.Create();
  if FSlots10 = nil then
  begin
    FSlots10 := TUnitSkillArr.Create();
    FSlots10.SetSize(10);
  end;
  if FInventory = nil then
    FInventory := TUnitInventory.Create(Self);

  if FBuffs = nil then
    FBuffs := TUnitBuffsArr.Create();
end;

function TRoomUnit.InAbsoluteSight: Boolean;
var
  i: Integer;
begin
  Result := False;
  if FBuffs = nil then Exit;
  for i := 0 to FBuffs.Count - 1 do
    if FBuffs[i].ID = bidAbsoluteSight then Exit(True);
end;

procedure TRoomUnit.WriteModels(const ACollection: IavModelInstanceArr; AType: TModelType);

  function IsEquipOverrided(slot: TRoomUnitEqSlot): Boolean;
  begin
    if FTemporaryEquippedModel[slot] <> nil then Exit(True);
    if slot = esBothHands then
    begin
      if FTemporaryEquippedModel[esLeftHand] <> nil then Exit(True);
      if FTemporaryEquippedModel[esRightHand] <> nil then Exit(True);
    end
    else
      if slot in [esRightHand, esLeftHand] then
      begin
        if FTemporaryEquippedModel[esBothHands] <> nil then Exit(True);
      end;
    Result := False;
  end;

var i: TRoomUnitEqSlot;
begin
  inherited WriteModels(ACollection, AType);
  if AType = mtDefault then
  begin
    for i := Low(TRoomUnitEqSlot) to High(TRoomUnitEqSlot) do
    begin
      if IsEquipOverrided(i) then
      begin
        if FTemporaryEquippedModel[i] <> nil then
        begin
          FTemporaryEquippedModel[i].Transform := Transform();
          ACollection.Add(FTemporaryEquippedModel[i]);
        end;
      end
      else
      begin
        if FEquippedModel[i] <> nil then
        begin
          FEquippedModel[i].Transform := Transform();
          ACollection.Add(FEquippedModel[i]);
        end;
      end;
    end;
  end;
end;

procedure TRoomUnit.WriteDepthOverrideModels(const ACollection: IavModelInstanceArr; const ADepthOverride: IOverrideColorArr);
var overrideColor: TVec3;
    start, i: Integer;
begin
  if IsDead() then Exit;
  if Self is TPlayer then
    overrideColor := Vec(0,0,1)
  else
    overrideColor := Vec(1,0,0);

  start := ACollection.Count;
  WriteModels(ACollection, mtDefault);
  WriteModels(ACollection, mtEmissive);

  for i := start to ACollection.Count - 1 do
    ADepthOverride.Add(overrideColor);
end;

function TRoomUnit.UnitSkills(): IUnitSkillArr;
begin
  Result := FUnitSkills;
end;

procedure TRoomUnit.LoadModels();
begin
  FViewAngle := Pi / 3;
  FViewRange := 10;
end;

procedure TRoomUnit.SetAnimation(const ANameSequence: array of string);
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

function TRoomUnit.FindPath(const ATarget: TVec2i; const AFilter: IRoomCellFilter): IRoomPath;
var graph: IRoomMapGraph;
    pf: IRoomMapPF;
begin
  graph := TRoomMapGraph_CustomFilter.Create(Room, AFilter);
  pf := TRoomMapPF.Create(graph);
  Result := pf.FindPath(RoomPos, ATarget, Infinity, False);
end;

function TRoomUnit.IsDead(): Boolean;
begin
  Result := HP <= 0;
end;

function TRoomUnit.InViewField(const AUnitPos: TVec2i; const AUnitDir: Integer; const APt: TVec2i): Boolean;
var v0, vView, vDir: TVec3;
begin
  if APt = AUnitPos then Exit(True);
  v0 := Room.UI.TilePosToWorldPos(AUnitPos);
  vDir := Room.UI.TilePosToWorldPos(APt) - v0;
  if LenSqr(vDir) <= ViewWholeRange*ViewWholeRange then Exit(True);
  if LenSqr(vDir) > ViewRange*ViewRange then Exit(False);
  vView := Room.UI.TilePosToWorldPos(RotateTileCoord(Vec(1,0), AUnitDir) + AUnitPos) - v0;
  vDir := normalize(vDir);
  vView := normalize(vView);
  Result := dot(vDir, vView) >= Cos(ViewAngle);
end;

function TRoomUnit.InViewField(const APt: TVec2i): Boolean;
begin
  Result := InViewField(RoomPos, RoomDir, APt);
end;

function TRoomUnit.InViewRange(const APt: TVec2i): Boolean;
begin
  Result := LenSqr(Room.UI.TilePosToWorldPos(RoomPos) - Room.UI.TilePosToWorldPos(APt)) <= FViewRange*FViewRange;
end;

function TRoomUnit.CanSeeFromPos(const AFromPos: TVec2i; const APos: TVec2i): Boolean;
begin
  if not InViewField(APos) then Exit(False);
  Result := Room.RayCastBoolean(RoomPos, APos);
end;

function TRoomUnit.CanSee(const APos: TVec2i): Boolean;
begin
  if not InViewField(APos) then Exit(False);
  Result := Room.RayCastBoolean(RoomPos, APos);
end;

function TRoomUnit.CanSee(const AOtherUnit: TRoomUnit): Boolean;
begin
  if AOtherUnit.InAbsoluteSight then Exit(True);
  if not InViewField(AOtherUnit.RoomPos) then Exit(False);
  Result := Room.RayCastBoolean(RoomPos, AOtherUnit.RoomPos);
end;

function TRoomUnit.GetShootPoints(): IVec2iArr;
begin
  Result := Room.GetShootPoints(RoomPos, ViewRange, nil);
end;

function TRoomUnit.GetShootPointsSet(): IVec2iSet;
begin
  Result := Room.GetShootPointsSet(RoomPos, ViewRange, nil);
end;

function TRoomUnit.GetObservablePointSet(const AInSet, AExSet: IVec2iSet): IVec2iSet;
var
    nonvisited_list: IVec2iArr;
    nonvisited_set: IVec2iSet;
    comp_maxdist: IComparer;
    j, i: Integer;
    pt: TVec2i;
    path: IRoomPath;
    inShadow: Boolean;
begin
  Result := TVec2iSet.Create;
  Result.Add(RoomPos);

  //get all cells for check
  nonvisited_set := TVec2iSet.Create();
  nonvisited_list := TVec2iArr.Create();
  for j := -Room.Radius to Room.Radius do
    for i := -Room.Radius to Room.Radius do
    begin
      pt := Vec(i, j);
      if not Room.IsCellExists(pt) then Continue;
      if Room.RoomFloor.IsHole[pt] then Continue;
      if AInSet <> nil then
        if not AInSet.Contains(pt) then Continue;
      if AExSet <> nil then
        if AExSet.Contains(pt) then Continue;
      if not InViewField(pt) then Continue;
      nonvisited_list.Add(pt);
      nonvisited_set.Add(pt);
    end;

  comp_maxdist := TRoomCellComparer_MaxDistance.Create(Self);
  nonvisited_list.Sort(comp_maxdist);
  for i := 0 to nonvisited_list.Count - 1 do
  begin
    if not nonvisited_set.Contains(nonvisited_list[i]) then Continue;

    path := Room.RayCast(RoomPos, nonvisited_list[i], False);
    inShadow := False;
    for j := 0 to path.Count - 1 do
    begin
      if not inShadow then
      begin
        if Room.IsCellBlockView(path[j]) then
          inShadow := True
        else
        begin
          if nonvisited_set.Contains(path[j]) then
            Result.Add(path[j]);
        end;
      end;
      nonvisited_set.Delete(path[j]);
    end;
  end;
end;

function TRoomUnit.GetMovePointsWeighted(AMaxDepth: Integer): IVec2iWeightedSet;
begin
  Result := GetMovePointsWeighted(AMaxDepth, TRoomCellFilter_ExcludeUnits.Create(Room));
end;

function TRoomUnit.GetMovePointsWeighted(AMaxDepth: Integer; const AFilter: IRoomCellFilter): IVec2iWeightedSet;
var graph: IRoomMapNonWeightedGraph;
    iterator: IRoomMapBFS;
    pt: TVec2i;
    depth: Integer;
begin
  Result := TVec2iWeightedSet.Create();

  graph := TRoomMapGraph_CustomFilter.Create(Room, AFilter);
  iterator := TRoomMapBFS.Create(graph);
  iterator.Reset(RoomPos);
  while iterator.Next(pt, depth) do
  begin
    if depth > AMaxDepth then Break;
    Result.Add(pt, depth);
  end;
end;

function TRoomUnit.GetMovePoints(AMaxDepth: Integer): IVec2iSet;
var graph: IRoomMapNonWeightedGraph;
    iterator: IRoomMapBFS;
    pt: TVec2i;
    depth: Integer;
begin
  Result := TVec2iSet.Create();

  graph := TRoomMapGraph_CustomFilter.Create(Room, TRoomCellFilter_ExcludeUnits.Create(Room));
  iterator := TRoomMapBFS.Create(graph);
  iterator.Reset(RoomPos);
  while iterator.Next(pt, depth) do
  begin
    if depth > AMaxDepth then Break;
    Result.Add(pt);
  end;
end;

function TRoomUnit.GetVisible(): Boolean;
begin
  {$IfDef DEBUGBOTS}
    Result := True;
  {$Else}
  if Room.CurrentPlayer = nil then
    Result := False//inherited GetVisible()
  else
    Result := Room.CurrentPlayer.CanSee(Self);
  {$EndIf}
end;

function TRoomUnit.Sound_Footstep(const AStepIndex: Integer): string;
begin
  Result := '';
end;

function TRoomUnit.Material_Body(): TRoomUnitMaterial;
begin
  Result := matNone;
end;

procedure TRoomUnit.ApplyBuff(ABuff: IUnitBuff);
begin
  if ABuff <> nil then
  begin
    FBuffs.Add(ABuff);
    ABuff.SetUnit(Self);
  end;
end;

procedure TRoomUnit.DealPureDamage(ADmg: Integer; AFromUnit: TRoomUnit; const AMsg: string);
var msg: TbFlyOutMessage;
    s: string;
begin
  HP := HP - ADmg;

  msg := TbFlyOutMessage.Create(World);
  msg.SetState(Pos+Vec(0,1.5,0), IntToStr(ADmg), Vec(1,0,0,1));

  if AMsg = '' then
    s := Name + ' получает ' + IntToStr(ADmg) + ' урона'
  else
    s := AMsg;
  if IsDead() then
  begin
    Room.AddMessage(s + ' и умирает');
    SetAnimation([]);
  end
  else
  begin
    Room.AddMessage(s);
    SetAnimation(['React0']);
  end;
end;

procedure TRoomUnit.DealDamage(ADmg: Integer; AFromUnit: TRoomUnit; const AMsg: string);
var
  i: Integer;
begin
  for i := 0 to FBuffs.Count - 1 do
    FBuffs[i].ProcessDamage(ADmg, AFromUnit);
  DealPureDamage(ADmg, AFromUnit, AMsg);
end;

procedure TRoomUnit.InstantKill(const AMessage: string);
begin
  HP := 0;
  Room.AddMessage(AMessage);
  SetAnimation([]);
end;

procedure TRoomUnit.SetAP(const AValue: Integer);
begin
  if FAP = AValue then Exit;
  FAP := AValue;
end;

function TRoomUnit.GetCurrentStats: TRoomUnitStats;
var itemStats: TRoomUnitStats;
  i: Integer;
begin
  Result := FStats;
  for i := 0 to FInventory.Items.Count - 1 do
  begin
    itemStats := FInventory.Items[i].StatsUp;
    Result.Lucky := Result.Lucky + itemStats.Lucky;
  end;
end;

procedure TRoomUnit.SetHP(const AValue: Integer);
begin
  if FHP = AValue then Exit;
  if FRegistered then
  begin
    if (FHP > 0) and (AValue <= 0) then
    begin
      FRoom.RemoveObject(Self);
      FRoom.RegInventoryObject(self);
      OnDead();
    end;
    if (FHP <= 0) and (AValue > 0) then
    begin
      FRoom.PutObject(Self);
      FRoom.UnRegInventoryObject(self);
      OnRessurect();
    end;
  end;
  FHP := AValue;

  if Room <> nil then
    if Room.BattleRoom <> nil then
      if Room.BattleRoom.UI <> nil then
        Room.BattleRoom.UI.InvalidateEnemiesBar;
end;

{ TRoomObject }

procedure TRoomObject.SetRoomPos(const AValue: TVec2i);
begin
  if FRoomPos = AValue then Exit;
  if not FNonRegistrable then
    if FRegistered then FRoom.RemoveObject(Self);
  FRoomPos := AValue;
  if not FNonRegistrable then
    if FRegistered then FRoom.PutObject(Self);
  Pos := FRoom.UI.TilePosToWorldPos(AValue);
end;

function TRoomObject.GetStatic: Boolean;
begin
  Result := inherited GetStatic;
  if not Result then
    Result := FRoom.BattleRoom.Player = nil;
end;

procedure TRoomObject.Notify_PlayerLeave;
begin

end;

procedure TRoomObject.Notify_PlayerEnter;
begin

end;

procedure TRoomObject.AfterRegister;
begin
  inherited AfterRegister;
  BBox := AABB(Vec(-0.5, 0, -0.5), Vec(0.5, 1.0, 0.5));
  Rot := TTileUtils.DirToQuat(FRoom.BattleRoom.RoomDir);
  Pos := FRoom.BattleRoom.WorldPos;
end;

procedure TRoomObject.SetRoomDir(const AValue: Integer);
begin
  if FRoomDir = AValue then Exit;
  if not FNonRegistrable then
    if FRegistered then FRoom.RemoveObject(Self);
  FRoomDir := AValue;
  if not FNonRegistrable then
    if FRegistered then FRoom.PutObject(Self);
  Rot := TTileUtils.DirToQuat(AValue + FRoom.BattleRoom.RoomDir);
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
begin
  Result := TTileUtils.RotateTileCoord(APos, ADir);
end;

function TRoomObject.Inventory(): IInventory;
begin
  Result := nil;
end;

procedure TRoomObject.SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean);
begin
  if not FNonRegistrable then
    if FRegistered then FRoom.RemoveObject(Self);
  FRoomPos := APos;
  FRoomDir := ADir;
  if not FNonRegistrable then
    if FRegistered then FRoom.PutObject(Self);
  if AAutoRegister then RegisterAtRoom();
  Pos := FRoom.UI.TilePosToWorldPos(APos);
  Rot := Quat(Vec(0, 1, 0), 2 * Pi * ((ADir + FRoom.BattleRoom.RoomDir) / 6));
end;

procedure TRoomObject.RegisterAtRoom();
begin
  if FNonRegistrable then Exit;
  if FRegistered then Exit;
  FRegistered := True;
  FRoom.PutObject(Self);
end;

procedure TRoomObject.UnregisterAtRoom();
begin
  if FNonRegistrable then Exit;
  if not FRegistered then Exit;
  FRoom.RemoveObject(Self);
  FRegistered := False;
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
  FTilesProg.Load('UI_DrawTiles', True, '..\shaders\!Out');
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

  FColors[TTileColorID.Normal] := Vec(0,0,0,0);
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

function TRoomUI.RoomMap: TRoomMap;
begin
  Result := Parent as TRoomMap;
end;

procedure TRoomUI.ClearTileColors();
var
  j, i: Integer;
  n: Integer;
begin
  ValidateTiles;
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
    ray: TLine;
begin
  ray := ARay * (Parent as TRoomMap).BattleRoom.RoomTransformInv;

  if Intersect(Plane(0,1,0,FPlaneY), ray, IntPt) then
  begin
    Result := GetTileAtCoords(Vec(IntPt.x, IntPt.z));
    Result := Clamp(Result, -Radius, Radius);
  end
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

function TRoomUI.TilePosToRoom3DPos(const ATilePos: TVec2i): TVec3;
var v: TVec2;
begin
  v := ATilePos * FAffinePack;
  Result := Vec(v.x, 0, v.y);
end;

function TRoomUI.TilePosToWorldPos(const ATilePos: TVec2i): TVec3;
var v: TVec2;
begin
  v := ATilePos * FAffinePack;
  Result := Vec(v.x, 0, v.y);
  Result := Result * RoomMap.BattleRoom.WorldRot + RoomMap.BattleRoom.WorldPos;
end;

function TRoomUI.TileToWorldTransform(const ATilePos: TVec2i; const ATileRot: Integer): TMat4;
begin
  Result := Mat4(Quat(Vec(0, 1, 0), 2 * Pi * (ATileRot / 6)) * RoomMap.BattleRoom.WorldRot, TilePosToWorldPos(ATilePos));
end;

function TRoomUI.AutoTileColor(const APos: TVec2i): TTileColorID;
var rm: TRoomMap;
    obj: TRoomObject;
begin
  ValidateTiles;
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

procedure TRoomUI.DrawUI(const ATransform: TMat4);
var tmpColors: TVec4Arr;
  i: Integer;
begin
  ValidateTiles;
  if FTilesData.Count = 0 then Exit;
  SetLength(tmpColors, Length(FColors));

  ValidateFogOfWar;

  FTilesProg.Select();
  FTilesProg.SetUniform('YPos', FPlaneY);

  for i := 0 to Length(FColors) - 1 do
    tmpColors[i] := FColorsFogOfWar[TTileColorID(i)];
  FTilesProg.SetAttributes(nil, nil, FTilesVBFogOfWar);
  FTilesProg.SetUniform('Transform', ATransform);
  FTilesProg.SetUniform('TileColors', tmpColors);
  FTilesProg.SetUniform('gradPow', 1.0);
  FTilesProg.SetUniform('minAlpha', 0.7);
  FTilesProg.Draw(ptTriangles, cmNone, False, FTilesData.Count, 0, 18);

  for i := 0 to Length(FColors) - 1 do
    tmpColors[i] := FColors[TTileColorID(i)];
  FTilesProg.SetAttributes(nil, nil, FTilesVB);
  FTilesProg.SetUniform('Transform', ATransform);
  FTilesProg.SetUniform('TileColors', tmpColors);
  FTilesProg.SetUniform('gradPow', 8.0);
  FTilesProg.SetUniform('minAlpha', 0.0);
  FTilesProg.Draw(ptTriangles, cmNone, False, FTilesData.Count, 0, 18);
end;

function TRoomUI.GetColors(ID: TTileColorID): TVec4;
begin
  Result := FColors[ID];
end;

procedure TRoomUI.SetColors(ID: TTileColorID; const AValue: TVec4);
begin
  ValidateTiles;
  FColors[ID] := AValue;
end;

procedure TRoomUI.ValidateFogOfWar;
var
  w, j, i: Integer;
  new_tile: THexTile;
  rm: TRoomMap;
begin
  if FFogOfWarValid then Exit;
  ValidateTiles;
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
  ValidateTiles;
  if not (Parent as TRoomMap).IsCellExists(APos) then Exit;

  pTile := PHexTile(FTilesData.PItem[PosToIndex(APos)]);
  if pTile^.vsColor = AColorID then Exit;
  pTile^.vsColor := AColorID;
  FTilesVB.Invalidate;
end;

function TRoomUI.GetTileColor(const APos: TVec2i): TTileColorID;
begin
  ValidateTiles;
  Result := PHexTile(FTilesData.PItem[PosToIndex(APos)])^.vsColor;
end;

procedure TRoomUI.ValidateTiles;
var
  j, i: Integer;
  new_tile: THexTile;
  w: Integer;
begin
  if FTilesData.Count > 0 then Exit;
  w := FRadius * 2 + 1;
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

procedure TRoomUI.SetMapRadius(const ARadius: Integer);
begin
  if ARadius = FRadius then Exit;
  FRadius := ARadius;
  FTilesData.Clear();
end;

{ TRoomMap }

procedure TRoomMap.SetRadius(const AValue: Integer);
begin
  if FRadius = AValue then Exit;
  FRadius := AValue;
  FRoomUI.Radius := AValue;

  FreeAndNil(FRoomFloor);
  if FRadius > 0 then
  begin
    FRoomFloor := TRoomFloor.Create(Self);
    FRoomFloor.InitModels;
  end;
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
  Result := TTileUtils.NeighbourTile(ACurrent, AIndex);
end;

procedure TRoomMap.AfterRegister;
begin
  inherited AfterRegister;
  FRoomUI := TRoomUI.Create(Self);

  FObjects := TObjectMap.Create();
  FInventoryObjects := TRoomObjectSet.Create;
end;

function TRoomMap.GetBattleRoom: TBattleRoom;
var obj: TavObject;
begin
  obj := Self;
  repeat
    obj := obj.Parent;
    if obj is TBattleRoom then Exit(TBattleRoom(obj));
  until obj = nil;
  Result := nil;
end;

procedure TRoomMap.Notify_PlayerLeave;
var
  i: Integer;
begin
  for i := 0 to ChildCount - 1 do
    if Child[i] is TRoomObject then
      TRoomObject(Child[i]).Notify_PlayerLeave;
end;

procedure TRoomMap.Notify_PlayerEnter;
var
  i: Integer;
begin
  for i := 0 to ChildCount - 1 do
    if Child[i] is TRoomObject then
      TRoomObject(Child[i]).Notify_PlayerEnter;
end;

procedure TRoomMap.SetDoorsRadius(const ADoors: TDoors; const ARadius: Integer);
begin
  FDoors := ADoors;
  Radius := ARadius;
end;

procedure TRoomMap.Draw();
begin
  FRoomUI.DrawUI(BattleRoom.RoomTransform);
end;

procedure TRoomMap.AddMessage(const AStr: string);
var broom: TBattleRoom;
begin
  broom := GetBattleRoom;
  if broom = nil then Exit;
  broom.UI.AddMessage(AStr);
end;

procedure TRoomMap.AddFlyOutMessage(const AStr: string; const ATilePos: TVec2i;
  const AColor: TVec3; AHeight: Single; const ATime: Integer);
var
  msg: TbFlyOutMessage;
begin
  msg := TbFlyOutMessage.Create(BattleRoom.World);
  msg.SetState(UI.TilePosToWorldPos(ATilePos)+Vec(0,AHeight,0), AStr, Vec(AColor,1), ATime);
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
  Result := Round(angle - BattleRoom.RoomDir) mod 6;
  if Result < 0 then
    Result := Result + 6;
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

function TRoomMap.RayCast(const APt1, APt2: TVec2i; const AFilter: IRoomCellFilter): IRoomPath;
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
    if AFilter <> nil then
      if not AFilter.IsValid(APt1 + pt) then
        Exit;
    pt := NextPointOnRay(pt, dir, dirStep);
  end;
  Result.Add(APt2);
end;

function TRoomMap.RayCastDist(const APt1, APt2: TVec2i; ADist: Single; const AllowHitBlockers: Boolean): IRoomPath;
var dir: TVec2i;
    dirStep: TVec2i;
    pt: TVec2i;
begin
  Result := TRoomPath.Create();
  if APt1 = APt2 then
    Exit;

  dir := APt2 - APt1;
  dirStep := Sign(dir);
  pt := NextPointOnRay(Vec(0,0), dir, dirStep);
  while Distance(Vec(0,0), pt) < ADist do
  begin
    if not IsCellExists(APt1 + pt) then Exit;
    Result.Add(APt1 + pt);
    if AllowHitBlockers then
      if IsCellBlockView(APt1 + pt) then
        Exit;
    pt := NextPointOnRay(pt, dir, dirStep);
  end;
end;

function TRoomMap.RayCastBoolean(const APt1, APt2: TVec2i): Boolean;
begin
  Result := RayCastBoolean(APt1, APt2, nil);
end;

function TRoomMap.RayCastBoolean(const APt1, APt2: TVec2i; AFilter: IRoomCellFilter): Boolean;
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
    if AFilter <> nil then
    begin
      if not AFilter.IsValid(APt1 + pt) then
        Exit(False);
    end
    else
      if IsCellBlockView(APt1 + pt) then
        Exit(False);
    pt := NextPointOnRay(pt, dir, dirStep);
  end;
  Result := True;
end;

function TRoomMap.GetShootPointsSet(const ASourcePt: TVec2i; const AViewRange: Single; const AInFilter: IVec2iSet): IVec2iSet;
var i, j: Integer;
    pt: TVec2i;
    filter: IRoomCellFilter;
    nonvisited_list: IVec2iArr;
    nonvisited_set: IVec2iSet;
    comp_maxdist: IComparer;
    path: IRoomPath;
    inShadow: Boolean;

    unitWorldPos: TVec3;
begin
  Result := TVec2iSet.Create;
  filter := TRoomCellFilter_ViewableExcludeUnits.Create(Self);
  comp_maxdist := TRoomCellComparer_MaxDistance.Create(Self, ASourcePt);

  unitWorldPos := UI.TilePosToWorldPos(ASourcePt);

  //get all cells in view range
  nonvisited_set := TVec2iSet.Create();
  nonvisited_list := TVec2iArr.Create();
  for j := -Radius to Radius do
    for i := -Radius to Radius do
    begin
      pt := Vec(i, j);
      if not IsCellExists(pt) then Continue;
      if AInFilter <> nil then
        if not AInFilter.Contains(pt) then Continue;
      if filter.IsValid(pt) then
      begin
        nonvisited_list.Add(pt);
        nonvisited_set.Add(pt);
      end;
    end;

  //collect visible set with raycast
  nonvisited_list.Sort(comp_maxdist);
  for i := 0 to nonvisited_list.Count - 1 do
  begin
    if not nonvisited_set.Contains(nonvisited_list[i]) then Continue;

    path := RayCast(ASourcePt, nonvisited_list[i], False);
    inShadow := False;
    for j := 0 to path.Count - 1 do
    begin
      if not inShadow then
      begin
        if not filter.IsValid(path[j]) then
          inShadow := True
        else
          if LenSqr( UI.TilePosToWorldPos(path[j]) - unitWorldPos ) <= AViewRange*AViewRange then
          begin
            if (AInFilter = nil) or AInFilter.Contains(path[j]) then
              Result.Add(path[j]);
          end;
      end;
      nonvisited_set.Delete(path[j]);
    end;
  end;
end;

function TRoomMap.GetShootPoints(const ASourcePt: TVec2i; const AViewRange: Single; const AInFilter: IVec2iSet): IVec2iArr;
begin
  Result := GetShootPointsSet(ASourcePt, AViewRange, AInFilter).ToIArray();
end;

function TRoomMap.IsCellExists(const APos: TVec2i): Boolean;
begin
  Result := Distance(Vec(0,0), APos) <= (FRadius - 1);
  if not Result then
    if FRoomFloor <> nil then
      Result := FRoomFloor.DoorCellIndex(APos) >= 0;
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
    if (roomobj is TRoomUnit) then
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
  if AObject.Registred then
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

procedure TRoomMap.RegInventoryObject(const AObject: TRoomObject);
begin
  FInventoryObjects.Add(AObject);
  InvalidateInventoryBagsOnGround;
end;

procedure TRoomMap.UnRegInventoryObject(const AObject: TRoomObject);
begin
  FInventoryObjects.Delete(AObject);
  InvalidateInventoryBagsOnGround;
end;

function TRoomMap.InventoryObjectsAt(const APos: TVec2i; const ANonEmpty: Boolean): IRoomObjectArr;
var obj: TRoomObject;
begin
  Result := TRoomObjectArr.Create();
  FInventoryObjects.Reset;
  while FInventoryObjects.Next(obj) do
  begin
    if obj.RoomPos <> APos then Continue;
    if ANonEmpty then
    begin
      if obj.Inventory.Items.Count > 0 then
        Result.Add(obj);
    end
    else
      Result.Add(obj);
  end;
end;

function TRoomMap.AllInventoryObjects(const ANonEmpty: Boolean): IRoomObjectArr;
var obj: TRoomObject;
begin
  Result := TRoomObjectArr.Create();
  FInventoryObjects.Reset;
  while FInventoryObjects.Next(obj) do
  begin
    if ANonEmpty then
    begin
      if obj.Inventory.Items.Count > 0 then
        Result.Add(obj);
    end
    else
      Result.Add(obj);
  end;
end;

function TRoomMap.AllInventoryObjectsPos(const ANonEmpty: Boolean): IVec2iSet;
var obj: TRoomObject;
begin
  Result := TVec2iSet.Create();
  FInventoryObjects.Reset;
  while FInventoryObjects.Next(obj) do
  begin
    if ANonEmpty then
    begin
      if obj.Inventory.Items.Count > 0 then
        Result.Add(obj.RoomPos);
    end
    else
      Result.Add(obj.RoomPos);
  end;
end;

procedure TRoomMap.InvalidateInventoryBagsOnGround;
begin
  FRoomFloor.UpdateInventoryObjects;
end;

function TRoomMap.InAction: Boolean;
begin
  Result := BattleRoom.InAction();
end;

procedure TRoomMap.AddAction(AAction: IBRA_Action);
begin
  GetBattleRoom.AddAction(AAction);
end;

{ TPlayer }

function TPlayer.GetActiveSkill: IUnitSkill;
begin
  Result := nil;
  if FActiveSkill = nil then Exit;
  if not FActiveSkill.UseReady(Self) then Exit;
  if AllSkills().IndexOf(FActiveSkill) < 0 then Exit;
  Result := FActiveSkill;
end;

procedure TPlayer.SetActiveSkill(const AValue: IUnitSkill);
begin
  if AValue = nil then
  begin
    FActiveSkill := nil;
    Exit;
  end;
  if AllSkills().IndexOf(AValue) < 0 then Exit;
  if not AValue.UseReady(Self) then Exit;
  FActiveSkill := AValue;
end;

procedure TPlayer.OnDead();
begin
  inherited OnDead();
  TryPlaySound3D('sounds\MaleD_Death3.mp3', Self);
end;

procedure TPlayer.Notify_PlayerLeave;
begin

end;

procedure TPlayer.Notify_PlayerEnter;
begin

end;

procedure TPlayer.UpdateStep;
var
  i: Integer;
begin
  inherited UpdateStep;

  for i := 0 to Length(FAnim) - 1 do
    FAnim[i].SetTime(World.GameTime);
end;

procedure TPlayer.SetAnimation(const ANameSequence: array of string);
var
  i: Integer;
begin
  inherited SetAnimation(ANameSequence);
  for i := 0 to Length(FAnim) - 1 do
    FAnim[i].AnimationSequence_StartAndStopOther(AddAnimationPrefix(ANameSequence), FAnimateState<>asDeath);
end;

function TPlayer.GetUnitMoveSpeed: Single;
begin
  Result := 4;
end;

procedure TPlayer.LoadModels();
var
  i: Integer;
  axe: IUnitItem;
begin
  MaxAP := {$IfDef DEBUGBOTS}30{$Else}10{$EndIf};
  MaxHP := {$IfDef DEBUGBOTS}1000{$Else}100{$EndIf};
  HP := MaxHP;
  AP := MaxAP;

  ViewAngle := 0.5 * Pi + EPS;
  ViewRange := 20.5;
  ViewWholeRange := 2.5;

  AddModel('EXO_Body', mtDefault);
  AddModel('EXO_BrowsLashes', mtDefault);
  AddModel('EXO_Eyes', mtDefault);
  AddModel('EXO_EyesSpec', mtDefault);
  AddModel('EXO_HeadMask', mtDefault);
  AddModel('EXO_Suit', mtDefault);

  FAnimationPrefix := 'Hero_';

  FUnitSkills.Add(TSkill_Kick.Create(nil, 0));
  FUnitSkills.Add(TSkill_Shoot.Create(nil, 0));
  FUnitSkills.Add(TSkill_AxeAttack.Create(nil, 0));
  FUnitSkills.Add(TSkill_AbsoluteSight.Create(nil, 0));

  FSlots10[0] := FUnitSkills[0];
  FSlots10[1] := FUnitSkills[1];
  FSlots10[2] := FUnitSkills[2];

  //Inventory().Push(TArcherBow.Create, 0);

  axe := TAxe.Create;
  Inventory().Push(axe, 0);
  Equip(axe);

  {$IfDef DEBUGBOTS}
  Inventory().Push(THealBottle2.Create, 0);
  Inventory().Push(THuntersBow.Create, 0);
  Inventory().Push(TScroll_ResonantArmor.Create, 0);
  Inventory().Push(TScroll_Bow_Mastery.Create, 0);
  Inventory().Push(TScroll_Bow_Mastery.Create, 0);
  Inventory().Push(TScroll_Axe_Mastery.Create, 0);
  Inventory().Push(TScroll_Axe_Mastery.Create, 0);
  {$EndIf}

  Inventory().Push(THealBottle.Create, 0);
  //Inventory().Push(THealBottle2.Create, 0);
  //Inventory().Push(TPoisonBottle.Create, 0);
//  Inventory().Push(TSocks.Create, 0);

  SetLength(FAnim, FModels.Count);
  for i := 0 to FModels.Count - 1 do
    FAnim[i] := Create_IavAnimationController(FModels[i].Mesh.Pose, World.GameTime);
  SetAnimation([]);
  SubscribeForUpdateStep;

  Preview96_128 := 'ui\units\player.png';
end;

function TPlayer.Sound_Footstep(const AStepIndex: Integer): string;
begin
  if AStepIndex mod 2 = 0 then
    Result := 'sounds\StepStone1.mp3'
  else
    Result := 'sounds\StepStone2.mp3';
end;

function TPlayer.Material_Body(): TRoomUnitMaterial;
begin
  Result := matFlesh;
end;

procedure TPlayer.DealPureDamage(ADmg: Integer; AFromUnit: TRoomUnit; const AMsg: string);
begin
  inherited DealPureDamage(ADmg, AFromUnit, AMsg);
  if not IsDead() then
    TryPlaySound3D('sounds\MaleD_Wound2.mp3', self);
end;

{ TBattleRoom }

procedure TBattleRoom.SetRoomDir(AValue: Integer);
begin
  if FRoomDir = AValue then Exit;
  FRoomDir := AValue;
  UpdateRoomTransform;
end;

function TBattleRoom.WorldPos: TVec3;
var
  WorldPos2d: TVec2;
begin
  WorldPos2d := (FRoomPos * FAffinePack) * ((cRoomRadius*2+2)*0.86602540378443864676372317075294);
  Result := Vec(WorldPos2d.x, 0, WorldPos2d.y);
end;

function TBattleRoom.WorldRot: TQuat;
begin
  Result := Quat(Vec(0, 1, 0), 2 * Pi * (FRoomDir / 6));
end;

procedure TBattleRoom.UpdateRoomTransform;
begin
  FRoomTransform := Mat4(WorldRot(), WorldPos());
  FRoomTransformInv := Inv(FRoomTransform);
end;

procedure TBattleRoom.SetRoomPos(AValue: TVec2i);
begin
  if FRoomPos = AValue then Exit;
  FRoomPos := AValue;
  UpdateRoomTransform;
end;

function TBattleRoom.IsPlayerTurn: Boolean;
begin
  if FUnits.Count = 0 then Exit(False);
  Result := FUnits[FActiveUnit] = FPlayer;
end;

function TBattleRoom.IsClear: Boolean;
begin
  Result := FRoomClear;
end;

function TBattleRoom.IsBotTurn: Boolean;
begin
  Result := FUnits[FActiveUnit] is TBot;
end;

function TBattleRoom.IsMouseOnUI: Boolean;
begin
  if FUI = nil then Exit(False);
  Result := FUI.IsMouseOnUI;
end;

procedure TBattleRoom.AutoOpenDoors;
var
  i: Integer;
begin
  if FUnits = nil then Exit;
  for i := 0 to FUnits.Count - 1 do
    if FUnits[i] is TBot then
      if not (FUnits[i] as TRoomUnit).IsDead() then
        Exit;
  FRoomClear := True;
  FMap.RoomFloor.DoorsOpened := True;
end;

procedure TBattleRoom.AutoCloseDoors;
var
  i: Integer;
begin
  if (not FMap.RoomFloor.DoorsOpened) and (FActions.Count = 0) then
  begin
    for i := 0 to 5 do
      if FMap.RoomFloor.FDoors[i] <> nil then
      begin
        if FMap.ObjectAt(FMap.RoomFloor.FDoors[i].RoomPos) = nil then
          FMap.RoomFloor.FDoors[i].Opened := False;
      end;
  end;
end;

procedure TBattleRoom.UpdateUnitsAtUI;
var enemies: IRoomUnitArr;
    i: Integer;
begin
  enemies := TRoomUnitArr.Create();
  for i := 0 to FUnits.Count - 1 do
    if not (FUnits[i] is TPlayer) then
      enemies.Add(FUnits[i]);
  FUI.SetEnemiesList(enemies);
end;

procedure TBattleRoom.AfterRegister;
begin
  inherited AfterRegister;
  FTryLeaveIndex := -1;
  FAffinePack.Row[0] := Vec(0.86602540378443864676372317075294, 0.5);
  FAffinePack.Row[1] := Vec(0, 1);
  FAffinePackInv := Inv(FAffinePack);
  UpdateRoomTransform;

  FUnits := TRoomUnitArr.Create();
  FActions := TBRA_ActionArr.Create();
  FObstacles := LoadObstacles(ExeRelativeFileName('models\scene1_obstacles.txt'));
end;

function TBattleRoom.World: TbWorld;
begin
  Result := FindAtParents(TbWorld) as TbWorld;
end;

procedure TBattleRoom.SetEditMode();
begin
  FMap.InEditMode := True;
end;

procedure TBattleRoom.KeyPress(KeyCode: Integer);
var n: Integer;
begin
  if FPlayer <> nil then
    if FPlayer.IsDead() then Exit;

  if KeyCode = Ord('O') then
    FMap.RoomFloor.DoorsOpened := not FMap.RoomFloor.DoorsOpened;

  if not IsPlayerTurn() then Exit;
  if (KeyCode = Ord('E')) then
    EndTurn();
  case KeyCode of
    Ord('0')..Ord('9') :
    begin
      n := KeyCode - Ord('0');
      if (n >= 0) and (n <= 9) then
      begin
        if (n = 0) then
          n := 9
        else
          n := n - 1;
        FPlayer.ActiveSkill := FPlayer.SkillSlots[n];
      end;
    end;
    Ord('G') :
    begin
      if (FPlayer <> nil) and not InAction() then
        FActions.Add(TBRA_LootGround.Create(FPlayer));
    end;
  end;
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
  if FPlayer <> nil then
    if FPlayer.IsDead() then Exit;
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

    if (obj is TRoomUnit) and (FPlayer <> obj) and (FPlayer.ActiveSkill <> nil) then
    begin
      new_action := FPlayer.ActiveSkill.DoAction(FPlayer, obj as TRoomUnit);
    end;

    if (obj is TRoomInteractiveObject) then
    begin
      new_action := TRoomInteractiveObject(obj).Interactive_Try(FPlayer);
    end;

    if new_action <> nil then
      FActions.Add(new_action);
    Exit;
  end;
end;

procedure TBattleRoom.EndTurn();

  function AnybodyAlive(): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    if FUnits = nil then Exit;
    for i := 0 to FUnits.Count - 1 do
      if not (FUnits[i] as TRoomUnit).IsDead() then
        Exit(True);
  end;

var unt: TRoomUnit;
begin
  if (FActions.Count > 0) then Exit;
  if not AnybodyAlive() then Exit;

  FActiveUnit := (FActiveUnit + 1) mod FUnits.Count;
  unt := FUnits[FActiveUnit] as TRoomUnit;
  unt.AP := unt.MaxAP;

  UI.SetActiveUnit(unt);
  if unt.IsDead() then
    EndTurn()
  else
  begin
    unt.ProcessBuffs;
    //if (unt.AP <= 0) then
    //  EndTurn()
    //else
    begin
      if (unt is TBot) then TBot(unt).NewTurn();
      if (unt is TPlayer) then
        UI.AddMessage('Ваш ход');
    end;
  end;
end;

function TBattleRoom.InAction(): Boolean;
begin
  Result := (FActions.Count > 0) or not IsPlayerTurn;
end;

procedure TBattleRoom.AddAction(AAction: IBRA_Action);
begin
  FActions.Add(AAction);
end;

procedure TBattleRoom.UpdateStep();
var bot: TBot;
    new_action: IBRA_Action;
    i: LongInt;

    lstnrPos: TListenerPos;
begin
  lstnrPos.Front := normalize(Main.Camera.Eye - Main.Camera.At);
  lstnrPos.Pos := Main.Camera.Eye;
  lstnrPos.Top := Main.Camera.Up;
  lstnrPos.Vel := Vec(0,0,0);
  GetLightPlayer.Listener3DPos := lstnrPos;

  if FUnits.Count = 0 then Exit;
  if FPlayer = nil then Exit;
  if FTryLeaveIndex >= 0 then
    if Assigned(FOnLeaveBattleRoom) then
    begin
      FOnLeaveBattleRoom(Self, FTryLeaveIndex);
      Exit;
    end;

  AutoOpenDoors();
  AutoCloseDoors();

  if TRoomUnit(FUnits[FActiveUnit]).IsDead() then
    EndTurn();

  UI.SetPlayerActiveSkill(FPlayer.ActiveSkill);
  UI.UpdateStep(IsPlayerTurn);

  //moved tile update
  if IsPlayerTurn then
  begin
    if FRoomClear and (FPlayer <> nil) and (FPlayer.AP = 0) and (not FPlayer.IsDead()) then
    begin
      FPlayer.AP := FPlayer.MaxAP;
      EndTurn();
    end;

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
    begin
      EndTurn();
      bot.LogAction('EndTurn');
    end
    else
    begin
      FActions.Add(new_action);
      bot.LogAction('Action: '+new_action.Name);
    end;
  end;

  for i := FActions.Count - 1 downto 0 do
  begin
    if not FActions[i].ProcessAction() then
    begin
      FActions[i].Done := True;
      FActions.DeleteWithSwap(i);
    end;
  end;
end;

procedure TBattleRoom.PrepareToDraw();

  procedure DrawTileMap(out AReservedPts: Integer);
  var
    i: Integer;
    unt: TRoomUnit;
    movedObj: TRoomObject;
    observSet: IVec2iSet;
    pt: TVec2i;
    //shootPts: IVec2iArr;
  begin
    AReservedPts := 0;
    FMap.UI.ClearTileColors();
    if {$IfDef DEBUGBOTS}True{$Else}IsPlayerTurn{$EndIf} then
    begin
      if (FMovePath <> nil) and (FActions.Count = 0) then
      begin
        for i := 0 to FMovePath.Count - 1 do
        begin
          if i >= FPlayer.AP then
            FMap.UI.TileColor[FMovePath[i]] := TTileColorID.HighlightedRed
          else
            FMap.UI.TileColor[FMovePath[i]] := TTileColorID.HighlightedGreen;
        end;
        AReservedPts := FMovePath.Count;
      end;
      if (not IsMouseOnUI) and (FMovePath = nil) then
        FMap.UI.TileColor[FMovedTile] := TTileColorID.Hovered;

      //shootPts := FPlayer.GetShootPoints();
      //for i := 0 to shootPts.Count - 1 do
      //  FMap.UI.TileColor[shootPts[i]] := TTileColorID.HighlightedRed;

      //if gvDebugPoints <> nil then
      //  for i := 0 to gvDebugPoints.Count - 1 do
      //    FMap.UI.TileColor[gvDebugPoints[i]] := TTileColorID.HighlightedGreen;
      //if gvDebugPoints2 <> nil then
      //  for i := 0 to gvDebugPoints2.Count - 1 do
      //    FMap.UI.TileColor[gvDebugPoints2[i]] := TTileColorID.HighlightedRed;

      movedObj := FMap.ObjectAt(FMovedTile);
      if (movedObj is TRoomUnit) and (movedObj <> FMap.CurrentPlayer) then
      begin
        unt := movedObj as TRoomUnit;
        if {$IfDef DEBUGBOTS}True{$Else}FPlayer.CanSee(unt){$EndIf} then
        begin
          observSet := unt.GetObservablePointSet(nil, nil);
          observSet.Reset;
          while observSet.Next(pt) do
            FMap.UI.TileColor[pt] := TTileColorID.HighlightedYellow;

          for i := 0 to FUnits.Count - 1 do
          begin
            if FUnits[i] = unt then
              FMap.UI.TileColor[FUnits[i].RoomPos] := TTileColorID.Hovered
            else
              if unt.CanSee(FUnits[i]) then
                FMap.UI.TileColor[FUnits[i].RoomPos] := TTileColorID.HighlightedYellow;
          end;
        end;
        if FPlayer.ActiveSkill <> nil then
          if FPlayer.ActiveSkill.CanUse(FPlayer, unt, -FPlayer.ActiveSkill.Cost) then
            AReservedPts := FPlayer.ActiveSkill.Cost;
      end;

      if FRayPath <> nil then
        for i := 0 to FRayPath.Count - 1 do
        begin
          FMap.UI.TileColor[FRayPath[i]] := TTileColorID.Hovered;
        end;
    end;
  end;
var reservedPts: Integer;
begin
  DrawTileMap(reservedPts);
  if FUI <> nil then
    FUI.SetReservedAP(reservedPts);
end;

procedure TBattleRoom.Draw3DUI();
var
  oldDepthWrite: Boolean;
begin
  oldDepthWrite := Main.States.DepthWrite;
  Main.States.DepthWrite := False;
  Main.States.Blending[0] := True;
  Main.States.SetBlendFunctions(bfSrcAlpha, bfInvSrcAlpha);
  FMap.Draw();
  Main.States.DepthWrite := oldDepthWrite;
end;

procedure TBattleRoom.GenerateWithLoad(const AFileName: string;
  const ADoors: TDoors; const AForPlayer: TPlayer; const AVisitedRooms: IVisitedRooms);

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
      if FMap.IsCellExists(Result) and (not FMap.RoomFloor.IsHole[Result]) and not FMap.IsCellBlocked(Result) then
        Exit(Result);
    until False;
  end;

  {$IfDef DEBUGBOTS}
  procedure SpawnDebugBot();
  var bot: TBot;
  begin
    bot := TBotWisp.Create(FMap);
    bot.LoadModels();
    bot.SetRoomPosDir(GetSpawnPlace(), Random(6));
    FUnits.Add(bot);

    bot := TBotWisp.Create(FMap);
    bot.LoadModels();
    bot.SetRoomPosDir(GetSpawnPlace(), Random(6));
    FUnits.Add(bot);

    bot := TBotWisp.Create(FMap);
    bot.LoadModels();
    bot.SetRoomPosDir(GetSpawnPlace(), Random(6));
    FUnits.Add(bot);

    bot := TBotMutant1.Create(FMap);
    bot.LoadModels();
    bot.SetRoomPosDir(GetSpawnPlace(), Random(6));
    FUnits.Add(bot);

    bot := TBotArcher1.Create(FMap);
    bot.LoadModels();
    bot.SetRoomPosDir(GetSpawnPlace(), Random(6));
    FUnits.Add(bot);

    bot := TBotHunter1.Create(FMap);
    bot.LoadModels();
    bot.SetRoomPosDir(GetSpawnPlace(), Random(6));
    //bot.SetRoomPosDir(Vec(0,0), Random(6));
    FUnits.Add(bot);
  end;
  {$EndIf}

var
  oldPlayer: TPlayer;
begin
  FreeAndNil(FMap);
  FMap := TRoomMap.Create(Self);
  FMap.SetDoorsRadius(ADoors, cRoomRadius);

  oldPlayer := FPlayer;
  FPlayer := AForPlayer;
  LoadObstacles();
  FPlayer := oldPlayer;

  FRoomClear := False;
  {$IfDef DEBUGBOTS}
    SpawnDebugBot();
  {$Else}
    FUnits.AddArray( GenBots(FMap, AVisitedRooms, AForPlayer) );
  {$EndIf}
end;

procedure TBattleRoom.GenerateEmpty();
var doors: TDoors;
    i: Integer;
begin
  for i := 0 to Length(doors) - 1 do doors[i] := True;

  FreeAndNil(FMap);
  FMap := TRoomMap.Create(Self);
  FMap.SetDoorsRadius(doors, cRoomRadius);

  //FEmptyLight := World.Renderer.CreatePointLight();
  //FEmptyLight.Pos := Vec(0, 10, 0);
  //FEmptyLight.Radius := 50;
  //FEmptyLight.Color := Vec(1,1,1);
  //FEmptyLight.CastShadows := st1024;
end;

{$IfDef FPC}
  {$WARN 5044 off : Symbol "$1" is not portable}
{$EndIf}
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

  inst := World.Renderer.CreateModelInstances([AName]);
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

    World.Renderer.ModelsProgram_NoLight.Select();
    World.Renderer.ModelsCollection.Select();
    World.Renderer.ModelsCollection.Draw(inst);

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

procedure TBattleRoom.DetachPlayer();
var n: Integer;
begin
  FPlayer.UnregisterAtRoom();
  n := FUnits.IndexOf(FPlayer);
  if n >= 0 then
    FUnits.Delete(n);
  FActiveUnit := 0;
  FActions.Clear();
  FMovePath := nil;
  FRayPath := nil;
  FMap.CurrentPlayer := nil;
  FPlayer := nil;
  FTryLeaveIndex := -1;

  FMap.Notify_PlayerLeave;
end;

procedure TBattleRoom.AttachPlayer(const APlayer: TPlayer; const ADoorIdx: Integer);
var
  door: TRoomDoor;
begin
  if TRoomMap(APlayer.Parent).BattleRoom <> Self then
  begin
    TRoomMap(APlayer.Parent).BattleRoom.DetachPlayer();
  end;

  if FPlayer = nil then
  begin
    FPlayer := APlayer;
    FMap.Notify_PlayerEnter;
    FPlayer.Parent := Map;

    door := APlayer.Room.RoomFloor.FDoors[ADoorIdx];
    Assert(door <> nil);

    FPlayer.RoomPos := TTileUtils.RotateTileCoord(Vec(-1,2), door.RoomDir) + door.RoomPos;
    FUnits.Insert(0, FPlayer);
    FActiveUnit := FUnits.Count - 1;
    FMap.CurrentPlayer := FPlayer;
    UpdateUnitsAtUI();
    EndTurn();
    AddAction(TBRA_ComingIn.Create(FPlayer, ADoorIdx));
    FTryLeaveIndex := -1;
  end;
end;

procedure TBattleRoom.AttachPlayer(const APlayer: TPlayer; const ARoomPos: TVec2i; ARoomDir: Integer);

  function GetSpawnPlace(): TVec2i;
  begin
    repeat
      Result.x := Random(FMap.Radius*2+1) - FMap.Radius;
      Result.y := Random(FMap.Radius*2+1) - FMap.Radius;
      if FMap.IsCellExists(Result) and (not FMap.RoomFloor.IsHole[Result]) and not FMap.IsCellBlocked(Result) then
        Exit(Result);
    until False;
  end;

begin
  if TRoomMap(APlayer.Parent).BattleRoom <> Self then
  begin
    TRoomMap(APlayer.Parent).BattleRoom.DetachPlayer();
  end;

  if FPlayer = nil then
  begin
    FPlayer := APlayer;
    FMap.Notify_PlayerEnter;
    FPlayer.Parent := Map;
    FPlayer.SetRoomPosDir(GetSpawnPlace, FPlayer.RoomDir);
    FUnits.Insert(0, FPlayer);
    FActiveUnit := FUnits.Count - 1;
    FMap.CurrentPlayer := FPlayer;
    UpdateUnitsAtUI();
    EndTurn();
  end;
end;

procedure TBattleRoom.TryLeaveRoom(const AUnit: TRoomUnit; const ADoorIdx: Integer);
begin
  FTryLeaveIndex := ADoorIdx;
end;

function TBattleRoom.Units: IRoomUnitArr;
begin
  Result := FUnits;
end;

{$IfDef FPC}
  {$WARN 5044 on : Symbol "$1" is not portable}
{$EndIf}

procedure TBattleRoom.SaveRoomMap(const AStream: TStream);
var obs: TObstacle;
    obsCount: Integer;
    i, n: Integer;
    holes: IVec2iSet;
    hole: TVec2i;
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

  holes := Map.RoomFloor.AllHoles;
  n := holes.Count;
  AStream.WriteBuffer(n, SizeOf(n));
  holes.Reset;
  while holes.Next(hole) do
    AStream.WriteBuffer(hole, SizeOf(hole));
end;

procedure TBattleRoom.LoadRoomMap(const AStream: TStream);
var obs: TObstacle;
    obsCount: Integer;
    i, n: Integer;
    astr_clsName : AnsiString;
    clsName : string;
    roomCls : TRoomObjectClass;
    hole: TVec2i;
begin
  obsCount := 0;
  AStream.ReadBuffer(obsCount, SizeOf(obsCount));

  for i := 0 to obsCount - 1 do
  begin
    StreamReadString(AStream, astr_clsName);
    clsName := string(astr_clsName);
    roomCls := FindRoomClass(clsName);
    Assert(roomCls <> nil);
    Assert(roomCls.InheritsFrom(TObstacle));
    obs := CreateRoomObject(roomCls) as TObstacle;
    obs.ReadStream(AStream);
  end;

  hole := Vec(0, 0);
  n := 0;
  AStream.ReadBuffer(n, SizeOf(n));
  for i := 0 to n - 1 do
  begin
    AStream.ReadBuffer(hole, SizeOf(hole));
    FMap.RoomFloor.IsHole[hole] := True;
  end;
end;

function TBattleRoom.CreateRoomObject(const AClass: TRoomObjectClass): TRoomObject;
begin
  Result := AClass.Create(FMap);
end;

initialization
  RegRoomClass(TObstacle);

end.

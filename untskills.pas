unit untSkills;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, untLevel, intfUtils, mutils;

type

  { TUnitSkill }

  TUnitSkill = class(TInterfacedObject, IUnitSkill)
  private
    FItem : IWeakRefIntf;
    FIndex: Integer;
    FLevel: Integer;
  protected
    function  GetSkillLevel: Integer;
    procedure SetSkillLevel(ALevel: Integer);

    function Item: IUnitItem;
    function Idx : Integer;
    function WearedOnly: Boolean; virtual; abstract;
    function UseReady(AUnit: TRoomUnit): Boolean; virtual; abstract;

    function ID   : TUnitSkillID; virtual;
    function Name : string; virtual; abstract;
    function Desc : string; virtual; abstract;
    function Ico  : string; virtual; abstract;
    function Sound: string; virtual; abstract;

    function Cost : Integer; virtual; abstract;
    function Range: Single; virtual; abstract;
    function Damage: TVec2i; virtual; abstract;
    function DamageScale: Single; virtual; abstract;
    function Accuracy: TVec2; virtual; abstract;

    function Req_WeaponType: TUnitItemKind; virtual;

    function Animation: string; virtual; abstract;
    function IsAttackSkill: Boolean; virtual; abstract;
    function IsBuffSkill: Boolean; virtual; abstract;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; virtual; abstract;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; virtual; abstract;
    function SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff; virtual; abstract;

    function DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action; virtual; abstract;
    function CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; virtual;
  public
    constructor Create(const AItem: IUnitItem; const AIndex: Integer);
  end;

  { TSkill_Kick }

  TSkill_Kick = class(TUnitSkill)
  protected
    function WearedOnly: Boolean; override;
    function UseReady(AUnit: TRoomUnit): Boolean; override;

    function Name : string; override;
    function Desc : string; override;
    function Ico  : string; override;
    function Sound: string; override;

    function Cost        : Integer; override;
    function Range       : Single; override;
    function Damage      : TVec2i; override;
    function DamageScale : Single; override;
    function Accuracy    : TVec2; override;

    function Animation: string; override;
    function IsAttackSkill: Boolean; override;
    function IsBuffSkill: Boolean; override;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; override;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; override;
    function SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff; override;

    function DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TSkill_MutantPunch }

  TSkill_MutantPunch = class(TUnitSkill)
  protected
    function WearedOnly: Boolean; override;
    function UseReady(AUnit: TRoomUnit): Boolean; override;

    function Name : string; override;
    function Desc : string; override;
    function Ico  : string; override;
    function Sound: string; override;

    function Cost        : Integer; override;
    function Range       : Single; override;
    function Damage      : TVec2i; override;
    function DamageScale : Single; override;
    function Accuracy    : TVec2; override;

    function Animation: string; override;
    function IsAttackSkill: Boolean; override;
    function IsBuffSkill: Boolean; override;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; override;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; override;
    function SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff; override;

    function DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TSkill_Shoot }

  TSkill_Shoot = class(TUnitSkill)
  private
    function RangeT(AOwner, ATarget: TRoomUnit): Single;
  protected
    function WearedOnly: Boolean; override;
    function UseReady(AUnit: TRoomUnit): Boolean; override;

    function ID   : TUnitSkillID; override;
    function Name : string; override;
    function Desc : string; override;
    function Ico  : string; override;
    function Sound: string; override;

    function Cost        : Integer; override;
    function Range       : Single; override;
    function Damage      : TVec2i; override;
    function DamageScale : Single; override;
    function Accuracy    : TVec2; override;

    function Req_WeaponType: TUnitItemKind; override;

    function Animation: string; override;
    function IsAttackSkill: Boolean; override;
    function IsBuffSkill: Boolean; override;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; override;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; override;
    function SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff; override;

    function DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TSkill_AxeAttack }

  TSkill_AxeAttack = class(TUnitSkill)
  private
  public
    function WearedOnly: Boolean; override;
    function UseReady(AUnit: TRoomUnit): Boolean; override;

    function ID   : TUnitSkillID; override;
    function Name : string; override;
    function Desc : string; override;
    function Ico  : string; override;
    function Sound: string; override;

    function Cost        : Integer; override;
    function Range       : Single; override;
    function Damage      : TVec2i; override;
    function DamageScale : Single; override;
    function Accuracy    : TVec2; override;

    function Req_WeaponType: TUnitItemKind; override;

    function Animation: string; override;
    function IsAttackSkill: Boolean; override;
    function IsBuffSkill: Boolean; override;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; override;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; override;
    function SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff; override;

    function DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TSkill_AbsoluteSight }

  TSkill_AbsoluteSight = class(TUnitSkill)
  private
  public
    function WearedOnly: Boolean; override;
    function UseReady(AUnit: TRoomUnit): Boolean; override;

    function Name : string; override;
    function Desc : string; override;
    function Ico  : string; override;
    function Sound: string; override;

    function Cost        : Integer; override;
    function Range       : Single; override;
    function Damage      : TVec2i; override;
    function DamageScale : Single; override;
    function Accuracy    : TVec2; override;

    function Req_WeaponType: TUnitItemKind; override;

    function Animation: string; override;
    function IsAttackSkill: Boolean; override;
    function IsBuffSkill: Boolean; override;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; override;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; override;
    function SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff; override;

    function DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TSkill_Resurrect }

  TSkill_Resurrect = class(TUnitSkill)
  private
  public
    function WearedOnly: Boolean; override;
    function UseReady(AUnit: TRoomUnit): Boolean; override;

    function Name : string; override;
    function Desc : string; override;
    function Ico  : string; override;
    function Sound: string; override;

    function Cost        : Integer; override;
    function Range       : Single; override;
    function Damage      : TVec2i; override;
    function DamageScale : Single; override;
    function Accuracy    : TVec2; override;

    function Req_WeaponType: TUnitItemKind; override;

    function Animation: string; override;
    function IsAttackSkill: Boolean; override;
    function IsBuffSkill: Boolean; override;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; override;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; override;
    function SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff; override;

    function DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

implementation

uses
  Math, untBuffs;

{ TSkill_MutantPunch }

function TSkill_MutantPunch.WearedOnly: Boolean;
begin
  Result := False;
end;

function TSkill_MutantPunch.UseReady(AUnit: TRoomUnit): Boolean;
begin
  Result := True;
end;

function TSkill_MutantPunch.Name: string;
begin
  Result := 'Пинок рукой';
end;

function TSkill_MutantPunch.Desc: string;
begin
  Result := 'Всегда ведь приятно ударить врага';
end;

function TSkill_MutantPunch.Ico: string;
begin
  Result := 'kick.png';
end;

function TSkill_MutantPunch.Sound: string;
begin
  Result := 'sounds\Punch.mp3';
end;

function TSkill_MutantPunch.Cost: Integer;
begin
  Result := 3;
end;

function TSkill_MutantPunch.Range: Single;
begin
  Result := 1;
end;

function TSkill_MutantPunch.Damage: TVec2i;
begin
  Result := Vec(20, 20);
end;

function TSkill_MutantPunch.DamageScale: Single;
begin
  Result := 1;
end;

function TSkill_MutantPunch.Accuracy: TVec2;
begin
  Result := Vec(1, 1);
end;

function TSkill_MutantPunch.Animation: string;
begin
  Result := 'Kick0';
end;

function TSkill_MutantPunch.IsAttackSkill: Boolean;
begin
  Result := True;
end;

function TSkill_MutantPunch.IsBuffSkill: Boolean;
begin
  Result := False;
end;

function TSkill_MutantPunch.SampleDamage(AOwner, ATarget: TRoomUnit): Integer;
begin
  Result := RandomRange(Damage.x, Damage.y+1);
end;

function TSkill_MutantPunch.SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean;
begin
  Result := True;
end;

function TSkill_MutantPunch.SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff;
begin
  Result := nil;
end;

function TSkill_MutantPunch.DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
  if not CanUse(AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;
  AOwner.Room.AddMessage(AOwner.Name + ' наносит удар');
  Result := TBRA_UnitDefaultAttack.Create(AOwner, ATarget, Self, 1890, 1200, 1020);
end;

function TSkill_MutantPunch.CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := inherited CanUse(AOwner, ATarget, AReservedPoints);
  if not Result then Exit;

  Result := False;
  if AOwner.AP - AReservedPoints < Cost then Exit;
  if AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos) > 1 then Exit;
  Result := True;
end;

{ TSkill_Resurrect }

function TSkill_Resurrect.WearedOnly: Boolean;
begin
  Result := False;
end;

function TSkill_Resurrect.UseReady(AUnit: TRoomUnit): Boolean;
begin
  Result := True;
end;

function TSkill_Resurrect.Name: string;
begin
  Result := 'Воскрешение';
end;

function TSkill_Resurrect.Desc: string;
begin
  Result := 'Возвращает павших на поле боя';
end;

function TSkill_Resurrect.Ico: string;
begin
  Result := 'noicon.png';
end;

function TSkill_Resurrect.Sound: string;
begin
  Result := 'sounds\Spell_02.mp3';
end;

function TSkill_Resurrect.Cost: Integer;
begin
  Result := 8;
end;

function TSkill_Resurrect.Range: Single;
begin
  Result := 16;
end;

function TSkill_Resurrect.Damage: TVec2i;
begin
  Result := Vec(0, 0);
end;

function TSkill_Resurrect.DamageScale: Single;
begin
  Result := 0;
end;

function TSkill_Resurrect.Accuracy: TVec2;
begin
  Result := Vec(1, 1);
end;

function TSkill_Resurrect.Req_WeaponType: TUnitItemKind;
begin
  Result := ikUnknown;
end;

function TSkill_Resurrect.Animation: string;
begin
  Result := 'SelfCast';
end;

function TSkill_Resurrect.IsAttackSkill: Boolean;
begin
  Result := False;
end;

function TSkill_Resurrect.IsBuffSkill: Boolean;
begin
  Result := False;
end;

function TSkill_Resurrect.SampleDamage(AOwner, ATarget: TRoomUnit): Integer;
begin
  Result := 0;
end;

function TSkill_Resurrect.SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean;
begin
  Result := True;
end;

function TSkill_Resurrect.SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff;
begin
  Result := nil;
end;

function TSkill_Resurrect.DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
  if not CanUse(AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;
  AOwner.Room.AddMessage(AOwner.Name + ' использует ' + Name + ' на союзника');
  Result := TBRA_UnitDefaultAttack.Create(AOwner, ATarget, Self, 1290, 660, 0);
  ATarget.HP := ATarget.MaxHP;
  ATarget.SetAnimation(['Raise0']);
end;

function TSkill_Resurrect.CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := inherited CanUse(AOwner, ATarget, AReservedPoints);
  if not Result then Exit;
  if not ATarget.IsDead() then Exit(False);
  if AOwner.AP - AReservedPoints < Cost then Exit(False);
  if ATarget.Room.ObjectAt(ATarget.RoomPos) <> nil then Exit(False);
  if ATarget.Room.RoomFloor.IsHole[ATarget.RoomPos] then Exit(False);
  Result := True;
end;

{ TSkill_AbsoluteSight }

function TSkill_AbsoluteSight.WearedOnly: Boolean;
begin
  Result := False;
end;

function TSkill_AbsoluteSight.UseReady(AUnit: TRoomUnit): Boolean;
begin
  Result := True;
end;

function TSkill_AbsoluteSight.Name: string;
begin
  Result := 'Абсолютное наблюдение';
end;

function TSkill_AbsoluteSight.Desc: string;
begin
  Result := 'Позволяет видеть врага где угодно';
end;

function TSkill_AbsoluteSight.Ico: string;
begin
  Result := 'abs_sight.png';
end;

function TSkill_AbsoluteSight.Sound: string;
begin
  Result := 'sounds\Spell_00.mp3';
end;

function TSkill_AbsoluteSight.Cost: Integer;
begin
  Result := 5;
end;

function TSkill_AbsoluteSight.Range: Single;
begin
  Result := 16;
end;

function TSkill_AbsoluteSight.Damage: TVec2i;
begin
  Result := Vec(0,0);
end;

function TSkill_AbsoluteSight.DamageScale: Single;
begin
  Result := 0;
end;

function TSkill_AbsoluteSight.Accuracy: TVec2;
begin
  Result := Vec(1,1);
end;

function TSkill_AbsoluteSight.Req_WeaponType: TUnitItemKind;
begin
  Result := inherited Req_WeaponType;
end;

function TSkill_AbsoluteSight.Animation: string;
begin
  Result := 'DirectCast';
end;

function TSkill_AbsoluteSight.IsAttackSkill: Boolean;
begin
  Result := False;
end;

function TSkill_AbsoluteSight.IsBuffSkill: Boolean;
begin
  Result := True;
end;

function TSkill_AbsoluteSight.SampleDamage(AOwner, ATarget: TRoomUnit): Integer;
begin
  Result := 0;
end;

function TSkill_AbsoluteSight.SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean;
begin
  Result := True;
end;

function TSkill_AbsoluteSight.SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff;
begin
  Result := TBuff_AbsoluteSight.Create(AOwner, 4);
end;

function TSkill_AbsoluteSight.DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
  if AOwner.AP < Cost then
    AOwner.Room.AddFlyOutMessage('Нужно ' + IntToStr(Cost) + ' ОД', AOwner.RoomPos, Vec(1,0,0));

  if not CanUse(AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;
  AOwner.Room.AddMessage(AOwner.Name + ' использует ' + Name);
  Result := TBRA_UnitDefaultAttack.Create(AOwner, ATarget, Self, 1290, 660, 0);
end;

function TSkill_AbsoluteSight.CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  if AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos) > Range then Exit(False);
  Result := inherited CanUse(AOwner, ATarget, AReservedPoints);
  if not Result then Exit;
  Result := False;
  if AOwner.AP - AReservedPoints < Cost then Exit;
  Result := True;
end;

{ TSkill_AxeAttack }

function TSkill_AxeAttack.WearedOnly: Boolean;
begin
  Result := True;
end;

function TSkill_AxeAttack.UseReady(AUnit: TRoomUnit): Boolean;
var itm: IUnitItem;
begin
  if AUnit = nil then Exit(False);
  itm := AUnit.GetEquip(esRightHand);
  if itm = nil then Exit(False);
  if itm.Kind <> ikAxe then Exit(False);
  Result := True;
end;

function TSkill_AxeAttack.ID: TUnitSkillID;
begin
  Result := sidAxeMastery;
end;

function TSkill_AxeAttack.Name: string;
begin
  Result := 'Владение топором ' + IntToStr(FLevel);
end;

function TSkill_AxeAttack.Desc: string;
begin
  Result := 'Простой рубящий удар';
end;

function TSkill_AxeAttack.Ico: string;
begin
  Result := 'axe_attack.png';
end;

function TSkill_AxeAttack.Sound: string;
begin
  Result := 'sounds\Axe.mp3';
end;

function TSkill_AxeAttack.Cost: Integer;
begin
  case FLevel of
    1 : Result := 3;
    2 : Result := 2;
  else
    Result := 2;
  end;
end;

function TSkill_AxeAttack.Range: Single;
begin
  Result := 1;
end;

function TSkill_AxeAttack.Damage: TVec2i;
begin
  case FLevel of
    1: Result := Vec(0, 0);
    2: Result := Vec(5, 10);
    3: Result := Vec(10, 20);
  else
    Result := Vec(10, 20);
  end;
end;

function TSkill_AxeAttack.DamageScale: Single;
begin
  case FLevel of
    1 : Result := 1;
    2 : Result := 1.2;
    3 : Result := 1.5;
  else
    Result := 1.5;
  end;
end;

function TSkill_AxeAttack.Accuracy: TVec2;
begin
  case FLevel of
    1 : Result := Vec(0.9, 0.9);
    2 : Result := Vec(0.95, 0.95);
    3 : Result := Vec(0.99, 0.99);
  end;
end;

function TSkill_AxeAttack.Req_WeaponType: TUnitItemKind;
begin
  Result := ikAxe;
end;

function TSkill_AxeAttack.Animation: string;
begin
  Result := 'Axe0_Attack0';
end;

function TSkill_AxeAttack.IsAttackSkill: Boolean;
begin
  Result := True;
end;

function TSkill_AxeAttack.IsBuffSkill: Boolean;
begin
  Result := False;
end;

function TSkill_AxeAttack.SampleDamage(AOwner, ATarget: TRoomUnit): Integer;
var itm: IUnitItem;
    dmg: TVec2;
begin
  itm := AOwner.GetEquip(esRightHand);
  if itm = nil then Exit(0);
  dmg := itm.Weapon_Damage + Damage;
  dmg.x := dmg.x * DamageScale;
  dmg.y := dmg.y * DamageScale;
  Result := Round( Lerp(dmg.x, dmg.y, Random) );
end;

function TSkill_AxeAttack.SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean;
begin
  Result := Random <= Accuracy.x;
end;

function TSkill_AxeAttack.SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff;
begin
  Result := nil;
end;

function TSkill_AxeAttack.DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
  if AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos) > 1 then
  begin
    AOwner.Room.AddFlyOutMessage('Слишком далеко', AOwner.RoomPos, Vec(1,0,0));
    Exit;
  end;
  if AOwner.AP < Cost then
    AOwner.Room.AddFlyOutMessage('Нужно ' + IntToStr(Cost) + ' ОД', AOwner.RoomPos, Vec(1,0,0));

  if not CanUse(AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;
  AOwner.Room.AddMessage(AOwner.Name + ' наносит рубящий удар');
  Result := TBRA_UnitDefaultAttack.Create(AOwner, ATarget, Self, 2000, 866, 0);
end;

function TSkill_AxeAttack.CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  if AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos) > 1 then Exit(False);
  Result := inherited CanUse(AOwner, ATarget, AReservedPoints);
  if not Result then Exit;
  Result := False;
  if AOwner.AP - AReservedPoints < Cost then Exit;
  Result := True;
end;

{ TSkill_Shoot }

function TSkill_Shoot.RangeT(AOwner, ATarget: TRoomUnit): Single;
var //fromPt, toPt: TVec3;
    dist: Single;
begin
  //fromPt := AOwner.Room.UI.TilePosToWorldPos(AOwner.RoomPos);
  //toPt := AOwner.Room.UI.TilePosToWorldPos(ATarget.RoomPos);
  //dist := Len(fromPt - toPt);
  dist := AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos);
  Result := clamp( max(0, (dist - 2)) / (Range - 2), 0.0, 1.0 );
end;

function TSkill_Shoot.WearedOnly: Boolean;
begin
  Result := True;
end;

function TSkill_Shoot.UseReady(AUnit: TRoomUnit): Boolean;
var itm: IUnitItem;
begin
  if AUnit = nil then Exit(False);
  itm := AUnit.GetEquip(esBothHands);
  if itm = nil then Exit(False);
  if itm.Kind <> ikBow then Exit(False);
  Result := True;
end;

function TSkill_Shoot.Name: string;
begin
  Result := 'Владение луком ' + IntToStr(FLevel);
end;

function TSkill_Shoot.Desc: string;
begin
  Result := 'Простой выстрел. Приченяет урон, если попадешь';
end;

function TSkill_Shoot.Ico: string;
begin
  Result := 'bow_shoot.png';
end;

function TSkill_Shoot.Sound: string;
begin
  Result := 'sounds\BowShot1.mp3';
end;

function TSkill_Shoot.ID: TUnitSkillID;
begin
  Result := sidBowMastery;
end;

function TSkill_Shoot.Cost: Integer;
begin
  if FLevel >= 3 then Result := 2 else Result := 3;
end;

function TSkill_Shoot.Range: Single;
begin
  case FLevel of
    1 : Result := 20;
    2 : Result := 25;
    3 : Result := 30;
  else
    Result := 30;
  end;
end;

function TSkill_Shoot.Damage: TVec2i;
begin
  case FLevel of
    1 : Result := Vec(0, 0);
    2 : Result := Vec(5, 0);
    3 : Result := Vec(10, 5);
  else
    Result := Vec(10, 5);
  end;
end;

function TSkill_Shoot.DamageScale: Single;
begin
  case FLevel of
    1 : Result := 1;
    2 : Result := 1.1;
    3 : Result := 1.5;
  end;
end;

function TSkill_Shoot.Accuracy: TVec2;
begin
  case FLevel of
    1 : Result := Vec(0.9, 0.3);
    2 : Result := Vec(0.9, 0.5);
    3 : Result := Vec(0.95, 0.7);
  end;
end;

function TSkill_Shoot.Req_WeaponType: TUnitItemKind;
begin
  Result := ikBow;
end;

function TSkill_Shoot.Animation: string;
begin
  Result := 'Archer_Bow_Attack0';
end;

function TSkill_Shoot.IsAttackSkill: Boolean;
begin
  Result := True;
end;

function TSkill_Shoot.IsBuffSkill: Boolean;
begin
  Result := False;
end;

function TSkill_Shoot.SampleDamage(AOwner, ATarget: TRoomUnit): Integer;
var t: Single;
    itm: IUnitItem;
    dmg: TVec2i;
begin
  itm := AOwner.GetEquip(esBothHands);
  if itm = nil then Exit(0);
  t := RangeT(AOwner, ATarget);
  dmg := itm.Weapon_Damage + Damage;
  dmg.x := Round(dmg.x * DamageScale);
  dmg.y := Round(dmg.y * DamageScale);
  Result := Round( Lerp(dmg.y, dmg.x, sqrt(t)) );
end;

function TSkill_Shoot.SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean;
begin
  Result := Random < Lerp(Accuracy.x, Accuracy.y, RangeT(AOwner, ATarget));
end;

function TSkill_Shoot.SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff;
begin
  Result := nil;
end;

function TSkill_Shoot.DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action;
var
  bullet: TRoomBullet;
begin
  Result := nil;
  if AOwner.AP < Cost then
    AOwner.Room.AddFlyOutMessage('Нужно ' + IntToStr(Cost) + ' ОД', AOwner.RoomPos, Vec(1,0,0));

  if not CanUse(AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;

  AOwner.Room.AddMessage(AOwner.Name + ' стреляет');

  bullet := TRoomBullet.Create(AOwner.Room);
  bullet.LoadModels('Erika_Archer_Arrow_Mesh');
  bullet.Owner := AOwner;
  bullet.Velocity := 20;
  bullet.Dmg := 10;
  bullet.MaxRange := Range;
  bullet.Target := ATarget.RoomPos;
  bullet.StartPt := AOwner.RoomPos;
  Result := TBRA_Shoot.Create(AOwner, [bullet], Self, 1150, 1.37);
end;

function TSkill_Shoot.CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := inherited CanUse(AOwner, ATarget, AReservedPoints);
  if not Result then Exit;

  Result := False;
  if AOwner.AP - AReservedPoints < Cost then Exit;
  Result := True;
end;

{ TUnitSkill }

function TUnitSkill.GetSkillLevel: Integer;
begin
  Result := FLevel;
end;

procedure TUnitSkill.SetSkillLevel(ALevel: Integer);
begin
  FLevel := ALevel;
end;

function TUnitSkill.Item: IUnitItem;
begin
  if FItem = nil then Exit(nil);
  Result := FItem.Intf as IUnitItem;
  if Result = nil then FItem := nil;
end;

function TUnitSkill.Idx: Integer;
begin
  Result := FIndex;
end;

function TUnitSkill.ID: TUnitSkillID;
begin
  Result := sidUnknown;
end;

function TUnitSkill.Req_WeaponType: TUnitItemKind;
begin
  Result := ikUnknown;
end;

function TUnitSkill.CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := UseReady(AOwner);
end;

constructor TUnitSkill.Create(const AItem: IUnitItem; const AIndex: Integer);
begin
  if AItem <> nil then
    FItem := (AItem as IWeakedInterface).WeakRef;
  FIndex := AIndex;
  FLevel := 1;
end;

{ TSkill_Kick }

function TSkill_Kick.WearedOnly: Boolean;
begin
  Result := False;
end;

function TSkill_Kick.UseReady(AUnit: TRoomUnit): Boolean;
begin
  Result := True;
end;

function TSkill_Kick.Name: string;
begin
  Result := 'Пинок ногой';
end;

function TSkill_Kick.Desc: string;
begin
  Result := 'Отбрасывает врага. Если отбросить в стену - может оглушить (шанс 50%)';
end;

function TSkill_Kick.Ico: string;
begin
  Result := 'kick.png';
end;

function TSkill_Kick.Sound: string;
begin
  Result := 'sounds\Kick.mp3';
end;

function TSkill_Kick.Cost: Integer;
begin
  Result := 2;
end;

function TSkill_Kick.Range: Single;
begin
  Result := 1;
end;

function TSkill_Kick.Damage: TVec2i;
begin
  Result := Vec(15, 15);
end;

function TSkill_Kick.DamageScale: Single;
begin
  Result := 1;
end;

function TSkill_Kick.Accuracy: TVec2;
begin
  Result := Vec(1, 1);
end;

function TSkill_Kick.Animation: string;
begin
  Result := 'Kick0';
end;

function TSkill_Kick.IsAttackSkill: Boolean;
begin
  Result := True;
end;

function TSkill_Kick.IsBuffSkill: Boolean;
begin
  Result := False;
end;

function TSkill_Kick.SampleDamage(AOwner, ATarget: TRoomUnit): Integer;
begin
  Result := RandomRange(Damage.x, Damage.y+1);
end;

function TSkill_Kick.SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean;
begin
  Result := True;
end;

function TSkill_Kick.SampleBuffChance(AOwner, ATarget: TRoomUnit): IUnitBuff;
begin
  if Random <= 0.5 then
    Result := TBuff_Stun.Create(ATarget, 2)
  else
    Result := nil;
end;

function TSkill_Kick.DoAction(AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
  if AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos) > 1 then
  begin
    AOwner.Room.AddFlyOutMessage('Слишком далеко', AOwner.RoomPos, Vec(1,0,0));
    Exit;
  end;
  if AOwner.AP < Cost then
    AOwner.Room.AddFlyOutMessage('Нужно ' + IntToStr(Cost) + ' ОД', AOwner.RoomPos, Vec(1,0,0));

  if not CanUse(AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;
  AOwner.Room.AddMessage(AOwner.Name + ' пинает врага');
  Result := TBRA_UnitKickAttack.Create(AOwner, ATarget, Self, 1200, 540, 420);
end;

function TSkill_Kick.CanUse(AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := inherited CanUse(AOwner, ATarget, AReservedPoints);
  if not Result then Exit;

  Result := False;
  if AOwner.AP - AReservedPoints < Cost then Exit;
  if AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos) > 1 then Exit;
  Result := True;
end;

end.


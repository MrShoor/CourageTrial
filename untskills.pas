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
  protected
    function Item: IUnitItem;
    function Idx : Integer;
    function WearedOnly: Boolean; virtual; abstract;
    function UseReady(AUnit: TRoomUnit): Boolean; virtual; abstract;

    function Name: string; virtual; abstract;
    function Desc: string; virtual; abstract;
    function Ico : string; virtual; abstract;

    function Cost : Integer; virtual; abstract;
    function Range: Single; virtual; abstract;
    function Damage: TVec2i; virtual; abstract;
    function DamageScale: Single; virtual; abstract;
    function Accuracy: TVec2; virtual; abstract;

    function Req_WeaponType: TUnitItemKind; virtual;

    function Animation: string; virtual; abstract;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; virtual; abstract;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; virtual; abstract;

    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; virtual; abstract;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; virtual;
  public
    constructor Create(const AItem: IUnitItem; const AIndex: Integer);
  end;

  { TSkill_Kick }

  TSkill_Kick = class(TUnitSkill)
  protected
    function WearedOnly: Boolean; override;
    function UseReady(AUnit: TRoomUnit): Boolean; override;

    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    function Cost        : Integer; override;
    function Range       : Single; override;
    function Damage      : TVec2i; override;
    function DamageScale : Single; override;
    function Accuracy    : TVec2; override;

    function Animation: string; override;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; override;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; override;

    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TSkill_Shoot }

  TSkill_Shoot = class(TUnitSkill)
  private
    function RangeT(AOwner, ATarget: TRoomUnit): Single;
  protected
    function WearedOnly: Boolean; override;
    function UseReady(AUnit: TRoomUnit): Boolean; override;

    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    function Cost        : Integer; override;
    function Range       : Single; override;
    function Damage      : TVec2i; override;
    function DamageScale : Single; override;
    function Accuracy    : TVec2; override;

    function Req_WeaponType: TUnitItemKind; override;

    function Animation: string; override;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; override;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; override;

    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TSkill_AxeAttack }

  TSkill_AxeAttack = class(TUnitSkill)
  private
  public
    function WearedOnly: Boolean; override;
    function UseReady(AUnit: TRoomUnit): Boolean; override;

    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    function Cost        : Integer; override;
    function Range       : Single; override;
    function Damage      : TVec2i; override;
    function DamageScale : Single; override;
    function Accuracy    : TVec2; override;

    function Req_WeaponType: TUnitItemKind; override;

    function Animation: string; override;
    function SampleDamage(AOwner, ATarget: TRoomUnit): Integer; override;
    function SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean; override;

    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

implementation

uses
  Math;

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

function TSkill_AxeAttack.Name: string;
begin
  Result := 'Рубануть';
end;

function TSkill_AxeAttack.Desc: string;
begin
  Result := 'Базовый навык владения топором';
end;

function TSkill_AxeAttack.Ico: string;
begin
  Result := 'axe_attack.png';
end;

function TSkill_AxeAttack.Cost: Integer;
begin
  Result := 3;
end;

function TSkill_AxeAttack.Range: Single;
begin
  Result := 1;
end;

function TSkill_AxeAttack.Damage: TVec2i;
begin
  Result := Vec(0, 0);
end;

function TSkill_AxeAttack.DamageScale: Single;
begin
  Result := 1;
end;

function TSkill_AxeAttack.Accuracy: TVec2;
begin
  Result := Vec(0.9, 0.9);
end;

function TSkill_AxeAttack.Req_WeaponType: TUnitItemKind;
begin
  Result := ikAxe;
end;

function TSkill_AxeAttack.Animation: string;
begin
  Result := 'Axe0_Attack0';
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

function TSkill_AxeAttack.DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
  if not CanUse(ASkillIndex, AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;
  AOwner.Room.AddMessage(AOwner.Name + ' наносит рубящий удар');
  Result := TBRA_UnitDefaultAttack.Create(AOwner, ATarget, Self, 2000, 866);
end;

function TSkill_AxeAttack.CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := inherited CanUse(ASkillIndex, AOwner, ATarget, AReservedPoints);
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
  Result := 'Простой выстрел';
end;

function TSkill_Shoot.Desc: string;
begin
  Result := 'Приченяет урон. Если попадешь';
end;

function TSkill_Shoot.Ico: string;
begin
  Result := 'bow_shoot.png';
end;

function TSkill_Shoot.Cost: Integer;
begin
  Result := 2;
end;

function TSkill_Shoot.Range: Single;
begin
  Result := 20;
end;

function TSkill_Shoot.Damage: TVec2i;
begin
  Result := Vec(0, 0);
end;

function TSkill_Shoot.DamageScale: Single;
begin
  Result := 1;
end;

function TSkill_Shoot.Accuracy: TVec2;
begin
  Result := Vec(0.9, 0.3);
end;

function TSkill_Shoot.Req_WeaponType: TUnitItemKind;
begin
  Result := ikBow;
end;

function TSkill_Shoot.Animation: string;
begin
  Result := 'Archer_Bow_Attack0';
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

function TSkill_Shoot.DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action;
var
  bullet: TRoomBullet;
begin
  Result := nil;

  if not CanUse(ASkillIndex, AOwner, ATarget) then Exit;
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

function TSkill_Shoot.CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := inherited CanUse(ASkillIndex, AOwner, ATarget, AReservedPoints);
  if not Result then Exit;

  Result := False;
  if AOwner.AP - AReservedPoints < Cost then Exit;
  Result := True;
end;

{ TUnitSkill }

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

function TUnitSkill.Req_WeaponType: TUnitItemKind;
begin
  Result := ikUnknown;
end;

function TUnitSkill.CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := UseReady(AOwner);
end;

constructor TUnitSkill.Create(const AItem: IUnitItem; const AIndex: Integer);
begin
  if AItem <> nil then
    FItem := (AItem as IWeakedInterface).WeakRef;
  FIndex := AIndex;
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
  Result := 'Всегда ведь приятно пнуть врага';
end;

function TSkill_Kick.Ico: string;
begin
  Result := 'kick.png';
end;

function TSkill_Kick.Cost: Integer;
begin
  Result := 3;
end;

function TSkill_Kick.Range: Single;
begin
  Result := 1;
end;

function TSkill_Kick.Damage: TVec2i;
begin
  Result := Vec(25, 25);
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

function TSkill_Kick.SampleDamage(AOwner, ATarget: TRoomUnit): Integer;
begin
  Result := RandomRange(Damage.x, Damage.y+1);
end;

function TSkill_Kick.SampleHitChance(AOwner, ATarget: TRoomUnit): Boolean;
begin
  Result := True;
end;

function TSkill_Kick.DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
  if not CanUse(ASkillIndex, AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;
  AOwner.Room.AddMessage(AOwner.Name + ' наносит удар');
  Result := TBRA_UnitDefaultAttack.Create(AOwner, ATarget, Self, 1000, 300);
end;

function TSkill_Kick.CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := inherited CanUse(ASkillIndex, AOwner, ATarget, AReservedPoints);
  if not Result then Exit;

  Result := False;
  if AOwner.AP - AReservedPoints < Cost then Exit;
  if AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos) > 1 then Exit;
  Result := True;
end;

end.


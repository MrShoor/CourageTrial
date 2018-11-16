unit untSkills;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, untLevel, intfUtils;

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
    function UseReady: Boolean; virtual; abstract;

    function Name: string; virtual; abstract;
    function Desc: string; virtual; abstract;
    function Ico : string; virtual; abstract;

    function Cost : Integer; virtual; abstract;
    function Range: Single; virtual; abstract;
    function Animation: string; virtual; abstract;

    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; virtual; abstract;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; virtual; abstract;
  public
    constructor Create(const AItem: IUnitItem; const AIndex: Integer);
  end;

  { TSkill_Kick }

  TSkill_Kick = class(TUnitSkill)
  protected
    function WearedOnly: Boolean; override;
    function UseReady: Boolean; override;

    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    function Cost : Integer; override;
    function Range: Single; override;
    function Animation: string; override;

    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TSkill_Shoot }

  TSkill_Shoot = class(TUnitSkill)
  protected
    function WearedOnly: Boolean; override;
    function UseReady: Boolean; override;

    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    function Cost : Integer; override;
    function Range: Single; override;
    function Animation: string; override;

    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

implementation

{ TSkill_Shoot }

function TSkill_Shoot.WearedOnly: Boolean;
begin
  Result := True;
end;

function TSkill_Shoot.UseReady: Boolean;
var itm: IUnitItem;
begin
  itm := Item;
  Result := itm.Equipped;
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

function TSkill_Shoot.Animation: string;
begin
  Result := 'Archer_Bow_Attack0';
end;

function TSkill_Shoot.DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action;
var
  bullet: TRoomBullet;
begin
  Result := nil;

  if not CanUse(ASkillIndex, AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;

  bullet := TRoomBullet.Create(AOwner.Room);
  bullet.LoadModels('Erika_Archer_Arrow_Mesh');
  bullet.Owner := AOwner;
  bullet.Velocity := 20;
  bullet.Dmg := 10;
  bullet.MaxRange := Range;
  bullet.Target := ATarget.RoomPos;
  bullet.StartPt := AOwner.RoomPos;
  Result := TBRA_Shoot.Create(AOwner, [bullet], Animation, 1150, 1.37);
end;

function TSkill_Shoot.CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
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

function TSkill_Kick.UseReady: Boolean;
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

function TSkill_Kick.Animation: string;
begin
  Result := 'Kick0';
end;

function TSkill_Kick.DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
  if not CanUse(ASkillIndex, AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - Cost;
  Result := TBRA_UnitDefaultAttack.Create(AOwner, ATarget, Animation, 1000, 300);
end;

function TSkill_Kick.CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer): Boolean;
begin
  Result := False;
  if AOwner.AP - AReservedPoints < Cost then Exit;
  if AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos) > 1 then Exit;
  Result := True;
end;

end.


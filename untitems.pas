unit untItems;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, untLevel;

type

  { TUnitItem }

  TUnitItem = class(TInterfacedObject, IUnitItem)
  private
  public
    function Slot  : TRoomUnitEqSlot; virtual; abstract;
    function Model : string;          virtual; abstract;
    function Ico48 : string;          virtual; abstract;

    function SkillsCount: Integer; virtual; abstract;
    function Animation(ASkillIndex: Integer): string; virtual; abstract;
    function ActionCost(ASkillIndex: Integer): Integer; virtual; abstract;
    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; virtual; abstract;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; virtual; abstract;
  end;

  { TDefaultKick }

  TDefaultKick = class(TUnitItem)
  public
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function SkillsCount: Integer; override;
    function Animation(ASkillIndex: Integer): string; override;
    function ActionCost(ASkillIndex: Integer): Integer; override;
    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TArcherBow }

  TArcherBow = class(TUnitItem)
  public
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function SkillsCount: Integer; override;
    function Animation(ASkillIndex: Integer): string; override;
    function ActionCost(ASkillIndex: Integer): Integer; override;
    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

  { TAxe }

  TAxe = class(TUnitItem)
  public
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function SkillsCount: Integer; override;
    function Animation(ASkillIndex: Integer): string; override;
    function ActionCost(ASkillIndex: Integer): Integer; override;
    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; override;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; override;
  end;

implementation

{ TAxe }

function TAxe.Slot: TRoomUnitEqSlot;
begin
  Result := esNone;
end;

function TAxe.Model: string;
begin
  Result := '';
end;

function TAxe.Ico48: string;
begin
  Result := 'axe.png';
end;

function TAxe.SkillsCount: Integer;
begin
  Result := 0;
end;

function TAxe.Animation(ASkillIndex: Integer): string;
begin
  Result := '';
end;

function TAxe.ActionCost(ASkillIndex: Integer): Integer;
begin
  Result := 0;
end;

function TAxe.DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
end;

function TAxe.CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit;
  AReservedPoints: Integer): Boolean;
begin
  Result := False;
end;

{ TDefaultKick }

function TDefaultKick.Slot: TRoomUnitEqSlot;
begin
  Result := esNone;
end;

function TDefaultKick.Model: string;
begin
  Result := '';
end;

function TDefaultKick.Ico48: string;
begin
  Result := '';
end;

function TDefaultKick.SkillsCount: Integer;
begin
  Result := 1;
end;

function TDefaultKick.Animation(ASkillIndex: Integer): string;
begin
  Result := 'Kick0';
end;

function TDefaultKick.ActionCost(ASkillIndex: Integer): Integer;
begin
  Result := 3;
end;

function TDefaultKick.DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action;
begin
  Result := nil;
  if not CanUse(ASkillIndex, AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - ActionCost(ASkillIndex);
  Result := TBRA_UnitDefaultAttack.Create(AOwner, ATarget, Animation(ASkillIndex), 1000, 300);
end;

function TDefaultKick.CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit;
  AReservedPoints: Integer): Boolean;
begin
  Result := False;
  if AOwner.AP - AReservedPoints < ActionCost(ASkillIndex) then Exit;
  if AOwner.Room.Distance(AOwner.RoomPos, ATarget.RoomPos) > 1 then Exit;
  Result := True;
end;

{ TArcherBow }

function TArcherBow.Slot: TRoomUnitEqSlot;
begin
  Result := esBothHands;
end;

function TArcherBow.Model: string;
begin
  Result := 'Archer_Bow';
end;

function TArcherBow.Ico48: string;
begin
  Result := 'bow.png';
end;

function TArcherBow.SkillsCount: Integer;
begin
  Result := 1;
end;

function TArcherBow.Animation(ASkillIndex: Integer): string;
begin
  Result := 'Archer_Bow_Attack0';
end;

function TArcherBow.ActionCost(ASkillIndex: Integer): Integer;
begin
  Result := 3;
end;

function TArcherBow.DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action;
var
  bullet: TRoomBullet;
begin
  Result := nil;

  if not CanUse(ASkillIndex, AOwner, ATarget) then Exit;
  AOwner.AP := AOwner.AP - ActionCost(ASkillIndex);

  bullet := TRoomBullet.Create(AOwner.Room);
  bullet.LoadModels('Erika_Archer_Arrow_Mesh');
  bullet.Owner := AOwner;
  bullet.Velocity := 20;
  bullet.Dmg := 10;
  bullet.MaxRange := 20;
  bullet.Target := ATarget.RoomPos;
  bullet.StartPt := AOwner.RoomPos;
  Result := TBRA_Shoot.Create(AOwner, [bullet], Animation(ASkillIndex), 1150, 1.37);
end;

function TArcherBow.CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit;
  AReservedPoints: Integer): Boolean;
begin
  Result := False;
  if AOwner.AP - AReservedPoints < ActionCost(ASkillIndex) then Exit;
  Result := True;
end;

end.


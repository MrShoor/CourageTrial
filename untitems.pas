unit untItems;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, untLevel, intfUtils, mutils;

type
  { TUnitItem }

  TUnitItem = class(TWeakedInterfacedObject, IUnitItem)
  protected
    FEquipped: Boolean;
    FSkills: array of IUnitSkill;
  public
    function  GetEquipped: Boolean;
    procedure SetEquipped(const AValue: Boolean);

    function Kind  : TUnitItemKind;   virtual; abstract;
    function Slot  : TRoomUnitEqSlot; virtual; abstract;
    function Model : string;          virtual; abstract;
    function Ico48 : string;          virtual; abstract;

    function Weapon_Damage: TVec2i; virtual;

    function SkillsCount: Integer; virtual;
    function Skill(ASkillIndex: Integer): IUnitSkill; virtual;

    constructor Create; virtual;
  end;

  { TArcherBow }

  TArcherBow = class(TUnitItem)
  public
    function Kind  : TUnitItemKind;   override;
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function Weapon_Damage: TVec2i; override;
  end;

  { TAxe }

  TAxe = class(TUnitItem)
  public
    function Kind  : TUnitItemKind;   override;
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function Weapon_Damage: TVec2i; override;
  end;

implementation

{ TAxe }

function TAxe.Kind: TUnitItemKind;
begin
  Result := ikAxe;
end;

function TAxe.Slot: TRoomUnitEqSlot;
begin
  Result := TRoomUnitEqSlot.esRightHand;
end;

function TAxe.Model: string;
begin
  Result := 'Axe0';
end;

function TAxe.Ico48: string;
begin
  Result := 'axe.png';
end;

function TAxe.Weapon_Damage: TVec2i;
begin
  Result := Vec(30, 40);
end;

{ TUnitItem }

function TUnitItem.GetEquipped: Boolean;
begin
  Result := FEquipped;
end;

procedure TUnitItem.SetEquipped(const AValue: Boolean);
begin
  if Slot = esNone then Exit;
  FEquipped := AValue;
end;

function TUnitItem.Weapon_Damage: TVec2i;
begin
  Result := Vec(0,0);
end;

function TUnitItem.SkillsCount: Integer;
begin
  Result := Length(FSkills);
end;

function TUnitItem.Skill(ASkillIndex: Integer): IUnitSkill;
begin
  Result := FSkills[ASkillIndex];
end;

constructor TUnitItem.Create;
begin

end;

{ TArcherBow }

function TArcherBow.Kind: TUnitItemKind;
begin
  Result := ikBow;
end;

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

function TArcherBow.Weapon_Damage: TVec2i;
begin
  Result := Vec(10, 30);
end;

end.


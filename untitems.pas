unit untItems;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, untLevel, intfUtils;

type

  { TUnitItem }

  TUnitItem = class(TWeakedInterfacedObject, IUnitItem)
  protected
    FEquipped: Boolean;
    FSkills: array of IUnitSkill;
  public
    function  GetEquipped: Boolean;
    procedure SetEquipped(const AValue: Boolean);

    function Slot  : TRoomUnitEqSlot; virtual; abstract;
    function Model : string;          virtual; abstract;
    function Ico48 : string;          virtual; abstract;

    function SkillsCount: Integer; virtual;
    function Skill(ASkillIndex: Integer): IUnitSkill; virtual;

    constructor Create; virtual;
  end;

  { TArcherBow }

  TArcherBow = class(TUnitItem)
  public
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;
  public
    constructor Create; override;
  end;

  { TAxe }

  TAxe = class(TUnitItem)
  public
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;
  end;

implementation

uses untSkills;

{ TAxe }

function TAxe.Slot: TRoomUnitEqSlot;
begin
  Result := TRoomUnitEqSlot.esNone;
end;

function TAxe.Model: string;
begin
  Result := '';
end;

function TAxe.Ico48: string;
begin
  Result := 'axe.png';
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

constructor TArcherBow.Create;
begin
  inherited Create;
  SetLength(FSkills, 1);
  FSkills[0] := TSkill_Shoot.Create(Self, 0);
end;

end.

